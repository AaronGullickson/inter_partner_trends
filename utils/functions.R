# functions.R



# Modeling functions ------------------------------------------------------


# an overarching function that combines calculate_lor_model and 
# extract_marg_effects
estimate_lor <- function(model_data,
                         selected_groups, 
                         composition_var = NULL,
                         conditional_var = NULL,
                         control_var = NULL,
                         by = c("year"),
                         se = TRUE,
                         pairwise = FALSE) {
  
  # in cases with large number of groups or composition/control variable
  # with lots of categories, it will be faster to estimate each pairwise
  # comparison and then combine them together than to estimate the full model
  if(pairwise) {
    groups <- get_permutations(selected_groups)
    pmap(groups, function(group1, group2) {
      model_data |>
        calculate_lor_model(c(group1, group2), 
                            composition_var, conditional_var, control_var)
    }) |>
      map(extract_marg_effects, by, se) |>
      bind_rows()
  } else {
    # otherwise do them altogether
    model_data |>
      calculate_lor_model(selected_groups, composition_var, conditional_var,
                          control_var) |>
      extract_marg_effects(by, se)
  }
}


calculate_lor_model <- function(model_data, 
                                selected_groups,
                                composition_var = NULL,
                                conditional_var = NULL,
                                control_var = NULL) {
  
  # trim down the data to selected groups and frequency counts greater than zero
  model_data <- model_data |>
    filter(race_husband %in% selected_groups,
           race_wife %in% selected_groups) |>
    mutate(year = as.factor(year))
  
  # create required variables
  for(i in 1:(length(selected_groups)-1)) {
    for(j in (i+1):length(selected_groups)) {
      
      race1 <- selected_groups[i]
      race2 <- selected_groups[j]
      
      var_suffix <- paste(str_to_lower(race1), str_to_lower(race2), sep = "_")
      var_suffix <- str_replace_all(var_suffix, "-", ".")
      
      var_name <- paste0("inter_", var_suffix)                 
      model_data <- model_data |>
        mutate({{ var_name }} := (race_husband == race1 & race_wife == race2))
      
      # for cases not involving the reference category, we need to add a 
      # variable for the two gender combinations with 1 and -1 coding so that 
      # the general effect is the average of the two
      if(i > 1) {
        # we need a gender adjuster
        var_name <- paste0("gender_", var_suffix)
        model_data <- model_data |>
          mutate(
            {{ var_name }} := case_when(
              race_husband == race1 & race_wife == race2 ~ 1,
              race_husband ==  race2 & race_wife ==  race1 ~ -1,
              TRUE ~ 0
            )
          )
      }
    }
  }
  
  # create formula
  formula_model <- "(race_husband+race_wife)"
  if(!is.null(composition_var)) {
    formula_model <- paste0(formula_model,
                            "*(",
                            paste(composition_var, collapse = "+"),
                            ")")
  }
  mar_terms <- colnames(model_data) |> str_subset("^inter_|^gender_")
  formula_model <- paste0(formula_model,
                          "+(",
                          paste(mar_terms, collapse = "+"),
                          ")")
  formula_model <- paste0("(", 
                          formula_model, 
                          ")*(",
                          paste(c(conditional_var, "year"), collapse = "*"),
                          ")")
  if(!is.null(control_var)) {
    formula_control <- paste(c(paste0("race_husband*", control_var, "_husband"),
                               paste0("race_wife*", control_var, "_wife")),
                             collapse = "+")
    formula_control <- paste0("(", formula_control, ")")
    if(!is.null(composition_var)) {
      formula_control <- paste0(formula_control, 
                                "*(",
                                paste(composition_var, collapse = "+"),
                                ")")
    }
    if(!is.null(conditional_var)) {
      formula_control <- paste0(formula_control, 
                                "*(",
                                paste(conditional_var, collapse = "+"),
                                ")")
    }
    formula_control <- paste(formula_control,
                             "+",
                             paste(paste(paste0(control_var, "_husband"), 
                                         paste0(control_var, "_wife"),
                                         sep = "*"),
                                   collapse = "+"))
    formula_control <- paste0("(", formula_control, ")*year")
    formula_model <- paste(formula_model, formula_control, sep = "+")
  }
  formula_model <- reformulate(formula_model, "freq")
  
  # run the model
  model <- glm(formula_model, data = model_data, family = poisson)
  
  return(model)
}

extract_marg_effects <- function(model, by = c("year"), se = TRUE) {
  
  missing <- coef(model) |> 
    enframe(name = "variable", value = "coef") |>
    filter(is.na(coef), 
           # in some cases, the dropped variable might be the gender_ case
           str_detect(variable, "^inter_|^gender_")) |>
    mutate(term = str_remove(str_split_i(variable, ":", 1), "TRUE"),
           # change name of gender_ to inter_ to facilitate linking
           term = str_replace(term, "gender_", "inter_"),
           year = as.numeric(str_sub(variable, start = -4)),
           year = if_else(is.na(year), 2000, year),
           missing = TRUE)  |>
    select(term, year, missing) |>
    distinct()
  
  # get variables we want
  vars  <- str_subset(names(model$coef), "^inter_(.+)TRUE$") |> 
    str_remove("TRUE$")
  
  marg <- avg_slopes(model, 
                     variables = vars,
                     by = by,
                     type = "link",
                     vcov = se) |>
    as_tibble() |>
    mutate(year = as.numeric(paste(year))) |> 
    left_join(missing) |>
    filter(is.na(missing)) |>
    select(-missing) |>
    # don't change term names until after the merge with missing
    mutate(term = get_intermar_names(term))
  
  return(marg)
}


get_intermar_names <- function(x) {
  x |>
    str_remove("^inter_") |>
    str_replace("_", " ") |>
    str_to_title() |>
    str_replace("Api", "API") |>
    str_replace("Aian", "AIAN") |>
    str_replace(" ", "/")
}

get_permutations <- function(groups) {
  
  permutations <- NULL
  for(i in 1:(length(groups)-1)) {
    for(j in (i+1):length(groups)) {
      if(i == j) {
        next
      }
      group1 <- groups[i]
      group2 <- groups[j]
      permutations <- permutations |>
        bind_rows(tibble(group1, group2))
    }
  }
  
  return(permutations)
  
}
