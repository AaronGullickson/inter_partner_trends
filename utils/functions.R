# functions.R



# Modeling functions ------------------------------------------------------

estimate_lor <- function(model_data, 
                         selected_groups,
                         composition_var = NULL,
                         conditional_var = NULL,
                         control_var = NULL,
                         by = c("year"),
                         se = TRUE) {
  
  ## prepare model data ##
  
  # trim down the data to selected groups and frequency counts greater than zero
  model_data <- model_data |>
    filter(race_husband %in% selected_groups,
           race_wife %in% selected_groups) |>
    mutate(year = as.factor(year))
  
  # hunt for zero values to identify bad estimates later
  zero_values <- model_data |>
    filter(freq == 0) |>
    mutate(term = NA_character_)
  
  # now remove zero values from the data or they will mess up the models
  model_data <- model_data |>
    filter(freq > 0)
  
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
      
      # while we are here, lets also construct the intermarriage label
      # for the zero cases
      zero_values <- zero_values |>
        mutate(
          term = case_when(
            (race_husband == race1 & 
               race_wife == race2) | 
              (race_husband == race2 & 
                 race_wife == race1) ~ paste(race1, race2, sep = "/"),
            TRUE ~ term
          )
        )
    }
  }
  
  ## create formula ##
  
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
  
  ## run the model ##
  model <- glm(formula_model, data = model_data, family = poisson)
  
  ## get marginal effects of variables we want ##
  vars  <- str_subset(names(model$coef), "^inter_(.+)TRUE$") |> 
    str_remove("TRUE$")
  
  marg <- avg_slopes(model, 
                     variables = vars,
                     by = by,
                     type = "link",
                     vcov = se) |>
    as_tibble() |>
    mutate(year = as.numeric(paste(year)),
           term = get_intermar_names(term))
  
  ## integrate information about zero cases and remove ##
  zero_values <- zero_values |>
    select(-race_husband, -race_wife) |>
    distinct() |>
    mutate(missing = TRUE,
           year = as.numeric(paste(year)))
  
  marg <- marg |> 
    left_join(zero_values) |>
    filter(is.na(missing)) |>
    select(-missing)
  
  return(marg)
}


get_intermar_names <- function(x) {
  x |>
    str_remove("^inter_") |>
    str_replace("_", " ") |>
    str_replace_all("white", "White") |>
    str_replace_all("black", "Black") |>
    str_replace_all("hispanic", "Hispanic") |>
    str_replace_all("api", "API") |>
    str_replace_all("aian", "AIAN") |>
    str_replace(" ", "/") |>
    str_replace_all("\\.", "-")
}
