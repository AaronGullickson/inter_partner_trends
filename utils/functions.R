# functions.R


# Globals -----------------------------------------------------------------

PAIRINGS <- c("White/Black",
              "White/AIAN",
              "White/API",
              "White/Hispanic",
              "Black/AIAN",
              "Black/API",
              "Black/Hispanic",
              "AIAN/API",
              "AIAN/Hispanic",
              "API/Hispanic",
              "White/White-Black",
              "White/White-AIAN",
              "White/White-API")

# number of bootstrap samples
# TODO: this is way too low for final analysis, but should be sufficient for
# preliminary runs where we just want a reasonably close estimate
B <- 10


# Modeling functions ------------------------------------------------------

bootstrap_model <- function(ind_data, 
                            selected_groups,
                            show_progress = FALSE,
                            ...) {
  
  if(show_progress) {
    pb <- progress_bar$new(format = "[:bar] :percent in :elapsed",
                           total = B + 2)
    pb$tick(0)
  }
  
  # filter out unnecessary groups for later speed improvements
  # estimate_lor does this but it will be faster to bootstrap sample a 
  # smaller individual dataset
  if(show_progress) {
    pb$tick()
  }
  ind_data <- ind_data |>
    filter(race_husband %in% selected_groups,
           race_wife %in% selected_groups) |>
    mutate(race_husband = fct_drop(race_husband),
           race_wife = fct_drop(race_wife))
  
  results <- map(1:B, function(i) {
    if(show_progress) {
      pb$tick()
    }
    
    result <- ind_data |>
      slice_sample(n = nrow(ind_data), replace = TRUE) |>
      estimate_lor(selected_groups, se = FALSE, ...)
    
    return(result)
  })
  
  if(show_progress) {
    pb$tick()
  }
  # do a full join here in case some terms are missing in some samples
  by_vars <- colnames(results[[1]])
  by_vars <- by_vars[by_vars != "estimate"]
  results <- reduce(results, full_join, by = by_vars)
  
  estimates <- results |> 
    select(starts_with("estimate"))
  
  results |>
    select(-starts_with("estimate")) |>
    bind_cols(tibble(
      estimate = apply(estimates, 1, mean, na.rm = TRUE),
      std.error = apply(estimates, 1, sd, na.rm = TRUE),
      conf.low = apply(estimates, 1, quantile, 0.025, na.rm = TRUE),
      conf.high = apply(estimates, 1, quantile, 0.975, na.rm = TRUE)
    ))
}

estimate_lor <- function(ind_data, 
                         selected_groups,
                         composition_var = NULL,
                         conditional_var = NULL,
                         control_var = NULL,
                         se = TRUE,
                         use_weights = TRUE,
                         pairwise = FALSE) {
  
  
  # In some complex cases, running each pairwise comparison separately
  # will be faster than running a single model
  if(pairwise) {
    groups <- get_permutations(selected_groups)
    results <- pmap(groups, function(group1, group2) {
      model_data |>
        estimate_lor(c(group1, group2), 
                     composition_var, conditional_var, control_var,
                     by, se, pairwise = FALSE)
    }) |>
      bind_rows()
    return(results)
  }
  
  ## prepare model data ##
  controls <- NULL
  if(!is.null(control_var)) {
    controls <- paste0(control_var, c("_husband", "_wife"))
  }                     
  grouping_vars <- c("race_husband", "race_wife", "year", 
                     composition_var, conditional_var, controls)
  sum_var <- ifelse(use_weights, "weight_age", "unity")
  
  model_data <- ind_data |>
    # trim to just selected groups and drop unused factor levels
    filter(race_husband %in% selected_groups,
           race_wife %in% selected_groups) |>
    mutate(race_husband = fct_drop(race_husband),
           race_wife = fct_drop(race_wife)) |>
    group_by(!!!syms(grouping_vars), .drop = FALSE) |>
    mutate(unity = 1) |>
    summarize(freq = sum(!!sym(sum_var)), .groups = "drop")
  
  # hunt for zero values to identify bad estimates later. We first need to 
  # aggregate data, ignoring compositional and control variables
  grouping_vars <- c("race_husband", "race_wife", "year", conditional_var)
  zero_values <- model_data |>
    group_by(!!!syms(grouping_vars)) |>
    summarize(freq = sum(freq), .groups = "drop") |>
    filter(freq == 0 & race_husband != race_wife) |>
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
  # turn off warnings about non-integer poisson - we know because of weights
  model <- suppressWarnings(
    glm(formula_model, data = model_data, family = poisson)
  )

   ## get marginal effects of variables we want ##
  vars  <- str_subset(names(model$coef), "^inter_(.+)TRUE$") |> 
    str_remove("TRUE$")
  
  # suppress warnings because we know some coefficients might be missing
  marg <- suppressWarnings(
    avg_slopes(model, 
               variables = vars,
               by = c(conditional_var, "year"),
               type = "link",
               vcov = se)
    ) |>
    as_tibble() |>
    mutate(year = as.numeric(paste(year)),
           term = get_intermar_names(term))
  
  ## integrate information about zero cases and remove ##
  # NOTE: There is still some small chance that if we have a zero count 
  # on an endogamy cell (e.g. White/White) but not on either exogamy cell,
  # we would fail to remove the case when it should be. However, this is
  # a highly unlikely prospect in our data, although somehthing to be 
  # alert for if we parse into very small categories.
  zero_values <- zero_values |>
    select(-race_husband, -race_wife, -freq) |>
    distinct() |>
    mutate(missing = TRUE,
           year = as.numeric(paste(year)))
  
  marg <- marg |> 
    left_join(zero_values, by = c("term", "year", conditional_var)) |>
    filter(is.na(missing)) |>
    select(-missing) |>
    mutate(term = factor(term, levels = PAIRINGS))
  
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
