# functions.R


# Globals -----------------------------------------------------------------

PAIRINGS <- c(
  # single race pairings
  "White/Black",
  "White/Indigenous",
  "White/API",
  "White/AIAN",
  "White/PI",
  "White/Asian",
  "White/EastAsian",
  "White/SEAsian",
  "White/SouthAsian",
  "White/Hispanic",
  "White/WhiteHispanic",
  "White/BlackHispanic",
  "White/OtherHispanic",
  "Black/Indigenous",
  "Black/AIAN",
  "Black/Asian",
  "Black/API",
  "Black/Hispanic",
  "Black/WhiteHispanic",
  "Black/BlackHispanic",
  "Black/OtherHispanic",
  "Indigenous/Asian",
  "Indigenous/Hispanic",
  "Asian/Hispanic",
  "AIAN/API",
  # single to multi race pairings
  "White/White-Black",
  "Black/White-Black",
  "White/White-Indigenous",
  "Indigenous/White-Indigenous",
  "White/White-Asian",
  "Asian/White-Asian",
  "Black/Black-Asian",
  "Asian/Black-Asian",
  "Black/Black-Indigenous",
  "Indigenous/Black-Indigenous",
  # panethnic pairings
  "WhiteHispanic/BlackHispanic",
  "WhiteHispanic/OtherHispanic",
  "BlackHispanic/OtherHispanic",
  "EastAsian/SEAsian",
  "EastAsian/SouthAsian",
  "SEAsian/SouthAsian",
  "AIAN/PI"
)

# number of bootstrap samples
# his is way too low for final analysis, but should be sufficient for
# preliminary runs where we just want a reasonably close estimate. The actual
# B will typically be set in the quarto doc where it is used.
B <- 10


# Modeling functions ------------------------------------------------------

generate_bootstrap_indices <- function(ind_data, 
                                       n_replicates = B, 
                                       sample_design = TRUE) {
  
  # create index variable
  ind_data <- ind_data |>
    mutate(idx = seq_len(nrow(ind_data)))
  
  map(1:n_replicates, function(i) {
    if(sample_design) {
      # we need to adjust for year and strata
      ind_data |>
        group_by(year, strata) |>
        group_split() |>
        map(function(stratum_data) {
          clusters <- unique(stratum_data$cluster)
          tibble(cluster = sample(clusters, length(clusters), replace = TRUE))
        }) |>
        bind_rows() |>
        left_join(ind_data, by = "cluster", relationship = "many-to-many") |>
        pull(idx)
    } else {
      # Simple bootstrap but still stratify by year
      ind_data |>
        group_by(year) |>
        group_split() |>
        map(function(year_data) {
          sample(year_data$idx, nrow(year_data), replace = TRUE)
        }) |>
        list_c()
    }
  })
}

bootstrap_model <- function(ind_data, 
                            selected_groups,
                            bootstrap_indices = bs_indices,
                            conf_level = 0.83,
                            show_progress = FALSE,
                            ...) {
  
  ci_upper <- 1-(1-conf_level)/2
  ci_lower <- (1-conf_level)/2
  
  if(show_progress) {
    pb <- progress_bar$new(format = "[:bar] :percent in :elapsed",
                           total = length(bootstrap_indices) + 1,
                           show_after = 0)
    pb$tick(0)
  }

  # first estimate the full model to get analytical point estimates
  point_estimates <- ind_data |>
    estimate_lor(selected_groups, se = FALSE, ...)
  if(show_progress) {
    pb$tick()
  }
  
  # now loop for the bootstrap
  results <- vector("list", length(bootstrap_indices))
  for(i in seq_len(length(bootstrap_indices))) {
    results[[i]] <- estimate_lor(ind_data[bootstrap_indices[[i]],],
                                 selected_groups, se = FALSE, ...)
    
    if (show_progress) {
      pb$tick()
    }
  }
  
  by_vars <- colnames(results[[1]])
  by_vars <- by_vars[by_vars != "estimate"]
  # full join here because its possible that some coefficients might be dropped
  # in some bootstrap samples if we get zero values
  results <- reduce(results, full_join, by = by_vars)
  
  # summarize results across estimates
  results |>
    rowwise(by_vars) |>
    summarize(
      std.error = sd(c_across(starts_with("estimate")), na.rm = TRUE),
      conf.low = quantile(c_across(starts_with("estimate")), ci_lower, na.rm = TRUE),
      conf.high = quantile(c_across(starts_with("estimate")), ci_upper, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # join back to the point estimates
    right_join(point_estimates, by = by_vars) |>
    # move estimate before inference measures
    relocate(estimate, .before = std.error) |>
    # change type to bootstrap
    mutate(type = "bootstrap")
}

estimate_lor <- function(ind_data, 
                         selected_groups,
                         composition_var = NULL,
                         conditional_var = NULL,
                         control_var = NULL,
                         se = TRUE,
                         use_weights_age = TRUE,
                         use_weights_sample = TRUE,
                         conf_level = 0.83,
                         pairwise = FALSE,
                         year_separate = FALSE) {
  
  
  # In some complex cases, running each pairwise comparison separately
  # will be faster than running a single model
  if(pairwise) {
    groups <- get_permutations(selected_groups)
    results <- pmap(groups, function(group1, group2) {
      ind_data |>
        estimate_lor(c(group1, group2), 
                     composition_var, conditional_var, control_var,
                     se, use_weights_age, use_weights_sample, conf_level, 
                     pairwise = FALSE, year_separate)
    }) |>
      bind_rows()
    return(results)
  }
  
  # it might also be much faster to do individual years separately
  if(year_separate) {
    results <- map(unique(ind_data$year), function(y) {
      ind_data |>
        filter(year == y) |>
        estimate_lor(selected_groups, 
                     composition_var, conditional_var, control_var,
                     se, use_weights_age, use_weights_sample, conf_level, 
                     pairwise, year_separate = FALSE)
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
  
  # pre-process individual data
  ind_data <- ind_data |>
    # trim to just selected groups and drop unused factor levels
    filter(race_husband %in% selected_groups,
           race_wife %in% selected_groups) |>
    mutate(year = fct_drop(year)) |>
    # set up a variable for summing frequencies
    mutate(sum_var = 1)
  
  # adjust based on weights
  if(use_weights_age) {
    ind_data <- ind_data |>
      mutate(sum_var = sum_var * weight_age)
  }
  
  if(use_weights_sample) {
    ind_data <- ind_data |>
      mutate(sum_var = sum_var * weight_sample)
  }
  
  # renormalize weights
  ind_data <- ind_data |>
    group_by(year) |>
    mutate(sum_var = sum_var / mean(sum_var)) |>
    ungroup()
  
  # create contingency table
  model_data <- ind_data |>
    group_by(!!!syms(grouping_vars), .drop = FALSE) |>
    summarize(freq = sum(sum_var), .groups = "drop")
  
  # clean up the memory here now that we don't need individual data
  rm(ind_data)
  gc()
  
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
  single_year <- length(unique(model_data$year)) == 1
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
  interactors <- ifelse(single_year, 
                        ifelse(is.null(conditional_var), "", conditional_var),
                        paste(c(conditional_var, "year"), collapse = "*"))
  if(interactors != "") {
    formula_model <- paste0("(", formula_model, ")*(", interactors, ")")
  }
  if(!is.null(control_var)) {
    formula_control <- paste(c(paste0("race_husband*", control_var, "_husband"),
                               paste0("race_wife*", control_var, "_wife")),
                             collapse = "+")
    formula_control <- paste0("(", formula_control, ")")
    formula_control <- paste(formula_control,
                             "+",
                             paste(paste(paste0(control_var, "_husband"), 
                                         paste0(control_var, "_wife"),
                                         sep = "*"),
                                   collapse = "+"))
    if(!single_year) {
      formula_control <- paste0("(", formula_control, ")*year")
    }
    formula_model <- paste(formula_model, formula_control, sep = "+")
  }
  formula_model <- reformulate(formula_model, "freq")
  
  ## run the model ##
  if(se) {
    # turn off warnings about non-integer poisson - we know because of weights
    model <- suppressWarnings(
      glm(formula_model, data = model_data, family = poisson)
    )
  } else {
    model <- glm(formula_model, data = model_data, family = quasipoisson)
  }

   ## get marginal effects of variables we want ##
  vars  <- str_subset(names(model$coef), "^inter_(.+)TRUE$") |> 
    str_remove("TRUE$")
  
  # suppress warnings because we know some coefficients might be missing
  marg <- suppressWarnings(
    avg_slopes(model, 
               variables = vars,
               by = c(conditional_var, "year"),
               type = "link",
               vcov = se, 
               conf_level = conf_level)
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
  
  # add some additional information here and clean up
  marg <- marg |>
    select(-contrast, -starts_with("predicted")) |>
    mutate(type = "glm",
           age_weighted = use_weights_age,
           sample_weighted = use_weights_sample,
           composition = ifelse(is.null(composition_var),
                                "none",
                                paste(composition_var, collapse = ",")),
           control = ifelse(is.null(control_var),
                            "none",
                            paste(control_var, collapse = ",")))
  
  if(se) {
    marg <- marg |>
      select(-statistic, -p.value, -s.value)
  }
  
  # some memory cleanup
  rm(model_data)
  rm(model)
  gc()
  
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
    str_replace_all("pi", "PI") |>
    str_replace_all("asian", "Asian") |>
    str_replace_all("aian", "AIAN") |>
    str_replace_all("indigenous", "Indigenous") |>
    str_replace_all("other", "Other") |>
    str_replace_all("east", "East") |>
    str_replace_all("se", "SE") |>
    str_replace_all("south", "South") |>
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
