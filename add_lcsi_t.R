add_lcsi_t<- function (.dataset, lcsi_stress_vars, lcsi_crisis_vars, lcsi_emergency_vars, 
          yes_val = NULL, no_val = NULL, exhausted_val = NULL, not_applicable_val = NULL, 
          ignore_NA = FALSE) 
{
  df <- .dataset
  if (is.null(yes_val)) {
    yes_val <- "yes"
  }
  if (is.null(no_val)) {
    no_val <- "no_had_no_need"
  }
  if (is.null(exhausted_val)) {
    exhausted_val <- "no_exhausted"
  }
  if (is.null(not_applicable_val)) {
    not_applicable_val <- "not_applicable"
  }
  if (length(unique(lcsi_stress_vars)) != 4) {
    stop(paste0("Need 4 seperate, unique 'stress' livelihood coping strategies. There were ", 
                length(lcsi_stress_vars), " variables were given for 'stress'."))
  }
  if (length(unique(lcsi_crisis_vars)) != 3) {
    stop(paste0("Need 3 seperate, unique 'crisis' livelihood coping strategies. There were ", 
                length(lcsi_crisis_vars), " variables were given for 'crisis'."))
  }
  if (length(unique(lcsi_emergency_vars)) != 3) {
    stop(paste0("Need 3 seperate, unique 'emergency' livelihood coping strategies. There were ", 
                length(lcsi_emergency_vars), " variables were given for 'emergency'."))
  }
  lcs_codes <- df %>% dplyr::select(c(lcsi_stress_vars, lcsi_crisis_vars, 
                                      lcsi_emergency_vars)) %>% t() %>% c() %>% unique()
  lcs_codes <- lcs_codes[!is.na(lcs_codes)]
  if (length(lcs_codes) > 4) {
    stop(paste0("There are ", length(lcs_codes), " in your LCSI variables, but there should only be 4 standard values (Yes, No, Exhausted, Not Applicable). Please check your inputs.s"))
  }
  a <- yes_val %in% lcs_codes
  b <- no_val %in% lcs_codes
  c <- exhausted_val %in% lcs_codes
  d <- not_applicable_val %in% lcs_codes
  e <- any(lcs_codes %in% yes_val)
  f <- any(lcs_codes %in% no_val)
  g <- any(lcs_codes %in% exhausted_val)
  h <- any(lcs_codes %in% not_applicable_val)
  if (length(lcs_codes) == 4 & !all(c(a, b, c, d, e, f, g, 
                                      h))) {
    print(paste0("There are 4 unique response values in your input, but at least one of them does not match your expected values. Please check your input."))
    print(paste0("Values in your dataset: "))
    print(paste0(lcs_codes))
    print(cat(paste0("Values you expected, or default values: \n", 
                     "Yes: ", yes_val, " \n", "No: ", no_val, " \n", 
                     "Exhausted: ", exhausted_val, " \n", "Not applicable: ", 
                     not_applicable_val, "\n")))
    stop("Please check your expected, or default, values match what is in your dataset.")
  }
  expected_values <- c(yes_val, no_val, exhausted_val, not_applicable_val)
  if (length(unique(expected_values)) != 4) {
    stop("There is duplication in the resposne values you input for yes, no, exhausted, and not applicable. Please verifiy your inputs.")
  }
  if (length(lcs_codes) < 4 & !all(a, b, c, d)) {
    warning("There are less than 4 LCSI responses observed in your dataset, and at least one of your expected values isn't observed. It's possible it hasn't been reported yet, but we should usually expect all values to appear at least once over the course of an assessment.")
  }
  df[c(lcsi_stress_vars, lcsi_crisis_vars, lcsi_emergency_vars)] <- lapply(df[c(lcsi_stress_vars, 
                                                                                lcsi_crisis_vars, lcsi_emergency_vars)], as.character)
  df <- df %>% dplyr::mutate(lcsi_stress1 = !!rlang::sym(lcsi_stress_vars[[1]]), 
                             lcsi_stress2 = !!rlang::sym(lcsi_stress_vars[[2]]), 
                             lcsi_stress3 = !!rlang::sym(lcsi_stress_vars[[3]]), 
                             lcsi_stress4 = !!rlang::sym(lcsi_stress_vars[[4]]), 
                             lcsi_crisis1 = !!rlang::sym(lcsi_crisis_vars[[1]]), 
                             lcsi_crisis2 = !!rlang::sym(lcsi_crisis_vars[[2]]), 
                             lcsi_crisis3 = !!rlang::sym(lcsi_crisis_vars[[3]]), 
                             lcsi_emergency1 = !!rlang::sym(lcsi_emergency_vars[[1]]), 
                             lcsi_emergency2 = !!rlang::sym(lcsi_emergency_vars[[2]]), 
                             lcsi_emergency3 = !!rlang::sym(lcsi_emergency_vars[[3]]))
  df <- df %>% dplyr::mutate_at(dplyr::vars(c("lcsi_stress1", 
                                              "lcsi_stress2", "lcsi_stress3", "lcsi_stress4", "lcsi_crisis1", 
                                              "lcsi_crisis2", "lcsi_crisis3", "lcsi_emergency1", "lcsi_emergency2", 
                                              "lcsi_emergency3")), list(~dplyr::case_when(. == yes_val ~ 
                                                                                            "yes", . == no_val ~ "no_had_no_need", . == exhausted_val ~ 
                                                                                            "no_exhausted", . == not_applicable_val ~ "not_applicable", 
                                                                                          TRUE ~ NA_character_)))
  df <- df %>% dplyr::mutate(lcsi_stress_yes = dplyr::case_when(lcsi_stress1 == 
                                                                  "yes" | lcsi_stress2 == "yes" | lcsi_stress3 == "yes" | 
                                                                  lcsi_stress4 == "yes" ~ "1", TRUE ~ "0"), lcsi_stress_exhaust = dplyr::case_when(lcsi_stress1 == 
                                                                                                                                                     "no_exhausted" | lcsi_stress2 == "no_exhausted" | lcsi_stress3 == 
                                                                                                                                                     "no_exhausted" | lcsi_stress4 == "no_exhausted" ~ "1", 
                                                                                                                                                   TRUE ~ "0"), lcsi_stress = dplyr::case_when(lcsi_stress_yes == 
                                                                                                                                                                                                 "1" | lcsi_stress_exhaust == "1" ~ "1", TRUE ~ "0"), 
                             lcsi_crisis_yes = dplyr::case_when(lcsi_crisis1 == "yes" | 
                                                                  lcsi_crisis2 == "yes" | lcsi_crisis3 == "yes" ~ 
                                                                  "1", TRUE ~ "0"), lcsi_crisis_exhaust = dplyr::case_when(lcsi_crisis1 == 
                                                                                                                             "no_exhausted" | lcsi_crisis2 == "no_exhausted" | 
                                                                                                                             lcsi_crisis3 == "no_exhausted" ~ "1", TRUE ~ "0"), 
                             lcsi_crisis = dplyr::case_when(lcsi_crisis_yes == "1" | 
                                                              lcsi_crisis_exhaust == "1" ~ "1", TRUE ~ "0"), lcsi_emergency_yes = dplyr::case_when(lcsi_emergency1 == 
                                                                                                                                                     "yes" | lcsi_emergency2 == "yes" | lcsi_emergency3 == 
                                                                                                                                                     "yes" ~ "1", TRUE ~ "0"), lcsi_emergency_exhaust = dplyr::case_when(lcsi_emergency1 == 
                                                                                                                                                                                                                           "no_exhausted" | lcsi_emergency2 == "no_exhausted" | 
                                                                                                                                                                                                                           lcsi_emergency3 == "no_exhausted" ~ "1", TRUE ~ 
                                                                                                                                                                                                                           "0"), lcsi_emergency = dplyr::case_when(lcsi_emergency_yes == 
                                                                                                                                                                                                                                                                     "1" | lcsi_emergency_exhaust == "1" ~ "1", TRUE ~ 
                                                                                                                                                                                                                                                                     "0"), lcsi_cat_yes = dplyr::case_when(lcsi_stress_yes != 
                                                                                                                                                                                                                                                                                                             "1" & lcsi_crisis_yes != "1" & lcsi_emergency_yes != 
                                                                                                                                                                                                                                                                                                             "1" ~ "None", lcsi_stress_yes == "1" & lcsi_crisis_yes != 
                                                                                                                                                                                                                                                                                                             "1" & lcsi_emergency_yes != "1" ~ "Stress", lcsi_crisis_yes == 
                                                                                                                                                                                                                                                                                                             "1" & lcsi_emergency_yes != "1" ~ "Crisis", lcsi_emergency_yes == 
                                                                                                                                                                                                                                                                                                             "1" ~ "Emergency", TRUE ~ NA_character_), lcsi_cat_exhaust = dplyr::case_when(lcsi_stress_exhaust != 
                                                                                                                                                                                                                                                                                                                                                                                             "1" & lcsi_crisis_exhaust != "1" & lcsi_emergency_exhaust != 
                                                                                                                                                                                                                                                                                                                                                                                             "1" ~ "None", lcsi_stress_exhaust == "1" & lcsi_crisis_exhaust != 
                                                                                                                                                                                                                                                                                                                                                                                             "1" & lcsi_emergency_exhaust != "1" ~ "Stress", 
                                                                                                                                                                                                                                                                                                                                                                                           lcsi_crisis_exhaust == "1" & lcsi_emergency_exhaust != 
                                                                                                                                                                                                                                                                                                                                                                                             "1" ~ "Crisis", lcsi_emergency_exhaust == "1" ~ 
                                                                                                                                                                                                                                                                                                                                                                                             "Emergency", TRUE ~ NA_character_), lcsi_cat = dplyr::case_when(lcsi_stress != 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1" & lcsi_crisis != "1" & lcsi_emergency != "1" ~ 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               "None", lcsi_stress == "1" & lcsi_crisis != "1" & 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               lcsi_emergency != "1" ~ "Stress", lcsi_crisis == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1" & lcsi_emergency != "1" ~ "Crisis", lcsi_emergency == 
                                                                                                                                                                                                                                                                                                                                                                                                                                                               "1" ~ "Emergency", TRUE ~ NA_character_))
  if (ignore_NA == FALSE) {
    which_na <- df %>% dplyr::select(lcsi_stress1:lcsi_emergency3) %>% 
      is.na() %>% rowSums() %>% as.logical()
    lcsi_added_cols <- c("lcsi_stress_yes", "lcsi_stress_exhaust", 
                         "lcsi_stress", "lcsi_crisis_yes", "lcsi_crisis_exhaust", 
                         "lcsi_crisis", "lcsi_emergency_yes", "lcsi_emergency_exhaust", 
                         "lcsi_emergency", "lcsi_cat_yes", "lcsi_cat_exhaust", 
                         "lcsi_cat")
    df[which_na, lcsi_added_cols] <- NA
  }
  return(df)
}
