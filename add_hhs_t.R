add_hhs_t <- function (.dataset, hhs_nofoodhh_1 = "fs_hhs_nofood_yn", hhs_nofoodhh_1a = "fs_hhs_nofood_freq", 
          hhs_sleephungry_2 = "fs_hhs_sleephungry_yn", hhs_sleephungry_2a = "fs_hhs_sleephungry_freq", 
          hhs_alldaynight_3 = "fs_hhs_daynoteating_yn", hhs_alldaynight_3a = "fs_hhs_daynoteating_freq", 
          yes_answer = "yes", no_answer = "no", rarely_answer = "rarely_1_2", sometimes_answer = "sometimes_3_10", 
          often_answer = "often_10_times",dnk_answer= "dnk", pnta_answer = "pnta")
{
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  tryCatch({
    .dataset %>% dplyr::select(dplyr::all_of(c(hhs_nofoodhh_1, 
                                               hhs_nofoodhh_1a, hhs_sleephungry_2, hhs_sleephungry_2a, 
                                               hhs_alldaynight_3, hhs_alldaynight_3a)))
  }, error = function(e) {
    message("Missing hhs columns")
  })
  if (!all(.dataset[[hhs_nofoodhh_1]] %in% c(yes_answer, no_answer, dnk_answer, pnta_answer, 
                                             NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_nofoodhh_1, 
                 paste0(unique(.dataset[[hhs_nofoodhh_1]][!.dataset[[hhs_nofoodhh_1]] %in% 
                 c(yes_answer, no_answer , dnk_answer, pnta_answer, NA )]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_sleephungry_2]] %in% c(yes_answer, 
                                                no_answer, dnk_answer, pnta_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_sleephungry_2, 
                 paste0(unique(.dataset[[hhs_sleephungry_2]][!.dataset[[hhs_sleephungry_2]] %in% 
                 c(yes_answer, no_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_alldaynight_3]] %in% c(yes_answer, 
                                                no_answer, dnk_answer, pnta_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_alldaynight_3, 
                 paste0(unique(.dataset[[hhs_alldaynight_3]][!.dataset[[hhs_alldaynight_3]] %in% 
                                                               c(yes_answer, no_answer, NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_nofoodhh_1a]] %in% c(rarely_answer, 
                                              sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_nofoodhh_1a, 
                 paste0(unique(.dataset[[hhs_nofoodhh_1a]][!.dataset[[hhs_nofoodhh_1a]] %in% 
                                                             c(rarely_answer, sometimes_answer, often_answer, 
                                                               NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_sleephungry_2a]] %in% c(rarely_answer, 
                                                 sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_sleephungry_2a, 
                 paste0(unique(.dataset[[hhs_sleephungry_2a]][!.dataset[[hhs_sleephungry_2a]] %in% 
                                                                c(rarely_answer, sometimes_answer, often_answer, 
                                                                  NA)]), collapse = "/")))
  }
  if (!all(.dataset[[hhs_alldaynight_3a]] %in% c(rarely_answer, 
                                                 sometimes_answer, often_answer, NA))) {
    stop(sprintf("Wrong values in %s: %s ", hhs_alldaynight_3a, 
                 paste0(unique(.dataset[[hhs_alldaynight_3a]][!.dataset[[hhs_alldaynight_3a]] %in% 
                                                                c(rarely_answer, sometimes_answer, often_answer, 
                                                                  NA)]), collapse = "/")))
  }
  
  .dataset_with_calculation <- .dataset %>% dplyr::mutate_at(c(hhs_nofoodhh_1, 
                                                              hhs_sleephungry_2, hhs_alldaynight_3),
                                                              ~dplyr::case_when(.x == yes_answer ~ 1, .x == no_answer ~ 0)) %>% 
                                                              dplyr::mutate_at(c(hhs_nofoodhh_1a, 
                                                              hhs_sleephungry_2a, hhs_alldaynight_3a), ~dplyr::case_when(.x %in% 
                                                              c(rarely_answer, sometimes_answer) ~ 1, .x == often_answer ~ 
                                                              2, TRUE ~ 0)) %>% dplyr::rowwise() %>% dplyr::mutate(hhs_comp1 = !!rlang::sym(hhs_nofoodhh_1) * 
                                                              !!rlang::sym(hhs_nofoodhh_1a), hhs_comp2 = !!rlang::sym(hhs_sleephungry_2) * 
                                                              !!rlang::sym(hhs_sleephungry_2a), hhs_comp3 = !!rlang::sym(hhs_alldaynight_3) * 
                                                              !!rlang::sym(hhs_alldaynight_3a), ) %>% dplyr::ungroup() %>% 
                                                              dplyr::mutate(hhs_score = rowSums(.[grep("^hhs_comp\\d$", 
                                                              names(.))])) %>% dplyr::mutate(hhs_cat_ipc = dplyr::case_when(hhs_score == 
                                                              0 ~ "None", hhs_score == 1 ~ "Little", hhs_score <= 
                                                              3 ~ "Moderate", hhs_score == 4 ~ "Severe", hhs_score <= 
                                                              6 ~ "Very Severe"), hhs_cat = dplyr::case_when(hhs_score <= 
                                                              1 ~ "No or Little", hhs_score <= 3 ~ "Moderate", hhs_score <= 
                                                              6 ~ "Severe", TRUE ~ NA_character_))
  
  columns_to_export <- .dataset_with_calculation %>% dplyr::rename_at(c(hhs_nofoodhh_1, 
                                                                        hhs_nofoodhh_1a, hhs_sleephungry_2, hhs_sleephungry_2a, 
                                                                        hhs_alldaynight_3, hhs_alldaynight_3a), ~paste0(.x, 
                                                                        "_recoded")) %>% dplyr::select(paste0(hhs_nofoodhh_1, 
                                                                        "_recoded"), paste0(hhs_nofoodhh_1a, "_recoded"), paste0(hhs_sleephungry_2, 
                                                                        "_recoded"), paste0(hhs_sleephungry_2a, "_recoded"), 
                                                                        paste0(hhs_alldaynight_3, "_recoded"), paste0(hhs_alldaynight_3a, 
                                                                        "_recoded"), hhs_comp1, hhs_comp2, hhs_comp3, hhs_score, hhs_cat_ipc, hhs_cat)
  .dataset <- .dataset %>% cbind(columns_to_export)
  return(.dataset)
}
