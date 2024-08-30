# https://impact-initiatives.github.io/addindicators/
install.packages("writexl")
install.packages("readxl")
library(readxl)
library(writexl)
library(addindicators)

file_path="C:/Users/test/Downloads/20240826_ETH2403_eth_msna_cleaned_data_NEW.xlsx"
print(file_path)
sheet_names <- excel_sheets(file_path)
dataframes1 <- list()
for (sheet in sheet_names) {
  dataframes1[[sheet]] <- read_excel(file_path, sheet = sheet)}
print(sheet_names)

df <- dataframes1[["cleaned_data"]]
#View(df)

# Example:: Add Food Consumption Score (FCS)

df_with_fcs <- df %>% add_fcs(
  cutoffs = "normal",
  fsl_fcs_cereal = "fsl_fcs_cereal",
  fsl_fcs_legumes = "fsl_fcs_legumes",
  fsl_fcs_veg = "fsl_fcs_veg",
  fsl_fcs_fruit = "fsl_fcs_fruit",
  fsl_fcs_meat = "fsl_fcs_meat",
  fsl_fcs_dairy = "fsl_fcs_dairy",
  fsl_fcs_sugar = "fsl_fcs_sugar",
  fsl_fcs_oil = "fsl_fcs_oil")

#View(df_with_fcs)

df_with_fcs %>%
  dplyr::select(
    `_uuid`,  fsl_fcs_score, fsl_fcs_cat, fcs_weight_cereal1, fcs_weight_legume2,
    fcs_weight_dairy3, fcs_weight_meat4, fcs_weight_veg5,
    fcs_weight_fruit6, fcs_weight_oil7, fcs_weight_sugar8
  ) %>%
  head(20)

#View(x)
# Example:: Add Household Hunger Scale (HHS)
source("add_hhs_t.R")

df_with_hhs <- df_with_fcs %>% add_hhs_t(
  hhs_nofoodhh_1 = "fsl_hhs_nofoodh",
  hhs_nofoodhh_1a = "fsl_hhs_nofoodhh_freq",
  hhs_sleephungry_2 = "fsl_hhs_sleephungry",
  hhs_sleephungry_2a = "fsl_hhs_sleephungry_freq",
  hhs_alldaynight_3 = "fsl_hhs_alldaynight",
  hhs_alldaynight_3a = "fsl_hhs_alldaynight_freq",
  yes_answer = "yes",
  no_answer = "no",
  dnk_answer= "dnk",
  pnta_answer = "pnta",
  rarely_answer = "rarely",
  sometimes_answer = "sometimes",
  often_answer = "often")
#View(df_with_hhs)

df_with_hhs %>%
  dplyr::select(
    `_uuid`, hhs_comp1, hhs_comp2, hhs_comp3,
    hhs_score, hhs_cat_ipc, hhs_cat, hh_size
  ) %>%
  head(20)


# Example:: Add Livelihood Coping Strategy score (LCSI)
source("add_lcsi_t.R")

df_with_lcsi <- df_with_hhs %>% add_lcsi_t(
  lcsi_stress_vars = c("fsl_lcsi_en_stress1", "fsl_lcsi_en_stress2", "fsl_lcsi_en_stress3", "fsl_lcsi_en_stress4"), #fsl_lcsi_en_stress5, fsl_lcsi_en_stress6
  lcsi_crisis_vars = c("fsl_lcsi_en_crisis1", "fsl_lcsi_en_crisis2", "fsl_lcsi_en_crisis3"), # fsl_lcsi_en_crisis4, fsl_lcsi_en_crisis5
  lcsi_emergency_vars = c("fsl_lcsi_en_emergency1", "fsl_lcsi_en_emergency2", "fsl_lcsi_en_emergency3"), # fsl_lcsi_en_emergency4
  yes_val = "yes",
  no_val = "no_had_no_need",
  exhausted_val = "no_exhausted",
  not_applicable_val = "not_applicable"
)

df_with_lcsi %>%
  dplyr::select(`_uuid`, lcsi_cat, lcsi_cat_exhaust, lcsi_cat_yes) %>%
  head(20)


# Example:: Add Reduced Household Coping Strategy score (rCSI)

df_with_rcsi <- df_with_lcsi %>% add_rcsi(
  rCSILessQlty = "rCSILessQlty",
  rCSIBorrow = "rCSIBorrow",
  rCSIMealSize = "rCSIMealSize",
  rCSIMealAdult = "rCSIMealAdult",
  rCSIMealNb = "rCSIMealNb",
  new_colname = "rcsi"
)

## Variable name for rcsi score is rcsi_score

## Variable name for rcsi category is rcsi_cat

df_with_rcsi %>%
  dplyr::select(uuid, rcsi_score, rcsi_cat) %>%
  head(20)

# Example:: Add Food Consumption Matrix (FCM)

df_with_fcm <- df_with_rcsi %>%
  add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    rcsi_column_name = "rcsi_cat",
    hhs_column_name = "hhs_cat_ipc",
    fcs_categories_acceptable = "Acceptable",
    fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    rcsi_categories_low = "No to Low",
    rcsi_categories_medium = "Medium",
    rcsi_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  )

df_with_fcm %>%
  dplyr::select(uuid, fc_cell, fc_phase) %>%
  head(20)


# Example:: Add FEWSNET Food Consumption-Livelihood Matrix (FCLCM)

df_with_fclcm <- df_with_fcm %>% ## Taken from previous Example
  add_fclcm_phase()
df_with_fclcm %>%
  dplyr::select(uuid, fclcm_phase) %>%
  head(20)

# Example:: Review of indicators

# The logic behind review_variables is to compare the results from 2 codes to create the composite variable.
# 
# In this example, the Food Consumption Score from the first example will be compared.

review_df <- addindicators_MSNA_template_data %>% add_fcs(
  cutoffs = "normal",
  fsl_fcs_cereal = "fs_fcs_cereals_grains_roots_tubers",
  fsl_fcs_legumes = "fs_fcs_beans_nuts",
  fsl_fcs_veg = "fs_fcs_vegetables_leaves",
  fsl_fcs_fruit = "fs_fcs_fruit",
  fsl_fcs_meat = "fs_fcs_meat_fish_eggs",
  fsl_fcs_dairy = "fs_fcs_dairy",
  fsl_fcs_sugar = "fs_fcs_sugar",
  fsl_fcs_oil = "fs_fcs_oil_fat_butter"
)

# The new results and the results to be reviewed are bound together by the uuid.

binded_df <- df_with_fcs %>%
  dplyr::full_join(review_df, by = "uuid")

# There are 2 functions to review: - review_one_variable, to review only one variable - review_variables, a wrapper around review_one_variable to be able to review several variables.
# 
# review_one_variable

review_one_variable <- review_one_variable(binded_df,
                                           column_to_review = "fsl_fcs_cat.x",
                                           column_to_compare_with = "fsl_fcs_cat.y")

review_one_variable$review_check_fsl_fcs_cat.x %>% mean()

## [1] 1

review_one_variable$review_comment_fsl_fcs_cat.x %>% table(useNA = "ifany")

## .
## Same results 
##          100

# review_variables

review_results <- review_variables(binded_df,
                                   columns_to_review = c("fsl_fcs_score.x", "fsl_fcs_cat.x"),
                                   columns_to_compare_with = c("fsl_fcs_score.y", "fsl_fcs_cat.y")
)

review_results$review_table %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(prop_correction = mean(review_check))

## # A tibble: 2 × 2
##   variable        prop_correction
##   <chr>                     <dbl>
## 1 fsl_fcs_cat.x                 1
## 2 fsl_fcs_score.x               1

review_results$review_table %>%
  dplyr::group_by(variable, review_comment) %>%
  dplyr::tally(sort = T)

## # A tibble: 2 × 3
## # Groups:   variable [2]
##   variable        review_comment     n
##   <chr>           <glue>         <int>
## 1 fsl_fcs_cat.x   Same results     100
## 2 fsl_fcs_score.x Same results     100

# Examples when differences exists

test_categorical <- data.frame(
  test = c(
    "test equality",
    "test difference",
    "test Missing in y",
    "test Missing in x",
    "test equality missing in both"
  ),
  var_x = c("A", "B", "C", NA, NA),
  var_y = c("A", "A", NA, "D", NA),
  uuid = letters[1:5]
)
review_one_variable(test_categorical,
                    column_to_review = "var_x",
                    column_to_compare_with = "var_y"
)

##   uuid review_check_var_x review_comment_var_x
## 1    a               TRUE         Same results
## 2    b              FALSE    Different results
## 3    c              FALSE     Missing in var_y
## 4    d              FALSE     Missing in var_x
## 5    e               TRUE         Same results