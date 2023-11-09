
getn <- designsG$d.design.adults %>%
  tbl_svysummary(
    by = sex,
    include = c(age_first_sex,
                sex_partners,
                sex_partners_year,
                strength,
                age,
                bmi,
                height,
                weight,
                partnered,
                edu,
                race,
                whitebloodcell,
                hemoglobin,
                testosterone,
                perceived_abnormal_weight,
                special_equipment,
                chronic_disease_score,
                physical_disease_count,
                depression,
                vigorous_work,
                moderate_work,
                vigorous_rec,
                moderate_rec,
                avgcalories),
    statistic = list(
      all_continuous() ~ "{N_nonmiss_unweighted}",
      all_categorical() ~ "{N_nonmiss_unweighted}"
    ),
    digits = all_continuous() ~ 0,
    type = list(
      chronic_disease_score ~ "continuous",
      physical_disease_count ~ "continuous"
    ),
    missing = "no"
  )

designsG$d.design.adults %>%
  tbl_svysummary(
    by = sex,
    include = c(
      age_first_sex,
      sex_partners,
      sex_partners_year,
      strength,
      age,
      bmi,
      height,
      weight,
      partnered,
      edu,
      race,
      whitebloodcell,
      hemoglobin,
      testosterone,
      perceived_abnormal_weight,
      special_equipment,
      chronic_disease_score,
      physical_disease_count,
      depression,
      vigorous_work,
      moderate_work,
      vigorous_rec,
      moderate_rec,
      avgcalories
    ),
    label  = list(
      age_first_sex = "Age at first sex (years)",
      sex_partners = "Lifetime number of sexual partners",
      sex_partners_year = "Past year number of sexual partners",
      strength = "Combined Grip Strength (kg)",
      age = "Age (Years)",
      bmi = "Body mass index (kg/m^2)",
      height = "Height (cm)",
      weight = "Weight (kg)",
      partnered = "Partnered",
      edu = "Education",
      race = "Race and Ethnicity",
      whitebloodcell = "White blood cell count (1000 cells/µL)",
      hemoglobin = "Hemoglobin (g/dL)",
      testosterone = "Testosterone (ng/dL)",
      perceived_abnormal_weight = "Perceived abnormal weight",
      special_equipment = "Special equipment needed to walk",
      chronic_disease_score = "Chronic Disease Score (0-6)",
      physical_disease_count = "Disease Impairment Score (0-5)",
      depression = "Depression Score (0-27)",
      vigorous_work = "Work involves vigorous activity",
      moderate_work = "Work involves moderate activity",
      vigorous_rec = "Recreation involves vigorous activity",
      moderate_rec = "Recreation involves moderate activity",
      avgcalories = "Dietary energy intake (kcals)"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n_unweighted} ({p}%)"),
    digits = all_continuous() ~ 2,
    type = list(
      chronic_disease_score ~ "continuous",
      physical_disease_count ~ "continuous"
    ),
    missing = "no"
  ) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(n_male = getn$table_body$stat_1, digits = 2) %>%
      dplyr::mutate(n_female = getn$table_body$stat_2, digits = 2) %>%
      dplyr::relocate(n_male, .before = stat_1) %>%
      dplyr::relocate(n_female, .before = stat_2)
  ) %>%
 # modify_column_hide(columns = ci) %>%
  modify_header(
    label = "**Variable**",
    n_male = "**N**",
    n_female = "**N**",
    stat_1 = "**Mean (SD)**",
    stat_2 = "**Mean (SD)**"
  ) %>%
  modify_spanning_header(list(
    c("stat_1", "n_male") ~ "**Male**",
    c("stat_2", "n_female") ~ "**Female**"
  )) %>%
 # add_n(statistic = "{N_nonmiss_unweighted}") %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_caption("Table 1. Participant Characteristics for all variables using population weights**") %>%
  bold_labels()

designsG$d.design.adults %>%
  tbl_svysummary(
    by = sex,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    type = list(
      chronic_disease_score ~ "continuous",
      physical_disease_count ~ "continuous"
    ),
    label  = list(
      age_first_sex = "Age at first sex (years)",
      sex_partners = "Lifetime number of sexual partners",
      sex_partners_year = "Past year number of sexual partners",
      strength = "Combined Grip Strength (kg)",
      age = "Age (Years)",
      bmi = "Body mass index (kg/m^2)",
      height = "Height (cm)",
      weight = "Weight (kg)",
      partnered = "Partnered",
      edu = "Education",
      race = "Race and Ethnicity",
      whitebloodcell = "White blood cell count (1000 cells/µL)",
      hemoglobin = "Hemoglobin (g/dL)",
      testosterone = "Testosterone (ng/dL)",
      perceived_abnormal_weight = "Perceived abnormal weight",
      special_equipment = "Special equipment needed to walk",
      chronic_disease_score = "Chronic Disease Score (0-6)",
      physical_disease_count = "Disease Impairment Score (0-5)",
      depression = "Depression Score (0-27)",
      vigorous_work = "Work involves vigorous activity",
      moderate_work = "Work involves moderate activity",
      vigorous_rec = "Recreation involves vigorous activity",
      moderate_rec = "Recreation involves moderate activity"
    ),
    missing = "no",
    include = c(
      "sex_partners",
      "sex_partners_year",
      "age_first_sex",
      "sex",
      "strength",
      "age",
      "partnered",
      "bmi",
      "edu",
      "race",
      "whitebloodcell",
      "hemoglobin",
      "depression",
      "chronic_disease_score",
      "physical_disease_count",
      "special_equipment",
      "perceived_abnormal_weight",
      "testosterone",
      "vigorous_work",
      "vigorous_rec",
      "moderate_work",
      "moderate_rec"
    )
  ) %>%
  # add_p() %>%
  # add_overall() %>%
  add_n(statistic = "{N_nonmiss_unweighted}") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Sex**") %>%
  modify_caption("**Table 1. Participant Characteristics for all variables using population weights**") %>%
  bold_labels()



# add_by_n <- function(data, variable, by, tbl, ...) {
#    data %>%
#    select(all_of(c(variable, by))) %>%
#     dplyr::group_by(.data[[by]]) %>%
#     dplyr::summarise_all(~sum(!is.na(.))) %>%
#     rlang::set_names(c("by", "variable")) %>%
#     dplyr::left_join(
#       tbl$df_by %>% select(by, by_col),
#       by = "by"
#     ) %>%
#     mutate(
#       by_col = paste0("add_n_", by_col),
#       variable = style_number(variable)
#     ) %>%
#     select(-by) %>%
#     tidyr::pivot_wider(names_from = by_col,
#                        values_from = variable)
# }

# confidence_intervals <- function(data, variable, by, ...) {
#
#   ## extract the confidence intervals and multiply to get percentages
#   props <- svyciprop(as.formula(paste0( "~" , variable)),
#                      data, na.rm = TRUE)
#
#   ## extract the confidence intervals
#   as.numeric(confint(props) * 100) %>% ## make numeric and multiply for percentage
#     round(., digits = 1) %>%           ## round to one digit
#     c(.) %>%                           ## extract the numbers from matrix
#     paste0(., collapse = "-")          ## combine to single character
# }


# tbl <- designsG$d.design.adults %>%
#   tbl_svysummary(
#     include = c(
#       age_first_sex,
#       sex_partners,
#       sex_partners_year),
#     by = sex) %>%
#   add_n() %>%
#   add_stat(
#     fns = everything() ~ add_by_n
#   )
