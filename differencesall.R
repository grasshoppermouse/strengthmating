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

getratio <- designsG$d.design.adults %>%
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
      all_continuous() ~ "{mean}",
      all_categorical() ~ "{p}"
    ),
    type = list(
      chronic_disease_score ~ "continuous",
      physical_disease_count ~ "continuous",
      avgcalories ~ "continuous"
    ),
    missing = "no"
  ) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(ratio = round(as.numeric(stat_1)/as.numeric(stat_2), digits = 2))
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
      dplyr::mutate(ratio = getratio$table_body$ratio, digits = 2) %>%
      dplyr::mutate(n_male = getn$table_body$stat_1, digits = 2) %>%
      dplyr::mutate(n_female = getn$table_body$stat_2, digits = 2) %>%
      dplyr::relocate(n_male, .before = stat_1) %>%
      dplyr::relocate(n_female, .before = stat_2)
  )  %>%
  add_difference(test = everything() ~ "smd", ,
                 estimate_fun = purrr::partial(style_ratio, digits = 2)),
) %>%
  modify_column_hide(columns = ci) %>%
  modify_header(
    label = "**Variable**",
    ratio = "**Ratio**",
    n_male = "**N**",
    n_female = "**N**",
    stat_1 = "**Mean (SD)**",
    stat_2 = "**Mean (SD)**",
    estimate = "*d*"
  ) %>%
  modify_spanning_header(list(
    c("stat_1", "n_male") ~ "**Male**",
    c("stat_2", "n_female") ~ "**Female**",
    c("ratio", "estimate") ~ "**Sexual Dimorphism**"
  )) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_caption("**Sex differences for participants ages 18-59 using population weights**") %>%
  bold_labels()
