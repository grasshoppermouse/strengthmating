getn <- designsG$d.design.adults %>%
  tbl_svysummary(
    by = sex,
    include = c(age,
                height,
                weight,
                bmi,
                strength,
                whitebloodcell,
                hemoglobin,
                testosterone),
    statistic = list(
      all_continuous() ~ "{N_nonmiss_unweighted}"
    ),
    digits = all_continuous() ~ 0,
    missing = "no"
  )

getratio <- designsG$d.design.adults %>%
  tbl_svysummary(
    by = sex,
    include = c(age,
                height,
                weight,
                bmi,
                strength,
                whitebloodcell,
                hemoglobin,
                testosterone),
    statistic = list(
      all_continuous() ~ "{mean}"
    ),
    missing = "no"
  ) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(ratio = round(as.numeric(stat_1)/as.numeric(stat_2), digits = 2))
  )



tab1 <- designsG$d.design.adults %>%
  tbl_svysummary(
    by = sex,
    include = c(
      age,
      height,
      weight,
      bmi,
      strength,
      whitebloodcell,
      hemoglobin,
      testosterone
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 2,
    label  = list(
      strength = "Combined Grip Strength (kg)",
      height = "Height (cm)",
      weight = "Weight (kg)",
      age = "Age (years)",
      bmi = "Body mass index (kg/m^2)",
      whitebloodcell = "White blood cell count (1000 cells/µL)",
      hemoglobin = "Hemoglobin (g/dL)",
      testosterone = "Testosterone (ng/dL)"
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
  add_difference(test = everything() ~ "smd",
                 estimate_fun = purrr::partial(style_ratio, digits = 2)) %>%
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
  modify_caption("**Table 1. Sex differences for participants ages 18-59 using population weights**") %>%
  bold_labels()



