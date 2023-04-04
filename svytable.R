#figure out how to add ns for male and female

tabbsvy <- designsG$d.design.adults %>%
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
  add_n() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(ratio = round(as.numeric(stat_1)/as.numeric(stat_2), digits = 2))
  ) %>%
  modify_header(ratio = "**Ratio**") %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(columns = ci)

tabbsvysd <- designsG$d.design.adults %>%
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
      all_continuous() ~ "{sd}"
    ),
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
  )

tabb3svy <- tabbsvy %>% modify_table_body(
  ~ .x %>%
    dplyr::mutate(sd1 = tabbsvysd$table_body$stat_1)
) %>% modify_table_body(
  ~ .x %>%
    dplyr::mutate(sd2 = tabbsvysd$table_body$stat_2)
) %>%
  modify_header(list(sd1 = "**SD**",
                     sd2 = "**SD**")) %>%
  modify_table_body(~.x %>% dplyr::relocate(sd1, .after = stat_1)) %>%
  modify_table_body(~.x %>% dplyr::relocate(sd2, .after = stat_2)) %>%
  modify_spanning_header(c("stat_1", "sd1") ~ "**Male**") %>%
  modify_spanning_header(c("stat_2", "sd2") ~ "**Female**") %>%
  modify_caption("**Table 1. Sex differences based on Lassek and Gaulin Table 1 for ages 18-59 using population weights**")
