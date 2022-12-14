mm1_exact <- svyglm(
  sex_partners ~
    age_centered + #* sex + #include interaction since we include sex?
    partnered +
    sex * strength_centered + #include interaction since we include sex?
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
) %>%
  tbl_regression(label = list(
    age_centered = "Age",
    sex = "Sex",
    strength_centered = "Grip strength",
    partnered = "Partnered",
    bmi_centered = "BMI"
  )) %>%
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Beta (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


# original tables ---------------------------------------------------------

mm1_exact <- svyglm(
  sex_partners ~
    age_centered * sex +
    sex * strength_centered +
    partnered +
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
) %>%
  tbl_regression() %>%
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Beta (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

mm1_exact <- svyglm(
  sex_partners ~
    age_centered  * sex + #include interaction since we include sex?
    sex * strength_centered + #include interaction since we include sex?
    partnered +
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
) %>%
  # tbl_regression(
  #                pvalue_fun = ~style_pvalue(.x, digits = 2),
  # ) %>%
  #   bold_p(t = 0.05) %>%
  #   bold_labels() %>%
  #   modify_header(label ~ "**Variable**") %>%
  #   add_significance_stars(
  #     hide_se = TRUE,
  #     pattern = "{estimate}{stars}<br>({std.error})"
  #   ) %>%
  #   modify_header(estimate ~ "**Beta (SE)**") %>%
#   modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)
tbl_regression(label = list(
  age_centered = "Age",
  sex = "Sex",
  strength_centered = "Grip strength",
  partnered = "Partnered",
  bmi_centered = "BMI"
)) %>%
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Beta (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)



mm2_exact <- svyglm(
  sex_partners_year ~
    sex * strength_centered +
    partnered*strength_centered +
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
) %>%
  tbl_regression %>%
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(estimate ~ "**Beta (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


tbl_merge_mm_exact <-
  tbl_merge(
    tbls = list(mm1_exact, mm2_exact),
    tab_spanner = c("Lifetime number of sexual partners",
                    "Past year number of sexual partners")
  )


tbl_merge_mm_exact %>%
  as_gt() %>%
  gt::tab_header(title = "Table 1. Exact Models of Mating Success",
                 subtitle = "Standardized regression coefficents from generalized
                 linear models for three indicies of mating success. Models derived
                 from Lassek and Gaulin 2009")


mm1_mod <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered +
    partnered +
    #  bmi_centered*sex +
    #  edu +
    median_salary_current +
    age_first_sex*sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
) %>%
  tbl_regression(
    pvalue_fun = ~style_pvalue(.x, digits = 2),
  ) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_column_hide(ci)
