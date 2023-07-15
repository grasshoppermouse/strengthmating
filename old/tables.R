mm1_exact <- svyglm(
  sex_partners ~
    age_centered + #* sex + #include interaction since we include sex?
    partnered +
    sex * strength_centered + #include interaction since we include sex?
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

mm2_exact <- svyglm(
  sex_partners_year ~
    sex * strength_centered +
    partnered*strength_centered +
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

mm3_exact <- svyglm(
  age_first_sex ~
    age_centered +
    partnered +
    edu +
    total_work_MET +
    sex*strength_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
)


mm1 <- tbl_regression(mm1_exact,
                      label = list(
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


mm2 <- tbl_regression(mm2_exact,
                      label = list(
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

mm3 <- tbl_regression(mm3_exact,
                      label = list(
                        age_centered = "Age",
                        sex = "Sex",
                        strength_centered = "Grip strength",
                        partnered = "Partnered",
                        edu = "Education",
                        total_work_MET = "Occupational exertion"
                      )) %>%
  add_significance_stars(
    hide_se = TRUE,
    pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Beta (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(mm1, mm2, mm3),
  tab_spanner = c("Lifetime number of sexual partners",
                  "Past year number of sexual partners",
                  "Age at first sexual intercourse")) %>%
  as_gt() %>%
  gt::tab_header(title = "Table 2. Exact Models of Mating Success",
                 subtitle = "Standardized regression coefficents from generalized
                 linear models for three indices of mating success for men and women ages
                 18-59. Models derived from Lassek and Gaulin 2009")
