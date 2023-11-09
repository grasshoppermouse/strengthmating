designsG$d.design.adults %>%
  tbl_svysummary(by = sex, include = c(age, edu, maritalstatus, income, race, bmi,
                                       whitebloodcell, strength, testosterone, partnered,
                                       height, weight, special_equipment))



d_G[d_G$age>=18 & d_G$age<=60,]  %>% select("sex", "age", "race", "strength", "bmi", "partnered", "edu", "height", "weight", "whitebloodcell", "testosterone") %>%
  tbl_summary(by = sex,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              digits = all_continuous() ~ 2,
        #      type = list(where(is.numeric) ~ "continuous"),
             # type = list(partnered ~ "categorical"),
              type = all_dichotomous() ~ "categorical",
              label  = list(
                age = "Age (Years)",
                race = "Race and Ethnicity",
                strength = "Combined Grip Strength",
                bmi = "BMI (kg/m^2)",
                partnered = "Partnered Status",
                edu = "Education",
                height = "Height (cm)",
                weight = "Weight (kg)",
                whitebloodcell = "WBCC (1000 cells/µL)",
                testosterone = "Testosterone (ng/dL)"),
              missing_text = "Missing"
  ) %>% add_p() %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Sex**")



