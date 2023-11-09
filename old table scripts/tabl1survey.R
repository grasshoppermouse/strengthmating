# has ratio and cohen's d but not sds

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



#get sds

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
   modify_table_body(~.x %>% dplyr::relocate(sd2, .after = stat_2))

# #has sds and ratio but not cohen's d
#
# sd_gts <- function(data, variable, ...) {
#   sd(data[[variable]], na.rm = TRUE)
# }
#
#
# tabbsvy2 <- designsG$d.design.adults %>%
#   tbl_strata(
#     strata = sex,
#     ~ .x %>%
#       tbl_svysummary(include = c(age,
#                                  height,
#                                  weight,
#                                  bmi,
#                                  strength,
#                                  whitebloodcell,
#                                  hemoglobin,
#                                  testosterone),
#         statistic = list(all_continuous() ~ "{mean}"),
#         digits = all_continuous() ~ 2,
#         label  = list(
#           strength = "Combined Grip Strength (kg)",
#           height = "Height (cm)",
#           weight = "Weight (kg)",
#           age = "Age (years)",
#           bmi = "Body mass index (kg/m^2)",
#           whitebloodcell = "White blood cell count (1000 cells/µL)",
#           hemoglobin = "Hemoglobin (g/dL)",
#           testosterone = "Testosterone (ng/dL)"
#         ),
#         missing = "no"
#       ) %>%
#       add_n() %>%
#       add_stat(fns = everything() ~ sd_gts)) %>%
#   modify_header(list(stat_0_1 ~ "**Mean**",
#                      stat_0_2 ~ "**Mean**",
#                      add_stat_1_1 ~ "**SD**",
#                      add_stat_1_2 ~ "**SD**")) %>%
#   modify_table_body(
#     ~ .x %>%
#       dplyr::mutate(ratio = round(as.numeric(stat_0_1)/as.numeric(stat_0_2), digits = 2))
#   ) %>%
#   modify_header(ratio = "**Ratio**")
#
#
# # has all!
# tabb3 <- tabb2 %>% modify_table_body(
#   ~ .x %>%
#     dplyr::mutate(d = round(tabb$table_body$estimate, digits = 2))) %>%
#   modify_header(d = "**d**") %>%
#   modify_header(label ~ "**Variable**") %>%
#   modify_spanning_header(c("ratio", "d") ~ "**Sexual dimorphism**") %>%
#   modify_caption("**Table 1. Sex differences based on Lassek and Gaulin Table 1**")
