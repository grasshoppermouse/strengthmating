
# Table 1. sex differences using sample data -----------------------------


# has ratio and cohen's d but not sds
tabb <- d_G[d_G$age >= 18 &
              d_G$age <= 60, ]  %>% select(
                "sex",
                "age",
                "height",
                "weight",
                "bmi",
                "strength",
                "whitebloodcell",
                "hemoglobin",
                "testosterone",
              ) %>%
  tbl_summary(
    by = sex,
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
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(ratio = round(as.numeric(stat_1)/as.numeric(stat_2), digits = 2))
  ) %>%
  modify_header(ratio = "**Ratio**") %>%
  add_difference(everything() ~ "cohens_d")


#has sds and ratio but not cohen's d

sd_gts <- function(data, variable, ...) {
  sd(data[[variable]], na.rm = TRUE)
}


tabb2 <- d_G[d_G$age >= 18 &
               d_G$age <= 60, ]  %>% select(
                 "sex",
                 "age",
                 "height",
                 "weight",
                 "bmi",
                 "strength",
                 "whitebloodcell",
                 "hemoglobin",
                 "testosterone",
               ) %>%
  tbl_strata(
  strata = sex,
  ~ .x %>%
    tbl_summary(
 #     by = sex,
      statistic = list(all_continuous() ~ "{mean}"),
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
    add_stat(fns = everything() ~ sd_gts)) %>%
  # modify_fmt_fun(list(add_stat_1 ~ partial(style_number, digits = 2))) %>%
  modify_header(list(stat_0_1 ~ "**Mean**",
                     stat_0_2 ~ "**Mean**",
                     add_stat_1_1 ~ "**SD**",
                     add_stat_1_2 ~ "**SD**")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(ratio = round(as.numeric(stat_0_1)/as.numeric(stat_0_2), digits = 2))
  ) %>%
  modify_header(ratio = "**Ratio**")


# has all!
tabb3 <- tabb2 %>% modify_table_body(
  ~ .x %>%
    dplyr::mutate(d = round(tabb$table_body$estimate, digits = 2))) %>%
  modify_header(d = "**d**") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("ratio", "d") ~ "**Sexual dimorphism**") %>%
  modify_caption("**Table 1. Sex differences based on Lassek and Gaulin Table 1**")




# Table 1 sex differences using survey weights ----------------------------

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


# one million versions that did not work ----------------------------------

# tab <- d_G[d_G$age >= 18 &
#              d_G$age <= 60, ]  %>% select(
#                "sex",
#                "age",
#                "height",
#                "weight",
#                "bmi",
#                "strength",
#                "whitebloodcell",
#                "hemoglobin",
#                "testosterone",
#              ) %>%
#   tbl_summary(
#     by = sex,
#     statistic = list(
#       all_continuous() ~ "{mean}",
#       all_categorical() ~ "{n} / {N} ({p}%)"
#     ),
#     digits = all_continuous() ~ 2,
#     label  = list(
#       strength = "Combined Grip Strength (kg)",
#       height = "Height (cm)",
#       weight = "Weight (kg)",
#       age = "Age (years)",
#       bmi = "Body mass index (kg/m^2)",
#       whitebloodcell = "White blood cell count (1000 cells/µL)",
#       hemoglobin = "Hemoglobin (g/dL)",
#       testosterone = "Testosterone (ng/dL)"
#     ),
#     missing = "no"
#   ) %>% add_overall() %>%
#   add_n() %>%
#   modify_header(label ~ "**Variable**") %>%
#   modify_spanning_header(c("stat_1", "stat_2") ~ "**Sex**") %>%
#   modify_caption("**Table 1. Participant Characteristics**") %>%
#   bold_labels()
#
#
# table <- add_difference(tab, list(all_continuous() ~ "cohens_d"))
#
#
#
# d_Gadults <- d_G[d_G$age >= 18 & d_G$age <= 60,]
# d_Gadults$sex2 = ifelse(d_G$adults$sex == "male", 1, 0)
#
#
#
# ratios <- as.tibble(tab$table_body %>%
#                       select(
#                         "stat_1",
#                         "stat_2"
#                       ))
#
# ratios$male_means <- as.numeric(unlist(ratios$stat_1))
# ratios$female_means <- as.numeric(unlist(ratios$stat_2))
#
# ratios$ratio <- round(ratios$male_means/ratios$female_means, digits = 2)
#
#
# add_ratio<-
#   table %>%
#   modify_table_body(~.x %>%
#                       ratios$ratio) %>%
#   modify_header(add_stat_1 ~ "**Ratio**")
#
# ######
#
# tabb <- d_G[d_G$age >= 18 &
#               d_G$age <= 60, ]  %>% select(
#                 "sex",
#                 "age",
#                 "height",
#                 "weight",
#                 "bmi",
#                 "strength",
#                 "whitebloodcell",
#                 "hemoglobin",
#                 "testosterone",
#               ) %>%
#   tbl_summary(
#     by = sex,
#     statistic = list(
#       all_continuous() ~ "{mean}"
#     ),
#     digits = all_continuous() ~ 2,
#     label  = list(
#       strength = "Combined Grip Strength (kg)",
#       height = "Height (cm)",
#       weight = "Weight (kg)",
#       age = "Age (years)",
#       bmi = "Body mass index (kg/m^2)",
#       whitebloodcell = "White blood cell count (1000 cells/µL)",
#       hemoglobin = "Hemoglobin (g/dL)",
#       testosterone = "Testosterone (ng/dL)"
#     ),
#     missing = "no"
#   ) %>%
#   modify_table_body(
#     ~ .x %>%
#       dplyr::mutate(ratio = round(as.numeric(stat_1)/as.numeric(stat_2), digits = 2))
#   ) %>%
#   modify_header(ratio = "**Ratio**") %>%
#   add_difference(everything() ~ "cohens_d"
#   )
#
#
#
# add_overall() %>%
#   add_n() %>%
#   modify_header(label ~ "**Variable**") %>%
#   modify_spanning_header(c("stat_1", "stat_2") ~ "**Sex**") %>%
#   modify_caption("**Table 1. Participant Characteristics**") %>%
#   bold_labels()
#
#
# table <- add_difference(tab, list(all_continuous() ~ "cohens_d"))
#
#
# #######
#
# sd_gts <- function(data, variable, ...) {
#   sd(data[[variable]], na.rm = TRUE)
# }
#
# tab <- d_G[d_G$age >= 18 &
#              d_G$age <= 60,]  %>% select(
#                "sex",
#                "age",
#                "height",
#                "weight",
#                "bmi",
#                "strength",
#                "whitebloodcell",
#                "hemoglobin",
#                "testosterone",
#              ) %>%
#   tbl_strata(
#     strata = sex,
#     ~.x %>%
#       tbl_summary(
#         by = sex,
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
#       )  %>%
#       add_stat(fns = everything() ~ sd_gts)) %>%
#   # modify_fmt_fun(list(add_stat_1 ~ partial(style_number, digits = 2))) %>%
#   modify_header(list(stat_0_1 ~ "**Mean**",
#                      stat_0_2 ~ "**Mean**",
#                      add_stat_1_1 ~ "**SD**",
#                      add_stat_1_2 ~ "**SD**")) %>%
#   modify_table_body(
#     ~ .x %>%
#       dplyr::mutate(ratio = round(as.numeric(stat_0_1)/as.numeric(stat_0_2), digits = 2))
#   ) %>%
#   modify_header(ratio = "**Ratio**") # %>%
#
# table <- add_difference(tab, list(all_continuous() ~ "cohens_d"))

######

# d_G[d_G$age >= 18 &
#       d_G$age <= 60,]  %>% select(
#         "sex",
#         "age",
#         "height",
#         "weight",
#         "bmi",
#         "strength",
#         "whitebloodcell",
#         "hemoglobin",
#         "testosterone",
#       ) %>%
#   tbl_summary(
#     by = sex,
#     statistic = list(all_continuous() ~ "{mean}"),
#     digits = all_continuous() ~ 2,
#     label  = list(
#       strength = "Combined Grip Strength (kg)",
#       height = "Height (cm)",
#       weight = "Weight (kg)",
#       age = "Age (years)",
#       bmi = "Body mass index (kg/m^2)",
#       whitebloodcell = "White blood cell count (1000 cells/µL)",
#       hemoglobin = "Hemoglobin (g/dL)",
#       testosterone = "Testosterone (ng/dL)"
#     ),
#     missing = "no"
#   ) %>% add_difference(everything() ~ "cohens_d"
#   )

#######

# tab <- d_G[d_G$age >= 18 &
#              d_G$age <= 60, ]  %>% select(
#                "sex",
#                "age",
#                "height",
#                "weight",
#                "bmi",
#                "strength",
#                "whitebloodcell",
#                "hemoglobin",
#                "testosterone",
#              ) %>%
#   tbl_strata(
#     strata = sex,
#     ~ .x %>%
#       tbl_summary(
#         #  by = sex,
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
#       add_stat(fns = everything() ~ sd_gts)
#   ) %>%
#   # modify_fmt_fun(list(add_stat_1 ~ partial(style_number, digits = 2))) %>%
#   modify_header(
#     list(
#       stat_0_1 ~ "**Mean**",
#       stat_0_2 ~ "**Mean**",
#       add_stat_1_1 ~ "**SD**",
#       add_stat_1_2 ~ "**SD**"
#     )
#   ) %>%
#   modify_table_body(~ .x %>%
#                       dplyr::mutate(ratio = round(
#                         as.numeric(stat_0_1) / as.numeric(stat_0_2), digits = 2
#                       ))) %>%
#   modify_header(ratio = "**Ratio**")
#
# table <- add_difference(tab, list(all_continuous() ~ "cohens_d"))

########

# tab <- d_G[d_G$age >= 18 &
#              d_G$age <= 60, ]  %>% select(
#                "sex",
#                "age",
#                "height",
#                "weight",
#                "bmi",
#                "strength",
#                "whitebloodcell",
#                "hemoglobin",
#                "testosterone",
#              ) %>%
#   tbl_strata(
#     strata = sex,
#     ~ .x %>%
#       tbl_summary(
#         by = sex,
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
#       add_stat(fns = everything() ~ sd_gts) %>%
#       add_difference(everything() ~ "cohens_d")
#   ) %>%
#   # modify_fmt_fun(list(add_stat_1 ~ partial(style_number, digits = 2))) %>%
#   modify_header(
#     list(
#       stat_0_1 ~ "**Mean**",
#       stat_0_2 ~ "**Mean**",
#       add_stat_1_1 ~ "**SD**",
#       add_stat_1_2 ~ "**SD**"
#     )
#   ) %>%
#   modify_table_body(~ .x %>%
#                       dplyr::mutate(ratio = round(
#                         as.numeric(stat_0_1) / as.numeric(stat_0_2), digits = 2
#                       ))) %>%
#   modify_header(ratio = "**Ratio**")
