library(survey)
library(ggplot2)
library(nhanesGH)
library(hagenutils)
library(ggpubr)
library(tidyverse)
library(gtsummary)

# mating success models and tbl_regression objects using d.design.adults (NOT restricted to self-identified heterosexuals)


# original models (adults with complete data 18-59) -------------------------------------------------

# lifetime partners - baseline/anthropometric

m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# lifetime partners - socioeconomic
msoc1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered + #* strength_centered + #added strength interaction 03.23 to match past year since lifetime partners is a function of past year; took out 4.6
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# lifetime partners - health model
mheal1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

# lifetime partners - hormone
mhor1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# lifetime partners - phys activity
mphys1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

# past year partners - baseline/ anthropometric
m_pastyear <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for these models
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# past year partners - socioeconomic
msoc2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# past year partners - health
mheal2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

# past year partners - hormone
mhor2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# past year partners - phys activity
mphys2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


#age first sex - baseline/anthropometric
m_agefirst <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = gaussian(),
  design = designsG$d.design.adults
)

#age first sex - socioeconomic
msoc3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      race +
      edu,
    family = gaussian(),
    design = designsG$d.design.adults
  )

#age first sex - health
mheal3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = gaussian(),
    design = designsG$d.design.adults
  )

#age at first sex - hormone
mhor3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = gaussian(),
    design = designsG$d.design.adults
  )


#age at first sex - physical activity
mphys3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = gaussian(),
    design = designsG$d.design.adults
  )


#partnered status - baseline/anthropometric
m_partnered <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - socioeconomic
msoc4 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      edu +
      race,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - health
mheal4 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - hormone
mhor4 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - phys activity
mphys4 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )



# restrict to heterosexual  -----------------------------------------------

designsG$d.design.adult.heterosexual <-
  subset(
    designsG$d.design.adults,
    sexualorientation == 1
  )

# lifetime partners - anthropometric model

m_lifetime_h <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adult.heterosexual
)

# lifetime partners (socioeconomic model)
msoc1_h <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )


# lifetime partners (health model)
mheal1_h <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )


mhor1_h <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )


#phys activity
mphys1_h <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )


#past year
m_pastyear_h <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered + #keeping partnered x strength interaction only for this model
      bmi_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )

msoc2_h <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )


# past year partners (health model)
mheal2_h <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )


mhor2_h <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )

#phys activity past year
mphys2_h <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = designsG$d.design.adult.heterosexual
  )

#age first sex (anthropometric)

m_agefirst_h <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = gaussian(),
  design = designsG$d.design.adult.heterosexual
)

#age first sex (socioeconomic)
msoc3_h <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      race +
      edu,
    family = gaussian(),
    design = designsG$d.design.adult.heterosexual
  )


#age first sex (health)
mheal3_h <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = gaussian(),
    design = designsG$d.design.adult.heterosexual
  )

mhor3_h <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = gaussian(),
    design = designsG$d.design.adult.heterosexual
  )


#age at first sex (physical activity)

mphys3_h <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = gaussian(),
    design = designsG$d.design.adult.heterosexual
  )

#partnered status - anthropometric

m_partnered_h <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adult.heterosexual
  )

#partnered status - socioeconomic
msoc4_h <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      edu +
      race,
    family = quasibinomial(),
    design = designsG$d.design.adult.heterosexual
  )

#partnered status - health
mheal4_h <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasibinomial(),
    design = designsG$d.design.adult.heterosexual
  )


#partnered status - hormone
mhor4_h <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adult.heterosexual
  )

#phys activity

mphys4_h <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasibinomial(),
    design = designsG$d.design.adult.heterosexual
  )


# heterosexual as control -------------------------------------------------

# lifetime partners - baseline/anthropometric

m_lifetime_h2 <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex +
    heterosexual,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# lifetime partners - socioeconomic
msoc1_h2 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered + #* strength_centered + #added strength interaction 03.23 to match past year since lifetime partners is a function of past year; took out 4.6
      edu +
      race  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# lifetime partners - health model
mheal1_h2 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

# lifetime partners - hormone
mhor1_h2 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# lifetime partners - phys activity
mphys1_h2 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

# past year partners - baseline/ anthropometric
m_pastyear_h2 <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for these models
    bmi_centered * sex  +
    heterosexual,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# past year partners - socioeconomic
msoc2_h2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      edu +
      race  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# past year partners - health
mheal2_h2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

# past year partners - hormone
mhor2_h2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      testosterone_sex_centered * sex  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# past year partners - phys activity
mphys2_h2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work  +
      heterosexual,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


#age first sex - baseline/anthropometric
m_agefirst_h2 <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex  +
    heterosexual,
  family = gaussian(),
  design = designsG$d.design.adults
)

#age first sex - socioeconomic
msoc3_h2 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      race +
      edu  +
      heterosexual,
    family = gaussian(),
    design = designsG$d.design.adults
  )

#age first sex - health
mheal3_h2 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression  +
      heterosexual,
    family = gaussian(),
    design = designsG$d.design.adults
  )

#age at first sex - hormone
mhor3_h2 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex  +
      heterosexual,
    family = gaussian(),
    design = designsG$d.design.adults
  )


#age at first sex - physical activity
mphys3_h2 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work  +
      heterosexual,
    family = gaussian(),
    design = designsG$d.design.adults
  )

#partnered status - baseline/anthropometric
m_partnered_h2 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex  +
      heterosexual,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - socioeconomic
msoc4_h2 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      edu +
      race  +
      heterosexual,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - health
mheal4_h2 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression  +
      heterosexual,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - hormone
mhor4_h2 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex  +
      heterosexual,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - phys activity
mphys4_h2 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work  +
      heterosexual,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )


# heterosexual * sex ------------------------------------------------------

# lifetime partners - baseline/anthropometric

m_lifetime_h3 <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex +
    heterosexual * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# past year partners - baseline/ anthropometric
m_pastyear_h3 <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for these models
    bmi_centered * sex +
    heterosexual * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

#age first sex - baseline/anthropometric
m_agefirst_h3 <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex +
    heterosexual * sex,
  family = gaussian(),
  design = designsG$d.design.adults
)

#partnered status - baseline/anthropometric
m_partnered_h3 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex  +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

# #partnered status - socioeconomic
# msoc4_h2 <-
#   svyglm(
#     partnered ~
#       age_centered * sex +
#       strength_centered * sex +
#       edu +
#       race  +
#       heterosexual * sex,
#     family = quasibinomial(),
#     design = designsG$d.design.adults
#   )
#
# #partnered status - health
# mheal4_h2 <-
#   svyglm(
#     partnered ~
#       age_centered * sex +
#       strength_centered * sex +
#       perceived_abnormal_weight +
#       whitebloodcell_centered +
#       hemoglobin_centered +
#       special_equipment +
#       chronic_disease_score +
#       physical_disease_count +
#       depression  +
#       heterosexual * sex,
#     family = quasibinomial(),
#     design = designsG$d.design.adults
#   )
#
# #partnered status - hormone
# mhor4_h2 <-
#   svyglm(
#     partnered ~
#       age_centered * sex +
#       strength_centered * sex +
#       testosterone_sex_centered * sex  +
#       heterosexual * sex,
#     family = quasibinomial(),
#     design = designsG$d.design.adults
#   )
#
# #partnered status - phys activity
# mphys4_h2 <-
#   svyglm(
#     partnered ~
#       age_centered * sex +
#       strength_centered * sex +
#       vigorous_rec +
#       moderate_rec +
#       vigorous_work +
#       moderate_work  +
#       heterosexual * sex,
#     family = quasibinomial(),
#     design = designsG$d.design.adults
#   )

# TABLES ----------------------------------------------------------

tabnames <- c(
  "strength_centered" = "Strength (S)",
  "sex" = "Sex (Female)",
  "sex:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partnered" = "Partnered",
  "strength_centered:partnered" = "Partnered x Strength (S)",
  "age_centered:sex" = "Age (S) x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sex:bmi_centered" = "Sex (Female) x BMI (S)",
  "sex:height_centered" = "Sex (Female) x Height (S)",
  "sex:weight_centered" = "Sex (Female) x Weight (S)",
  'perceived_abnormal_weight' = "Perceived Abnormal Weight",
  "whitebloodcell_centered" = "White Blood Cell Count (S)",
  "hemoglobin_centered" = "Hemoglobin (S)",
  "special_equipment" = "Need special equip to walk",
  "chronic_disease_score" = "Chronic Disease Score",
  "physical_disease_count" = "Physical Disease Count",
  "depression" = "Depression Score",
  "log(testosterone)" = "Testosterone (log)",
  "sex:log(testosterone)" = "Sex (Female) x Testosterone (log)",
  "testosterone_sex_centered" = "Testosterone (S by Sex)",
  "sex:testosterone_sex_centered"= "Sex (Female) x Testosterone (S by Sex)",
  "vigorous_rec" = "Vigorous Recreation",
  "moderate_rec" = "Moderate Recreation",
  "vigorous_work" = "Vigorous Work",
  "moderate_work" = "Moderate Work",
  "edu" = "Education",
  "OtherHispanic" = "Other Hispanic",
  "raceNonHispanicBlack" = "Non-Hispanic Black",
  "raceNonHispanicAsian" = "Non-Hispanic Asian",
  "raceNonHispanicWhite" = "Non-Hispanic White",
  "raceOtherRace" = "Other Race",
  "foodinsecurity_adult" = "Food Insecurity",
  "tot_MET_centered" = "Total MET (S)",
  "avgcalories_centered" = "Average calories per day (S)"
)

# lifetime partners ----------------------------------------------------------

tanth1p <- tbl_regression(
  m_lifetime,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_lifetime, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth1h <- tbl_regression(
  m_lifetime_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_lifetime_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth1h2 <- tbl_regression(
  m_lifetime_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_lifetime_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth3h <- tbl_regression(
  m_lifetime_h3,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "heterosexual", "sex:heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_lifetime_h3, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tsoc1p <- tbl_regression(
  msoc1,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc1, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc1h <- tbl_regression(
  msoc1_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc1_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc1h2 <- tbl_regression(
  msoc1_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "heterosexual")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc1_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal1p <- tbl_regression(
  mheal1,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
  ) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mheal1, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal1h <- tbl_regression(
  mheal1_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal1_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal1h2 <- tbl_regression(
  mheal1_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal1_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor1p <- tbl_regression(
  mhor1,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
  ) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mhor1, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor1h <- tbl_regression(
  mhor1_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor1_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor1h2 <- tbl_regression(
  mhor1_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor1_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys1p <- tbl_regression(
  mphys1,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
  ) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mphys1, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys1h <- tbl_regression(
  mphys1_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys1_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys1h2 <- tbl_regression(
  mphys1_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys1_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

lifetime_compare <- tbl_merge(
  tbls = list(tanth1p, tanth1h, tanth1h2, tsoc1p, tsoc1h, tsoc1h2, theal1p, theal1h, theal1h2, thor1p, thor1h, thor1h2, tphys1p, tphys1h, tphys1h2),
  tab_spanner = c("Lifetime partners - anthropometric",
                  "Lifetime partners - anthropometric (heterosexual)",
                  "Lifetime partners - anthropometric (control)",
                  "Lifetime partners - socioeconomic",
                  "Lifetime partners - socioeconomic (heterosexual)",
                  "Lifetime partners - socioeconomic (control)",
                  "Lifetime partners - health",
                  "Lifetime partners - health (heterosexual)",
                  "Lifetime partners - health (control)",
                  "Lifetime partners - hormone",
                  "Lifetime partners - hormone (heterosexual)",
                  "Lifetime partners - hormone (control)",
                  "Lifetime partners - phys activity",
                  "Lifetime partners - phys activity (heterosexual)",
                  "Lifetime partners - phys activity (control)"
)) %>%
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))


# past year partners ----------------------------------------------------------
tanth2p <- tbl_regression(
  m_pastyear,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_pastyear, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth2h <- tbl_regression(
  m_pastyear_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_pastyear_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth2h2 <- tbl_regression(
  m_pastyear_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_pastyear_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc2p <- tbl_regression(
  msoc2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "strength_centered:partnered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc2h <- tbl_regression(
  msoc2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "strength_centered:partnered")
) %>%
  remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc2h2 <- tbl_regression(
  msoc2_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "strength_centered:partnered", "heterosexual")
) %>%
  remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc2_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal2p <- tbl_regression(
  mheal2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "strength_centered:partnered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mheal2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal2h <- tbl_regression(
  mheal2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal2h2 <- tbl_regression(
  mheal2_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "strength_centered:partnered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal2_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor2p <- tbl_regression(
  mhor2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "strength_centered:partnered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mhor2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor2h <- tbl_regression(
  mhor2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor2h2 <- tbl_regression(
  mhor2_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "strength_centered:partnered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor2_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys2p <- tbl_regression(
  mphys2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "strength_centered:partnered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mphys2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys2h <- tbl_regression(
  mphys2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tphys2h2 <- tbl_regression(
  mphys2_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "strength_centered:partnered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys2_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )
pastyear_compare <- tbl_merge(
  tbls = list(tanth2p, tanth2h, tanth2h2, tsoc2p, tsoc2h, tsoc2h2, theal2p, theal2h, theal2h2, thor2p, thor2h, thor2h2, tphys2p, tphys2h, tphys2h2),
  tab_spanner = c("Past year partners - anthropometric",
                  "Past year partners - anthropometric (heterosexual)",
                  "Past year partners - anthropometric (control)",
                  "Past year partners - socioeconomic",
                  "Past year partners - socioeconomic (heterosexual)",
                  "Past year partners - socioeconomic (control)",
                  "Past year partners - health",
                  "Past year partners - health (heterosexual)",
                  "Past year partners - health (control)",
                  "Past year partners - hormone",
                  "Past year partners - hormone (heterosexual)",
                  "Past year partners - hormone (control)",
                  "Past year partners - phys activity",
                  "Past year partners - phys activity (heterosexual)",
                  "Past year partners - phys activity (control)"
  )) %>%
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))

# age at first sex  -----------------------------------------------------------------

tanth3p <- tbl_regression(
  m_agefirst,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_agefirst, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth3h <- tbl_regression(
  m_agefirst_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_agefirst_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth3h2 <- tbl_regression(
  m_agefirst_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_agefirst_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc3p <- tbl_regression(
  msoc3,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc3, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )
tsoc3h <- tbl_regression(
  msoc3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc3h2 <- tbl_regression(
  msoc3_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "heterosexual")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc3_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal3p <- tbl_regression(
  mheal3,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mheal3, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )
theal3h <- tbl_regression(
  mheal3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal3h2 <- tbl_regression(
  mheal3_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal3_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor3p <- tbl_regression(
  mhor3,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mhor3, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor3h <- tbl_regression(
  mhor3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor3h2 <- tbl_regression(
  mhor3_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor3_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys3p <- tbl_regression(
  mphys3,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mphys3, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys3h <- tbl_regression(
  mphys3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys3h2 <- tbl_regression(
  mphys3_h2,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys3_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

agefirst_compare <- tbl_merge(
  tbls = list(tanth3p, tanth3h, tanth3h2, tsoc3p, tsoc3h, tsoc3h2, theal3p, theal3h, theal3h2, thor3p, thor3h, thor3h2, tphys3p, tphys3h, tphys3h2),
  tab_spanner = c("Age first sex - anthropometric",
                  "Age first sex - anthropometric (heterosexual)",
                  "Age first sex - anthropometric (control)",
                  "Age first sex - socioeconomic",
                  "Age first sex - socioeconomic (heterosexual)",
                  "Age first sex - socioeconomic (control)",
                  "Age first sex - health",
                  "Age first sex - health (heterosexual)",
                  "Age first sex - health (control)",
                  "Age first sex - hormone",
                  "Age first sex - hormone (heterosexual)",
                  "Age first sex - hormone (control)",
                  "Age first sex - phys activity",
                  "Age first sex - phys activity (heterosexual)",
                  "Age first sex - phys activity (control)"
  )) %>%
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))

# partnered ---------------------------------------------------------------
tanth4p <- tbl_regression(
  m_partnered,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_partnered, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth4h <- tbl_regression(
  m_partnered_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_partnered_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tanth4h2 <- tbl_regression(
  m_partnered_h2,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_partnered_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tsoc4p <- tbl_regression(
  msoc4,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc4, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )
tsoc4h <- tbl_regression(
  msoc4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc4_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tsoc4h2 <- tbl_regression(
  msoc4_h2,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "heterosexual")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc4_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal4p <- tbl_regression(
  mheal4,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mheal4, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal4h <- tbl_regression(
  mheal4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal4_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal4h2 <- tbl_regression(
  mheal4_h2,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal4_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor4p <- tbl_regression(
  mhor4,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mhor4, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor4h <- tbl_regression(
  mhor4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor4_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor4h2 <- tbl_regression(
  mhor4_h2,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor4_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys4p <- tbl_regression(
  mphys4,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mphys4, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys4h <- tbl_regression(
  mphys4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys4_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys4h2 <- tbl_regression(
  mphys4_h2,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys4_h2, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

partnered_compare <- tbl_merge(
  tbls = list(tanth4p, tanth4h, tanth4h2, tsoc4p, tsoc4h, tsoc4h2, theal4p, theal4h, theal4h2, thor4p, thor4h, thor4h2, tphys4p, tphys4h, tphys4h2),
  tab_spanner = c("Partnered - anthropometric",
                  "Partnered - anthropometric (heterosexual)",
                  "Partnered - anthropometric (control)",
                  "Partnered - socioeconomic",
                  "Partnered - socioeconomic (heterosexual)",
                  "Partnered - socioeconomic (control)",
                  "Partnered - health",
                  "Partnered - health (heterosexual)",
                  "Partnered - health (control)",
                  "Partnered - hormone",
                  "Partnered - hormone (heterosexual)",
                  "Partnered - hormone (control)",
                  "Partnered - phys activity",
                  "Partnered - phys activity (heterosexual)",
                  "Partnered - phys activity (control)"
  )) %>%
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))

# original models - married -----------------------------------------------

#(maritalstatus == 1) status - baseline/anthropometric
m_married <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )


#(maritalstatus == 1) status - socioeconomic
msoc4_m <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      edu +
      race,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - health
mheal4_m <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - hormone
mhor4_m <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - phys activity
mphys4_m <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )


# heterosexual control - married ------------------------------------------

#(maritalstatus == 1) status - baseline/anthropometric
m_married_mh <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex  +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - socioeconomic
msoc4_mh <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      edu +
      race  +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - health
mheal4_mh <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression  +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - hormone
mhor4_mh <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex  +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#(maritalstatus == 1) status - phys activity
mphys4_mh <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work  +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )



# tables ------------------------------------------------------------------

tanth4m <- tbl_regression(
  m_married,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_married, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tanth4mh <- tbl_regression(
  m_married_mh,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "heterosexual", "sex:heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_married_mh, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tsoc4m <- tbl_regression(
  msoc4_m,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc4_m, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tsoc4mh <- tbl_regression(
  msoc4_mh,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "heterosexual", "sex:heterosexual")
) %>% remove_row_type(race, type = c("header")) %>%
  remove_row_type(race, type = c("reference")) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(msoc4_mh, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

theal4m <- tbl_regression(
  mheal4_m,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mheal4_m, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


theal4mh <- tbl_regression(
  mheal4_mh,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "special_equipment", "perceived_abnormal_weight", "heterosexual", "sex:heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal4_mh, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

thor4m <- tbl_regression(
  mhor4_m,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mhor4_m, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


thor4mh <- tbl_regression(
  mhor4_mh,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:testosterone_sex_centered", "heterosexual", "sex:heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor4_mh, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )

tphys4m <- tbl_regression(
  mphys4_m,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered")
) %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(p.value = coef(summary(mphys4_m, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs) %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


tphys4mh <- tbl_regression(
  mphys4_mh,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "heterosexual", "sex:heterosexual")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys4_mh, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})"
  ) %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)  %>%
  modify_table_styling(
    columns = estimate,
    rows = p.value >= 0.05 & p.value <= 0.10,
    text_format = "bold"
  )


Married_compare2 <- tbl_merge(
  tbls = list(tanth4m, tanth4mh, tsoc4m, tsoc4mh, theal4m, theal4mh, thor4m, thor4mh, tphys4m, tphys4mh),
  tab_spanner = c("Married - anthropometric",
                  "Married - anthropometric (control)",
                  "Married - socioeconomic",
                  "Married - socioeconomic (control)",
                  "Married - health",
                  "Married - health (control)",
                  "Married - hormone",
                  "Married - hormone (control)",
                  "Married - phys activity",
                  "Married - phys activity (control)"
  )) %>%
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))

