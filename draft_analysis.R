library(survey)
library(ggplot2)
library(nhanesGH)
library(hagenutils)
library(ggpubr)


# Anthropometric models ---------------------------------------------------------

m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

#visreg(m_lifetime,  xvar = "strength_centered", by = "sex", scale = "response")

#plot(allEffects(m_lifetime))

m_pastyear <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for this model
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

m_agefirst <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = gaussian(),
  design = designsG$d.design.adults
)

m_partnered <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(m_partnered, df.resid = Inf)



mnames1 <- c(
  "Age at first intercourse",
  "Lifetime Number of Sexual Partners",
  "Past Year Number of Sexual Partners",
  "Currently Partnered"
)
#
# vnames1 <- c(
#   "strength_centered" = "Strength (S)",
#   "sexfemale" = "Sex (Female)",
#   "sexfemale:strength_centered" = "Sex (Female) x Strength",
#   "age_centered" = "Age (S)",
#   "partneredTRUE" = "Partnered",
#   "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
#   "age_centered:sexfemale" = "Age x Sex (Female)",
#   "height_centered" = "Height (S)",
#   "weight_centered" = "Weight (S)",
#   "bmi_centered" = "BMI (S)",
#   "sexfemale:bmi_centered" = "Sex (Female) x BMI"
# )

# forestplot(
#   m_agefirst,
#   m_lifetime,
#   m_pastyear,
#   intercept = F,
#   facet = F,
#   dodgewidth = .5,
#   modelnames = mnames1,
#   varnames = vnames1
# )$plot + theme_minimal(20) +
#   geom_pointrange(size = 1.2, position = position_dodge(width = .5)) +
#   labs(title = "Exact Models: Regression coefficients from generalized linear models", color = "Model") +
#   guides(colour = guide_legend(reverse = T))


# Lifetime partner models -------------------------------------------------


# lifetime partners (anthropometric model) #no longer needed since baseline model includes bmi
# manth1 <-
#   svyglm(
#     sex_partners ~
#       age_centered * sex +
#       strength_centered * sex +
#       partnered * strength_centered + #added strength interaction 03.23 to match past year since lifetime partners is a function of past year
#       bmi_centered * sex,
#     family = quasipoisson(),
#     design = designsG$d.design.adults
#   )


# lifetime partners (socioeconomic model)
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
summary(msoc1, df.resid = Inf)

# lifetime partners (health model)
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
summary(mheal1, df.resid = Inf)

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
summary(mhor1, df.resid = Inf)

#phys activity
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
summary(mphys1, df.resid = Inf)


# Past year partner models ------------------------------------------------

# past year partners (anthropometric model)

# manth2 <-
#   svyglm(
#     sex_partners_year ~
#       age_centered * sex +
#       strength_centered * sex +
#       partnered * strength_centered +
#       bmi_centered * sex,
#     family = quasipoisson(),
#     design = designsG$d.design.adults
#   )
# summary(manth2)

# past year partners (socioeconomic model)
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
summary(msoc2)

# past year partners (health model)
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
summary(mheal2)

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
summary(mhor2, df.resid = Inf)

#phys activity past year
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
summary(mphys2)



# Age at first sex models -------------------------------------------------

#age first sex (anthropometric)
# manth3 <-
#   svyglm(
#     age_first_sex ~
#       age_centered * sex +
#       strength_centered * sex +
#       partnered +
#       bmi_centered * sex,
#     family = gaussian(),
#     design = designsG$d.design.adults
#   )
# summary(manth3)

#age first sex (socioeconomic)
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
summary(msoc3, df.resid = Inf)

#age first sex (health)
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
summary(mheal3)

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
summary(mhor3)

#age at first sex (physical activity)

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
summary(mphys3)


# partnered models --------------------------------------------------------

# manth4 <-
#   svyglm(
#     partnered ~
#       age_centered * sex +
#       strength_centered * sex +
#       bmi_centered * sex,
#     family = quasibinomial(),
#     design = designsG$d.design.adults
#   )
# summary(manth4)

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
summary(msoc4)

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
summary(mheal4)

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
summary(mhor4)

#phys activity

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
summary(mphys4)


# immune model ------------------------------------------------------------


mwbc <- svyglm(whitebloodcell ~
                 age_centered * sex +
                 strength_centered * sex +
                 bmi_centered,
               family= quasipoisson(),
               design=designsG$d.design.adults)
summary(mwbc, df.resid = Inf)




# alternate immune model --------------------------------------------------

mwbc_alt <- svyglm(whitebloodcell ~
                 age_centered * sex +
                 strength_centered * sex +
                 bmi_centered +
                 testosterone_sex_centered * sex +
                foodinsecurity_adult  +
                  avgcalories +
                   tot_MET,
               family= quasipoisson(),
               design=designsG$d.design.adults)

summary(mwbc_alt, df.resid = Inf)

# Dietary Intake model ----------------------------------------------------

m_energy <- svyglm( #use this
  avgcalories ~
    age_centered +
    tot_MET  +
    strength_centered +
    bmi_centered  +
    sex,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_energy, df.resid = Inf)

m_energy_alt <- svyglm( #use this
  avgcalories ~
    age_centered +
    tot_MET +
    strength_centered +
    sex +
    bmi_centered +
    whitebloodcell +
    foodinsecurity_adult,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_energy_alt, df.resid = Inf)


# coef plots --------------------------------------------------------------

mnames <- c(
 # "Anthropometric", baseline model has bmi
  "Socieoeconomic",
  "Health",
  # "Hormone", old model removed since log(testosterone) is confounded with sex
  "Physical Activity",
  "Hormone" #sub new model with sex centered testosterone
)

immunenames <- c(
  "WBCC",
  "WBCC with hormone control"
)

vnames <- c(
  "strength_centered" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age (S) x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI (S)",
  "sexfemale:height_centered" = "Sex (Female) x Height (S)",
  "sexfemale:weight_centered" = "Sex (Female) x Weight (S)",
  'perceived_abnormal_weightTRUE' = "Perceived Abnormal Weight",
  "whitebloodcell_centered" = "White Blood Cell Count (S)",
  "hemoglobin_centered" = "Hemoglobin (S)",
  "special_equipmentTRUE" = "Need special equip to walk",
  "chronic_disease_score" = "Chronic Disease Score",
  "physical_disease_count" = "Physical Disease Count",
  "depression" = "Depression Score",
  "log(testosterone)" = "Testosterone (log)",
  "sexfemale:log(testosterone)" = "Sex (Female) x Testosterone (log)",
  "testosterone_sex_centered" = "Testosterone (S by Sex)",
  "sexfemale:testosterone_sex_centered"= "Sex (Female) x Testosterone (S by Sex)",
  "vigorous_rec" = "Vigorous Recreation",
  "moderate_rec" = "Moderate Recreation",
  "vigorous_work" = "Vigorous Work",
  "moderate_work" = "Moderate Work",
  "edu" = "Education",
  "raceOtherHispanic" = "Other Hispanic",
  "raceNonHispanicBlack" = "Non-Hispanic Black",
  "raceNonHispanicAsian" = "Non-Hispanic Asian",
  "raceNonHispanicWhite" = "Non-Hispanic White",
  "raceOtherRace" = "Other Race",
  "foodinsecurity_adult" = "Food Insecurity",
  "tot_MET" = "Total MET",
  "avgcalories" = "Average calories per day"
)


# vnames <- c(
#   "strength_centered" = "Strength (S)",
#   "sexfemale" = "Sex (Female)",
#   "sexfemale:strength_centered" = "Sex (Female) x Strength",
#   "age_centered" = "Age (S)",
#   "partneredTRUE" = "Partnered",
#   "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
#   "age_centered:sexfemale" = "Age x Sex (Female)",
#   "height_centered" = "Height (S)",
#   "weight_centered" = "Weight (S)",
#   "bmi_centered" = "BMI (S)",
#   "sexfemale:bmi_centered" = "Sex (Female) x BMI",
#   "sexfemale:height_centered" = "Sex (Female) x Height",
#   "sexfemale:weight_centered" = "Sex (Female) x Weight",
#   'perceived_abnormal_weightTRUE' = "Perceived Abnormal Weight",
#   "whitebloodcell_centered" = "White Blood Cell Count (S)",
#   "hemoglobin_centered" = "Hemoglobin (S)",
#   "special_equipmentTRUE" = "Need special equip to walk",
#   "chronic_disease_score" = "Chronic Disease Score",
#   "physical_disease_count" = "Physical Disease Count",
#   "depression" = "Depression Score",
#   "log(testosterone)" = "Testosterone (log)",
#   "sexfemale:log(testosterone)" = "Sex (Female) x Testosterone (log)",
#   "testosterone_sex_centered" = "Testosterone Centered by Sex",
#   "sexfemale:testosterone_sex_centered"= "Sex (Female) x Testosterone Centered by Sex",
#   "vigorous_rec" = "Vigorous Recreation",
#   "moderate_rec" = "Moderate Recreation",
#   "vigorous_work" = "Vigorous Work",
#   "moderate_work" = "Moderate Work",
#   "edu" = "Education",
#   "raceOtherHispanic" = "Other Hispanic",
#   "raceNonHispanicBlack" = "Non-Hispanic Black",
#   "raceNonHispanicAsian" = "Non-Hispanic Asian",
#   "raceNonHispanicWhite" = "Non-Hispanic White",
#   "raceOtherRace" = "Other Race"
# )


# fig1 <- forestplot(
#   manth1, msoc1, mheal1, mphys1, mhor1a,
#   intercept = F,
#   facet = F,
#   dodgewidth = .8,
#   modelnames = mnames, varnames = vnames)$plot + theme_minimal(20) +
#   geom_pointrange(size = 1.2, position = position_dodge(width = .7)) +
#   labs(title = "Lifetime Number of Sexual Partners", color = "Controls") +
#   coord_cartesian(clip = "off")
#
#
# fig2 <- forestplot(
#   manth2,msoc2,mheal2, mphys2, mhor2a,
#   intercept = F,
#   facet = F,
#   dodgewidth = .8,
#   modelnames = mnames,
#   varnames = vnames
# )$plot + theme_minimal(20) +
#   geom_pointrange(size = 1.2, position = position_dodge(.8)) +
#   labs(title = "Past Year Number of Sexual Partners", color = "Controls") +
#   coord_cartesian(clip = "off")
#
#
# fig3 <- forestplot(
#   manth3, msoc3, mheal3, mphys3, mhor3a,
#   intercept = F,
#   facet = F,
#   dodgewidth = .8,
#   modelnames = mnames,
#   varnames = vnames)$plot +
#   theme_minimal(20) +
#   geom_pointrange(size = 1.2, position = position_dodge(width = .7)) +
#   labs(title = "Age at First Sex", color = "Controls") +
#   coord_cartesian(clip = "off")
#
#
# fig4 <- forestplot(
#   manth4, msoc4, mheal4, mphys4, mhor4a,
#   intercept = F,
#   facet = F,
#   dodgewidth = .8,
#   modelnames = mnames,
#   varnames = vnames)$plot +
#   theme_minimal(20) + geom_pointrange(size = 1.2, position = position_dodge(width = .8)) +
#   labs(title = "Currently Partnered", color = "Controls") +
#   coord_cartesian(clip = "off")
#
#
# fig1234 <- ggarrange(fig1, fig2, fig3, fig4,
#                      labels = "AUTO",
#                      font.label = list(size = 20),
#                      ncol = 1, nrow = 4,
#                      common.legend = TRUE, legend="bottom")
#
# fig <- annotate_figure(fig1234,
#                        top = text_grob("Figure 2. Regression coefficients from generalized linear models of mating success\n",
#                                        size = 20, face = "bold",
#                                        hjust = 0,
#                                        x = 0.01),
#                        bottom = text_grob("\nVariables labelled (S) have been standarized by 2 SD. Base level of race/ethnicity is Mexican American.",
#                                           size = 18))
#
#
#
# fig5 <- forestplot(
#   mwbc,
#   intercept = F,
#   facet = F,
#   dodgewidth = .8,
#   varnames = vnames)$plot +
#   theme_minimal(25) +
#   geom_pointrange(size = 2, position = position_dodge(width = .8)) +
#   labs(title = "White blood cell count") +
#   # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
#   #      subtitle = "White Blood Cell Count",
#   #      caption = "Variables with (S) have been standarized by 2 SD.") +
#   theme(legend.position = "none")
#
# fig_wbc <- annotate_figure(fig5,
#                            top = text_grob("Figure 3. Regression coefficients from generalized linear model of immune investment\n",
#                                            size = 20, face = "bold",
#                                            hjust = 0,
#                                            x = 0.01),
#                            bottom = text_grob("Variables labelled (S) have been standarized by 2 SD.",
#                                               size = 18))
#
#
#
# forestplot(
#   mwbc_alt,
#   intercept = F,
#   facet = F,
#   dodgewidth = .8,
#   varnames = vnames)$plot +
#   theme_minimal(25) +
#   geom_pointrange(size = 2, position = position_dodge(width = .8)) +
#   labs(title = "White blood cell count") +
#   # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
#   #      subtitle = "White Blood Cell Count",
#   #      caption = "Variables with (S) have been standarized by 2 SD.") +
#   theme(legend.position = "none")
#

