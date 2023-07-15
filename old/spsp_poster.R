# spsp poster forestplot
library(survey)
library(ggplot2)
library(nhanesGH)
library(hagenutils)
library(ggpubr)

# Lifetime partner models -------------------------------------------------


# lifetime partners (anthropometric model)
manth1 <-
  svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
   # height_centered * sex +
   # weight_centered * sex,
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(manth1)

# lifetime partners (socioeconomic model)
msoc1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(msoc1, df.resid=Inf)

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
summary(mheal1)

#lifetime partners (hormone model) #log(test) confounded with sex resulting in huge error bars, sub model below
# mhor1 <-
#   svyglm(
#     sex_partners ~
#       age_centered * sex +
#       strength_centered * sex +
#       partnered +
#      log(testosterone) * sex,
#     family = quasipoisson(),
#     design = designsG$d.design.adults
#   )
# summary(mhor1)

mhor1a <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mhor1a, df = Inf)

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
summary(mphys1)


# Past year partner models ------------------------------------------------

# past year partners (anthropometric model)

manth2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
   #   height_centered * sex +
  #    weight_centered * sex,
     bmi_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(manth2, df.resid = Inf)

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


# past year partners (hormone model)
# mhor2 <-
#   svyglm(
#     sex_partners_year ~
#       age_centered * sex +
#       strength_centered * sex +
#       partnered * strength_centered +
#       log(testosterone) * sex,
#     family = quasipoisson(),
#     design = designsG$d.design.adults
#   )
# summary(mhor2, df.resid = Inf)

mhor2a <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mhor2a)

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
manth3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
    #  height_centered * sex +
    #  weight_centered * sex,
       bmi_centered * sex,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(manth3)

#age first sex (socioeconomic)
msoc3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      edu +
      race,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(msoc3)

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

#age first sex (hormone)
# mhor3 <-
#   svyglm(
#     age_first_sex ~
#       age_centered * sex +
#       strength_centered * sex +
#       partnered +
#       log(testosterone) * sex,
#     family = gaussian(),
#     design = designsG$d.design.adults
#   )
# summary(mhor3)

mhor3a <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(mhor3a)

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

manth4 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
    #  height_centered * sex +
    #  weight_centered * sex,
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(manth4)

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

# mhor4 <-
#   svyglm(
#     partnered ~
#       age_centered * sex +
#       strength_centered * sex +
#       log(testosterone) * sex,
#     family = quasibinomial(),
#     design = designsG$d.design.adults
#   )
# summary(mhor4)

mhor4a <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(mhor4a)

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
                 bmi_centered * sex,
               family= quasipoisson(),
               design=designsG$d.design.adults)
summary(mwbc)


# coef plots --------------------------------------------------------------

mnames <- c(
  "Anthropometric",
  "Socieoeconomic",
  "Health",
#  "Hormone", old model removed since log(testosterone) is confounded with sex
  "Physical Activity",
  "Hormone" #sub new model with sex centered testosterone
)

vnames <- c(
  "strength_centered" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI",
  "sexfemale:height_centered" = "Sex (Female) x Height",
  "sexfemale:weight_centered" = "Sex (Female) x Weight",
  'perceived_abnormal_weightTRUE' = "Perceived Abnormal Weight",
  "whitebloodcell_centered" = "White Blood Cell Count (S)",
  "hemoglobin_centered" = "Hemoglobin (S)",
  "special_equipmentTRUE" = "Need special equip to walk",
  "chronic_disease_score" = "Chronic Disease Score",
  "physical_disease_count" = "Physical Disease Count",
  "depression" = "Depression Score",
 "log(testosterone)" = "Testosterone (log)",
 "sexfemale:log(testosterone)" = "Sex (Female) x Testosterone (log)",
 "testosterone_sex_centered" = "Testosterone centered by sex",
 "sexfemale:testosterone_sex_centered"= "sex (Female) x testosterone",
  "vigorous_rec" = "Vigorous Recreation",
  "moderate_rec" = "Moderate Recreation",
  "vigorous_work" = "Vigorous Work",
  "moderate_work" = "Moderate Work",
  "edu" = "Education",
  "raceOtherHispanic" = "Other Hispanic",
  "raceNonHispanicBlack" = "Non-Hispanic Black",
  "raceNonHispanicAsian" = "Non-Hispanic Asian",
  "raceNonHispanicWhite" = "Non-Hispanic White",
  "raceOtherRace" = "Other Race"
  )





fig1 <- forestplot(
  manth1, msoc1, mheal1, mphys1, mhor1a,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mnames, varnames = vnames) + theme_minimal(20) +
  geom_pointrange(size = 1.2, position = position_dodge(width = .7)) +
  labs(title = "Lifetime Number of Sexual Partners", color = "Controls") +
  coord_cartesian(clip = "off")
# + xlim(-2.5, 3.55)

fig2 <- forestplot(
  manth2,msoc2,mheal2, mphys2, mhor2a,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mnames,
  varnames = vnames
) + theme_minimal(20) +
  geom_pointrange(size = 1.2, position = position_dodge(.8)) +
  labs(title = "Past Year Number of Sexual Partners", color = "Controls") +
  coord_cartesian(clip = "off")
# + xlim(-2.5, 3.55)

fig3 <- forestplot(
  manth3, msoc3, mheal3, mphys3, mhor3a,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mnames,
  varnames = vnames) +
  theme_minimal(20) +
  geom_pointrange(size = 1.2, position = position_dodge(width = .7)) +
  labs(title = "Age at First Sex", color = "Controls") +
  coord_cartesian(clip = "off")
# + xlim(-2.5, 3.55)

fig4 <- forestplot(
  manth4, msoc4, mheal4, mphys4, mhor4a,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mnames,
  varnames = vnames) +
  theme_minimal(20) + geom_pointrange(size = 1.2, position = position_dodge(width = .8)) +
  labs(title = "Currently Partnered", color = "Controls") +
  coord_cartesian(clip = "off")
# + xlim(-2.5, 3.55)

fig1234 <- ggarrange(fig1, fig2, fig3, fig4,
                   labels = "AUTO",
                   font.label = list(size = 20),
                   ncol = 1, nrow = 4,
                   common.legend = TRUE, legend="bottom")

fig <- annotate_figure(fig1234,
                       top = text_grob("Figure 2. Regression coefficients from generalized linear models of mating success\n",
                       size = 20, face = "bold",
                       hjust = 0,
                       x = 0.01),
                       bottom = text_grob("\nVariables labelled (S) have been standarized by 2 SD. Base level of race/ethnicity is Mexican American.",
                       size = 18))

ragg::agg_png("Fig1.600dpi.png", width = 12, height = 42, units = "in", res = 600)
fig
dev.off()

fig5 <- forestplot(
  mwbc,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  varnames = vnames) +
  theme_minimal(25) +
  geom_pointrange(size = 2, position = position_dodge(width = .8)) +
  labs(title = "White blood cell count") +
  # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
  #      subtitle = "White Blood Cell Count",
  #      caption = "Variables with (S) have been standarized by 2 SD.") +
  theme(legend.position = "none")

fig_wbc <- annotate_figure(fig5,
top = text_grob("Figure 3. Regression coefficients from generalized linear model of immune investment\n",
                size = 20, face = "bold",
                hjust = 0,
                x = 0.01),
bottom = text_grob("Variables labelled (S) have been standarized by 2 SD.",
                   size = 18))

ragg::agg_png("Fig2.600dpi.png", width = 12, height = 8, units = "in", res = 600)
fig_wbc
dev.off()
