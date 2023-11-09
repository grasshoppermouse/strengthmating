# new exploratory analyses based on reveiwers commments

library(survey)
library(tidyverse)
library(nhanesGH)
library(effects)

# unweighted comparison ---------------------------------------------------
# we cannot control for all variables accounted for through survey weights (eg response rate and income), and those we can control for (race/ethnicity)
# are not accounted for in every model and would require completely restructuring our model design. Responded to reviewers that we will retain survey weights
# but will report an unweighted analysis in the SI

scale2 <- function(x) scale(x)[,1]/2

m_lifetime_uw <- glm(
  sex_partners ~
    scale2(age) * sex +
    scale2(strength) * sex +
    partnered +
    race +
    scale2(bmi) * sex,
  family = quasipoisson(),
  data = d_G[d_G$age>=18 & d_G$age<=60,]
)


m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    race +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(m_lifetime, df.resid = Inf)
summary(m_lifetime_uw)

# lifetime partners socieconomic model

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
summary(msoc1, df.resid = Inf)


msoc1_uw <-
  glm(
    sex_partners ~
      scale2(age) * sex +
      scale2(strength) * sex +
      partnered +
      edu +
      race,
    family = quasipoisson(),
    data = d_G[d_G$age>=18 & d_G$age<=60,]
  )
summary(msoc1_uw)

# correlation matrices ----------------------------------------------------
# reported in SI
# need sex specific testosterone?

cor_mat_f <- svycor(
  ~ sex_partners +
    sex_partners_year +
    age_first_sex +
    age +
    strength +
    bmi +
    edu +
    hemoglobin +
    whitebloodcell +
    depression +
    chronic_disease_score +
    physical_disease_count +
    testosterone +
    vigorous_rec +
    moderate_rec +
    vigorous_work +
    moderate_work,
  designsG$d.design.adult.female,
  na.rm = T
)

cor_mat_cors_f <- cor_mat_f$cors
corr_f <- corrplot(cor_mat_cors_f, method = 'shade', addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75,
                   title = "Correlation matrix of coninuous study variables, Females 18-60")

cor_mat_m <- svycor(
  ~ sex_partners +
    sex_partners_year +
    age_first_sex +
    age +
    strength +
    bmi +
    edu +
    hemoglobin +
    whitebloodcell +
    depression +
    chronic_disease_score +
    physical_disease_count +
    testosterone +
    vigorous_rec +
    moderate_rec +
    vigorous_work +
    moderate_work,
  designsG$d.design.adult.male,
  na.rm = T
)

cor_mat_cors_m <- cor_mat_m$cors
corrplot(cor_mat_cors_m, method = 'shade', addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75)

corr_m <- corrplot(cor_mat_cors_m, method = 'shade', addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75,
                   title = "Correlation matrix of coninuous study variables, Males 18-60")



# exploratory-partnered ---------------------------------------------------

partnered_ordinal


# exploratory-sexual orientation ------------------------------------------

table(d_G$sexualorientation, useNA = "a")

# 1	Heterosexual or straight (attracted to [])
# 2	Homosexual or lesbian (attracted to [])
# 3	Bisexual (attracted to men and women)
# 4	Something else
# 5	Not sure

# sexual orientation
designsG$d.design.adults <- update(designsG$d.design.adults, sexualorientation2 = factor(sexualorientation))
ori <- svyglm(strength_centered ~ sexualorientation2*sex + partnered, family=gaussian(), design=designsG$d.design.adults)
summary(ori)
plot(allEffects(ori))

designsG$d.design.adult.male <- update(designsG$d.design.adult.male, sexualorientation2 = factor(sexualorientation))
ori2 <- svyglm(testosterone ~ sexualorientation2, family=gaussian(), design=designsG$d.design.adult.male)
summary(ori2)
plot(allEffects(ori2))

designsG$d.design.adult.female <- update(designsG$d.design.adult.female, sexualorientation2 = factor(sexualorientation))
ori2 <- svyglm(testosterone ~ sexualorientation2, family=gaussian(), design=designsG$d.design.adult.female)
summary(ori2)
plot(allEffects(ori2))




# vaginal sex models ------------------------------------------------------

# total vaginal sex partners
#anthropometric model
m_lifetime_v <-
  svyglm(
  vaginal_sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# lifetime partners (socioeconomic model)
msoc1_v <-
  svyglm(
    vaginal_sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# lifetime partners (health model)
mheal1_v <-
  svyglm(
    vaginal_sex_partners ~
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


mhor1_v <-
  svyglm(
    vaginal_sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


#phys activity
mphys1_v <-
  svyglm(
    vaginal_sex_partners ~
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


#past year vaginal sex partners
m_pastyear_v <-
  svyglm(
  vaginal_sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for this model
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

msoc2_v <-
  svyglm(
    vaginal_sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


# past year partners (health model)
mheal2_v <-
  svyglm(
    vaginal_sex_partners_year ~
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


mhor2_v <-
  svyglm(
    vaginal_sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )


#phys activity past year
mphys2_v <-
  svyglm(
    vaginal_sex_partners_year ~
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

mvnames <- c(
  "Anthropometric", #baseline model has bmi
  "Socieoeconomic",
  "Health",
  "Physical Activity",
  "Hormone" #sub new model with sex centered testosterone
)

fig1v <- forestplot(
  m_lifetime_v, msoc1_v, mheal1_v, mphys1_v, mhor1_v,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7) + theme_minimal() +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11)) +
  labs(title = "Lifetime Number of Vaginal Sexual Partners")


fig2v <- forestplot(
  m_pastyear_v, msoc2_v, mheal2_v, mphys2_v, mhor2_v,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Past Year Number of Vaginal Sexual Partners")  +
  theme(axis.text.y = element_blank(), plot.title = element_text(size = 11))


#same models with any sex
fig1 <- forestplot(
  m_lifetime, msoc1, mheal1, mphys1, mhor1,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7) + theme_minimal() +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11)) +
  labs(title = "Lifetime Number of Sexual Partners")


fig2 <- forestplot(
  m_pastyear, msoc2, mheal2, mphys2, mhor2,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Past Year Number of Sexual Partners")  +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))


compare_any <- (fig1 +
                  xlim(-3, 6) + fig1v +xlim(-3, 6)) + plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "bottom")
compare_any

compare_pastyear <- (fig2 +xlim(-3, 6) + fig2v +xlim(-3, 6)) + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
compare_pastyear


ggplot(d_G, aes(sex_partners_year, vaginal_sex_partners_year)) + geom_count() + facet_wrap(~sex)

ggplot(d_G, aes(sex_partners, vaginal_sex_partners)) + geom_count() + facet_wrap(~sex)

ggplot(d_G, aes(vaginal_sex_partners_year, vaginal_sex_partners)) + geom_count() + facet_wrap(~sex)


x <- d_G %>%
 dplyr::mutate(discrepant = vaginal_sex_partners_year > sex_partners_year,
               discrepancies = vaginal_sex_partners_year - sex_partners_year)


table(x$discrepant, x$sex)
prop.table(table(x$race, x$discrepant), margin = 1)

table(x$discrepant, x$partnered)

ggplot(x, aes(age, colour = discrepant)) + geom_density() + facet_wrap(~sex)

svymean(~sex_partners_year, design = designsG$d.design.adults, na.rm = TRUE)
svyvar(~sex_partners_year, design = designsG$d.design.adults, na.rm = TRUE)

svymean(~vaginal_sex_partners_year, design = designsG$d.design.adults, na.rm = TRUE)
svyvar(~vaginal_sex_partners_year, design = designsG$d.design.adults, na.rm = TRUE)


summary(mhor2_v)


# depression --------------------------------------------------------------


mheal1_1 <-
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
      depression*sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mheal1_1, df.resid = Inf)

mheal3_1 <-
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
      depression*sex,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(mheal3_1, df.resid = Inf)
