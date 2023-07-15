library(survey)
library(tidyverse)
library(nhanesGH)
library(visreg)
library(hagenutils)
library(patchwork)
library(foreign)
library(effects)
library(gtsummary)

# compute occupational expenditure per Lassek and Gaulin
# we have MET from participant reported occ exertion, a better var than this, also NHANES recommends using
# diff weights for dietary data
#
# occ_exertion <- svyby(~avgcalories, ~census_code_best, design = designsG$d.design.adults, svymean)
# occ_exertion_dict <- occ_exertion$avgcalories
# names(occ_exertion_dict) <- occ_exertion$census_code_curr
# designsG$d.design.adults <- update(designsG$d.design.adults, occ_exertion = occ_exertion_dict[census_code_curr])
#
#
# x <- svyby(~avgcalories, ~race, design = designsG$d.design.adults, svymean, na.rm = T)


# use both number of partners and num vaginal partners, missing 743 who report
# having vaginal sex but are missing number of vaginal sex partners
# much fewer (~56) missing number of total sex partners
# around 686 ppl skipped whole module?


# conceptual rep of Lassek and Gaulin, using grip strength and including males and females

# Table 2 (Exact models: Total partners, Partners past year, age at first intercourse)

mm1_exact <- svyglm(
  sex_partners ~
    age_centered * strength_centered + # * sex + #include interaction in SI
    partnered + #* sex +
    sex * strength_centered + #always include
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(mm1_exact)


mm2_exact <- svyglm(
  sex_partners_year ~
    # age_centered * strength_centered +
    sex * strength_centered +
    partnered*strength_centered +
    #  partnered*sex +
    #  bmi_centered*sex  #add to SI
    bmi_centered*partnered, #leave in
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(mm2_exact)

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
summary(mm3_exact)

# Model 1 (Table 2) Lifetime sexual partners ------------------------------



mm1_mod <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered, # +
    # bmi_centered*sex +
    #  edu +
   # median_salary_current +
   # age_first_sex*sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(mm1_mod)
plot(allEffects(mm1_mod))

# ggplot(d_G, aes(age_first_sex, sex_partners)) + geom_count() + scale_y_log10() +facet_grid(I(cut(age, 5))~sex) +geom_smooth()

m1_vaginal_sex_partners <- svyglm(
  vaginal_sex_partners ~
    age_centered * sex +
    sex * strength_centered +
    partnered +
    bmi_centered +
    edu +
    age_first_sex*sex,
  #  tot_MET,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(m1_vaginal_sex_partners)

# Past year sexual partners -----------------------------------------------



mm2_mod <- svyglm(
  sex_partners_year ~
    partnered*strength_centered +
    bmi_centered*partnered +
 #   edu*partnered +
#    median_salary_current*partnered +
   # age*partnered +
  # total_work_MET +
  # total_rec_MET +
  sex*strength_centered,
#  age_first_sex, #* sex, leave out since no rationale
 # tot_MET*sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

summary(mm2_mod)

# ggplot(d_G, aes(sex_partners, vaginal_sex_partners)) + geom_count() + geom_abline(slope = 1) + scale_x_log10() + scale_y_log10() + facet_wrap(~sex)
# ggplot(d_G, aes(sex_partners, sex_partners_year)) + geom_count() + geom_abline(slope = 1) + facet_wrap(~sex) + scale_x_log10() + scale_y_log10()

m2_vaginal_pastyear <- svyglm(
  vaginal_sex_partners_year ~
    age_centered +
    sex * strength_centered +
    partnered +
    bmi_centered +
    edu +
    age_first_sex*sex +
    partnered * strength_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(m2_vaginal_pastyear, df.resid = Inf)

# Age at first intercourse ------------------------------------------------



mm3_mod <- svyglm(
  age_first_sex ~
    age_centered +
    strength_centered*sex +
    edu +
   # median_salary_current +
    total_work_MET*sex +
    total_rec_MET*sex +
    bmi_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(mm3_mod)
plot(allEffects(mm3_mod))



# Daily Energy Intake (Table 3) -------------------------------------------

# bone mineral density not collected in G, collected in H in altered sample

# not including in rep because we do not have ffm

# adults = d_G$age>=18 & d_G$age<=60
#
# d.design.dietary <-
#   svydesign(
#     id = ~SDMVPSU ,
#     strata = ~SDMVSTRA ,
#     nest = TRUE ,
#     weights = ~WTMEC2YR, #~WTINT2YR ,
#     data = d_G
#   )
#
# d.design.dietary.adults <-
#   subset(
#     d.design.dietary,
#     adults
#   )
#
# m <- svyglm(
#   log(avgcalories) ~
#     age +
#     sex * tot_MET +
#     strength +
#     weight +
#     height,
#   family = gaussian(),
#   design = d.design.dietary.adults
# )
# summary(m)
# plot(allEffects(m))
#
# x <- c(na.omit(d_G$avgcalories))
#
# m <- svyglm(
#   log(avgcalories) ~
#     age +
#     #sex +
#     total_work_MET *sex +
#     total_rec_MET *sex +
#     wob_MET * sex +
#     strength +
#     weight ,
#   family = gaussian(),
#   design = d.design.dietary.adults
# )
# summary(m)
# plot(allEffects(m))
#
# x <- c(na.omit(d_G$whitebloodcell))
#
# descdist(x, discrete = T)


# wbcc --------------------------------------------------------------------


m_wbcc_exact <- svyglm(
  log(whitebloodcell) ~
    age +
    strength * sex +
    bmi,
  family = gaussian(),
  design = designsG$d.design.adults
)
summary(m_wbcc_exact)



m_l <- svyglm(
  log(lymphocytes) ~
    age +
    strength +
    sex +
    bmi,
  family = gaussian(),
  design = designsG$d.design.adults
)
summary(m_l)


m_m <- svyglm(
  log(1+monocytes) ~
    age +
    strength +
    sex +
    bmi,
  family = gaussian(),
  design = designsG$d.design.adults
)
summary(m_m)


m_n <- svyglm(
  log(neutrophils) ~
    age +
    strength +
    sex +
    bmi,
  family = gaussian(),
  design = designsG$d.design.adults
)
summary(m_n)

m_e <- svyglm(
  log(1+eosinophils) ~
    age +
    strength +
    sex +
    bmi,
  family = gaussian(),
  design = designsG$d.design.adults
)
summary(m_e)


m_b <- svyglm(
  log(1+basophils) ~
    age +
    strength +
    sex +
    bmi,
  family = gaussian(),
  design = designsG$d.design.adults
)
summary(m_b)

### hormone model with by sex testosterone

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
