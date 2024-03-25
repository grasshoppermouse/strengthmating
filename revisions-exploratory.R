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



# partnered 2 -------------------------------------------------------------

designsG$d.design.adults <- update(designsG$d.design.adults,
                                   partnered3 = case_when(
                                     maritalstatus == 2 |
                                       maritalstatus == 3 |
                                       maritalstatus == 4 |
                                       maritalstatus == 5 ~ "Unpartnered",
                                     maritalstatus == 6 ~ "Livingwithpartner",
                                     maritalstatus == 1 ~ "Married"
                                   )
)


m_lifetime_p2 <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered3 * sex +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

summary(m_lifetime, df.resid = Inf)
summary(m_lifetime_p2, df.resid = Inf)


m_lifetime_p3 <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    (maritalstatus == 1) * sex+
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)



m_partnered_1 <-
  svyglm(
    (maritalstatus == 1) ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

m_partnered_2 <-
  svyglm(
    (maritalstatus == 6) ~
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )


#partnered status - hormone
mhor4_1 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex +
      heterosexual,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - phys activity
mphys4_1 <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )



tbl_merge(
  tbls = list(mm1p, mm1h, mm2p, mm2h, mm3p, mm3h, mm4p, mm4h),
  tab_spanner = c(
    "Lifetime number of sexual partners",
    "Lifetime number of sexual partners (heterosexual)",
    "Past year number of sexual partners",
    "Past year number of sexual partners (heterosexual)",
    "Age first sex",
    "Age first sex (heterosexual)",
    "Partnered",
    "Partnered (heterosexual)"
  )
) %>%
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))













theal2h <- tbl_regression(
  mheal2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

theal3h <- tbl_regression(
  mheal3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

theal4h <- tbl_regression(
  mheal4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mheal4_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)





thor2h <- tbl_regression(
  mhor2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

thor3h <- tbl_regression(
  mhor3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

thor4h <- tbl_regression(
  mhor4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mhor4_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)





tphys2h <- tbl_regression(
  mphys2_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys2_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

tphys3h <- tbl_regression(
  mphys3_h,
  label = tabnames,
  show_single_row = c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys3_h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

tphys4h <- tbl_regression(
  mphys4_h,
  label = tabnames,
  show_single_row = c("sex", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(mphys4h, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)


# code --------------------------------------------------------------------

#save model summary using df.resid
m <- summary(m_lifetime, df.resid = Inf)

#save coefficients as data frame
mm <- as.data.frame(m$coefficients)

mm$`Pr(>|t|)`

coef(summary(m_lifetime, df.resid = Inf))["age_centered", "Pr(>|t|)"]
coef(summary(m_lifetime, df.resid = Inf))[, 4]


# new vnames list with categorical variables collapsed
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
  'perceived_abnormal_weightTRUE' = "Perceived Abnormal Weight",
  "whitebloodcell_centered" = "White Blood Cell Count (S)",
  "hemoglobin_centered" = "Hemoglobin (S)",
  "special_equipmentTRUE" = "Need special equip to walk",
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
  "raceOtherHispanic" = "Other Hispanic 1",
  "raceNonHispanicBlack" = "Non-Hispanic Black",
  "raceNonHispanicAsian" = "Non-Hispanic Asian",
  "raceNonHispanicWhite" = "Non-Hispanic White",
  "raceOtherRace" = "Other Race",
  "foodinsecurity_adult" = "Food Insecurity",
  "tot_MET_centered" = "Total MET (S)",
  "avgcalories_centered" = "Average calories per day (S)"
)
catvars2 <- c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered", "strength_centered:partnered")
catvars <- c("sex", "partnered", "age_centered:sex", "sex:strength_centered", "sex:bmi_centered")

mm1_p <- tbl_regression(
  m_lifetime,
  label = tabnames,
  show_single_row = catvars
) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value = coef(summary(m_lifetime, df.resid = Inf))[-1, 4], digits = 2)) %>%
  add_significance_stars(hide_se = TRUE,
                         hide_p = FALSE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

mm1 <- tbl_regression(
  m_lifetime,
  label = tabnames,
  show_single_row = catvars
) %>%
  add_significance_stars(hide_se = TRUE,
                         hide_p = FALSE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)


tbl_merge(
  tbls = list(mm1, mm1_p),
  tab_spanner = c(
    "Lifetime number of sexual partners (design df)",
    "Lifetime number of sexual partners (df.resid = Inf)"
  )
)


mm2 <- tbl_regression(
  m_pastyear,
  label = vnames
) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

mm3 <- tbl_regression(
  m_agefirst,
  label = vnames
) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE) %>%
  add_glance_table(include = nobs)

mm4 <- tbl_regression(
  m_partnered,
  label = vnames
) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)%>%
  add_glance_table(include = nobs)

tbl_merge(
  tbls = list(mm1, mm2, mm3, mm4),
  tab_spanner = c(
    "Lifetime number of sexual partners",
    "Past year number of sexual partners",
    "Age at first sexual intercourse",
    "Currently partnered"
  )
) %>%
  as_gt() %>%
  gt::tab_header(
    title = "Table 2. Baseline Models of Mating Success",
    subtitle = "Regression coefficients from generalized
                 linear models for three indices of mating success for men and women ages
                 18-59. Models derived from Lassek and Gaulin 2009"
  )

library(broom)

#and then

tidy_coefs_with_ref <- function(mod_obj){

  term_names <- labels(mod_obj) %>% c('\\(Intercept\\)') %>% paste(collapse = '|')

  tidy_coefs <-
    tidy(mod_obj) %>%
    mutate(variable = str_extract(term, term_names)) %>%
    rowwise() %>%
    mutate(level = str_remove(term, stringr::fixed(variable)))

  xlevels <- mod_obj$xlevels

  missing_levels <- xlevels %>%
    enframe() %>%
    unnest() %>%
    set_names(c("variable", "level"))

  missing_levels %>%
    anti_join(tidy_coefs) %>%
    bind_rows(tidy_coefs) %>%
    arrange(variable, level)

}

o <- tidy_coefs_with_ref(msoc1_h)


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
  add_glance_table(include = nobs)


msoc_exp <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered + #* strength_centered + #added strength interaction 03.23 to match past year since lifetime partners is a function of past year; took out 4.6
      edu +
      race +
      heterosexual*sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )

mhor3_exp <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered3 +
      testosterone_sex_centered * sex +
      heterosexual,
    family = gaussian(),
    design = designsG$d.design.adults
  )

#partnered status - hormone
mhor4_exp <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )

#partnered status - phys activity
mphys4_exp <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work +
      heterosexual * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )


# wbc count  ------------------------------------------------------

i1 <- tbl_regression(mwbc, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


i2 <- tbl_regression(mwbc_alt, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(i1, i2),
  tab_spanner = c("Replication Model",
                  "Expanded Control Model")) %>%
  as_gt() %>%
  gt::tab_header(title = "Native Immune Investment (WBCC)",
                 subtitle = "Regression coefficients from generalized
                 linear models")

summary(mwbc, df.resid = Inf)
summary(mwbc_alt, df.resid = Inf)


# dietary energy ----------------------------------------------------------


d1 <- tbl_regression(m_energy, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


d2 <- tbl_regression(m_energy_alt, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(d1, d2),
  tab_spanner = c("Replication Model",
                  "Expanded Control Model")) %>%
  as_gt() %>%
  gt::tab_header(title = "Dietary Energy Intake (kcals)",
                 subtitle = "Regression coefficients from generalized
                 linear models")

summary(m_energy, df.resid = Inf)
summary(m_energy_alt, df.resid = Inf)


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
                     avgcalories_centered +
                     tot_MET_centered,
                   family= quasipoisson(),
                   design=designsG$d.design.adults)

summary(mwbc_alt, df.resid = Inf)

# Dietary Intake model ----------------------------------------------------

m_energy <- svyglm( #use this
  avgcalories ~
    age_centered +
    tot_MET_centered  +
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
    tot_MET_centered +
    strength_centered +
    sex +
    bmi_centered +
    whitebloodcell_centered +
    foodinsecurity_adult,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_energy_alt, df.resid = Inf)




##

mvnames <- c(
  "Anthropometric", #baseline model has bmi
  "Socieoeconomic",
  "Health",
  "Physical Activity",
  "Hormone" #sub new model with sex centered testosterone
)

fig1h <- forestplot(
  m_lifetime_h, msoc1_h, mheal1_h, mphys1_h, mhor1_h,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7) + theme_minimal() +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11)) +
  labs(title = "Lifetime Number of Sexual Partners (restricted to self-identified heterosexual)")


fig2h <- forestplot(
  m_pastyear_h, msoc2_h, mheal2_h, mphys2_h, mhor2_h,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Past Year Number of Sexual Partners (restricted to self-identified heterosexual)")  +
  theme(axis.text.y = element_blank(), plot.title = element_text(size = 11))

fig3h <- forestplot(
  m_agefirst_h, msoc3_h, mheal3_h, mphys3_h, mhor3_h,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Age at first sex (restricted to self-identified heterosexual)")  +
  theme(axis.text.y = element_blank(), plot.title = element_text(size = 11))

fig4h <- forestplot(
  m_partnered_h, msoc4_h, mheal4_h, mphys4_h, mhor4_h,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Currently partnered (restricted to self-identified heterosexual)")  +
  theme(axis.text.y = element_blank(), plot.title = element_text(size = 11))


#same models not restricted to heterosexual
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


fig3 <- forestplot(
  m_agefirst, msoc3, mheal3, mphys3, mhor3,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Age at first sex")  +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))

fig4 <- forestplot(
  m_partnered, msoc4, mheal4, mphys4, mhor4,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  modelnames = mvnames,
  varnames = vnames,
  size = 0.7,
  linewidth = 0.7
) + theme_minimal() +
  labs(title = "Currently partnered")  +
  theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))

compare_any <- (fig1 + fig1h) + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
compare_any

compare_pastyear <- (fig2 + fig2h) + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
compare_pastyear

compare_agefirst <- (fig3 + fig3h) + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
compare_agefirst

compare_partnered <- (fig4 + fig4h) + plot_layout(guides = "collect", ncol = 2) & theme(legend.position = "bottom")
compare_partnered


# strength centered sex ---------------------------------------------------

# Very little difference in coefficients compared to models without sex-specific strength standarization

mean_strength_sex <- svyby(~strength, by =~sex, design = designsG$d.design.adults, FUN = svymean, na.rm = T)
var_strength_sex <- svyby(~strength, by =~sex, design = designsG$d.design.adults, FUN = svyvar, na.rm = T)

designsG$d.design.adults <- update(
  designsG$d.design.adults,
  strength_sex_centered = ifelse(
    sex == 'female',
    (strength - mean_strength_sex$strength[2])/(2*sqrt(var_strength_sex$strength[2])),
    (strength - mean_strength_sex$strength[1])/(2*sqrt(var_strength_sex$strength[1]))
  ))


# lifetime partners - baseline/anthropometric

m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_sex_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)


m_pastyear_c <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_sex_centered * sex +
    partnered * strength_sex_centered + #keeping partnered x strength interaction only for these models
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

m_lifetime_d <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

w <- subset(designsG$d.design.adults, sex_partners <= 100)
w2 <- subset(designsG$d.design.adults, sex_partners_year <= 10)

m_pastyear_d <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for these models
    bmi_centered * sex,
  family = quasipoisson(),
  design = w2
)

# dietary protein ---------------------------------------------------------

m_protein <- svyglm( #use this
  avgprotein ~
    age_centered +
    tot_MET_centered  +
    strength_centered +
    bmi_centered  +
    sex,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_protein, df.resid = Inf)

m_protein_alt <- svyglm( #use this
  avgprotein ~
    age_centered +
    tot_MET_centered +
    strength_centered +
    sex +
    bmi_centered +
    whitebloodcell_centered +
    foodinsecurity_adult,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_protein_alt, df.resid = Inf)
