library(survey)
library(tidyverse)
library(nhanesGH)
library(visreg)
library(hagenutils)
library(patchwork)
library(foreign)
library(effects)
library(gtsummary)
#library(sjstats)

#remove 10 males who report taking testosterone supplements
designsG$d.design.adults.noT <- subset(designsG$d.design.adults, TESTOSTERONE == 0)

m <- svyglm(whitebloodcell ~ bmi_centered + strength_centered + log(testosterone)*sex + age_centered*sex, family = gaussian(), design = designsG$d.design.adults.noT)
summary(m)
plot(allEffects(m))

m1f <- svyglm(whitebloodcell ~ bmi + strength + testosterone + age, family = gaussian(), design = designsG$d.design.adult.female)
summary(m1f)
plot(allEffects(m1f))



m <- svyglm(whitebloodcell ~ bmi_centered + strength_centered + testosterone*sex + age_centered*sex, family = quasipoisson(), design = designsG$d.design.adults)
summary(m)
plot(allEffects(m))

m <- svyglm(whitebloodcell ~ bmi_centered*sex + strength_centered + log(testosterone)*sex + age_centered*sex + race, family = quasipoisson(), design = designsG$d.design.adults)
summary(m)
plot(allEffects(m))

m2 <- svyglm(whitebloodcell ~  race*strength + age + bmi, family = gaussian(), design = designsG$d.design.adult.male)
summary(m2)
plot(allEffects(m2))
plot(predictorEffects(m2, ~ strength))

m3 <- svyglm(whitebloodcell ~  strength_centered*sex + race, family = quasipoisson(), design = designsG$d.design.adults.noT)
summary(m3)
plot(allEffects(m3))

m2 <- svyglm(whitebloodcell ~ bmi + strength_centered + age, family = quasipoisson(), design = designsG$d.design.adults)
summary(m2)

m2 <- svyglm(whitebloodcell ~ bmi + strength_centered*sex + age, family = gaussian(), design = designsG$d.design.adults)
summary(m2)

# partnered status models -------------------------------------------------


m <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + edu + bmi, family = binomial, designsG$d.design.adults)
summary(m)
plot(allEffects(m))
# visreg(m, xvar = 'strength_centered', by = 'sex', scale = 'response', gg = T) + theme_minimal()

# Males
smooth_partnered_m <- svysmooth(strength ~ age, subset(designsG$d.design.male, partnered == T))
smooth_unpartnered_m <- svysmooth(strength ~ age, subset(designsG$d.design.male, partnered == F))
d1 <- svysmooth2df(partnered = smooth_partnered_m, unpartnered = smooth_unpartnered_m)
ggplot(d1, aes(age, strength, colour = Smooth)) +
  geom_line() +
  theme_minimal(15)

# Females
smooth_partnered_f <- svysmooth(strength ~ age, subset(designsG$d.design.female, partnered == T))
smooth_unpartnered_f <- svysmooth(strength ~ age, subset(designsG$d.design.female, partnered == F))
d2 <- svysmooth2df(partnered = smooth_partnered_f, unpartnered = smooth_unpartnered_f)
ggplot(d2, aes(age, strength, colour = Smooth)) +
  geom_line() +
  theme_minimal(15)


#partnered status - anthropometrics

m14 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + height_centered*sex + weight_centered + bmi_centered*sex, family=quasibinomial(), design=designsG$d.design.adults)
summary(m14)
plot(allEffects(m14))
nobs(m14)
nobs(mp)

mp <- svyglm(partnered ~ sex + age, family = quasibinomial(), design=designsG$d.design)
summary(mp) #no sex difference after controlling for age in adults 18-60


# m14.1 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + height_centered*sex + weight_centered * sex +
#                   height_centered * weight_centered, family=quasibinomial(), design=designsG$d.design.adults)
# summary(m14.1)

designsG$d.design.adults <- update(designsG$d.design.adults, leglenth_prop = leglength/height)
m14.2 <- svyglm(partnered ~ strength_centered*sex + leglenth_prop*sex + age_centered*sex + weight_centered, family=quasibinomial(), design=designsG$d.design.adults)
summary(m14.2)
plot(allEffects(m14.2)) #marginal negative effect of armlength on partnered, no theory
Anova(m14.2, type = 3)

# m15 <- svyglm(partnered ~ height_centered*sex + age_centered*sex, family=quasibinomial(), design=designsG$d.design.adults)
# summary(m15)

#partnered status - socioeconomic
m13 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + race, family=quasibinomial(), design=designsG$d.design.adults)
summary(m13)
nobs(m13)


# visreg(m13, by = "race", xvar = "sex",  scale = "response", rug = FALSE, gg = TRUE) + coord_flip()
#
# visreg(m13, xvar="strength_centered", by = "sex", scale= "response", rug = TRUE, gg= TRUE)
# visreg(m13, xvar="age_centered", by = "sex", scale= "response", rug = TRUE, gg= TRUE)

m16 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + race + edu, family=quasibinomial(), design=designsG$d.design.adults )
summary(m16)

#partnered status - health
m17 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression, family=quasibinomial(), design=designsG$d.design.adults)
summary(m17)

m18 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                race + special_equipment + whitebloodcell + hemoglobin*sex + chronic_disease_score + physical_disease_count, family=quasibinomial(), design=designsG$d.design.adults)
summary(m18)

m19 <- svyglm(partnered ~ strength_centered*sex + race + hemoglobin_centered*sex + age_centered*sex + special_equipment +
                chronic_disease_score + physical_disease_count + whitebloodcell_centered, family=quasibinomial(), design=designsG$d.design.adults)
summary(m19, df.resid = Inf)

#partnered status - hormone

m20 <- svyglm(partnered ~ sex*strength_centered + age_centered*sex + sex*testosterone_centered,  family=quasibinomial(), design=designsG$d.design.adults)
summary(m20)
plot(allEffects(m20))
exp(coef(m20))

# visreg(m20, xvar = "testosterone", by = "sex", scale = "response", xtrans = log)

designsG$d.design.adults <- update(designsG$d.design.adults, household_size2 = ifelse(partnered, household_size -1, household_size))

designsG$d.design.adults <- update(designsG$d.design.adults, testosterone_sup = ifelse(TESTOSTERONE > 0, TRUE, FALSE))


m20b <- svyglm(partnered ~ strength_centered*sex + age_centered*sex + log(testosterone)*sex + household_size2,  family=quasibinomial(), design=designsG$d.design.adults)
summary(m20b)
exp(coef(m20))

m20c <- svyglm(partnered ~ strength_centered*sex + age_centered*sex + log(testosterone)*sex + testosterone_sup,  family=quasibinomial(), design=designsG$d.design.adults)
summary(m20c)

designsG$d.design.adult.male <- update(designsG$d.design.adult.male, household_size2 = ifelse(partnered, household_size -1, household_size))

t <- svyglm(testosterone ~ household_size2 + partnered, family = gaussian(), design= designsG$d.design.adult.male)
summary(t)

#phys activity

phys <- svyglm(partnered ~ vigorous_rec + moderate_rec + vigorous_work + moderate_work + age_centered*sex + strength_centered*sex, family=quasibinomial(), design=designsG$d.design.adults)
summary(phys)


# visreg(m20c, xvar = "testosterone2", by = "sex", xtrans = log)

# m21 <- svyglm(partnered ~ strength_centered*sex + age_centered*sex + t4free,  family=quasibinomial(), design=designsG$d.design.adults)
# summary(m21)

ggplot(d_G, aes(age, testosterone, colour=sex)) + geom_point(alpha = 0.1) + geom_smooth() + ylim(0,1000) + scale_y_log10()

# Exploratory (G only) ----------------------------------------------------

# lifetime number of sex partners (heterosexual)
m <- svyglm(sex_partners ~ age_centered*sex + sex*strength_centered + partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m)
plot(allEffects(m))

d = data.frame(
  age_centered = c(0, 0),
  strength_centered = c(-.5, .5),
  sex = c("male", "male"),
  partnered = c(FALSE, FALSE)
)
predict(m, d, type = "response")
plot(emmeans(m, specs = "strength_centered", by = "sex", type = "response"))

# visreg(m, xvar = "strength_centered", by = "sex", scale = "response")
# visreg(m)

# lifetime partners (anthropometric model)
manth1 <- svyglm(sex_partners ~ age_centered*sex + strength_centered*sex + height_centered*sex + weight_centered*sex + partnered, family=quasipoisson(), design=designsG$d.design.adults)
summary(manth1)
# visreg(manth1)
plot(allEffects(manth1))

plot(svysmooth(weight~age, designsG$d.design.adult.male))

# lifetime partners (socioeconomic model)
msoc1 <- svyglm(sex_partners ~ age_centered*sex + strength_centered*sex + edu + race + partnered, family=quasipoisson(), design=designsG$d.design.adults )
summary(msoc1, df.resid=Inf)

# lifetime partners (health model)
mheal1 <- svyglm(sex_partners ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression + partnered, family=quasipoisson(), design=designsG$d.design.adults)
summary(mheal1)
nobs(mheal1)

#lifetime partners (hormone model)
mhor1 <- svyglm(sex_partners ~ strength_centered*sex + age_centered*sex + log(testosterone)*sex + partnered,  family=quasipoisson(), design=designsG$d.design.adults)
summary(mhor1)
plot(allEffects(mhor1))

#phys activity
phys2 <- svyglm(sex_partners ~ vigorous_rec + moderate_rec + vigorous_work + moderate_work + age_centered*sex + strength_centered*sex + partnered, family=quasipoisson(), design=designsG$d.design.adults)
summary(phys2)

# past year partners
mpy <- svyglm(sex_partners_year ~ age_centered*sex + strength_centered*sex + bmi_centered + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(mpy, df.resid = Inf)

# past year partners (anthropometric model)
manth2 <- svyglm(sex_partners_year ~ age_centered*sex + strength_centered*sex + height_centered*sex + weight_centered + bmi_centered*sex + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(manth2, df.resid = Inf)

manth2.1 <- svyglm(sex_partners_year ~ poly(age_centered, 2) + strength_centered*sex + weight_centered*sex + partnered*strength_centered*sex, family=quasipoisson(), design=designsG$d.design.adults)
summary(manth2.1)
plot(allEffects(manth2.1))
Anova(manth2.1, type = 3)

plot(svysmooth(strength~age, designsG$d.design.adult.female)) #inverted U relationship for both males and females
#bimodalstrengthXsex partner svysmooth relationship, both males and females

m <- svyglm(sex_partners ~ poly(strength_centered, 3)*sex + poly(age_centered, 2)*sex, family = quasipoisson(), design=designsG$d.design.adults)


#age not linearly related to sex partners, nor is strength
plot(svysmooth(sex_partners~testosterone, bandwidth = 40, designsG$d.design.adult.male))
#age effect v cohort effect v period effect

svyhist(~log10(testosterone), designsG$d.design.adult.male)

ggplot(d_G, aes(age, sex_partners, color = sex)) + geom_point() + geom_smooth() + scale_y_log10()

# past year partners (socioeconomic model)
msoc2 <- svyglm(sex_partners_year ~ age_centered*sex + strength_centered*sex + edu + race + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults )
summary(msoc2)

# past year partners (health model)
mheal2 <- svyglm(sex_partners_year ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(mheal2)
nobs(mheal2)

# past year partners (hormone model)
mhor2 <- svyglm(sex_partners_year ~ strength_centered*sex + age_centered*sex + testosterone_centered*sex + partnered*strength_centered,  family=quasipoisson(), design=designsG$d.design.adults)
summary(mhor2, df.resid = Inf)

#phys activity past year
phys3 <- svyglm(sex_partners_year ~ vigorous_rec + moderate_rec + vigorous_work + moderate_work + age_centered*sex + strength_centered*sex + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(phys3)

#age first sex
age <- svyglm(age_first_sex ~ strength_centered*sex + age_centered*sex, family=gaussian(), design=designsG$d.design.adults)
summary(age)

#age first sex (anthropometric)
age2 <- svyglm(age_first_sex ~ age_centered*sex + strength_centered*sex + height_centered*sex + weight_centered + bmi_centered*sex, family=gaussian(), design=designsG$d.design.adults)
summary(age2)

#age first sex (socioeconomic)
age3 <- svyglm(age_first_sex ~ age_centered*sex + strength_centered*sex + race + edu, family=gaussian(), design=designsG$d.design.adults )
summary(age3)

m1 <- svyglm(strength_centered ~ race*sex, family=gaussian(), design=designsG$d.design.adults)
summary(m1)
plot(allEffects(m1))

#age first sex (health)
age4<- svyglm(age_first_sex ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression, family=gaussian(), design=designsG$d.design.adults)
summary(age4)

#age first sex (hormone)
age5 <- svyglm(age_first_sex ~ strength_centered*sex + age_centered*sex + testosterone*sex,  family=gaussian(), design=designsG$d.design.adults)
summary(age5)

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



# number of sex partners past year (heterosexual)
m <- svyglm(sex_partners_year ~ age_centered + strength_centered * sex + partnered*strength_centered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m)

#lifetime vaginal partners (men)
m1 <- svyglm(SXQ824 ~ age_centered + strength_centered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m1)

#lifetime vaginal partners (females)
m2 <- svyglm(SXQ724 ~ age_centered + strength_centered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m2)

#ever had sex, both males and females
m3 <- svyglm(SXD021==1 ~ age_centered + strength_centered + sex, family = quasibinomial(), design= designsG$d.design.adults)
summary(m3)
exp(coef(m3))

#past year vaginal sex partners (males)
m4 <- svyglm(SXQ827 ~ age_centered*partnered + strength_centered * partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m4)

#past year vaginal sex partners (females)
m5 <- svyglm(SXQ727 ~ age_centered + strength_centered + partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m5)

#lifetime number female partners any sex (males)
m6 <- svyglm(SXD171 ~ age_centered + strength_centered * partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m6)

#lifetime number male partners any sex (females)
m7 <- svyglm(SXD101 ~ age_centered + strength_centered * partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m7)

#sexual orientation males
m8 <- svyglm(SXQ292==1 ~ age_centered + strength_centered, family = quasibinomial(), design= designsG$d.design.adults)
summary(m8)
exp(coef(m8))

#sexual orientation females
m9 <- svyglm(SXQ294==1 ~ age_centered + strength_centered, family = quasibinomial(), design= designsG$d.design.adults)
summary(m9)

#partners 5 or more years older past year
m10 <- svyglm(SXQ590 ~ age_centered*sex + strength_centered * sex + partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m10)

#partners 5 or more years younger past year
m11 <- svyglm(SXQ600 ~ age_centered*sex + strength_centered * age_centered + partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m11)

#wbcc
m12 <- svyglm(whitebloodcell ~ strength_centered*sex + age_centered, family= quasipoisson(), design=designsG$d.design.adults)
summary(m12)


#correlation matrix
d <-
  d_G %>%
  dplyr::filter(age>=18) %>%
  dplyr::select(where(is.numeric), sex, -SEQN, -SDMVPSU, -SDMVSTRA, -WTINT2YR,-WTMEC2YR) %>%
  mutate(sex=as.numeric(sex)) %>%
  cor(use= "pairwise.complete.obs") %>%
  as_tibble(rownames="variable") %>%
  pivot_longer(-variable) %>%
  dplyr::filter(variable=="strength", !str_detect(name, "MGX"), name != "strength")


x <- d$value
names(x) <- d$name
ggdotchart(x[x< -.2])

d_female <-
  d_G %>%
  dplyr::filter(age>=18, sex == "female") %>%
  dplyr::select(where(is.numeric), -SEQN, -SDMVPSU, -SDMVSTRA, -WTINT2YR,-WTMEC2YR) %>%
  cor(use= "pairwise.complete.obs") %>%
  as_tibble(rownames="variable") %>%
  pivot_longer(-variable) %>%
  dplyr::filter(variable=="strength", !str_detect(name, "MGX"), name != "strength", !is.na(value))

x <- d_female$value
names(x) <- d_female$name
fem_neg <- ggdotchart(x[x< -.1])
fem_pos <- ggdotchart(x[x >.1])

d_male <-
  d_G %>%
  dplyr::filter(age>=18, sex == "male") %>%
  dplyr::select(where(is.numeric), -SEQN, -SDMVPSU, -SDMVSTRA, -WTINT2YR,-WTMEC2YR, -pregnant) %>%
  cor(use= "pairwise.complete.obs") %>%
  as_tibble(rownames="variable") %>%
  pivot_longer(-variable) %>%
  dplyr::filter(variable=="strength", !str_detect(name, "MGX"), name != "strength", !is.na(value))

x <- d_male$value
names(x) <- d_male$name
mal_neg <- ggdotchart(x[x < -.1])
mal_pos <- ggdotchart(x[x > .1])

(fem_pos + mal_pos) /
  (fem_neg + mal_neg)

paq <- read.xport("../nhanesGH/data-raw/NHANES data/PAQ_G.XPT")
names(paq)
# m <- prcomp(na.omit(paq[-c(1,21)]), scale. = TRUE)


# presentation ------------------------------------------------------------

# lifetime number of sex partners (heterosexual, any sex)
# In your lifetime, with how many [women/men] have you had any kind of sex?
# SXD171 - # female sex partners/lifetime (Males 18-69)
# SXD101 - # male sex partners/lifetime (Females 18-69)
# sex_partners = ifelse(RIAGENDR == 2, SXD101, SXD171)

#designsG$d.design.adults <- update(designsG$d.design.adults, sex_partners2 = ifelse(sex_partners>100, 100, sex_partners))

library(MASS)
# des <- update(des, scaledweights = WTINT2YR/mean(WTINT2YR))
# model0 <- glm.nb(total~factor(R1AGENDR) * (log(age)+factor(RIDRETHI)) , data=model.frane(des))
# model1 <- glm.nb(total~factor(RIAGENDR)*(log(age)+factor(RIDRETHl)), data=model.frame(des) , weights=scaledweights)
# model2 <- svyglm(total~factor(RIAGENDR)*(log(age)+factor(RIDRETHl)), design=des, family=quasipoisson)

des <- update(designsG$d.design.adults, scaledweights = WTINT2YR/mean(WTINT2YR))

m0 <- glm.nb(sex_partners ~ age_centered*sex + strength_centered * sex + bmi_centered + edu + partnered, data = model.frame(des))
m1 <- glm.nb(sex_partners ~ age_centered*sex + strength_centered * sex + bmi_centered + edu + partnered, data = model.frame(des), weights=scaledweights)
m2 <- svyglm(sex_partners ~ age_centered*sex + strength_centered * sex + bmi_centered + edu + partnered, family = quasipoisson(), design= des)

round(cbind(coef(m0), coef(m1), coef(m2)), 2)
summary(m0)

ggplot(d_G, aes(age, sex_partners, colour = sex)) + geom_point() + geom_smooth() + scale_y_log10()

m <- svyglm(sex_partners ~ age_centered*sex + strength_centered*sex + bmi_centered + edu + partnered, family = quasipoisson(), design= designsG$d.design.adults)
summary(m)
plot(allEffects(m))
nobs(m)

# m <- svyglm.nb(sex_partners ~ age_centered + strength_centered * sex + bmi_centered + edu + partnered, design= designsG$d.design.adults)
# summary(m)
# plot(allEffects(m))
#
# m <- MASS::glm.nb(sex_partners ~ age*sex + strength*sex + bmi + edu + partnered, data = d_G, subset = age<=59)
# summary(m)
# plot(allEffects(m))

# m <- glm(sex_partners ~ age + strength * sex + bmi + edu + partnered, family = quasipoisson(), data = d_G, subset = age <=59)
# summary(m)
# plot(allEffects(m))

# m1 <- svyglm(log10(sex_partners2 + 1) ~ age_centered + strength_centered * sex + bmi_centered + edu + partnered, family = gaussian(), design= designsG$d.design.adults)
# summary(m1)
# plot(allEffects(m))


#vaginal_sex_partners = ifelse(RIAGENDR == 2, SXQ724, SXQ824)

svyhist(~sex_partners_year, design=designsG$d.design.adults)

ggplot(d_G, aes(strength, sex_partners_year, colour = sex)) + geom_point() + geom_smooth()

# past year partners
mpy <- svyglm(sex_partners_year ~ age_centered*sex  + bmi_centered + edu + strength_centered*partnered + strength_centered*sex, family=quasipoisson(), design=designsG$d.design.adults)
summary(mpy)
plot(allEffects(mpy))
nobs(mpy)
visreg(mpy, xvar = "strength_centered", by = "sex", scale = "response", gg = TRUE) + ylim(0,20)


# zero inflated model?

m <- glm(sex_partners_year ~ age_centered*sex  + bmi + edu + partnered*strength + strength_centered*sex, family = quasipoisson(), data = d_G, subset = age <=59)
summary(m)
plot(allEffects(m))

m <- svyglm(sex_partners_year ~  bmi_centered + edu + partnered*strength + age_centered*sex + strength_centered*sex, family = quasipoisson(), design=designsG$d.design.adults)
summary(m)
plot(allEffects(m))


m <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + edu + bmi_centered, family = binomial, designsG$d.design.adults)
summary(m)
plot(allEffects(m))
nobs(m)
mheal2 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression, family=binomial(), design=designsG$d.design.adults)
summary(mheal2)


#white blood cells

#sexual dimorphism reduces post menopause, control?
#time of day
#neutrophil to lymphocyte ratio

m2 <- svyglm(whitebloodcell ~ bmi_centered + age*sex + strength_centered*sex, family = gaussian(), design = designsG$d.design.adults)
summary(m2)
plot(allEffects(m2))
nobs(m2)


# cals

m3 <- svyglm(avgcalories ~ bmi_centered + age*sex + strength_centered*sex, family = gaussian(), design = designsG$d.design.adults)
summary(m3)
plot(allEffects(m3))

m4 <- svyglm(avgcalories ~ bmi_centered + age*sex + strength_centered*sex + vigorous_rec + moderate_rec + vigorous_work + moderate_work, family = gaussian(), design = designsG$d.design.adults)
summary(m4)
plot(allEffects(m4))


# past year partners (health model)
mheal2 <- svyglm(sex_partners_year ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(mheal2, df.resid = Inf)


mheal3 <- svyglm(vaginal_sex_partners_year ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(mheal3, df.resid = Inf)
nobs(mheal2)


m <- svyglm(vaginal_sex_partners ~ age_centered*sex + strength_centered*sex + factor(sexualorientation), family=quasipoisson(), design=designsG$d.design.adults)
summary(m)



mheala <- svyglm(sex_partners ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression + partnered*strength_centered + heterosexual, family=quasipoisson(), design=designsG$d.design.adults)
summary(mheala, df.resid = Inf)


mhealb <- svyglm(vaginal_sex_partners ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                   whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression + partnered*strength_centered, family=quasipoisson(), design=designsG$d.design.adults)
summary(mhealb, df.resid = Inf)

ggplot(d_G, aes(strength, whitebloodcell, colour = sex)) + geom_point() + geom_smooth()
ggplot(d_G, aes(strength, neutrophils, colour = sex)) + geom_point() + geom_smooth()
ggplot(d_G, aes(strength, monocytes, colour = sex)) + geom_point() + geom_smooth()
ggplot(d_G, aes(strength, eosinophils, colour = sex)) + geom_point() + geom_smooth()
ggplot(d_G, aes(strength, basophils, colour = sex)) + geom_point() + geom_smooth()
ggplot(d_G, aes(strength, lymphocytes, colour = sex)) + geom_point() + geom_smooth()




ggplot(d_G, aes(sex_partners, numsamesexpartners, colour = factor(eversamesexpartner))) +
         geom_point() + scale_x_log10() +facet_wrap(~sex) + scale_y_log10() + geom_smooth()

svychisq(~(sex_partners>0 & eversamesexpartner) + sex, design = designsG$d.design.adults)

# 1 = heterosexual, 2 = homosexual, 3 = bi, 4 = something else, 5 = not sure
t <- table(d_G$sexualorientation, d_G$sex, useNA = 'a')

plot(d_G$sexualorientation, d_G$sex_partners)

ggplot(d_G, aes(sexualorientation, sex_partners, colour = sex)) + geom_point() + geom_jitter() + facet_wrap(~sex) + scale_y_log10()

ggplot(d_G, aes(sexualorientation, numsamesexpartners, colour = sex)) + geom_point() + geom_jitter() + facet_grid(~sex)

m <- svyglm(sex_partners ~ age_centered*sex + strength_centered*sex + factor(sexualorientation), family=quasipoisson(), design=designsG$d.design.adults)
summary(m)

ggplot(d_G, aes(sexualorientation, evervaginalsex, colour = sex)) + geom_bar(stat = "identity") + facet_wrap(~eversamesexpartner)


d_G$sexualorientationf <- factor(d_G$sexualorientation)

m <- glm(sex_partners ~ age*sex + strength*sex + sexualorientationf, family=quasipoisson(), data = d_G, subset = age <=59)
summary(m)
plot(allEffects(m))

m <- glm(sex_partners ~ age*sex + strength*sex, family=quasipoisson(), data = d_G, subset = age <=59 & sexualorientationf == 1)
summary(m)
plot(allEffects(m))

#other
#only asks about hormonal birth control pills and depo-provera :|


designsG$d.design.adults %>%
  tbl_svysummary(include = c(age, edu, maritalstatus, income, race, bmi,
                             whitebloodcell, strength, testosterone, partnered,
                             height, weight, special_equipment),
                 statistic = list(all_continuous() ~ "{mean} ({sd})",  # stats and format for continuous columns
                                  all_categorical() ~ "{n} / {N} ({p}%)"),   # stats and format for categorical columns
                 digits = all_continuous() ~ 2,          # rounding for continuous columns
                 #type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
                 type = list(where(is.numeric) ~ "continuous"),
                 label  = list(                                   # display labels for column names
                   age  = "Age (years)",
                   edu = "Education (highest level completed)" ,
                   maritalstatus = "Marital Status",
                   income = "2021 Household Income ($)",
                   bmi = "BMI (kg/m^2)"),
                 missing_text = "Missing"             # how missing values should display
  )

designsG$d.design.adults %>%
  tbl_svysummary(by = sex, include = c(age, edu, maritalstatus, income, race, bmi,
                             whitebloodcell, strength, testosterone, partnered,
                             height, weight, special_equipment))
