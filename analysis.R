library(survey)
library(nhanesGH)
library(visreg)
library(ggplot2)
library(hagenutils)
library(tidyverse)
library(patchwork)
library(foreign)

m <- svyglm(partnered ~ strength_centered * sex + age_centered, family = binomial, designsGH$d.design.adults)
summary(m)
visreg(m, xvar = 'strength_centered', by = 'sex', scale = 'response', gg = T) + theme_minimal()

# Males
smooth_partnered_m <- svysmooth(strength ~ age, subset(designsGH$d.design.male, partnered == T))
smooth_unpartnered_m <- svysmooth(strength ~ age, subset(designsGH$d.design.male, partnered == F))
d1 <- svysmooth2df(partnered = smooth_partnered_m, unpartnered = smooth_unpartnered_m)
ggplot(d1, aes(age, strength, colour = Smooth)) +
  geom_line() +
  theme_minimal(15)

# Females
smooth_partnered_f <- svysmooth(strength ~ age, subset(designsGH$d.design.female, partnered == T))
smooth_unpartnered_f <- svysmooth(strength ~ age, subset(designsGH$d.design.female, partnered == F))
d2 <- svysmooth2df(partnered = smooth_partnered_f, unpartnered = smooth_unpartnered_f)
ggplot(d2, aes(age, strength, colour = Smooth)) +
  geom_line() +
  theme_minimal(15)


# Exploratory (G only) ----------------------------------------------------

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

#partnered status - anthropometrics
m13 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + race, family=quasibinomial(), design=designsG$d.design.adults)
summary(m13)
visreg(m13, xvar = "race", scale = "response", rug = FALSE, gg = TRUE) + coord_flip()


visreg(m13, xvar="strength_centered", by = "sex", scale= "response", rug = TRUE, gg= TRUE)
visreg(m13, xvar="age_centered", by = "sex", scale= "response", rug = TRUE, gg= TRUE)

m14 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + height_centered + weight_centered + bmi_centered*sex, family=quasibinomial(), design=designsG$d.design.adults)
summary(m14)

m15 <- svyglm(partnered ~ height_centered*sex + age_centered*sex, family=quasibinomial(), design=designsG$d.design.adults)
summary(m15)

#partnered status - socioeconomic
m16 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + edu + race, family=quasibinomial(), design=designsG$d.design.adults )
summary(m16)

#partnered status - health
m17 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                whitebloodcell + hemoglobin + special_equipment + chronic_disease_score + physical_disease_count, family=quasibinomial(), design=designsG$d.design.adults)
summary(m17)

m18 <- svyglm(partnered ~ age_centered*sex + strength_centered*sex + perceived_abnormal_weight +
                race + special_equipment + whitebloodcell + hemoglobin*sex + chronic_disease_score + physical_disease_count, family=quasibinomial(), design=designsG$d.design.adults)
summary(m18)

m19 <- svyglm(partnered ~ strength_centered*sex + race + hemoglobin_centered*sex + age_centered*sex + special_equipment +
                chronic_disease_score + physical_disease_count + whitebloodcell_centered, family=quasibinomial(), design=designsG$d.design.adults)
summary(m19, df.resid = Inf)

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
m <- prcomp(na.omit(paq[-c(1,21)]), scale. = TRUE)




