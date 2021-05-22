library(survey)
library(nhanesGH)
library(visreg)
library(ggplot2)
library(hagenutils)

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
