library(mgcv)
library(gratia)

d <- designsGH2$d.design.adults$variables
d$partnered <- factor(d$partnered)
d_males <- d[d$sex == 'male',]
d_females <- d[d$sex == 'female',]


m <- gam(
  sex_partners ~ 
    sex + 
    s(income, by = sex) +
    # edu + 
    race +
    as.numeric(partnered) + 
    te(age, strength_sex_centered, by = sex), 
  # family = quasipoisson,
  family = nb,
  data = d)
summary(m)

draw(m, fun = exp, continuous_fill = scico::scale_fill_scico("vik")) # viridis::scale_fill_viridis(option = 'A'))

vis.gam(
  m, 
  view = c('age', 'strength_sex_centered'), 
  type = 'response', 
  plot.type = 'contour',
  # plot.type = 'persp',
  # theta = 30,
  # phi = 45,
  # ticktype = 'detailed',
  cond = list(sex = 'female'),
  main = 'Females'
)

vis.gam(
  m, 
  view = c('age', 'strength_sex_centered'), 
  type = 'response', 
  plot.type = 'contour',
  # plot.type = 'persp',
  # theta = 30,
  # phi = 45,
  # ticktype = 'detailed',
  cond = list(sex = 'male'),
  main = 'Males'
)

out <- smooth_estimates(m)
out$estimate <- exp(out$.estimate)
ggplot(out, aes(age, strength_sex_centered)) + 
  geom_tile(aes(fill = estimate)) + 
  geom_contour(aes(z = estimate), bins = 20, colour = 'white') +
  geom_point(data = d, aes(age, strength_sex_centered), colour = 'white', alpha = 0.1, inherit.aes = F) +
  viridis::scale_fill_viridis(option = 'A') +
  facet_wrap(~sex) +
  theme_minimal(15)

m2 <- gam(
  sex_partners ~ 
    sex + 
    race +
    as.numeric(partnered) + 
    s(age, by = sex) +
    # offset(log(age)) +
    te(income_centered, strength_sex_centered, by = sex), 
  family = nb,
  data = d)
summary(m2)
draw(m2)

vis.gam(
  m2, 
  view = c('income_centered', 'strength_sex_centered'), 
  type = 'response', 
  # plot.type = 'contour',
  plot.type = 'persp',
  theta = 30,
  phi = 15,
  ticktype = 'detailed',
  cond = list(sex = 'male', age = 38),
  main = 'Males'
)

m3 <- (
  sex_gampartners ~ 
    partnered + 
    race +
    s(edu_centered, k=3) +
    s(age, by = partnered) +
    te(income_centered, strength_sex_centered, by = partnered), 
  family = nb,
  data = d_males
)
summary(m3)
draw(m3)

vis.gam(
  m3, 
  view = c('income_centered', 'strength_sex_centered'), 
  type = 'response', 
  # plot.type = 'contour',
  plot.type = 'persp',
  theta = 30,
  phi = 15,
  ticktype = 'detailed',
  cond = list(sex = 'male', age = 38, partnered = T),
  main = 'Males (partnered)'
)

m4 <- gam(
  sex_partners ~ 
    partnered + 
    race +
    s(age, by = partnered) +
    te(income_centered, strength_sex_centered, by = partnered), 
  family = nb,
  data = d_females
)
summary(m4)
draw(m4)

vis.gam(
  m4, 
  view = c('income_centered', 'strength_sex_centered'), 
  type = 'response', 
  # plot.type = 'contour',
  plot.type = 'persp',
  theta = 30,
  phi = 15,
  ticktype = 'detailed',
  cond = list(sex = 'male', age = 38, partnered = F),
  main = 'Females (unpartnered)'
)

m5 <- gam(
  sex_partners ~ 
    partnered + 
    race +
    s(age, by = partnered) +
    s(income) +
    te(edu_centered, strength_sex_centered, by = partnered), 
  family = nb,
  data = d_males
)
summary(m5)
draw(m5)

vis.gam(
  m5, 
  view = c('edu_centered', 'strength_sex_centered'), 
  type = 'response', 
  # plot.type = 'contour',
  plot.type = 'persp',
  theta = 30,
  phi = 15,
  ticktype = 'detailed',
  cond = list(sex = 'male', age = 38, partnered = T),
  main = 'Males (partnered)'
)

m6 <- gam(
  partnered ~ 
    sex +
    s(sex_partners, by = sex) + 
    race +
    s(age, by = sex) +
    s(income_centered, k = 3) +
    te(edu_centered, strength_sex_centered, by = sex),
  family = quasibinomial,
  data = d
)
summary(m6)
draw(m6)

vis.gam(
  m6, 
  view = c('edu_centered', 'strength_sex_centered'), 
  type = 'response', 
  # plot.type = 'contour',
  plot.type = 'persp',
  theta = 30,
  phi = 15,
  ticktype = 'detailed',
  cond = list(sex = 'female', age = 38),
  main = 'Females'
)

msvy <- svyglm(
  sex_partners ~
    strength_sex_centered * sex * income_centered +
    sex * income_centered +
    # strength_sex_centered  +
    race +
    offset(log(years_sexually_mature)),
  family = quasipoisson,
  design = subset(designsH2$d.design.adults, partnered == F)
)
summary(msvy)
