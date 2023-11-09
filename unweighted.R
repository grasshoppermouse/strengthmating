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

