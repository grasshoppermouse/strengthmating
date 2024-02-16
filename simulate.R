#+ message=F, warning=F
library(tidyverse)
library(nhanesGH)
library(survey)
library(boot)

d_adult <- d_G[d_G$age >= 18 & d_G$age < 60,]

rqpois <- function(n, mu, theta) rnbinom(n=n, mu=mu, size=mu/(theta-1))

simdata <- function(
    N,
    b0=0,
    b_sex=0,
    b_age=0,
    b_partnered=0,
    b_strength=0,
    b_sex_strength=0,
    b_sex_age=0,
    b_strength_partnered=0,
    theta_year = 2.5,
    theta_life = 170
){
  tibble(
    sex = rbinom(N, 1, 0.5),
    age = rnorm(N, 39, 12.5),
    age_centered = c(scale(age))/2,
    partnered = rbinom(N, 1, prob = 0.56),
    strength = ifelse(sex == 1, rnorm(sum(sex), 58.5, 10.76), rnorm(N-sum(sex), 91.99, 17.37)),
    strength_centered = c(scale(strength))/2,
    sex_partners_year = rqpois(N, exp(b0 + b_sex*sex + b_age*age_centered + b_partnered*partnered + b_strength*strength_centered + b_sex_strength*sex*strength_centered + b_sex_age*sex*age_centered + b_strength_partnered*partnered*strength_centered), theta_year),
    sex_partners = rqpois(N, exp(b0 + b_sex*sex + b_age*age_centered + b_strength*strength_centered + b_sex_strength*sex*strength_centered + b_sex_age*sex*age_centered), theta_life),
    partnered2 = rbinom(N, 1, prob = inv.logit(b0 + b_sex*sex + b_age*age_centered + b_strength*strength_centered + b_sex_strength*sex*strength_centered + b_sex_age*sex*age_centered))
  )
}

#for past year partner models
getstats_year <- function(params){
  d <- do.call(simdata, params)
  m <- glm(sex_partners_year ~ sex*strength_centered + partnered*strength_centered + sex*age_centered, family = quasipoisson, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

#for lifetime partner models
getstats_life <- function(params){
  d <- do.call(simdata, params)
  m <- glm(sex_partners ~ sex*strength_centered + sex*age_centered, family = quasipoisson, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

#for partnered models
getstats_partnered <- function(params){
  d <- do.call(simdata, params)
  m <- glm(partnered2 ~ sex*strength_centered + sex*age_centered, family = quasibinomial, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

# past year

# Coefs from this model:

m_year <- svyglm(
  sex_partners_year ~
    strength_centered * sex + age_centered*sex + partnered*strength_centered,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(m_year)

# Sample size in H series
N_year = length(na.omit(d_H$sex_partners_year))

# With parameters equal to those in the G series data
year_params <- list(
  N = N_year, # Sample size in H series
  b0 = 0.43,
  b_sex = -0.23,
  b_age = -0.33,
  b_partnered = -0.27,
  b_strength = 0.43, # 0.43 is value in G series
  b_sex_strength = -0.43, # Set to the negative of the male value
  b_sex_age = -0.09,
  b_strength_partnered = -0.50,
  theta_year = 2.5
)

pvalues_year <- map_df(1:1000, ~getstats_year(year_params))
sum(pvalues_year$'strength_centered' < 0.05)/1000
sum(pvalues_year$'sex:strength_centered' < 0.05)/1000

# With strength parameter set to 3/4 of the G series
year_params <- list(
  N = N_year, # Sample size in H series
  b0 = 0.43,
  b_sex = -0.23,
  b_age = -0.33,
  b_partnered = -0.27,
  b_strength = 0.3, # 0.43 is value in G series
  b_sex_strength = -0.3, # Set to the negative of the male value
  b_sex_age = -0.09,
  b_strength_partnered = -0.50,
  theta_year = 2.5
)

pvalues_year <- map_df(1:1000, ~getstats_year(year_params))
sum(pvalues_year$'strength_centered' < 0.05)/1000
sum(pvalues_year$'sex:strength_centered' < 0.05)/1000


# lifetime

m_life <- svyglm(
  sex_partners ~
    strength_centered * sex +
    age_centered*sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(m_life)

w <- subset(designsG$d.design.adults, sex_partners <= 100)
# w2 <- subset(designsG$d.design.adults, sex_partners_year <= 10)

m_life_w <- svyglm(
  sex_partners ~
    strength_centered * sex +
    age_centered*sex,
  family = quasipoisson(),
  design = w
)
summary(m_life_w)

# Sample size lifetime
N_life <- length(na.omit(d_H$sex_partners[d_H$sex_partners<=100]))

# With strength params set to 0.4 compared to 0.56 in G series
life_params <- list(
  N = N_life,
  b0 = 2.4,
  b_sex = -0.25,
  b_age = 0.55,
  b_strength = 0.4,
  b_sex_strength = -0.4, # We set this to half or full male value
  b_sex_age = -0.4,
  theta_life = 18
)

pvalues_life <- map_df(1:1000, ~getstats_life(life_params))
sum(pvalues_life$'strength_centered' < 0.05)/1000
sum(pvalues_life$'sex:strength_centered' < 0.05)/1000


# Partnered status --------------------------------------------------------

m_partnered <-
  svyglm(
    partnered ~
      age_centered * sex +
      strength_centered * sex,
    family = quasibinomial,
    design = designsG$d.design.adults
  )
summary(m_partnered, df.resid = Inf)

N_partnered <- length(na.omit(d_H$partnered))
partnered_params <- list(
  N = N_partnered,
  b0 = -0.06,
  b_sex = 0.69,
  b_age = 1.2,
  b_strength = 0.75, # About 3/4 of original value
  b_sex_strength = -0.75, # We set this to half or full male value
  b_sex_age = -0.68
)
pvalues_partnered <- map_df(1:1000, ~getstats_partnered(partnered_params))
sum(pvalues_partnered$'strength_centered' < 0.05)/1000
sum(pvalues_partnered$'sex:strength_centered' < 0.05)/1000

