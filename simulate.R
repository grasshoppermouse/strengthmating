#+ message=F, warning=F
library(tidyverse)
library(nhanesGH)
library(survey)
library(boot)
library(furrr)

plan("multisession")

d_adult <- d_G[d_G$age >= 18 & d_G$age <= 60,]
designsG$d.design.adults <- subset(designsG$d.design.adults, sex_partners < 100)

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

summary(m_pastyear, df.resid = Inf)

# Sample size in H series
N_year = length(na.omit(d_H$sex_partners_year[d_H$sex_partners<=100]))

# With parameters equal to those in the G series data
year_params <- list(
  N = N_year, # Sample size in H series
  b0 = 0.40,
  b_sex = -0.29,
  b_age = -0.39,
  b_partnered = -0.21,
  b_strength = 0.30,
  b_sex_strength = -0.225, # Set to the negative 3/4 of the male value
  b_sex_age = 0.01,
  b_strength_partnered = -0.23,
  theta_year = 2
)

pvalues_year <- future_map_dfr(1:1000, ~getstats_year(year_params), .options = furrr_options(seed = T))
sum(pvalues_year$'strength_centered' < 0.05)/1000
sum(pvalues_year$'sex:strength_centered' < 0.05)/1000

# With strength parameter set to 3/4 of the G series
year_params <- list(
  N = N_year, # Sample size in H series
  b0 = 0.40,
  b_sex = -0.29,
  b_age = -0.39,
  b_partnered = -0.21,
  b_strength = 0.225,
  b_sex_strength = -0.17, # Set to the negative of the male value
  b_sex_age = 0.01,
  b_strength_partnered = -0.23,
  theta_year = 2
)

pvalues_year <- future_map_dfr(1:1000, ~getstats_year(year_params), .options = furrr_options(seed = T))
sum(pvalues_year$'strength_centered' < 0.05)/1000
sum(pvalues_year$'sex:strength_centered' < 0.05)/1000


# lifetime

summary(m_lifetime, df.resid = Inf)

# Sample size lifetime
N_life <- length(na.omit(d_H$sex_partners[d_H$sex_partners<=100]))

# With strength params set to 3/4 of that in G series
life_params <- list(
  N = N_life,
  b0 = 2.7,
  b_sex = -0.44,
  b_age = 0.54,
  b_strength = 0.375,
  b_sex_strength = -0.28,
  b_sex_age = -0.37,
  theta_life = 14
)

pvalues_life <- future_map_dfr(1:1000, ~getstats_life(life_params), .options = furrr_options(seed = T))
sum(pvalues_life$'strength_centered' < 0.05)/1000
sum(pvalues_life$'sex:strength_centered' < 0.05)/1000


# Partnered status --------------------------------------------------------

summary(m_partnered, df.resid = Inf)

N_partnered <- length(na.omit(d_H$partnered[d_H$sex_partners<=100]))
partnered_params <- list(
  N = N_partnered,
  b0 = 0.77,
  b_sex = -0.1,
  b_age = 1.5,
  b_strength = 0.75, # About 3/4 of original value
  b_sex_strength = -0.56, # We set this to half or full male value
  b_sex_age = -0.9
)
pvalues_partnered <- future_map_dfr(1:1000, ~getstats_partnered(partnered_params), .options = furrr_options(seed = T))
sum(pvalues_partnered$'strength_centered' < 0.05)/1000
sum(pvalues_partnered$'sex:strength_centered' < 0.05)/1000

