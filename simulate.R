#+ message=F, warning=F
library(tidyverse)
library(nhanesGH)
library(survey)
library(boot)
library(furrr)

source("analysis.R")

plan("multisession")

d_adult <- d_G[d_G$age >= 18 & d_G$age <= 60,]
designsG$d.design.adults <- subset(designsG$d.design.adults, sex_partners < 100)

rqpois <- function(n, mu, theta) rnbinom(n=n, mu=mu, size=mu/(theta-1))

simdata <- function(
    N,
    `(Intercept)`=0,
    sexfemale=0,
    age_centered=0,
    partneredTRUE=0,
    strength_centered=0,
    sex_partners_scaled=0,
    `sexfemale:strength_centered`=0,
    `age_centered:sexfemale`=0,
    `strength_centered:partneredTRUE`=0,
    theta = 0,
    ...
){
  tibble(

    # Explanatory variables
    sex = rbinom(N, 1, 0.5),
    age = rnorm(N, 39, 12.5),
    agecentered = c(scale(age))/2,
    partnered2 = rbinom(N, 1, prob = 0.56),
    strength = ifelse(sex == 1, rnorm(sum(sex), 58.5, 10.76), rnorm(N-sum(sex), 91.99, 17.37)),
    strengthcentered = ifelse(sex ==1, c(scale(strength[sex==1]))/2,c(scale(strength[sex==0]))/2),

    # Models of outcome variables
    sexpartnersyear = rqpois(N, exp(
      `(Intercept)` +
        sexfemale*sex +
        age_centered*agecentered +
        partneredTRUE*partnered2 +
        strength_centered*strengthcentered +
        `sexfemale:strength_centered`*sex*strengthcentered +
        `age_centered:sexfemale`*sex*agecentered +
        `strength_centered:partneredTRUE`*partnered2*strengthcentered),
      theta
    ),
    sexpartners = rqpois(N, exp(
      `(Intercept)` +
        sexfemale*sex +
        age_centered*agecentered +
        strength_centered*strengthcentered +
        `sexfemale:strength_centered`*sex*strengthcentered +
        `age_centered:sexfemale`*agecentered*sex
    ),
    theta
    ),
    partnered = rbinom(N, 1, prob = inv.logit(
      `(Intercept)` +
        sexfemale*sex +
        age_centered*agecentered +
        strength_centered*strengthcentered +
        `sexfemale:strength_centered`*sex*strengthcentered +
        `age_centered:sexfemale`*sex*agecentered
    )
    )
  )
}

#for past year partner models
getstats_year <- function(params){
  d <- do.call(simdata, params)
  m <- glm(sexpartnersyear ~ sex*strengthcentered + partnered2*strengthcentered + sex*agecentered, family = quasipoisson, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

#for lifetime partner models
getstats_life <- function(params){
  d <- do.call(simdata, params)
  m <- glm(sexpartners ~ sex*strengthcentered + sex*agecentered, family = quasipoisson, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

#for partnered models
getstats_partnered <- function(params){
  d <- do.call(simdata, params)
  m <- glm(partnered ~ sex*strengthcentered + sex*agecentered, family = quasibinomial, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

pwr <- function(N, model, getstatsfunction, outcome, scale_effect = 1){
  params <- as.list(coef(model))
  params$N <- N
  params$strength_centered <- scale_effect * params$strength_centered
  params$`sexfemale:strength_centered` <- scale_effect * params$`sexfemale:strength_centered`
  params$theta <- summary(model)$dispersion[1]
  pvalues <- future_map_dfr(1:1000, ~getstatsfunction(params), .options = furrr_options(seed = T))
  data.frame(
    Outcome = outcome,
    scale = scale_effect,
    strength = sum(pvalues$strengthcentered < 0.05)/1000,
    strengthXsex = sum(pvalues$`sex:strengthcentered` < 0.05)/1000
  )
}

# Sample size
N <- sum(d_H$sex_partners <= 100, na.rm = T)

df_pwr <- bind_rows(
  pwr_partnered <- map(seq(0.25, 1, 0.25), \(x) pwr(N, m_partnered, getstats_partnered, 'Partnered', scale_effect = x)) |> list_rbind(),
  pwr_lifetime  <- map(seq(0.25, 1, 0.25), \(x) pwr(N, m_lifetime, getstats_life, 'Lifetime partners', scale_effect = x)) |> list_rbind(),
  pwr_pastyear  <- map(seq(0.25, 1, 0.25), \(x) pwr(N, m_pastyear, getstats_year, 'Last year partners', scale_effect = x)) |> list_rbind()
)
