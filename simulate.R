library(boot)

set.seed(765456)

d_adult <- d_G[d_G$age >= 18 & d_G$age <= 60,]

rqpois <- function(n, mu, theta) rnbinom(n=n, mu=mu, size=mu/(theta-1))

param_dict <- c(
  "(Intercept)" = "(Intercept)",
  "sexfemale" = "sexfemale",
  "age_centered" = "age_centered",
  "partneredTRUE" = "partneredTRUE",
  "strength_sex_centered" = "strength_sex_centered",
  "sex_partners_scaled" = "sex_partners_scaled",

  "sexfemale:strength_sex_centered" = "SEX_STRENGTH",
  "strength_sex_centered:sexfemale" = "SEX_STRENGTH",
  "age_centered:sexfemale" = "SEX_AGE",
  "sexfemale:age_centered" = "SEX_AGE",
  "strength_sex_centered:partneredTRUE" = "STRENGTH_PARTNERED",
  "partneredTRUE:strength_sex_centered" = "STRENGTH_PARTNERED"
)

simdata <- function(
    N,
    `(Intercept)` = 0,
    sexfemale = 0,
    age_centered = 0,
    partneredTRUE = 0,
    strength_sex_centered = 0,
    sex_partners_scaled = 0,
    SEX_STRENGTH = 0,
    SEX_AGE = 0,
    STRENGTH_PARTNERED = 0,
    theta = 0,
    ...
){

  tibble(

    # Explanatory variables
    sex = rbinom(N, 1, 0.5),
    age = sample(d_adult$age, N, replace = T),
    agecentered = c(scale(age))/2,
    years_sexually_mature = age - 12,
    strength = ifelse(sex == 1, rnorm(sum(sex), 58.5, 10.76), rnorm(N-sum(sex), 91.99, 17.37)),
    strengthcentered = ifelse(sex == 1, c(scale(strength[sex==1]))/2,c(scale(strength[sex==0]))/2),
    strengthcentered2 = c(scale(strength))/2,

    # Models of outcome variables
    partnered = rbinom(N, 1, prob = inv.logit(
      `(Intercept)` +
        sexfemale*sex +
        age_centered*agecentered +
        strength_sex_centered*strengthcentered +
        SEX_STRENGTH*sex*strengthcentered +
        SEX_AGE*sex*agecentered
    )
    ),
    sexpartnersyear = rqpois(N, exp(
      `(Intercept)` +
        sexfemale*sex +
        age_centered*agecentered +
        partneredTRUE*partnered +
        strength_sex_centered*strengthcentered +
        SEX_STRENGTH*sex*strengthcentered +
        SEX_AGE*sex*agecentered +
        STRENGTH_PARTNERED*partnered*strengthcentered),
      theta
    ),
    sexpartners = rqpois(N, exp(
      `(Intercept)` +
        sexfemale*sex +
        log(years_sexually_mature) +
        strength_sex_centered*strengthcentered +
        SEX_STRENGTH*sex*strengthcentered +
        SEX_AGE*agecentered*sex +
        partneredTRUE*partnered
    ),
    theta
    )
  )
}

#for past year partner models
getstats_year <- function(params){
  d <- do.call(simdata, params)
  m <- glm(sexpartnersyear ~ sex*strengthcentered + partnered*strengthcentered + sex*agecentered, family = quasipoisson, d)
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

#for lifetime partner models
getstats_life <- function(params){
  d <- do.call(simdata, params)
  m <- glm(sexpartners ~ sex*strengthcentered + partnered + offset(log(years_sexually_mature)), family = quasipoisson, d)
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
  names(params) <- param_dict[names(params)]
  params$N <- N
  params$strength_sex_centered <- scale_effect * params$strength_sex_centered
  params$SEX_STRENGTH <- scale_effect * params$SEX_STRENGTH
  params$theta <- summary(model)$dispersion[1]
  pvalues <- map_dfr(1:1000, ~getstatsfunction(params))
  data.frame(
    strength = sum(pvalues$strengthcentered < 0.05)/1000,
    `sex X strength` = sum(pvalues$`sex:strengthcentered` < 0.05)/1000,
    check.names = F
  )
}

# Sample size
N <- sum(d_H$sex_partners < 100, na.rm = T)
scale_fctrs <- seq(0.25, 1, 0.25)

pwr_partnered <- map(scale_fctrs, \(x) pwr(N, models$Model$manth4, getstats_partnered, 'Partnered', scale_effect = x)) |> list_rbind()
pwr_lifetime <- map(scale_fctrs, \(x) pwr(N, models$Model$manth1, getstats_life, 'Lifetime partners', scale_effect = x)) |> list_rbind()
pwr_pastyear <- map(scale_fctrs, \(x) pwr(N, models$Model$manth2, getstats_year, 'Last year partners', scale_effect = x)) |> list_rbind()

df_pwr <- bind_cols(
  Scale = scale_fctrs,
  pwr_partnered,
  pwr_lifetime,
  pwr_pastyear,
  .name_repair = 'minimal'
)
