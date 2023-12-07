library(tidyverse)
library(nhanesGH)

d_adult <- d_G[d_G$age >= 18 & d_G$age < 60,]

m <- glm(sex_partners_year ~ 1, family = quasipoisson, d_adult)
m2 <- glm(sex_partners_year ~ sex*strength + partnered, family = quasipoisson, d_adult)
m3 <- glm(sex_partners_year ~ sex*strength, family = quasipoisson, d_adult)
m4 <- glm(sex_partners_year ~ sex*strength + partnered*strength, family = quasipoisson, d_adult)

m5 <- glm(sex_partners ~ 1, family = quasipoisson, d_adult)
m6 <- glm(sex_partners ~ sex*strength + age, family = quasipoisson, d_adult)

rqpois <- function(n, mu, theta) rnbinom(n=n, mu=mu, size=mu/(theta-1))


simdata <- function(N, b0, b1, b2, b3){
  tibble(
    sex = rbinom(N, 1, 0.5),
    strength = ifelse(sex == 1, rnorm(sum(sex), 58.5, 10.76), rnorm(N-sum(sex), 91.99, 17.37)),
    strength_centered = c(scale(strength))/2,
    sex_partners_year = rqpois(N, exp(b0 + b1*sex + b2*strength_centered + b3*sex*strength_centered), theta = 4.7)
  )
}


simdata <- function(N, b0, b1, b2, b3){
  tibble(
    sex = rbinom(N, 1, 0.5),
    strength = ifelse(sex == 1, rnorm(sum(sex), 58.5, 10.76), rnorm(N-sum(sex), 91.99, 17.37)),
    strength_centered = c(scale(strength))/2,
    sex_partners_year = rqpois(N, exp(b0 + b1*sex + b2*strength_centered + b3*sex*strength_centered), theta = 4.7),
    sex_partners  = rqpois(N, exp(b0 + b1*sex + b2*strength_centered + b3*sex*strength_centered), theta = 243.23)
  )
}

d <- simdata(3000, 0.09, -0.76, 0.004, 0.009)
m3 <- glm(sex_partners_year ~ sex*strength, family = quasipoisson, d)

#for past year partner models
getstats <- function(N, b0, b1, b2, b3){
  m <- glm(sex_partners_year ~ sex*strength, family = quasipoisson, simdata(N, 0.09, -0.76, 0.004, 0.009))
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

#for lifetime partner models
getstats2 <- function(N, b0, b1, b2, b3){
  m <- glm(sex_partners ~ sex*strength, family = quasipoisson, simdata(N, 2.5, -0.76, 0.004, 0.009))
  m_sum <- summary(m)
  m_sum$coefficients[, 4]
}

getstats(3000, 0.09, -0.76, 0.004, 0.009)

#past year
pvalues <- map_df(1:1000, ~getstats(5000, 0.09, -0.76, 0.004, -0.004)) #coefs from m2 above
pvalues <- map_df(1:1000, ~getstats(5000, -0.1, -0.52, 0.008, 0.005))

#lifetime
pvalues <- map_df(1:1000, ~getstats2(5000, 2.5, -1.2, 0.009, 0.01)) #coefs from m6 above



sum(pvalues$sex < 0.05)/1000
sum(pvalues$strength < 0.05)/1000
sum(pvalues$'sex:strength' < 0.05)/1000


