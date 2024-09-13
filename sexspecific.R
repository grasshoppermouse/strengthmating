library(tidyverse)

N = 3000
d <- tibble(
  v1 = rnorm(N),
  v2 = rnorm(N),
  v3 = rnorm(N),
  v4 = rnorm(N),
  sex = rbinom(N, 1, 0.5), # 0: females, 1: males
  males = sum(sex),
  strength = ifelse(sex == 0, rnorm(N - males, -1), rnorm(males, 1)),
  partners = strength * sex + v1 + v2 + v3 + v4 + rnorm(N, 0, 5)
)

m <- lm(partners ~ strength*sex + v1 + v2 + v3 + v4, d)
summary(m)

mf <- lm(partners ~ strength + v1 + v2 + v3 + v4, d, subset = sex == 0)
summary(mf)

mm <- lm(partners ~ strength + v1 + v2 + v3 + v4, d, subset = sex == 1)
summary(mm)
