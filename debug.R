library(tidyverse)
library(profvis)
library(survey)
data(api)

x <- list(tibble(v1 = rnorm(10000)))
for (i in 1:100){
  x <- list(x, tibble(v1 = rnorm(10000)))
}

d <- tibble(
  v1 = x,
  v2 = x,
  v3 = x,
  v4 = x
)


p <- profvis({
  names(d)
}, rerun = T)
htmlwidgets::saveWidget(p, "profile.html")
