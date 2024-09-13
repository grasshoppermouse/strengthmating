library(tidyverse)
tibble(
  Sex0 = rbinom(2001, 1, 0.5),
  Sex = ifelse(Sex0 == 0, 'Female', 'Male'),
  Strength = seq(-1, 1, 0.001),
  `Mating success` = Strength*Sex0 + rnorm(2001, 0, 3)
) |> 
  ggplot(aes(Strength, `Mating success`, colour = Sex)) + 
  # geom_line(linewidth = 1) + +
  geom_smooth(method = 'lm') +
  hagenutils::scale_color_binary() +
  theme_minimal(20) +
  theme(
    axis.text = element_blank(),
    # axis.title.y = element_text(angle = 0),
    # legend.position = 'top'
  )
