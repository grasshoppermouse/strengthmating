forestplot(
  mwbc,
  mwbc_alt,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  varnames = vnames)$plot +
  theme_minimal(25) +
  geom_pointrange(size = 2, position = position_dodge(width = .8)) +
  labs(title = "White blood cell count")
