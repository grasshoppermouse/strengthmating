# Fit models on combined G & H series data (2011-2014)

designsGH2 <- update_designs(designsGH, cutoff = 100)

modelsGH <- fitmodels(designsGH2$d.design.adults, designsGH2$d.design.dietary.adults, designsGH$d.design.adults)
modelsummariesGH <- getmodelsummaries(modelsGH)
marginalGH <- marginals(modelsGH)

d_allstatsGH <- allstats(modelsGH)

benefit_statsGH <-
  d_allstatsGH |>
  dplyr::filter(benefit_cost == "Benefit")

benefit_stats_strengthGH <-
  benefit_statsGH |>
  dplyr::filter(term2 %in% sex_strength_terms) |>
  mutate(
    term = factor(term, levels = (unique(term)))
  )

plot_strengthGH <-
  ggplot(benefit_stats_strengthGH, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(title = "GH data (2011-2014)", x = "Estimate (95% CI)", y = "") +
  facet_grid(Outcome ~ term) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_strengthGH

# Immunity ------------------------------------------------------

immune_stats_GH <-
  d_allstatsGH |>
  dplyr::filter(Outcome == "Immunity")

immune_stats_strength_GH <-
  immune_stats_GH |>
  dplyr::filter(str_detect(term, "Strength")) |>
  mutate(
    term = factor(term, levels = (unique(term)))
  )

plot_immune_strength_GH <-
  ggplot(immune_stats_strength_GH, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange(position = position_dodge(width = -0.5), linewidth = 2, size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_color_binary() +
  guides(shape = guide_legend(override.aes = list(linetype = 0)), colour = guide_legend(reverse = T)) +
  labs(title = "Combined data (2011-2014)", x = "\nStrength coefficient estimate (95% CI)", y = "") +
  facet_wrap(~term) +
  theme_bw(20) +
  theme(strip.text.y = element_text(angle = 0))
plot_immune_strength_GH

# Energy and protein ------------------------------------------------------

intake_stats_strength_GH <-
  d_allstatsGH |>
  dplyr::filter(Outcome %in% c('Energy (kcal)', 'Protein (g)'), term2 %in% sex_strength_terms)|>
  mutate(
    term = factor(term, levels = (unique(term)))
  )

plot_intake_strength_GH <-
  ggplot(intake_stats_strength_GH, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange(position = position_dodge(width=-0.5), linewidth = 2, size = 1.5) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  # scale_color_binary() +
  guides(shape = guide_legend(override.aes = list(linetype = 0)), colour = guide_legend(reverse = T, override.aes = list(linetype = 0))) +
  labs(title = "Combined data (2011-2014)", x = '\nStrength coefficient estimates (95% CI)', y = '') +
  # facet_grid2(Outcome ~ Stage, scales = 'free_x', independent = 'x') +
  facet_wrap(~Outcome, scales = 'free_x', ncol = 1, strip.position = 'right') +
  theme_bw(20) +
  theme(strip.text.y = element_text(angle = 0))
plot_intake_strength_GH

# Effects plots ------------------------------

plot_effects_lifetime_GH <- 
  plot_predictions(modelsGH$Model$manth1, condition = c("strength_sex_centered", "sex")) +
  scale_color_binary() +
  scale_fill_binary() +
  theme_minimal(20)

plot_effects_partners_GH <- 
  plot_predictions(modelsGH$Model$manth4, condition = c("strength_sex_centered", "sex")) +
  scale_color_binary() +
  scale_fill_binary() +
  theme_minimal(20)
