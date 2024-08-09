

# Cumulative distributions of sex partners ------------------------

series_dict <- c(
  "G" = "Pilot",
  "H" = "Confirmatory"
)

plot_sexpartners <-
  d_GH |> 
  dplyr::filter(!is.na(sex_partners)) |> 
  mutate(
    series2 = paste(str_to_title(sex), series_dict[series]),
    series2 = factor(series2, levels = c("Female Pilot", "Female Confirmatory", "Male Pilot", "Male Confirmatory"))
  ) |> 
  ggplot(aes(sex_partners + 1, colour = series2)) +
  stat_ecdf() +
  geom_vline(xintercept = 100, linetype = 'dotted') +
  annotate("text", label = 'Cutoff', x = 110, y = 0.80, hjust = 0) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 100, 1000)) +
  # scale_color_binary() +
  guides(colour = guide_legend(title = '', reverse = F, override.aes = list(linewidth=2))) +
  labs(x = "\nNumber of lifetime sex partners + 1", y = 'ECDF\n') +
  theme_minimal(15)
plot_sexpartners
ggsave("Figures/plot_sexpartners.pdf", plot_sexpartners, width = 9, height = 6)

plot_sexpartners_year <-
  ggplot(d_G[!is.na(d_G$sex_partners_year),], aes(sex_partners_year + 1, colour = sex)) +
  stat_ecdf() +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50)) +
  scale_color_binary() +
  guides(colour = guide_legend(title = '', reverse = T, override.aes = list(linewidth=2))) +
  labs(x = "\nNumber of last year sex partners + 1", y = 'ECDF\n') +
  theme_minimal(15)
plot_sexpartners_year
# ggsave("Figures/plot_sexpartners_year.pdf", plot_sexpartners_year, width = 9, height = 6)

# Update pilot design objects -----------------------------------

designsG2 <- update_designs(designsG)

# Fit models --------------------------------------------------------------

models <- fitmodels(designsG2$d.design.adults, designsG2$d.design.dietary.adults, designsG$d.design.adults)
modelsummaries <- getmodelsummaries(models)
marginal <- marginals(models)

d_allstats <- allstats(models)

benefit_stats <-
  d_allstats |>
  dplyr::filter(benefit_cost == 'Benefit')

plot_allcoefs <-
  ggplot(benefit_stats, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
  geom_vline(xintercept=0, linetype='longdash') +
  labs(x='\nEstimate (95% CI)', y='') +
  facet_wrap(~ Outcome, ncol = 4, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank())
plot_allcoefs
ggsave("Figures/plot_allcoefs.pdf", plot_allcoefs, width = 14, height = 8)

benefit_stats_strength <-
  benefit_stats |>
  dplyr::filter(term2 %in% sex_strength_terms)

plot_coefs <-
  ggplot(benefit_stats_strength, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = 'none') + # guide_legend(reverse = T)
  labs(title='Stage 1: Pilot study', x = 'Estimate (95% CI)', y = '') +
  facet_grid(Outcome ~ term) +
  theme_bw(15) +
  theme(
    strip.text.y = element_blank()
  )
  # theme(strip.text.y = element_text(angle = 0))
plot_coefs
# ggsave("Figures/plot_coefs.pdf", plot_coefs, width = 10, height = 10)

d_lifetime_stats <-
  d_allstats |>
  dplyr::filter(Outcome == 'Partnered', term2 == 'sex_partners_scaled') |>
  mutate(
    across(estimate:conf.high, exp)
  )

plot_lifetime_coefs <-
  ggplot(d_lifetime_stats, aes(estimate, Controls, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = 'dotted') +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Adjusted odds ratios (95% CI)', y = '') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_lifetime_coefs
ggsave("Figures/plot_lifetime_coefs.pdf", plot_lifetime_coefs)


# Cost plots --------------------------------------------------------------

intake_stats <-
  d_allstats |>
  dplyr::filter(Outcome %in% c('Energy', 'Protein'))

intake_stats_strength <-
  intake_stats |>
  dplyr::filter(str_detect(term, 'Strength'))

ggplot(intake_stats_strength, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = 'none') + # guide_legend(reverse = T)
  labs(title='Stage 1: Pilot study', x = 'Estimate (95% CI)', y = '') +
  facet_grid2(Outcome ~ term, scales = 'free_x', independent = 'x') +
  theme_bw(15) +
  # theme(
  #   strip.text.y = element_blank()
  # )
  theme(strip.text.y = element_text(angle = 0))


# Immune

immune_stats <-
  d_allstats |>
  dplyr::filter(Outcome == 'Immunity')

immune_stats_strength <-
  immune_stats |>
  dplyr::filter(str_detect(term, 'Strength'))

# plot_immune <- forestplot(
#   immune_models[[1]],
#   immune_models[[2]],
#   # immune_models$mwbc,
#   # immune_models$mwbc_alt,
#   intercept = F,
#   facet = F,
#   dodgewidth = .4,
#   varnames = vnames,
#   modelnames = mmnames) +
#   scale_color_binary() +
#   ggtitle("Stage 1: Pilot") +
#   theme_minimal(17)

# plot_energy <- forestplot(
#   intake_models$m_energy,
#   intake_models$m_energy_alt,
#   intercept = F,
#   facet = F,
#   dodgewidth = .4,
#   varnames = vnames,
#   modelnames = mmnames) +
#   scale_color_binary() +
#   guides(colour = 'none', shape = 'none') +
#   ggtitle('Energy (Stage 1: Pilot)') +
#   theme_bw(17)
#
# plot_protein <- forestplot(
#   intake_models$m_protein,
#   intake_models$m_protein_alt,
#   intercept = F,
#   facet = F,
#   dodgewidth = .4,
#   varnames = vnames,
#   modelnames = mmnames) +
#   scale_color_binary() +
#   guides(colour = 'none', shape = 'none') +
#   ggtitle('Protein (Stage 1: Pilot)') +
#   theme_bw(17)

# SI ----------------------------------------------------------------------

