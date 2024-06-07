
designsH2 <- update_designs(designsH, cutoff = 100)

models_stage2 <- fitmodels(designsH2$d.design.adults)
modelsummaries_stage2 <- getmodelsummaries(models_stage2)
immune_models_stage2 <- fit_immune_models(designsH2$d.design.adults, designsH2$d.design.dietary.adults) # Need to update to dietary design object
intake_models_stage2 <- fit_intake_models(designsH2$d.design.dietary.adults)
d_allstats_stage2 <- allstats(models_stage2)
d_strength_stats_stage2 <- strength_stats(models_stage2)
d_lifetime_stats_stage2 <- lifetime_stats(models_stage2)
marginal_stage2 <- marginals(models_stage2)

plot_allcoefs_stage2 <-
  ggplot(d_allstats_stage2, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
  geom_vline(xintercept=0, linetype='longdash') +
  labs(x='\nEstimate (95% CI)', y='') +
  facet_wrap(~ Outcome, ncol = 4, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank())
plot_allcoefs_stage2
ggsave("Figures/plot_allcoefs_stage2.pdf", plot_allcoefs_stage2, width = 14, height = 8)

plot_coefs_stage2 <-
  ggplot(d_strength_stats_stage2, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(title='Stage 2: Confirmatory study', x = 'Estimate (95% CI)', y = '') +
  facet_grid(Outcome ~ term) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_coefs_stage2
ggsave("Figures/plot_coefs_stage2.pdf", plot_coefs_stage2, width = 10, height = 10)

d_strength_stats_combined <-
bind_rows(
  list(
    Pilot = d_strength_stats,
    Confirmatory = d_strength_stats_stage2
  ),
  .id = 'Stage'
)

plot_coefs_combined <-
  ggplot(d_strength_stats_combined, aes(estimate, Outcome, xmin = conf.low, xmax = conf.high, shape = Significant, colour = Stage)) +
  geom_pointrange(position = position_dodge(width=0.3)) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Estimate (95% CI)', y = '') +
  facet_grid(Controls ~ term) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_coefs_combined


plot_lifetime_coefs_stage2 <-
  ggplot(d_lifetime_stats_stage2, aes(estimate, Controls, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = 'dotted') +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Adjusted odds ratios (95% CI)', y = '') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_lifetime_coefs_stage2

# effects_plots(models_stage2)

# Exploratory

# Cumulative distributions of sex partners ------------------------

plot_sexpartners_stage2 <-
  ggplot(d_H[!is.na(d_H$sex_partners),], aes(sex_partners + 1, colour = sex)) +
  stat_ecdf() +
  geom_vline(xintercept = 100, linetype = 'dotted') +
  annotate("text", label = 'Cutoff', x = 110, y = 0.80, hjust = 0) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 100, 1000)) +
  scale_color_binary() +
  guides(colour = guide_legend(title = '', reverse = T, override.aes = list(linewidth=2))) +
  labs(x = "\nNumber of lifetime sex partners + 1", y = 'ECDF\n') +
  theme_minimal(15)
plot_sexpartners_stage2
ggsave("Figures/plot_sexpartners_stage2.pdf", plot_sexpartners_stage2, width = 9, height = 6)

plot_sexpartners_year_stage2 <-
  ggplot(d_H[!is.na(d_H$sex_partners_year),], aes(sex_partners_year + 1, colour = sex)) +
  stat_ecdf() +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50)) +
  scale_color_binary() +
  guides(colour = guide_legend(title = '', reverse = T, override.aes = list(linewidth=2))) +
  labs(x = "\nNumber of last year sex partners + 1", y = 'ECDF\n') +
  theme_minimal(15)
plot_sexpartners_year_stage2


# Immunity ----------------------------------------------------------------

plot_immune_stage2 <- forestplot(
  immune_models_stage2$mwbc,
  immune_models_stage2$mwbc_alt,
  intercept = F,
  facet = F,
  dodgewidth = .4,
  varnames = vnames,
  modelnames = mmnames) +
  scale_color_binary() +
  ggtitle("Stage 2: Confirmatory") +
  theme_minimal(17) +
  guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T))
plot_immune_stage2


# Energy and protein ------------------------------------------------------

plot_energy_stage2 <- forestplot(
  intake_models_stage2$m_energy,
  intake_models_stage2$m_energy_alt,
  intercept = F,
  facet = F,
  dodgewidth = .4,
  varnames = vnames,
  modelnames = mmnames) +
  scale_color_binary() +
  ggtitle('Stage 2: Confirmatory Energy') +
  theme_bw(17) +
  guides(colour = guide_none(), shape = guide_none())

plot_protein_stage2 <- forestplot(
  intake_models_stage2$m_protein,
  intake_models_stage2$m_protein_alt,
  intercept = F,
  facet = F,
  dodgewidth = .4,
  varnames = vnames,
  modelnames = mmnames) +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
  ggtitle('Stage 2: Confirmatory Protein') +
  theme_bw(17) +
  theme(axis.text.y = element_blank())

plot_diet_stage2 <- plot_energy_stage2 + plot_protein_stage2 + plot_layout(axes = 'collect')
plot_diet_stage2
