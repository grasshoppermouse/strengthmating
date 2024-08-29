
designsH2 <- update_designs(designsH, cutoff = 100)

models_stage2 <- fitmodels(designsH2$d.design.adults, designsH2$d.design.dietary.adults, designsH$d.design.adults)
modelsummaries_stage2 <- getmodelsummaries(models_stage2)
marginal_stage2 <- marginals(models_stage2)

d_allstats_stage2 <- allstats(models_stage2)

d_allstats_combined <-
  bind_rows(
    "Stage 1: Pilot" = d_allstats,
    "Stage 2: Confirmatory" = d_allstats_stage2,
    .id = 'Stage'
  )

benefit_stats_stage2 <-
  d_allstats_stage2 |>
  dplyr::filter(benefit_cost == 'Benefit')

plot_allcoefs_stage2 <-
  ggplot(benefit_stats_stage2, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
  geom_vline(xintercept=0, linetype='longdash') +
  labs(x='\nEstimate (95% CI)', y='') +
  facet_wrap(~ Outcome, ncol = 4, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank())
plot_allcoefs_stage2
ggsave("Figures/plot_allcoefs_stage2.pdf", plot_allcoefs_stage2, width = 14, height = 8)

benefit_stats_strength_stage2 <-
  benefit_stats_stage2 |>
  dplyr::filter(term2 %in% sex_strength_terms)

plot_coefs_stage2 <-
  ggplot(benefit_stats_strength_stage2, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
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

benefit_strength_stats_combined <-
  d_allstats_combined |>
  dplyr::filter(
    benefit_cost == 'Benefit',
    term2 %in% sex_strength_terms
    ) |>
  mutate(
    term = factor(term, levels = (unique(term)))
  )

plot_coefs_combined <-
  ggplot(benefit_strength_stats_combined, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
  geom_pointrange(position = position_dodge(width=0.5)) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(title = 'Grip strength', x = 'Estimate (95% CI)', y = '') +
  facet_grid(Outcome ~ term) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_coefs_combined
ggsave("Figures/plot_coefs_combined.pdf", width = 12, height = 6)

d_lifetime_stats_combined <-
  d_allstats_combined |>
  dplyr::filter(
    Outcome == 'Partnered',
    term2 == 'sex_partners_scaled'
    ) |>
  mutate(
    across(estimate:conf.high, exp)
  )

plot_lifetime_coefs_combined <-
  ggplot(d_lifetime_stats_combined, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Stage)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 1, linetype = 'dotted') +
  scale_colour_binary() +
  # guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Adjusted odds ratios (95% CI)', y = '') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_lifetime_coefs_combined

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

immune_stats_stage2 <-
  d_allstats_stage2 |>
  dplyr::filter(Outcome == 'Immunity')

immune_stats_strength_stage2 <-
  immune_stats_stage2 |>
  dplyr::filter(str_detect(term, 'Strength'))

immune_stats_strength_combined <-
  d_allstats_combined |>
  dplyr::filter(Outcome == 'Immunity', term2 %in% sex_strength_terms)|>
  mutate(
    term = factor(term, levels = (unique(term)))
  )

plot_immune_strength_combined <-
  ggplot(immune_stats_strength_combined, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, shape = Stage, colour = Significant)) +
  geom_pointrange(position = position_dodge(width=-0.5), linewidth = 2, size = 1.5) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(shape = guide_legend(override.aes = list(linetype = 0)), colour = guide_legend(reverse = T)) +
  labs(x = '\nEstimate (95% CI)', y = '') +
  facet_wrap(~ term) +
  theme_bw(20) +
  theme(strip.text.y = element_text(angle = 0))
plot_immune_strength_combined


immune_stats_combined <-
  d_allstats_combined |>
  dplyr::filter(Outcome == 'Immunity')


plot_immune_combined <-
  ggplot(immune_stats_combined, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(position = position_dodge(width=0.5)) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
  labs(x = '\nEstimate (95% CI)', y = '') +
  facet_wrap(~Stage, scales = 'free_x') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_immune_combined


# plot_immune_stage2 <- forestplot(
#   immune_models_stage2$mwbc,
#   immune_models_stage2$mwbc_alt,
#   intercept = F,
#   facet = F,
#   dodgewidth = .4,
#   varnames = vnames,
#   modelnames = mmnames) +
#   scale_color_binary() +
#   ggtitle("Stage 2: Confirmatory") +
#   theme_minimal(17) +
#   guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T))
# plot_immune_stage2


# Energy and protein ------------------------------------------------------

intake_stats_stage2 <-
  d_allstats_stage2 |>
  dplyr::filter(Outcome %in% c('Energy (kcal)', 'Protein (g)'))

intake_stats_strength_stage2 <-
  intake_stats_stage2 |>
  dplyr::filter(str_detect(term, 'Strength'))

intake_stats_strength_combined <-
  d_allstats_combined |>
  dplyr::filter(Outcome %in% c('Energy (kcal)', 'Protein (g)'), term2 %in% sex_strength_terms)|>
  mutate(
    term = factor(term, levels = (unique(term)))
  )

plot_intake_strength_combined <-
  ggplot(intake_stats_strength_combined, aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
  geom_pointrange(position = position_dodge(width=-0.5), linewidth = 2, size = 1.5) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(shape = guide_legend(override.aes = list(linetype = 0)), colour = guide_legend(reverse = T, override.aes = list(linetype = 0))) +
  labs(x = '\nStrength coefficient estimates (95% CI)', y = '') +
  # facet_grid2(Outcome ~ Stage, scales = 'free_x', independent = 'x') +
  facet_wrap(~Outcome, scales = 'free_x', ncol = 1, strip.position = 'right') +
  theme_bw(20) +
  theme(strip.text.y = element_text(angle = 0))
plot_intake_strength_combined



# Effects plots -----------------------------------------------------------

out <- effects_plots3(models, models_stage2)
out2 <-
  bind_rows(out$Data) |>
  mutate(
    Stage = factor(Stage, levels = c('Pilot', 'Confirmatory'), labels = c('Stage 1: Pilot', 'Stage 2: Confirmatory')),
    Outcome = ifelse(Outcome == 'Lifetime partners\n(partners per year)', 'Lifetime partners\n(partners per year)', Outcome)
  )

plot_effects_all <-
  ggplot(out2, aes(strength_sex_centered, estimate, group=sex)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=sex), alpha=0.2) +
  geom_line(aes(colour=sex), linewidth = 1) +
  scale_color_binary() +
  scale_fill_binary() +
  guides(color = guide_legend(override.aes = list(linewidth=3)), fill = 'none') +
  ylim(0, NA) +
  xlab('Strength (centered by sex)') +
  facet_grid(Outcome~Stage, scales = 'free_y') +
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0),
    strip.text.y = element_text(angle = 0)
  )
ggsave("Figures/plot_effects_all.pdf", plot_effects_all, width = 10, height = 10)
