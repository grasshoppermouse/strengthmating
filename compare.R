
design <- designsG$d.design.adults

originalmodels0 <- list(

  ## Lifetime partner models -------------------------------------------------

  # Anthropometric
  manth1 = svyglm(
    sex_partners ~
      age_centered * sex +
      strength_sex_centered * sex +
      partnered +
      bmi_centered * sex,
    family = quasipoisson(),
    design = design
  ),

  # Socioeconomic model
  msoc1 = svyglm(
    sex_partners ~
      age_centered * sex +
      strength_sex_centered * sex +
      partnered +
      edu +
      race,
    family = quasipoisson(),
    design = design
  ),

  # Health model
  mheal1 = svyglm(
    sex_partners ~
      age_centered * sex +
      strength_sex_centered * sex +
      partnered +
      perceived_abnormal_weight +
      whitebloodcell_centered +
      hemoglobin_centered +
      special_equipment +
      chronic_disease_score +
      physical_disease_count +
      depression,
    family = quasipoisson(),
    design = design
  ),

  mhor1 = svyglm(
    sex_partners ~
      age_centered * sex +
      strength_sex_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = design
  ),

  # Physical activity
  mphys1 = svyglm(
    sex_partners ~
      age_centered * sex +
      strength_sex_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = design
  )
)

modelnames <- names(originalmodels0)

originalmodels <- tibble(
  Controls = name_dict[str_remove(modelnames, "\\d")],
  Outcome = outcome_dict[str_extract(modelnames, "\\d")],
  Model = originalmodels0,
  Stats = map(Model, \(x) {x$df.residual <- Inf; tidy(x, conf.int = T)})
)

originalstats <- strength_stats(originalmodels)
newstats <- d_strength_stats |> dplyr::filter(Outcome == 'Lifetime partners (partners per year)')
# newstats <- d_strength_stats_stage2 |> dplyr::filter(Outcome == 'Lifetime partners (partners per year)')

originalstats$type <- 'Original'
newstats$type <- 'New'
thestats <- bind_rows(originalstats, newstats)

plot_compare <-
  ggplot(thestats, aes(estimate, type, xmin = conf.low, xmax = conf.high, colour = type)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  # scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Estimate (95% CI)', y = '') +
  facet_grid(Controls ~ term) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
