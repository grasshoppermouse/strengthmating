# Immune models ------------------------------------------------------------

# design.adults
fit_immune_models <- function(design1, design2){

  list(

    mwbc = svyglm(
      whitebloodcell ~
        age_centered * sex +
        strength_centered * sex +
        bmi_centered,
      family = quasipoisson(),
      design = design1
    ),

    mwbc_alt = svyglm(
      whitebloodcell ~
        age_centered * sex +
        strength_centered * sex +
        weight_centered +
        height_centered +
        testosterone_sex_centered * sex +
        foodinsecurity_adult  +
        avgcalories_centered +
        tot_MET_centered +
        depression,
      family = quasipoisson(),
      design = design2
    )
  )
}

# Dietary Intake models ----------------------------------------------------

# design.dietary.adults
fit_intake_models <- function(design){

  list(

    ## Energy models -----------------------------------------------------------

    m_energy = svyglm(
      avgcalories ~
        age_centered +
        tot_MET_centered  +
        strength_centered +
        bmi_centered  +
        sex,
      family = gaussian(),
      design = design
    ),

    m_energy_alt = svyglm(
      avgcalories ~
        age_centered +
        tot_MET_centered +
        strength_centered +
        sex +
        weight_centered +
        height_centered +
        whitebloodcell_centered +
        foodinsecurity_adult,
      family = gaussian(),
      design = design
    ),

    ## Protein model -----------------------------------------------------------------

    m_protein = svyglm(
      avgprotein ~
        age_centered +
        tot_MET_centered  +
        strength_centered +
        bmi_centered  +
        sex,
      family = gaussian(),
      design = design
    ),

    m_protein_alt = svyglm(
      avgprotein ~
        age_centered +
        tot_MET_centered +
        strength_centered +
        sex +
        weight_centered +
        height_centered +
        whitebloodcell_centered +
        foodinsecurity_adult,
      family = gaussian(),
      design = design
    )
  )
}


strength_stats <- function(models){
  models |>
    mutate(
      Stats = map(Model, extract_stats)
    ) |>
    unnest(Stats) |>
    mutate(
      term = ifelse(term == 'strength_sex_centered', 'Strength (S by sex)', 'Strength (S by sex) X Sex (female)'),
      term = factor(term, levels = c('Strength (S by sex)', 'Strength (S by sex) X Sex (female)')),
      Outcome = factor(Outcome, levels = rev(c('Partnered', 'Lifetime partners\n(partners per year)', 'Past year partners', 'Age of first sex'))),
      Significant = sign(conf.low) == sign(conf.high)
    )
}

lifetime_stats <-function(models){
  models |>
    mutate(
      Stats = map(Model, \(obj) extract_stats(obj, terms = 'sex_partners_scaled'))
    ) |>
    unnest(Stats) |>
    mutate(
      across(estimate:conf.high, exp)
    )
}

extract_stats <- function(obj, terms = c('strength_sex_centered', 'sexfemale:strength_sex_centered', 'strength_sex_centered:sexfemale')){
  obj$df.residual <- Inf
  tidy(obj, conf.int = T) |>
    dplyr::filter(term %in% terms) |>
    dplyr::select(term, estimate, conf.low, conf.high)
}

# Combine all dietary coefficients ----------------------------------------

dietary_coefs <-
  list(
    Energy_1_rep = intake_models$m_energy,
    Energy_1_alt = intake_models$m_energy_alt,
    Energy_2_rep = intake_models_stage2$m_energy,
    Energy_2_alt = intake_models_stage2$m_energy_alt,

    Protein_1_rep = intake_models$m_protein,
    Protein_1_alt = intake_models$m_protein_alt,
    Protein_2_rep = intake_models_stage2$m_protein,
    Protein_2_alt = intake_models_stage2$m_protein_alt
  ) |>
  map(\(m) tidy(m, conf.int = T)) |>
  list_rbind(names_to = 'Model') |>
  separate_wider_delim(Model, delim = '_', names = c('Model', 'Stage', 'Controls')) |>
  dplyr::filter(term != '(Intercept)') |>
  mutate(
    term = vnames[term],
    term = factor(term, levels = rev(unique(term))),
    Stage = ifelse(Stage == 1, 'Stage 1: Pilot', 'Stage 2: Confirmatory'),
    Controls = ifelse(Controls == 'rep', 'Lassek and Gaulin', 'Expanded controls')
  )

plot_energy_coefs <-
  ggplot(dietary_coefs[dietary_coefs$Model == 'Energy',], aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
  xlab('Estimate (95% CI)') +
  ylab('') +
  facet_grid(Model ~ Stage, scales = 'free_x') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_energy_coefs

plot_protein_coefs <-
  ggplot(dietary_coefs[dietary_coefs$Model == 'Protein',], aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
  xlab('Estimate (95% CI)') +
  ylab('') +
  facet_grid(Model ~ Stage, scales = 'free_x') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_protein_coefs
