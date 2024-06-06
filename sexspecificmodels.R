# library(tidyverse)
# library(nhanesGH)
# library(survey)
# library(broom)

# These models were fit at the request of an anonymous reviewer

sex_specific_models <- function(design){

  # Removing those with >=100 lifetime sex partners
  design$d.design.adult.female <- subset(design$d.design.adult.female, sex_partners < 100)
  design$d.design.adult.male <- subset(design$d.design.adult.male, sex_partners < 100)

  # Scale sex_partners by the interquartile range of all adults
  design$d.design.adult.female <- update(design$d.design.adult.female, sex_partners_scaled = sex_partners/10)
  design$d.design.adult.male <- update(design$d.design.adult.male, sex_partners_scaled = sex_partners/10)

  # Sexually mature
  design$d.design.adult.female <- update(design$d.design.adult.female, years_sexually_mature = age - 12)
  design$d.design.adult.male <- update(design$d.design.adult.male, years_sexually_mature = age - 12)

  sexspecificmodels <- list(
    # Lifetime partners models -------------------------------------------------

    # Anthropometric
    manth_1_f = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        bmi,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    manth_1_m = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        bmi,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Socioeconomic model
    msoc_1_f = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        edu +
        race,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    msoc_1_m = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        edu +
        race,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Health model
    mheal_1_f = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    mheal_1_m = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    mhor_1_f = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        testosterone,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    mhor_1_m = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        testosterone,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Physical activity
    mphys_1_f = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    mphys_1_m = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength +
        partnered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Past year partners models ------------------------------------------------

    # Anthropometric model
    manth_2_f = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        bmi,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    manth_2_m = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        bmi,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Socioeconomic model
    msoc_2_f = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        edu +
        race,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    msoc_2_m = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        edu +
        race,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Health model
    mheal_2_f = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    mheal_2_m = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    mhor_2_f = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        testosterone,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    mhor_2_m = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        testosterone,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Physical activity
    mphys_2_f = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasipoisson(),
      design = design$d.design.adult.female
    ),

    mphys_2_m = svyglm(
      sex_partners_year ~
        age_centered +
        strength +
        partnered * strength +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasipoisson(),
      design = design$d.design.adult.male
    ),

    # Age at first sex models -------------------------------------------------

    # Anthropometric
    manth_3_f = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        bmi,
      family = gaussian(),
      design = design$d.design.adult.female
    ),

    manth_3_m = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        bmi,
      family = gaussian(),
      design = design$d.design.adult.male
    ),

    # Socioeconomic
    msoc_3_f = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        race +
        edu,
      family = gaussian(),
      design = design$d.design.adult.female
    ),

    msoc_3_m = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        race +
        edu,
      family = gaussian(),
      design = design$d.design.adult.male
    ),

    # Health
    mheal_3_f = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = gaussian(),
      design = design$d.design.adult.female
    ),

    mheal_3_m = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = gaussian(),
      design = design$d.design.adult.male
    ),

    mhor_3_f = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        testosterone,
      family = gaussian(),
      design = design$d.design.adult.female
    ),

    mhor_3_m = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        testosterone,
      family = gaussian(),
      design = design$d.design.adult.male
    ),

    # Physical activity
    mphys_3_f = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = gaussian(),
      design = design$d.design.adult.female
    ),

    mphys_3_m = svyglm(
      age_first_sex ~
        age +
        strength +
        partnered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = gaussian(),
      design = design$d.design.adult.male
    ),

    # Partnered models --------------------------------------------------------

    # Anthropometric model
    manth_4_f = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        bmi,
      family = quasibinomial(),
      design = design$d.design.adult.female
    ),

    manth_4_m = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        bmi,
      family = quasibinomial(),
      design = design$d.design.adult.male
    ),

    # Socioeconomic model
    msoc_4_f = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        edu +
        race,
      family = quasibinomial(),
      design = design$d.design.adult.female
    ),

    msoc_4_m = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        edu +
        race,
      family = quasibinomial(),
      design = design$d.design.adult.male
    ),

    # Health
    mheal_4_f = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasibinomial(),
      design = design$d.design.adult.female
    ),

    mheal_4_m = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        perceived_abnormal_weight +
        whitebloodcell +
        hemoglobin +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasibinomial(),
      design = design$d.design.adult.male
    ),

    # Hormone
    mhor_4_f = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        testosterone,
      family = quasibinomial(),
      design = design$d.design.adult.female
    ),

    mhor_4_m = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        testosterone,
      family = quasibinomial(),
      design = design$d.design.adult.male
    ),

    # Physical activity
    mphys_4_f = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasibinomial(),
      design = design$d.design.adult.female
    ),

    mphys_4_m = svyglm(
      partnered ~
        sex_partners_scaled +
        age +
        strength +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasibinomial(),
      design = design$d.design.adult.male
    )
  )

  modelnames <- names(sexspecificmodels)

  dfmodels0 <- tibble(
    Controls = name_dict[str_split_i(modelnames, "_", 1)],
    Outcome = outcome_dict[str_split_i(modelnames, "_", 2)],
    Sex = str_split_i(modelnames, "_", 3),
    Model = sexspecificmodels,
    Stats = map(Model, \(x) {x$df.residual <- Inf; tidy(x, conf.int = T)})
  )

  dfmodelsummaries <-
    dfmodels0 |>
    group_by(Outcome, Sex) |>
    summarise(
      summary = custommodelsummary(Controls, Model),
      title = paste(ifelse(unique(Sex) == 'f', 'Female', 'Male'), unique(Outcome), '(', toupper(Model[[1]]$family$family), ')'),
      .groups = "drop"
    )

  dfmodels <-
    dfmodels0 |>
    unnest(Stats) |>
    dplyr::select(-Model) |>  # Remove due to RStudio bug
    dplyr::filter(term != "(Intercept)") |>
    mutate(
      Outcome = factor(Outcome, levels = c('Partnered', 'Lifetime partners', 'Past year partners', 'Age of first sex')),
      Significant = p.value < 0.05
    )

  strengthstats <-
    dfmodels |>
    dplyr::filter(term == 'strength')

  plot_sexspecific_strength_coefs <-
    ggplot(strengthstats, aes(estimate, Outcome, xmin = conf.low, xmax = conf.high, colour = Sex, shape = Sex, alpha = Significant)) +
    geom_pointrange(position = position_dodge(width = 0.5), linewidth = 1) +
    geom_vline(xintercept = 0, linetype = 'dotted') +
    scale_alpha_ordinal(range = c(0.35, 1)) +
    hagenutils::scale_color_binary() +
    guides(colour = guide_legend(reverse = T), shape = 'none', alpha = 'none') +
    labs(x = '\nStrength coefficient (95% CI)', y = '') +
    facet_wrap(~Controls, ncol = 1) +
    theme_bw(15)

  return(list(summaries = dfmodelsummaries, stats = strengthstats, plot = plot_sexspecific_strength_coefs))
}


