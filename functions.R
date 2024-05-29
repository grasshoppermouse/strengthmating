library(tidyverse)
library(survey)
library(nhanesGH)
library(broom)
library(modelsummary)

name_dict <- c(
  manth = 'Anthropomentric',
  mheal = 'Health',
  mhor = 'Hormone',
  mphys = 'Activity',
  msoc = 'Socioeconomic'
)

outcome_dict <- c(
  "1" = "Lifetime partners",
  "2" = "Past year partners",
  "3" = "Age of first sex",
  "4" = "Partnered"
)

# Coef plot labels --------------------------------------------------------------

mnames1 <- c(
  "Age at first intercourse",
  "Lifetime Number of Sexual Partners",
  "Past Year Number of Sexual Partners",
  "Currently Partnered"
)

mnames <- c(
  "Anthropometric",
  "Socieoeconomic",
  "Health",
  "Physical Activity",
  "Hormone"
)

mmnames <- c(
  "Replication",
  "Expanded Controls"
)

vnames <- c(
  "strength_centered" = "Strength (S by sex)",
  "strength_centered2" = "Strength (S)",
  "strength" = "Strength",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "sexfemale:strength_centered2" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "age" = "Age",
  "partneredTRUE" = "Partnered",
  "sex_partners" = "Lifetime sex partners",
  "sex_partners_scaled" = "Lifetime sex partners (S)",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age (S) x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "bmi" = "BMI",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI (S)",
  "sexfemale:height_centered" = "Sex (Female) x Height (S)",
  "sexfemale:weight_centered" = "Sex (Female) x Weight (S)",
  'perceived_abnormal_weightTRUE' = "Perceived Abnormal Weight",
  "whitebloodcell_centered" = "White Blood Cell Count (S)",
  "whitebloodcell" = "White Blood Cell Count",
  "hemoglobin_centered" = "Hemoglobin (S)",
  "hemoglobin" = "Hemoglobin",
  "special_equipmentTRUE" = "Need special equip to walk",
  "chronic_disease_score" = "Chronic Disease Score",
  "physical_disease_count" = "Physical Disease Count",
  "depression" = "Depression Score",
  "log(testosterone)" = "Testosterone (log)",
  "sexfemale:log(testosterone)" = "Sex (Female) x Testosterone (log)",
  "testosterone_sex_centered" = "Testosterone (S by Sex)",
  "testosterone" = "Testosterone",
  "sexfemale:testosterone_sex_centered"= "Sex (Female) x Testosterone (S by Sex)",
  "vigorous_rec" = "Vigorous Recreation",
  "moderate_rec" = "Moderate Recreation",
  "vigorous_work" = "Vigorous Work",
  "moderate_work" = "Moderate Work",
  "edu" = "Education",
  "raceOtherHispanic" = "Other Hispanic",
  "raceNonHispanicBlack" = "Non-Hispanic Black",
  "raceNonHispanicAsian" = "Non-Hispanic Asian",
  "raceNonHispanicWhite" = "Non-Hispanic White",
  "raceOtherRace" = "Other Race",
  "foodinsecurity_adult" = "Food Insecurity",
  "tot_MET_centered" = "Total MET (S)",
  "avgcalories_centered" = "Average calories per day (S)"
)

# Mating regression models -------------------------------------------------------

fitmodels <- function(design){

  models0 <- list(

    ## Lifetime partner models -------------------------------------------------

    # Anthropometric
    manth1 = svyglm(
      sex_partners ~
        age_centered * sex +
        strength_centered * sex +
        partnered +
        bmi_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Socioeconomic model
    msoc1 = svyglm(
      sex_partners ~
        age_centered * sex +
        strength_centered * sex +
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
        strength_centered * sex +
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
        strength_centered * sex +
        partnered +
        testosterone_sex_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Physical activity
    mphys1 = svyglm(
      sex_partners ~
        age_centered * sex +
        strength_centered * sex +
        partnered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasipoisson(),
      design = design
    ),

    ## Past year partner models ------------------------------------------------

    # Anthropometric model
    manth2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_centered * sex +
        partnered * strength_centered + #keeping partnered x strength interaction only for this model
        bmi_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Socioeconomic model
    msoc2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_centered * sex +
        partnered * strength_centered +
        edu +
        race,
      family = quasipoisson(),
      design = design
    ),

    # Health model
    mheal2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_centered * sex +
        partnered * strength_centered +
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

    mhor2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_centered * sex +
        partnered * strength_centered +
        testosterone_sex_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Physical activity
    mphys2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_centered * sex +
        partnered * strength_centered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasipoisson(),
      design = design
    ),

    ## Age at first sex models -------------------------------------------------

    # Anthropometric
    manth3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_centered * sex +
        partnered  +
        bmi_centered * sex,
      family = gaussian(),
      design = design
    ),

    # Socioeconomic
    msoc3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_centered * sex +
        partnered +
        race +
        edu,
      family = gaussian(),
      design = design
    ),

    # Health
    mheal3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_centered * sex +
        partnered +
        perceived_abnormal_weight +
        whitebloodcell_centered +
        hemoglobin_centered +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = gaussian(),
      design = design
    ),

    mhor3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_centered * sex +
        partnered +
        testosterone_sex_centered * sex,
      family = gaussian(),
      design = design
    ),

    # Physical activity
    mphys3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_centered * sex +
        partnered +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = gaussian(),
      design = design
    ),

    ## Partnered models --------------------------------------------------------

    # Anthropometric model
    manth4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_centered * sex +
        bmi_centered * sex,
      family = quasibinomial(),
      design = design
    ),

    # Socioeconomic model
    msoc4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_centered * sex +
        edu +
        race,
      family = quasibinomial(),
      design = design
    ),

    # Health
    mheal4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_centered * sex +
        perceived_abnormal_weight +
        whitebloodcell_centered +
        hemoglobin_centered +
        special_equipment +
        chronic_disease_score +
        physical_disease_count +
        depression,
      family = quasibinomial(),
      design = design
    ),

    # Hormone
    mhor4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_centered * sex +
        testosterone_sex_centered * sex,
      family = quasibinomial(),
      design = design
    ),

    # Physical activity
    mphys4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_centered * sex +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasibinomial(),
      design = design
    )
  )

  modelnames <- names(models0)

  tibble(
    Controls = name_dict[str_remove(modelnames, "\\d")],
    Outcome = outcome_dict[str_extract(modelnames, "\\d")],
    Model = models0,
    Stats = map(Model, \(x) {x$df.residual <- Inf; tidy(x, conf.int = T)})
  )
}


# Immune models ------------------------------------------------------------

# design.adults
fit_immune_models <- function(design){

  list(

    mwbc = svyglm(
      whitebloodcell ~
        age_centered * sex +
        strength_centered2 * sex +
        bmi_centered,
      family= quasipoisson(),
      design=design
    ),

    mwbc_alt = svyglm(
      whitebloodcell ~
        age_centered * sex +
        strength_centered2 * sex +
        weight_centered +
        height_centered +
        testosterone_sex_centered * sex +
        foodinsecurity_adult  +
        avgcalories_centered +
        tot_MET_centered +
        depression,
      family= quasipoisson(),
      design=design
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
        strength_centered2 +
        bmi_centered  +
        sex,
      family = gaussian(),
      design = design
    ),

    m_energy_alt = svyglm(
      avgcalories ~
        age_centered +
        tot_MET_centered +
        strength_centered2 +
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
        strength_centered2 +
        bmi_centered  +
        sex,
      family = gaussian(),
      design = design
    ),

    m_protein_alt = svyglm(
      avgprotein ~
        age_centered +
        tot_MET_centered +
        strength_centered2 +
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

# Extract model stats -----------------------------------------------------

extract_stats <- function(obj, terms = c('strength_centered', 'sexfemale:strength_centered')){
  obj$df.residual <- Inf
  tidy(obj, conf.int = T) |>
    dplyr::filter(term %in% terms) |>
    dplyr::select(term, estimate, conf.low, conf.high)
}

custommodelsummary <- function(Controls, Model){
  models <- as.list(Model)
  names(models) <- Controls
  list(suppressWarnings(modelsummary(models, coef_rename = vnames, gof_map = "nobs")))
}

getmodelsummaries <- function(models){
  models |>
    group_by(Outcome) |>
    summarise(
      summary = custommodelsummary(Controls, Model),
      title = paste(unique(Outcome), '(', toupper(Model[[1]]$family$family), ')')
    )
}

allstats <- function(models){
  models |>
    mutate(
      Stats = map(Model, \(m) {m$df.residual <- Inf; tidy(m, conf.int = T)})
    ) |>
    unnest(Stats) |>
    dplyr::select(-Model) |>  # Remove due to RStudio bug
    dplyr::filter(term != "(Intercept)") |>
    mutate(
      Outcome = factor(Outcome, levels = c('Partnered', 'Lifetime partners', 'Past year partners', 'Age of first sex')),
      term = factor(term, levels=rev(names(vnames)), labels=rev(vnames))
    )
}

strength_stats <- function(models){
  models |>
    mutate(
      Stats = map(Model, extract_stats)
    ) |>
    unnest(Stats) |>
    mutate(
      term = ifelse(term == 'strength_centered', 'Strength (S)', 'Strength (S) X Sex (female)'),
      term = factor(term, levels = c('Strength (S)', 'Strength (S) X Sex (female)')),
      Outcome = factor(Outcome, levels = rev(c('Partnered', 'Lifetime partners', 'Past year partners', 'Age of first sex'))),
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

marginals <- function(models){
  bind_rows(
    avg_comparisons(models$Model$manth4, variables = list(strength_centered = 1), by = 'sex', wts = "(weights)") |> mutate(Model = 'Partnered'),
    avg_comparisons(models$Model$manth1, variables = list(strength_centered = 1), by = c('sex', 'partnered'), wts = "(weights)") |> mutate(Model = 'Lifetime'),
    avg_comparisons(models$Model$manth2, variables = list(strength_centered = 1), by = c('sex', 'partnered'), wts = "(weights)") |> mutate(Model = 'Past year'),
    avg_comparisons(models$Model$manth3, variables = list(strength_centered = 1), by = 'sex', wts = "(weights)") |> mutate(Model = 'Age first sex')
  ) |>
    dplyr::select(sex, partnered, estimate)
}
