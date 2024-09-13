library(tidyverse)
library(survey)
library(nhanesGH)
library(broom)
library(modelsummary)
library(hagenutils)
library(gtsummary)
library(jtools)
library(marginaleffects)
library(tinytable)
library(patchwork)
library(ggcorrplot)
library(ggh4x)

control_dict <- c(
  manth = 'Anthropometric',
  mheal = 'Health',
  mhor = 'Hormone',
  mphys = 'Activity',
  msoc = 'Socioeconomic',
  mrep = 'Lassek and Gaulin',
  mexp = 'Expanded controls'
)

outcome_dict <- c(
  "1" = "Lifetime partners\n(partners per year)",
  "1b" = "Lifetime partners\n(partners per year)",
  "2" = "Past year partners",
  "2b" = "Past year partners",
  "3" = "Age of first sex",
  "3b" = "Age of first sex",
  "4" = "Partnered",
  "4b" = "Partnered",
  "5" = "Immunity",
  "6" = "Energy (kcal)",
  "7" = "Protein (g)"
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
  "strength_sex_centered" = "Grip Strength (S by sex)",
  "strength_centered" = "Grip Strength (S)",
  "strength" = "Grip Strength",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_sex_centered" = "Sex (Female) x Grip Strength (S by sex)",
  "strength_sex_centered:sexfemale" = "Sex (Female) x Grip Strength (S by sex)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength (S)",
  "age_centered" = "Age (S)",
  "age" = "Age",
  "partneredTRUE" = "Partnered",
  "sex_partners" = "Lifetime sex partners",
  "sex_partners_scaled" = "Lifetime sex partners (S)",
  "strength_sex_centered:partneredTRUE" = "Partnered x Grip Strength (S by sex)",
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
  "avgcalories_centered" = "Average calories per day (S)",

  "arm_lean_centered" = "Arm lean mass (S)",
  "leg_lean_centered" = "Legs lean mass (S)",
  "trunk_lean_centered" = "Trunk lean mass (S)",
  "upper_lean_centered" = "Upper lean mass (S)",
  "total_lean_centered" = "Total lean mass (S)",

  "arm_lean_sex_centered" = "Arm lean mass (S by sex)",
  "leg_lean_sex_centered" = "Legs lean mass (S by sex)",
  "trunk_lean_sex_centered" = "Trunk lean mass (S by sex)",
  "total_lean_sex_centered" = "Total lean mass (S by sex)",

  "arm_lean_sex_centered:sexfemale" = "Sex (female) x Arm lean mass (S by sex)",
  "leg_lean_sex_centered:sexfemale" = "Sex (female) x Legs lean mass (S by sex)",
  "trunk_lean_sex_centered:sexfemale" = "Sex (female) x Trunk lean mass (S by sex)",
  "upper_lean_sex_centered:sexfemale" = "Sex (female) x Upper lean mass (S by sex)",
  "total_lean_sex_centered:sexfemale" = "Sex (female) x Total lean mass (S by sex)",

  "sexfemale:arm_lean_sex_centered" = "Sex (female) x Arm lean mass (S by sex)",
  "sexfemale:leg_lean_sex_centered" = "Sex (female) x Legs lean mass (S by sex)",
  "sexfemale:trunk_lean_sex_centered" = "Sex (female) x Trunk lean mass (S by sex)",
  "sexfemale:upper_lean_sex_centered" = "Sex (female) x Upper lean mass (S by sex)",
  "sexfemale:total_lean_sex_centered" = "Sex (female) x Total lean mass (S by sex)",

  "sexfemale:arm_lean_centered" = "Sex (female) x Arm lean mass (S)",
  "sexfemale:leg_lean_centered" = "Sex (female) x Legs lean mass (S)",
  "sexfemale:trunk_lean_centered" = "Sex (female) x Trunk lean mass (S)",
  "sexfemale:upper_lean_centered" = "Sex (female) x Upper lean mass (S)",
  "sexfemale:total_lean_centered" = "Sex (female) x Total lean mass (S)",

  "arm_leanbmc_centered" = "Arm lean mass (S)",
  "leg_leanbmc_centered" = "Legs lean mass (S)",
  "trunk_leanbmc_centered" = "Trunk lean mass (S)",
  "total_leanbmc_centered" = "Total lean mass (S)",

  "arm_leanbmc_sex_centered" = "Arm lean mass (S by sex)",
  "leg_leanbmc_sex_centered" = "Legs lean mass (S by sex)",
  "trunk_leanbmc_sex_centered" = "Trunk lean mass (S by sex)",
  "total_leanbmc_sex_centered" = "Total lean mass (S by sex)",

  "arm_leanbmc_sex_centered:sexfemale" = "Sex (female) x Arm lean mass (S by sex)",
  "leg_leanbmc_sex_centered:sexfemale" = "Sex (female) x Legs lean mass (S by sex)",
  "trunk_leanbmc_sex_centered:sexfemale" = "Sex (female) x Trunk lean mass (S by sex)",
  "upper_leanbmc_sex_centered:sexfemale" = "Sex (female) x Upper lean mass (S by sex)",
  "total_leanbmc_sex_centered:sexfemale" = "Sex (female) x Total lean mass (S by sex)",

  "sexfemale:arm_leanbmc_sex_centered" = "Sex (female) x Arm lean mass (S by sex)",
  "sexfemale:leg_leanbmc_sex_centered" = "Sex (female) x Legs lean mass (S by sex)",
  "sexfemale:trunk_leanbmc_sex_centered" = "Sex (female) x Trunk lean mass (S by sex)",
  "sexfemale:upper_leanbmc_sex_centered" = "Sex (female) x Upper lean mass (S by sex)",
  "sexfemale:total_leanbmc_sex_centered" = "Sex (female) x Total lean mass (S by sex)"

)

strength_terms0 <- c("strength", "arm_lean", "leg_lean", "trunk_lean", "total_lean", "upper_lean")
strength_terms0 <- c(strength_terms0, paste0(strength_terms0, 'bmc'))
strength_terms0 <- c(paste0(strength_terms0, "_centered"), paste0(strength_terms0, '_sex_centered'))
sex_strength_terms <- c(strength_terms0, paste0(strength_terms0, ':sexfemale'), paste0('sexfemale:', strength_terms0))

# Update design objects -----------------------------------


update_designs <- function(designs, cutoff = 100){

  # Scale sex_partners by the interquartile range
  designs$d.design.adults <- update(designs$d.design.adults, sex_partners_scaled = sex_partners/10)

  # Create numeric version of sex for correlation matrix
  designs$d.design.adults <- update(designs$d.design.adults, sex2 = ifelse(sex == "male", 1, 0))

  # Compute years since sexually maturity, defined as age 12
  designs$d.design.adults <- update(designs$d.design.adults, years_sexually_mature = age - 12)

  # Removing those with very high numbers of lifetime sex partners
  designs$d.design.adults <- subset(designs$d.design.adults, (sex_partners < cutoff) | is.na(sex_partners))

  return(designs)
}


# Regression models -------------------------------------------------------

fitmodels <- function(design, design_dietary, design_full, muscle = 'strength_centered', muscle_sex = 'strength_sex_centered'){

  formulas <- list(
    'manth1' = 'sex_partners ~ offset(log(years_sexually_mature)) + {muscle_sex} * sex + partnered + bmi_centered * sex',
    'manth1b' = 'sex_partners ~ offset(log(years_sexually_mature)) + {muscle_sex} * sex + partnered + height_centered + weight_centered',
    'msoc1' =  'sex_partners ~ offset(log(years_sexually_mature)) + {muscle_sex} * sex + partnered + edu + race',
    'mheal1' = 'sex_partners ~ offset(log(years_sexually_mature)) + {muscle_sex} * sex + partnered + perceived_abnormal_weight + whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression',
    'mhor1' = 'sex_partners ~ offset(log(years_sexually_mature)) + {muscle_sex} * sex + partnered + testosterone_sex_centered * sex',
    'mphys1' = 'sex_partners ~ offset(log(years_sexually_mature)) + {muscle_sex} * sex + partnered + vigorous_rec + moderate_rec + vigorous_work + moderate_work',

    'manth2' = 'sex_partners_year ~ age_centered * sex + {muscle_sex} * sex + partnered * {muscle_sex} +  bmi_centered * sex',
    'manth2b' = 'sex_partners_year ~ age_centered * sex + {muscle_sex} * sex + partnered * {muscle_sex} +  height_centered + weight_centered',
    'msoc2' = 'sex_partners_year ~ age_centered * sex + {muscle_sex} * sex + partnered * {muscle_sex} + edu + race',
    'mheal2' = 'sex_partners_year ~ age_centered * sex + {muscle_sex} * sex + partnered * {muscle_sex} + perceived_abnormal_weight + whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression',
    'mhor2' = 'sex_partners_year ~ age_centered * sex + {muscle_sex} * sex + partnered * {muscle_sex} + testosterone_sex_centered * sex',
    'mphys2' = 'sex_partners_year ~ age_centered * sex + {muscle_sex} * sex + partnered * {muscle_sex} + vigorous_rec + moderate_rec + vigorous_work + moderate_work',

    'manth3' = 'age_first_sex ~ age_centered * sex + {muscle_sex} * sex + partnered  + bmi_centered * sex',
    'manth3b' = 'age_first_sex ~ age_centered * sex + {muscle_sex} * sex + partnered  + height_centered + weight_centered',
    'msoc3' =  'age_first_sex ~ age_centered * sex + {muscle_sex} * sex + partnered + race + edu',
    'mheal3' = 'age_first_sex ~ age_centered * sex + {muscle_sex} * sex + partnered  + bmi_centered * sex',
    'mhor3' = 'age_first_sex ~ age_centered * sex + {muscle_sex} * sex + partnered + testosterone_sex_centered * sex',
    'mphys3' = 'age_first_sex ~ age_centered * sex + {muscle_sex} * sex + partnered + vigorous_rec + moderate_rec + vigorous_work + moderate_work',

    'manth4' = 'partnered ~ sex_partners_scaled + age_centered * sex + {muscle_sex} * sex + bmi_centered * sex',
    'manth4b' = 'partnered ~ sex_partners_scaled + age_centered * sex + {muscle_sex} * sex + height_centered + weight_centered',
    'msoc4' =  'partnered ~ sex_partners_scaled + age_centered * sex + {muscle_sex} * sex + edu + race',
    'mheal4' = 'partnered ~ sex_partners_scaled + age_centered * sex + {muscle_sex} * sex + perceived_abnormal_weight + whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression',
    'mhor4' = 'partnered ~ sex_partners_scaled + age_centered * sex + {muscle_sex} * sex + testosterone_sex_centered * sex',
    'mphys4' = 'partnered ~ sex_partners_scaled + age_centered * sex + {muscle_sex} * sex + vigorous_rec + moderate_rec + vigorous_work + moderate_work',

    'mrep5' = 'whitebloodcell ~ age_centered * sex + {muscle} * sex + bmi_centered',
    'mexp5' = 'whitebloodcell ~ age_centered * sex + {muscle} * sex + weight_centered + height_centered + testosterone_sex_centered * sex + foodinsecurity_adult  + avgcalories_centered + tot_MET_centered + depression',

    'mrep6' = 'avgcalories ~ age_centered + tot_MET_centered  + {muscle} + bmi_centered  + sex',
    'mexp6' = 'avgcalories ~ age_centered + tot_MET_centered + {muscle} + sex + weight_centered + height_centered + whitebloodcell_centered + foodinsecurity_adult',

    'mrep7' = 'avgprotein ~ age_centered + tot_MET_centered  + {muscle} + bmi_centered  + sex',
    'mexp7' = 'avgprotein ~ age_centered + tot_MET_centered + {muscle} + sex + weight_centered + height_centered + whitebloodcell_centered + foodinsecurity_adult'
  )

  formulas <- map(formulas, \(x) formula(str_glue(x)))

  models0 <- list(

    ## Lifetime partner models -------------------------------------------------

    # Anthropometric
    manth1 = svyglm(
      formulas$manth1,
      family = quasipoisson(),
      design = design
    ),

    # Socioeconomic model
    msoc1 = svyglm(
      formulas$msoc1,
      family = quasipoisson(),
      design = design
    ),

    # Health model
    mheal1 = svyglm(
      formulas$mheal1,
      family = quasipoisson(),
      design = design
    ),

    mhor1 = svyglm(
      formulas$mhor1,
      family = quasipoisson(),
      design = design
    ),

    # Physical activity
    mphys1 = svyglm(
      formulas$mphys1,
      family = quasipoisson(),
      design = design
    ),

    ## Past year partner models ------------------------------------------------

    # Anthropometric model
    manth2 = svyglm(
      formulas$manth2,
      family = quasipoisson(),
      design = design
    ),

    # Socioeconomic model
    msoc2 = svyglm(
      formulas$msoc2,
      family = quasipoisson(),
      design = design
    ),

    # Health model
    mheal2 = svyglm(
      formulas$mheal2,
      family = quasipoisson(),
      design = design
    ),

    mhor2 = svyglm(
      formulas$mhor2,
      family = quasipoisson(),
      design = design
    ),

    # Physical activity
    mphys2 = svyglm(
      formulas$mphys2,
      family = quasipoisson(),
      design = design
    ),

    ## Age at first sex models -------------------------------------------------

    # Anthropometric
    manth3 = svyglm(
      formulas$manth3,
      family = gaussian(),
      design = design
    ),

    # Socioeconomic
    msoc3 = svyglm(
      formulas$msoc3,
      family = gaussian(),
      design = design
    ),

    # Health
    mheal3 = svyglm(
      formulas$mheal3,
      family = gaussian(),
      design = design
    ),

    mhor3 = svyglm(
      formulas$mhor3,
      family = gaussian(),
      design = design
    ),

    # Physical activity
    mphys3 = svyglm(
      formulas$mphys3,
      family = gaussian(),
      design = design
    ),

    ## Partnered models --------------------------------------------------------

    # Anthropometric model
    manth4 = svyglm(
      formulas$manth4,
      family = quasibinomial(),
      design = design
    ),

    # Socioeconomic model
    msoc4 = svyglm(
      formulas$msoc4,
      family = quasibinomial(),
      design = design
    ),

    # Health
    mheal4 = svyglm(
      formulas$mheal4,
      family = quasibinomial(),
      design = design
    ),

    # Hormone
    mhor4 = svyglm(
      formulas$mhor4,
      family = quasibinomial(),
      design = design
    ),

    # Physical activity
    mphys4 = svyglm(
      formulas$mphys4,
      family = quasibinomial(),
      design = design
    ),

    # Immune models -------------------------------------------------------------

    mrep5 = svyglm(
      formulas$mrep5,
      family = quasipoisson(),
      design = design_full
    ),

    mexp5 = svyglm(
      formulas$mexp5,
      family = quasipoisson(),
      design = design_dietary
    ),

    ## Energy models -----------------------------------------------------------

    mrep6 = svyglm(
      formulas$mrep6,
      family = gaussian(),
      design = design_dietary
    ),

    mexp6 = svyglm(
      formulas$mexp6,
      family = gaussian(),
      design = design_dietary
    ),

    ## Protein models -----------------------------------------------------------------

    mrep7 = svyglm(
      formulas$mrep7,
      family = gaussian(),
      design = design_dietary
    ),

    mexp7 = svyglm(
      formulas$mexp7,
      family = gaussian(),
      design = design_dietary
    )
  )

  modelnames <- names(models0)

  tibble(
    modelnames = modelnames,
    Controls = control_dict[str_remove(modelnames, "\\d")],
    Outcome = outcome_dict[str_extract(modelnames, "\\db*")],
    Model = models0,
    Stats = map(Model, \(x) {x$df.residual <- Inf; tidy(x, conf.int = T)}),
    benefit_cost = ifelse(Outcome %in% c('Immunity', 'Energy (kcal)', 'Protein (g)'), 'Cost', 'Benefit')
  )
}

# Extract model stats -----------------------------------------------------

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
    dplyr::select(-Model) |>  # Remove due to RStudio bug
    unnest(Stats) |>
    dplyr::filter(term != "(Intercept)") |>
    mutate(
      term2 = term,
      term = factor(term, levels=rev(names(vnames)), labels=rev(vnames)),
      Significant = sign(conf.low) == sign(conf.high)
    )
}

marginals <- function(models){
  bind_rows(
    avg_comparisons(models$Model$manth4, variables = list(strength_sex_centered = 1), by = 'sex', wts = "(weights)") |> mutate(Model = 'Partnered'),
    avg_comparisons(models$Model$manth1, variables = list(strength_sex_centered = 1), by = c('sex', 'partnered'), wts = "(weights)") |> mutate(Model = 'Lifetime'),
    avg_comparisons(models$Model$manth2, variables = list(strength_sex_centered = 1), by = c('sex', 'partnered'), wts = "(weights)") |> mutate(Model = 'Past year'),
    avg_comparisons(models$Model$manth3, variables = list(strength_sex_centered = 1), by = 'sex', wts = "(weights)") |> mutate(Model = 'Age first sex')
  ) |>
    dplyr::select(sex, partnered, estimate)
}


# Effects plots -----------------------------------------------------------

effects_plots <- function(models, controls = "Anthropometric"){
  out <-
    models |>
    dplyr::filter(Controls == controls) |>
    rowwise() |>
    mutate(
      Plot = list(
        plot_predictions(Model, condition = c("strength_sex_centered", "sex")) +
          scale_color_binary() +
          xlab("Grip Strength (S by sex)") +
          ylab(Outcome) +
          theme_minimal()
      )
    )
  wrap_plots(out$Plot, ncol = 2) + plot_layout(axes = 'collect', guides = 'collect')
}

effects_plots2 <- function(df1, df2, controls = "Anthropometric"){
  out <-
    bind_rows(list(Pilot = df1, Confirmatory = df2), .id = 'Stage') |>
    dplyr::filter(Controls == controls) |>
    rowwise() |>
    mutate(
      Plot = list(
        plot_predictions(Model, condition = c("strength_sex_centered", "sex")) +
          scale_color_binary() +
          xlab("Grip Strength (S by sex)") +
          ylab(Outcome) +
          theme_minimal()
      )
    ) |>
    ungroup() |>
    arrange(Outcome, desc(Stage))
  wrap_plots(out$Plot, ncol = 2, byrow = T) + plot_layout(axes = 'collect', guides = 'collect')
}

plot_predictions2 <- function(model, stage){
  d <- plot_predictions(model, condition = c("strength_sex_centered", "sex"), draw = F)
  d$Stage <- stage
  return(d)
}

effects_plots3 <- function(df1, df2, controls = "Anthropometric"){
  out <-
    bind_rows(list(Pilot = df1, Confirmatory = df2), .id = 'Stage') |>
    dplyr::filter(Controls == controls) |>
    rowwise() |>
    mutate(
      Plot = list(
        plot_predictions2(Model, Stage) |>
        suppressWarnings()
      ) |> setNames(Outcome)
    ) |>
    ungroup() |>
    group_by(Outcome) |>
    summarise(
      Data = list(bind_rows(Plot, .id = 'Outcome')),
    )
  return(out)
}

# Lean mass plot -------------------------------

lean_mass_plot <- function(title, muscle, muscle_sex, mating_only = T){
  modelsG <-
    fitmodels(
      designsG2$d.design.adults,
      designsG2$d.design.dietary.adults,
      designsG$d.design.adults,
      muscle = muscle,
      muscle_sex = muscle_sex
    )

  modelsH <-
    fitmodels(
      designsH2$d.design.adults,
      designsH2$d.design.dietary.adults,
      designsH$d.design.adults,
      muscle = muscle,
      muscle_sex = muscle_sex
    )

  lean_stats <- bind_rows(
    "Stage 1: Pilot" = allstats(modelsG),
    "Stage 2: Confirmatory" = allstats(modelsH),
    .id = "Stage"
  ) |>
  mutate(term = factor(term, levels = unique(term)))

  p1 <- lean_stats |>
    dplyr::filter(benefit_cost == "Benefit", term2 %in% sex_strength_terms) |>
    ggplot(aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
    geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    scale_color_binary() +
    labs(title = paste(title, "vs. mating success"), x = "\nEstimate (95% CI)", y = "") +
    facet_grid(Outcome ~ term) +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle = 0))

  if (mating_only) return(p1)

  print(p1)

  p2 <- lean_stats |>
    dplyr::filter(str_detect(Outcome, "Energy|Protein"), term2 %in% sex_strength_terms) |>
    ggplot(aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
    geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    scale_color_binary(direction = -1) +
    labs(title = paste(title, "vs. intake"), x = "\nEstimate (95% CI)", y = "") +
    facet_wrap(~Outcome, scales = "free_x", ncol = 1, strip.position = "right") +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle = 0))

  print(p2)

  p3 <- lean_stats |>
    dplyr::filter(str_detect(Outcome, "Immunity"), term2 %in% sex_strength_terms) |>
    ggplot(aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
    geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    scale_color_binary() +
    labs(title = paste(title, "vs. immunity"), x = "\nEstimate (95% CI)", y = "") +
    facet_wrap(~term) +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle = 0))

  print(p3)
}

# Summary tables ----------------------------------------------------------

summary_tables <- function(design) {
  getncat <- design %>%
    tbl_svysummary(
      by = sex,
      include = c(
        partnered,
        edu,
        race,
        perceived_abnormal_weight,
        special_equipment,
        vigorous_work,
        moderate_work,
        vigorous_rec,
        moderate_rec
      ),
      statistic = list(
        all_categorical() ~ "{N_nonmiss_unweighted}"
      ), missing = "no"
    )

  getncon <- design %>%
    tbl_svysummary(
      by = sex,
      include = c(
        age,
        age_first_sex,
        sex_partners,
        sex_partners_year,
        strength,
        ArmLeanexclBMC,
        LegLeanexclBMC,
        TrunkLeanexclBMC,
        TotalLeanexclBMC,
        bmi,
        height,
        weight,
        whitebloodcell,
        hemoglobin,
        testosterone,
        chronic_disease_score,
        physical_disease_count,
        depression,
        avgcalories,
        avgprotein,
        foodinsecurity_adult,
        tot_MET
      ),
      statistic = list(
        all_continuous() ~ "{N_nonmiss_unweighted}",
        all_categorical() ~ "{N_nonmiss_unweighted}"
      ),
      digits = all_continuous() ~ 0,
      type = list(
        chronic_disease_score ~ "continuous",
        physical_disease_count ~ "continuous",
        foodinsecurity_adult ~ "continuous"
      ),
      missing = "no"
    )

  getratio <- design %>%
    tbl_svysummary(
      by = sex,
      include = c(
        age,
        age_first_sex,
        sex_partners,
        sex_partners_year,
        strength,
        ArmLeanexclBMC,
        LegLeanexclBMC,
        TrunkLeanexclBMC,
        TotalLeanexclBMC,
        bmi,
        height,
        weight,
        whitebloodcell,
        hemoglobin,
        testosterone,
        chronic_disease_score,
        physical_disease_count,
        depression,
        avgcalories,
        avgprotein,
        foodinsecurity_adult,
        tot_MET
      ),
      statistic = list(
        all_continuous() ~ "{mean}",
        all_categorical() ~ "{p}"
      ),
      digits = list(
        all_continuous() ~ c(2)
      ),
      type = list(
        chronic_disease_score ~ "continuous",
        physical_disease_count ~ "continuous",
        foodinsecurity_adult ~ "continuous"
      ),
      missing = "no"
    ) %>%
    modify_table_body(
      ~ .x %>%
        dplyr::mutate(ratio = round(as.numeric(gsub(",", "", stat_1)) / as.numeric(gsub(",", "", stat_2)), digits = 2))
    )

  list(
    cat = design %>%
      tbl_svysummary(
        by = sex,
        include = c(
          partnered,
          edu,
          race,
          perceived_abnormal_weight,
          special_equipment,
          vigorous_work,
          moderate_work,
          vigorous_rec,
          moderate_rec
        ),
        label = list(
          partnered = "Partnered",
          edu = "Education",
          race = "Race and Ethnicity",
          perceived_abnormal_weight = "Perceived abnormal weight",
          special_equipment = "Special equipment needed to walk",
          vigorous_work = "Work involves vigorous activity",
          moderate_work = "Work involves moderate activity",
          vigorous_rec = "Recreation involves vigorous activity",
          moderate_rec = "Recreation involves moderate activity"
        ),
        statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
        missing = "no"
      ) %>%
      modify_table_body(
        ~ .x %>%
          dplyr::mutate(n_male = getncat$table_body$stat_1, digits = 2) %>%
          dplyr::mutate(n_female = getncat$table_body$stat_2, digits = 2) %>%
          dplyr::relocate(n_male, .before = stat_1) %>%
          dplyr::relocate(n_female, .before = stat_2)
      ) %>%
      modify_header(
        label = "**Variable**",
        n_male = "**N**",
        n_female = "**N**",
        stat_1 = "**Mean (SD) or n (%)**",
        stat_2 = "**Mean (SD) or n (%)**",
      ) %>%
      modify_spanning_header(list(
        c("stat_1", "n_male") ~ "**Male**",
        c("stat_2", "n_female") ~ "**Female**"
      )) %>%
      bold_labels(),
    con = design %>%
      tbl_svysummary(
        by = sex,
        include = c(
          age,
          age_first_sex,
          sex_partners,
          sex_partners_year,
          strength,
          ArmLeanexclBMC,
          LegLeanexclBMC,
          TrunkLeanexclBMC,
          TotalLeanexclBMC,
          bmi,
          height,
          weight,
          whitebloodcell,
          hemoglobin,
          testosterone,
          chronic_disease_score,
          physical_disease_count,
          depression,
          avgcalories,
          avgprotein,
          foodinsecurity_adult,
          tot_MET
        ),
        label = list(
          age = "Age (Years)",
          age_first_sex = "Age at first sex (years)",
          sex_partners = "Lifetime number of sexual partners",
          sex_partners_year = "Past year number of sexual partners",
          strength = "Combined Grip Strength (kg)",
          ArmLeanexclBMC = "Arm lean mass (g)",
          LegLeanexclBMC = "Leg lean mass (g)",
          TrunkLeanexclBMC = "Trunk lean mass (g)",
          TotalLeanexclBMC = "Total lean mass (g)",
          bmi = "Body mass index (kg/m^2)",
          height = "Height (cm)",
          weight = "Weight (kg)",
          whitebloodcell = "White blood cell count (1000 cells/µL)",
          hemoglobin = "Hemoglobin (g/dL)",
          testosterone = "Testosterone (ng/dL)",
          chronic_disease_score = "Chronic Disease Score (0-6)",
          physical_disease_count = "Disease Impairment Score (0-5)",
          depression = "Depression Score (0-27)",
          avgcalories = "Dietary energy intake (kcals)",
          avgprotein = "Dietary protein intake (grams)",
          foodinsecurity_adult = "Food Insecurity Rating (1-4)",
          tot_MET = "Total MET"
        ),
        statistic = list(
          all_continuous() ~ "{mean} ({sd})",
          all_categorical() ~ "{n_unweighted} ({p}%)"
        ),
        digits = all_continuous() ~ 2,
        type = list(
          chronic_disease_score ~ "continuous",
          physical_disease_count ~ "continuous",
          foodinsecurity_adult ~ "continuous"
        ),
        missing = "no"
      ) %>%
      modify_table_body(
        ~ .x %>%
          dplyr::mutate(ratio = getratio$table_body$ratio, digits = 2) %>%
          dplyr::mutate(n_male = getncon$table_body$stat_1, digits = 2) %>%
          dplyr::mutate(n_female = getncon$table_body$stat_2, digits = 2) %>%
          dplyr::relocate(n_male, .before = stat_1) %>%
          dplyr::relocate(n_female, .before = stat_2)
      ) %>%
      add_difference(
        test = everything() ~ "smd",
        estimate_fun = list(all_continuous() ~ purrr::partial(style_ratio, digits = 2))
      ) %>%
      modify_column_hide(columns = ci) %>%
      modify_header(
        label = "**Variable**",
        ratio = "**Ratio**",
        n_male = "**N**",
        n_female = "**N**",
        stat_1 = "**Mean (SD)**",
        stat_2 = "**Mean (SD)**",
        estimate = "**SMD**"
      ) %>%
      modify_spanning_header(list(
        c("stat_1", "n_male") ~ "**Male**",
        c("stat_2", "n_female") ~ "**Female**",
        c("ratio", "estimate") ~ "**Sexual Dimorphism**"
      )) %>%
      modify_footnote(all_stat_cols() ~ "Weighted means and standard deviations shown for continuous variables, unweighted n (%) for categorical variables") %>%
      # modify_caption("Descriptive statistics and sex differences for participants ages 18-60 using population weights") %>%
      bold_labels()
  )
}




# Correlation matrices ----------------------------------------------------

cor_mats <- function(designs){

  f <-
    ~ sex_partners +
    sex_partners_year +
    age_first_sex +
    age +
    strength +
    ArmLeanexclBMC +
    LegLeanexclBMC +
    TrunkLeanexclBMC +
    TotalLeanexclBMC +
    bmi +
    edu +
    hemoglobin +
    whitebloodcell +
    depression +
    chronic_disease_score +
    physical_disease_count +
    testosterone +
    vigorous_rec +
    moderate_rec +
    vigorous_work +
    moderate_work

  cor_mat_f <- svycor(
    f,
    designs$d.design.adult.female,
    na.rm = T
  )

  cor_mat_m <- svycor(
    f,
    designs$d.design.adult.male,
    na.rm = T
  )

  cor_mat <- svycor(
    update(f, ~ . + sex2),
    designs$d.design.adults,
    na.rm = T
  )

  list(
    cor_mat_cors_f = cor_mat_f$cors,
    cor_mat_cors_m = cor_mat_m$cors,
    cor_mat_cors = cor_mat$cors
  )
}


# Sex differences ---------------------------------------------------------

sexdiff <- function(var, design){
  means <- svyby(make.formula(var), by =~sex, design = design, FUN = svymean, na.rm = T)
  means <- means[[var]]
  vars <- svyby(make.formula(var), by =~sex, design = design, FUN = svyvar, na.rm = T)
  vars <- vars[[var]]
  (means[1] - means[2])/sqrt((vars[1] + vars[2])/2)
}
