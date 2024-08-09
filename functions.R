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
  mrep = 'Replication',
  mexp = 'Expanded'
)

outcome_dict <- c(
  "1" = "Lifetime partners\n(partners per year)",
  "2" = "Past year partners",
  "3" = "Age of first sex",
  "4" = "Partnered",
  "5" = "Immunity",
  "6" = "Energy",
  "7" = "Protein"
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
  "strength_sex_centered" = "Strength (S by sex)",
  "strength_centered" = "Strength (S)",
  "strength" = "Strength",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_sex_centered" = "Sex (Female) x Strength (S by sex)",
  "strength_sex_centered:sexfemale" = "Sex (Female) x Strength (S by sex)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength (S)",
  "age_centered" = "Age (S)",
  "age" = "Age",
  "partneredTRUE" = "Partnered",
  "sex_partners" = "Lifetime sex partners",
  "sex_partners_scaled" = "Lifetime sex partners (S)",
  "strength_sex_centered:partneredTRUE" = "Partnered x Strength (S by sex)",
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

sex_strength_terms <- c(
  'sexfemale:strength_sex_centered',
  'strength_sex_centered:sexfemale',
  'strength_sex_centered',
  'sexfemale:strength_centered',
  'strength_centered'
  )

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

fitmodels <- function(design, design_dietary, design_full){

  models0 <- list(

    ## Lifetime partner models -------------------------------------------------

    # Anthropometric
    manth1 = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength_sex_centered * sex +
        partnered +
        bmi_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Socioeconomic model
    msoc1 = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
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
        offset(log(years_sexually_mature)) +
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
        offset(log(years_sexually_mature)) +
        strength_sex_centered * sex +
        partnered +
        testosterone_sex_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Physical activity
    mphys1 = svyglm(
      sex_partners ~
        offset(log(years_sexually_mature)) +
        strength_sex_centered * sex +
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
        strength_sex_centered * sex +
        partnered * strength_sex_centered + #keeping partnered x strength interaction only for this model
        bmi_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Socioeconomic model
    msoc2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_sex_centered * sex +
        partnered * strength_sex_centered +
        edu +
        race,
      family = quasipoisson(),
      design = design
    ),

    # Health model
    mheal2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_sex_centered * sex +
        partnered * strength_sex_centered +
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
        strength_sex_centered * sex +
        partnered * strength_sex_centered +
        testosterone_sex_centered * sex,
      family = quasipoisson(),
      design = design
    ),

    # Physical activity
    mphys2 = svyglm(
      sex_partners_year ~
        age_centered * sex +
        strength_sex_centered * sex +
        partnered * strength_sex_centered +
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
        strength_sex_centered * sex +
        partnered  +
        bmi_centered * sex,
      family = gaussian(),
      design = design
    ),

    # Socioeconomic
    msoc3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_sex_centered * sex +
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
        strength_sex_centered * sex +
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
        strength_sex_centered * sex +
        partnered +
        testosterone_sex_centered * sex,
      family = gaussian(),
      design = design
    ),

    # Physical activity
    mphys3 = svyglm(
      age_first_sex ~
        age_centered * sex +
        strength_sex_centered * sex +
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
        strength_sex_centered * sex +
        bmi_centered * sex,
      family = quasibinomial(),
      design = design
    ),

    # Socioeconomic model
    msoc4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_sex_centered * sex +
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
        strength_sex_centered * sex +
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
        strength_sex_centered * sex +
        testosterone_sex_centered * sex,
      family = quasibinomial(),
      design = design
    ),

    # Physical activity
    mphys4 = svyglm(
      partnered ~
        sex_partners_scaled +
        age_centered * sex +
        strength_sex_centered * sex +
        vigorous_rec +
        moderate_rec +
        vigorous_work +
        moderate_work,
      family = quasibinomial(),
      design = design
    ),

    # Immune models -------------------------------------------------------------

    mrep5 = svyglm(
      whitebloodcell ~
        age_centered * sex +
        strength_centered * sex +
        bmi_centered,
      family = quasipoisson(),
      design = design_full
    ),

    mexp5 = svyglm(
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
      design = design_dietary
    ),

    ## Energy models -----------------------------------------------------------

    mrep6 = svyglm(
      avgcalories ~
        age_centered +
        tot_MET_centered  +
        strength_centered +
        bmi_centered  +
        sex,
      family = gaussian(),
      design = design_dietary
    ),

    mexp6 = svyglm(
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
      design = design_dietary
    ),

    ## Protein models -----------------------------------------------------------------

    mrep7 = svyglm(
      avgprotein ~
        age_centered +
        tot_MET_centered  +
        strength_centered +
        bmi_centered  +
        sex,
      family = gaussian(),
      design = design_dietary
    ),

    mexp7 = svyglm(
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
      design = design_dietary
    )
  )

  modelnames <- names(models0)

  tibble(
    Controls = control_dict[str_remove(modelnames, "\\d")],
    Outcome = outcome_dict[str_extract(modelnames, "\\d")],
    Model = models0,
    Stats = map(Model, \(x) {x$df.residual <- Inf; tidy(x, conf.int = T)}),
    benefit_cost = ifelse(Outcome %in% c('Immunity', 'Energy', 'Protein'), 'Cost', 'Benefit')
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
          xlab("Strength (S by sex)") +
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
          xlab("Strength (S by sex)") +
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


# Summary tables ----------------------------------------------------------

summary_tables <- function(design){

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
        moderate_rec),
      statistic = list(
        all_categorical() ~ "{N_nonmiss_unweighted}"
      ) , missing = "no" )

  getncon = design %>%
    tbl_svysummary(
      by = sex,
      include = c(age_first_sex,
                  sex_partners,
                  sex_partners_year,
                  strength,
                  age,
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
                  tot_MET),
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

  getratio = design %>%
    tbl_svysummary(
      by = sex,
      include = c(age_first_sex,
                  sex_partners,
                  sex_partners_year,
                  strength,
                  age,
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
                  tot_MET),
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
        dplyr::mutate(ratio = round(as.numeric(gsub(",", "", stat_1))/as.numeric(gsub(",", "", stat_2)), digits = 2))
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
        label  = list(
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
          age_first_sex,
          sex_partners,
          sex_partners_year,
          strength,
          age,
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
        label  = list(
          age_first_sex = "Age at first sex (years)",
          sex_partners = "Lifetime number of sexual partners",
          sex_partners_year = "Past year number of sexual partners",
          strength = "Combined Grip Strength (kg)",
          age = "Age (Years)",
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
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n_unweighted} ({p}%)"),
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
      )  %>%
      add_difference(test = everything() ~ "smd",
                     estimate_fun = list(all_continuous() ~ purrr::partial(style_ratio, digits = 2))) %>%
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
