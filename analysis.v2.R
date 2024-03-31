library(tidyverse)
library(survey)
library(nhanesGH)
library(hagenutils)
library(broom)
library(gtsummary)
library(jtools)

# Cumulative distributions of sex partners ------------------------

plot_sexpartners <-
  ggplot(d_G, aes(sex_partners + 1, colour = sex)) +
  stat_ecdf() +
  geom_vline(xintercept = 100, linetype = 'dotted') +
  annotate("text", label = 'Cutoff', x = 110, y = 0.80, hjust = 0) +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50, 100, 1000)) +
  scale_color_binary() +
  guides(colour = guide_legend(title = '', reverse = T, override.aes = list(linewidth=2))) +
  labs(x = "\nNumber of lifetime sex partners + 1", y = 'ECDF\n') +
  theme_minimal(15)
plot_sexpartners
ggsave("Figures/plot_sexpartners.pdf", plot_sexpartners, width = 9, height = 6)

plot_sexpartners_year <-
  ggplot(d_G, aes(sex_partners_year + 1, colour = sex)) +
  stat_ecdf() +
  scale_x_log10(breaks = c(1, 5, 10, 25, 50)) +
  scale_color_binary() +
  guides(colour = guide_legend(title = '', reverse = T, override.aes = list(linewidth=2))) +
  labs(x = "\nNumber of last year sex partners + 1", y = 'ECDF\n') +
  theme_minimal(15)
plot_sexpartners_year
ggsave("Figures/plot_sexpartners_year.pdf", plot_sexpartners, width = 9, height = 6)


# Update design objects -----------------------------------

design_all <- designsG$d.design.adults # Entire sample

# Removing those with >=100 lifetime sex partners
designsG$d.design.adults <- subset(designsG$d.design.adults, sex_partners < 100)

# Scale sex_partners by the interquartile range
designsG$d.design.adults <- update(designsG$d.design.adults, sex_partners_scaled = sex_partners/10)

# Difference between current age and age at first sex
designsG$d.design.adults <- update(designsG$d.design.adults, age_diff = age - age_first_sex)

# Create numeric version of sex for correlation matrix
designsG$d.design.adults <- update(designsG$d.design.adults, sex2 = ifelse(sex == "male", 1, 0))

# set strength_centered2 to the values standardized across both sexes
designsG$d.design.dietary.adults <- update(designsG$d.design.dietary.adults, strength_centered2 = strength_centered)
designsG$d.design.adults <- update(designsG$d.design.adults, strength_centered2 = strength_centered)

# Set strength_centered to the sex-specific values
designsG$d.design.adults <- update(designsG$d.design.adults, strength_centered = strength_sex_centered)

# Lifetime partner models -------------------------------------------------

# Anthropometric
m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# Socioeconomic model
msoc1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(msoc1, df.resid = Inf)

# Health model
mheal1 <-
  svyglm(
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
    design = designsG$d.design.adults
  )
summary(mheal1, df.resid = Inf)

mhor1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mhor1, df.resid = Inf)

# Physical activity
mphys1 <-
  svyglm(
    sex_partners ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mphys1, df.resid = Inf)

# Past year partner models ------------------------------------------------

# Anthropometric model
m_pastyear <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered + #keeping partnered x strength interaction only for this model
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

# Socioeconomic model
msoc2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      edu +
      race,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(msoc2)

# Health model
mheal2 <-
  svyglm(
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
    design = designsG$d.design.adults
  )
summary(mheal2)

mhor2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      testosterone_sex_centered * sex,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mhor2, df.resid = Inf)

# Physical activity
mphys2 <-
  svyglm(
    sex_partners_year ~
      age_centered * sex +
      strength_centered * sex +
      partnered * strength_centered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasipoisson(),
    design = designsG$d.design.adults
  )
summary(mphys2)

# Age at first sex models -------------------------------------------------

# Anthropometric
m_agefirst <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = gaussian(),
  design = designsG$d.design.adults
)

# Socioeconomic
msoc3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      race +
      edu,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(msoc3, df.resid = Inf)

# Health
mheal3 <-
  svyglm(
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
    design = designsG$d.design.adults
  )
summary(mheal3)

mhor3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      testosterone_sex_centered * sex,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(mhor3)

# Physical activity
mphys3 <-
  svyglm(
    age_first_sex ~
      age_centered * sex +
      strength_centered * sex +
      partnered +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = gaussian(),
    design = designsG$d.design.adults
  )
summary(mphys3)

# Partnered models --------------------------------------------------------

# Anthropometric model
m_partnered <-
  svyglm(
    partnered ~
      sex_partners_scaled +
      age_centered * sex +
      strength_centered * sex +
      bmi_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(m_partnered, df.resid = Inf)

# Socioeconomic model
msoc4 <-
  svyglm(
    partnered ~
      sex_partners_scaled +
      age_centered * sex +
      strength_centered * sex +
      edu +
      race,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(msoc4)

# Health
mheal4 <-
  svyglm(
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
    design = designsG$d.design.adults
  )
summary(mheal4)

# Hormone
mhor4 <-
  svyglm(
    partnered ~
      sex_partners_scaled +
      age_centered * sex +
      strength_centered * sex +
      testosterone_sex_centered * sex,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(mhor4)

# Physical activity
mphys4 <-
  svyglm(
    partnered ~
      sex_partners_scaled +
      age_centered * sex +
      strength_centered * sex +
      vigorous_rec +
      moderate_rec +
      vigorous_work +
      moderate_work,
    family = quasibinomial(),
    design = designsG$d.design.adults
  )
summary(mphys4)

# Immune model ------------------------------------------------------------

mwbc <-
  svyglm(
    whitebloodcell ~
      age_centered * sex +
      strength_centered2 * sex +
      bmi_centered,
    family= quasipoisson(),
    design=designsG$d.design.adults
  )
summary(mwbc, df.resid = Inf)

mwbc_alt <- svyglm(
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
  design=designsG$d.design.adults)

summary(mwbc_alt, df.resid = Inf)

# Dietary Intake model ----------------------------------------------------

m_energy <- svyglm( #use this
  avgcalories ~
    age_centered +
    tot_MET_centered  +
    strength_centered2 +
    bmi_centered  +
    sex,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_energy, df.resid = Inf)

m_energy_alt <- svyglm( #use this
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
  design = designsG$d.design.dietary.adults
)
summary(m_energy_alt, df.resid = Inf)

# Protein model -----------------------------------------------------------------

m_protein <- svyglm( #use this
  avgprotein ~
    age_centered +
    tot_MET_centered  +
    strength_centered2 +
    bmi_centered  +
    sex,
  family = gaussian(),
  design = designsG$d.design.dietary.adults
)
summary(m_protein, df.resid = Inf)

m_protein_alt <- svyglm( #use this
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
  design = designsG$d.design.dietary.adults
)
summary(m_protein_alt, df.resid = Inf)

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
  "Replication Model",
  "Expanded Controls Model"
)

vnames <- c(
  "strength_centered" = "Strength (S by sex)",
  "strength_centered2" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "sexfemale:strength_centered2" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "sex_partners" = "Lifetime sex partners",
  "sex_partners_scaled" = "Lifetime sex partners (S)",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age (S) x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI (S)",
  "sexfemale:height_centered" = "Sex (Female) x Height (S)",
  "sexfemale:weight_centered" = "Sex (Female) x Weight (S)",
  'perceived_abnormal_weightTRUE' = "Perceived Abnormal Weight",
  "whitebloodcell_centered" = "White Blood Cell Count (S)",
  "hemoglobin_centered" = "Hemoglobin (S)",
  "special_equipmentTRUE" = "Need special equip to walk",
  "chronic_disease_score" = "Chronic Disease Score",
  "physical_disease_count" = "Physical Disease Count",
  "depression" = "Depression Score",
  "log(testosterone)" = "Testosterone (log)",
  "sexfemale:log(testosterone)" = "Sex (Female) x Testosterone (log)",
  "testosterone_sex_centered" = "Testosterone (S by Sex)",
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

# Extract model stats -----------------------------------------------------

extract_stats <- function(obj, terms = c('strength_centered', 'sexfemale:strength_centered')){
  obj$df.residual <- Inf
  tidy(obj, conf.int = T) |>
    dplyr::filter(term %in% terms) |>
    dplyr::select(term, estimate, conf.low, conf.high)
}

# Plot strength and sex:strength coefficients -----------------------------

models <- tribble(
  ~Theme,               ~Outcome,             ~Model,
  "Anthropometric",     "Age of first sex",   m_agefirst,
  "Anthropometric",     "Lifetime partners",  m_lifetime,
  "Anthropometric",     "Partnered",          m_partnered,
  "Anthropometric",     "Past year partners", m_pastyear,

  "Health",             "Lifetime partners",  mheal1,
  "Health",             "Past year partners", mheal2,
  "Health",             "Age of first sex",   mheal3,
  "Health",             "Partnered",          mheal4,

  "Hormone",            "Lifetime partners",  mhor1,
  "Hormone",            "Past year partners", mhor2,
  "Hormone",            "Age of first sex",   mhor3,
  "Hormone",            "Partnered",          mhor4,

  "Activity",           "Lifetime partners",  mphys1,
  "Activity",           "Past year partners", mphys2,
  "Activity",           "Age of first sex",   mphys3,
  "Activity",           "Partnered",          mphys4,

  "Socioeconomic",      "Lifetime partners",  msoc1,
  "Socioeconomic",      "Past year partners", msoc2,
  "Socioeconomic",      "Age of first sex",   msoc3,
  "Socioeconomic",      "Partnered",          msoc4
)

d_strength_stats <-
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

plot_coefs <-
  ggplot(d_strength_stats, aes(estimate, Outcome, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Estimate (95% CI)', y = '') +
  facet_grid(Theme ~ term) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_coefs

d_lifetime_stats <-
  models |>
  mutate(
    Stats = map(Model, \(obj) extract_stats(obj, terms = 'sex_partners_scaled'))
  ) |>
  unnest(Stats) |>
  mutate(
    across(estimate:conf.high, exp)
  )

plot_lifetime_coefs <-
  ggplot(d_lifetime_stats, aes(estimate, Theme, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = 'dotted') +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Adjusted odds ratios (95% CI)', y = '') +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_lifetime_coefs

# Forestplots -------------------------------------------------------------

forestplot2 <- function(..., title=""){
  models <- list(...)
  models <- map(models, \(m) {m$df.residual = Inf; return(m)})
  models <- c(models, list(
    intercept = F,
    facet = F,
    dodgewidth = .8,
    modelnames = mnames,
    varnames = vnames,
    size = 0.7,
    linewidth = 0.7
  ))

  p <- do.call(forestplot, models)

  p +
    guides(colour = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
    labs(title = title) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))
}

# SI ----------------------------------------------------------------------

# Descriptive statistics tables

getncat <- designsG$d.design.adults %>%
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

cat <- designsG$d.design.adults %>%
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
  bold_labels()

getncon <- designsG$d.design.adults %>%
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

getratio <- designsG$d.design.adults %>%
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
      dplyr::mutate(ratio = round(as.numeric(stat_1)/as.numeric(stat_2), digits = 2))
  )

con <- designsG$d.design.adults %>%
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
  modify_caption("Descriptive statistics and sex differences for participants ages 18-60 using population weights") %>%
  bold_labels()

# Correlation matrices
cor_mat_f <- svycor(
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
    moderate_work,
  designsG$d.design.adult.female,
  na.rm = T
)

cor_mat_cors_f <- cor_mat_f$cors

cor_mat_m <- svycor(
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
    moderate_work,
  designsG$d.design.adult.male,
  na.rm = T
)

cor_mat_cors_m <- cor_mat_m$cors

cor_mat <- svycor(
  ~ sex2 +
    sex_partners +
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
    moderate_work,
  designsG$d.design.adults,
  na.rm = T
)

cor_mat_cors <- cor_mat$cors
