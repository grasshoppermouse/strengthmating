

# Cumulative distributions of sex partners ------------------------

plot_sexpartners <-
  ggplot(d_G[!is.na(d_G$sex_partners),], aes(sex_partners + 1, colour = sex)) +
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

designsG <- update_designs(designsG)

# Fit models --------------------------------------------------------------

models <- fitmodels(designsG$d.design.adults)
modelsummaries <- getmodelsummaries(models)
immune_models <- fit_immune_models(designsG$d.design.adults)
intake_models <- fit_intake_models(designsG$d.design.dietary.adults)
d_allstats <- allstats(models)
d_strength_stats <- strength_stats(models)
d_lifetime_stats <- lifetime_stats(models)
marginal <- marginals(models)

plot_allcoefs <-
  ggplot(d_allstats, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = Controls)) +
  geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
  geom_vline(xintercept=0, linetype='longdash') +
  labs(x='\nEstimate (95% CI)', y='') +
  facet_wrap(~ Outcome, ncol = 4, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.title = element_blank())
plot_allcoefs
ggsave("Figures/plot_allcoefs.pdf", plot_allcoefs, width = 14, height = 8)

plot_coefs <-
  ggplot(d_strength_stats, aes(estimate, Outcome, xmin = conf.low, xmax = conf.high, colour = Significant)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = 'none') + # guide_legend(reverse = T)
  labs(title='Stage 1: Pilot study', x = 'Estimate (95% CI)', y = '') +
  facet_grid(Controls ~ term) +
  theme_bw(15) +
  theme(
    strip.text.y = element_blank()
  )
  # theme(strip.text.y = element_text(angle = 0))
plot_coefs
ggsave("Figures/plot_coefs.pdf", plot_coefs, width = 10, height = 10)

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
      dplyr::mutate(ratio = round(as.numeric(gsub(",", "", stat_1))/as.numeric(gsub(",", "", stat_2)), digits = 2))
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
