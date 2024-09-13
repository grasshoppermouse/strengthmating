# Quantile regression -----------------------------------------------------

library(Qtools)
library(quantreg)

d <-
  d_GH |>
  dplyr::filter(age >=18, age <= 60) |>
  mutate(
    age_centered = c(scale(age))/2,
    whitebloodcell_centered = c(scale(whitebloodcell))/2,
    hemoglobin_centered = c(scale(hemoglobin))/2,
    years_sexually_mature = age - 12
  ) |>
  group_by(sex) |>
  mutate(
    strength_sex_centered = c(scale(strength))/2,
    testosterone_sex_centered = c(scale(testosterone))/2
  ) |>
  ungroup()

formulas <- c(
  anthro_lifetime  = "sex_partners ~ offset(log(years_sexually_mature)) + strength_sex_centered*sex + partnered + bmi*sex",
  anthro_year      = "sex_partners_year ~ age_centered * sex + strength_sex_centered*sex + partnered + bmi*sex",
  socio_lifetime   = "sex_partners ~ offset(log(years_sexually_mature)) + strength_sex_centered * sex + partnered + edu + race",
  socio_year       = "sex_partners_year ~ age_centered * sex + strength_sex_centered * sex + partnered + edu + race",
  health_lifetime  = "sex_partners ~ offset(log(years_sexually_mature)) + strength_sex_centered * sex + partnered + perceived_abnormal_weight + whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression",
  health_year      = "sex_partners_year ~ age_centered * sex + strength_sex_centered * sex + partnered + perceived_abnormal_weight + whitebloodcell_centered + hemoglobin_centered + special_equipment + chronic_disease_score + physical_disease_count + depression",
  phys_lifetime    = "sex_partners ~ offset(log(years_sexually_mature)) + strength_sex_centered * sex + partnered + vigorous_rec + moderate_rec + vigorous_work + moderate_work",
  phys_year        = "sex_partners_year ~ age_centered * sex + strength_sex_centered * sex + partnered + vigorous_rec + moderate_rec + vigorous_work + moderate_work",
  hormone_lifetime = "sex_partners ~ offset(log(years_sexually_mature)) + strength_sex_centered * sex + partnered * strength_sex_centered + testosterone_sex_centered * sex",
  hormone_year     = "sex_partners_year ~ age_centered * sex + strength_sex_centered * sex + partnered * strength_sex_centered + testosterone_sex_centered * sex"
)

partners_rq <- function(f, tau){
  print(tau)
  m <- rq.counts(
    as.formula(f),
    tau = tau,
    data = d
  )
  out <- m$tTable[c('strength_sex_centered', 'strength_sex_centered:sexfemale'), c('Value', 'lower bound', 'upper bound')]
  colnames(out) <- c('estimate', 'conf.low', 'conf.high')
  as_tibble(out, rownames = 'term')
}

out <-
  purrr::map(seq(0.1, 0.90, 0.05), \(tau) partners_rq(formulas[1], tau)) |>
  list_rbind(names_to = 'tau') |>
  mutate(
    tau = as.character(tau/20)
  )

ggplot(out, aes(estimate, tau, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  scale_color_binary() +
  guides(colour = guide_legend(reverse = T)) +
  labs(x = 'Coefficient', y = '') +
  facet_wrap(~term) +
  theme_bw(15)

# out2 <-
#   purrr::map(formulas, \(f) partners_rq(f, 0.25)) |>
#   list_rbind(names_to = 'model')
#
# ggplot(out2, aes(estimate, model, xmin = conf.low, xmax = conf.high)) +
#   geom_pointrange() +
#   geom_vline(xintercept = 0, linetype = 'dotted') +
#   scale_color_binary() +
#   guides(colour = guide_legend(reverse = T)) +
#   labs(x = 'Coefficient', y = '') +
#   facet_wrap(~term) +
#   theme_bw(15)
