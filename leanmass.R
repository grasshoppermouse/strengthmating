#+ message=F, warning=F, fig.width=11
source("functions.R")

designsG2 <- update_designs(designsG)
designsH2 <- update_designs(designsH)
designsGH2 <- update_designs(designsGH)

lean_mass_plot <- function(title, muscle, muscle_sex) {
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
  )

  p1 <- lean_stats |>
    dplyr::filter(benefit_cost == "Benefit", term2 %in% sex_strength_terms) |>
    ggplot(aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
    geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    scale_color_binary() +
    labs(title = title, x = "\nEstimate (95% CI)", y = "") +
    facet_grid(Outcome ~ term) +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle = 0))

  print(p1)

  p2 <- lean_stats |>
    dplyr::filter(str_detect(Outcome, "Energy|Protein"), term2 %in% sex_strength_terms) |>
    ggplot(aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant, shape = Stage)) +
    geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
    geom_vline(xintercept = 0, linetype = "longdash") +
    scale_color_binary(direction = -1) +
    labs(title = title, x = "\nEstimate (95% CI)", y = "") +
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
    labs(title = title, x = "\nEstimate (95% CI)", y = "") +
    facet_wrap(~term) +
    theme_bw(15) +
    theme(strip.text.y = element_text(angle = 0))

  print(p3)
}

lean_mass_plot("Arm lean mass", "arm_lean_centered", "arm_lean_sex_centered")
lean_mass_plot("Leg lean mass", "leg_lean_centered", "leg_lean_sex_centered")
lean_mass_plot("Trunk lean mass", "trunk_lean_centered", "trunk_lean_sex_centered")
lean_mass_plot("Upper lean mass", "upper_lean_centered", "upper_lean_sex_centered")
lean_mass_plot("Total lean mass", "total_lean_centered", "total_lean_sex_centered")

# lean_mass_plot("Arm lean BMC mass", 'arm_leanbmc_centered', 'arm_leanbmc_sex_centered')
# lean_mass_plot("Leg lean BMC mass", 'leg_leanbmc_centered', 'leg_leanbmc_sex_centered')
# lean_mass_plot("Trunk lean BMC mass", 'trunk_leanbmc_centered', 'trunk_leanbmc_sex_centered')
# lean_mass_plot("Upper lean BMC mass", 'upper_leanbmc_centered', 'upper_leanbmc_sex_centered')
# lean_mass_plot("Total lean BMC mass", 'total_leanbmc_centered', 'total_leanbmc_sex_centered')

#' # Combined G & H series

# modelsGH <-
#   fitmodels(
#     designsGH2$d.design.adults,
#     designsGH2$d.design.dietary.adults,
#     designsGH$d.design.adults,
#     muscle = muscle,
#     muscle_sex = muscle_sex
#   )

# lean_statsGH <- allstats(modelsGH)

# lean_statsGH |>
# dplyr::filter(benefit_cost == "Benefit", term2 %in% sex_strength_terms) |>
# ggplot(aes(estimate, Controls, xmin = conf.low, xmax = conf.high, colour = Significant)) +
# geom_pointrange(size = 0.4, linewidth = 0.7, position = position_dodge(width = 0.8)) +
# geom_vline(xintercept = 0, linetype = "longdash") +
# scale_color_binary() +
# labs(title = 'Combined G and H', x = "\nEstimate (95% CI)", y = "") +
# facet_grid(Outcome~term) +
# theme_bw(15) +
# theme(strip.text.y = element_text(angle = 0))
# d <- d_G |> dplyr::select(strength, ArmLeanexclBMC, ArmLeaninclBMC, TrunkLeanexclBMC, TrunkLeaninclBMC, TotalLeanexclBMC, TotalLeaninclBMC, UpperLeanexclBMC, UpperLeaninclBMC, LegLeanexclBMC, LegLeaninclBMC, TotalPercentFat)
# ggcorrplot(cor(d, use = 'pairwise.complete.obs'), lab = T, hc.order = T, hc.method = 'ward.D')
