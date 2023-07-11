library(fitdistrplus)

# Daily Energy Intake (Table 3) -------------------------------------------

# bone mineral density not collected in G, collected in H in altered sample

# compute occupational expenditure per Lassek and Gaulin
# we have MET from participant reported occ exertion, a better var than this, also NHANES recommends using
# diff weights for dietary data
#
# occ_exertion <- svyby(~avgcalories, ~census_code_best, design = designsG$d.design.adults, svymean)
# occ_exertion_dict <- occ_exertion$avgcalories
# names(occ_exertion_dict) <- occ_exertion$census_code_curr
# designsG$d.design.adults <- update(designsG$d.design.adults, occ_exertion = occ_exertion_dict[census_code_curr])
#
#
# x <- svyby(~avgcalories, ~race, design = designsG$d.design.adults, svymean, na.rm = T)

# not including in rep because we do not have ffm


x <- c(na.omit(d_G$avgcalories))
x <- c(na.omit(d_G$whitebloodcell))

descdist(x, discrete = T)


adults = d_G$age>=18 & d_G$age<=60

d.design.dietary1 <-
  svydesign(
    id = ~SDMVPSU ,
    strata = ~SDMVSTRA ,
    nest = TRUE ,
    weights = ~WTMEC2YR, #~WTINT2YR ,
    data = d_G
  )

d.design.dietary.adults1 <-
  subset(
    d.design.dietary1,
    adults
  )

# m <- svyglm(
#   log(avgcalories) ~
#     age +
#     sex * tot_MET +
#     strength +
#     weight +
#     height,
#   family = gaussian(),
#   design = d.design.dietary.adults
# )
# summary(m)
# plot(allEffects(m))
#


m <- svyglm(
  log(avgcalories) ~
    age +
    total_work_MET  +
    total_rec_MET +
    wob_MET +
    strength +
    height +
    weight,
  family = gaussian(),
  design = d.design.dietary.adults1
)
summary(m, df.resid = Inf)
plot(allEffects(m))

m_energy1 <- svyglm( #use this
  avgcalories ~
    age +
    total_work_MET  +
    total_rec_MET +
    wob_MET +
    strength +
    height +
    weight,
  family = gaussian(),
  design = d.design.dietary.adults1
)
summary(m_energy1, df.resid = Inf)
plot(m)


# use diff weights --------------------------------------------------------

# Most analyses of NHANES data use data collected in the MEC and the variable WTMEC2YR should be used for the sample weights.
# However, for the WWEIA dietary data, different sample weights are recommended for analysis. Although attempts are made to
# schedule MEC exams uniformly throughout the week, proportionally more exams occur on weekend days than on weekdays. Because
# food intake can vary by day of the week, use of the MEC weights would disproportionately represent intakes on weekends.
#
# A set of weights (WTDRD1) is provided that should be used when an analysis uses the Day 1 dietary recall data (either alone
# or when Day 1 nutrient data are used in conjunction with MEC data). The set of weights (WTDRD1) is applicable to the 8,519
# participants with Day 1 data. Day 1 weights were constructed by taking the MEC sample weights (WTMEC2YR) and further adjusting
# for (a) the additional non-response and (b) the differential allocation by day of the week for the dietary intake data
# collection. These Day 1 weights are more variable than the MEC weights, and the sample size is smaller, so estimated standard
# errors using Day 1 data and Day 1 weights are larger than standard errors for similar estimates based on MEC weights.
#
# When analysis is based on both days of dietary intake, only 7,605 sample participants have complete data. The NHANES protocol
# requires an attempt to collect the second day of dietary data at least 3 days after the first day, but the actual number of
# days between the two interviews is variable. A set of adjusted weights, WTDR2D, is to be used only when an analysis uses both
# Day 1 and Day 2 dietary data. This two-day weight was constructed for the 7,605 participants by taking the Day 1 weights
# (WTDRD1) and further adjusting for (a) the additional non-response for the second recall and (b) for the proportion of
# weekend-weekday combinations of Day 1 and Day 2 recalls.
#
# Note that all sample weights are person-level weights and each set of dietary weights should sum to the same population
# control total as the MEC weights (WTMEC2YR). In addition, the MEC weights (WTMEC2YR) are appropriate for use in the analysis
# of the fish and shellfish consumption data (i.e., variables DRD340, DRD350A-K, DRD350AQ-JQ DRD360, DRD370A-V, and DRD370AQ-UQ)
# located in the Day 1 Total Nutrient Intake File (DR1TOT_G), if no other dietary data are included in the analysis. Additional
# explanation of sample weights and appropriate uses are included in the NHANES Analytic Guidelines. Please also refer to the
# on-line NHANES Tutorial for further details on other analytic issues.

#sum(is.na(d_G$WTDR2D)) = 2151

#7605
adults_diet = !is.na(d_G$WTDR2D) & d_G$age>=18 & d_G$age<=60

adultsd = !is.na(d_G$WTDR2D)

d_G_diet <- d_G[!is.na(d_G$WTDR2D), ]
adults_diet = d_G_diet$age>=18 & d_G_diet$age<=60

d.design.dietary <-
  svydesign(
    id = ~SDMVPSU ,
    strata = ~SDMVSTRA ,
    nest = TRUE ,
    weights = ~WTDR2D , #~WTMEC2YR, #~WTINT2YR ,
    data = d_G_diet
  )

d.design.dietary.adults <-
  subset(
    d.design.dietary,
    adults_diet
  )

m1 <- svyglm(
  log(avgcalories) ~
    age +
    total_work_MET  +
    total_rec_MET +
    wob_MET +
    strength +
    height +
    weight,
  family = gaussian(),
  design = d.design.dietary.adults
)
summary(m1, df.resid = Inf)
plot(allEffects(m))

m_energy <- svyglm( #use this
  avgcalories ~
    age +
    total_work_MET  +
    total_rec_MET +
    wob_MET +
    strength +
    height +
    weight,
  family = gaussian(),
  design = d.design.dietary.adults
)
summary(m_energy, df.resid = Inf)
plot(m)

