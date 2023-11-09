


# anthropometric model ----------------------------------------------------


mm1 <- tbl_regression(
  m_lifetime,
  label = vnames
) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


mm2 <- tbl_regression(
  m_pastyear,
  label = vnames
) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

mm3 <- tbl_regression(
  m_agefirst,
  label = vnames
) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

mm4 <- tbl_regression(
    m_partnered,
    label = vnames
  ) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(mm1, mm2, mm3, mm4),
  tab_spanner = c(
    "Lifetime number of sexual partners",
    "Past year number of sexual partners",
    "Age at first sexual intercourse",
    "Currently partnered"
  )
) %>%
  as_gt() %>%
  gt::tab_header(
    title = "Table 2. Baseline Models of Mating Success",
    subtitle = "Regression coefficients from generalized
                 linear models for three indices of mating success for men and women ages
                 18-59. Models derived from Lassek and Gaulin 2009"
  )

summary(m_lifetime, df.resid = Inf)
summary(m_pastyear, df.resid = Inf)
summary(m_agefirst, df.resid = Inf)
summary(m_partnered, df.resid = Inf)

# lifetime partners ----------------------------------------------------------

l1 <- tbl_regression(msoc1, label = vnames)  %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l2 <- tbl_regression(mheal1, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l3 <- tbl_regression(mhor1, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l4 <- tbl_regression(mphys1, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(l1, l2, l3, l4),
  tab_spanner = c("Socioeconomic",
                  "Health",
                  "Hormone",
                  "Physical Activity")) %>%
  as_gt() %>%
  gt::tab_header(title = "Lifetime number of partners control models",
                 subtitle = "Regression coefficients from generalized
                 linear models")


summary(msoc1, df.resid = Inf)
summary(mheal1, df.resid = Inf)
summary(mhor1, df.resid = Inf)
summary(mphys1, df.resid = Inf)



# past year partners ----------------------------------------------------------


l1 <- tbl_regression(msoc2, label = vnames)  %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l2 <- tbl_regression(mheal2, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l3 <- tbl_regression(mhor2, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l4 <- tbl_regression(mphys2, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(l1, l2, l3, l4),
  tab_spanner = c("Socioeconomic",
                  "Health",
                  "Hormone",
                  "Physical Activity")) %>%
  as_gt() %>%
  gt::tab_header(title = "Past year number of partners control models",
                 subtitle = "Regression coefficients from generalized
                 linear models")


summary(msoc2, df.resid = Inf)
summary(mheal2, df.resid = Inf)
summary(mhor2, df.resid = Inf)
summary(mphys2, df.resid = Inf)



# age at first sex  -----------------------------------------------------------------


l1 <- tbl_regression(msoc3, label = vnames)  %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l2 <- tbl_regression(mheal3, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l3 <- tbl_regression(mhor3, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l4 <- tbl_regression(mphys3, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(l1, l2, l3, l4),
  tab_spanner = c("Socioeconomic",
                  "Health",
                  "Hormone",
                  "Physical Activity")) %>%
  as_gt() %>%
  gt::tab_header(title = "Past year number of partners control models",
                 subtitle = "Regression coefficients from generalized
                 linear models")


summary(msoc3, df.resid = Inf)
summary(mheal3, df.resid = Inf)
summary(mhor3, df.resid = Inf)
summary(mphys3, df.resid = Inf)



# partnered ---------------------------------------------------------------


l1 <- tbl_regression(msoc4, label = vnames)  %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l2 <- tbl_regression(mheal4, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l3 <- tbl_regression(mhor4, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

l4 <- tbl_regression(mphys4, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(l1, l2, l3, l4),
  tab_spanner = c("Socioeconomic",
                  "Health",
                  "Hormone",
                  "Physical Activity")) %>%
  as_gt() %>%
  gt::tab_header(title = "Past year number of partners control models",
                 subtitle = "Regression coefficients from generalized
                 linear models")


s1 <- summary(msoc4, df.resid = Inf)
summary(mheal4, df.resid = Inf)
summary(mhor4, df.resid = Inf)
summary(mphys4, df.resid = Inf)




# wbc count  ------------------------------------------------------

i1 <- tbl_regression(mwbc, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


i2 <- tbl_regression(mwbc_alt, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(i1, i2),
  tab_spanner = c("Replication Model",
                  "Expanded Control Model")) %>%
  as_gt() %>%
  gt::tab_header(title = "Native Immune Investment (WBCC)",
                 subtitle = "Regression coefficients from generalized
                 linear models")

summary(mwbc, df.resid = Inf)
summary(mwbc_alt, df.resid = Inf)


# dietary energy ----------------------------------------------------------


d1 <- tbl_regression(m_energy, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)


d2 <- tbl_regression(m_energy_alt, label = vnames) %>%
  add_significance_stars(hide_se = TRUE,
                         pattern = "{estimate}{stars}<br>({std.error})") %>%
  modify_header(label ~ "**Variable**", estimate ~ "**Estimate (SE)**") %>%
  modify_footnote(estimate ~ "SE = Standard Error", abbreviation = TRUE)

tbl_merge(
  tbls = list(d1, d2),
  tab_spanner = c("Replication Model",
                  "Expanded Control Model")) %>%
  as_gt() %>%
  gt::tab_header(title = "Dietary Energy Intake (kcals)",
                 subtitle = "Regression coefficients from generalized
                 linear models")

summary(m_energy, df.resid = Inf)
summary(m_energy_alt, df.resid = Inf)
