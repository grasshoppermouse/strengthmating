

m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)
summary(m_lifetime, df.resid = Inf)



m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered +
    weight_centered * sex +
    height_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

summary(m_lifetime, df.resid = Inf)



#04.11.2023
library(jtools)

m_lifetime <- svyglm(
  sex_partners ~
    strength_centered * sex +
    age_centered * sex +
    partnered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

summary(m_lifetime, df.resid = Inf)
sum <- summary(m_lifetime, df.resid = Inf)$coefficients

summ(m_lifetime, df.resid = Inf)


#04.07.2023
library(hagenutils)
fem <- svysmooth(whitebloodcell~age, designsG$d.design.adult.female)

mal <- svysmooth(whitebloodcell~age, designsG$d.design.adult.male)

d <- svysmooth2df(female = fem, male = mal)

ggplot(d, aes(age, whitebloodcell, colour = Smooth)) + geom_line()


library(mgcv)

m <- gam(whitebloodcell ~ s(strength, by = sex) + s(age, by = sex), data = d_G)


#03.31.23

cordat <- d_G[, c("age", "strength", "bmi")]
cordat <- cordat[cordat$age>=18 & cordat$age<60,]
cormat <- round(cor(cordat, use = "complete.obs"),2)


cordat <- d_Gadults[, c("age", "strength", "bmi", "testosterone")]
cordat <- cordat[cordat$sex == "male",]
cormat <- round(cor(cordat, use = "complete.obs"),2)


ggplot(d_G) +
     aes(x = "", y = testosterone) + facet_wrap(~sex) +
      geom_boxplot(fill = "#0c4c8a") +
      theme_minimal()

d_Gadults <- d_G[d_G$age >= 18 & d_G$age <= 60,]
tapply(d_Gadults$testosterone, d_Gadults$sex, summary)


# 03.23.23 correlation matrix ------------------------------------------------------
library(jtools)
library(qgraph)
library(corrplot)

# cordat <- d_G[, c("age", "sex1", "strength", "bmi")]
# cordat <- cordat[cordat$age>=18 & cordat$age<60,]
# cormat <- round(cor(cordat, use = "complete.obs"),2)


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
corr_f <- corrplot(cor_mat_cors_f, method = 'shade', addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75,
         title = "Correlation matrix of coninuous study variables, Females 18-60")

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
corrplot(cor_mat_cors_m, method = 'shade', addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75)

corr_m <- corrplot(cor_mat_cors_m, method = 'shade', addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75,
         title = "Correlation matrix of coninuous study variables, Males 18-60")



# meltedcor_mat <- melt(cor_mat_cors)
#
# ggplot(data = meltedcor_mat, aes(x=Var1, y=Var2, fill = value)) +
#   geom_tile()



design_new <- update(designsG$d.design.adults, sex2 = ifelse(sex == "male", 1, 0))

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
  design_new,
  na.rm = T
)

cor_mat_cors <- cor_mat$cors
corrplot(cor_mat_cors, addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75)

p <- EBICglasso(cor_mat_cors, 2800)
qgraph(p, layout = "spring")



# coef plots mating models ------------------------------------------------



m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

m_pastyear <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

m_agefirst <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = gaussian(),
  design = designsG$d.design.adults
)


mnames1 <- c(
  "Age at first intercourse",
  "Lifetime Number of Sexual Partners",
  "Past Year Number of Sexual Partners"
)

vnames1 <- c(
  "strength_centered" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI"
)

forestplot(m_agefirst, m_lifetime, m_pastyear, intercept = F, facet = F, dodgewidth = .5, modelnames = mnames1,
           varnames = vnames1)$plot + theme_minimal(20) +
  geom_pointrange(size = 1.2, position = position_dodge(width = .5)) +
  labs(title = "Exact Models: Regression coefficients from generalized linear models", color = "Model") +
  guides(colour = guide_legend(reverse = T))




mwbc <- svyglm(whitebloodcell ~
                 age_centered * sex +
                 strength_centered * sex +
                 bmi_centered * sex +
                 testosterone_sex_centered * sex,
               family= quasipoisson(),
               design=designsG$d.design.adults)
summary(mwbc)




forestplot(
  mwbc,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  varnames = vnames)$plot +
  theme_minimal(25) +
  geom_pointrange(size = 2, position = position_dodge(width = .8)) +
  labs(title = "White blood cell count") +
  # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
  #      subtitle = "White Blood Cell Count",
  #      caption = "Variables with (S) have been standarized by 2 SD.") +
  theme(legend.position = "none")



mwbc1 <- svyglm(whitebloodcell ~
                 age_centered * sex +
                 strength_centered * sex +
                 bmi_centered * sex +
                 testosterone_centered * sex,
               family= quasipoisson(),
               design=designsG$d.design.adults)
summary(mwbc1)


vnames <- c(
  "strength_centered" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI",
  "testosterone_centered" = "Testosterone",
  "sexfemale:testosterone_centered" = "Testosterone x Sex Female"
)

forestplot(
  mwbc1,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  varnames = vnames)$plot +
  theme_minimal(25) +
  geom_pointrange(size = 2, position = position_dodge(width = .8)) +
  labs(title = "White blood cell count") +
  # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
  #      subtitle = "White Blood Cell Count",
  #      caption = "Variables with (S) have been standarized by 2 SD.") +
  theme(legend.position = "none")


# 3.23 --------------------------------------------------------------------

library(qgraph)

design_new <- update(designsG$d.design.adults, sex2 = ifelse(sex == "male", 1, 0))

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
  design_new,
  na.rm = T
)

cor_mat_cors <- cor_mat$cors
corrplot(cor_mat_cors, addCoef.col = "black", number.digits = 2, type = "lower", number.cex = 0.75)

p <- EBICglasso(cor_mat_cors, 2800)
qgraph(p, layout = "spring")


m_lifetime <- svyglm(
  sex_partners ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

m_pastyear <- svyglm(
  sex_partners_year ~
    age_centered * sex +
    strength_centered * sex +
    partnered * strength_centered +
    bmi_centered * sex,
  family = quasipoisson(),
  design = designsG$d.design.adults
)

m_agefirst <-  svyglm(
  age_first_sex ~
    age_centered * sex +
    strength_centered * sex +
    partnered  +
    bmi_centered * sex,
  family = gaussian(),
  design = designsG$d.design.adults
)


mnames1 <- c(
  "Age at first intercourse",
  "Lifetime Number of Sexual Partners",
  "Past Year Number of Sexual Partners"
)

vnames1 <- c(
  "strength_centered" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI"
)

forestplot(m_agefirst, m_lifetime, m_pastyear, intercept = F, facet = F, dodgewidth = .5, modelnames = mnames1,
           varnames = vnames1)$plot + theme_minimal(20) +
  geom_pointrange(size = 1.2, position = position_dodge(width = .5)) +
  labs(title = "Exact Models: Regression coefficients from generalized linear models", color = "Model") + guides(colour = guide_legend(reverse = T))




mwbc <- svyglm(whitebloodcell ~
                 age_centered * sex +
                 strength_centered * sex +
                 bmi_centered * sex +
                 testosterone_sex_centered * sex,
               family= quasipoisson(),
               design=designsG$d.design.adults)
summary(mwbc)




forestplot(
  mwbc,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  varnames = vnames)$plot +
  theme_minimal(25) +
  geom_pointrange(size = 2, position = position_dodge(width = .8)) +
  labs(title = "White blood cell count") +
  # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
  #      subtitle = "White Blood Cell Count",
  #      caption = "Variables with (S) have been standarized by 2 SD.") +
  theme(legend.position = "none")



mwbc1 <- svyglm(whitebloodcell ~
                  age_centered * sex +
                  strength_centered * sex +
                  bmi_centered * sex +
                  testosterone_centered * sex,
                family= quasipoisson(),
                design=designsG$d.design.adults)
summary(mwbc1)


vnames <- c(
  "strength_centered" = "Strength (S)",
  "sexfemale" = "Sex (Female)",
  "sexfemale:strength_centered" = "Sex (Female) x Strength",
  "age_centered" = "Age (S)",
  "partneredTRUE" = "Partnered",
  "strength_centered:partneredTRUE" = "Partnered x Strength (S)",
  "age_centered:sexfemale" = "Age x Sex (Female)",
  "height_centered" = "Height (S)",
  "weight_centered" = "Weight (S)",
  "bmi_centered" = "BMI (S)",
  "sexfemale:bmi_centered" = "Sex (Female) x BMI",
  "testosterone_centered" = "Testosterone",
  "sexfemale:testosterone_centered" = "Testosterone x Sex Female"
)

forestplot(
  mwbc1,
  intercept = F,
  facet = F,
  dodgewidth = .8,
  varnames = vnames)$plot +
  theme_minimal(25) +
  geom_pointrange(size = 2, position = position_dodge(width = .8)) +
  labs(title = "White blood cell count") +
  # labs(title = "Figure.3 Regression coefficients from generalized linear model of immune investment.",
  #      subtitle = "White Blood Cell Count",
  #      caption = "Variables with (S) have been standarized by 2 SD.") +
  theme(legend.position = "none")
