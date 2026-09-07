# Базовые смешанные модели со случайными эффектами на время и фиксированными по годам

library(lme4)
library(clubSandwich)
library(performance)
library(modelsummary)

# Чтение файлов

y2 <- read.csv('https://raw.githubusercontent.com/ShapovalovaMariia/coursework3/refs/heads/main/y2_R.csv')
y3 <- read.csv('https://raw.githubusercontent.com/ShapovalovaMariia/coursework3/refs/heads/main/y3_R.csv')

#spec1 = ['ln_population', 'ln_grp_pc_L1', 'log_growth', 'russian_share'] # базовая спецификация
#spec2 = ['ln_population', 'ln_grp_pc_L1', 'log_growth', 'russian_share', 'turnout'] # + административный ресурс (явка)
#spec3 = ['ln_population', 'ln_grp_pc_L1', 'log_growth', 'russian_share', 'UR'] # + элект. лояльность партии власти
#spec4 = ['ln_population', 'ln_grp_pc_L1', 'log_growth', 'russian_share', 'Putin'] # + элект. лояльность президенту

#########################################################################################
##### Политически чувствительные трансферты (PST) | #####################################
##### Y2 (доля трансфертов в регионе, относительно всех федеральных трансфертов, %) #####
#########################################################################################

#####
### Спецификация 1.a PST ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1a_mixed <- lmer(
  PST ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1a <- vcovCR(m1a_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m1a_mixed, vcov = V_cr1a)

######
### Спецификация 2.a PST ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
######

m2a_mixed <- lmer(
  PST ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2a <- vcovCR(m2a_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m2a_mixed, vcov = V_cr2a)

######
### Спецификация 3.a PST ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
######

m3a_mixed <- lmer(
  PST ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3a <- vcovCR(m3a_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m3a_mixed, vcov = V_cr3a)

###### 
### Спецификация 4.a PST ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
######

m4a_mixed <- lmer(
  PST ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4a <- vcovCR(m4a_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m4a_mixed, vcov = V_cr4a)

#####
### Спецификация 1.b PST ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1b_mixed <- lmer(
  PST ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1b <- vcovCR(m1b_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m1b_mixed, vcov = V_cr1b)

#####
### Спецификация 2.b PST ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
#####

m2b_mixed <- lmer(
  PST ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2b <- vcovCR(m2b_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m2b_mixed, vcov = V_cr2b)

#####
### Спецификация 3.b PST ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
#####

m3b_mixed <- lmer(
  PST ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3b <- vcovCR(m3b_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m3b_mixed, vcov = V_cr3b)

#####
### Спецификация 4.b PST ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
#####

m4b_mixed <- lmer(
  PST ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4b <- vcovCR(m4b_mixed, cluster = y2$region_name, type = "CR0")

coef_test(m4b_mixed, vcov = V_cr4b)


#########################################################################################
##### Субсидии (subsidies) | ############################################################
##### Y2 (доля трансфертов в регионе, относительно всех федеральных трансфертов, %) #####
#########################################################################################

#####
### Спецификация 1.a subsidies ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1a_sub <- lmer(
  subsidies ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1a_sub <- vcovCR(m1a_sub, cluster = y2$region_name, type = "CR0")

coef_test(m1a_sub, vcov = V_cr1a_sub)

######
### Спецификация 2.a subsidies ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
######

m2a_sub <- lmer(
  subsidies ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2a_sub <- vcovCR(m2a_sub, cluster = y2$region_name, type = "CR0")

coef_test(m2a_sub, vcov = V_cr2a_sub)

######
### Спецификация 3.a subsidies ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
######

m3a_sub <- lmer(
  subsidies ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3a_sub <- vcovCR(m3a_sub, cluster = y2$region_name, type = "CR0")

coef_test(m3a_sub, vcov = V_cr3a_sub)

###### 
### Спецификация 4.a subsidies ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
######

m4a_sub <- lmer(
  subsidies ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4a_sub <- vcovCR(m4a_sub, cluster = y2$region_name, type = "CR0")

coef_test(m4a_sub, vcov = V_cr4a_sub)

#####
### Спецификация 1.b subsidies ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1b_sub <- lmer(
  subsidies ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1b_sub <- vcovCR(m1b_sub, cluster = y2$region_name, type = "CR0")

coef_test(m1b_sub, vcov = V_cr1b_sub)

#####
### Спецификация 2.b subsidies ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
#####

m2b_sub <- lmer(
  subsidies ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2b_sub <- vcovCR(m2b_sub, cluster = y2$region_name, type = "CR0")

coef_test(m2b_sub, vcov = V_cr2b_sub)

#####
### Спецификация 3.b subsidies ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
#####

m3b_sub <- lmer(
  subsidies ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3b_sub <- vcovCR(m3b_sub, cluster = y2$region_name, type = "CR0")

coef_test(m3b_sub, vcov = V_cr3b_sub)

#####
### Спецификация 4.b subsidies ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
#####

m4b_sub <- lmer(
  subsidies ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4b_sub <- vcovCR(m4b_sub, cluster = y2$region_name, type = "CR0")

coef_test(m4b_sub, vcov = V_cr4b_sub)


#########################################################################################
##### Другие дотации (other_dotations) | ################################################
##### Y2 (доля трансфертов в регионе, относительно всех федеральных трансфертов, %) #####
#########################################################################################

#####
### Спецификация 1.a other_dotations ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1a_oth <- lmer(
  other_dotations ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1a_oth <- vcovCR(m1a_oth, cluster = y2$region_name, type = "CR0")

coef_test(m1a_oth, vcov = V_cr1a_oth)

######
### Спецификация 2.a other_dotations ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
######

m2a_oth <- lmer(
  other_dotations ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2a_oth <- vcovCR(m2a_oth, cluster = y2$region_name, type = "CR0")

coef_test(m2a_oth, vcov = V_cr2a_oth)

######
### Спецификация 3.a other_dotations ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
######

m3a_oth <- lmer(
  other_dotations ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3a_oth <- vcovCR(m3a_oth, cluster = y2$region_name, type = "CR0")

coef_test(m3a_oth, vcov = V_cr3a_oth)

###### 
### Спецификация 4.a other_dotations ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
######

m4a_oth <- lmer(
  other_dotations ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4a_oth <- vcovCR(m4a_oth, cluster = y2$region_name, type = "CR0")

coef_test(m4a_oth, vcov = V_cr4a_oth)

#####
### Спецификация 1.b other_dotations ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1b_oth <- lmer(
  other_dotations ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1b_oth <- vcovCR(m1b_oth, cluster = y2$region_name, type = "CR0")

coef_test(m1b_oth, vcov = V_cr1b_oth)

#####
### Спецификация 2.b other_dotations ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
#####

m2b_oth <- lmer(
  other_dotations ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2b_oth <- vcovCR(m2b_oth, cluster = y2$region_name, type = "CR0")

coef_test(m2b_oth, vcov = V_cr2b_oth)

#####
### Спецификация 3.b other_dotations ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
#####

m3b_oth <- lmer(
  other_dotations ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3b_oth <- vcovCR(m3b_oth, cluster = y2$region_name, type = "CR0")

coef_test(m3b_oth, vcov = V_cr3b_oth)

#####
### Спецификация 4.b other_dotations ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
#####

m4b_oth <- lmer(
  other_dotations ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4b_oth <- vcovCR(m4b_oth, cluster = y2$region_name, type = "CR0")

coef_test(m4b_oth, vcov = V_cr4b_oth)


#########################################################################################
##### Иные межбюджетные трансферты (other_transfers) | ##################################
##### Y2 (доля трансфертов в регионе, относительно всех федеральных трансфертов, %) #####
#########################################################################################

#####
### Спецификация 1.a other_transfers ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1a_tr <- lmer(
  other_transfers ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1a_tr <- vcovCR(m1a_tr, cluster = y2$region_name, type = "CR0")

coef_test(m1a_tr, vcov = V_cr1a_tr)

######
### Спецификация 2.a other_transfers ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
######

m2a_tr <- lmer(
  other_transfers ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2a_tr <- vcovCR(m2a_tr, cluster = y2$region_name, type = "CR0")

coef_test(m2a_tr, vcov = V_cr2a_tr)

######
### Спецификация 3.a other_transfers ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
######

m3a_tr <- lmer(
  other_transfers ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3a_tr <- vcovCR(m3a_tr, cluster = y2$region_name, type = "CR0")

coef_test(m3a_tr, vcov = V_cr3a_tr)

###### 
### Спецификация 4.a other_transfers ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
######

m4a_tr <- lmer(
  other_transfers ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4a_tr <- vcovCR(m4a_tr, cluster = y2$region_name, type = "CR0")

coef_test(m4a_tr, vcov = V_cr4a_tr)

#####
### Спецификация 1.b other_transfers ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1b_tr <- lmer(
  other_transfers ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1b_tr <- vcovCR(m1b_tr, cluster = y2$region_name, type = "CR0")

coef_test(m1b_tr, vcov = V_cr1b_tr)

#####
### Спецификация 2.b other_transfers ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
#####

m2b_tr <- lmer(
  other_transfers ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2b_tr <- vcovCR(m2b_tr, cluster = y2$region_name, type = "CR0")

coef_test(m2b_tr, vcov = V_cr2b_tr)

#####
### Спецификация 3.b other_transfers ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
#####

m3b_tr <- lmer(
  other_transfers ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3b_tr <- vcovCR(m3b_tr, cluster = y2$region_name, type = "CR0")

coef_test(m3b_tr, vcov = V_cr3b_tr)

#####
### Спецификация 4.b other_transfers ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
#####

m4b_tr <- lmer(
  other_transfers ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4b_tr <- vcovCR(m4b_tr, cluster = y2$region_name, type = "CR0")

coef_test(m4b_tr, vcov = V_cr4b_tr)


#########################################################################################
##### Субвенции (subventions) | #########################################################
##### Y2 (доля трансфертов в регионе, относительно всех федеральных трансфертов, %) #####
#########################################################################################

#####
### Спецификация 1.a subventions ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1a_subv <- lmer(
  subventions ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1a_subv <- vcovCR(m1a_subv, cluster = y2$region_name, type = "CR0")

coef_test(m1a_subv, vcov = V_cr1a_subv)

######
### Спецификация 2.a subventions ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
######

m2a_subv <- lmer(
  subventions ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2a_subv <- vcovCR(m2a_subv, cluster = y2$region_name, type = "CR0")

coef_test(m2a_subv, vcov = V_cr2a_subv)

######
### Спецификация 3.a subventions ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
######

m3a_subv <- lmer(
  subventions ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3a_subv <- vcovCR(m3a_subv, cluster = y2$region_name, type = "CR0")

coef_test(m3a_subv, vcov = V_cr3a_subv)

###### 
### Спецификация 4.a subventions ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
######

m4a_subv <- lmer(
  subventions ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4a_subv <- vcovCR(m4a_subv, cluster = y2$region_name, type = "CR0")

coef_test(m4a_subv, vcov = V_cr4a_subv)

#####
### Спецификация 1.b subventions ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1b_subv <- lmer(
  subventions ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1b_subv <- vcovCR(m1b_subv, cluster = y2$region_name, type = "CR0")

coef_test(m1b_subv, vcov = V_cr1b_subv)

#####
### Спецификация 2.b subventions ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
#####

m2b_subv <- lmer(
  subventions ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2b_subv <- vcovCR(m2b_subv, cluster = y2$region_name, type = "CR0")

coef_test(m2b_subv, vcov = V_cr2b_subv)

#####
### Спецификация 3.b subventions ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
#####

m3b_subv <- lmer(
  subventions ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3b_subv <- vcovCR(m3b_subv, cluster = y2$region_name, type = "CR0")

coef_test(m3b_subv, vcov = V_cr3b_subv)

#####
### Спецификация 4.b subventions ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
#####

m4b_subv <- lmer(
  subventions ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4b_subv <- vcovCR(m4b_subv, cluster = y2$region_name, type = "CR0")

coef_test(m4b_subv, vcov = V_cr4b_subv)


#########################################################################################
##### Дотации на выравнивание (dotations_equalization) | ################################
##### Y2 (доля трансфертов в регионе, относительно всех федеральных трансфертов, %) #####
#########################################################################################

#####
### Спецификация 1.a dotations_equalization ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1a_eq <- lmer(
  dotations_equalization ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1a_eq <- vcovCR(m1a_eq, cluster = y2$region_name, type = "CR0")

coef_test(m1a_eq, vcov = V_cr1a_eq)

######
### Спецификация 2.a dotations_equalization ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
######

m2a_eq <- lmer(
  dotations_equalization ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2a_eq <- vcovCR(m2a_eq, cluster = y2$region_name, type = "CR0")

coef_test(m2a_eq, vcov = V_cr2a_eq)

######
### Спецификация 3.a dotations_equalization ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
######

m3a_eq <- lmer(
  dotations_equalization ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3a_eq <- vcovCR(m3a_eq, cluster = y2$region_name, type = "CR0")

coef_test(m3a_eq, vcov = V_cr3a_eq)

###### 
### Спецификация 4.a dotations_equalization ~ closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
######

m4a_eq <- lmer(
  dotations_equalization ~ factor(year) +
    closeness_centr + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_closeness_centr + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4a_eq <- vcovCR(m4a_eq, cluster = y2$region_name, type = "CR0")

coef_test(m4a_eq, vcov = V_cr4a_eq)

#####
### Спецификация 1.b dotations_equalization ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share ###
#####

m1b_eq <- lmer(
  dotations_equalization ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr1b_eq <- vcovCR(m1b_eq, cluster = y2$region_name, type = "CR0")

coef_test(m1b_eq, vcov = V_cr1b_eq)

#####
### Спецификация 2.b dotations_equalization ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout ###
#####

m2b_eq <- lmer(
  dotations_equalization ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + turnout +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_turnout +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr2b_eq <- vcovCR(m2b_eq, cluster = y2$region_name, type = "CR0")

coef_test(m2b_eq, vcov = V_cr2b_eq)

#####
### Спецификация 3.b dotations_equalization ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR ###
#####

m3b_eq <- lmer(
  dotations_equalization ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + UR +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_UR +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr3b_eq <- vcovCR(m3b_eq, cluster = y2$region_name, type = "CR0")

coef_test(m3b_eq, vcov = V_cr3b_eq)

#####
### Спецификация 4.b dotations_equalization ~ patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin ###
#####

m4b_eq <- lmer(
  dotations_equalization ~ factor(year) +
    patronaged + t1 + ln_population + ln_grp_pc_L1 + log_growth + russian_share + Putin +
    mean_patronaged + mean_t1 + mean_ln_population + mean_ln_grp_pc_L1 + mean_log_growth + mean_Putin +
    (1 | region_name),
  data = y2,
  REML = FALSE
)

V_cr4b_eq <- vcovCR(m4b_eq, cluster = y2$region_name, type = "CR0")

coef_test(m4b_eq, vcov = V_cr4b_eq)


