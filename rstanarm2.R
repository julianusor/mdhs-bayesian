# Leer datos
source("functions/read_dhs.R")
library(mortDHS)
library(haven)
library(tidyverse)
library(rstanarm)

data_siblings <- read_dhs("data/descomprimir/rwanda-2020.dta", 400)

# Los datos vienen en el formato 
# 0 = hermano muerto
# 1 = hermano vivo

# death_time y quitar death_cmc
# Tiempo de muerte para los que murieron
data_siblings <- data_siblings %>%
  mutate(death_time = death_cmc - birth_cmc) #%>%
  #select(-(death_cmc))

# agregar death_time pero para censura
# Tiempo de censura = fecha de entrevista - fecha de nacimiento
# Si survival_status == 1 significa no muerto (indicador de censura)
data_siblings[data_siblings$survival_status == 1,]$death_time <-
  data_siblings[data_siblings$survival_status == 1,]$interview_cmc -
  data_siblings[data_siblings$survival_status == 1,]$birth_cmc

# ver 0

data_siblings <- data_siblings %>%
  mutate(death_time = if_else(death_time == 0, 0.1, death_time))


# 1 = child (0-14 yrs)
# 2 = youth (15-24 yrs)
# 3 = adults (25-64 yrs)
# 4 = seniors (65+ years)

data_siblings <- data_siblings %>%  
  mutate(age_group = ifelse(death_time < 14*12, 1, 
                            ifelse(death_time < 24*12, 2, 
                                   ifelse(death_time < 64*12, 3, 4)))) 

data_siblings$age_group <- data_siblings$age_group %>% as.factor()

data_siblings$sex <- data_siblings$sex %>% as.factor()

# cambiar survival_status de 0 = muerto a 1 = muerto 
data_siblings$survival_status <-
  as.integer(!data_siblings$survival_status)

# =========== modelo

mod1 <- stan_surv(formula = Surv(death_time, survival_status) ~ -1 + sex + age_group,
                  data = data_siblings, basehaz="ms") #, basehaz_ops = list(degree = 3, knots = seq(0.1,700, length.out = 10)))

mod2 <- stan_surv(formula = Surv(death_time, survival_status) ~ -1 + sex + age_group,
                  data = data_siblings, basehaz="exp")

mod3 <- stan_surv(formula = Surv(death_time, survival_status) ~ -1 + sex + age_group,
                  data = data_siblings, basehaz="weibull")

# basehaz_ops = list(degree = 2, knots = c(10,20))
bayesplot::color_scheme_set("red")

# area para coeficientes de splines
bayesplot::mcmc_areas(mod1, regex_pars = "m-spl*", prob = 0.95)

# area para coeficientes de splines
bayesplot::mcmc_areas(mod1, regex_pars = "age*|*Intercept*", prob = 0.95)

# area para coeficientes de splines
bayesplot::mcmc_areas(mod1, regex_pars = "sex*", prob = 0.95)


# rhat
plot(mod1, "rhat")

#_________

# Autocorrelation para todos menos m-spline
plot(mod1, "acf", pars = "(Intercept)", regex_pars = "age*|sex*")

# Traceplot para los 4
plot(mod1, "trace")

##
ps_check(mod1)


# Para la presentación
# a prioris y modelo bien definidos
# mostrar la base de datos y su formato
# tablas en latex 
# explicar la supervivencia


### coxph
library("survival")
library("survminer")

# para la funcion Surv 
# survival status = 0 vivo
# survival status = 1 muerto
res.cox <-
  coxph(Surv(death_time, survival_status) ~ -1 + sex + age_group, data =  data_siblings)

summary(res.cox)

