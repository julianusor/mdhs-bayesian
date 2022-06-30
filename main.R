# Leer datos
source("functions/read_dhs.R")
library(mortDHS)
library(haven)
library(tidyverse)
library(Rcpp)
library(rstanarm)
library(bayesplot)
library(cowplot)

# Here we read the datasets we want to compare
# In this example we only used 3
# These files are stored in the "data" folder
# For terms of computation only the first 400 siblings 
# were used

data_1 <- read_dhs_surv("data/rwanda-2020.dta", n_max = 400)
data_2 <- read_dhs_surv("data/malawi-2015-16.dta", n_max = 400)
data_3 <- read_dhs_surv("data/senegal-2017.dta", n_max = 400)

# A column of countries name is created to identify 
# data before merge

data_1$country <- "rw"
data_2$country <- "ma"
data_3$country <- "se"

# The three dataframes are merged into a 
# single one (data_siblings)

data_siblings <- bind_rows(data_1, data_2)
data_siblings <- bind_rows(data_siblings, data_3)
data_siblings <- bind_rows(data_siblings, data_4)
rm(data_1, data_2, data_3)

# FILTER START --OPTIONAL--

# In this example: people born from year 1960 to 1980
year <- 1960
n <- 20 # Number of years
start <- (year - 1900) * 12 + 0
finish <- (year - 1900) * 12 + 12 * n

condition <-
  (data_siblings$birth_cmc < finish) &
  (data_siblings$birth_cmc > start)
data_siblings <- data_siblings[condition, ]

data_siblings$sex <- data_siblings$sex %>% as.factor()
data_siblings$country <- data_siblings$country %>% as.factor()

# FILTER END --OPTIONAL--

# "0" survival times to are transformed to 0.1
data_siblings <- data_siblings %>%
  mutate(death_time = if_else(death_time == 0, 0.1, death_time))

# --Models--

knotlist <- quantile(data_siblings$death_time, seq(0.05, 0.95, length.out = 4), na.rm = TRUE)
knotlist <- as.vector(knotlist)

mod_spline <-
  stan_surv(
    formula = Surv(death_time, survival_status) ~ sex + country,
    data = data_siblings,
    basehaz = "ms" ,
    basehaz_ops = list(degree = 3, knots = knotlist),
    iter = 2000
  )

mod_exp <-
  stan_surv(
    formula = Surv(death_time, survival_status) ~ sex + country,
    data = data_siblings,
    basehaz = "exp"
  )

mod_weibull <-
  stan_surv(
    formula = Surv(death_time, survival_status) ~ sex + country,
    data = data_siblings,
    basehaz = "weibull"
  )
# This is used to see the trained model coefficients
summary(mod_spline, digits = 5)

# This is used to compare models
loo_compare(loo(mod_spline), loo(mod_exp), loo(mod_weibull))

# This is used to compare survival functions for each group
# M-Spline Model
p1=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="1", country = "rw")), main= "rwandan men")
p2=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="2", country = "rw")), main = "rwandan women")
p3=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="1", country = "se")), main = "Senegalese men")
p4=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="2", country = "se")), main = "Senegalese women")
p5=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="1", country = "ma")), main= "Malawian men")
p6=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="2", country = "ma")), main= "Malawian women")


plot_grid(p1,
          p2,
          ncol = 2)


plot_grid(p3,
          p4,
          ncol = 2)


plot_grid(p5,
          p6,
          ncol = 2)

# This is used to compare survival functions for each group
# Exponential Model
p1=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="1", country = "rw")))
p2=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="2", country = "rw")))
p3=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="1", country = "se")))
p4=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="2", country = "se")))
p5=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="1", country = "ma")))
p6=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="2", country = "ma")))

plot_grid(p1,
          p2,
          p3,
          p4,
          p5,
          p6,
          p7, 
          p8,
          ncol = 2)


# This is used to compare survival functions for each group
# Weibull Model
p1=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="1", country = "rw")))
p2=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="2", country = "rw")))
p3=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="1", country = "se")))
p4=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="2", country = "se")))
p5=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="1", country = "ma")))
p6=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="2", country = "ma")))

plot_grid(p1,
          p2,
          p3,
          p4,
          p5,
          p6,
          ncol = 2)

color_scheme_set("red")

plot(mod1, plotfun = "basehaz")

prior_summary(mod1) #-> prioris

# area para coeficientes de splines
mcmc_areas(mod1, regex_pars = "m-sp*", prob = 0.90, prob_outer = 0.95)

# area para todos
mcmc_areas(mod1, regex_pars = "(Intercept)|country*", prob = 0.90, prob_outer = 0.95)
# como sex2 tiene un valor negativo pero cerca de 0 significa que las mujeres 
# tienen una mortalidad mayor (la curva decrece mas rapido)
# pero el grafico de abajo me contradice

# pag 18
#"pag 24"

nd1 <- data.frame(sex = "1", country = c("rw", "ma", "se", "co"))
nd2 <- data.frame(sex = "2", country = c("rw", "ma", "se", "co"))

posterior_survfit(
  mod1,
  newdata = nd1,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

posterior_survfit(
  mod1,
  newdata = nd2,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf2

pf1 %>% filter(time < 810 & time > 800)

plot(pf1)


nd <- data.frame(sex = c("1", "2"), age_group = "mayor")
posterior_survfit(
  mod1,
  newdata = nd,
  times = 0,
  last_time = 100,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

plot(pf1)

posterior_survfit(
  mod1,
  newdata = nd,
  condition =  FALSE,
  extrapolate = FALSE,
  times = 200,
  prob = 0.95
)

# rhat
plot(mod1, "rhat")

#_________

# Autocorrelation para todos menos m-spline
plot(mod1, "acf", pars = "(Intercept)", regex_pars = "sex*", ylim = c(0, 0.1))

# Autocorrelation para todos m-spline
plot(mod1, "acf", pars = "(Intercept)", regex_pars = "m-spl*")

# Traceplot para los 4
plot(mod1, "trace")

##
ps_check(mod_spline)

##Comparación de modelos 
loo(mod1)
loo(mod2)
loo(mod3)
waic(mod1)
waic(mod2)
waic(mod3)


compare_models(loo(mod1),
               loo(mod2))




p1=plot(posterior_survfit(mod1, newdata=data.frame(sex="1", age_group="menor", country = "rw")))
p2=plot(posterior_survfit(mod1, newdata=data.frame(sex="1", age_group="menor", country = "ml")))
p3=plot(posterior_survfit(mod1, newdata=data.frame(sex="2", age_group="menor", country = "rw")))
p4=plot(posterior_survfit(mod1, newdata=data.frame(sex="2", age_group="menor", country = "ml")))

p5=plot(posterior_survfit(mod1, newdata=data.frame(sex="1", age_group="mayor", country = "rw")))
p6=plot(posterior_survfit(mod1, newdata=data.frame(sex="1", age_group="mayor", country = "ml")))
p7=plot(posterior_survfit(mod1, newdata=data.frame(sex="2", age_group="mayor", country = "rw")))
p8=plot(posterior_survfit(mod1, newdata=data.frame(sex="2", age_group="mayor", country = "ml")))

library(cowplot)
p_combined2 <- plot_grid(p1,
                        p2,
                        p3,
                        p4,
                        p5,
                        p6,
                        p7,
                        p8,
                        ncol = 2)
p_combined2




ps2 <- posterior_survfit(mod_spline, type="surv", standardise = FALSE, times = 0,
                         control = list(epoints = 20))
plot(ps2)

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
res.cox <- coxph(Surv(death_time, survival_status) ~ sex + country, data =  data_siblings)


summary(res.cox)




