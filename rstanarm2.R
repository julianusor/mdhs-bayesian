# Leer datos
source("functions/read_dhs.R")
library(mortDHS)
library(haven)
library(tidyverse)
library(rstanarm)

data_siblings <- read_dhs_surv("data/descomprimir/rwanda-2020.dta", n_max = 100)

# 71901 rows
#data_siblings <- read_dhs_surv("data/descomprimir/rwanda-2020.dta")

# Los datos vienen en el formato 
# 0 = hermano muerto
# 1 = hermano vivo
# al usar la function queda alreves
# 0 = vivo (censurado)
# 1 = muerto

# formato
#1       male
#2     female


# filter : 1980 a 1982

group_year <- 1900
group_time <- 200
l <- (group_year - 1900) * 12 + 0
u <- (group_year - 1900) * 12 + 12 * group_time


data_siblings <- data_siblings %>% filter((birth_cmc < u) & (birth_cmc > l))


data_siblings$sex <- data_siblings$sex %>% as.factor()



# =========== modelo
qlist <- quantile(data_siblings$death_time, seq(0.05, 0.95, length.out = 6), na.rm = TRUE)
(qlist)
qlist <- as.vector(qlist)
(qlist)
mod1 <-
  stan_surv(
    formula = Surv(death_time, survival_status) ~ sex,
    data = data_siblings,
    basehaz = "ms" ,
    basehaz_ops = list(degree = 3, knots = qlist)
  )

# estos modelos no son tan suaves comparados al modelo no parametrico

mod2 <- stan_surv(formula = Surv(death_time, survival_status) ~ -1 + sex + age_group,
                  data = data_siblings, basehaz="exp")

mod3 <- stan_surv(formula = Surv(death_time, survival_status) ~ -1 + sex + age_group,
                  data = data_siblings, basdehaz="weibull")


#basehaz_ops = list(degree = 3, knots = c(10,20))

plot(mod1, plotfun = "basehaz")
plot_grid(mod1,  ncol = 1)
# muy util 
print(mod1, digits = 4)
prior_summary(mod1) #-> prioris
# basehaz_ops = list(degree = 2, knots = c(10,20))
bayesplot::color_scheme_set("red")

# area para coeficientes de splines
bayesplot::mcmc_areas(mod1, regex_pars = "m-sp*", prob = 0.95)

# area para coeficientes de splines
bayesplot::mcmc_areas(mod1, regex_pars = "*Intercept*|sex*", prob = 0.95)
# como sex2 tiene un valor negativo pero cerca de 0 significa que las mujeres 
# tienen una mortalidad mayor (la curva decrece mas rapido)
# pero el grafico de abajo me contradice

# pag 18
nd <- data.frame(sex = c("1", "2"))
#"pag 24"
posterior_survfit(
  mod1,
  newdata = nd,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

plot(pf1)
# rhat
plot(mod1, "rhat")

#_________

# Autocorrelation para todos menos m-spline
plot(mod1, "acf", pars = "(Intercept)", regex_pars = "sex*")

# Autocorrelation para todos m-spline
plot(mod1, "acf", pars = "(Intercept)", regex_pars = "m-spl*")

# Traceplot para los 4
plot(mod1, "trace")

##
ps_check(mod1)


ps2 <- posterior_survfit(mod1, type="surv", standardise = FALSE, times = 0,
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
res.cox <-
  coxph(Surv(death_time, survival_status) ~ sex , data =  data_siblings)

summary(res.cox)

