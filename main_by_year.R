# If you don't want to run everything from 0 see the file
# example.R, where a pre-trained model is used

# Read data
source("functions/read_dhs.R")


# the library "mortDHS" and "rstanarm" are required and they need to be downloaded as follow

#library(devtools)
#install_github("kendavidn/mortDHS") 
#install_github("stan-dev/rstanarm", ref = "feature/survival", build_vignettes = FALSE) 
library(mortDHS)
library(rstanarm)

library(haven)
library(tidyverse)
library(bayesplot)
library(cowplot)

# Here we read the datasets we want to compare
# In this example we only used 5
# These files are stored in the "data" folder
# For terms of computation only the first 2000 siblings 
# were used, this can be changed.

# 5 dataframes are loaded inside "full_data.RData" so 

load("data/countries_data.RData")

# is used instead of ...
# data_zambia <- read_dhs_surv("data/zambia_2018.dta")
# data_gambia <- read_dhs_surv("data/gambia_2019-20.dta")
# data_rwanda <- read_dhs_surv("data/rwanda-2019-20.dta")
# data_benin <- read_dhs_surv("data/benin-2017-18.dta")
# data_sierra_leone <- read_dhs_surv("data/sierra-leone-2019.dta")
# data_mali <- read_dhs_surv("data/mali-2018.dta")
# data_liberia <- read_dhs_surv("data/liberia-2019-20.dta")

set.seed(1000)
n_sample <- 2500
{
data_zambia <- data_zambia[sample(1:(nrow(data_zambia)), n_sample), ]
data_gambia <- data_gambia[sample(1:(nrow(data_gambia)), n_sample), ]
data_rwanda <- data_rwanda[sample(1:(nrow(data_rwanda)), n_sample), ]
data_benin <- data_benin[sample(1:(nrow(data_benin)), n_sample), ]
data_sierra_leone <- data_sierra_leone[sample(1:(nrow(data_sierra_leone)), n_sample), ]
data_mali <- data_mali[sample(1:(nrow(data_mali)), n_sample), ]
data_liberia <- data_liberia[sample(1:(nrow(data_liberia)), n_sample), ]
}
# censorship and filtering by selected year

# The column "country" is created to identify each row before merge
{
data_zambia$country <- "zmb" #zambia 
data_gambia$country <- "gmb" #gambia 
data_rwanda$country <- "rwa" #rwanda
data_benin$country <- "ben" #benin
data_sierra_leone$country <- "sle" #sierra leone
data_mali$country <- "mli" #mali
data_liberia$country <- "lbr" #liberia
}

# The dataframes are merged into a single one (data_siblings)
{
data_siblings <- bind_rows(data_zambia, data_gambia)
data_siblings <- bind_rows(data_siblings, data_rwanda)
data_siblings <- bind_rows(data_siblings, data_benin)
data_siblings <- bind_rows(data_siblings, data_sierra_leone)
data_siblings <- bind_rows(data_siblings, data_mali)
data_siblings <- bind_rows(data_siblings, data_liberia)
}

rm(data_zambia, data_gambia, data_rwanda, data_benin, data_sierra_leone, data_mali, data_liberia)

# Convert category column to factors
data_siblings$sex <- data_siblings$sex %>% as.factor()
data_siblings$country <- data_siblings$country %>% as.factor()

# cortar donde la persona no ha muerto para que el maximo sea el año a filtrar
#data_siblings <- data_siblings %>% mutate(death_time = case_when(
#  survival_status == 0 ~ date_to_cmc(year = year_filter, month = 12) - birth_cmc,
#  TRUE ~ death_time
#))

# --Models--

# knot position can be changed to try to have the best fit possible
# the package places two knots by default 
# a value close to 0 means that knot is not useful 
# (the survival function is almost flat at that point)

#knotlist <- quantile(data_siblings$death_time, seq(0.05, 0.95, length.out = 4), na.rm = TRUE)
#knotlist <- as.vector(knotlist)

mod_spline <-
  stan_surv(
    formula = Surv(death_time, survival_status) ~ sex + country,
    data = data_siblings,
    basehaz = "ms" ,
    basehaz_ops = list(degree = 3, knots = 4),
    iter = 2000
  )

#
model_splines_2015 <- mod_spline

# This is used to print the trained model coefficients
summary(mod_spline, digits = 5)

# This is used to compare models
loo_compare(loo(mod_spline), loo(mod_exp), loo(mod_weibull))

# This is used to compare survival functions for each group
# M-Spline Model
p1=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="1", country = "rw")))
p2=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="2", country = "rw")))
p3=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="1", country = "se")))
p4=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="2", country = "se")))
p5=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="1", country = "ma")))
p6=plot(posterior_survfit(mod_spline, newdata=data.frame(sex="2", country = "ma")))

plot_grid(p1,p2,
          p3, p4, 
          p5, p6,
          ncol = 2)

# This is used to compare survival functions for each group
# Exponential Model
p1=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="1", country = "rw")))
p2=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="2", country = "rw")))
p3=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="1", country = "se")))
p4=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="2", country = "se")))
p5=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="1", country = "ma")))
p6=plot(posterior_survfit(mod_exp, newdata=data.frame(sex="2", country = "ma")))

plot_grid(p1,p2,
          p3, p4, 
          p5, p6,
          ncol = 2)


# This is used to compare survival functions for each group
# Weibull Model
p1=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="1", country = "rw")))
p2=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="2", country = "rw")))
p3=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="1", country = "se")))
p4=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="2", country = "se")))
p5=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="1", country = "ma")))
p6=plot(posterior_survfit(mod_weibull, newdata=data.frame(sex="2", country = "ma")))

plot_grid(p1,p2,
          p3, p4, 
          p5, p6,
          ncol = 2)

# Set custom color to charts
color_scheme_set("red")

# Plot the hazard base
plot(mod_spline, plotfun = "basehaz")

# Print the prior summary
prior_summary(mod_spline) 

# Probability areas given to splines
mcmc_areas(mod_spline, regex_pars = "m-sp*", prob = 0.90, prob_outer = 0.95)

# Probability areas 
mcmc_areas(mod_spline, regex_pars = "(Intercept)|country|sex*", prob = 0.90, prob_outer = 0.95)

# This is used to compare mortalities between
nd1 <- data.frame(sex = "1", country = c("rw", "ma", "se"))
nd2 <- data.frame(sex = "2", country = c("rw", "ma", "se"))

posterior_survfit(
  mod_spline,
  newdata = nd1,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

posterior_survfit(
  mod_spline,
  newdata = nd2,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf2

pf1 %>% filter(time < 810 & time > 800)

plot(pf1)


nd <- data.frame(sex = c("1", "2"), age_group = "mayor")
posterior_survfit(
  mod_spline,
  newdata = nd,
  times = 0,
  last_time = 100,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

plot(pf1)


# The next plots are used to evaluate MCMC convergence  

# Rhat
plot(mod_spline, "rhat")

# Autocorrelation chart for all coefficients except spline ones
plot(mod_spline, "acf", pars = "(Intercept)", regex_pars = "sex*|country*", ylim = c(0, 0.1))

# Autocorrelation chart for all splines coefficients
plot(mod_spline, "acf", pars = "(Intercept)", regex_pars = "m-spl*")

# Traceplot
plot(mod_spline, "trace")

# KM vs our model
ps_check(mod_spline)

## Model comparision
loo(mod_spline)
loo(mod_weibull)
loo(mod_exp)
waic(mod_spline)
waic(mod_weibull)
waic(mod_exp)

compare_models(loo(mod_spline),
               loo(mod_weibull),
               loo(mod_exp))
