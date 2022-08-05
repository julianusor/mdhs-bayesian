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

load("data/full_data.RData")

# is used instead of ...
#data_1 <- read_dhs_surv("data/rwanda-2019-20.dta")
#data_2 <- read_dhs_surv("data/benin-2017-18.dta")
#data_3 <- read_dhs_surv("data/sierra-leone-2019.dta")
#data_4 <- read_dhs_surv("data/mali-2018.dta")
#data_5 <- read_dhs_surv("data/liberia-2019-20.dta")

year_filter <- 2015
set.seed(year_filter)
data_1 <- data_1[sample(1:(nrow(data_1)), 2000), ]
data_2 <- data_2[sample(1:(nrow(data_2)), 2000), ]
data_3 <- data_3[sample(1:(nrow(data_3)), 2000), ]
data_4 <- data_4[sample(1:(nrow(data_4)), 2000), ]
data_5 <- data_5[sample(1:(nrow(data_5)), 2000), ]

# censorship and filtering by selected year

data_1 <- data_filter_year_surv(data_1, year = year_filter)
data_2 <- data_filter_year_surv(data_2, year = year_filter)
data_3 <- data_filter_year_surv(data_3, year = year_filter)
data_4 <- data_filter_year_surv(data_4, year = year_filter)
data_5 <- data_filter_year_surv(data_5, year = year_filter)

# The column "country" is created to identify each row before merge

data_1$country <- "rwa" # this is the order of data_x dataframes
data_2$country <- "ben"
data_3$country <- "sle"
data_4$country <- "mli"
data_5$country <- "lbr"

# The dataframes are merged into a single one (data_siblings)

data_siblings <- bind_rows(data_1, data_2)
data_siblings <- bind_rows(data_siblings, data_3)
data_siblings <- bind_rows(data_siblings, data_4)
data_siblings <- bind_rows(data_siblings, data_5)

rm(data_1, data_2, data_3, data_4, data_5)

# Convert category column to factors
data_siblings$sex <- data_siblings$sex %>% as.factor()
data_siblings$country <- data_siblings$country %>% as.factor()


data_siblings <- data_siblings %>% mutate(death_time = case_when(
  survival_status == 0 ~ date_to_cmc(year = year_filter, month = 12) - birth_cmc,
  TRUE ~ death_time
))

# --Models--

# knot position can be changed to try to have the best fit possible
# the package places two knots by default 
# a value close to 0 means that knot is not useful 
# (the survival function is almost flat at that point)

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
