library(usethis)
library(devtools)      
library(tidyverse)
devtools::install_github("stan-dev/rstanarm", ref = "feature/survival", build_vignettes = FALSE)
library(Rcpp)
library(rstanarm)
library(tidybayes)
library(bayesplot)
library(survival)



source("functions/read_dhs.R")

data_siblings <- read_dhs("data/rwanda-2020.dta", 700)

# Tiempo de muerte para los que murieron
data_siblings <- data_siblings %>%
  mutate(death_time = death_cmc - birth_cmc) %>%
  select(-(death_cmc))

# Tiempo de censura = fecha de entrevista - fecha de nacimiento
# Si survival_status == 1 significa no muerto (censura)
data_siblings[data_siblings$survival_status == 1, ]$death_time <-
  data_siblings %>%
  filter(survival_status == 1) %>%
  mutate(death_time = interview_cmc - birth_cmc) %>%
  select(death_time)

data_siblings[data_siblings$survival_status == 1,]$death_time <-
  data_siblings[data_siblings$survival_status == 1,]$interview_cmc -
  data_siblings[data_siblings$survival_status == 1,]$birth_cmc



mod1 <- stan_surv(formula = Surv(death_time, survival_status) ~ sex,
                  data = data_siblings, basehaz="ms")

summary(mod1)
prior_summary(mod1)
