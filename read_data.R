install_github("kendavidn/mortDHS")

library(devtools)
library(haven)
library(mortDHS)

library(tidyverse)

# Leer datos
#malawi bien 2015-16
#rwanda si 2020
#continous dhs No
# senegal bien 2017
data <- read_dta("C:/Users/Usuario/Documents/GitHub/FinalProjectBayesian/data/rwanda-2020.dta", n_max = 1000)

# Obtener el formato hermano-por-fila para las columnas mm1, mm2, mm4, etc
data_siblings <- mortDHS_reshape(data = data, sib_cols = c(1,2,4,5,7,8,9,11))


# Separar case id en tres columnas
data_siblings$caseid <-
  gsub("( )+(?!\\d)", "", data_siblings$caseid, perl = T)
data_siblings$caseid <- str_sub(data_siblings$caseid, 2,-1)
data_siblings <-
  separate(
    data = data_siblings,
    col = caseid,
    into = c("cluster", "household", "person"),
    sep = " "
  )

# Agregar el numero de hermanos para cada registro (si una persona tiene dos hermanos los dos registros van a tener n_siblings = 2)
data_siblings <- data_siblings %>% group_by(cluster, household, person) %>% mutate(n_siblings = n())

# Hacer la distinción por las columnas mm1, ...
data_siblings <- data_siblings %>% group_by(cluster, household) %>% distinct(mm1, mm2, mm4, mm5, mm8, mm9, n_siblings)


data_siblings <- data_siblings %>% rename_with(
  .col = c("mm1", "mm2", "mm4", "mm8", "mm9"),
  ~ c(
    "sex",
    "survival_status",
    "birth_cmc",
    "death_cmc",
    "pregnant_when_died"
  )
)


data_siblings <- data_siblings %>%
  mutate(death_time = death_cmc - birth_cmc) %>% 
  select(-(death_cmc))


library('survival')
surv_object <- Surv(time = data_siblings$death_time , event = data_siblings$survival_status == 0)
surv_fit <- survfit(surv_object~1)

# https://injepijournal.biomedcentral.com/track/pdf/10.1186/s40621-018-0174-7.pdf
library(ggplot2)
library(GGally)
mortDHS_obs_time(data = data_siblings)
ggsurv(surv_fit,
       main = "Survival curve for Rwandans born in 1980", plot.cens = FALSE)
