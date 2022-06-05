library(devtools)

install_github("kendavidn/mortDHS")

library(haven)
library(mortDHS)
library(tidyverse)
library('survival')
library("survminer")
library("GGally")


# Diccionario de variables
# https://dhsprogram.com/pubs/pdf/DHSG4/Recode7_DHS_10Sep2018_DHSG4.pdf

# Individual Women's Data - Individual Recode (IR)
# This dataset has one record for every eligible woman as defined by the household schedule.


# Leer datos
data <- read_dta("data/descomprimir/rwanda-2020.dta", n_max = 1000)


# Obtener el formato hermano-por-fila para las columnas mm1, mm2, mm4, etc
data_siblings <-
  mortDHS_reshape(data = data, sib_cols = c(1, 2, 4, 7, 8, 9, 11, 16))


# Añadir nuevas columnas
columns_to_add <- c("caseid", "v008")
data_siblings <- data_siblings %>% left_join(data[, columns_to_add])


# Separar case id en tres columnas
data_siblings$caseid <-
  gsub("( )+(?!\\d)", "", data_siblings$caseid, perl = T)
data_siblings$caseid <- str_sub(data_siblings$caseid, 2, -1)
data_siblings <-
  separate(
    data = data_siblings,
    col = caseid,
    into = c("cluster", "household", "person"),
    sep = " "
  )

# Agregar el numero de hermanos para cada registro (si una persona tiene dos hermanos los dos registros van a tener n_siblings = 2)
data_siblings <-
  data_siblings %>% group_by(cluster, household, person) %>% mutate(n_siblings = n())

# Hacer la distinción por las columnas mm1, ...
data_siblings <-
  data_siblings %>% group_by(cluster, household) %>% distinct(mm1, mm2, mm4, mm8, mm9, mm16, n_siblings, v008)



data_siblings <- data_siblings %>% rename_with(
  .col = c("mm1", "mm2", "mm4", "mm8", "mm9", "v008"),
  ~ c(
    "sex",
    "survival_status",
    "birth_cmc",
    "death_cmc",
    "pregnant_when_died",
    "interview_cmc"
  )
)

# Tiempo de muerte para los que murieron
data_siblings <- data_siblings %>%
  mutate(death_time = death_cmc - birth_cmc) %>%
  select(-(death_cmc))

# Censura
# tiempo de censura = fecha de entrevista - fecha de nacimiento
data_siblings[data_siblings$survival_status == 1, ]$death_time <-
  data_siblings %>%
  filter(survival_status == 1) %>%
  mutate(death_time = interview_cmc - birth_cmc) %>%
  select(death_time)

data_siblings[data_siblings$survival_status == 1,]$death_time <-
  data_siblings[data_siblings$survival_status == 1,]$interview_cmc -
  data_siblings[data_siblings$survival_status == 1,]$birth_cmc

# cambiar survival_status de 0 = muerto a 1 = muerto 
#data_siblings <- data_siblings[!is.na(data_siblings$death_time), ]
data_siblings$survival_status <-
  as.integer(!data_siblings$survival_status)



# born from 1960 to 1980
year <- 1960
n <- 20
l <- (year - 1900) * 12 + 0
u <- (year - 1900) * 12 + 12 * n


#condition <-
#  (data_siblings$birth_cmc < u) &  (data_siblings$birth_cmc > l)
# data_siblings <- data_siblings[condition,]

# ------------------------------
# Cox Proportions Hazard Model 
# ------------------------------

mod.siblings <-
  coxph(Surv(death_time, survival_status) ~ sex, data =
          data_siblings)


cox_fit <- survfit(mod.siblings)

ggsurvplot(
  survfit(
    mod.siblings,
    data = data_siblings,
    color = "#2E9FDF",
    ggtheme = theme_minimal()
  )
)

