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
data <- read_dta("data/rwanda-2020.dta", n_max = 1000)


# Obtener el formato hermano-por-fila para las columnas mm1, mm2, mm4, etc
data_siblings <-
  mortDHS_reshape(data = data, sib_cols = c(1, 2, 4, 7, 8, 9, 11, 16))


# Añadir nuevas columnas
columns_to_add <- c("caseid", "v008")
data_siblings <- data_siblings %>% left_join(data[, columns_to_add])


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



####### pruebitas ########


plot(survfit(mod.siblings, newdata=data.frame(sex=1)),
     xlab = "meses", ylab="Survival",col=1,conf.int=F) 
lines(survfit(mod.siblings, newdata=data.frame(sex=2)),
      xlab = "meses", ylab="Survival",col=2, conf.int=F)


temp <- cox.zph(mod.siblings,transform="km")
print(temp)
win.graph()
plot(temp)



######### COXPH VS ICBAYES - EJEMPLO #######

library(coda)
library(HI) # install.packages("HI")
library(magrittr)
library(survival)
require(ICBayes) # install.packages("ICBayes")
require(tidyverse)

data("bcdata")
base<-as.data.frame(bcdata)
head(bcdata)

## EJEMPLO CANCER DE SENO ## 

try1 <- ICBayes(formula=Surv(L, R, type="interval2") ~ x1,
                data=base, model="case2ph", status=base$status,
                order=4, coef_range=2, x_user=c(0,1), niter=11000,
                burnin=1000, knots=seq(0.1, 60.1, length=10),
                grids=seq(0.1, 60.1, by=1), seed=12242016)
summary(try1)


## COMO COX SOLO TOMA CENSURA A DERECHA SE USAN SOLO LOS DATOS CENSURADOS A DERECHA
## DE LA BASE DE CANCER DE SENO PARA COMPARAR LA ESTIMACION CON COX 


base2 <- base %>% filter( is.na(R))


try2 <- ICBayes(formula=Surv(L, R, type="interval2") ~ x1,
                data=base2, model="case2ph", status=base2$status,
                order=4, coef_range=2, x_user=c(0,1), niter=11000,
                burnin=1000, knots=seq(0.1, 60.1, length=10),
                grids=seq(0.1, 60.1, by=1), seed=12242016)
summary(try2)


fit <- coxph(Surv(L, status) ~ x1, data = base) 
fit


log(0.7243237)

## SE OBTIENEN VALORES PARECIDOS -1.13 Y -1.28  para x1 ##


# --------------------------------------------------------
## hazard promedio

### Hombre - sex = 1

### Mujer - sex = 2

## tiempo promedio
## boxplot por género
## edades
## causas de muerte