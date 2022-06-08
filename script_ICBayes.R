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

# L
data_siblings <- data_siblings %>%  mutate(L = ifelse(survival_status == 1, death_time, 0)) 

data_siblings <- data_siblings %>%  mutate(status = ifelse(survival_status == 1, 2, 0)) 

# R
data_siblings <- data_siblings %>%  mutate(R = ifelse(survival_status == 1, NA, death_time)) 

#max(data_siblings$death_time, na.rm = TRUE)
#min(data_siblings$death_time, na.rm = TRUE)

try1 <- ICBayes(formula=Surv(L, R, type="interval2") ~ sex,
                data=data_siblings, model="case2ph", status=data_siblings$status,
                order=4, coef_range=2, x_user=c(0,1), niter=11000,
                burnin=1000, knots=seq(3, 834, length=15),
                grids=seq(3, 834, by=1), seed=12242016)
summary(try1)
