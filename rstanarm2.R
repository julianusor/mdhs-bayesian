# Leer datos
source("functions/read_dhs.R")
data_siblings <- read_dhs("data/descomprimir/rwanda-2020.dta", 700)

# death_time y quitar death_cmc
# Tiempo de muerte para los que murieron
data_siblings <- data_siblings %>%
  mutate(death_time = death_cmc - birth_cmc) %>%
  select(-(death_cmc))

# agregar death_time pero para censura
# Tiempo de censura = fecha de entrevista - fecha de nacimiento
# Si survival_status == 1 significa no muerto (indicador de censura)
data_siblings[data_siblings$survival_status == 1, ]$death_time <-
  data_siblings %>%
  filter(survival_status == 1) %>%
  mutate(death_time = interview_cmc - birth_cmc) %>%
  select(death_time)


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

bayesplot::mcmc_areas_ridges(mod1, regex_pars = "*", prob = 0.90)

mod1 <- stan_surv(formula = Surv(death_time, survival_status) ~ sex,
                  data = data_siblings, basehaz="ms")

mod1 <-
  stan_surv(
    formula = Surv(death_time, survival_status) ~ sex + age_group,
    data = data_siblings,
    basehaz = "exp"
  )

bayesplot::color_scheme_set("red")

# rhat
plot(mod1, "rhat")

#_________

x <- as.array(mod1, regex_pars = "factor*")
bayesplot::mcmc_areas(x, prob = 0.5, prob_outer = 0.9)

x <- as.array(mod1, regex_pars = "sex*|(*Intercept*)")
bayesplot::mcmc_areas(x, prob = 0.9, prob_outer = 0.95)

plot(mod1, "acf", pars = "sex2", regex_pars = "age*")


plot(mod1, "trace", pars = c("(Intercept)", "sex2", "age_group2", "age_group3"),
     facet_args = list(nrow = 2))

# Para la presentación
# a prioris y modelo bien definidos
# mostrar la base de datos y su formato
# tablas en latex 
# explicar la supervivencia

