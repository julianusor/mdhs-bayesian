library(usethis)
library(devtools)      
library(tidyverse)
#devtools::install_github("stan-dev/rstanarm", ref = "feature/survival", build_vignettes = FALSE)
library(Rcpp)
library(rstanarm)
library(tidybayes)
library(bayesplot)
library(survival)



source("functions/read_dhs.R")

data_siblings <- read_dhs("data/rwanda-2020.dta", 700)
data_siblings <- read_dhs("data/descomprimir/rwanda-2020.dta", 700)
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


data_siblings$edad= round(data_siblings$death_time/12,0) 
data_siblings %>% summary
base= data_siblings %>% select(edad, sex, death_time, survival_status) 
base %>% summary 

data_siblings$edad= round(data_siblings$death_time/12,0) 

base=na.omit(base)

base$factor_Edad= cut(base$edad, breaks = c(-0.01, 14, 24, 64, Inf),
    labels = c("Niño", "Joven", "Adulto", "Mayor"))

base %>% summary


mod1 <- stan_surv(formula = Surv(death_time, survival_status) ~ sex + factor_Edad,
                  data = base, basehaz="ms")

summary(mod1)
prior_summary(mod1)



mod2 <- stan_surv(formula = Surv(death_time, survival_status) ~ as.factor(sex) + factor_Edad,
                  data = base, basehaz="exp")

summary(mod2)


mod3 <- stan_surv(formula = Surv(death_time, survival_status) ~ as.factor(sex) + factor_Edad,
                  data = base, basehaz="weibull")

summary(mod3)


plotfun <- function(model, title) {
    plot(model, plotfun = "basehaz") +
    coord_cartesian(ylim = c(0,0.4)) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5))
  }

p_exp <- plotfun(mod1, "ms")
p_exp


bayesplot::color_scheme_set("pink")
(trace <- plot(mod1, "trace"))


# Boxplot por grupo de edad
ggplot(base, aes(x=factor_Edad, y=death_time, fill=factor_Edad)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red") +
  theme(legend.position="right") +
  scale_fill_brewer(palette="Set1") +
  scale_fill_discrete(labels=c("Niño","Joven","Adulto","Mayor"))
