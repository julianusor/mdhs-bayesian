# This file is used to demonstrate the package and see the 
# spline model properties and results used a pre-trained model

# load the model
# this model was filtered and contains only people born from
# year 1960 to 1980, as seen in the main.R script
load("data/spline-model.RData")

# This is used to print the trained model coefficients
summary(mod_spline, digits = 5)

# This is used to compare survival functions for each group
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

# Kaplan-Meier vs Spline Model
# here we see how far is our approximation (Splines model) 
# from the non-parametric (KM) model
ps_check(mod_spline)

# Areas of coefficients
# we can see that sex2 has a negative coefficient, that
# women have less mortality than men
# and we can see too that Senegalese people have less mortality
# than people from Malawi (the intercept).
# people from Rwanda have a higher mortality but because it 
# contains 0 that means that there is not big of a difference
# between these two countries
mcmc_areas(mod_spline, regex_pars = "(Intercept)|country*|sex*", prob = 0.90, prob_outer = 0.95)


# Compare mortalities using the posterior
nd1 <- data.frame(sex = "1", country = c("rw", "ma", "se"))
posterior_survfit(
  mod_spline,
  newdata = nd1,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

pf1 %>% filter(time < 10*12 & time > 9.5*12)
# id cond_time     time median  ci_lb  ci_ub
# 1        NA 115.3939 0.8757 0.8584 0.8924
# 2        NA 115.3939 0.8821 0.8666 0.8962
# 3        NA 115.3939 0.9240 0.9119 0.9344

# this means that for kids of around 9 and a half years from
# Rwanda the survival lays inside the 
# interval [0.858, 0.892] with a probability of 0.95
# for kids from Malawi it is [0.866 0.896]
# and for kids from Senegal it is [0.911 0.934]
# As we can see the Rwanda and Malawi intervals are very common
# as we suspected early with the coefficient areas
# the Senegal kids have the lowest mortality

# Compare woman from Senegal with man from Rwanda with ages 18

nd1 <- data.frame(sex = "1", country = "rw")
posterior_survfit(
  mod_spline,
  newdata = nd1,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf1

nd2 <- data.frame(sex = "2", country = "se")
posterior_survfit(
  mod_spline,
  newdata = nd2,
  times = 0,
  prob = 0.95,
  extrapolate = TRUE
) -> pf2


pf1 %>% filter(time < 18*12 & time > 17*12)

pf2 %>% filter(time < 18*12 & time > 17*12)

# We can see that 
# women from Senegal have a survival of [0.898 0.923] 
# men from Rwanda have a survival of  [0.810 0.851]
# We can see a big difference between these two groups, their
# intervals have a 0.95 probability