library(brms)
library(bayestestR)

COVID_mental_health <- readRDS("COVID_mental_health.RDS")

COVID_mental_health$employed <- factor(COVID_mental_health$employed, levels = c("No", "Yes"))
contrasts(COVID_mental_health$employed) <- contr.orthonorm
contrasts(COVID_mental_health$gender) <- contr.orthonorm
COVID_mental_health$going_out_c <- scale(COVID_mental_health$going_out, center = TRUE, scale = FALSE)
COVID_mental_health$age_dec_c <- scale(COVID_mental_health$age, center = TRUE, scale = FALSE) / 10

saveRDS(COVID_mental_health, "COVID_mental_health.RDS")

get_prior(more_depressed ~ age_dec_c + going_out_c + gender + employed,
          data = COVID_mental_health,
          family = bernoulli())

my_priors <- 
  set_prior("normal(0,4)", class = "Intercept") + 
  # Covariables of interest 
  set_prior("student_t(3, 0, 0.1)", class = "b", coef = "going_out_c") + 
  set_prior("student_t(3, 0, 0.4)", class = "b", coef = "employed1") + 
  # Nuisance covariables
  set_prior("student_t(3, 0, 2)", class = "b", coef = "gender1") +
  set_prior("student_t(3, 0, 2)", class = "b", coef = "age_dec_c")


m_depressed <- brm(
  more_depressed ~ age_dec_c + gender + going_out_c + employed, 
  data = COVID_mental_health,
  prior = my_priors,
  family = bernoulli()
)  


m_depressed_prior <- unupdate(m_depressed)


saveRDS(m_depressed, "m_depressed.RDS")
saveRDS(m_depressed_prior, "m_depressed_prior.RDS")

