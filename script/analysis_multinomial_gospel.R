# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(brms)
library(broom.mixed)

# -- read -- #

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- set factors -- #

d2 = d |> 
  mutate(
    log_lemma_freq_scaled = scales::rescale(log_lemma_freq),
    lemma_length = scales::rescale(nchar(lemma)),
    number = fct_relevel(number, 'S'),
    definite = ifelse(definite, 'definite', 'indefinite'),
    person = fct_relevel(as.factor(person), '3','2','1'),
    prefix = ifelse(prefix, 'prefixed verb', 'other verb') |> 
      fct_relevel('other verb'),
    modal_verb = ifelse(modal_verb, 'modal verb', 'other verb') |> 
      fct_relevel('other verb'),
    communication_verb = ifelse(communication_verb, 'communicative verb', 'other verb') |> 
      fct_relevel('other verb'),
    motion_verb = ifelse(motion_verb, 'motion verb', 'other verb') |> 
      fct_relevel('other verb'),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

# -- fit -- #

# fit1 = brm(
#   verb_past_class_complex ~ log_lemma_freq_scaled + translation + number +
#     person + prefix + definite + motion_verb + communication_verb + modal_verb +
#     lemma_length,
#   data = d2,
#   family = categorical(link = "logit", refcat = "imperfective"),
#   cores = 4,
#   chains = 4,
#   iter = 2000
# )
# 
# saveRDS(fit1, 'models/fit1.rds')

laplace_priors = c(
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muperfective"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "mucomplexperfective"),
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "mucompleximperfective"),
  prior(double_exponential(0, 1), class = "b", dpar = "muperfective"),
  prior(double_exponential(0, 1), class = "b", dpar = "mucomplexperfective"),
  prior(double_exponential(0, 1), class = "b", dpar = "mucompleximperfective")
)

fit2 = brm(
  verb_past_class_complex ~ log_lemma_freq_scaled + translation + definite + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length,
  data = d2,
  family = categorical(link = "logit", refcat = "imperfective"),
  prior = laplace_priors,
  cores = 4,
  chains = 4,
  iter = 2000
)

saveRDS(fit2, 'models/fit2.rds')

fit3 = brm(
  verb_past_class_complex ~ log_lemma_freq_scaled + translation + definite + number +
    person + prefix + motion_verb + communication_verb + modal_verb +
    lemma_length + (1|book:chapter),
  data = d2,
  family = categorical(link = "logit", refcat = "imperfective"),
  cores = 4,
  chains = 4,
  iter = 3000
)

saveRDS(fit3, 'models/fit3.rds')

fit4 = brm(
  verb_past_class_complex ~ log_lemma_freq_scaled + translation + definite + number +
    person + prefix + motion_verb + communication_verb + modal_verb +
    lemma_length + (1|book:chapter),
  prior = laplace_priors,
  data = d2,
  family = categorical(link = "logit", refcat = "imperfective"),
  cores = 4,
  chains = 4,
  iter = 3000
)

saveRDS(fit4, 'models/fit4.rds')

loo1 = loo(fit1)
loo2 = loo(fit2)
loo3 = loo(fit3)

print('loo1, loo2:')
loo_compare(loo1,loo2)
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -1.4       0.8  
print('loo1, loo3:')
loo_compare(loo1,loo3)
# elpd_diff se_diff
# fit3     0.0       0.0
# fit1 -1511.7      55.1

# -- save -- #

saveRDS(fit1, 'models/fit1.rds')
saveRDS(fit2, 'models/fit2.rds')
saveRDS(fit3, 'models/fit3.rds')

# -- load -- #

fit1 = readRDS('models/fit1.rds')

# -- vif -- #

X = model.matrix(~ log_lemma_freq_scaled + translation + number + person + 
                    prefix + motion_verb + communication_verb + modal_verb + 
                    lemma_length, 
                  data = d2)

# Remove intercept column
X = X[, -1]

# Fit a dummy model and check VIF
dummy_model = lm(rnorm(nrow(X)) ~ ., data = as.data.frame(X))
car::vif(dummy_model)

# "We assessed collinearity by calculating variance inflation factors from the predictor design matrix. Standard VIF diagnostics are not directly applicable to multinomial models, so we computed VIF using the predictor matrix in a linear model framework (car package in R). All VIF values were below 5."
# escalate:
# "VIF diagnostics for multinomial models are not well-established in the literature. We verified collinearity using multiple approaches including pairwise correlations and VIF calculated from the design matrix. All diagnostics indicated no collinearity issues."
