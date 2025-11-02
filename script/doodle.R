# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(lme4)
library(performance)
library(sjPlot)
library(broom.mixed)

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- counts -- #

d = d |> 
  mutate(
    number_plural = as.factor(number == 'P'),
    person_other = person != 3,
    person_other = as.factor(ifelse(word == 'vala', 0, person_other)),
    verb_def = as.factor(def),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

my_counts = d |> 
  count(lemma,log_lemma_freq,class,number_plural,person_other,verb_def,translation,year) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

# -- fit -- #

fit1 = glm(cbind(Ipf,Past) ~ log_lemma_freq + translation + number_plural + person_other + verb_def, data = my_counts, family = binomial)
fit2 = glm(cbind(Ipf,Past) ~ log_lemma_freq * translation + number_plural + person_other + verb_def, data = my_counts, family = binomial)
fit3 = glm(cbind(Ipf,Past) ~ translation * number_plural + log_lemma_freq + person_other + verb_def, data = my_counts, family = binomial)
fit4 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def, data = my_counts, family = binomial)
fit5 = glm(cbind(Ipf,Past) ~ translation * verb_def + log_lemma_freq + number_plural + person_other + verb_def, data = my_counts, family = binomial)
fit6 = glm(cbind(Ipf,Past) ~ translation * person_other + translation * number_plural + log_lemma_freq + number_plural + person_other + verb_def, data = my_counts, family = binomial)
fit7 = glm(cbind(Ipf,Past) ~ translation * person_other + translation * log_lemma_freq + number_plural + verb_def, data = my_counts, family = binomial)

plot(compare_performance(fit1,fit2,fit3,fit4,fit5,metrics = 'common'))
check_collinearity(fit5)
plot(compare_performance(fit1,fit2,fit3,fit4,fit6,fit7,metrics = 'common'))
check_collinearity(fit4)
summary(fit4)

plot_model(fit4, 'pred', terms = c('translation', 'person_other'))
