# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(lme4)
library(performance)
library(sjPlot)
library(broom.mixed)
library(patchwork)

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- counts -- #

d = d |> 
  mutate(
    lemma_length = nchar(lemma),
    number_plural = as.factor(number == 'P'),
    person_other = as.factor(person != 3),
    verb_def = as.factor(def),
    preverb = as.factor(preverb),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

simple_counts = d |> 
  count(lemma,log_lemma_freq,class,translation,year,lemma_length + preverb,preverb) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

my_counts = d |> 
  count(lemma,lemma_length + preverb,log_lemma_freq,class,number_plural,person_other,verb_def,translation,year,preverb) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

# -- fit: simple -- #

fit0a = glm(cbind(Ipf,Past) ~ log_lemma_freq + translation + preverb + lemma_length + preverb, data = simple_counts, family = binomial)
fit0b = glm(cbind(Ipf,Past) ~ log_lemma_freq * translation + preverb + lemma_length + preverb, data = simple_counts, family = binomial)

check_collinearity(fit0b)
plots = plot(check_model(fit0b))
plots = plot(check_model(fit0a))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

# -- fit -- #

fit1 = glm(cbind(Ipf,Past) ~ log_lemma_freq + translation + number_plural + person_other + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit2 = glm(cbind(Ipf,Past) ~ log_lemma_freq * translation + number_plural + person_other + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit3 = glm(cbind(Ipf,Past) ~ translation * number_plural + log_lemma_freq + person_other + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit4 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit5 = glm(cbind(Ipf,Past) ~ translation * verb_def + log_lemma_freq + number_plural + person_other + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit6 = glm(cbind(Ipf,Past) ~ translation * verb_def + translation * person_other + translation * number_plural + log_lemma_freq + lemma_length + preverb, data = my_counts, family = binomial)
fit7 = glm(cbind(Ipf,Past) ~ translation * verb_def + translation * person_other + log_lemma_freq + number_plural + lemma_length + preverb, data = my_counts, family = binomial)
fit8 = glm(cbind(Ipf,Past) ~ translation * person_other + translation * number_plural + log_lemma_freq + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit9 = glm(cbind(Ipf,Past) ~ translation * verb_def + translation * number_plural + log_lemma_freq + person_other + lemma_length + preverb, data = my_counts, family = binomial)
fit10 = glm(cbind(Ipf,Past) ~ translation * person_other + translation * number_plural + translation * log_lemma_freq + verb_def + lemma_length + preverb, data = my_counts, family = binomial)
fit11 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def + lemma_length + translation * preverb, data = my_counts, family = binomial)


plot(compare_performance(fit1,fit2,fit3,fit4,fit5,metrics = 'common'))
check_collinearity(fit2) # hupsz
check_collinearity(fit3)
check_collinearity(fit4)
check_collinearity(fit5)
check_collinearity(fit6) # hupsz
check_collinearity(fit7) # hupsz
check_collinearity(fit8)
check_collinearity(fit9) # hupsz
check_collinearity(fit10) # hupsz
check_collinearity(fit11)

plot(compare_performance(fit3,fit4,fit5,fit8,fit11,metrics = 'common'))

test_likelihoodratio(fit4,fit8) # s
p0 = plot_model(fit8, 'pred', terms = c('translation'))
p1 = plot_model(fit8, 'pred', terms = c('translation', 'person_other'))
p2 = plot_model(fit8, 'pred', terms = c('translation', 'number_plural'))
p3 = plot_model(fit8, 'pred', terms = c('log_lemma_freq [all]'))
p4 = plot_model(fit8, 'pred', terms = c('lemma_length'))
p5 = plot_model(fit8, 'pred', terms = c('preverb'))

(p0 + p1 + p2) / (p3 + p4 + p5)
ggsave('viz/best_model.png', width = 12, height = 4, dpi = 'print')

plots = plot(check_model(fit8))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

plot_model(fit8, 'est', transform = NULL)

