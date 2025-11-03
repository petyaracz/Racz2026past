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
    mond = as.factor(lemma == 'mond'),
    lemma_length = nchar(lemma),
    number_plural = as.factor(number == 'P'),
    person_other = as.factor(person != 3),
    verb_def = as.factor(def),
    preverb = as.factor(preverb),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

simple_counts = d |> 
  count(lemma,log_lemma_freq,class,translation,year,lemma_length,preverb) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

my_counts = d |> 
  count(lemma,lemma_length,log_lemma_freq,class,number_plural,person_other,verb_def,translation,year,preverb) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

my_counts_var = d |> 
  count(lemma,lemma_length,log_lemma_freq,class,number_plural,person_other,verb_def,translation,year,preverb) |>  
  pivot_wider(names_from = class, values_from = n) |> 
  filter(!is.na(Ipf),!is.na(Past)) |> 
  mutate(p = Ipf / (Ipf + Past))

my_counts_mond = d |> 
  count(lemma,lemma_length,log_lemma_freq,class,number_plural,person_other,verb_def,translation,year,preverb,mond) |>  
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
check_collinearity(fit5) # hupsz
check_collinearity(fit6) # hupsz
check_collinearity(fit7) # hupsz
check_collinearity(fit8)
check_collinearity(fit9) # hupsz
check_collinearity(fit10) # hupsz
check_collinearity(fit11)

plot(compare_performance(fit1,fit3,fit4,fit8,fit11,metrics = 'common'))

test_likelihoodratio(fit4,fit8) # ns
p0 = plot_model(fit4, 'pred', terms = c('translation'))
p1 = plot_model(fit4, 'pred', terms = c('translation', 'person_other'))
p3 = plot_model(fit4, 'pred', terms = c('log_lemma_freq [all]'))
p4 = plot_model(fit4, 'pred', terms = c('lemma_length'))
p5 = plot_model(fit4, 'pred', terms = c('preverb'))

(p0 + p1) / (p3 + p4 + p5)
# ggsave('viz/best_model.png', width = 12, height = 4, dpi = 'print') # no, bad model

plots = plot(check_model(fit4))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

plot_model(fit4, 'est', transform = NULL)

# -- fit, var -- #

vfit1 = glm(cbind(Ipf,Past) ~ log_lemma_freq + translation + number_plural + person_other + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit2 = glm(cbind(Ipf,Past) ~ log_lemma_freq * translation + number_plural + person_other + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit3 = glm(cbind(Ipf,Past) ~ translation * number_plural + log_lemma_freq + person_other + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit4 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit5 = glm(cbind(Ipf,Past) ~ translation * verb_def + log_lemma_freq + number_plural + person_other + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit6 = glm(cbind(Ipf,Past) ~ translation * verb_def + translation * person_other + translation * number_plural + log_lemma_freq + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit7 = glm(cbind(Ipf,Past) ~ translation * verb_def + translation * person_other + log_lemma_freq + number_plural + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit8 = glm(cbind(Ipf,Past) ~ translation * person_other + translation * number_plural + log_lemma_freq + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit9 = glm(cbind(Ipf,Past) ~ translation * verb_def + translation * number_plural + log_lemma_freq + person_other + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit10 = glm(cbind(Ipf,Past) ~ translation * person_other + translation * number_plural + translation * log_lemma_freq + verb_def + lemma_length + preverb, data = my_counts_var, family = binomial)
vfit11 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def + lemma_length + translation * preverb, data = my_counts_var, family = binomial)

plot(compare_performance(vfit1,vfit2,vfit3,vfit4,vfit5,metrics = 'common'))
check_collinearity(vfit1)
check_collinearity(vfit2) # hupsz
check_collinearity(vfit3)
check_collinearity(vfit4) # hupsz
check_collinearity(vfit5) # hupsz
check_collinearity(vfit6) # hupsz
check_collinearity(vfit7) # hupsz
check_collinearity(vfit8) # hupsz
check_collinearity(vfit9) # hupsz
check_collinearity(vfit10) # hupsz
check_collinearity(vfit11) # hupsz

plot(compare_performance(vfit1,vfit3,metrics = 'common'))

test_likelihoodratio(vfit1,vfit3)

p0 = plot_model(vfit1, 'pred', terms = c('translation'))
p3 = plot_model(vfit1, 'pred', terms = c('log_lemma_freq [all]'))
p4 = plot_model(vfit1, 'pred', terms = c('lemma_length'))
p5 = plot_model(vfit1, 'pred', terms = c('preverb'))

p0 / (p3 + p4 + p5)
# ggsave('viz/best_model_var.png', width = 12, height = 4, dpi = 'print') # no bad model

plots = plot(check_model(vfit1))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

plot_model(vfit1, 'est', transform = NULL)

# variable forms behave same as all forms, but provide worse fit

my_counts_var |> 
  ggplot(aes(log_lemma_freq,p)) +
  geom_point() +
  geom_smooth()

my_counts |> 
  ggplot(aes(log_lemma_freq,p)) +
  geom_point() +
  geom_smooth()

# mond is doing a lot of work

# -- fit: mond -- #

# -- fit -- #

mfit1 = glm(cbind(Ipf,Past) ~ log_lemma_freq + translation + number_plural + person_other + verb_def + lemma_length + preverb + mond, data = my_counts_mond, family = binomial)
mfit2 = glm(cbind(Ipf,Past) ~ log_lemma_freq * translation + number_plural + person_other + verb_def + lemma_length + preverb + mond, data = my_counts_mond, family = binomial)
mfit3 = glm(cbind(Ipf,Past) ~ translation * number_plural + log_lemma_freq + person_other + verb_def + lemma_length + preverb + mond, data = my_counts_mond, family = binomial)
mfit4 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def + lemma_length + preverb + mond, data = my_counts_mond, family = binomial)
mfit5 = glm(cbind(Ipf,Past) ~ translation * verb_def + log_lemma_freq + number_plural + person_other + verb_def + lemma_length + preverb + mond, data = my_counts_mond, family = binomial)
mfit6 = glm(cbind(Ipf,Past) ~ log_lemma_freq + number_plural + person_other + verb_def + lemma_length + preverb + mond * translation, data = my_counts_mond, family = binomial)
mfit7 = glm(cbind(Ipf,Past) ~ translation * person_other + log_lemma_freq + number_plural + verb_def + lemma_length + preverb + translation * mond, data = my_counts_mond, family = binomial)

check_collinearity(mfit1)
check_collinearity(mfit2) # upsz
check_collinearity(mfit3)
check_collinearity(mfit4)
check_collinearity(mfit5) # upsz
check_collinearity(mfit6)
check_collinearity(mfit7)

plot(compare_performance(mfit1,mfit3,mfit4,mfit6,mfit7))

# nice labels
my_counts_mond_tidy = my_counts_mond |> 
  mutate(
    log10_lemma_freq = log_lemma_freq,
    preverb_particle = ifelse(as.logical(preverb), 'present', 'absent'),
    person = ifelse(as.logical(person_other), 'other', '3'),
    number = ifelse(as.logical(number_plural), 'pl', 'sg'),
    verb_definiteness = ifelse(as.logical(verb_def), 'definite', 'indefinite')
  )

tidy_fit = glm(
  cbind(Ipf,Past) ~ 
    translation +
    log10_lemma_freq +
    preverb_particle +
    person +
    number +
    verb_definiteness +
    lemma_length +
    mond,
  data = my_counts_mond_tidy,
  family = binomial
                 )

p0 = plot_model(tidy_fit, 'pred', terms = c('translation')) + ggtitle('') + theme_bw() + ylab('p(Ipf)')
p1 = plot_model(tidy_fit, 'pred', terms = c('log10_lemma_freq [all]')) + ggtitle('') + theme_bw() + ylab('p(Ipf)') + xlab('log10 lemma freq')
p2 = plot_model(tidy_fit, 'pred', terms = c('person')) + ggtitle('') + theme_bw() + ylab('p(Ipf)')
p3 = plot_model(tidy_fit, 'pred', terms = c('number')) + ggtitle('') + theme_bw() + ylab('p(Ipf)')
p4 = plot_model(tidy_fit, 'pred', terms = c('lemma_length')) + ggtitle('') + theme_bw() + ylab('p(Ipf)')
p5 = plot_model(tidy_fit, 'pred', terms = c('preverb_particle')) + ggtitle('') + theme_bw() + ylab('p(Ipf)')
p6 = plot_model(tidy_fit, 'pred', terms = c('verb_definiteness')) + ggtitle('') + theme_bw() + ylab('p(Ipf)')

p0 / (p1 + p2 + p3) / (p4 + p5 + p6)
ggsave('viz/best_model_mond.png', width = 6, height = 6, dpi = 'print')
