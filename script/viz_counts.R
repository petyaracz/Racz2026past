# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(lme4)
library(sjPlot)
library(performance)

# -- read -- #

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- counts -- #

counts1 = d |> 
  count(verb_past_class_complex,translation,year,book,chapter) |> 
  mutate(translation = fct_reorder(translation,year))

counts2 = counts1 |> 
  mutate(
    sum = sum(n),
    p = n/sum,
    .by = c(translation,year,book,chapter)
  )

# -- fit -- #

fit1 = glmer(n ~ verb_past_class_complex + translation + 
               (1 | book) + 
               (1 | book:chapter),
             family = poisson, data = counts1)
fit2 = glmer(n ~ verb_past_class_complex * translation + 
               (1 | book) + 
               (1 | book:chapter),
             family = poisson, data = counts1)

plot(compare_performance(fit1,fit2,metrics = 'common'))
plots = plot(check_model(fit2))
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plot_model(fit1, 'pred')

# -- okay right -- #

count(d,translation,year,book,chapter,verse)
