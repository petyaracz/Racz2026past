# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(rstanarm)
library(performance)
library(sjPlot)
library(broom.mixed)
library(patchwork)

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- set factors -- #

d2 = d |> 
  mutate(
    mond = ifelse(lemma == 'mond', 'mond', 'other verb'),
    log_lemma_freq_scaled = scales::rescale(log_lemma_freq),
    lemma_length = scales::rescale(nchar(lemma)),
    number = fct_relevel(number, 'S'),
    person = fct_relevel(as.factor(person), '3','2','1'),
    prefix = ifelse(prefix, 'prefixed verb', 'other verb') |> 
      fct_relevel('other verb'),
    modal_verb = ifelse(modal_verb, 'other verb', 'modal verb') |> 
      fct_relevel('other verb'),
    communication_verb = ifelse(communication_verb, 'other verb', 'communicative verb') |> 
      fct_relevel('other verb'),
    motion_verb = ifelse(motion_verb, 'other verb', 'motion verb') |> 
      fct_relevel('other verb'),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

# -- counts -- #

my_counts = d2 |> 
  count(lemma,log_lemma_freq_scaled,class,translation,year,lemma_length,prefix,motion_verb,modal_verb,communication_verb,number,person,mond) |>  
  pivot_wider(names_from = class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

# -- fit -- #

fit0 = stan_glm(
  cbind(Ipf, Past) ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length + mond,
  data = my_counts,
  family = binomial,
  chains = 4,
  iter = 2000,
  seed = 123
)

fit1 = stan_glm(
  cbind(Ipf, Past) ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length + mond,
  data = my_counts,
  family = binomial,
  prior = normal(0, scale = 1),  # adjust scale based on my hunch: 1~20% 2~50% .2~5%
  prior_intercept = normal(0, 2.5),
  chains = 4,
  iter = 2000,
  seed = 123
)

fit2 = stan_glm(
  cbind(Ipf, Past) ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length + mond,
  data = my_counts,
  family = binomial,
  prior = normal(0, scale = .5),  # adjust scale based on my hunch: 1~20% 2~50% .2~5%
  prior_intercept = normal(0, 2.5),
  chains = 4,
  iter = 2000,
  seed = 123
)

comparisons = loo_compare(loo(fit0, k_threshold = .7),loo(fit1, k_threshold = .7),loo(fit2, k_threshold = .7))
# elpd_diff se_diff
# fit2  0.0       0.0   
# fit1 -1.0       2.6   
# fit0 -1.0       3.7 
as_tibble(comparisons) |> 
  write_tsv('dat/comparisons.tsv')

summary(fit0)

plot_model(fit0, 'est', transform = NULL) +
  theme_bw() +
  scale_colour_grey() +
  scale_fill_grey()

my_predictors = names(my_counts)[c(2:3,5:12)]
my_pred_plots = map(my_predictors, ~ 
                      plot_model(fit0, 'pred', terms = .) +
                      theme_bw() +
                      ggtitle('') +
                      # coord_cartesian(ylim = c(.1,.9)) +
                      ylab('p(Ipf)')
                    )

wrap_plots(my_pred_plots, ncol = 4)
ggsave('viz/best_model_mond.png', width = 12, height = 9, dpi = 'print')
