# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(rstanarm)
library(performance)
library(sjPlot)
library(broom.mixed)
library(ggthemes)
library(patchwork)

d = read_tsv('dat/gospel_and_dict.tsv.gz')

# -- filter for ipf/past and no complex -- #

d2 = d |> 
  filter(class_subtype == 'other')

# -- set factors -- #

d3 = d2 |> 
  mutate(
    log_lemma_freq_scaled = scales::rescale(log_lemma_freq),
    lemma_length = scales::rescale(nchar(lemma)),
    number = fct_relevel(number, 'S'),
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

# -- counts -- #

my_counts = d3 |> 
  count(lemma,log_lemma_freq_scaled,verb_past_class,translation,year,lemma_length,prefix,motion_verb,modal_verb,communication_verb,number,person) |>  
  pivot_wider(names_from = verb_past_class, values_from = n, values_fill = 0) |> 
  mutate(p = Ipf / (Ipf + Past))

# -- corr -- #

my_counts |> 
  mutate(
    translation = as.double(translation),
    prefix = prefix != 'other verb',
    modal_verb = modal_verb != 'other verb',
    motion_verb = motion_verb != 'other verb',
    communication_verb = communication_verb != 'other verb',
    number = number == 'S',
    person = as.double(person)
  ) |> 
  select(lemma_length,log_lemma_freq_scaled,translation,prefix,modal_verb,motion_verb,communication_verb,number,person,p) |> 
  cor() |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  pivot_longer(-rowname) |> 
  ggplot(aes(rowname,name,fill = value)) +
  geom_tile() +
  scale_fill_viridis_b(n.breaks = 6) +
  theme_few()

# -- fit -- #

fit0 = stan_glm(
  cbind(Ipf, Past) ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length,
  data = my_counts,
  family = binomial,
  prior = student_t(df = 1, location = 0, scale = 2.5),
  prior_intercept = student_t(df = 1, location = 0, scale = 2.5),
  chains = 4,
  cores = 4,
  iter = 2000
)

fit1 = stan_glm(
  cbind(Ipf, Past) ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length,
  data = my_counts,
  family = binomial,
  prior = laplace(location = 0, scale = 1),
  prior_intercept = student_t(df = 1, location = 0, scale = 2.5),
  chains = 4,
  cores = 4,
  iter = 2000
)

fit2 = stan_glm(
  cbind(Ipf, Past) ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length,
  data = my_counts,
  family = binomial,
  prior = hs(df = 3, global_scale = 0.01),
  prior_intercept = student_t(df = 1, location = 0, scale = 2.5),
  cores = 4,
  chains = 4,
  iter = 2000
)

# -- eval -- #

loo0 = loo(fit0, k_threshold = .7)
loo1 = loo(fit1, k_threshold = .7)
loo2 = loo(fit2, k_threshold = .7)

# -- save -- #

saveRDS(loo0, 'models/loo0.rds')
saveRDS(loo1, 'models/loo1.rds')
saveRDS(loo2, 'models/loo2.rds')

saveRDS(fit0, 'models/fit0.rds')
summary(fit0)

saveRDS(fit1, 'models/fit1.rds')
summary(fit1)

saveRDS(fit2, 'models/fit2.rds')
summary(fit2)

# -- load -- #

loo0 = readRDS('models/loo0.rds')
loo1 = readRDS('models/loo1.rds')
loo2 = readRDS('models/loo2.rds')

loo_compare(loo0,loo1,loo2)

fit0 = readRDS('models/fit0.rds')

# -- summary -- #

tidy(fit0, conf.int = T) |> 
  write_tsv('dat/best_model_coef_binomial.tsv')

# -- viz -- #

plot_model(fit0, 'est', transform = NULL) +
  theme_bw() +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave('viz/best_model_coef_binomial.png', dpi = 'print', width = 6, height = 3)

my_predictors = names(my_counts)[c(2:3,5:11)]
my_pred_plots = map(my_predictors, ~
                      plot_model(fit0, 'pred', terms = .) +
                      theme_bw() +
                      xlab(str_replace_all(., '_', ' ')) +
                      theme(
                        axis.title.x=element_blank()
                      ) +
                      ggtitle('') +
                      coord_flip()
)

wrap_plots(my_pred_plots, ncol = 3)
ggsave('viz/best_model_binomial.png', width = 8, height = 5, dpi = 'print')

# -- stats -- #

# 460,073 words
# .6% past tense verbs in these two classes
# 25772 verb forms
# 3601 lemmata
# 7 translations
# 9 predictors