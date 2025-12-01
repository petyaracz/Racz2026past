# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(brms)
library(broom.mixed)

# -- read -- #

d = read_tsv('dat/tmk_and_dict.tsv.gz')
fit3 = readRDS('models/fit3.rds')

# -- set factors -- #

d2 = d |> 
  mutate(
    log_lemma_freq_scaled = scales::rescale(log_lemma_freq),
    lemma_length = scales::rescale(nchar(lemma)),
    number = fct_relevel(number, 'S'),
    person = fct_relevel(as.factor(person), '3','2','1'),
    definite = ifelse(definite, 'definite', 'indefinite') |> 
      fct_relevel('indefinite'),
    prefix = ifelse(prefix, 'prefixed verb', 'other verb') |> 
      fct_relevel('other verb'),
    modal_verb = ifelse(modal_verb, 'modal verb', 'other verb') |> 
      fct_relevel('other verb'),
    communication_verb = ifelse(communication_verb, 'communicative verb', 'other verb') |> 
      fct_relevel('other verb'),
    motion_verb = ifelse(motion_verb, 'motion verb', 'other verb') |> 
      fct_relevel('other verb'),
    translation = 'Karoli'
  )

# -- predict -- #

preds = predict(fit3, d2, re_formula = NA)

preds2 = as_tibble(preds) |> 
  rename(
    pred_complex_imperfective = `P(Y = complex imperfective)`,
    pred_complex_perfective = `P(Y = complex perfective)`,
    pred_imperfective = `P(Y = imperfective)`,
    pred_perfective = `P(Y = perfective)`
  )

# -- write -- #

bind_cols(d2,preds) |> 
  write_tsv('dat/tmk_and_dict_and_pred.tsv.gz')
