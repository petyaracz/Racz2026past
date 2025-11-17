# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(brms)
library(broom.mixed)

# -- read -- #

d = read_tsv('dat/tmk_and_dict.tsv.gz')
fit1b = readRDS('models/fit1b.rds')

# -- set factors -- #

d2 = d |> 
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
      fct_relevel('other verb')
  )

# -- predict -- #

preds = predict(fit1b, d2)

preds2 = as_tibble(preds) |> 
  rename(
    pred_ipf = `P(Y = ipf)`,
    pred_past = `P(Y = past)`,
    pred_past_complex = `P(Y = past_complex)`,
    pred_perf_complex = `P(Y = perf_complex)`
  )

# -- write -- #

bind_cols(d2,preds) |> 
  write_tsv('dat/tmk_and_dict_and_pred.tsv.gz')
