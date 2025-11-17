# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(ggthemes)
library(patchwork)

# -- fun -- #

# for column in d, show how predictions from Gospels and raw probs compare.
drawComparison = function(dat,varname){
  
  my_preds = dat |> 
    summarise(
      pred_ipf = mean(`P(Y = ipf)`),
      pred_past = mean(`P(Y = past)`),
      pred_past_complex = mean(`P(Y = past_complex)`),
      pred_perf_complex = mean(`P(Y = perf_complex)`),
      .by = {{varname}}
    ) |> 
    pivot_longer(-{{varname}}, names_to = 'pred_level', values_to = 'pred') |> 
    mutate(level = str_remove(pred_level, '^pred_')) |> 
    select({{varname}},level,pred)
  
  my_probs = dat |>
    mutate(
      n_cat = n(),
      .by = {{varname}}
    ) |> 
    summarise(
      n_spec = n(),
      .by = c({{varname}},n_cat,verb_past_class_complex)
    ) |> 
    mutate(
      prob = n_spec / n_cat,
      level = verb_past_class_complex
    ) |> 
    select({{varname}},level,prob)
  
  left_join(my_probs,my_preds) |>
    ggplot(aes(prob,pred,colour = level)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
    geom_point() +
    facet_wrap(vars({{varname}})) +
    theme_bw() +
    scale_colour_colorblind()
  
}

# -- read -- #

d = read_tsv('dat/tmk_and_dict_and_pred.tsv.gz')

# -- comp -- #

predictors = c(
'number',
'person',
'prefix',
'motion_verb',
'communication_verb',
'modal_verb'
)

d |> 
  mutate(freq_ntile = ntile(log_lemma_freq, 4)) |> 
  drawComparison(freq_ntile)
d |> 
  mutate(length_ntile = ntile(lemma_length, 4)) |> 
  drawComparison(length_ntile)
drawComparison(d,number)
drawComparison(d,person)
drawComparison(d,motion_verb)
drawComparison(d,communication_verb)

