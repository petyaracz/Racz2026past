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

# -- viz -- #

d |> 
  count(verb_past_class_complex,year) |> 
  ggplot(aes(year,n,colour = verb_past_class_complex, group = verb_past_class_complex)) +
  geom_point() +
  # geom_line() +
  geom_smooth() +
  theme_bw() +
  scale_colour_colorblind()

# -- comp -- #

predictors = c(
'number',
'person',
'prefix',
'motion_verb',
'communication_verb',
'modal_verb'
)

p1 = d |> 
  mutate(freq_ntile = ntile(log_lemma_freq, 4)) |> 
  drawComparison(freq_ntile) + ggtitle('lemma freq (quartiles)')
p2 = d |> 
  mutate(length_ntile = ntile(lemma_length, 4)) |> 
  drawComparison(length_ntile) + ggtitle('lemma length (quartiles)')
p3 = drawComparison(d,number) + ggtitle('number')
p4 = drawComparison(d,person) + ggtitle('person')
p5 = drawComparison(d,motion_verb) + ggtitle('motion verb')
p6 = drawComparison(d,communication_verb) + ggtitle('communicative verb')

(p1 + p2) / (p3 + p4) / (p5 + p6) + plot_layout(heights = c(2,1,1), guides = 'collect')
ggsave('viz/gospel_to_tmk_preds.png', dpi = 'print', width = 9, height = 9)
