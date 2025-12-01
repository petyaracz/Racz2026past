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
      pred_imperfective = mean(`P(Y = imperfective)`),
      pred_perfective = mean(`P(Y = perfective)`),
      pred_complex_imperfective = mean(`P(Y = complex imperfective)`),
      pred_complex_perfective = mean(`P(Y = complex perfective)`),
      .by = {{varname}}
    ) |> 
    pivot_longer(-{{varname}}, names_to = 'pred_level', values_to = 'pred') |> 
    mutate(level = pred_level |> 
             str_remove_all('^pred_') |> 
             str_replace('_', ' ')
    ) |> 
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
    mutate(diff = prob - pred) |> 
    ggplot(aes(diff,{{varname}}, fill = level)) +
    geom_vline(xintercept = 0, lty = 1) +
    geom_col(position = position_dodge()) +
    theme_bw() +
    scale_fill_colorblind()
  
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
'modal_verb',
'definite'
)

p1 = d |> 
  mutate(freq_ntile = glue::glue('Q{ntile(log_lemma_freq, 4)}')) |> 
  drawComparison(freq_ntile) + ggtitle('lemma freq (quartiles)')
p2 = d |> 
  mutate(length_ntile = glue::glue('Q{ntile(lemma_length, 4)}')) |> 
  drawComparison(length_ntile) + ggtitle('lemma length (quartiles)')
p3 = drawComparison(d,number) + ggtitle('number')
p4 = d |> 
  mutate(person = as.factor(person)) |> 
  drawComparison(person) + ggtitle('person')
p5 = drawComparison(d,prefix) + ggtitle('prefix')
p6 = drawComparison(d,definite) + ggtitle('definite')
p7 = drawComparison(d,modal_verb) + ggtitle('modal verb')
p8 = drawComparison(d,motion_verb) + ggtitle('motion verb')
p9 = drawComparison(d,communication_verb) + ggtitle('communicative verb')

wrap_plots(p1,p2,p3,p4,p5,p6,p7,p8,p9) + plot_layout(guides = 'collect')
ggsave('viz/gospel_to_tmk_preds.png', dpi = 'print', width = 9, height = 6)
