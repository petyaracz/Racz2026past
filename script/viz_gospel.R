# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(brms)
library(broom.mixed)
library(ggthemes)
library(patchwork)
library(ggeffects)

# -- plotting fun -- #

plotCont = function(fit,pred_name){
  ggpredict(fit, terms = pred_name) |> 
  ggplot(aes(x = x, y = predicted, colour = response.level, fill = response.level)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, colour = NA) +
    labs(x = pred_name, y = "p", colour = "category", fill = "category") +
    theme_minimal() +
    scale_colour_colorblind() +
    scale_fill_colorblind()
}

plotCat = function(fit, pred_name){
  ggpredict(fit, terms = pred_name) |> 
  ggplot(aes(x = x, y = predicted, colour = response.level, fill = response.level)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2, 
                  position = position_dodge(width = 0.3)) +
    labs(x = pred_name, y = "p", colour = "category", fill = "category") +
    theme_minimal() +
    scale_colour_colorblind() +
    scale_fill_colorblind() +
    guides(colour = 'none', fill = 'none')
}

# -- read -- #

d = read_tsv('dat/gospel_and_dict.tsv.gz')
fit3 = readRDS('models/fit3.rds')

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
      fct_relevel('other verb'),
    translation = translation |> 
      fct_reorder(year) |> ordered()
  )

# -- corr -- #

d2 |> 
  mutate(
    prefix = prefix != 'other verb',
    modal_verb = modal_verb != 'other verb',
    motion_verb = motion_verb != 'other verb',
    communication_verb = communication_verb != 'other verb',
    number = number == 'S',
    person = as.double(person),
    translation_MunchK = translation == 'MunchK',
    translation_JordK = translation == 'JordK',
    translation_Pesti = translation == 'Pesti',
    translation_Sylvester = translation == 'Sylvester',
    translation_Heltai = translation == 'Heltai',
    translation_Karoli = translation == 'Karoli',
    translation_Kaldi = translation == 'Kaldi'
  ) |> 
  select(lemma_length,log_lemma_freq_scaled,prefix,modal_verb,motion_verb,communication_verb,number,person,all_of(starts_with('translation_'))) |> 
  cor(method = 'spearman') |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  pivot_longer(-rowname) |> 
  ggplot(aes(rowname,name,fill = value, label = round(value,2))) +
  geom_tile() +
  geom_text(colour = 'darkgrey') +
  scale_fill_viridis_b(n.breaks = 6) +
  theme_few() +
  labs(fill = 'Spearman r') +
  xlab('') +
  ylab('') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave('viz/cors_multi.png', dpi = 'print', width = 10, height = 8)

# -- summary -- #

tidy(fit3, conf.int = T) |> 
  write_tsv('dat/best_model_coef_multi.tsv')

# -- pred -- #

cont_pred = c("log_lemma_freq_scaled", "lemma_length")
cat_pred = c("number", "person", "definite", "prefix", "motion_verb", "communication_verb", "modal_verb")

set1 = map(cont_pred,
            ~ plotCont(fit3, .)
          )

set2 = map(cat_pred,
           ~ plotCat(fit3, .)
          )

set3 = plotCat(fit3, "translation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

wrap_plots(c(set1,set3,set2), nrow = 4) + plot_layout(guides = 'collect', heights = c(2,1,1,1))
ggsave('viz/best_model_multi.png', width = 10, height = 8, dpi = 'print')


# -- ranef -- #

ranef(fit3)
# you could do something with this bud
# Drager, Katie, and Jennifer Hay. "Exploiting random intercepts: Two case studies in sociophonetics." Language Variation and Change 24, no. 1 (2012): 59-78.
tidy_re = tidy(fit3, effects = "ran_vals", robust = TRUE)

tidy_re |> 
  mutate(
    outcome = str_remove(group, 'book:chapter__mu'),
    book = str_extract(level, '^.*(?=_)'),
    verse = str_extract(level, '(?<=_).*$') |> as.double()
  ) |> 
  ggplot(aes(verse, estimate, colour = outcome)) +
  geom_line() +
  facet_wrap(~ book, ncol = 1) +
  theme_few() +
  scale_colour_viridis_d()

ggsave('viz/best_model_multi_ranef.png', width = 10, height = 8, dpi = 'print')
