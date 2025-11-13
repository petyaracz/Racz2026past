# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(brms)
library(performance)
library(ggeffects)
library(broom.mixed)
library(ggthemes)
library(patchwork)

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

# -- fit -- #

fit3 = brm(
  verb_past_class_complex ~ log_lemma_freq_scaled + translation + number + 
    person + prefix + motion_verb + communication_verb + modal_verb + 
    lemma_length,
  data = d2,
  family = categorical(link = "logit", refcat = "ipf"),
  chains = 4,
  iter = 2000
)

# -- save -- #

saveRDS(fit3, 'models/fit3.rds')
summary(fit3)

# -- load -- #

fit3 = readRDS('models/fit3.rds')

# -- summary -- #

tidy(fit3, conf.int = T) |> 
  write_tsv('dat/best_model_coef_multi.tsv')

# -- viz -- #

cont_pred = c("log_lemma_freq_scaled", "lemma_length")
cat_pred = c("number", "person", "prefix", "motion_verb", "communication_verb", "modal_verb")

plotCont(fit3, 'lemma_length')

set1 = map(cont_pred,
            ~ plotCont(fit3, .)
          )

set2 = map(cat_pred,
           ~ plotCat(fit3, .)
          )

set3 = plotCat(fit3, "translation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

wrap_plots(c(set1,set3,set2), ncol = 3) + plot_layout(guides = 'collect')
ggsave('viz/best_model_multi.png', width = 9, height = 6, dpi = 'print')

# -- get epreds -- #

fitted_vals = fitted(fit3, probs = c(0.05, 0.95))
dim(fitted_vals)

d3 = fitted_vals |> 
  as_tibble() |> 
  bind_cols(d2)

write_tsv(d3, 'dat/fitted_multinomial.tsv.gz')