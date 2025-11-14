# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(brms)
library(performance)
library(ggeffects)
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

# # -- fit -- #
# 
# fit3 = brm(
#   verb_past_class_complex ~ log_lemma_freq_scaled + translation + number + 
#     person + prefix + motion_verb + communication_verb + modal_verb + 
#     lemma_length,
#   data = d2,
#   family = categorical(link = "logit", refcat = "ipf"),
#   chains = 4,
#   iter = 2000
# )
# 
# # -- save -- #
# 
# saveRDS(fit3, 'models/fit3.rds')
# summary(fit3)

# -- load -- #

fit3 = readRDS('models/fit3.rds')

# -- summary -- #

tidy(fit3, conf.int = T) |> 
  write_tsv('dat/best_model_coef_multi.tsv')

# -- vif -- #

X = model.matrix(~ log_lemma_freq_scaled + translation + number + person + 
                    prefix + motion_verb + communication_verb + modal_verb + 
                    lemma_length, 
                  data = d2)

# Remove intercept column
X = X[, -1]

# Fit a dummy model and check VIF
dummy_model = lm(rnorm(nrow(X)) ~ ., data = as.data.frame(X))
car::vif(dummy_model)

# "We assessed collinearity by calculating variance inflation factors from the predictor design matrix. Standard VIF diagnostics are not directly applicable to multinomial models, so we computed VIF using the predictor matrix in a linear model framework (car package in R). All VIF values were below 5."
# escalate:
# "VIF diagnostics for multinomial models are not well-established in the literature. We verified collinearity using multiple approaches including pairwise correlations and VIF calculated from the design matrix. All diagnostics indicated no collinearity issues."

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