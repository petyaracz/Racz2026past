# trying out alternatives for plot_model "pred"

# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(patchwork)
library(marginaleffects)
library(effects)

# -- read -- #

d = read_tsv('dat/fitted_multinomial.tsv.gz')

# -- order -- #

d = d |> 
  mutate(translation = fct_reorder(translation,year))

# -- longer -- #

dl = d |>
  pivot_longer(
    cols = starts_with(c("Estimate.", "Est.Error.", "Q5.", "Q95.")),
    names_to = c(".value", "level"),
    names_pattern = "(Estimate|Est\\.Error|Q5|Q95)\\.P\\(Y = (.+)\\)"
  ) |>
  rename(
    estimate = Estimate,
    std.error = Est.Error,
    q5 = Q5,
    q95 = Q95
  )

# -- plot -- #

## discrete

dl |> 
  filter(
    prefix == 'other verb'
  ) |> 
  summarise(
    estimate = median(estimate),
    q5 = median(q5),
    q95 = median(q95),
    .by = c(motion_verb,level)
  ) |> 
  ggplot() +
  geom_line(aes(motion_verb,estimate,colour = level,group = level)) +
  geom_line(aes(motion_verb,q5,colour = level,group = level), alpha = .5) +
  geom_line(aes(motion_verb,q95,colour = level,group = level), alpha = .5) +
  theme_bw() +
  scale_colour_viridis_d(labels = c('mondá','mondotta','mondta vala','mondja vala'), name = 'past tense') +
  ylab('p')

# ho hum

## continuous

dl |> 
  filter(
    motion_verb == 'other verb',
    communication_verb == 'other verb',
    modal_verb == 'other verb',
    prefix == 'other verb'
  ) |> 
  summarise(
    estimate = median(estimate),
    q5 = median(q5),
    q95 = median(q95),
    .by = c(translation,level)
  ) |> 
  ggplot() +
  geom_line(aes(translation,estimate,colour = level,group = level)) +
  geom_line(aes(translation,q5,colour = level,group = level), alpha = .5) +
  geom_line(aes(translation,q95,colour = level,group = level), alpha = .5) +
  theme_bw() +
  scale_colour_viridis_d(labels = c('mondá','mondotta','mondta vala','mondja vala'), name = 'past tense') +
  ylab('p')

dl |> 
  mutate(freq_ntile = ntile(log_lemma_freq_scaled, 10)) |> 
  filter(
    motion_verb == 'other verb',
    communication_verb == 'other verb',
    modal_verb == 'other verb',
    prefix == 'other verb'
  ) |> 
  summarise(
    estimate = median(estimate),
    q5 = median(q5),
    q95 = median(q95),
    .by = c(freq_ntile,level)
  ) |> 
  ggplot() +
  geom_line(aes(freq_ntile,estimate,colour = level,group = level)) +
  geom_line(aes(freq_ntile,q5,colour = level,group = level), alpha = .5) +
  geom_line(aes(freq_ntile,q95,colour = level,group = level), alpha = .5) +
  theme_bw() +
  scale_colour_viridis_d(labels = c('mondá','mondotta','mondta vala','mondja vala'), name = 'past tense') +
  ylab('p') +
  xlab('log10 lemma frequency (deciles)')

dl |> 
  mutate(length_ntile = ntile(lemma_length, 10)) |> 
  filter(
    motion_verb == 'other verb',
    communication_verb == 'other verb',
    modal_verb == 'other verb',
    prefix == 'other verb'
  ) |> 
  summarise(
    estimate = median(estimate),
    q5 = median(q5),
    q95 = median(q95),
    .by = c(length_ntile,level)
  ) |> 
  ggplot() +
  geom_line(aes(length_ntile,estimate,colour = level,group = level)) +
  geom_line(aes(length_ntile,q5,colour = level,group = level), alpha = .5) +
  geom_line(aes(length_ntile,q95,colour = level,group = level), alpha = .5) +
  theme_bw() +
  scale_colour_viridis_d(labels = c('mondá','mondotta','mondta vala','mondja vala'), name = 'past tense') +
  ylab('p') +
  xlab('lemma length in characters (deciles)')
