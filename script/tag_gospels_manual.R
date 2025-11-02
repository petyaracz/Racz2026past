# use the gospel list
# try to generalise stem-ipf and find the patterns
# some of them can be derived from past: evett-evé

# tag gospels by hand

# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)
library(tidytext)
library(glue)
library(googlesheets4)

# -- read -- #

# par bible
b = read_tsv('~/Github/Racz2025Bible/dat/gospels.gz')
# omagyar dictionary
o = read_tsv('dat/omagyar_dictionary.tsv.gz')

# -- strings -- #

any_vowel = '[aáeéiíoóöőuúüű]'
any_consonant = '[^aáeéiíoóöőuúüű]'

# -- unnest -- #

d = b |> 
  filter(type == 'normalised') |>
  unnest_tokens(word, text, drop = F)

w = d |> 
  count(word, name = 'freq')

# -- find past -- #

# 3sg
# indef 
# ipf: -a/-e
# past: -tt/-t
# def
# ipf: -á/-é
# past: -ta/-te

w1 = w |> 
  mutate(
    tag = case_when(
      str_detect(word, glue('({any_consonant}t|[eoö]tt)$')) ~ '3sg.past.indef',
      str_detect(word, glue('t[ae]$')) ~ '3sg.past.def'
      )
  ) |> 
  filter(
    !is.na(tag),
    !str_detect(word, '[aeoö]t[oeö](tt[ae]|tt)$'),
    freq > 4
    ) |> 
  arrange(-freq)
  
w1 |> 
  write_sheet('https://docs.google.com/spreadsheets/d/1aHORd9t4nkJeZ5pSVX6gri14Hr5ObCQMK75OyKVBUoc/edit?usp=sharing', 'past')

# hand-checked...

w2 = read_sheet('https://docs.google.com/spreadsheets/d/1aHORd9t4nkJeZ5pSVX6gri14Hr5ObCQMK75OyKVBUoc/edit?usp=sharing', 'past')

past_types = w2 |> 
  filter(
    is.na(drop),
    !word %in% c('bűnt','megtisztulásodért','szelement')
         ) |> 
  select(-drop) |> 
  mutate(
    base = str_remove(word, '^(meg|el|fel|be|ki)'),
    lemma = case_when(
      word == 'odament' ~ 'megy',
      word == 'általment' ~ 'megy',
      word == 'volt' ~ 'van',
      word == 'lett' ~ 'lesz',
      word == 'hitt' ~ 'hisz',
      word == 'hitte' ~ 'hisz',
      word == 'jött' ~ 'jön',
      word == 'eljött' ~ 'jön',
      word == 'kijött' ~ 'jön',
      word == 'megjött' ~ 'jön',
      word == 'tett' ~ 'tesz',
      word == 'cselekedett' ~ 'cselekszik',
      word == 'vette' ~ 'vesz',
      word == 'vett' ~ 'vesz',
      word == 'tette' ~ 'tesz',
      word == 'ette' ~ 'eszik',
      word == 'megette' ~ 'eszik',
      word == 'ivott' ~ 'iszik',
      base == 'ment' ~ 'megy',
      tag == '3sg.past.indef' & str_detect(word, '[oeö]tt$') ~ str_remove(base, '[oeö]tt$'),
      tag == '3sg.past.indef' & !str_detect(word, '[oeö]tt$') ~ str_remove(base, 't$'),
      tag == '3sg.past.def' & str_detect(word, '[oeö]tt[ae]$') ~ str_remove(base, '[oeö]tt[ae]$'),
      tag == '3sg.past.def' & !str_detect(word, '[oeö]tt[ae]$') ~ str_remove(base, 't[ae]$'),
    )
  )

w3 = w |> 
  mutate(
    tag = case_when(
      str_detect(word, glue('[ae]$')) ~ '3sg.ipf.indef',
      str_detect(word, glue('[áé]$')) ~ '3sg.ipf.def'
    )
  ) |> 
  filter(
    !is.na(tag),
    !str_detect(word, '[aeoö]t[oeö]t[aeáé]$'),
    freq > 4,
    !word %in% past_types$word,
    str_count(word, any_vowel) > 1,
    !str_detect(word, '[jh][ae]$'),
    !str_detect(word, glue('{any_consonant}[br][ae]$'))
  ) |> 
  arrange(-freq)

w3 |> 
  write_sheet('https://docs.google.com/spreadsheets/d/1aHORd9t4nkJeZ5pSVX6gri14Hr5ObCQMK75OyKVBUoc/edit?usp=sharing', 'ipf')

# hand-checked...

w4 = read_sheet('https://docs.google.com/spreadsheets/d/1aHORd9t4nkJeZ5pSVX6gri14Hr5ObCQMK75OyKVBUoc/edit?usp=sharing', 'ipf') |> 
  filter(
    is.na(drop)
  )

ipf_types = w4 |> 
  select(-drop) |> 
  mutate(
    base = str_remove(word, '^(meg|el|fel|be|ki)'),
    lemma = str_remove(base, '[aeáé]$')
  )

past_types |> 
  bind_rows(ipf_types) |> 
  write_tsv('dat/hand_plucked_types.tsv')
