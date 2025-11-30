# collect codices w/ morphological analysis and combine them
# these are the old Hungarian texts so far

# -- head -- #

setwd('~/Github/Racz2026past/')

library(tidyverse)

# -- fun -- #

# -- read -- #

# http://omagyarkorpusz.nytud.hu/hu-texts.html

festetics = read_tsv('http://omagyarkorpusz.nytud.hu/documents/kodexek/FestK/rmkinput', skip = 1, col_names = F)

# Festetics-kódex, 1494 előtt. A nyelvemlék hasonmása és betűhű átirata bevezetéssel és jegyzetekkel. Közzéteszi, a bevezetést és a jegyzeteket írta: N. ABAFFY Csilla, Budapest, Argumentum, Magyar Nyelvtudományi Társaság, 1996. (Régi Magyar Kódexek 20.)

guary = read_tsv('http://omagyarkorpusz.nytud.hu/documents/kodexek/GuaryK/rmkinput', skip = 1, col_names = F)

# 1508 elott

jokai = read_tsv('http://omagyarkorpusz.nytud.hu/documents/kodexek/JokK/rmkinput', skip = 1, col_names = F)

# 1440 korul

konyvecse = read_tsv('http://omagyarkorpusz.nytud.hu/documents/kodexek/Konyvecse/rmkinput', skip = 1, col_names = F)

# Könyvecse az szent apostoloknak méltóságokról 1521. A nyelvemlék hasonmása és betűhű átirata. Közzéteszi bevezetést és a jegyzeteket írta: PUSZTAI István, Budapest, Magyar Nyelvtudományi Társaság, 1985. (Régi Magyar Kódexek, 1.) 

jordanszky = read_tsv('http://omagyarkorpusz.nytud.hu/documents/kodexek/JordK/rmkinput', skip = 1, col_names = F)

jordanszky2 = jordanszky |> 
  rownames_to_column() |> 
  slice(102728:n())

# 1516-19

muncheni = read_tsv('http://omagyarkorpusz.nytud.hu/documents/kodexek/MunchK/rmkinput', skip = 1, col_names = F)

# A Müncheni Kódex 1466-ból. Kritikai szövegkiadás a latin megfelelővel együtt, szerk.: NYÍRI Antal, Budapest, Akadémiai, 1971. (Codices Hungarici, 7.)

# -- combine -- #

# colnames absolutely don't line up and there's no col dictionary, so I'll wing it

dat1 = festetics |> 
  rename(
    block = X1,
    word = X2,
    norm = X3,
    note = X6,
    lemma = X7,
    tag = X8
  ) |> 
  rownames_to_column() |> 
  select(rowname,block,word,norm,note,lemma,tag) |> 
  mutate(
    source = 'Festetics', 
    year = 1494,
    block = as.character(block)
  )

dat2 = guary |> 
  rename(
    block = X2,
    word = X3,
    norm = X4,
    note = X7,
    lemma = X8,
    tag = X9
  ) |> 
  rownames_to_column() |> 
  select(rowname,block,word,norm,note,lemma,tag) |> 
  mutate(
    source = 'Guary',
    year = 1508,
    block = as.character(block)
  )

dat3 = jokai |> 
  rename(
    block = X2,
    word = X3,
    norm = X4,
    note = X7,
    lemma = X8,
    tag = X9
  ) |> 
  rownames_to_column() |> 
  select(rowname,block,word,norm,note,lemma,tag) |> 
  mutate(
    source = 'Jokai',
    year = 1440,
    block = as.character(block)
  )

dat4 = konyvecse |> 
  rename(
    block = X1,
    word = X2,
    norm = X3,
    note = X6,
    lemma = X7,
    tag = X8
  ) |> 
  rownames_to_column() |> 
  select(rowname,block,word,norm,note,lemma,tag) |> 
  mutate(
    source = 'Konyvecse', 
    year = 1521,
    block = as.character(block)
  )

dat5 = muncheni |> 
  rename(
    block = X2,
    word = X4,
    norm = X5,
    note = X8,
    lemma = X9,
    tag = X10
  ) |> 
  rownames_to_column() |> 
  select(rowname,block,word,norm,note,lemma,tag) |> 
  mutate(
    source = 'Muncheni',
    year = 1466,
    block = as.character(block)
  )

dat6 = jordanszky2 |> 
  select(rowname,X1,X4,X5,X10) |> 
  rename(
    block = X1,
    word = X4,
    norm = X5,
    tag = X10
  ) |> 
  mutate(
    source = 'Jordanszky',
    year = 1516,
    block = as.character(block)
  )

d = bind_rows(dat1,dat2,dat3,dat4,dat5,dat6) |> 
  mutate(
    norm = str_to_lower(norm),
    lemma = str_to_lower(lemma)
  )

# -- tags -- #

d = d |> 
  mutate(
    verb_past_class = str_extract(tag, 'Past|Ipf'),
    verb_present = str_detect(tag, 'V\\.([SP][123]|Cond\\.[SP][123]|Mod\\.[SP][123]|)'),
    person = ifelse(
      !str_detect(tag, 'V'),
      NA,
      str_extract(tag, '(?<=[SP])[123]')
    ),
    number = ifelse(
      !str_detect(tag, 'V'),
      NA,
      str_extract(tag, '[SP](?=[123])')
    ),
    person = ifelse(
      is.na(tag),
      NA,
      person
    ),
    number = ifelse(
      is.na(tag),
      NA,
      number
    ),
    definite = str_detect(tag, 'Def'),
    definite = ifelse(
      is.na(tag),
      NA,
      definite
    ),
    prefix = str_detect(tag,'VPfx'),
    motion_verb = str_detect(lemma, '(jön|megy|indul)'),
    modal_verb = str_detect(lemma, '(^akar|tud|kell$)'),
    communication_verb = str_detect(lemma, '(mond|szól|beszél|kér|parancsol|kérdez)')
  ) |> 
  filter(!lemma %in% c('van','lesz','el','alá','elevemegy','ki','meg','elő','fel','alá','be','le','által','ide','rá','össze','bele','oda','kibik','haza','hátra','vissza','')) # vala and volt are auxes

# -- dict -- #

dict = d |> 
  count(norm,lemma,verb_past_class,verb_present,tag,number,person,definite,prefix,modal_verb,motion_verb,communication_verb, name = 'omagyar_freq')

# -- write -- #

write_tsv(d, 'dat/combined_data.tsv.gz')
write_tsv(dict, 'dat/omagyar_dictionary.tsv.gz')
