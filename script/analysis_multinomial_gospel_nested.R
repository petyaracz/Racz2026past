fit1c = brm(
  verb_past_class_complex ~ log_lemma_freq_scaled + translation + number +
    person + prefix + motion_verb + communication_verb + modal_verb +
    lemma_length + (1|book:chapter),
  data = d2,
  family = categorical(link = "logit", refcat = "ipf"),
  cores = 4,
  chains = 4,
  iter = 2000
)

saveRDS(fit1c, 'models/fit1c.rds')

summary(fit1c)
sjPlot::plot_model(fit1c, 'pred')
# you don't get a lot out of this. I don't think a larger sample size would make a difference.