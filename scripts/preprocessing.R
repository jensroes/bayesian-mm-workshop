library(tidyverse)

d <- readRDS("data/spl2_somedata.rds")

d %>% as_tibble() %>%
  glimpse()

length(unique(d$SubNo))
unique(d$transition_type)
unique(d$Location)

d_sent <- d %>% 
  as_tibble() %>%
  filter(grepl(pattern = "sentence", x = transition_type)) %>%
  mutate(transition_type = recode(transition_type, sentence_before_edit = "edit",
                                  sentence_before = "noedit")) %>%
  select(-Transition, -fix_duration,-Timestamp,-Z,-time,-Event,-Location)

write_csv(d_sent, "data/sentence_transitions.csv")


d_word <- d %>% 
  as_tibble() %>%
  filter(grepl(pattern = "word_before", x = transition_type)) %>%
  mutate(transition_type = recode(transition_type, word_before_edit = "edit",
                                  word_before = "noedit")) %>%
  select(-Transition, -fix_duration,-Timestamp,-Z,-time,-Event,-Location) %>%
  select(SubNo, Lang, transition_type, IKI)

write_csv(d_word, "data/word_transitions.csv")


ggplot(d_word, aes(x = Lang, y = IKI, color = transition_type)) +
  geom_jitter(size = .1, position = position_jitterdodge(.25)) +
  scale_y_log10()


ggplot(d_sent, aes(x = n_fix_back, colour = transition_type, fill = transition_type)) +
  geom_histogram(position = "identity", alpha = .25) +
  facet_grid(~Lang)


ggplot(d_sent, aes(x = fix_nwords, colour = Lang, fill = Lang)) +
  geom_histogram(position = "identity", alpha = .25) +
  facet_grid(~transition_type)


ggplot(d_sent, aes(x = IKI, colour = transition_type, fill = transition_type)) +
  geom_histogram(position = "identity", alpha = .25) +
  facet_grid(~Lang) +
  scale_x_log10()


d_sent %>%
  group_by(Lang, transition_type) %>%
  summarise(mean = mean(is_lookback, na.rm=T),
            sd = sd(is_lookback, na.rm=T))


ggplot(d_sent, aes(y = is_lookback, x = transition_type)) +
#  geom_jitter(width = .1, height = .1, size = .1)  + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1) +
  facet_grid(~Lang)

ggplot(d_sent, aes(y = IKI, x = transition_type)) +
#  geom_jitter(width = .1, height = .1, size = .1)  + 
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1) +
  facet_grid(~Lang) +
  scale_y_log10()

