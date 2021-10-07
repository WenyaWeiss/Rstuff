library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(tvthemes)
library(extrafont)
library(ggtext)
library(wesanderson)

import_avatar()
loadfonts()


avatar <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv")

avatar


sorted_avatar <- avatar %>%
  mutate(chat_len = str_count(character_words, "\\W+")) %>%
  mutate(character = factor(c(avatar$character)) ) %>%
  mutate(book = factor(c(avatar$book), levels =  c("Water", "Earth", "Fire"))) %>%
  drop_na()


temp <- sorted_avatar %>%
  group_by(character) %>%
  summarise(sor = sum(chat_len)) %>%
  arrange(desc(sor))

elected <- temp$character[1:5]

sorted_avatar <- sorted_avatar %>%
  filter(character %in% elected) %>%
  group_by(chapter_num, character, book) %>%
  summarise(alchat = sum(chat_len))

sorted_avatar

chat <- ggplot(sorted_avatar, aes(x = sorted_avatar$character, y = sorted_avatar$alchat, fill = character)) +
  geom_boxplot() +
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1" )) +
  geom_point(size=3, alpha=0.5) +
#  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1"))
  
  facet_grid(cols = vars(book)) +
#  theme_avatar(text.font = "Slayer",
#               title.font = "Slayer") +
  
  theme(plot.title = element_markdown(size = 20,
                                      hjust = 0.06),
        plot.subtitle = element_markdown(hjust = 0.1),
        plot.caption = element_text(margin = margin(t = 10)),
        panel.spacing.y = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 11,
                                    margin = margin(r = 10)),
        #axis.text.y = element_blank(),
        axis.title.x = element_text(size = 11,
                                    margin = margin(t = 10))) +
  labs(x = "Characters", y = "Numbers of words spoken per episode", 
       title = " <br>Who is the chattiest character in<br><b>  Avatar: The Last Airbender</b>",
       subtitle = "Five mist talkative characters have more than a hundred words every episode")


chat

