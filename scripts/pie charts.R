rm(list = ls())

library(tidyverse)

setwd("/Users/kelly.andrews/Documents/Manuscripts/kelp stressors review paper")

dat <- read.csv("life cycle literature stats for figure 2.csv", header = TRUE)

#pivot data and change factor level order of lifestages
dat <- dat %>%
  pivot_longer(cols = c("Adult","Juvenile","Gametophytes","Zoospores"),
               names_to = "lifestage",
               values_to = "papers") %>%
  mutate(lifestage = factor(lifestage, levels = c("Zoospores", "Gametophytes", "Juvenile", "Adult")))

  
# Compute the position of labels
data <- dat %>%
  group_by(Stressor) %>%
  mutate(prop = papers / sum(papers) *100)

ggplot(data %>%
         filter(papers != 0),
       aes(x="", y=prop, fill=lifestage)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=pi/2) +
  theme_no_axes() + 
  theme(legend.position="bottom") +
  geom_text(aes(label = papers), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("seagreen4", "goldenrod", "darksalmon", "dodgerblue3")) +
  facet_wrap(Stressor ~.)

                    


library(ggplot2)
library(ggforce)
library(dplyr)

dat <- read.csv("life cycle literature stats for figure 2.csv", header = TRUE)

#pivot data and change factor level order of lifestages
dat <- dat %>%
  pivot_longer(cols = c("Adult","Juvenile","Gametophytes","Zoospores"),
               names_to = "lifestage",
               values_to = "papers") %>%
  mutate(lifestage = factor(lifestage, levels = c("Juvenile", "Gametophytes", "Zoospores", "Adult")))

# calculate the start and end angles for each pie
dat_pies <- left_join(dat,
                      dat %>% 
                        group_by(Stressor) %>%
                        summarize(Cnt_total = sum(papers))) %>%
  group_by(Stressor) %>%
  mutate(end_angle = 2*pi*cumsum(papers)/Cnt_total,      # ending angle for each pie slice
         start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
         mid_angle = 0.5*(start_angle + end_angle))   # middle of each pie slice, for the text label

rpie = 1 # pie radius
rlabel = 0.7 * rpie # radius of the labels; a number slightly larger than 0.5 seems to work better,
# but 0.5 would place it exactly in the middle as the question asks for.
rot = pi  #in order to keep adults in the left/upper portion of all the pies

# draw the pies
ggplot(dat_pies %>%
         filter(papers != 0)) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle + rot, end = end_angle + rot, fill = lifestage)) +
  geom_text(aes(x = rlabel*sin(mid_angle + rot), y = rlabel*cos(mid_angle + rot), label = papers),
            hjust = 0.5, vjust = 0.5) +
  theme_bw() +
  coord_fixed() +
  scale_x_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
  scale_fill_manual(values = c("seagreen4", "goldenrod", "darksalmon",  "dodgerblue3")) +
  facet_wrap(Stressor~.)
