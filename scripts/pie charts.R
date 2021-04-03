#packages needed
library(tidyverse)
library(ggforce)
library(patchwork)
library(ggpubr)
library(magick)

#import life cycle image from Su Kim
lc <- image_read("data/Bull kelp life cycle APR022021.png")

lc <- ggplot() +
  theme_void() +
  background_image(lc) +
  theme(plot.margin = unit(c(0,0,0,0), "lines")) +
  coord_fixed()

#import life cycle related data
dat <- read.csv("data/life cycle literature stats for figure 2.csv", header = TRUE)

#pivot data and change factor level order of lifestages to get in correct life-cycle order
dat <- dat %>%
  pivot_longer(cols = c("Adult","Zoospores","Gametophytes","Juvenile"),
               names_to = "lifestage",
               values_to = "papers") %>%
  mutate(lifestage = factor(lifestage, levels = c("Adult", "Zoospores", "Gametophytes", "Juvenile")))

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
# but 0.5 would place it exactly in the middle.
rot = pi * 1.1  #in order to keep adults in the left/upper portion of all the pies; larger values for "rot" rotate Adult's lower starting point clockwise


# draw the pies
pies1 <- ggplot(dat_pies %>%
         filter(papers != 0,
                Stressor %in% c("Algal competition", "Benthic sediment", "Contaminants"))) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle + rot, end = end_angle + rot, fill = lifestage), color = "white") +
  geom_text(aes(x = rlabel*sin(mid_angle + rot), y = rlabel*cos(mid_angle + rot), label = papers),
            hjust = 0.5, vjust = 0.5) +
  #geom_text(aes(label = Stressor), x = 0.5, y = 1, hjust = 1.5, vjust = 0.8) +
  theme_minimal() +
  theme(plot.margin = unit(c(0,0,0,0), "lines"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  coord_fixed() +
  scale_fill_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  facet_grid(Stressor ~ .)
pies1

pies2 <- ggplot(dat_pies %>%
                  filter(papers != 0,
                         Stressor %in% c("Currents", "Epiphytes", "Grazing", "Irradiance"))) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle + rot, end = end_angle + rot, fill = lifestage), color = "white") +
  geom_text(aes(x = rlabel*sin(mid_angle + rot), y = rlabel*cos(mid_angle + rot), label = papers),
            hjust = 0.5, vjust = 0.5) +
  #geom_text(aes(label = Stressor), x = 0.5, y = 1, hjust = 1.5, vjust = 0.8) +
  theme_minimal() +
  theme(plot.margin = unit(c(0,0,0,0), "lines"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  #coord_fixed() +
  scale_fill_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  facet_wrap(Stressor~., ncol = 1, nrow = 4)
pies2

pies3 <- ggplot(dat_pies %>%
                  filter(papers != 0,
                         Stressor %in% c("Light", "Mechanical damage", "Nutrients", "Salinity"))) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle + rot, end = end_angle + rot, fill = lifestage), color = "white") +
  geom_text(aes(x = rlabel*sin(mid_angle + rot), y = rlabel*cos(mid_angle + rot), label = papers),
            hjust = 0.5, vjust = 0.5) +
  #geom_text(aes(label = Stressor), x = 0.5, y = 1, hjust = 1.5, vjust = 0.8) +
  theme_minimal() +
  theme(plot.margin = unit(c(0,0,0,0), "lines"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  #coord_fixed() +
  scale_fill_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  facet_wrap(Stressor~., ncol = 1, nrow = 4)
pies3

pies4 <- ggplot(dat_pies %>%
                  filter(papers != 0,
                         Stressor %in% c("Temperature", "Water clarity", "Water/wave motion"))) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle + rot, end = end_angle + rot, fill = lifestage), color = "white") +
  geom_text(aes(x = rlabel*sin(mid_angle + rot), y = rlabel*cos(mid_angle + rot), label = papers),
            hjust = 0.5, vjust = 0.5) +
  #geom_text(aes(label = Stressor), x = 0.5, y = 1, hjust = 1.5, vjust = 0.8) +
  theme_minimal() +
  theme(plot.margin = unit(c(0,0,0,0), "lines"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  #coord_fixed() +
  scale_fill_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  facet_wrap(Stressor~., ncol = 1, nrow = 4)
pies4

png("Figure 2. Life cycle literature stats.png", width = 6.5, height = 4, units = "in", res = 300)
pies1 + pies2 + lc + pies3 + pies4 + plot_layout(nrow = 1)
dev.off()

png("Figure 2. Life cycle literature stats.png", width = 6.5, height = 4, units = "in", res = 300)
#pies1 + pies2 + lc + pies3 + pies4 + plot_layout(nrow = 1)
ggarrange(pies1, pies2, lc, pies3, pies4, nrow = 1)
dev.off()

