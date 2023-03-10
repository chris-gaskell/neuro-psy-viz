###############################################################
#libraries
###############################################################
#setwd("/Users/christophergaskell/OneDrive/neuro-psy-viz")
library(tidyverse)
library(patchwork)
library(showtext)
library(tibble)
library(ggtext)
library(rlang)
library(geomtextpath)
library(ggradar)
library(fmsb)
library(ggradar)
library(scales)

###############################################################
# data
###############################################################
wais_color  = "#6491E3"
wms_color  = "#00918B"

domain = c("Full Scale IQ", "Verbal Comprehension", "Perceptual Reasoning",
                 "Working Memory", "Processing Speed", "Immediate Memory", "Delayed Memory",
                 "Visual-Working Memory")


data <- data.frame(
  patient = rep(c(1:12), each = 8),
  index = rep(domain, 12),
  domain = rep(c("intelligence","intelligence","intelligence","intelligence","intelligence","memory", "memory", "memory"), 12),
  composite = sample(70:140, size = 96, replace = T)
)
data$index <- factor(data$index)


p <- data %>% #filter(patient == 1) %>%
  ggplot(aes(x = fct_inorder(index), y = composite, fill = domain)) +
    geom_col(color = "black") +
  scale_fill_manual(values = c(wais_color, wms_color)) +
  coord_polar() +
  theme_bw()




p + facet_wrap(~patient, nrow = 3) +
  theme(
    strip.text = element_text(
      face = "bold", color = "black",
      hjust = 0.5, size = 12
    ),
    strip.background = element_rect(#fill = "#985b10",
                                    linetype = "dotted")
  ) +
  scale_x_discrete(labels = function(x)
    stringr::str_wrap(x, width = 11))


# criticisms of spider plots https://www.data-to-viz.com/caveat/spider.html
# developing radar using datanovia
# pretty https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html
#




#p <-
data %>% filter(patient == 1) %>%
ggplot() +
  geom_hline(aes(yintercept = y),
    data.frame(y = c(80, 90, 110)),
    color = "black") +
  geom_segment(aes(x = reorder(str_wrap(index, 5), composite),
                   y = composite, xend = str_wrap(index, 5), yend = 130),
               linetype = "dashed", color = "gray12") +
  scale_fill_manual(values = c(wais_color, wms_color)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray12", size = 10),
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  annotate("rect", xmin = 9, xmax = 0, ymin = 90, ymax = 110, alpha = .1) +
  annotate("rect", xmin = 9, xmax = 0, ymin = 80, ymax = 90, alpha = .1, fill = "grey12") +
  annotate("rect", xmin = 9, xmax = 0, ymin = 0, ymax = 80, alpha = .3, fill = "grey12") +
  annotate(x = 0, y = 115, label = "Above Average", geom = "text", color = "gray12", size = 3) +
  annotate(x = 0, y = 100, label = "Average", geom = "text", color = "gray12", size = 3) +
  annotate(x = 0, y = 85, label = "Low Average", geom = "text", color = "gray12", size = 3) +
  annotate(x = 0, y = 75, label = "Borderline", geom = "text", color = "gray12", size = 3) +
geom_col(aes(x = reorder(str_wrap(index, 5), composite),
             y = composite,fill = domain),
         position = "dodge2", show.legend = TRUE, alpha = 1) +
  geomtextpath::coord_curvedpolar() +
  geom_point(aes(x = reorder(str_wrap(index, 3), composite),
                 y = composite,), size = 2, color = "gray12")

#+
  #theme_bw()



