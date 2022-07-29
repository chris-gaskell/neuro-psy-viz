###############################################################
#libraries
###############################################################
setwd("/Users/chris/Desktop/neuro-psy-viz/2. premorbid discepancies")
library(tidyverse)
library(patchwork)
library(showtext)
library(tibble)
library(showtext)
library(ggtext)
library(rlang)
source(file = "/Users/chris/Desktop/neuro-psy-viz/2. premorbid discepancies/funs.R")

###############################################################
# data
###############################################################

# Taken from https://www.pearsonclinical.com.au/filemanager/uploads/Webinar%20Files/TOPF%20Webinar%20Handout.pdf

test.scores <-
  tribble(
  ~domain.key,    ~domain.name,            ~test,        ~premorbid,           ~actual,    ~predicted,     ~difference,  ~critical,   ~sig.diff,   ~base.rate,
  "FSIQ",    "Full Scale IQ",           "Intelligence",     "Pre-morbid",         103,        116,             -13,       5.31,       "Y",       11.4,
  "VCI",     "Verbal Comprehension",    "Intelligence",     "Pre-morbid",         110,        115,             -5,        6.14,       "N",       NA,
  "PRI",     "Perceptual Reasoning",    "Intelligence",     "Pre-morbid",         104,        112,             -8,        7.06,       "Y",       26.9,
  "WMI",     "Working Memory",          "Intelligence",     "Pre-morbid",         105,        114,             -9,        7.77,       "Y",       25.7,
  "PSI",     "Processing Speed",        "Intelligence",     "Pre-morbid",         86,         107,             -21,       9.06,       "Y",       5.3,
  "IMI",     "Immediate Memory",        "Memory",           "Pre-morbid",         86,         106,             -20,       7.52,       "Y",       6.4,
  "DMI",     "Delayed Memory",          "Memory",           "Pre-morbid",         82,         104,             -22,       7.45,       "Y",       5.2,
  "VWMI",    "Visual-Working Memory",   "Memory",           "Pre-morbid",         97,         110,             -13,       8.11,       "Y",       18.0
  ) # confidence intervals completely made up

###############################################################
# presets
###############################################################

wais_color  = "#6491E3"
wms_color  = "#00918B" # dark #00918B # mid #33B7B5
topf_color  = "#781D7C" # dark #781D7C # light #B28DB9 # mid #92579B
background_color = "#FFE7FF" #FFE7FF #B6A6B5 #FFABFF
font_add_google("Special Elite", family = "special")
domains = c("PRI", "VCI", "WMI", "PSI", "FSIQ", "IMI", "DMI", "VWMI")
stroke = 1.4
text.size = 12
font = "special"
main.title.size = 28
subtitle.size = 14
plot.text.size = 12
axis.text = 10.5
showtext::showtext_auto()


###############################################################
# Mapping data
###############################################################

plot <-
  test.scores %>%
    mutate(domain.key = factor(domain.key, levels = domains)) %>%
    ggplot(aes(x = domain.key, y = predicted, group = 1)) +
    geom_point(aes(col = premorbid),
            size = 3, shape = 16, stroke = stroke) +
    geom_point(aes(y = actual, group = 1, color = factor(test)),
             size = 4, shape = 3, stroke = stroke) +
    geom_errorbar(aes(ymin=predicted-critical , ymax=predicted+critical),
             width=.3, size = .3, col = topf_color) +
    scale_y_continuous(breaks=seq(40,150, 20), limits = c(50,140)) +
    facet_grid(. ~ test, scales = "free", space = "free") +
    scale_color_manual(values = c(wais_color, wms_color, topf_color))



###############################################################
# labs
###############################################################

plot <-
plot +
  labs(title = "Looking at **discrepencies**",
       subtitle = "A Simle scatter plot to show the scaled and composite scores of the <span style = 'color:#6491E3;'>**WAIS-IV**</span>. Composite score error bars are produced using confidence intervals.",
       x = NULL,
       y = "Composite Score",
       color = NULL) +
  theme(plot.title.position = "plot") +
  guides(color = guide_legend(
    override.aes = list(linetype = c(wais_color, wms_color, topf_color),
                        shape = c(3, 3, 16))))

###############################################################
# background color
###############################################################

plot <-
  plot +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = background_color),
        legend.text = element_text(size = text.size),
        plot.background = element_rect(fill = background_color),
        panel.background = element_rect(fill = background_color,
                                        color = topf_color, size = 0.5)
)



###############################################################
# extra colors
###############################################################

plot <-
  plot +
  theme(
    plot.title = element_textbox_simple(
      hjust = 0, box.color = topf_color,
      fill = topf_color, size = main.title.size,
      colour = "white", padding = margin(5, 0, 5, 0)),
    plot.subtitle = element_textbox_simple(
      width = grid::unit(7, "in"), family = font,
      hjust = 0, size = subtitle.size,
      padding = margin(3, 0, 3, 0)),
    strip.text = element_text(colour = 'white', face = "bold",
                              size = 12, family = font),
    axis.line = element_line(colour = topf_color, size = 1,
                             linetype = "solid"),
    axis.text = element_text(family = font,size = axis.text),
    axis.title = element_text(family = font,
                              size = plot.text.size, face = "bold"),
  )





plot <-
plot +
  theme(
    panel.grid.major = element_line(color = "gray70", size = .1),
    panel.grid.minor = element_line(color = "gray70", size = .05),
    plot.margin = unit(c(0.1, 0.5, .1, 0.5), "cm"),
    #axis.text.x = element_blank(),
    #axis.ticks = element_blank(),
    strip.text = element_text(
        size = 14, face = "bold",
        color = "white"),
    strip.background = element_rect(
      fill = topf_color, color = "white"
    # strip.text = element_textbox_highlight(
    #   size = 14, face = "bold",
    #   fill = wais_color, box.color = "white",
    #   color = "white",
    #   halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1.5, "npc"),
    #   height = unit(1, "cm"),
    #  padding = margin(4, 0, 4, 0),
    #  hi.labels = "Memory", #hi.family = "Bangers",
    #  hi.fill = wms_color, hi.box.col = "white", hi.col = "white"
    )
)




p2 <-
test.scores %>%
  mutate(domain.key = factor(domain.key, levels = domains)) %>%
  select(domain.key, difference, base.rate, test) %>%
  pivot_longer(c(difference, base.rate),
               names_to = "layer", values_to = "label") %>%
  ggplot(aes(x = domain.key,
             y = factor(layer, c("difference", "base.rate")))) +
  geom_tile(fill = background_color, alpha = .4, color = "black") +
  geom_text(aes(label = label), size = 5, family = font) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  labs(y = "", x = "Domain") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = background_color),
        axis.line = element_blank(),
        panel.grid = element_blank(), strip.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
        ) +
  facet_grid(. ~ test, scales = "free", space = "free") +
  theme(axis.text.y = element_blank()
        #panel.border = element_rect(color = background_color)
)




plot.combined <-
plot / p2 +  plot_layout(heights = c(8, 1.5))


plot.combined <-
  plot.combined +
  theme(plot.background = element_rect(fill = background_color,
                                       colour = background_color),

        axis.text = element_text(family = font,size = axis.text),
        axis.title = element_text(family = font,
                                  size = plot.text.size, face = "bold")
        #panel.background = element_rect(fill = background_color,
        #                                color = topf_color, size = 0.5
        ) +
  labs(caption =
        bquote(~bold("Source:")~"Pearson publically available report: http://images.pearsonclinical.com/images/Products/WAIS-IV;"
               ~bold("Plot by:")~"@cgaskell92"))




ggsave("2. premorbid discepencies/2_discrpencies-plot.png", plot = plot, width = 20, height = 25, units = "cm")
