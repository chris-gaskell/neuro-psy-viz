library(tidyverse)
library(patchwork)
library(showtext)
library(tibble)
library(ggtext)

font_add_google("Special Elite", family = "special")
primary_color = "#6491E3"
font = "special"
main.title.size = 28
subtitle.size = 14
plot.text.size = 12
axis.text = 10.5
background_color = "#E4F0FF"
# colors, dark = #6491E3, light = #DCF7FE  also  #E4F0FF


# data. taken from http://images.pearsonclinical.com/images/Products/WAIS-IV/WAIS-IV_Writer_Report_Handout.pdf
scaled_scores <-
  tribble(
    ~subtest.key,   ~subtest.name,          ~type,      ~domain.key,    ~domain.name,        ~age.norms, ~raw, ~scaled.score,   ~descriptor,         ~rank, 
    "BD",           "Block Design",        "core",         "PRI",  "Perceptual Reasoning",     "16-17",  38,     9,               "Average ",         "47",
    "SI",           "Similarities",        "core",         "VCI",  "Verbal Comprehension",     "16-17",  30,     12,              "High Average ",    "82",  
    "DS",           "Digit Span",          "core",         "WMI",  "Working Memory",           "16-17",  34,     13,              "High Average ",    "90",
    "MR",           "Matrix Reasoning",    "core",         "PRI",  "Perceptual Reasoning",     "16-17",  12,     7,               "Low Average ",     "23",
    "VC",           "Vocabulary",          "core",         "VCI",  "Verbal Comprehension",     "16-17",  49,     13,              "High Average ",    "90", 
    "AR",           "Arithmetic",          "core",         "WMI",  "Working Memory",           "16-17",  17,     12,              "High Average ",    "82",  
    "SS",           "Symbol Search",       "core",         "PSI",  "Processing Speed",         "16-17",  29,     9,               "Average ",         "47",
    "VP",           "Visual Puzzles",      "core",         "PRI",  "Perceptual Reasoning",     "16-17",  12,     8,               "Average ",         "34",
    "IN",           "Information",         "core",         "VCI",  "Verbal Comprehension",     "16-17",  18,     12,              "High Average ",     "82", 
    "CD",           "Coding",              "core",         "PSI",  "Processing Speed",         "16-17",  52,     7,               "Low Average ",      "23",
    #"LN",           "Letter-Number",       "supplementary","WMI",  "Working Memory",           "16-17",  NA,    NA,
    #"FW",           "Figure Weights",      "supplementary","PRI",  "Perceptual Reasoning",     "16-17",  NA,    NA,
    #"CO",           "Comprehension",       "supplementary","VCI",  "Verbal Comprehension",     "16-17",  NA,    NA,
    #"CA",           "Cancellation",        "supplementary","PSI",  "Processing Speed",         "16-17",  NA,    NA,
    #"PCm",          "Picture Completion",  "supplementary","PRI",  "Perceptual Reasoning",     "16-17",  NA,    NA
  )

composite.scores <-
  tribble(
    ~domain.key,    ~domain.name, ~completed.tests, ~sum.scaled, ~composite, ~centile,   ~`95ci`, ~rank,
    "PRI",     "Perceptual Reasoning",  3,              24,          88,        "21",  "82-95",     "21",
    "VCI",     "Verbal Comprehension",  3,              37,          112,       "79",  "106-117",   "79",
    "WMI",     "Working Memory",        2,              25,          114,       "82",  "106-120",   "82",
    "PSI",     "Processing Speed",      2,              16,          89,        "23",   "82-98",    "23",
    "FSIQ",    "Full Scale IQ",         10,             102,         101,       "53",   "97-105",   "53"
  )



scaled_scores <- 
  scaled_scores %>% 
  mutate(rank = as.numeric(rank))
composite.scores <- 
  composite.scores %>% 
  mutate(rank = as.numeric(rank))


# scaled score plot
plot.scaled.scores <-
  scaled_scores %>%
  ggplot(aes(
    x = fct_inorder(subtest.key),
    y = scaled.score,
    group = domain.name,
    color = descriptor)) +
  geom_point(size = 2.5) +
  geom_line(color = primary_color) +
  facet_grid(~ factor(
    domain.name, levels = c("Perceptual Reasoning", "Verbal Comprehension",
                            "Working Memory", "Processing Speed")),
    scales = "free_x",
    space = "free_x",
    labeller = label_wrap_gen(width = 12, multi_line = TRUE)) +
  scale_y_continuous(breaks=seq(0,20,2), limits = c(0,20))



plot.scaled.scores <-
  plot.scaled.scores +
  labs(title = "An **Intelligent** Visualisation",
       subtitle = "A simple scatter plot to show the scaled and composite scores of the <span style = 'color:#6491E3;'>**WAIS-IV**</span>.",
       x = "Subtest",
       y = "Scaled Score") +
  theme(plot.title.position = "plot")



showtext::showtext_auto()

plot.scaled.scores <-
  plot.scaled.scores +
  theme(
    plot.title = element_textbox_simple(
      hjust = 0, box.color = primary_color,
      fill = primary_color, size = main.title.size,
      colour = "white", padding = margin(5, 0, 5, 0)),
    plot.subtitle = element_textbox_simple(
      width = grid::unit(7, "in"), family = font,
      hjust = 0, size = subtitle.size,
      padding = margin(3, 0, 3, 0)),
    plot.background = element_rect(fill = background_color),
    strip.background =element_rect(fill= primary_color),
    strip.text = element_text(colour = 'white', face = "bold",
                              size = 12, family = font),
    axis.line = element_line(colour = primary_color, size = 1,
                             linetype = "solid"),
    axis.text = element_text(family = font,size = axis.text),
    axis.title = element_text(family = font,
                              size = plot.text.size, face = "bold"),
  )


plot.scaled.scores <- 
  plot.scaled.scores +
  theme(legend.background = element_rect(fill = "transparent"),
        #legend.background = element_rect(fill = primary_color),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(family = font,size = 13),
        legend.position = "top") +
  labs(color = "")



# composite score plot

plot.composite.scores <-
  composite.scores %>%
  mutate(facet = "Domain Scores") %>%
  separate(`95ci`, c("lci", "uci"), sep = "-", convert = TRUE) %>%
  ggplot(aes(x = fct_inorder(domain.key),
             y = rank, group = 1)) +
  geom_point(color = primary_color, size = 5, shape = 4, stroke = 1.5) +
  geom_line(linetype = "dashed") +
  #geom_errorbar(aes(ymin=lci, ymax=uci), width=.2,
  #              position=position_dodge(.9)) + #jtools::theme_apa() +
  scale_y_continuous(breaks=seq(0,100, 10), limits = c(0,100)) +
  facet_grid(. ~ facet) +
  xlab("Domain") + ylab ("Percentile Rank")




plot.composite.scores +
  theme(plot.background = element_rect(fill = background_color),
        strip.background = element_rect(fill= primary_color),
        strip.text = element_text(colour = 'white',
                                  face = "bold", size = 12, family = font),
        axis.line = element_line(colour = primary_color,
                                 size = 1, linetype = "solid"),
        axis.text = element_text(family = font,size = axis.text),
        axis.title = element_text(family = font, size = plot.text.size, face = "bold"))

# Bring together
plot.combined <-
  plot.scaled.scores / plot_spacer() / plot.composite.scores &
  theme(plot.background = element_rect(fill = "#E4F0FF",
                                       colour = "#E4F0FF"))

# Change spacing
plot.combined <-
  plot.combined +
  plot_layout(heights = c(40, 0.1, 40))

# Re-theme
plot.combined <-
  plot.combined +
  theme(plot.background = element_rect(fill = background_color),
        strip.background = element_rect(fill= primary_color),
        strip.text = element_text(colour = 'white', face = "bold", size = 12, family = font),
        axis.line = element_line(colour = primary_color, size = 1, linetype = "solid"),
        axis.text = element_text(family = font,size = axis.text),
        legend.text = element_text(family = font,size = axis.text),
        axis.title = element_text(family = font, size = plot.text.size, face = "bold"))

# titles
plot.combined <-
  plot.combined +
  plot_annotation(
    caption =
      bquote(~bold("Source:")~"Pearson publically available report: http://images.pearsonclinical.com/images/Products/WAIS-IV;"
             ~bold("Plot by:")~"@cgaskell92"))


plot.combined



ggsave("1. scatter_plots/1.2_scatter-plot.png", plot = plot.combined, width = 20, height = 25, units = "cm")






