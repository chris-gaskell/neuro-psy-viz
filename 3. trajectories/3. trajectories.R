###############################################################
#libraries
###############################################################
#setwd("/Users/christophergaskell/OneDrive/neuro-psy-viz")
library(tidyverse)
library(patchwork)
library(showtext)
library(tibble)
library(showtext)
library(ggtext)
library(rlang)

###############################################################
# data
###############################################################

dat <-
  data.frame(
                    Time =   c(0,    1,    1.9,   2,    2.4,   3,     4,     5,   6,    6.1,   7,    8,   9,    10,   11,   12),
      "Normal development" = c(0,    1,    1.9,   2,    2.4,   3,     4,     5,   6,    6.1,    7,    8,   9,    10,   11,   12),
        "Natural recovery" = c(NA,  NA,    1.9,  0.5,   1.3,  1.75,  2.5, 3.25,  3.48, 3.5,  3.75,  4,   4.2,  4.4, 4.6, 4.8),
     "Appropriate support" = c(NA,  NA,    NA,   NA,    NA,   NA,    NA,   NA,   3.48, 4.7,  5.5,   5.9, 6.4,  6.9, 7.5, 7.9)
) %>%
  pivot_longer(cols = 2:4, names_to = "Trajectory")

###############################################################
# presets
###############################################################

background_color  = "white"
font_add_google("Special Elite", family = "special")
stroke = 1.4
text.size = 12
font = "special"
main.title.size = 28
subtitle.size = 18
plot.text.size = 12
axis.text = 10.5
showtext::showtext_auto()

###############################################################
# Plot 1: Mapping data
###############################################################

plot.base <-
  ggplot(subset(dat, Trajectory %in% c("Normal.development", "Natural.recovery")),
         aes(x = Time, y = value, color = Trajectory, label = Trajectory)) +
  geom_labelpath(fill = "#F6F6FF", size = 4, linewidth = 1, boxlinewidth = 0.3,
                 arrow = arrow(length = unit(0.40,"cm"))) +
  scale_colour_manual(values = c("tomato4", "deepskyblue4")) +
  theme_minimal() +
  theme(legend.position = "none")  +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                       ends = "last")),
        axis.text.y = element_blank(),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                       ends = "last")),
        axis.text.x = element_blank()
  )


###############################################################
# Plot 1: Mapping data
###############################################################

plot.1 <-
  plot.base +
  geom_label(aes(x = 1.1,
                 y = 3.5,
                 label = "Point of injury"),
             fill = "white", color = "tomato4") +
  annotate("segment", x = 0.7, xend = 1.6, y = 3, yend = 2.2,
           colour = "tomato4", size=1, alpha=0.7, arrow=arrow()) +
  labs(subtitle = str_wrap("Following a brain injury there may be a drop in a young person's cognitive ability.
                           For some, early sudden gains in the acute recovery stage will bring cognition back to
                           a near preinjury level.",
                           width = 42)) +
  theme(plot.subtitle = element_text(size = subtitle.size),
        axis.title = element_text(size = 15))


plot.2 <-
  plot.base +
  annotate("segment", x = 2.325, xend = 2.5, y = 2.0, yend = 1.5,
             color = "grey40", alpha = 0.9, size = 0.81, #linetype = "dashed",
             arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", x = 5.9, xend = 6.2, y = 5.1, yend = 4.2,
           color = "grey40", alpha = 0.9, size = 0.81, #linetype = "dashed",
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", x = 10.25, xend = 11.5, y = 9.7, yend = 5, size = .8,
           color = "grey40", alpha = 0.9, size = 0.81, #linetype = "dashed",
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  labs(subtitle = str_wrap("For some young people, difficulties can become more pronounced over time as
                            the learning of skills typically learnt in later adolescence proves more
                            difficult to aquire ('growing into' difficulties).",
                           42)) +
    theme(plot.subtitle = element_text(size = subtitle.size),
          axis.title = element_text(size = 15))




###############################################################
# Plot 2: Mapping data
###############################################################

plot.3 <-
  ggplot(dat, aes(x = Time, y = value, color = Trajectory, label = Trajectory)) +
    geom_labelpath(fill = "#F6F6FF", size = 4, linewidth = 1, boxlinewidth = 0.3,
                   arrow = arrow(length = unit(0.40,"cm"))) +
    scale_colour_manual(values = c("forestgreen", "tomato4", "deepskyblue4")) +
    theme_minimal() +
    theme(legend.position = "none")  +
    theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "last")),
          axis.text.y = element_blank(),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "last")),
          axis.text.x = element_blank()
          ) +
  labs(subtitle = str_wrap("While the rate of development may pleateau over time,
                            access to appropriate and individually tailored rehabiliation can provide
                            the greatest opportunity for optimal rates of growth.", 42)) +
  theme(plot.subtitle = element_text(size = subtitle.size),
        axis.title = element_text(size = 15))


finished.plot <- plot.1 + plot.2 + plot.3 +
  plot_annotation(title = 'Recovery from Pediatric Brain Injury',
                   caption = bquote(
                     ~bold('Source:')~'Fictitous Data;   '
                     ~bold('Plot by:')~'@cgaskell92'),
                  theme = theme(plot.title = element_text(size = main.title.size, face = 'bold'),
                                plot.caption = element_text(size = 15),
                                axis.title = element_text(size = 15))) &
  theme(text = element_text('mono')) &
  xlab("Time") & ylab("Cognitive Ability & Function")


ggsave("/Users/christophergaskell/OneDrive/neuro-psy-viz/3. trajectories/plot.png",
       plot = finished.plot, width = 50, height =18, units = "cm")

