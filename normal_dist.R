ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm) + geom_vline(
    xintercept = c(-3, -2, -1, 1, 2, 3),
    #data.frame(x = c(8, 9, 11)),
    color = "black"
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    #axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12),
    # Move the legend to the bottom
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.background = element_blank(),
  ) +
  annotate("rect", xmin = -1, xmax = 1, ymin = 0.5, ymax = 0,
           alpha = .8)

plot_norm(mean = 3.39, sd = 0.55,
          shadeValues = c(3, 3.5),
          direction = "between",
          col.shade = "forestgreen")







# credit to: https://sebastiansauer.github.io/shade_Normal_curve/

mean.1 <-0
sd.1 <- 1
zstart <- 2
zend <- 3
zcritical <- 1.65
my_col <- "#00998a"


x <- seq(from = mean.1 - 3*sd.1, to = mean.1 + 3*sd.1, by = .01)
my.df <- data.frame(x = x, y = dnorm(x, mean = mean.1, sd = sd.1))

shade_curve <- function(my.df, zstart, zend, fill = "red", alpha = .5){
  geom_area(data = subset(my.df, x >= mean.1 + zstart*sd.1
                          & x < mean.1 + zend*sd.1),
            aes(y=y), fill = fill, color = NA, alpha = alpha)
}


p1a <- ggplot(my.df, aes(x = x, y = y)) + geom_line() +
  shade_curve(my.df = my.df, zstart = -1, zend = 1, fill = my_col, alpha = .3) +
  shade_curve(my.df = my.df, zstart = 1, zend = 2, fill = my_col, alpha = .5) +
  shade_curve(my.df = my.df, zstart = -2, zend = -1, fill = my_col, alpha = .5) +
  shade_curve(my.df = my.df, zstart = 2, zend = 6, fill = my_col, alpha = .7) +
  shade_curve(my.df = my.df, zstart = -3, zend = -2, fill = my_col, alpha = .7) +
  scale_x_continuous(breaks = -3:3) +
  scale_y_continuous(breaks = NULL) +
  theme_classic() +
  ylab("") + xlab("")

p1a



my.df %>%
  mutate(y_cdf = cumsum(y)) -> my.df

my.df %>%
  filter(x %in% c(-3, -2.58, -2, -1.65, -1, -.5, 0, .5, 1, 1.65, 2, 2.58, 3)) -> my.df_filtered

p1a + geom_text(data = my.df_filtered,
                aes(x = x, y = -0.05, label = paste(round(y_cdf, 0),"%")),
                check_overlap = TRUE) +
  geom_segment(data = my.df_filtered,
               aes(x = x, xend = x, y = 0, yend = y), linetype = "dashed")




