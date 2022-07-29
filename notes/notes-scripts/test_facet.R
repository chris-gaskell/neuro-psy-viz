
wais_domains <- x$composites.scores
wais_domains$test <- "wais"

wms_domains <- x$composites.scores
wms_domains$test <- "wms"

wms_domains <- wms_domains %>% mutate(
  domain.name = case_when(
    domain.key == "PRI" ~ "Auditory Memory Index",
    domain.key == "VCI" ~ "Visual Memory Index",
    domain.key == "WMI" ~ "Immediate Memory Index",
    domain.key == "PSI" ~ "Delayed Memory Index",
    )
)

combined_dat <- rbind(wais_domains, wms_domains)


combined_dat %>%
  #mutate(facet = "Composite Scores") %>%
  #separate(`95ci`, c("lci", "uci"), sep = "-", convert = TRUE) %>%
  ggplot(aes(x = domain.name,
             y = composite, group = 1)) +
  geom_point(color = "blue", size = 5, shape = 4, stroke = 1.5) +
  geom_line(linetype = "dashed") +
  #geom_hline(yintercept = comp.dat[5,3], color = primary_color, linetype = "dotted") +
  #geom_errorbar(aes(ymin=lci, ymax=uci), width=.2,
  #              position=position_dodge(.9)) + #jtools::theme_apa() +
  #scale_y_continuous(breaks=seq(40,150, 20), limits = c(50,140)) +
  facet_grid(. ~ test) +
  theme(strip.background = element_rect(fill= primary_color),
        strip.text = element_text(colour = 'white', face = "bold", size = 12),
        axis.line = element_line(colour = primary_color,
                                 size = 1, linetype = "solid")) +
  xlab("Domain") + ylab ("Composite Score")
