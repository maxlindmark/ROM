theme_rom <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = 12, base_family = "") +
    theme(
      axis.text = element_text(size = 8), 
      axis.title = element_text(size = 8),
      axis.ticks.length = unit(0.05, "cm"),
      axis.line = element_line(colour = "black",
                               size = 0.3), 
      text = element_text(family = "sans"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(hjust = 0.5, 
                                margin = margin(b = -3), 
                                size = 9.6, 
                                face = "bold"),
      legend.position = c(0.5, -0.25),
      legend.text = element_text(size = 8),
      legend.justification = "bottom", 
      legend.background = element_rect(fill = "transparent"), 
      legend.key = element_rect(fill = "transparent"),
      aspect.ratio = 1,
      plot.margin = unit(c(5.5, 5.5, 20, 5.5), 
                         "points")
      )
}


p <- ggplot(dat, aes(År, Ton, color = Sjö)) +
  geom_bar(data = subset(dat, Sjö == "Stora sjöarna"), 
           aes(x = År, y = Ton), stat = "identity", color = pal[1], fill = pal[1], 
           width = 0.6) +
  geom_line(data = dat, aes(År, Ton, color = Sjö, alpha = Sjö), 
            size = 1) + 
  geom_point(data = subset(dat, Sjö == "Fritidsfiske"), 
             aes(År, Ton, fill = Sjö), size = 2, color = pal[6]) +
  geom_errorbar(data = subset(dat, Sjö == "Fritidsfiske"), 
                aes(x = År, ymin = rec_minu, ymax = rec_plus), 
                show.legend = FALSE, width  = 1, color = pal[6]) +
  scale_color_manual(values = pal) +
  scale_alpha_manual(values = c(1, 1, 1, 1, 1, 0)) +
  labs(x = "", y = "Landningar (ton)") +
  ggtitle("Landningar") +
  guides(fill  = FALSE,
         alpha = FALSE,
         color = guide_legend(nrow = 3, 
                              title = "",
                              override.aes = list(size = 1.3, 
                                                  color = pal),
                              keywidth = 0.3,
                              keyheight = 0.1,
                              default.unit = "inch")) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  theme_rom() +
  NULL  

p
        
ggsave("fig_full2.tiff", plot = p, dpi = 300, width = 8, height = 8, units = "cm")

                        
                        
