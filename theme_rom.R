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
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.background = element_rect(fill = "transparent"), 
      legend.key = element_rect(fill = "transparent"),
      legend.box.margin = margin(-25,-25,-25,-25), 
      aspect.ratio = 1,
      plot.margin = unit(c(5.5, 5.5, 20, 5.5), 
                         "points")
    )
}