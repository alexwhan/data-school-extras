library(tidyverse)

curve1 <- dnorm(seq(from = -5, to = 20, length.out = 200))
curve2 <- dnorm(seq(from = -5, to = 20, length.out = 200), sd = 1.5, mean = 1.25)
post_effect <- -curve2
post_effect[1:which.max(curve2)] <- 0
post_effect[(which.max(curve2) + 1):length(post_effect)] <- post_effect[(which.max(curve2) + 1):length(post_effect)] +
  curve2[which.max(curve2)]
curve_tbl <- tibble(time = 1:200,
               curve1 = curve1,
               curve2 = 3 * curve2 - 1 * post_effect) %>% 
  gather(type, energy, -time) %>% 
  mutate(type = factor(type, levels = sort(unique(type), decreasing = TRUE), labels = c("Two-day course", "Data School"), ordered = TRUE))

poly_tbl <- curve_tbl %>% 
  mutate(energy = case_when(
    type == "curve1" ~ energy,
    energy > 0 ~ energy,
    TRUE ~ 0
  ))

curves_p <- curve_tbl %>% 
  ggplot(aes(time, energy, colour = type)) +
  geom_line(data = filter(curve_tbl, type == "Two-day course"), size = 3, show.legend = FALSE) +
  geom_polygon(data = filter(poly_tbl, type == "Two-day course"), aes(fill = type), alpha = 0, colour = NA) +
  geom_line(data = filter(curve_tbl, type == "Data School"), size = 3, show.legend = FALSE) +
  geom_polygon(data = filter(poly_tbl, type == "Data School"), aes(fill = type), alpha = 0, colour = NA) +
  scale_colour_brewer(palette = "Set1", labels = c("Two-day course", "Data School")) +
  scale_fill_brewer(palette = "Set1", labels = c("Two-day course", "Data School")) +
  guides(colour = guide_legend(title.position = NULL,
                             keywidth = unit(1, "cm"),
                             keyheight = unit(1, "cm"),
                             override.aes = list(shape = 22,
                                                 size = 10,
                                                 alpha = 1))) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("figures/data-school-energy-curve.png", curves_p, width = 12, height = 10)

curves_fill_p <- curve_tbl %>% 
  ggplot(aes(time, energy, colour = type)) +
  geom_line(data = filter(curve_tbl, type == "Two-day course"), size = 3, show.legend = FALSE) +
  geom_polygon(data = filter(poly_tbl, type == "Two-day course"), aes(fill = type), alpha = 0.5, colour = NA) +
  geom_line(data = filter(curve_tbl, type == "Data School"), size = 3, show.legend = FALSE) +
  geom_polygon(data = filter(poly_tbl, type == "Data School"), aes(fill = type), alpha = 0.5, colour = NA) +
  scale_colour_brewer(palette = "Set1", labels = c("Two-day course", "Data School")) +
  scale_fill_brewer(palette = "Set1", labels = c("Two-day course", "Data School")) +
  guides(colour = guide_legend(title.position = NULL,
                               keywidth = unit(1, "cm"),
                               keyheight = unit(1, "cm"),
                               override.aes = list(shape = 22,
                                                   size = 10,
                                                   alpha = 1))) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("figures/data-school-energy-curve-filled.png", curves_fill_p, width = 12, height = 10)
