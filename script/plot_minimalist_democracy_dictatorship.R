library(tidyverse)
library(ggtext)
library(here)

democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

# Primary palette 
# Background: 
bg_color <- "#3D3131"
# Highlight: 
hl_color <- "#61926F"
# Complementary
cm_color <- "#F54242"
# Neutral: 
nt_color <- "#B8B2AD"
# Texto
txt_color <- "#B8B2AD"

democracy_data %>%
  filter(!is.na(is_democracy)) %>%
  group_by(year, is_democracy) %>%
  count() %>%
  group_by(is_democracy) %>%
  mutate(
    is_last = year == max(year), # Identificar el último punto
    value_label = if_else(is_last, as.character(n), NA), # Etiqueta con el valor del último punto
    hjust_value = if_else(is_democracy == "TRUE", -0.55, -1.10) # Ajuste de hjust por categoría
  ) %>% 
  ggplot(aes(x=year, y=n, color=is_democracy)) +
  geom_line(size=3) + 
  geom_point(
    data = . %>% filter(is_last), # Agregar puntos solo en los últimos valores
    size = 5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.2)) # Ampliar margen derecho del eje X
  ) +
  scale_color_manual(
    values = c("TRUE" = hl_color, "FALSE" = cm_color) 
  ) +
  geom_text(
    aes(label = value_label, hjust = hjust_value), 
    size = 4, 
    fontface  = "bold",
    na.rm = TRUE
  ) +
  labs(
    title = glue::glue("<span style='color:{hl_color};'>Democracy</span> vs. <span style='color:{cm_color};'>Dictactorship</span>"),
    x = "", 
    y = "", 
    subtitle = "Number of regime types per year", 
    caption = paste0("@pablohaya | Source: Bjørnskov and Rode (2019)") 
  ) +
  theme(
    plot.background = element_rect(fill = bg_color),
    plot.title = element_markdown(size = 16, color= txt_color, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color= txt_color, face = "italic"),
    plot.caption = element_text(color= nt_color),
    panel.background = element_rect(fill = bg_color),
    panel.border = element_blank(), 
    panel.grid = element_blank(),
    axis.text.x = element_text(color = txt_color, size=14),
    axis.text.y = element_blank(), 
    axis.ticks.length = unit(8, "pt"),
    axis.ticks.x = element_line(color = nt_color, size=1.5),
    legend.position = "none"
  )

# Save the plot in-feed instagram format
ggsave(here("output/minimalist_democracy_dictatorship.png"), 
        width = 1080, height = 1350, unit="px", 
        bg = bg_color)

# Save the plot in-feed square instagram format
ggsave(here("output/minimalist_democracy_dictatorship_square.png"), 
       width = 1080, height = 1080, unit="px", 
       bg = bg_color)

# Instagram in-feed
# 1080 x 1350 pixeles (4:5)
# Instagram 
# 1080x1920 (9:16)