library(tidyverse)
library(here)

democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')

# Primary palette 
# Background: 
bg_color <- "#3D3131"
# Highlight: 
hl_color <- "#F54242"
# Complementary
cm_color <- "#42F575"
# Neutral: 
nt_color <- "#B8B2AD"

#60A072


democracy_data %>%
  filter(!is.na(is_democracy)) %>%
  group_by(year, is_democracy) %>%
  count() %>%
  group_by(is_democracy) %>%
  mutate(
    is_last = year == max(year), # Identificar el último punto
    label = if_else(is_last, 
                    if_else(is_democracy, "Democracias", "Dictaduras"), 
                    NA),
    value_label = if_else(is_last, as.character(n), NA), # Etiqueta con el valor del último punto
    vjust_value = if_else(is_democracy, 1.8, -1) # Ajustar posición vertical para cada tipo
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
    values = c("TRUE" = hl_color, "FALSE" = nt_color),
    labels = c("TRUE" = "Democracias", "FALSE" = "Dictaduras") 
  ) +
  geom_text(
    aes(label = value_label, vjust = vjust_value), # Ajustar posición vertical de los valores
    size = 4, # Tamaño del texto de los valores
    na.rm = TRUE
  ) +
  geom_text(
    aes(label = label),
    hjust = -0.2, # Ajuste de posición horizontal
    size = 5, # Tamaño del texto
    na.rm = TRUE # Ignorar valores faltantes (sin etiquetas)
  ) +
  labs(
    title = "AAAA",
    x = "", 
    y = "", 
    subtitle = "", 
    caption = paste0("&#xe61b","@pablohaya") 
  ) +
  theme(
    plot.background = element_rect(fill = bg_color), 
    plot.caption = element_text(color= bg_color),
    panel.background = element_rect(fill = bg_color),
    panel.border = element_blank(), 
    panel.grid = element_blank(),
    axis.text.x = element_text(color = nt_color, size=14),
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "none"
  )

# Save the plot in landscape format
ggsave(here("output/minimalist_democracy_dictatorship.png"), 
        width = 24, height = 12, dpi = 300, unit="cm", 
        bg = bg_color)
