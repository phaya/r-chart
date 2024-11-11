library(ggplot2)
library(here)

# Create a dataframe with simulated data
days <- 1:15 # Days since the outbreak began
initial_cases <- 5 # Initial number of cases
growth_rate <- 0.3 # Daily growth rate

cases <- initial_cases * exp(growth_rate * days) # Exponential growth formula

data <- data.frame(Day = days, Cases = cases)

# Primary palette 
# Background: 
bg_color <- "#3D3131"
# Highlight: 
hl_color <- "#F54242"
# Neutral: 
nt_color <- "#B8B2AD"

# Alternative palette
# Light background:
#bg_color <- "#F5E4E4"
# Dark highlight: 
#hl_color <- "#C91513"
# Neutral: 
#nt_color <- "#F5E4E4" 

ggplot(data, aes(x = Day, y = Cases)) +
  geom_line(color = hl_color, size = 2) + 
  geom_point(color = hl_color, size = 5) + 
  labs(
    title = "",
    x = "", 
    y = "", 
    subtitle = "", 
    caption = "" 
  ) +
  theme(
    plot.background = element_rect(fill = bg_color), 
    panel.background = element_rect(fill = bg_color),
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank()
  )

# Save the plot in landscape format
ggsave(here("output/minimalist_exponential_growth.png"), 
        width = 24, height = 12, dpi = 300, unit="cm", 
        bg = bg_color)