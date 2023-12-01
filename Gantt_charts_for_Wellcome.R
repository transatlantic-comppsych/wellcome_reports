library(ggplot2)
library("ggpubr")
df <- read.csv("C:/Users/Elena/OneDrive - University College London/Desktop/Surprise study/Surprise_pilots_dates_and_platforms.csv")
df$duration <- as.numeric(df$duration) 
df$start_dates <- as.Date(df$start_dates)

# Calculate end dates
df$end_dates <- df$start_dates + df$duration

# Define colors for each platform
platform_colors <- c("Prolific " = "darkblue", "Testable Minds " = "#529EFF", "Amazon MTurk " = "darkorange" )

# Create Gantt chart with colors and legend
ggplot(df, aes(x = start_dates, xend = end_dates, y = tasks, yend = tasks, color = platforms)) +
  geom_segment(size = 7) +
  geom_text(aes(x = start_dates + duration/2, y = tasks, label = paste("n=", participants)), 
            position = position_nudge(y = 0), 
            size = 4, color = "white") +
  scale_color_manual(values = platform_colors) +  # Assign colors based on platforms
  labs(title = "Surprise Task Recruitment, Total n = 533", x = "Timeline", y = "Tasks", color = "Platforms") +
  theme_minimal() +  # You can choose different themes as per your preference
  guides(color = guide_legend(title = "Platforms")) # Legend title 
