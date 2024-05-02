# Exemplary-Code-Chunk--Analyst-Profile

This repository highlights a chunk of code I am proud of. The code replicates a column plot produced by FiveThirtyEight showing the percentage chance of Democrats and Republicans winning senate seats in the 2022 midterms. The image was replicated for an assignment in SIS-750: Data Analysis, a course at the School of International Service, American University. The FiveThirtyEight image is available in this respository along with the R Script I used to replicate the image. I used ggplot2 to create my replication. The following is the code I wrote.

#+ Exemplary Code Chunk- Analyst Profile
#+ 1 May 2024

# Load packages
library(tidyverse)
library(scales)
library(haven)

# Load data
senate = read.csv('senate_seat_distribution_2022.csv')

# Replicate Senate projection column plot ----------------
senate2 = 
  senate |>
  mutate( # Creates new variable containing the text of a color depending on senate seat party affiliation
    mycolor = if_else(seatsheld < 50, 'indianred2', if_else( 
      seatsheld > 51, '#8b97c6', '#5668ae', missing = NULL
    ), missing = NULL)
  ) |>
  filter(expression == '_deluxe', !is.na(seatprob_Dparty)) # Filters for FiveThirtyEight's deluxe model

border = c(NA, NA, NA, NA, NA, NA, NA, NA, 'black', 'black', NA, NA, NA, NA, NA, NA, NA, NA, NA) # Creates an array which will be used to create a border on center columns

p = 
  senate2 |>
  filter(expression == '_deluxe', !is.na(seatprob_Dparty)) |> # Filters for FiveThirtyEight's deluxe model
    ggplot(aes(y = seatprob_Dparty, x = seatsheld)) + # Creates plot of % chance of winning a senate seat by seats held
    geom_col(fill = senate2$mycolor, # Adds columns
             alpha = if_else(senate2$mycolor == '#5668ae', 1, 3/4, missing = NULL), 
             color = border, size = 0.25,
             width = 0.91) +
    labs( # Adds labels
      x = element_blank(),
      y = element_blank(),
      title = 'How many Senate seats we expect each party to win',
      subtitle = "Party seat counts based on who wins the Senate in our Deluxe model's 40,000 simultations",
      caption = 'FIVETHIRTYEIGHT DELUXE FORECAST, NOV. 8, 2022, 1 P.M. ET'
    ) +
    scale_y_continuous( # Adjusts y-axis values
      labels = c('0', '5', '10', '15%', ''),
      expand = c(0, 0)
    ) + 
    scale_x_continuous( # Adjusts x-axis values
      limits = c(40, 62),
      breaks = c(40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62),
      labels = c('60', '58', '56', '54', '52', '50', '52', '54', '56', '58', '60', '62'),
    )

p2 =      
p + theme( # Adds a custom theme
  plot.title = element_text(hjust = 0.2, face = 'bold', size = 12),
  plot.subtitle = element_text(hjust = 1, size = 8.5, margin = margin(0, 0, 50, 0)), 
  plot.caption = element_text(hjust = 1.05, size = 6, color = 'gray35'),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color = 'gray', linewidth = 0.25),
  panel.grid.major.x = element_blank(),
  axis.line.x.bottom = element_line(color = 'black', linewidth = 0.5),
  axis.text.x = element_text(size = 8, 
      color = c('indianred2', 'indianred2', 'indianred2', 'indianred2', 'indianred2', '#9b9b9b', 
        '#8b97c6', '#8b97c6', '#8b97c6', '#8b97c6', '#8b97c6', '#8b97c6')
      ),
  axis.text.y = element_text(size = 8),
  plot.margin = margin(5, 10, 2, 0)
)

ggsave( # Saves the plot as an image
  'senate.png',
  plot = p2,
  width = 5,
  height = 3.5
)
