##packages
install.packages("devtools")
devtools::install_github("danmorse314/hockeyR")
install.packages("hockeyR")
install.packages("gtExtras")





library(tidyverse)
library(janitor)
library(hockeyR)
library(sportyR)
library(gt)
library(gtExtras)
library(ggtext)







# Read data from CSV file
data <- read.csv("SKATERS_.csv")

# Define position rank based on EVP_60 totals
position_rank <- c("Center", "LW", "RW", "RD", "LD")
data$Position_Rank <- factor(data$Pos, levels = rev(position_rank))  # Reverse factor levels

library(ggplot2)

# Scatterplot with enhanced aesthetics
ggplot(data, aes(x = GP, y = EVP_60, color = Team, size = Position_Rank)) +
  geom_point(alpha = 0.7, shape = 16) +  # Change point size and shape
  geom_text(aes(label = Player), size = 3, vjust = -0.5, check_overlap = TRUE) +  # Add player names
  labs(title = "Scatterplot of GP vs EVP_60 by Team", x = "Games Played (GP)", y = "Even Strength Points per 60 (EVP_60)") +
  scale_color_manual(values = c("BOS" = "black", "TOR" = "blue")) +  # Customize team colors
  theme_minimal() +  # Change plot theme
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "gray90")) +  # Adjust panel background color
  guides(color = guide_legend(title = "Team")) +  # Customize legend title
  coord_cartesian(clip = "off") +  # Allow points and labels to extend beyond plot area
  scale_x_continuous(limits = c(0, NA)) +  # Adjust x-axis limits
  scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))  # Add dashed gridlines



Data1 <- read.csv("GOALIES_.csv")
# Load necessary library
# library(ggplot2)

# Melt the data for ggplot2
library(reshape2)
data_melted <- melt(Data1, id.vars = "Team")

# Plot side-by-side barplot with TOR as blue and BOS as black
ggplot(data_melted, aes(x = variable, y = value, fill = Team)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Variable", y = "Value") +
  scale_fill_manual(values = c("BOS" = "black", "TOR" = "blue")) +
  theme_minimal()

##After some basic browsing, it was proven that Boston defeated Toronto for the
#   second time in three days on March 7. Let's plot that 3.7 game using pre-
# existing code.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##playbyplay data

season <- load_pbp('2023-24')
##Plotting TOR @ BOS, 3.7.24


game <- season %>%
  filter(game_date == "2024-03-07" & event_team_abbr == "BOS")


team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$event_team_abbr) | team_abbr == unique(game$away_abbr)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team" = "team_abbr"))


geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n
    {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation}"
    ),
    caption = "data from hockeyR | plot made with sportyR"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )



##      Prediction: Bruins in 6. 












