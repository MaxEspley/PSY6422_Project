#Requirements
library(tidyverse)
library(here)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plotly)
library(gganimate)
library(gifski)
library(forcats)
library(ggtext)
library(scales)
library(prismatic)

#Import Datasets
ftse100pathway <- 
  here("projectfolder", "data", "FTSE100_Data_2002_2024.csv")
ftse100 <- read.csv(ftse100pathway)

resultspathway <- 
  here("projectfolder", "data","results.csv")
results <- read.csv(resultspathway)

shootoutspathway <- 
  here("projectfolder", "data", "shootouts.csv")
shootouts <- read.csv(shootoutspathway)


#Check Datasets have Imported
head(ftse100)
head(results)
head(shootouts)



#PLOT ONE
#How have UK Stocks changed over time in the ftse100 dataset?


#Convert date values to a date variable on R
ftse100$Date <- as.Date(ftse100$Date, format = "%d/%m/%Y")

#Convert Price to a numerical variable
ftse100$Price <- as.numeric(gsub(",", "", ftse100$Price)) #Remove confusing commas
ftse100$Price <- as.numeric(ftse100$Price)


#Define Colour Scale of the Line of Visualisation
#Low Values show up as Red
#High Values show up as Green
colour_scale <- scale_color_gradient(low = "red", high = "green")


#Create the initial (not animated) plot
#Line graph
stockplot <- ggplot(ftse100, 
            aes(x = Date, y = Price, color = Price)) +
  geom_line() +
  
  #Add heading and axis titles
  labs(title = "UK Stocks Since 2002", 
       subtitle = paste("Change in FTSE100 Daily Stock Values from",
                        format(min(ftse100$Date), "%B %Y"),
                        "to",
                        format(max(ftse100$Date), "%B %Y")),
       x = "Year", y = "UK Stock Price (£)") +
  
  #Choose Theme
  theme_classic() +
  
  #Add Colour Scale
  colour_scale +
  
  #Determine X axis Labels every 2 years
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  
  #Add vertical dashed lines coming from the X axis labels
  geom_vline(xintercept = as.numeric(seq(min(ftse100$Date), max(ftse100$Date), by = "2 years")), 
             color = "gray", linetype = "dashed", alpha = 0.5) +
  
  #Add data source below plot
  annotate("text", x = as.Date("2014-01-01"), y = min(ftse100$Price), 
           label = "Data Source: https://uk.investing.com", hjust = 0, vjust = -1,
           color = "darkgray", size = 3) +
  
  #Colour code, add fonts, determine size of writing
  theme(
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(color = "#1b2860"),
    axis.title.y = element_text(color = "#1b2860"),
    plot.title = element_text(
      family = "sans",
      size = 20,
      face = "bold",
      color = "#1b2860"),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(
      family = "sans",
      size = 10, 
      color = "#1b2860"),
    axis.title = element_text(
      family = "sans",
      size = 12))

#Show Non-animated Plot
print(stockplot)

# Animate the Plot
stocks_animated <- stockplot + transition_reveal(Date) +
  enter_fade() +
  exit_fade()


#View the Animated Plot
print(stocks_animated)


# Save the animated plot to the "figs" subfolder
animate(stocks_animated, renderer = gifski_renderer(here("projectfolder", "figs", "stocksplot.gif")))



#Prepare for creation of later visualisations
#Begin by merging the ftse100, results, and shootouts datasets


#Convert all date values to a date variable on R
ftse100$Date <- 
  as.Date(ftse100$Date, format = "%d/%m/%Y")

results$date <- 
  as.Date(results$date, format = "%Y-%m-%d")

shootouts$date <- 
  as.Date(shootouts$date, format = "%d/%m/%Y")


#Merge Datasets using this Date variable
data <- 
  merge(ftse100, results, 
        by.x = "Date", 
        by.y = "date", 
        all = TRUE)
data <- 
  merge(data, shootouts, 
        by.x = c("Date", "home_team", "away_team"), 
        by.y = c("date", "home_team", "away_team"), 
        all.x = TRUE)

#Check that Datasets have merged, looking at variables of new dataset
head(data)


#Fill in missing Stock data
#Some days do not have stock data
#This code adds to missing info using the next known stock values

data <- data[order(data$Date), ]
data <- data %>%
  fill(Price, Open, High, Low, Vol., Change.., .direction = "down")

#Check for NA values in one of these variables
num_na <- sum(is.na(data$Price))
print(num_na)

#We then needed to create a new variable showing who won football matches
#So far we only had the winners of games that went to penalties
#We needed to add to the current winner variable without changing the current values


#Determine which rows need filling in the winner variable
newwinners <- is.na(data$winner)


#If home_score > away_score, put the home team in winner variable
#If away_score > home_score, put the away team in winner variable
#If else (scores are even), put "draw"
data$winner[newwinners] <- 
  
  ifelse(data$home_score[newwinners] > data$away_score[newwinners], 
         data$home_team[newwinners], 
         
         ifelse(data$home_score[newwinners] < data$away_score[newwinners], 
                data$away_team[newwinners], 
                
                "draw"))


#View the dataset to check this has worked
view(data)


#Remove Rows with no Football Match
data <- data[complete.cases(data$home_team), ]


#PLOT 2
#An informative plot of how UK football teams have performed throughout history


#This analysis would require some changes to the dataset
#I do not want to carry these changes through to the final visualisation
#I therefore created a copy of the dataset for this process


#Create new dataset for plot 2
df_footy <- data


#This plot was focused on football results only
#We therefore did not need the stocks data


#Remove Unneeded Columns
df_footy <- 
  subset(df_footy, select = -c(Price, Open, High, Low, Vol., Change.., city, country, first_shooter))


#I wanted to see how nations had differed in their wins, losses, draws
#I therefore created a new dataset showing these variables, called nationstats

#We first needed the stats of when each team was in home team
#We then performed the same methods on when each team was in away team
nationstats <- df_footy %>%
  group_by(team = home_team) %>%   #Create variable named "team" for each national football team
  summarise(
    games_played = n(),   #Total games played by how many times they are in home_team
    games_won = sum(winner == home_team),   #If team is in winner, they won the game
    games_lost = sum(winner != home_team & winner != "draw")   #If team is not winner and the game was not a draw, put down as a loss
  ) %>%
  ungroup() %>%  # Remove grouping
  bind_rows(  #Bind rows to this dataset using the same process for away teams
    df_footy %>%
      group_by(team = away_team) %>%
      summarise(
        games_played = n(),
        games_won = sum(winner == away_team),
        games_lost = sum(winner != away_team & winner != "draw")
      )) %>%
  arrange(desc(games_played))  # Arrange by total games played, descending


#View the new dataset
view(nationstats)


#We have two rows for each team
#One shows their home results and the other shows their away results
#Bind Rows Together to show total matches/wins/losses
nationstats <- nationstats %>%
  group_by(team) %>%
  summarise(
    games_played_total = sum(games_played),
    games_won_total = sum(games_won),
    games_lost_total = sum(games_lost),
    .groups = "drop"  # Ensure that grouped data is returned as a flat data frame
  )


#Create individual datasets for countries of interest (UK nations)

#Determine which nations i want to keep
countries_to_keep <- 
  c("England", "Scotland", "Wales", "Northern Ireland")

#Create new dataset with only values of those teams
UKfooty <- nationstats %>%
  filter(team %in% countries_to_keep)

#View new dataset
head(UKfooty)


#Prepare for creation of Plot 2


#Arrange the nations in descending order of total matches played
#Create percentages of result frequency for each team
UKfooty <- UKfooty %>% 
  mutate(team = fct_rev(fct_inorder(team))) |> 
  pivot_longer(
    cols = -c(team, games_played_total),
    names_to = "type",
    values_to = "result"
  ) |> 
  mutate(share = result / games_played_total) |> 
  arrange(team, -share) |> 
  mutate(is_smaller = if_else(row_number() == 1, 0, 1), .by = team)


#Set Colours that will be used in the Visualisation
pal_base <- c("#d73027", "#1b9e77")
pal_dark <- clr_darken(pal_base, .25)   #darken these colours slightly for another use

blue_base <- "#1b2890"
blue_dark <- "#1b2860"


#Determine the colour coding/themes of Plot 2
#I took a lot of inspiration from another football dataset
#https://r-graph-gallery.com/web-dumbell-chart.html

theme_set(theme_minimal(base_family = "sans", base_size = 22)) #set theme
theme_update(
  axis.title = element_blank(),   #no initial axis titles
  axis.text.y = element_text(hjust = 0, color = blue_dark),   #set colour of team names
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),   #no panel grids
  
  #Set plot title characteristics
  plot.title = element_textbox_simple(
    size = rel(1.25), face = "plain", lineheight = 1.05, 
    fill = "transparent", width = unit(8, "inches"), box.color = "transparent", 
    margin = margin(0, 0, 35, 0), hjust = 0, halign = 0),
  
  #Set caption characteristics
  plot.caption = element_markdown(
    size = rel(.5), color = blue_base, hjust = 0, margin = margin(t = 20, b = 0),
    family = 'sans'),
  
  #Set positions of words in plot
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(25, 25, 15, 25),
  
  #Add background colour
  panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
  plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
  legend.position = "none"
)


#Set Title and Subtitle

#Apply different colours to different parts of the title
title <- paste0(
  "Out of the Four UK National Football Teams, <b>England have the <b style='color:",
  pal_dark[1], ";'>lowest loss</b> and <b style='color:", pal_dark[2], ";'>highest win</b> rates</b>"
)

#Set caption
caption <- paste0(
  "<b>Teams are ranked according to the number of International Football matches played so far | Source: Mart Jürisoo"
)


#What is the df?
n <- length(unique(UKfooty$team)) - 1


#Create Plot
UKFootballPlot = ggplot(UKfooty, aes(x = share, y = team)) +
  
  #dumbbell segments
  stat_summary(
    geom = "linerange", fun.min = "min", fun.max = "max",
    linewidth = c(rep(.8, n), 1.2), color = c(rep(grey_base, n), grey_dark)
  ) +
  
  #dumbbell points
  #white point to go over line endings
  geom_point(
    aes(x = share), size = 6, shape = 21, stroke = 1, color = "white", fill = "white"
  ) +
  #semi-transparent point fill
  geom_point(
    aes(x = share, fill = type), size = 6, shape = 21, stroke = 1, color = "white", alpha = .7
  ) +
  #point outline
  geom_point(
    aes(x = share), size = 6, shape = 21, stroke = 1, color = "white", fill = NA
  ) +
  
  #result labels
  geom_text(
    aes(label = percent(share, accuracy = 1, prefix = "    ", suffix = "%    "), 
        x = share -0.02, hjust = is_smaller-0.32, color = type),
    fontface = c(rep("plain", n*2), rep("bold", 2)),
    family = "sans", size = 4.2
  ) +
  
  #legend labels
  annotate(
    geom = "text", x = c(.18, .60), y = n + 1.8, 
    label = c("matches lost", "matches won"), family = "sans", 
    fontface = "bold", color = pal_base, size = 5, hjust = .5
  ) +
  
  coord_cartesian(clip = "off") +   #Prevent clipping of points
  scale_x_continuous(expand = expansion(add = c(.035, .05)), guide = "none") +   #Expand x-axis and remove x-axis guide
  scale_y_discrete(expand = expansion(add = c(.35, 1))) +   #Expand y axis
  scale_color_manual(values = pal_dark) +   #Set colour scale for lines
  scale_fill_manual(values = pal_base) +   #Set fill scale for points
  labs(title = title, caption = caption) +   #Set plot title and caption
  theme(axis.text.y = element_text(face = "bold", size=14))   #Set y axis text to bold and adjust size

#View the Plot
print(UKFootballPlot)


# Set the dimensions of the plot
plot2_width <- 10 # Adjust as needed
plot2_height <- 6 # Adjust as needed


#Save the Plot
ggsave(here("projectfolder", "figs", "ukfootballplot.png"), plot = UKFootballPlot, width = plot2_width, height = plot2_height)



#PLOT 3
#How have football results of UK countries affected the UK stock market since 2002?
#Use the dataset that we were working on before the creation of Plot 2


#Create individual dataset for UK countries only

#Set countries of interest
countries_to_keep <- 
  c("England", "Scotland", "Wales", "Northern Ireland")

#Keep rows if UK country is a home or away team
data <- 
  data[data$home_team %in% countries_to_keep | 
         data$away_team %in% countries_to_keep, ]


#Remove data before 2002
remove_date <- as.Date("2002-01-01")
data <- data[data$Date >= remove_date, ]


#Remove Unneeded Columns
data <- 
  subset(data, select = -c(Open, High, Low, Vol., city, country, first_shooter))


#Check data looks ok
view(data)


#Add match winners to winner variable
#Use same process as before
newwinners <- is.na(data$winner)
data$winner[newwinners] <- 
  
  ifelse(data$home_score[newwinners] > data$away_score[newwinners], 
         data$home_team[newwinners], 
         
         ifelse(data$home_score[newwinners] < data$away_score[newwinners], 
                data$away_team[newwinners], 
                
                "draw"))


#Create result variable
#Prioritise result of country with highest population
#England > Scotland > Wales > Northern Ireland
#If winner == draw, show draw
#If winner == England, show win
#If winner == Scotland, show win if England didn't play
#If winner == Wales, show win if England or Scotland didn't play
#If winner == Northern Ireland, show win if England, Scotland, or Wales didn't play
#If else, show loss

#Create function to do this
get_result <- function(winner, home_team, away_team) {
  if (winner == "draw") {
    return("draw")
  } else if (winner == "England") {
    return("win")
  } else if (winner == "Scotland") {
    if ("England" %in% c(home_team, away_team)) {
      return("loss")
    } else {
      return("win")
    }
  } else if (winner == "Wales") {
    if ("England" %in% c(home_team, away_team) || "Scotland" %in% c(home_team, away_team)) {
      return("loss")
    } else {
      return("win")
    }
  } else if (winner == "Northern Ireland") {
    if ("England" %in% c(home_team, away_team) || "Scotland" %in% c(home_team, away_team) || "Wales" %in% c(home_team, away_team)) {
      return("loss")
    } else {
      return("win")
    }
  } else {
    return("loss")
  }
}

# Apply the function to dataset
data$result <- mapply(get_result, data$winner, data$home_team, data$away_team)


#Ensure all stock change values are numerical
data$Change.. <- as.numeric(sub("%", "", data$Change..))


#Reorder result columns to win, draw, loss
data$result <- factor(data$result, levels = c("win", "draw", "loss"))


#National football matches typically only matter to fans when they are competitive
#The two most important competitions for UK nations are the Euros and World Cup

#Filter for only Tournament Games

#Determine which Tournaments to keep
tournaments_to_keep <- c("FIFA World Cup", "UEFA Euro")

#Filter data accordingly
tournament_data <- data %>%
  filter(tournament %in% tournaments_to_keep)


#Additional information for interactive plot
#How many wins/draws/losses are there in total?
#Calculate number of each condition
result_n <- tournament_data %>%
  group_by(result) %>%
  summarize(count = n())


#Create Plot 3

tournaments_plot <- ggplot(tournament_data %>%  # Start with tournament data
                             group_by(result) %>%  # Group by tournament result
                             summarize(Mean_Change = mean(Change..)),  # Summarize mean change for each result
                           aes(x = result, y = Mean_Change, fill = result,  # Define aesthetics
                               text = paste("Result:", result,  # Tooltip text
                                            "<br>Mean UK Stock Change (%):", round(Mean_Change, 3),
                                            "<br>Number of Values:", result_n$count[result]))) +  # Include count of values
  
  geom_bar(  # Add bar plot
    stat = "identity",  # Use identity statistics
    position = "identity",  # Use identity position
    width = 0.8,  # Set bar width
    colour = "#1b2860",  # Set border color
    size = 0.3) +  # Set border size
  
  # Set plot labels
  labs(title = "Football and The Stock Market",
       x = "UK International Football Result (WC and EUROs only)", 
       y = "Mean Stock Value Change (%)",
       subtitle = "Can UK National Teams' Football Results Influence Daily FTSE100 Stock Prices?") +
  
  # Set fill colors manually
  scale_fill_manual(values = c("win" = "#1b9e77", "draw" = "#fdae61", "loss" = "#d73027"),
                    name = NULL) +
  
  # Set plot theme
  theme_light() +  # Set light theme
  theme(
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),  # Set panel background
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),  # Set plot background
    panel.border = element_blank(),  # Remove panel border
    panel.grid.major.x = element_blank(),  # Remove major gridlines on x-axis
    axis.title.x = element_text(color = "#1b2860"),  # Set x-axis title color
    axis.title.y = element_text(color = "#1b2860"),  # Set y-axis title color
    plot.title = element_text(  # Set plot title text properties
      family = "sans",  # Set font family
      size = 20,  # Set font size
      face = "bold",  # Set font face
      color = "#1b2860"),  # Set font color
    axis.text = element_text(  # Set axis text properties
      family = "sans",  # Set font family
      size = 10,  # Set font size
      color = "#1b2860"),  # Set font color
    axis.title = element_text(  # Set axis title properties
      family = "sans",  # Set font family
      size = 12)) +  # Set font size
  
  geom_hline(yintercept = 0, linetype = "solid", color = "#1b2860") +  # Add horizontal line at y = 0 for reference
  guides(fill = "none")  # Remove fill legend



#Make the plot interactive
tournaments_animated <- ggplotly(tournaments_plot, tooltip = "text")

#A seperate process was needed to include a subtitle to the interactive plot
tournaments_animated <- tournaments_animated %>%
  layout(
    annotations = list(
      text = "Can UK National Teams' Football Results Influence Daily FTSE100 Stock Prices?",
      x = 0, y = 1.04,
      font = list(size = 14, color = "black", family = "sans"),
      xref = "paper", yref = "paper",
      showarrow = FALSE
    )
  )

# Print the modified plotly plot
print(tournaments_animated)



#Add Data Sources
#Add a text annotation for the stock data source
tournaments_animated <- tournaments_animated %>%
  layout(
    annotations = list(
      text = "Stock Data Source: https://uk.investing.com",
      x = 1,
      y = 0.85,  # Adjust the y-coordinate to position the text at the bottom
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(
        family = "sans",
        size = 10,  # Adjust the font size as needed
        color = "darkgrey"
      )
    )
  )


# Add a text annotation for the football data source
tournaments_animated <- tournaments_animated %>%
  layout(
    annotations = list(
      text = "Football Data Source: Mart Jürisoo",
      x = 1,
      y = 0.9,  # Adjust the y-coordinate to position the text at the bottom
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(
        family = "sans",
        size = 10,  # Adjust the font size as needed
        color = "darkgrey"
      )
    )
  )

# Print the annotated plotly plot
print(tournaments_animated)



#Save Original Plot 3 Barplot
ggsave(file = here("projectfolder", "figs", "footystocks.png"), plot = tournaments_plot)
