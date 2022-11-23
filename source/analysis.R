library(tidyverse)
library(dplyr)
library('ggplot2')
incarceration_trends <- read.csv("C:/Users/16287/Documents/info201/data/incarceration-trends/incarceration_trends.csv")
View(incarceration_trends)
# The functions might be useful for A4
source("C:/Users/16287/Documents/info201/assignments/a4-Ian-Chiu333/source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
avgBlackInc2018 <- incarceration_trends %>%
  select(year, black_pop_15to64) %>%
  filter(year == "2018") %>%
  drop_na() %>%
  summarize(avg = mean(black_pop_15to64)) %>%
  pull(avg)
  

avgWhiteInc2018 <- incarceration_trends %>%
  select(year, white_pop_15to64) %>%
  filter(year == "2018") %>%
  drop_na() %>%
  summarize(avg = mean(white_pop_15to64)) %>%
  pull(avg)

avgMaleInc2018 <- incarceration_trends %>%
  select(male_pop_15to64) %>%
  drop_na() %>%
  summarize(avg = mean(male_pop_15to64)) %>%
  pull(avg)
## Section 3 ----

#-----------------------------------#
# Growth of the U.S. Prison Population
# These functions first sort and refine the data and then graph a bar chart of total jail population over year.
#----------------------------------------------------------------------------#
# This function sorts through the data and returns the total jail population per year
get_year_jail_pop <- function() {
  yearTotalPop <- incarceration_trends %>%
    select(year, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize(total = sum(total_jail_pop))
    
return(yearTotalPop)   
}

get_year_jail_pop()
# This function creates a bar graph of the total jail population per year.
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total)) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return(plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# These functions graph prison populations by state
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  stateJailPop <- incarceration_trends %>%
    select(year, total_jail_pop, state) %>%
    drop_na() %>%
    filter(state == states) %>%
    group_by(year, state) %>%
    summarize(total = sum(total_jail_pop)) %>%
    arrange(state)
  return(stateJailPop)
}
total <- get_jail_pop_by_states(c("CA", "WA"))

plot_jail_pop_by_states <- function(states) {
    plot <- ggplot(data = get_jail_pop_by_states(states), aes(x=year, y=total, color=state)) +
    geom_line() +
    geom_point() +
    ggtitle("Jail Populations By State")
  return(plot)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# African-American incarceration rate by state
# These functions graph African American incarceration rates by state
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states_black_white <- function(states) {
  stateJailPop <- incarceration_trends %>%
    select(year, black_pop_15to64, white_pop_15to64, state) %>%
    drop_na() %>%
    filter(state == states) %>%
    group_by(year, state) %>%
    summarize(totalBlack = sum(black_pop_15to64)) %>%
    summarize(totalWhite = sum(white_pop_15to64)) %>%
    mutate(totaldiff = totalBlack - totalWhite) %>%
    arrange(state)
  return(stateJailPop)
}

whiteBlackJailPop <- get_jail_pop_by_states_black_white(c("AL", "CA", "NY", "FL"))


plot_jail_pop_by_race_state <- function() {
  plot <- ggplot(data = whiteBlackJailPop, aes(x=year, y=total, color=state)) +
    geom_line() +
    ggtitle("Jail Populations By State and Race")
  return(plot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Map showing black incarceration by state
# These functions clean up the data and create a mpa of black incarceration
# See Canvas
#----------------------------------------------------------------------------#

state_shape <- map_data("state")
dataOrganizationAAI <- function() {
  df <- incarceration_trends %>%
    select(year, state, black_pop_15to64) %>%
    filter(year == "2018") %>%
    group_by(year, state) %>%
    summarize(Total = sum(black_pop_15to64)) %>%
    mutate(state = tolower(state.name[match(state, state.abb)]))
  return(df)
}

dataJoin <- function() {
  df <- dataOrganizationAAI()
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(df, by="state")
  return(state_shape)
}

plotMapAAI <- function() {
  state_shape <- dataJoin()
  plotMap <- ggplot(state_shape, aes(long, lat, group=group, fill=Total)) +
    geom_polygon(color="grey") +
    ggtitle("Black Incarceration by State")
  return(plotMap)
}

