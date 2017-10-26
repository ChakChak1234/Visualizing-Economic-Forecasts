library(tidyverse)
library(rwars)


swmovies <- get_all_films()

swdata <- map_df(swmovies$results, function(x){
  data_frame(
    movie = x$title,
    species = length(x$species),
    planets = length(x$planets),
    characters = length(x$characters),
    vehicles = length(x$vehicles),
    release = x$release_date
  )
}) 

swdata <- gather(swdata, key, number, -movie, -release) %>% 
  arrange(release)

swdata

hchart(data_long, "line", hcaes(x = Year, y = Value, group = Scenario),
       color = c("red", "blue", "green"))


hchart(swdata, "line", hcaes(x = movie, y = number, group = key),
       color = c("#e5b13a", "#4bd5ee", "#4AA942", "#FAFAFA")) %>% 
  hc_title(
    text = "Diversity in <span style=\"color:#e5b13a\"> STAR WARS</span> movies",
    useHTML = TRUE) %>% 
  hc_tooltip(table = TRUE, sort = TRUE) %>% 
  hc_credits(
    enabled = TRUE,
    text = "Source: SWAPI via rwars package",
    href = "https://swapi.co/") %>% 
  hc_add_theme(
    hc_theme_flatdark(
      chart = list(
        backgroundColor = "transparent",
        divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
      )
    )
  )

