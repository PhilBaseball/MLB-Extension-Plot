library(baseballr)
library(ggforce)
library(tidyverse)
library(dplyr)
library(ggplot2)


### MLB Extension/Release Point Plot ####

### Scrape data from September 5th ###

x <- map_df(.x = seq.Date(as.Date('2024-09-05'), 
                          as.Date('2024-09-05'), 
                          'day'), 
            ~get_game_pks_mlb(date = .x, 
                              level_ids = c(1))
)

safe_mlb <- safely(get_pbp_mlb)

# filter the game files for only those games that were completed and pull the game_pk as a numeric vector
Game <- map(.x = x %>%
              filter(status.codedGameState == "F") %>% 
              pull(game_pk), 
            ~safe_mlb(game_pk = .x)) %>%
  map('result') %>%
  bind_rows()


## Select our pitcher of choice and remove untagged pitches ###

pitcher_data<- Game %>% filter(matchup.pitcher.fullName == "Austin Gomber") %>% filter(!is.na(details.type.code))



### Plot ##
ggplot(data = pitcher_data,
       aes(x = pitchData.coordinates.x0 * -1, y = pitchData.extension, fill = details.type.code)) + xlab("Release Side (Ft.)")+ ylab("Extension (ft.)")+
  
  xlim(-10,10) + ylim(-10,15) + labs(color = "",title = paste("Release Point" ), subtitle = paste0(pitcher_data$matchup.pitcher.fullName[1]," ", pitcher_data$matchup.pitchHand.description[1], "-Handed Pitcher") )+
  
  geom_circle(aes(x0=0, y0=1.5, r=9), color='#CC9966',
              fill='#CC9966', lwd= 1, inherit.aes=FALSE) +
  coord_fixed()+ geom_point(size = 5, pch = 21, color = "black") + 
  scale_fill_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
  annotate(geom = "rect", xmin = -1, xmax = 1,ymin = -0.25,ymax = 0.25,color = "white", alpha = 1,size = 1,fill = c("white")) +
  annotate("text", x = 0, y = 14, label = "Home Plate This Way", color = "white", size = 5) + 
  geom_segment(aes(x = -9, y = 12, xend = -9, yend = 15), size = 1.5,
               arrow = arrow(length = unit(0.5, "cm")))+
  geom_segment(aes(x = 9, y = 12, xend = 9, yend = 15), size = 1.5,
               arrow = arrow(length = unit(0.5, "cm")))+
  theme(legend.key  = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = '#339933', colour = 'black'),
        strip.text = element_text(size = 6, face = 'bold'),
        axis.text.x=element_blank(), #remove x axis labels
        axis.text.y=element_blank(),  #remove y axis labels
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank() )

