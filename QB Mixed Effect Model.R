library(tidyverse)
library(dplyr)
library(cfbscrapR)
library(lme4)

seasons <- 2014:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})


pbp <- pbp_2020 %>% 
  mutate(passer = ifelse(pass == 1, 
                         ifelse(str_detect(play_text, "from"), 
                                str_extract(play_text, "(?<= from )(.{0,30})(?= \\()"), 
                                str_extract(play_text, "(.{0,30})(?= pass| sacked)")), NA))


pbp <- pbp_2020 %>%
  mutate(
    rush_player = ifelse(.data$rush == 1, 
                         str_extract(.data$play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA),
    rush_player = str_remove(.data$rush_player, " run | \\d+ Yd Run"))

sp <- data.frame()
for(i in 2014:2020){
  data <- cfb_ratings_sp(year = i) %>% mutate(year = i)
  df <- data.frame(data)
  sp <- bind_rows(sp, df)
}
sp$defense_rating <- as.numeric(sp$defense_rating)
sp <- sp %>% select(year, team, defense_rating)
pbp <- pbp %>% left_join(sp, by = c("year" = "year", 
                                              "defense_play" = "team"))


#Most of the mixed effect model code is from @adrian_cadem. Changes were made for college football

dataraw <- pbp  %>% 
  filter(pass == 1 | rush == 1, 
         penalty_flag == FALSE, 
         !is.na(down), 
         !is.na(yards_to_goal)) %>% 
  mutate(in_red_zone = if_else(yards_to_goal <= 20, 1, 0),
         in_fg_range = if_else(yards_to_goal <= 35, 1, 0))


prev_play <- dataraw %>%
  group_by(game_id, offense_play) %>%
  mutate(
    total_runs = if_else(rush == 1,
                         cumsum(rush) - 1, cumsum(rush)
    ),
    total_pass = if_else(pass == 1,
                         cumsum(pass) - 1, cumsum(pass)
    ),
    previous_play = if_else(offense_play == dplyr::lag(offense_play),
                            dplyr::lag(play_type), "First play of Drive"
    ),
    previous_play = if_else(is.na(previous_play),
                            replace_na("First play of Drive"), previous_play
    )
  ) 

prev_play <- unite(prev_play, name, c(passer,rush_player), remove = FALSE, na.rm = TRUE)
prev_play <- prev_play %>% filter(name != "")

data_filt <- prev_play %>%
  filter(!is_na(EPA),
         !is.na(down),
         !is_na(name))


epa_data<- data_filt %>% 
  select(season, game_id, id_play, EPA, name, offense_play, defense_play,
         pass, yards_to_goal, yards_gained, down, distance, previous_play, 
         TimeSecsRem, Under_two, scoring_opp, rz_play, wp_before, period, def_EPA, 
         middle_8, defense_conference, defense_rating, home, away, score_diff) %>% 
  mutate(
    down = as.factor(down),
    t=paste(season,game_id,id_play,sep=''),
    posid = paste(home,away,as.character(season),offense_play,sep=''),
    defid = paste(home,away,as.character(season),defense_play,sep=''),
    t = paste(season,game_id,id_play),
    log_ydstogo = log(distance),
    t = paste(id_play,game_id,season),
    converted = if_else(yards_gained - distance>0,1,0),
    prev_play_run = if_else(previous_play=='run',1,0),
    first_play_drive = if_else(previous_play=='First play of Drive',1,0)
  ) 

plays_qb <- epa_data %>%
  group_by(name) %>% 
  summarise(
    num_plays = n()
  );colnames(plays_qb)[1] <- 'level'

#Fixes a couple Zach Wilson plays
epa_data <- epa_data %>% 
  mutate(name = case_when(
    name == "Zach. Wilson" ~ "Zach Wilson", 
    TRUE ~ name), 
    pos_home = ifelse(home == offense_play, 1, 0))

mixed<-lmer(EPA ~
              yards_to_goal +
              distance + 
              down +
              distance*down +
              TimeSecsRem +
              period +
              score_diff +
              defense_conference +
              defense_rating +
              pos_home + 
              Under_two +
              scoring_opp + 
              rz_play + 
              wp_before + 
              middle_8 + 
              previous_play +
              (1|name), 
            data=epa_data)


tt<-broom.mixed::tidy(mixed,effects="ran_vals") %>% 
  merge(plays_qb,by='level',all.x = TRUE,no.dups=TRUE) %>%
  filter(num_plays >300) %>%
  mutate(
    QB = as.factor(level),
    t_stat = estimate/std.error
  )

#If you want to filter down to top 10 statistically significant QBs
tt<-tt[order(-tt$estimate),]
tt<-head(tt,n=10)
z <- 1.282 #This is the 90% Z Score
tt<-tt %>% filter(
  abs(t_stat) > z
) 

#This section is entirely data viz for the 2016-2020 QB draft prospects
#Yes I know this is a bad way to do this but...... it worked lol 

tt <- tt %>% filter(level %in% c("Jared Goff", "Dak Prescott", 
                                 "Deshaun Watson", "Patrick Mahomes", 
                                 "Mitch Trubisky", "Baker Mayfield", 
                                 "Josh Rosen", "Lamar Jackson", "Sam Darnold", 
                                 "Kyler Murray", "Drew Lock", "Dwayne Haskins", 
                                 "Daniel Jones", "Jalen Hurts", "Tua Tagovailoa", 
                                 "Justin Herbert", "Joe Burrow", 
                                 "Justin Fields", "Trevor Lawrence", "Zach Wilson", 
                                 "Mac Jones", "Josh Allen"))

tt<-tt[order(-tt$estimate),]
tt<-head(tt,n=10)
z <- 1.282

teams <- epa_data %>% ungroup() %>% select(name, offense_play) %>% 
  filter(name %in% c("Jared Goff", "Dak Prescott", 
                     "Deshaun Watson", "Patrick Mahomes", 
                     "Mitch Trubisky", "Baker Mayfield", 
                     "Josh Rosen", "Lamar Jackson", "Sam Darnold", 
                     "Kyler Murray", "Drew Lock", "Dwayne Haskins", 
                     "Daniel Jones", "Jalen Hurts", "Tua Tagovailoa", 
                     "Justin Herbert", "Joe Burrow", 
                     "Justin Fields", "Trevor Lawrence", "Zach Wilson", 
                     "Mac Jones", "Josh Allen"),
         offense_play != "Navy") %>% distinct() %>% arrange(name)
#This gets rid of previous transfer locations (Yes, Georiga went with Jake Fromm)
teams <- teams[-c(7,10, 14, 17), ] 

tt <- tt %>% left_join(teams, by = c("QB" = "name"))
cfblogos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv")
tt <- tt %>% left_join(cfblogos, by = c("offense_play" = "school"))

tt<-tt[order(tt$estimate),]
tt %>%
  ggplot(aes(x=factor(QB, level = QB),estimate)) + 
  geom_point()+
  geom_pointrange(aes(ymin=estimate - z*std.error,
                      ymax=estimate + z*std.error), 
                  colour = tt$color)+
  coord_flip() + 
  labs(y = "Random Effects Estimate | EPA/Play",
       x = "Quarterback", 
       subtitle = "Data: 2014-2020, Passes & Rushes", 
       caption = "Model Code: @adrian_cadem | Figure: @CFBNumbers | Data: @CFB_Data with #cfbscrapR",
       title = "QB Rankings Via Mixed Effects Model") + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 10), 
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 9, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0.5))

ggsave("MixedEffectsNew.png", dpi = 600)


