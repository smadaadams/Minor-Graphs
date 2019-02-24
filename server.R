
require(shiny)
#require(baseballr) <- don't need this currently b/c I create the funcitons below and haven't figured out downloading github packages to shiny server yet lol
require(tidyverse)
require(lubridate)
require(zoo)
require(ggridges)
require(ggiraph)
require(gghighlight)

# Scrape MLB game logs for batters from Fangraphs (from baseballr package)
batter_game_logs_fg <- function(playerid, year = 2017) {
  url <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                playerid,
                "&season=",
                year,
                "&position=PB")
  
  payload <- xml2::read_html(url) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload <- payload %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>%
    dplyr::rename(BB_perc = BB., K_perc = K.,
                  wRC_plus = wRC.)
  
  payload <- as.data.frame(sapply(payload, function(x) (gsub("\\ %", "", x))),
                           stringsAsFactors=F)
  
  payload$BB_perc <- as.numeric(payload$BB_perc)/100
  payload$K_perc <- as.numeric(payload$K_perc)/100
  
  payload
}

pitcher_game_logs_fg <- function(playerid, year = 2017) {
  url_basic <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=P","&type=1")
  
  url_adv <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=P","&type=2")
  
  payload1 <- xml2::read_html(url_basic) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>% 
    select(-GS.1)
  
  payload2 <- xml2::read_html(url_adv) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>%
    dplyr::rename(BB_perc = BB., K_perc = K.,
                  LOB_perc = LOB., K_per_BB = K.BB,
                  HR_per_9 = HR.9, K_minus_BB = K.BB.,
                  ERA_minus = ERA., FIP_minus = FIP.)
  
  payload2 <- as.data.frame(sapply(payload2, function(x) (gsub("\\ %", "", x))),
                            stringsAsFactors=F)
  
  payload2 <- payload2 %>% mutate(
    BB_perc = as.numeric(BB_perc)/100,
    K_perc = as.numeric(K_perc)/100,
    LOB_perc = as.numeric(LOB_perc)/100, 
    K_minus_BB = as.numeric(K_minus_BB)/100
  )
  
  payload <- merge(payload1,payload2)
  # separate Team column into Team & MiLB level
  payload <- payload %>% 
    separate(Team, into = c("Team","Level"),sep=" ")
  payload
}

# Scrape MiLB game logs for batters from Fangraphs, combining 'standard' and 'advanced' tabs
milb_batter_game_logs_fg <- function(playerid, year = 2017) {
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=PB","&type=-1")
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=PB","&type=-2")
  
  Birthday <-  xml2::read_html(url_basic) %>%
    rvest::html_nodes("div.player-info-bio") #%>%
  Birthday <- sub(".*Birthdate: </strong>", "", Birthday[1])
  Birthday <- trimws(gsub("\\(.*", "", Birthday[1]))
  # standard table
  payload1 <- xml2::read_html(url_basic) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Date|Total", Date)) 
  # advanced table
  payload2 <- xml2::read_html(url_adv) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>% 
    dplyr::rename(BB_perc = BB., K_perc = K.,
                  wRC_plus = wRC., BB_per_K = BB.K)
  payload2 <- as.data.frame(sapply(payload2, function(x) (gsub("\\ %", "", x))),
                            stringsAsFactors=F)
  
  payload2$BB_perc <- as.numeric(payload2$BB_perc)/100
  payload2$K_perc <- as.numeric(payload2$K_perc)/100
  # combine standard & advanced game log tabs
  payload <- merge(payload1,payload2)
  # separate Team column into Team & MiLB level
  payload <- payload %>% 
    separate(Team, into = c("Team","Level"),sep=" ") %>% mutate(Birthday)
  payload
}  

milb_pitcher_game_logs_fg <- function(playerid, year = 2017) {
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=P","&type=-1")
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=P","&type=-2")
  # standard table
  payload1 <- xml2::read_html(url_basic) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Date|Total", Date)) 
  # advanced table
  payload2 <- xml2::read_html(url_adv) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Date|Total", Date)) %>% 
    dplyr::rename(BB_perc = BB., K_perc = K.,
                  LOB_perc = LOB., K_per_BB = K.BB,
                  HR_per_9 = HR.9, K_minus_BB = K.BB.
    )
  payload2 <- as.data.frame(sapply(payload2, function(x) (gsub("\\ %", "", x))),
                            stringsAsFactors=F)
  payload2 <- payload2 %>% mutate(
    BB_perc = as.numeric(BB_perc)/100,
    K_perc = as.numeric(K_perc)/100,
    LOB_perc = as.numeric(LOB_perc)/100, 
    K_minus_BB = as.numeric(K_minus_BB)/100
  )
  
  # combine standard & advanced game log tabs
  payload <- merge(payload1,payload2)
  # separate Team column into Team & MiLB level
  payload <- payload %>% 
    separate(Team, into = c("Team","Level"),sep=" ")
  payload
}  

milb_adv_scrape_game <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=batter&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% filter(!is.na(hc_x)) %>% 
    mutate(Date = game_date,
           GB = if_else(hit_trajectory=="ground_ball",1,0),
           FB = if_else(hit_trajectory=="fly_ball",1,0),
           LD = if_else(hit_trajectory=="line_drive",1,0),
           PU = if_else(hit_trajectory=="popup",1,0),
           a = sqrt(250^2+(-250)^2),
           b = sqrt(((hc_x)-(-125))^2+((250-hc_y)-295)^2),
           c = sqrt(((hc_x)-125)^2+((250-hc_y)-45)^2),
           angle = 180-acos((b^2-a^2-c^2)/(2*a*c))*180/pi,
           Pull = if_else(angle<30 & bat_side=="R", 1, if_else(angle>=60 & bat_side=="L",1,0)),
           Center = if_else(angle<60 & angle>=30, 1, 0),
           Oppo = if_else(angle>=60 & bat_side=="R", 1, if_else(angle<30 & bat_side=="L",1,0))
    )
  
  game_summary <- game_summary %>% group_by(Date) %>% 
    summarise(GB=sum(GB), FB=sum(FB), LD=sum(LD), PU=sum(PU), 
              Pull=sum(Pull), Center=sum(Center), Oppo=sum(Oppo)) 
  
  game_summary <- game_summary %>% group_by(Date) %>% 
    mutate(in_play = sum(GB,FB,LD,PU), FBPU = sum(FB,PU)) %>% 
    filter(in_play != 0) %>% 
    mutate(Season = year(Date),Date2 = paste0("2017-",month(Date),"-",day(Date)))
}

milb_adv_scrape_game_pitcher <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=pitcher&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% filter(!is.na(hc_x)) %>% 
    mutate(Date = game_date,
           GB = if_else(hit_trajectory=="ground_ball",1,0),
           FB = if_else(hit_trajectory=="fly_ball",1,0),
           LD = if_else(hit_trajectory=="line_drive",1,0),
           PU = if_else(hit_trajectory=="popup",1,0),
           a = sqrt(250^2+(-250)^2),
           b = sqrt(((hc_x)-(-125))^2+((250-hc_y)-295)^2),
           c = sqrt(((hc_x)-125)^2+((250-hc_y)-45)^2),
           angle = 180-acos((b^2-a^2-c^2)/(2*a*c))*180/pi,
           Pull = if_else(angle<30 & bat_side=="R", 1, if_else(angle>=60 & bat_side=="L",1,0)),
           Center = if_else(angle<60 & angle>=30, 1, 0),
           Oppo = if_else(angle>=60 & bat_side=="R", 1, if_else(angle<30 & bat_side=="L",1,0))
    )
  
  game_summary <- game_summary %>% group_by(Date) %>% 
    summarise(GB=sum(GB), FB=sum(FB), LD=sum(LD), PU=sum(PU), 
              Pull=sum(Pull), Center=sum(Center), Oppo=sum(Oppo)) 
  
  game_summary <- game_summary %>% group_by(Date) %>% 
    mutate(in_play = sum(GB,FB,LD,PU), FBPU = sum(FB,PU)) %>% 
    filter(in_play != 0) %>% 
    mutate(Season = year(Date),Date2 = paste0("2017-",month(Date),"-",day(Date)))
}

milb_adv_scrape_in_play <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=batter&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% filter(!is.na(hc_x)) %>% 
    mutate(Date = game_date,
           GB = if_else(hit_trajectory=="ground_ball",1,0),
           FB = if_else(hit_trajectory=="fly_ball",1,0),
           LD = if_else(hit_trajectory=="line_drive",1,0),
           PU = if_else(hit_trajectory=="popup",1,0),
           a = sqrt(250^2+(-250)^2),
           b = sqrt(((hc_x)-(-125))^2+((250-hc_y)-295)^2),
           c = sqrt(((hc_x)-125)^2+((250-hc_y)-45)^2),
           angle = 180-acos((b^2-a^2-c^2)/(2*a*c))*180/pi,
           Pull = if_else(angle<30 & bat_side=="R", 1, if_else(angle>=60 & bat_side=="L",1,0)),
           Center = if_else(angle<60 & angle>=30, 1, 0),
           Oppo = if_else(angle>=60 & bat_side=="R", 1, if_else(angle<30 & bat_side=="L",1,0)),
           est_distance = 2.20*sqrt((250-hc_x-125)^2+(250-hc_y-45)^2)+13.1
    )
}

milb_adv_scrape_in_play_pitcher <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=pitcher&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% filter(!is.na(hc_x)) %>% 
    mutate(Date = game_date,
           GB = if_else(hit_trajectory=="ground_ball",1,0),
           FB = if_else(hit_trajectory=="fly_ball",1,0),
           LD = if_else(hit_trajectory=="line_drive",1,0),
           PU = if_else(hit_trajectory=="popup",1,0),
           a = sqrt(250^2+(-250)^2),
           b = sqrt(((hc_x)-(-125))^2+((250-hc_y)-295)^2),
           c = sqrt(((hc_x)-125)^2+((250-hc_y)-45)^2),
           angle = 180-acos((b^2-a^2-c^2)/(2*a*c))*180/pi,
           Pull = if_else(angle<30 & bat_side=="R", 1, if_else(angle>=60 & bat_side=="L",1,0)),
           Center = if_else(angle<60 & angle>=30, 1, 0),
           Oppo = if_else(angle>=60 & bat_side=="R", 1, if_else(angle<30 & bat_side=="L",1,0)),
           est_distance = 2.20*sqrt((250-hc_x-125)^2+(250-hc_y-45)^2)+13.1
    )
}

theme_smada <- function(){
  theme(text = element_text("sans-serif"),
        plot.title = element_text(size=20, vjust=5),
        plot.subtitle = element_text(face="italic", hjust = .27, vjust= 2.5, color="#2d2d2d"),
        #plot.subtitle = element_text(face="italic"),#, hjust = 0, margin = margin(5,20,20,20)),
        plot.caption = element_text(color="#696969", hjust=0),
        plot.margin = unit(c(1,1,1,1), "cm"),

        axis.title.x = element_text(size = 15, margin = margin(20, 20, 20, 20)),
        axis.title.y = element_text(size = 15, margin = margin(20, 20, 20, 20)), 
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        
        strip.text.x = element_text(size=12, face="bold"),
        
        legend.title = element_text(size=15, vjust = 5, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(),
        legend.box.spacing = unit(1,"cm"),
        legend.box.margin = margin(20, 20, 20, 20),
        legend.justification = "top"
  )
}

theme_spray <- function(){
  theme(text = element_text("sans-serif"),
        plot.title = element_text(size=16, vjust=5),
        plot.subtitle = element_text(size = 9, face="italic", color="#2d2d2d"),
        plot.caption = element_text(color="#696969", hjust=0),
        plot.margin = unit(c(1,1,1,1), "cm"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        
        strip.text.x = element_text(size=12, face="bold"),
        
        legend.title = element_text(size=12, vjust = 7, face = "bold"),
        legend.text = element_text(size=8),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(),
        legend.box.spacing = unit(1,"cm"),
        legend.box.margin = margin(20, 20, 20, 20),
        legend.justification = "top"
  )
}

### Read in batters
battersMLB <- read.csv("data/mlb_2018_batter_ids.csv",header=T)
battersMiLB <- read.csv("data/milb_2018_batter_ids.csv",header=T)
battersMLB <- battersMLB %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")")))
battersMiLB <- battersMiLB %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")")))

### Read in pitchers
pitchersMLB <- read.csv("data/mlb_2018_pitcher_ids_new.csv",header=T)
pitchersMiLB <- read.csv("data/milb_2018_pitcher_ids_new.csv",header=T)
pitchersMLB <- pitchersMLB %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")")))
pitchersMiLB <- pitchersMiLB %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")")))

### Create Names list to reference in drop down
MLBnames <- sort(battersMLB$NameTeam)
MiLBnames <- sort(battersMiLB$NameTeam)

MLBpitchernames <- sort(pitchersMLB$NameTeam)
MiLBpitchernames <- sort(pitchersMiLB$NameTeam)

year <- "all"

batter_logs <- NULL
batter_logs_mlb <- NULL
batter_logs_MiLB <- NULL
batter_logs_savant <- NULL
batter_logs_in_play <- NULL

pitcher_logs <- NULL
pitcher_logs_mlb <- NULL
pitcher_logs_MiLB <- NULL
pitcher_logs_savant <- NULL
pitcher_logs_in_play <- NULL

#logo <- image_read("www/PL_icon_slate.png")
#logo <- rasterGrob(logo, interpolate=TRUE)

server <- function(input, output, session) {
  
  #setBookmarkExclude(c("yearlower", "yearupper", ))
  
  metrics <- reactiveValues()
  metricsP <- reactiveValues()
  
  updatePoolcount <- reactiveValues()
  updatePoolcount$H <- 0
  updatePoolcount$P <- 0
  
  # BATTERS - updates player dropdown based on MLB or MiLB selection
  observe({
    if(updatePoolcount$H > 0){
      if(input$playerlistchoice == "MLB"){
        updateSelectInput(session, "playername",
                          choices = MLBnames,
                          selected = battersMLB$NameTeam[which(battersMLB$Name=="Ozzie Albies")]
        )
    } else if(input$playerlistchoice == "MiLB"){
      
      # update player pool to MiLB w/ Eloy as default selection, change back to MLB w/ Albies as default
      updateSelectInput(session, "playername",
                        choices = MiLBnames,
                        selected = battersMiLB$NameTeam[which(battersMiLB$Name=="Eloy Jimenez")]
      )
    }
    }
  })
  
  observeEvent(input$playerlistchoice, ({
      updatePoolcount$H <- updatePoolcount$H + 1
    })
  )
  
  # PITCHERS - updates player dropdown based on MLB or MiLB selection
  observe({
      if(input$playerlistchoiceP == "MLB"){
        updateSelectInput(session, "playernameP",
                          choices = MLBpitchernames,
                          selected = pitchersMLB$NameTeam[which(pitchersMLB$Name=="Jack Flaherty")]
        )
      } else if(input$playerlistchoiceP == "MiLB"){
        
        # update player pool to MiLB w/ Eloy as default selection, change back to MLB w/ Albies as default
        updateSelectInput(session, "playernameP",
                          choices = MiLBpitchernames,
                          selected = pitchersMiLB$NameTeam[which(pitchersMiLB$Name=="Forrest Whitley")]
        )
      }
  })
  
  
  ### HITTERS 
  filteredTable <- reactive({
    if(isolate(input$createGraph)>0){
    #progress bar
    withProgress(message = "" , style = "notification", value = 0.9, {
      Sys.sleep(0.25)
      
      # check if MLB or MiLB
      if(isolate(input$playerlistchoice)=="MLB"){
        player <- which(battersMLB$NameTeam==input$playername)
        
        ### if we haven't scraped the data, do that
        if(is.null(batter_logs_mlb[player][[1]])){
          batter_logs[player] <- list(as.data.frame(milb_batter_game_logs_fg(battersMLB$playerid[player],year)))
          batter_logs_mlb[player] <- list(as.data.frame(batter_game_logs_fg(battersMLB$playerid[player],year)))
          batter_logs_savant[player] <- list(as.data.frame(milb_adv_scrape_game(battersMLB$mlbid[player])))
          
          # Create a MiLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
          TempLog <- as.data.frame(batter_logs[player]) %>% filter(PA!=0) %>% 
            mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date)),
                   Date = as.Date(Date),
                   SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA)) 
          
          # Create a temporary MLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
          TempLogMLB <- as.data.frame(batter_logs_mlb[player]) %>% filter(PA!=0) %>% 
            mutate(Season = year(Date),Level = "MLB",Date2 = paste0("2017-",month(Date),"-",day(Date)),
                   Date = as.Date(Date),
                   SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA))
          
          TempLogSavant <- as.data.frame((batter_logs_savant[player])) %>% 
            mutate(Date = as.Date(Date))
          
          # Create MiLB career avgs
          metrics$MiLB_BB_perc <- sum(as.numeric(TempLog$BB),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_K_perc <- sum(as.numeric(TempLog$SO),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_wRC_plus <- mean(as.numeric(TempLog$wRC_plus))
          metrics$MiLB_ISO <- mean(as.numeric(TempLog$ISO))
          metrics$MiLB_BABIP <- mean(as.numeric(TempLog$BABIP))
          metrics$MiLB_SB_per_600 <- sum(as.numeric(TempLog$SB),na.rm = T)*600/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_SB_attempts_per_600 <- (sum(as.numeric(TempLog$CS),na.rm = T)+sum(as.numeric(TempLog$SB),na.rm = T))*600/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_BA <- sum(as.numeric(TempLog$H),na.rm = T)/sum(as.numeric(TempLog$AB),na.rm=T)
          metrics$MiLB_OBP <- sum(sum(as.numeric(TempLog$H),na.rm = T),sum(as.numeric(TempLog$HBP),na.rm=T),sum(as.numeric(TempLog$BB),na.rm = T))/sum(as.numeric(TempLog$PA),na.rm=T)
          metrics$MiLB_Slug <- sum(sum(as.numeric(TempLog$X1B),na.rm = T),2*sum(as.numeric(TempLog$X2B),na.rm = T),3*sum(as.numeric(TempLog$X3B),na.rm = T),4*sum(as.numeric(TempLog$HR),na.rm = T))/sum(as.numeric(TempLog$AB),na.rm = T)
          metrics$MLB_SB_per_600 <- sum(as.numeric(TempLog$SB))*600/sum(as.numeric(TempLog$PA))
          
          # Merge two game logs for MLB players
          Combined_milb_mlbLog <- merge(TempLog,TempLogMLB,all=T)
          
          # Merge Fangraphs data with Savant data and order levels correctly
          Combined_milb_mlbLog2 <- TempLogSavant %>% left_join(Combined_milb_mlbLog, by=c("Date","Season","Date2")) %>%
            filter (!is.na(Level)) %>%
            mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
          
          MiLB_Only <- Combined_milb_mlbLog2 %>%
            filter(Level != "MLB")
          
          metrics$MiLB_FB <- sum(as.numeric(MiLB_Only$FBPU),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metrics$MiLB_GB <- sum(as.numeric(MiLB_Only$GB),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metrics$MiLB_LD <- sum(as.numeric(MiLB_Only$LD),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metrics$MiLB_PU <- sum(as.numeric(MiLB_Only$PU),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metrics$MiLB_HRFB <- sum(as.numeric(MiLB_Only$HR),na.rm = T)/sum(as.numeric(MiLB_Only$FBPU),na.rm = T)
          metrics$MiLB_Pull <- sum(as.numeric(MiLB_Only$Pull),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metrics$MiLB_Center <- sum(as.numeric(MiLB_Only$Center),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metrics$MiLB_Oppo <- sum(as.numeric(MiLB_Only$Oppo),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          
          Combined_milb_mlbLog2
        }
      }
      ### Create table for MiLB selection
      else {
        player <- which(battersMiLB$NameTeam==input$playername)
        
        ### if we haven't scraped the data, do that
        if(is.null(batter_logs_MiLB[player][[1]])){
          batter_logs[player] <- list(as.data.frame(milb_batter_game_logs_fg(battersMiLB$playerid[player],year)))
          batter_logs_savant[player] <- list(as.data.frame(milb_adv_scrape_game(battersMiLB$mlbid[player])))
          
          # Create a temporary MiLB game log that removes 0 PA games & adds Season variable, SB per 600 metrics
          # Create Date2 variable for consistent x-axis 
          TempLog <- as.data.frame(batter_logs[player]) %>% filter(PA!=0) %>% 
            mutate(Season = year(Date), Date = as.Date(Date),
                   Date2 = paste0("2017-",month(Date),"-",day(Date)),
                   SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA)) 
          
          TempLogSavant <- as.data.frame((batter_logs_savant[player])) %>%
            mutate(Date = as.Date(Date))
          
          # Calculate MiLB career avgs
          metrics$MiLB_BB_perc <- sum(as.numeric(TempLog$BB),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_K_perc <- sum(as.numeric(TempLog$SO),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_wRC_plus <- mean(as.numeric(TempLog$wRC_plus))
          metrics$MiLB_ISO <- mean(as.numeric(TempLog$ISO))
          metrics$MiLB_BABIP <- mean(as.numeric(TempLog$BABIP))
          metrics$MiLB_SB_per_600 <- sum(as.numeric(TempLog$SB),na.rm = T)*600/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_SB_attempts_per_600 <- (sum(as.numeric(TempLog$CS),na.rm = T)+sum(as.numeric(TempLog$SB),na.rm = T))*600/sum(as.numeric(TempLog$PA),na.rm = T)
          metrics$MiLB_BA <- sum(as.numeric(TempLog$H),na.rm = T)/sum(as.numeric(TempLog$AB),na.rm=T)
          metrics$MiLB_OBP <- sum(sum(as.numeric(TempLog$H),na.rm = T),sum(as.numeric(TempLog$HBP),na.rm=T),sum(as.numeric(TempLog$BB),na.rm = T))/sum(as.numeric(TempLog$PA),na.rm=T)
          metrics$MiLB_Slug <- sum(sum(as.numeric(TempLog$X1B),na.rm = T),2*sum(as.numeric(TempLog$X2B),na.rm = T),3*sum(as.numeric(TempLog$X3B),na.rm = T),4*sum(as.numeric(TempLog$HR),na.rm = T))/sum(as.numeric(TempLog$AB),na.rm = T)
          metrics$MLB_SB_per_600 <- sum(as.numeric(TempLog$SB))*600/sum(as.numeric(TempLog$PA))
          
          metrics$MiLB_FB <- sum(as.numeric(TempLogSavant$FBPU),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metrics$MiLB_GB <- sum(as.numeric(TempLogSavant$GB),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metrics$MiLB_LD <- sum(as.numeric(TempLogSavant$LD),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metrics$MiLB_PU <- sum(as.numeric(TempLogSavant$PU),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metrics$MiLB_HRFB <- sum(as.numeric(TempLog$HR),na.rm = T)/sum(as.numeric(TempLogSavant$FBPU),na.rm = T)
          metrics$MiLB_Pull <- sum(as.numeric(TempLogSavant$Pull),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metrics$MiLB_Center <- sum(as.numeric(TempLogSavant$Center),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metrics$MiLB_Oppo <- sum(as.numeric(TempLogSavant$Oppo),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          
          # Merge Fangraphs data with Savant data and order levels correctly
          
          Combined_milb_mlbLog2 <- TempLogSavant %>% left_join(TempLog, by=c("Date","Season","Date2")) %>%
            filter (!is.na(Level)) %>% 
            mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
        }
      }
    })
    }
  })
  
  ### PITCHERS 
  # Create the table that is used to create the plots
  filteredTableP <- reactive({
    #progress bar
    withProgress(message = '', style = "notification", value = 0.9, {
      Sys.sleep(0.25)
      
      # check if MLB or MiLB
      if(isolate(input$playerlistchoiceP)=="MLB"){
        player <- which(pitchersMLB$NameTeam==input$playernameP)
        
        ### if we haven't scraped the data, do that
        if(is.null(pitcher_logs_mlb[player][[1]])){
          pitcher_logs[player] <- list(as.data.frame(milb_pitcher_game_logs_fg(pitchersMLB$playerid[player],year)))
          pitcher_logs_mlb[player] <- list(as.data.frame(pitcher_game_logs_fg(pitchersMLB$playerid[player],year)))
          pitcher_logs_savant[player] <- list(as.data.frame(milb_adv_scrape_game_pitcher(pitchersMLB$mlbid[player])))
          
          # Create a MiLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
          TempLog <- as.data.frame(pitcher_logs[player]) %>% filter(TBF!=0) %>% 
            mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date)),
                   Date = as.Date(Date))
          
          # Create a temporary MLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
          TempLogMLB <- as.data.frame(pitcher_logs_mlb[player]) %>% filter(TBF!=0) %>% 
            mutate(Season = year(Date),Level = "MLB",Date2 = paste0("2017-",month(Date),"-",day(Date)),
                   Date = as.Date(Date))
          
          TempLogSavant <- as.data.frame((pitcher_logs_savant[player])) %>% 
            mutate(Date = as.Date(Date))
          
          # Create MiLB career avgs
          metricsP$MiLB_BB_perc <- sum(as.numeric(TempLog$BB),na.rm = T)/sum(as.numeric(TempLog$TBF),na.rm = T)
          metricsP$MiLB_K_perc <- sum(as.numeric(TempLog$SO),na.rm = T)/sum(as.numeric(TempLog$TBF),na.rm = T)
          metricsP$MiLB_K9 <- sum(as.numeric(TempLog$SO),na.rm = T)*9/sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_BB9 <- sum(as.numeric(TempLog$BB),na.rm = T)*9/sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_KBB <- metricsP$MiLB_K_perc - metricsP$MiLB_BB_perc
          metricsP$MiLB_BABIP <- mean(as.numeric(TempLog$BABIP),na.rm = T)
          metricsP$MiLB_GS <- mean(as.numeric(TempLog$GS),na.rm = T)
          metricsP$MiLB_IPG <- mean(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_FIP <- mean(as.numeric(TempLog$FIP),na.rm = T)
          metricsP$MiLB_ERA <- sum(as.numeric(TempLog$ER),na.rm = T)*9/sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_WHIP <- (sum(as.numeric(TempLog$BB),na.rm = T) + sum(as.numeric(TempLog$H),na.rm = T))/
            sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_BA <- mean(as.numeric(TempLog$AVG),na.rm = T)
          
          # Merge two game logs for MLB players
          Combined_milb_mlbLog <- merge(TempLog,TempLogMLB,all=T)
          
          # Merge Fangraphs data with Savant data and order levels correctly
          Combined_milb_mlbLog2 <- TempLogSavant %>% left_join(Combined_milb_mlbLog, by=c("Date","Season","Date2")) %>%
            filter (!is.na(Level)) %>%
            mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
          
          MiLB_Only <- Combined_milb_mlbLog2 %>%
            filter(Level != "MLB")
          
          metricsP$MiLB_FB <- sum(as.numeric(MiLB_Only$FBPU),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metricsP$MiLB_GB <- sum(as.numeric(MiLB_Only$GB),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metricsP$MiLB_LD <- sum(as.numeric(MiLB_Only$LD),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metricsP$MiLB_PU <- sum(as.numeric(MiLB_Only$PU),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metricsP$MiLB_HRFB <- sum(as.numeric(MiLB_Only$HR),na.rm = T)/sum(as.numeric(MiLB_Only$FBPU),na.rm = T)
          metricsP$MiLB_Pull <- sum(as.numeric(MiLB_Only$Pull),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metricsP$MiLB_Center <- sum(as.numeric(MiLB_Only$Center),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          metricsP$MiLB_Oppo <- sum(as.numeric(MiLB_Only$Oppo),na.rm = T)/sum(as.numeric(MiLB_Only$in_play),na.rm = T)
          
          Combined_milb_mlbLog2
        }
      }
      ### Create table for MiLB selection
      else {
        player <- which(pitchersMiLB$NameTeam==input$playernameP)
        
        ### if we haven't scraped the data, do that
        if(is.null(pitcher_logs_MiLB[player][[1]])){
          pitcher_logs[player] <- list(as.data.frame(milb_pitcher_game_logs_fg(pitchersMiLB$playerid[player],year)))
          pitcher_logs_savant[player] <- list(as.data.frame(milb_adv_scrape_game_pitcher(pitchersMiLB$mlbid[player])))
          
          # Create a temporary MiLB game log that removes 0 PA games & adds Season variable, SB per 600 metrics
          # Create Date2 variable for consistent x-axis 
          TempLog <- as.data.frame(pitcher_logs[player]) %>% filter(TBF!=0) %>% 
            mutate(Season = year(Date), Date = as.Date(Date),
                   Date2 = paste0("2017-",month(Date),"-",day(Date))) 
          
          TempLogSavant <- as.data.frame((pitcher_logs_savant[player])) %>%
             mutate(Date = as.Date(Date))

          # Calculate MiLB career avgs
          
          metricsP$MiLB_BABIP <- mean(as.numeric(TempLog$BABIP),na.rm = T)
          metricsP$MiLB_K_perc <- sum(as.numeric(TempLog$SO),na.rm = T)/sum(as.numeric(TempLog$TBF),na.rm = T)
          metricsP$MiLB_BB_perc <- sum(as.numeric(TempLog$BB),na.rm = T)/sum(as.numeric(TempLog$TBF),na.rm = T)
          metricsP$MiLB_KBB <- metricsP$MiLB_K_perc - metricsP$MiLB_BB_perc
          metricsP$MiLB_K9 <- sum(as.numeric(TempLog$SO),na.rm = T)*9/sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_BB9 <- sum(as.numeric(TempLog$BB),na.rm = T)*9/sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_GS <- mean(as.numeric(TempLog$GS),na.rm = T)
          metricsP$MiLB_IPG <- mean(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_FIP <- mean(as.numeric(TempLog$FIP),na.rm = T)
          metricsP$MiLB_ERA <- sum(as.numeric(TempLog$ER),na.rm = T)*9/sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_WHIP <- (sum(as.numeric(TempLog$BB),na.rm = T) + sum(as.numeric(TempLog$H),na.rm = T))/
                                 sum(as.numeric(TempLog$IP),na.rm = T)
          metricsP$MiLB_BA <- mean(as.numeric(TempLog$AVG),na.rm = T)
          metricsP$MiLB_FB <- sum(as.numeric(TempLogSavant$FBPU),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metricsP$MiLB_GB <- sum(as.numeric(TempLogSavant$GB),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metricsP$MiLB_LD <- sum(as.numeric(TempLogSavant$LD),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metricsP$MiLB_PU <- sum(as.numeric(TempLogSavant$PU),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metricsP$MiLB_HRFB <- sum(as.numeric(TempLog$HR),na.rm = T)/sum(as.numeric(TempLogSavant$FBPU),na.rm = T)
          metricsP$MiLB_Pull <- sum(as.numeric(TempLogSavant$Pull),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metricsP$MiLB_Center <- sum(as.numeric(TempLogSavant$Center),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          metricsP$MiLB_Oppo <- sum(as.numeric(TempLogSavant$Oppo),na.rm = T)/sum(as.numeric(TempLogSavant$in_play),na.rm = T)
          
          # Merge Fangraphs data with Savant data and order levels correctly
          
          Combined_milb_mlbLog2 <- TempLogSavant %>% left_join(TempLog, by=c("Date","Season","Date2")) %>%
            filter (!is.na(Level)) %>% 
            mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
        }
      }
    })
  })
  
  filteredTable_in_play <- reactive({
    #progress bar
    withProgress(message = '', style = "notification", value = 0.9, {
      Sys.sleep(0.25)
      
      if(isolate(input$playerlistchoice)=="MLB"){
        player <- which(battersMLB$NameTeam==input$playername)
        ### if we haven't scraped the data, do that
        if(is.null(batter_logs_mlb[player][[1]])){
          batter_logs_in_play[player] <- list(as.data.frame(milb_adv_scrape_in_play(battersMLB$mlbid[player])))
        
          TempLogSavant <- as.data.frame((batter_logs_in_play[player])) %>% 
            mutate(Date = as.Date(Date)) %>% 
            mutate(BIP_direction = if_else (Oppo== 1, "oppo", 
                                   if_else (Center==1, "center", "pull")))
          
          TempLogSavant
        }
      } 
      else {
        player <- which(battersMiLB$NameTeam==input$playername)
        ### if we haven't scraped the data, do that
        if(is.null(batter_logs_MiLB[player][[1]])){
          batter_logs_in_play[player] <- list(as.data.frame(milb_adv_scrape_in_play(battersMiLB$mlbid[player])))
          
          TempLogSavant <- as.data.frame((batter_logs_in_play[player])) %>% 
            mutate(Date = as.Date(Date)) %>% 
            mutate(BIP_direction = if_else (Oppo== 1, "oppo", 
                                   if_else (Center==1, "center", "pull")))
          
          TempLogSavant
        }
      }  
    })
  })
  
  ### PITCHERS
  filteredTable_in_playP <- reactive({
    #progress bar
    withProgress(message = '', style = "notification", value = 0.9, {
      Sys.sleep(0.25)
      
      if(isolate(input$playerlistchoiceP)=="MLB"){
        player <- which(pitchersMLB$NameTeam==input$playernameP)
        ### if we haven't scraped the data, do that
        if(is.null(pitcher_logs_mlb[player][[1]])){
          pitcher_logs_in_play[player] <- list(as.data.frame(milb_adv_scrape_in_play_pitcher(pitchersMLB$mlbid[player])))
          
          TempLogSavant <- as.data.frame((pitcher_logs_in_play[player])) %>% 
            mutate(Date = as.Date(Date)) %>% 
            mutate(BIP_direction = if_else (Oppo== 1, "oppo", 
                                            if_else (Center==1, "center", "pull")))
          
          TempLogSavant
        }
      } 
      else {
        player <- which(pitchersMiLB$NameTeam==input$playernameP)
        ### if we haven't scraped the data, do that
        if(is.null(pitcher_logs_MiLB[player][[1]])){
          pitcher_logs_in_play[player] <- list(as.data.frame(milb_adv_scrape_in_play_pitcher(pitchersMiLB$mlbid[player])))
          
          TempLogSavant <- as.data.frame((pitcher_logs_in_play[player])) %>% 
            mutate(Date = as.Date(Date)) %>% 
            mutate(BIP_direction = if_else (Oppo== 1, "oppo", 
                                            if_else (Center==1, "center", "pull")))
          
          TempLogSavant
        }
      }  
    })
  })
  
  ### HITTERS
  # observe({ 
  #   # FanGraphs Tab - find URL to iframe in
  #   if(input$playerlistchoice=="MLB"){
  #     player <- which(battersMLB$NameTeam==input$playername)
  #     test <<- paste0("http://www.fangraphs.com/statss.aspx?playerid=",
  #                   battersMLB$playerid[player])
  #   } else {
  #     player <- which(battersMiLB$NameTeam==input$playername)
  #     test <<- paste0("http://www.fangraphs.com/statss.aspx?playerid=",
  #                     battersMiLB$playerid[player])
  #   }
  # })
  
  observe({ 
    # Prospects Live Tab - find URL to iframe in
    if(input$playerlistchoice=="MiLB"){
      player <- which(battersMiLB$NameTeam==input$playername)
      test2 <<- paste0("https://www.prospectslive.com/search?q=",
                      battersMiLB$Name[player])
    } else {
      player <- which(battersMLB$NameTeam==input$playername)
      test2 <<- paste0("https://www.prospectslive.com/search?q=",
                      battersMLB$Name[player])
    }
  })
  
  # create iframe for Fangraphs tab
  # output$FGframe <- renderUI({
  #   input$createGraph
  #   my_test <- tags$iframe(src=test, height=1200, width=1200)
  #   print(my_test)
  #   my_test
  # })
  
  # create iframe for Prospects live tab
  output$PLframe <- renderUI({
    input$createGraph
    my_test <- tags$iframe(src=test2, height=1200, width=1200)
    print(my_test)
    my_test
  })
  
  ### Pitchers
  # observe({ 
  #   # FanGraphs Tab - find URL to iframe in
  #   if(input$playerlistchoiceP=="MLB"){
  #     player <- which(pitchersMLB$NameTeam==input$playernameP)
  #     testP <<- paste0("http://www.fangraphs.com/statss.aspx?playerid=",
  #                     pitchersMLB$playerid[player])
  #   } else {
  #     player <- which(pitchersMiLB$NameTeam==input$playernameP)
  #     testP <<- paste0("http://www.fangraphs.com/statss.aspx?playerid=",
  #                     pitchersMiLB$playerid[player])
  #   }
  # })
  
  observe({ 
    # Prospects Live Tab - find URL to iframe in
    if(input$playerlistchoiceP=="MiLB"){
      player <- which(pitchersMiLB$NameTeam==input$playernameP)
      test2P <<- paste0("https://www.prospectslive.com/search?q=",
                        pitchersMiLB$Name[player])
    } else {
      player <- which(pitchersMLB$NameTeam==input$playernameP)
      test2P <<- paste0("https://www.prospectslive.com/search?q=",
                        pitchersMLB$Name[player])
    }
  })
  
  # create iframe for Fangraphs tab
  # output$FGframeP <- renderUI({
  #   input$createGraph
  #   my_test <- tags$iframe(src=testP, height=1200, width=1200)
  #   print(my_test)
  #   my_test
  # })
  # 
  # create iframe for Prospects live tab
  output$PLframeP <- renderUI({
    input$createGraph
    my_test <- tags$iframe(src=test2P, height=1200, width=1200)
    print(my_test)
    my_test
  })
  
  #HITTERS
  output$Plot1 <- renderggiraph({
    
    # run when create graph button is clicked
    input$createGraph
    
    if(input$createGraph>0){
      
    #progress bar
    withProgress(message = "", style = "notification", value = 0.9, {
      Sys.sleep(0.25)
      
    isolate({
      
    # create rolling avg variable
    FilteredGraphData <- filteredTable() %>% 
      mutate(Roll_wRC_plus = round(as.numeric(rollmean(as.numeric(wRC_plus), isolate(as.numeric(input$rolling)), fill=NA, align="right")),0),
             Roll_BA = round(as.numeric(rollmean(as.numeric(AVG), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
             Roll_BABIP = round(as.numeric(rollmean(as.numeric(BABIP), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
             Roll_OBP = round(as.numeric(rollmean(as.numeric(OBP), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
             Roll_SLG = round(as.numeric(rollmean(as.numeric(SLG), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
             Roll_BB = round(as.numeric(rollmean(as.numeric(BB_perc), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
             Roll_K = round(as.numeric(rollmean(as.numeric(K_perc), isolate(as.numeric(input$rolling)), fill=NA, align="right")),3),
             Roll_ISO = round(as.numeric(rollmean(as.numeric(ISO), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
             Roll_SB = round(as.numeric(rollmean(as.numeric(SB_per_600), isolate(input$rolling), fill=NA,align="right")),0),
             Roll_SB_Attempts = round(as.numeric(rollmean(as.numeric(SB_attempts_per_600), isolate(input$rolling), fill=NA,align="right")),0),
             Roll_FB = round(as.numeric(rollmean(as.numeric(FBPU), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_GB = round(as.numeric(rollmean(as.numeric(GB), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_LD = round(as.numeric(rollmean(as.numeric(LD), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_PU = round(as.numeric(rollmean(as.numeric(PU), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_HRFB = round(as.numeric(rollmean(as.numeric(HR), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(FBPU), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_Pull = round(as.numeric(rollmean(as.numeric(Pull), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_Center = round(as.numeric(rollmean(as.numeric(Center), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
             Roll_Oppo = round(as.numeric(rollmean(as.numeric(Oppo), isolate(input$rolling), fill=NA,align="right"))/
                               as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3)
             
        )   
    
    FB_distance <- filteredTable_in_play() %>% 
      filter(hit_trajectory=="fly_ball") %>% 
      group_by(Date) %>% 
      summarise(est_distance = mean(est_distance)) %>% 
      mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
      left_join(FilteredGraphData, by = c("Date","Season","Date2")) %>% 
      filter(!is.na(Level)) %>% 
      mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"),
             Roll_FB_dist = round(as.numeric(rollmean(as.numeric(est_distance), isolate(input$rolling), fill=NA,align="right")),0))  
    
    FB_distance_MiLB <- FB_distance %>% filter(Level != "MLB")
    
    metrics$MiLB_FB_dist <- mean(as.numeric(FB_distance_MiLB$est_distance),na.rm = T)
        
    if(input$playerlistchoice=="MLB"){
      player <- which(battersMLB$Name==isolate(input$playername))
      NameTemp <- battersMLB$Name[player]
      titlevar <- "MiLB & MLB"
    } else {
      player <- which(battersMiLB$Name==isolate(input$playername))
      NameTemp <- battersMiLB$Name[player]
      titlevar <- "MiLB"
    }
    
    if(1 %in% input$graphControl){alphaLine="solid"} else {alphaLine="blank"} 
    if(2 %in% input$graphControl){alphaPoint=1} else {alphaPoint=0}
    if(3 %in% input$graphControl){alphaAvg=1} else {alphaAvg=0}  
      
    yearlower <- isolate(input$yearlower)
    yearupper <- isolate(input$yearupper)
    FilteredGraphData <- FilteredGraphData %>% 
      filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
      filter(Season<=ifelse(yearupper=="",3000,yearupper))  
    
    FB_distance <- FB_distance %>% 
      filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
      filter(Season<=ifelse(yearupper=="",3000,yearupper))
    
    footerComment <- "-----------------------------------------------------------------------------------------------------------------------------------
Data from Fangraphs.com & MLB.com, Pulled w/ help using Bill Petti's baseballr package
Create graphs at SmadaPlaysFantasy.com/MiLB_Trend_Graphs, Twitter: @smada_bb"
    
    if(input$metric == "K%"){
      ### K% graph
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_K)) + 
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_K, group=Level, color=Level),size=1.5) + 
        geom_point(aes(y=Roll_K, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept=metrics$MiLB_K_perc,linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        ggtitle(paste(isolate(input$playername),titlevar,"K% rolling",isolate(as.numeric(input$rolling)),"game average"), 
          subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) + 
        labs(x="Month", y="K Rate") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        scale_y_continuous(limits=c(0, NA)) +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_K,"% K Rate"), sep='\n'), 
                                   data_id= Date, color=Level),size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "BB%"){
      ### BB% graph
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BB)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_BB, group=Level, color=Level), size=1.5) + 
        geom_point(aes(y=Roll_BB, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_BB_perc, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"BB% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) + 
        labs(x="Month", y="BB Rate") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        scale_y_continuous(limits=c(0, NA)) +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_BB, "% BB Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F,linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "SLG"){
      ### SLG graph
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_SLG)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_SLG, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_SLG,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_Slug, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"SLG rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="SLG") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_SLG, " SLG"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;") )
    } else if(input$metric == "wRC+"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_wRC_plus)) +
        geom_line(aes(y=Roll_wRC_plus,group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_wRC_plus, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept=metrics$MiLB_wRC_plus, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        geom_hline(yintercept=100) + 
        ggtitle(paste(isolate(input$playername),titlevar,"wRC+ rolling",isolate(as.numeric(input$rolling)),"game average"), 
                subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="wRC+") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date),
                                                   Level, paste0(Roll_wRC_plus," wRC+"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
      
    } else if(input$metric == "ISO"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_ISO)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_ISO,group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_ISO,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_smooth(se=F, linetype=alphaLine) +
        geom_hline(yintercept=metrics$MiLB_ISO,linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"ISO rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="ISO") +
        scale_x_date(date_breaks = "1 month", date_labels="%b")+
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_ISO," ISO"),sep='\n'), 
                                   data_id= Date, color=Level),size=1) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "BA"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BA)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_BA,group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_BA,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_smooth(se=F, linetype=alphaLine) +
        geom_hline(yintercept=metrics$MiLB_BA, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"BA rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="BA") +
        scale_x_date(date_breaks = "1 month", date_labels="%b")+
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_BA," BA"),sep='\n'), 
                                   data_id= Date, color=Level),size=1) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "BABIP"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BABIP)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_BABIP,group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_BABIP,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_smooth(se=F, linetype=alphaLine) +
        geom_hline(yintercept=metrics$MiLB_BABIP, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"BABIP rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="BABIP") +
        scale_x_date(date_breaks = "1 month", date_labels="%b")+
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_BABIP," BABIP"),sep='\n'), 
                                   data_id= Date, color=Level),size=1) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "OBP"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_OBP)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_OBP, group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_OBP, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_smooth(se=F, linetype=alphaLine) +
        geom_hline(yintercept=metrics$MiLB_OBP, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"OBP rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="OBP") +
        scale_x_date(date_breaks = "1 month", date_labels="%b")+
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_OBP," OBP"),sep='\n'), 
                                   data_id= Date, color=Level),size=1) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "SB & Attempts per 600 PA"){
      ### SB & Attempts
      g <- ggplot(data= FilteredGraphData, aes(as.Date(Date2), Roll_SB)) +
        geom_line(aes(y=Roll_SB, group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_SB, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_SB_per_600, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        ggtitle(paste(isolate(input$playername),titlevar,"SBs & Attempts per 600 PA rolling",isolate(as.numeric(input$rolling)),"game average"), 
                subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="SBs & Attempts") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_SB," SB per 600 PA"), paste0(Roll_SB_Attempts," Attempts per 600 PA"), 
                                                   paste0(round(100*Roll_SB/Roll_SB_Attempts,0),"% Success Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(aes(y=Roll_SB),se=F, linetype=alphaLine) +
        geom_smooth(aes(y=Roll_SB_Attempts),se=F,color="Red") +
        theme_smada()
      gg <- girafe(print(g), width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5), opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "HR/FB"){
      ### FB% graph
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_HRFB)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_HRFB, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_HRFB, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_HRFB, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"HR/FB rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="HR/FB") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_HRFB, "% HR/FB"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;") )
    } else if(input$metric == "LD%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_LD)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_LD, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_LD,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_LD, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"LD% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="LD%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_LD, "% LD Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;")) 
    } else if(input$metric == "PU%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_PU)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_PU, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_PU,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_PU, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"PU% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="PU%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_PU, "% PU Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))  
    } else if(input$metric == "GB%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_GB)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_GB, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_GB,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_GB, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"GB% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="GB%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_GB, "% GB Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))  
    } else if(input$metric == "FB%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_FB)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_FB, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_FB,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_FB, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"FB% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="FB%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_FB, "% FB Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "Pull%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_Pull)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_Pull, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_Pull,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_Pull, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"Pull% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="Pull%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_Pull, "% Pull Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "Cent%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_Center)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_Center, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_Center, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_Center, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"Cent% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="Cent%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_Center, "% Cent Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "Oppo%"){
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_Oppo)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_Oppo, group=Level, color=Level), size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_Oppo, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_Oppo, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"Oppo% rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="Oppo%") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(100*Roll_Oppo, "% Oppo Rate"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
    } else if(input$metric == "Estimated FB Distance"){
      g <- ggplot(data=FB_distance, aes(as.Date(Date2), Roll_FB_dist)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_FB_dist, group=Level, color=Level), size=1.5) + 
        #ylim(200, NA) +
        geom_point(aes(y=Roll_FB_dist, group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept = metrics$MiLB_FB_dist, linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        # TITLE
        ggtitle(paste(isolate(input$playername),titlevar,"Est. FB Distance rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) +
        labs(x="Month", y="Est. FB Distance") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                                   Level, paste0(Roll_FB_dist, " ft. Est. FB Distance"), sep='\n'), 
                                   data_id= Date, color=Level), size=1) +
        geom_smooth(se=F, linetype=alphaLine) +
        theme_smada()
      gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
      girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;")) 
    }
  })
  })
  }
  })
    
    #PITCHERS
    output$Plot1P <- renderggiraph({
      
      if(input$createGraphP>0){
      
      # run when create graph button is clicked
      input$createGraphP
      
      #progress bar
      withProgress(message = '', style = "notification", value = 0.9, {
        Sys.sleep(0.25)
        
        isolate({
          
          # create rolling avg variable
          FilteredGraphData <- filteredTableP() %>% 
            mutate(#Roll_BA = round(as.numeric(rollmean(as.numeric(AVG), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                   Roll_BABIP = round(as.numeric(rollmean(as.numeric(BABIP), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 3),
                   Roll_K = round(as.numeric(rollmean(as.numeric(SO), isolate(as.numeric(input$rollingP)), fill=NA, align="right"))/
                     as.numeric(rollmean(as.numeric(TBF), isolate(as.numeric(input$rollingP)), fill=NA, align="right")), 3),
                   Roll_BB = round(as.numeric(rollmean(as.numeric(BB), isolate(as.numeric(input$rollingP)), fill=NA, align="right"))/
                                    as.numeric(rollmean(as.numeric(TBF), isolate(as.numeric(input$rollingP)), fill=NA, align="right")), 3),
                   Roll_KBB = Roll_K-Roll_BB,
                   Roll_K9 = round(as.numeric(rollmean(as.numeric(SO), isolate(as.numeric(input$rollingP)), fill=NA, align="right"))*9/
                             as.numeric(rollmean(as.numeric(IP), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 1)        ,
                   Roll_BB9 = round(as.numeric(rollmean(as.numeric(BB), isolate(as.numeric(input$rollingP)), fill=NA, align="right"))*9/
                              as.numeric(rollmean(as.numeric(IP), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 1),
                   Roll_GS = round(as.numeric(rollmean(as.numeric(GS), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 2),
                   Roll_IPG = round(as.numeric(rollmean(as.numeric(IP), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 2),
                   Roll_FIP = round(as.numeric(rollmean(as.numeric(FIP), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 2),
                   Roll_ERA = round(as.numeric(rollmean(as.numeric(ER), isolate(as.numeric(input$rollingP)), fill=NA, align="right"))*9/
                                    as.numeric(rollmean(as.numeric(IP), isolate(as.numeric(input$rollingP)), fill=NA, align="right")), 3),
                   Roll_WHIP = round((as.numeric(rollmean(as.numeric(BB), isolate(as.numeric(input$rollingP)), fill=NA, align="right")) + 
                                      as.numeric(rollmean(as.numeric(H), isolate(as.numeric(input$rollingP)), fill=NA, align="right")))/
                                      as.numeric(rollmean(as.numeric(IP), isolate(as.numeric(input$rollingP)), fill=NA, align="right")), 2),
                   Roll_BA = round(as.numeric(rollmean(as.numeric(AVG), isolate(as.numeric(input$rollingP)), fill=NA,align="right")), 3),
                   Roll_FB = round(as.numeric(rollmean(as.numeric(FBPU), isolate(input$rollingP), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_GB = round(as.numeric(rollmean(as.numeric(GB), isolate(input$rollingP), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_LD = round(as.numeric(rollmean(as.numeric(LD), isolate(input$rollingP), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_PU = round(as.numeric(rollmean(as.numeric(PU), isolate(input$rollingP), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_HRFB = round(as.numeric(rollmean(as.numeric(HR), isolate(input$rollingP), fill=NA,align="right"))/
                                       as.numeric(rollmean(as.numeric(FBPU), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_Pull = round(as.numeric(rollmean(as.numeric(Pull), isolate(input$rollingP), fill=NA,align="right"))/
                                       as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_Center = round(as.numeric(rollmean(as.numeric(Center), isolate(input$rollingP), fill=NA,align="right"))/
                                         as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3),
                   Roll_Oppo = round(as.numeric(rollmean(as.numeric(Oppo), isolate(input$rollingP), fill=NA,align="right"))/
                                       as.numeric(rollmean(as.numeric(in_play), isolate(input$rollingP), fill=NA,align="right")),3)
                   
            )   
          
          FB_distance <- filteredTable_in_playP() %>% 
            filter(hit_trajectory=="fly_ball") %>% 
            group_by(Date) %>% 
            summarise(est_distance = mean(est_distance)) %>% 
            mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
            left_join(FilteredGraphData, by = c("Date","Season","Date2")) %>% 
            filter(!is.na(Level)) %>% 
            mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"),
                   Roll_FB_dist = round(as.numeric(rollmean(as.numeric(est_distance), isolate(input$rollingP), fill=NA,align="right")),0))  
          
          FB_distance_MiLB <- FB_distance %>% filter(Level != "MLB")
          
          metricsP$MiLB_FB_dist <- mean(as.numeric(FB_distance_MiLB$est_distance),na.rm = T)
          
          if(input$playerlistchoiceP=="MLB"){
            player <- which(pitchersMLB$NameTeam==isolate(input$playernameP))
            TitleName <- pitchersMLB$Name[player]
            TitleLevel <- FilteredGraphData$Level[length(FilteredGraphData$Level)]
            TitleTeam <- FilteredGraphData$Team[length(FilteredGraphData$Team)]
            titlevar <- "MiLB & MLB"
          } else {
            player <- which(pitchersMiLB$NameTeam==isolate(input$playernameP))
            TitleName <- pitchersMiLB$Name[player]
            TitleLevel <- FilteredGraphData$Level[length(FilteredGraphData$Level)]
            TitleTeam <- FilteredGraphData$Team[length(FilteredGraphData$Team)]
            titlevar <- "MiLB"
          }
          
          titlefull <- paste(TitleName,
                             paste0("(",TitleTeam," ",TitleLevel,")"),
                             titlevar, input$metricP, "rolling",
                             input$rollingP,"game average")
          
          if(1 %in% input$graphControlP){alphaLine="solid"} else {alphaLine="blank"} 
          if(2 %in% input$graphControlP){alphaPoint=1} else {alphaPoint=0}
          if(3 %in% input$graphControlP){alphaAvg=1} else {alphaAvg=0}  
          
          yearlower <- isolate(input$yearlowerP)
          yearupper <- isolate(input$yearupperP)
          FilteredGraphData <- FilteredGraphData %>% 
            filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
            filter(Season<=ifelse(yearupper=="",3000,yearupper))  
          
          FB_distance <- FB_distance %>% 
            filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
            filter(Season<=ifelse(yearupper=="",3000,yearupper))
          
          footerComment <- "-----------------------------------------------------------------------------------------------------------------------------------
          Data from Fangraphs.com & MLB.com, Pulled w/ help using Bill Petti's baseballr package
          Create graphs at SmadaPlaysFantasy.com/MiLB_Trend_Graphs, Twitter: @smada_bb"
          
          if(input$metricP == "K%"){
            ### K% graph
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_K)) + 
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_K, group=Level, color=Level),size=1.5) + 
              geom_point(aes(y=Roll_K, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_hline(yintercept=metricsP$MiLB_K_perc,linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) + 
              labs(x="Month", y="K Rate") +
              scale_x_date(date_breaks = "1 month", date_labels="%b") +
              scale_y_continuous(limits=c(0, NA)) +
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_K,"% K Rate"), sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              geom_smooth(se=F, linetype=alphaLine) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "BB%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BB)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_BB, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_BB, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_BB_perc, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="BB%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b") +
              scale_y_continuous(limits=c(0, NA)) +
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_BB,"% BB%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "K-BB%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_KBB)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_KBB, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_KBB, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_KBB, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="K-BB%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b") +
              scale_y_continuous(limits=c(0, NA)) +
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_KBB,"% K-BB%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))  
          } else if(input$metricP == "FIP"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_FIP)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_FIP, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_FIP, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_FIP, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="FIP") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_FIP," FIP"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "ERA"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_ERA)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_ERA, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_ERA, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_ERA, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="ERA") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_ERA," ERA"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "WHIP"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_WHIP)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_WHIP, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_WHIP, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_WHIP, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="WHIP") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_WHIP," WHIP"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "BA Against"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BA)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_BA, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_BA, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_BA, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="BA Against") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_BA," BA Against"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;")) 
          } else if(input$metricP == "K/9"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_K9)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_K9, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_K9, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_K9, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="K/9") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_K9," K/9"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "BB/9"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BB9)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_BB9, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_BB9, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_BB9, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="BB/9") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_BB9," BB/9"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))  
          } else if(input$metricP == "BABIP"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_BABIP)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_BABIP, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_BABIP, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_BABIP, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="BABIP") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_BABIP," BABIP"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "HR/FB"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_HRFB)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_HRFB, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_HRFB, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_HRFB, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="HR/FB") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_HRFB," HR/FB"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "GS%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_GS)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_GS, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_GS, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_GS, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="GS%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_GS," GS%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "Oppo%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_Oppo)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_Oppo, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_Oppo, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_Oppo, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="Oppo%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_Oppo," Oppo%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "Cent%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_Center)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_Center, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_Center, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_Center, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="Cent%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_Center," Cent%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "Pull%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_Pull)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_Pull, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_Pull, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_Pull, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="Pull%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_Pull," Pull%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "Estimated FB Distance"){
            g <- ggplot(data=FB_distance, aes(as.Date(Date2), Roll_FB_dist)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_FB_dist, group=Level, color=Level), size=1.5) + 
              #ylim(200, NA) +
              geom_point(aes(y=Roll_FB_dist, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_hline(yintercept = metricsP$MiLB_FB_dist, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="Est. FB Distance") +
              scale_x_date(date_breaks = "1 month", date_labels="%b") +
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_FB_dist, " ft. Est. FB Distance"), sep='\n'), 
                                         data_id= Date, color=Level), size=1) +
              geom_smooth(se=F, linetype=alphaLine) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;")) 
          } else if(input$metricP == "FB%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_FB)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_FB, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_FB, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_FB, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="FB%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_FB," FB%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "LD%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_LD)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_LD, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_LD, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_LD, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="LD%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_LD," LD%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "PU%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_PU)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_PU, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_PU, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_PU, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="PU%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_PU," PU%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "GB%"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_GB)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_GB, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_GB, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_GB, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="GB%") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(100*Roll_GB," GB%"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          } else if(input$metricP == "IP/G"){
            g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_IPG)) +
              # Line, Points, Smoothed Line
              geom_line(aes(y=Roll_IPG, group=Level, color=Level),size=1.5) + 
              ylim(0, NA) +
              geom_point(aes(y=Roll_IPG, group=Level, color=Level), size=2, alpha=alphaPoint) +
              geom_smooth(se=F, linetype=alphaLine) +
              geom_hline(yintercept=metricsP$MiLB_IPG, linetype="dashed", alpha=alphaAvg) + 
              facet_wrap(~Season) +
              # TITLE
              ggtitle(titlefull, 
                      subtitle = "Dashed Line = MiLB Career Average") +
              labs(caption = footerComment) +
              labs(x="Month", y="IP/G") +
              scale_x_date(date_breaks = "1 month", date_labels="%b")+
              geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rollingP)," Games up to ", Date), 
                                                         Level, paste0(Roll_IPG," IP/G"),sep='\n'), 
                                         data_id= Date, color=Level),size=1) +
              theme_smada()
            gg <- girafe(print(g),width=1, width_svg = 12.5, height_svg = 6.25)
            girafe_options(gg, opts_zoom(max = 5),opts_hover(css = "fill:red;r:4pt;"))
          }
        })
      })
      }
  })

  output$Plot2 <- renderPlot(width = 1200, height=600, {
    input$createGraph
    
    if(input$createGraph>0){
    
    #progress bar
    withProgress(message = '', style = "notification", value = 0.5, {
      Sys.sleep(0.25)
      
      isolate({
        
        FilteredGraphData <- filteredTable() %>% 
          mutate(Roll_wRC_plus = round(as.numeric(rollmean(as.numeric(wRC_plus), isolate(as.numeric(input$rolling)), fill=NA, align="right")),0),
                 Roll_BA = round(as.numeric(rollmean(as.numeric(AVG), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                 Roll_BABIP = round(as.numeric(rollmean(as.numeric(BABIP), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                 Roll_OBP = round(as.numeric(rollmean(as.numeric(OBP), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                 Roll_SLG = round(as.numeric(rollmean(as.numeric(SLG), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                 Roll_BB = round(as.numeric(rollmean(as.numeric(BB_perc), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                 Roll_K = round(as.numeric(rollmean(as.numeric(K_perc), isolate(as.numeric(input$rolling)), fill=NA, align="right")),3),
                 Roll_ISO = round(as.numeric(rollmean(as.numeric(ISO), isolate(as.numeric(input$rolling)), fill=NA,align="right")),3),
                 Roll_SB = round(as.numeric(rollmean(as.numeric(SB_per_600), isolate(input$rolling), fill=NA,align="right")),0),
                 Roll_SB_Attempts = round(as.numeric(rollmean(as.numeric(SB_attempts_per_600), isolate(input$rolling), fill=NA,align="right")),0),
                 Roll_FB = round(as.numeric(rollmean(as.numeric(FBPU), isolate(input$rolling), fill=NA,align="right"))/
                                   as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_GB = round(as.numeric(rollmean(as.numeric(GB), isolate(input$rolling), fill=NA,align="right"))/
                                   as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_LD = round(as.numeric(rollmean(as.numeric(LD), isolate(input$rolling), fill=NA,align="right"))/
                                   as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_PU = round(as.numeric(rollmean(as.numeric(PU), isolate(input$rolling), fill=NA,align="right"))/
                                   as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_HRFB = round(as.numeric(rollmean(as.numeric(HR), isolate(input$rolling), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(FBPU), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_Pull = round(as.numeric(rollmean(as.numeric(Pull), isolate(input$rolling), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_Center = round(as.numeric(rollmean(as.numeric(Center), isolate(input$rolling), fill=NA,align="right"))/
                                       as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3),
                 Roll_Oppo = round(as.numeric(rollmean(as.numeric(Oppo), isolate(input$rolling), fill=NA,align="right"))/
                                     as.numeric(rollmean(as.numeric(in_play), isolate(input$rolling), fill=NA,align="right")),3)
          )   
        
        FB_distance <- filteredTable_in_play() %>% 
          filter(hit_trajectory=="fly_ball") %>% 
          group_by(Date) %>% 
          summarise(est_distance = mean(est_distance)) %>% 
          mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
          left_join(FilteredGraphData, by = c("Date","Season","Date2")) %>% 
          filter(!is.na(Level)) %>% 
          mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"),
                 Roll_FB_dist = round(as.numeric(rollmean(as.numeric(est_distance), isolate(input$rolling), fill=NA,align="right")),0))  
        
        FB_distance_MiLB <- FB_distance %>% filter(Level != "MLB")
        
        metrics$MiLB_FB_dist <- mean(as.numeric(FB_distance_MiLB$est_distance),na.rm = T)
        
        if(input$playerlistchoice=="MLB"){
          player <- which(battersMLB$Name==input$playername)
          titlevar <- "MiLB & MLB"
        } else {
          player <- which(battersMiLB$Name==input$playername)
          titlevar <- "MiLB"
        }
        
        if(1 %in% input$graphControl){alphaLine="density_ridges"} else {alphaLine="binline"} 
        if(2 %in% input$graphControl){alphaPoint=T} else {alphaPoint=F}
        if(3 %in% input$graphControl){alphaAvg=1} else {alphaAvg=0}  
        
        yearlower <- isolate(input$yearlower)
        yearupper <- isolate(input$yearupper)
        FilteredGraphData <- FilteredGraphData %>% 
          filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
          filter(Season<=ifelse(yearupper=="",3000,yearupper))    
        
        FB_distance <- FB_distance %>% 
          filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
          filter(Season<=ifelse(yearupper=="",3000,yearupper))
        
        footerComment <- "-----------------------------------------------------------------------------------------------------------------------------------
        Data from Fangraphs.com & MLB.com, Pulled w/ help using Bill Petti's baseballr package
        Create graphs at SmadaPlaysFantasy.com/MiLB_Trend_Graphs, Twitter: @smada_bb"
        
        if(input$metric == "K%"){
          ### K% graph
          ggplot(data=FilteredGraphData, aes(x=Roll_K, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            #scale_point_color_hue(l = 40) +
            geom_vline(xintercept=metrics$MiLB_K_perc, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="K%", y="Season") +
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"K% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
          
        } else if(input$metric == "BB%"){
          ### BB% graph
          ggplot(data=FilteredGraphData, aes(x=Roll_BB, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_BB_perc, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="BB%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"BB% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "SLG"){
          ggplot(data=FilteredGraphData, aes(x=Roll_SLG, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_Slug, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="SLG", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"SLG rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
          
        } else if(input$metric == "wRC+"){
          ggplot(data=FilteredGraphData, aes(x=Roll_wRC_plus, y=as.factor(Season),fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=100) +
            geom_vline(xintercept=metrics$MiLB_wRC_plus,linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="wRC+", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"wRC+ rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
          
        } else if(input$metric == "ISO"){
          ggplot(data=FilteredGraphData, aes(x=Roll_ISO, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_ISO,linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="ISO", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"ISO rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "BA"){
          ggplot(data=FilteredGraphData, aes(x=Roll_BA, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_BA, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="BA", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"BA rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "BABIP"){
          ggplot(data=FilteredGraphData, aes(x=Roll_BABIP, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_BABIP, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="BABIP", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"BABIP rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment) 
        } else if(input$metric == "OBP"){
          ggplot(data=FilteredGraphData, aes(x=Roll_OBP, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_OBP, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="OBP", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"OBP rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "FB%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_FB, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_FB, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="FB%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"FB% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "GB%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_GB, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_GB, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="GB%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"GB% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "LD%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_LD, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_LD, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="LD%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"LD% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "PU%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_PU, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_PU, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="PU%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"PU% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "HR/FB"){
          ggplot(data=FilteredGraphData, aes(x=Roll_HRFB, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_HRFB, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="HR/FB", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"HR/FB rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)    
        } else if(input$metric == "SB & Attempts per 600 PA"){
          #   ### SB & Attempts
          ggplot(data=FilteredGraphData, aes(x=Roll_SB, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_SB_per_600, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="SB per 600 PA", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"SB per 600 PA rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment) 
        } else if(input$metric == "Pull%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_Pull, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_Pull, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="Pull%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"Pull% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "Cent%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_Center, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_Center, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="Cent%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"Cent% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "Oppo%"){
          ggplot(data=FilteredGraphData, aes(x=Roll_Oppo, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_Oppo, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="Oppo%", y="Season")+
            scale_x_continuous(labels = scales::percent) +
            ggtitle(paste(isolate(input$playername),titlevar,"Oppo% rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)
        } else if(input$metric == "Estimated FB Distance"){
          ggplot(data=FB_distance, aes(x=Roll_FB_dist, y=as.factor(Season), fill=Level)) +
            geom_density_ridges(stat=alphaLine, alpha=.4, scale=.9, aes(point_color=Level, point_fill=Level), 
                                jittered_points=alphaPoint, scale=.9, quantile_lines=T, quantiles=2) +
            geom_vline(xintercept=metrics$MiLB_FB_dist, linetype="dashed", alpha=alphaAvg)  +
            theme_smada() +
            labs(x="Est. FB Distance", y="Season")+
            ggtitle(paste(isolate(input$playername),titlevar,"Est. FB Distance rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average") +
            labs(caption = footerComment)   
        }
      })
    }) 
    }
  })

  output$Plot3 <- renderggiraph({
    input$updatespray
    
    if(input$createGraph>0){
    
    #progress bar
    withProgress(message = '', style = "notification", value = 0.5, {
      Sys.sleep(.25)
      
      refine = input$refine
      
      isolate({
        
        Filtered_spray <- filteredTable_in_play() %>% mutate(Season = year(Date)) %>% 
          mutate(play_result = if_else(is_hit_into_play_basehit == 1, event_and_next_event_type, "Out"))
        
        yearlower <- input$yearlower
        yearupper <- input$yearupper
        Filtered_spray <- Filtered_spray %>% 
          filter(Season>=ifelse(yearlower=="",0,yearlower)) %>% 
          filter(Season<=ifelse(yearupper=="",3000,yearupper))
        
        yearstitle <- if (min(Filtered_spray$Season) == yearupper) yearupper
        else paste0(" (",min(Filtered_spray$Season)," - ",yearupper,")")
        
        if(input$highlight_result == "All"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy1 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy1 = if_else(event_and_next_event_type == input$highlight_result, 1, 0))
        }
        
        if(input$highlight_trajectory == "All"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy2 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy2 = if_else(hit_trajectory == input$highlight_trajectory, 1, 0))
        }
        
        if(input$highlight_pitch_hand == "All"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy3 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy3 = if_else(pitch_hand == input$highlight_pitch_hand, 1, 0))
        }
        
        if(input$highlight_balls == "All"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy4 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy4 = if_else(balls == as.numeric(input$highlight_balls), 1, 0))
        }
        
        if(input$highlight_strikes == "All"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy5 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy5 = if_else(strikes == as.numeric(input$highlight_strikes), 1, 0))
        } 
        
        if(input$highlight_direction == "All"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy6 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy6 = if_else(BIP_direction == input$highlight_direction, 1, 0))
        } 
        
        if(input$highlight_distance == "0"){
          Filtered_spray <- Filtered_spray %>% mutate(dummy7 = 1)
        } else {
          Filtered_spray <- Filtered_spray %>%
            mutate(dummy7 = if_else(est_distance >= as.numeric(input$highlight_distance), 1, 0))
        } 
        
        Filtered_spray <- Filtered_spray %>%
          mutate(master_dummy = dummy1*dummy2*dummy3*dummy4*dummy5*dummy6*dummy7)
        
        Filtered_spray$color_by <- if (input$color_by == "Hit Trajectory") Filtered_spray$hit_trajectory 
        else if (input$color_by == "Play Result") Filtered_spray$play_result
        else if (input$color_by == "Hit Direction") Filtered_spray$BIP_direction
        
        home_x <- 125
        home_y <- 45
        
        spray_subtitle <- paste(
          if (length(unique(Filtered_spray$master_dummy)) == 1) "Highlighted BIP: 100%" 
          else paste0("Highlighted BIP: ", 
                      round(100*sum(Filtered_spray$master_dummy)/length(Filtered_spray$master_dummy),1),"%"),
          if (input$highlight_result == "All") "" else paste0("| ",input$highlight_result,"s"),
          if (input$highlight_trajectory == "All") "" else paste0("| ",input$highlight_trajectory,"s"),
          if (input$highlight_pitch_hand == "All") "" else paste0("| vs ",input$highlight_pitch_hand,"HPs"),
          if (input$highlight_balls == "All") "" else paste0("| ",input$highlight_balls," balls"),
          if (input$highlight_strikes == "All") "" else paste0("| ",input$highlight_strikes," strikes"),
          if (input$highlight_direction == "All") "" else paste0("| ",input$highlight_direction),
          if (input$highlight_distance == "0") "" else paste0("| >= ",input$highlight_distance," ft")
        )
        
        if(input$distancemarkers == "No"){
          curvecolor = "white"
        } else {
          curvecolor = "blue"
        }
        
        if(input$HeatOverlay == "No"){
        
        g <- ggplot(data = Filtered_spray, aes(x=hc_x,y= 250 - hc_y)) + 
          
          
          #xlim(0,250) +
          ylim(0,250) +
          
          geom_segment(x=home_x, y=home_y, xend=home_x+175, yend=home_y+175) +
          geom_segment(x=home_x, y=home_y, xend=home_x-175, yend=home_y+175) +
          #est_distance = 2.20*sqrt((250-hc_x-125)^2+(250-hc_y-45)^2)+13.1
          geom_curve(aes(x = 215, y = 140, xend = 35, yend = 140), linetype="dashed", color=curvecolor, curvature = .37) +
          geom_curve(aes(x = 251, y = 171, xend = -1, yend = 171), linetype="dashed", color=curvecolor, curvature = .37) +
          geom_point_interactive(aes(tooltip = paste(Date, 
                                                     htmltools::htmlEscape(event_description, TRUE),
                                                     paste0("vs. ", htmltools::htmlEscape(pitcher_name, TRUE)),
                                                     paste0(round(est_distance,0)," ft. Est. Distance"),
                                                     sep='\n'),
                                     data_id=hc_x, color=color_by)) +
          labs(x="", y="") +
          gghighlight(master_dummy==1, use_direct_label = F, use_group_by=F) + 
          ggtitle(paste(input$playername,"Spray Chart",yearstitle), subtitle=spray_subtitle) +
          # annotate("text", x = 50, y = 175, label = "Center:") +
          # annotate("text", x = 125, y = 200, label = "Center:") +
          # annotate("text", x = 200, y = 175, label = "Right:") +
          annotate("text", x = 27, y = 132, label = "300", color=curvecolor, size = 3) +
          annotate("text", x = -7, y = 165, label = "400", color=curvecolor, size = 3) +
          theme_spray()
        girafe(print(g), width=1, width_svg = 7, height_svg = 5)
        } else {
          g <- ggplot(data = Filtered_spray, aes(x=hc_x,y= 250 - hc_y)) + 
            geom_curve(aes(x = 215, y = 140, xend = 35, yend = 140), linetype="dashed", color=curvecolor, curvature = .37) +
            geom_curve(aes(x = 251, y = 171, xend = -1, yend = 171), linetype="dashed", color=curvecolor, curvature = .37) +
            geom_point_interactive(aes(tooltip = paste(Date, 
                                                       htmltools::htmlEscape(event_description, TRUE),
                                                       paste0("vs. ", htmltools::htmlEscape(pitcher_name, TRUE)),
                                                       paste0(round(est_distance,0)," ft. Est. Distance"),
                                                       sep='\n'),
                                       data_id=hc_x, color=color_by)) +
            #xlim(0,250) +
            ylim(0,250) +
            geom_segment(x=home_x, y=home_y, xend=home_x+175, yend=home_y+175) +
            geom_segment(x=home_x, y=home_y, xend=home_x-175, yend=home_y+175) +
            labs(x="", y="") +
            gghighlight(master_dummy==1, use_direct_label=F, use_group_by=F) + 
            
            ggtitle(paste(input$playername,"Spray Chart",yearstitle), subtitle=spray_subtitle) +
            stat_density_2d(aes(fill = ..level.., alpha=.5), geom = "polygon", n=refine ,show.legend=FALSE) +
            annotate("text", x = 27, y = 132, label = "300", color=curvecolor, size = 3) +
            annotate("text", x = -7, y = 165, label = "400", color=curvecolor, size = 3) +
            theme_spray()
          girafe(print(g), width=1, width_svg = 7, height_svg = 5)
        } 
        
      })
    })
    }
  })
  
  output$logo1 <- renderImage({
    input$createGraph
    if(input$createGraph>0){
      list(
        src = paste("www/PL_icon_slate.png"),
        contentType = "image/png",
        width = 120,
        alt = ""
      )
    } else {
      list(
        src = paste("www/dummy.png"),
        contentType = "image/png",
        width = 50,
        alt = ""
      )
    }
    }, deleteFile = FALSE)
  
  output$logo1P <- renderImage({
    input$createGraph
    if(input$createGraphP>0){
      list(
        src = paste("www/PL_icon_slate.png"),
        contentType = "image/png",
        width = 120,
        alt = ""
      )
    } else {
      list(
        src = paste("www/dummy.png"),
        contentType = "image/png",
        width = 50,
        alt = ""
      )
    }
  }, deleteFile = FALSE)
  
  output$logo2 <- renderImage({
    input$createGraph
    if(input$createGraph>0){
      list(
        src = paste("www/PL_icon_slate.png"),
        contentType = "image/png",
        width = 120,
        alt = ""
      )
    } else {
      list(
        src = paste("www/dummy.png"),
        contentType = "image/png",
        width = 50,
        alt = ""
      )
    }
  }, deleteFile = FALSE)
  
  output$logo3 <- renderImage({
    input$createGraph
    input$updatespray
    if(input$createGraph>0){
      list(
        src = paste("www/PL_icon_slate.png"),
        contentType = "image/png",
        width = 120,
        alt = ""
      )
    } else {
      list(
        src = paste("www/dummy.png"),
        contentType = "image/png",
        width = 50,
        alt = ""
      )
    }
  }, deleteFile = FALSE)
  
  
}
