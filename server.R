
require(shiny)
#require(baseballr) <- don't need this currently b/c I create the funcitons below and haven't figured out downloading github packages to shiny server yet lol
require(tidyverse)
require(lubridate)
require(zoo)
require(ggridges)
require(ggiraph)


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
    separate(Team, into = c("Team","Level"),sep=" ")
  payload
}  

milb_adv_scrape <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=batter&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% mutate(Date = game_date,
                                     GB = if_else(hit_trajectory=="ground_ball",1,0),
                                     FB = if_else(hit_trajectory=="fly_ball",1,0),
                                     LD = if_else(hit_trajectory=="line_drive",1,0),
                                     PU = if_else(hit_trajectory=="popup",1,0))
}

theme_smada <- function(){
  theme(text = element_text("sans-serif"),
        plot.title = element_text(size=20, vjust=5),
        plot.subtitle = element_text(face="italic"),
        plot.caption = element_text(color="#696969", hjust=0),
        plot.margin = unit(c(1,1,1,1), "cm"),

        axis.title.x = element_text(size = 15, margin = margin(20, 20, 20, 20)),
        axis.title.y = element_text(size = 15, margin = margin(20, 20, 20, 20)), 
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        
        strip.text.x = element_text(size=12, face="bold"),
        
        legend.title = element_text(size=15, vjust = 7, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(),
        legend.box.spacing = unit(1.5,"cm"),
        legend.box.margin = margin(20, 20, 20, 20),
        legend.justification = "top"
  )
}

### Read in MLB Batters
battersMLB <- read.csv("data/mlb_2018_batter_ids.csv",header=T)
battersMiLB <- read.csv("data/milb_2018_batter_ids.csv",header=T)
battersMLB <- battersMLB %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")")))
battersMiLB <- battersMiLB %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")")))

### Create Names list to reference in drop down
MLBnames <- sort(battersMLB$NameTeam)
MiLBnames <- sort(battersMiLB$NameTeam)

year <- "all"
game_rolling <- 15
#player <- which(batters$Name=="Mike Trout")

batter_logs <- NULL
batter_logs_mlb <- NULL
batter_logs_MiLB <- NULL

server <- function(input, output, session) {
  
  metrics <-reactiveValues()
  playerselected <- reactive({input$playername})
  
  # updates player dropdown based on MLB or MiLB selection
  observe({
    withProgress(message = 'Changing Player Pool', style = "notification", value = 0.9, {
      Sys.sleep(0.25)
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
    })
  })
  
  # Create the table that is used to create the plots
  filteredTable <- reactive({
    #progress bar
    withProgress(message = 'Pulling Data', style = "notification", value = 0.9, {
      Sys.sleep(0.25)
    
    # check if MLB or MiLB
    if(isolate(input$playerlistchoice)=="MLB"){
      player <- which(battersMLB$NameTeam==isolate(input$playername))
      
      ### if we haven't scraped the data, do that
      if(is.null(batter_logs_mlb[player][[1]])){
        batter_logs[player] <- list(as.data.frame(milb_batter_game_logs_fg(battersMLB$playerid[player],year)))
        batter_logs_mlb[player] <- list(as.data.frame(batter_game_logs_fg(battersMLB$playerid[player],year)))
        
        # Create a MiLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
        TempLog <- as.data.frame(batter_logs[player]) %>% filter(PA!=0) %>% 
          mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date)),
                 SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA)) 
        
        # Create a temporary MLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
        TempLogMLB <- as.data.frame(batter_logs_mlb[player]) %>% filter(PA!=0) %>% 
          mutate(Season = year(Date),Level = "MLB",Date2 = paste0("2017-",month(Date),"-",day(Date)),
                 SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA))
        
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
        
        metrics$name <- input$playername
        
        # Merge two game logs for MLB players
        Combined_milb_mlbLog <- merge(TempLog,TempLogMLB,all=T)
        
        # Order MiLB levels correctly
        Combined_milb_mlbLog2 <- Combined_milb_mlbLog %>% 
          # reorder the levels to move (R) to the top
          mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
        }
      }
      ### Create table for MiLB selection
      else {
        player <- which(battersMiLB$NameTeam==input$playername)
        
        ### if we haven't scraped the data, do that
        if(is.null(batter_logs_MiLB[player][[1]])){
          batter_logs[player] <- list(as.data.frame(milb_batter_game_logs_fg(battersMiLB$playerid[player],year)))
          
          # Create a temporary MiLB game log that removes 0 PA games & adds Season variable, SB per 600 metrics
          # Create Date2 variable for consistent x-axis 
          TempLog <- as.data.frame(batter_logs[player]) %>% filter(PA!=0) %>% 
            mutate(Season = year(Date), 
                   Date2 = paste0("2017-",month(Date),"-",day(Date)),
                   SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA)) 
          
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
          
          metrics$name <- input$playername
          
          # Order MiLB levels correctly
          Combined_milb_mlbLog2 <- TempLog %>% 
            # reorder the levels to move (R) to the top
            mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
          
        }
      }
    })
  })
  
  #########
  ######### MiLB Savant grab
  # filteredTable <- reactive({
  #   #progress bar
  #   withProgress(message = 'Pulling Data', style = "notification", value = 0.9, {
  #     Sys.sleep(0.25)
  #     
  #     # check if MLB or MiLB
  #     if(input$playerlistchoice=="MLB"){
  #       player <- which(battersMLB$NameTeam==input$playername)
  #       
  #       ### if we haven't scraped the data, do that
  #       if(is.null(batter_logs_mlb[player][[1]])){
  #         batter_logs[player] <- list(as.data.frame(milb_batter_game_logs_fg(battersMLB$playerid[player],year)))
  #         batter_logs_mlb[player] <- list(as.data.frame(batter_game_logs_fg(battersMLB$playerid[player],year)))
  #         
  #         # Create a temporary MiLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
  #         TempLog <- as.data.frame(batter_logs[player]) %>% filter(PA!=0) %>% 
  #           mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date)),
  #                  SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA)) 
  #         
  #         # Create a temporary MLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
  #         TempLogMLB <- as.data.frame(batter_logs_mlb[player]) %>% filter(PA!=0) %>% 
  #           mutate(Season = year(Date),Level = "MLB",Date2 = paste0("2017-",month(Date),"-",day(Date)),
  #                  SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA))
  #         
  #         # Create MiLB career avgs
  #        # metrics$MiLB_BB_perc <- sum(as.numeric(TempLog$BB),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
  #        # metrics$MiLB_K_perc <- sum(as.numeric(TempLog$SO),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
  # 
  #         # Merge two game logs for MLB players
  #         Combined_milb_mlbLog <- merge(TempLog,TempLogMLB,all=T)
  #         
  #         # Order MiLB levels correctly
  #         Combined_milb_mlbLog2 <- Combined_milb_mlbLog %>% 
  #           # reorder the levels to move (R) to the top
  #           mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
  #         
  #       }
  #     }
  #     else {
  #       player <- which(battersMiLB$NameTeam==input$playername)
  #       
  #       ### if we haven't scraped the data, do that
  #       if(is.null(batter_logs_MiLB[player][[1]])){
  #         batter_logs[player] <- list(as.data.frame(milb_batter_game_logs_fg(battersMiLB$playerid[player],year)))
  #         
  #         # Create a temporary MiLB game log that removes 0 PA appearances & adds Season variable, SB per 600 metrics
  #         TempLog <- as.data.frame(batter_logs[player]) %>% filter(PA!=0) %>% 
  #           mutate(Season = year(Date), Date2 = paste0("2017-",month(Date),"-",day(Date)),
  #                  SB_per_600 = as.numeric(SB)*600/as.numeric(PA), SB_attempts_per_600 = (as.numeric(SB)+as.numeric(CS))*600/as.numeric(PA)) 
  #         
  #         # Create MiLB career avgs
  #        # metrics$MiLB_BB_perc <- sum(as.numeric(TempLog$BB),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
  #        # metrics$MiLB_K_perc <- sum(as.numeric(TempLog$SO),na.rm = T)/sum(as.numeric(TempLog$PA),na.rm = T)
  #       
  #         # Order MiLB levels correctly
  #         Combined_milb_mlbLog2 <- TempLog %>% 
  #           # reorder the levels to move (R) to the top
  #           mutate(Level = fct_relevel(Level,"(R)","(A-)","(A)","(A+)","(AA)","(AAA)","MLB"))
  #         
  #       }
  #     }
  #   })
  # })
  
  observe({ 
    # FanGraphs Tab - find URL to iframe in
    if(input$playerlistchoice=="MLB"){
      player <- which(battersMLB$NameTeam==input$playername)
      test <<- paste0("http://www.fangraphs.com/statss.aspx?playerid=",
                    battersMLB$playerid[player])
    } else {
      player <- which(battersMiLB$NameTeam==input$playername)
      test <<- paste0("http://www.fangraphs.com/statss.aspx?playerid=",
                      battersMiLB$playerid[player])
    }
  })
  
  # create iframe for Fangraphs tab
  output$frame <- renderUI({
    input$createGraph
    my_test <- tags$iframe(src=test, height=1200, width=1200)
    print(my_test)
    my_test
  })
  
  output$Plot1 <- renderggiraph({
    
    # run when create graph button is clicked
    input$createGraph
    
    #progress bar
    withProgress(message = 'Creating Graph', style = "notification", value = 0.9, {
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
             Roll_SB_Attempts = round(as.numeric(rollmean(as.numeric(SB_attempts_per_600), isolate(input$rolling), fill=NA,align="right")),0)
      )   
        
    if(input$playerlistchoice=="MLB"){
      player <- which(battersMLB$Name==isolate(input$playername))
      titlevar <- "MiLB & MLB"
    } else {
      player <- which(battersMiLB$Name==isolate(input$playername))
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

    footerComment <- "-----------------------------------------------------------------------------------------------------------------------------------
Data from Fangraphs.com & MLB.com, Pulled w/ help using Bill Petti's baseballr package
Create graphs at SmadaPlaysFantasy.com/MiLB_Trend_Graphs, Twitter: @smada_bb"
    
    if(input$metric == "K%"){
      ### K% graph
      g <- ggplot(data=FilteredGraphData, aes(as.Date(Date2), Roll_K)) + 
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_K,group=Level, color=Level),size=1.5) + 
        geom_point(aes(y=Roll_K,group=Level, color=Level), size=2, alpha=alphaPoint) +
        geom_hline(yintercept=metrics$MiLB_K_perc,linetype="dashed", alpha=alphaAvg) + 
        facet_wrap(~Season) +
        ggtitle(paste(isolate(input$playername),titlevar,"K% rolling",isolate(as.numeric(input$rolling)),"game average"), 
          subtitle = "Dashed Line = MiLB Career Average") +
        labs(caption = footerComment) + 
        labs(x="Month", y="K Rate") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
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
        scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
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
        ggtitle(paste(isolate(input$playername),"MiLB & MLB SLG rolling",isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average") +
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
        ggtitle(paste(isolate(input$playername),titlevar,"wRC+ rolling", isolate(as.numeric(input$rolling)),"game average"), subtitle = "Dashed Line = MiLB Career Average, Solid Line = 100 wRC+") +
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
                subtitle = "Dashed Line = MiLB Career Average, Red Line = Smoothed SB Attempts per 600 PA") +
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
    }
  })
  })
  })
  output$Plot2 <- renderPlot(width = 1200, height=600, {
    input$createGraph
    
    #progress bar
    withProgress(message = 'Creating Graph', style = "notification", value = 0.5, {
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
             Roll_SB_Attempts = round(as.numeric(rollmean(as.numeric(SB_attempts_per_600), isolate(input$rolling), fill=NA,align="right")),0)
      )   
      
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
      ggtitle(paste(isolate(input$playername),titlevar,"wRC+ rolling",isolate(as.numeric(input$rolling)),"game samples"), subtitle = "Dashed Line = MiLB Career Average, Solid Line = 100 wRC+") +
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
    }
    })
    })  
  })
    
}