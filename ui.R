
require(shiny)
#require(baseballr) <- don't need this currently b/c I create the funcitons below
require(tidyverse)
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

### Read in MLB/MiLB Batters from 2017/2018
batters2 <- read.csv("data/mlb_2018_batter_ids.csv",header=T)
batters3 <- read.csv("data/milb_2018_batter_ids.csv",header =T)
### Create Names list to reference in drop down
nameselect <- batters3 %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")"))) %>% arrange(NameTeam) %>% select(NameTeam)
nameselect <- as.list(nameselect)

### Read in MLB/MiLB Batters from 2017/2018
pitchers2 <- read.csv("data/mlb_2018_pitcher_ids.csv",header=T)
pitchers3 <- read.csv("data/milb_2018_pitcher_ids.csv",header =T)
### Create Names list to reference in drop down
nameselectP <- pitchers3 %>% mutate(NameTeam = as.character(paste0(Name," (",Team,")"))) %>% arrange(NameTeam) %>% select(NameTeam)
nameselectP <- as.list(nameselectP)

year <- "all"
game_rolling <- 15

batter_logs <- NULL
batter_logs_mlb <- NULL

ui <- fluidPage(
  #tags$a(href="https://www.prospectslive.com",img(src='prospectslive.jpg', align = "middle", width = "100%", height = "auto")),
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "/www/favicon.ico"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js", 
                type="text/javascript"),
    tags$style(
      type="text/css",
      "#image img {max-width: 100%; width: 100%; height: auto}"
    ),
    tags$style(
      paste0(
        ".shiny-progress-notification{",
        "  position:fixed !important;",
        "  top: calc(50%) !important;",
        "  left: calc(25%) !important;",
        "  font-size: 15px !important;",
        "  font-style: bold !important;",
        "  width: 600px !important;",
        "}"
      )
    ),
  HTML("
    <!-- Global site tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-119804730-1'></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
    
      gtag('config', 'UA-119804730-1');
    </script>"
  )
  ),
  HTML("
    <style type='text/css'>
       #titleBar1 {
       height: auto;
       background-color: #3475b4;
       overflow: hidden;
       border-bottom: 1px solid #3475b3;
       -moz-box-shadow:    0px 0px 10px 3px #BBC;
       -webkit-box-shadow: 0px 0px 10px 3px #BBC;
       box-shadow:         0px 0px 10px 3px #BBC;
       }
       #titleBar1 #container1 {
       margin-top: 14px;
       }
       #titleBar1 h1 {
       margin: 0 auto .5em auto;
       padding: .2em;
       color: #EEE;
       text-align: center;
       </style>
       "
  ),
  # Application title
  #titlePanel("Minor Graphs by Prospects Live"),
  # well panel that holds all the selections
  tabsetPanel(type="tabs",
  tabPanel(tags$h4("Hitters"),
           br(),
  wellPanel(
    fluidRow(
      column(4,
             radioButtons("playerlistchoice",
              "Select from either MiLB or MLB player pool:",
              choices = list("MiLB" = "MiLB","MLB" = "MLB"), inline = TRUE, selected = "MiLB"),
             # Choose Player
             selectInput("playername", "Player (delete & type)", choices = nameselect$NameTeam, selected = "Eloy Jimenez (- - -)"),
             # Choose Metric
             selectInput("metric", "Metric", choices = list(
               "wRC+", "ISO", "K%", "BB%", "BABIP", "BA", "OBP", "SLG", "SB & Attempts per 600 PA", 
               "FB%", "GB%", "LD%", "PU%", "HR/FB", "Estimated FB Distance","Pull%", "Cent%", "Oppo%" 
             )),
             # slider for number of games for rolling avg
             sliderInput("rolling",
                          "Games For Rolling Avg:",
                          min = 5, max=162, value = 30),
             # filter season text boxes
             div(style="display:inline-block",textInput("yearlower","Filter Season Start:",value="2016")),
             div(style="display:inline-block",textInput("yearupper","Filter Season End:",value="2018")),
             # graph options check boxes
             checkboxGroupInput("graphControl", label = "Graph Options (Add/Remove)", 
                                choices = list("Smoothed Line" = 1, "Points" = 2, "Career Avg" = 3),
                                selected = c(1,2,3), inline=T),
             
             # button that creates the graphs
             #bookmarkButton(),
             actionButton("createGraph",tags$h4(tags$b("Create Graph")))
      )
    )
  ),
  br(),
 # section for output  
 tabsetPanel(type="tabs",
  tabPanel("Trend",

    column(12,
       div(style="position:absolute; top:60px; left:163px; opacity: 1;", plotOutput("logo1")),
   # interactive output
       div(id='header',style="position:absolute; top:0px; topwidth:1000px;z-index: -1;",ggiraphOutput("Plot1", width = "1200px", height = "600px"))#,
    )     
  ),
  tabPanel("Distribution",
    column(12,
           br(),
           div(style="position:absolute; top:42px; left:143px; opacity: 1;", plotOutput("logo2")),
           div(id='header',style="position:absolute; top:0px; topwidth:1000px;z-index: -1;", plotOutput("Plot2"))
    )
  ),
  tabPanel("Spray Chart",
      fluidRow(           
        column(1,
               br(),
               div(h4("Color:"),align="center")
           ),
        column(2,
               selectInput("color_by","", choices = list("Hit Trajectory", "Play Result", "Hit Direction"))
        )
      ),
      fluidRow(
           column(1,
                  br(),
                  div(h4("Highlight:"),align="center")
           ),
           column(2,
                  selectInput("highlight_result", "Result", 
                              choices = list("All","single", "double", "triple", "home_run", 
                                             "field_out", "force_out", "grounded_into_double_play", "double_play", "field_error", "sac_fly", 
                                             "sac_bunt", "fielders_choice", "fielders_choice_out"))
           ),
           column(2,
                  selectInput("highlight_trajectory", "Hit Trajectory", 
                     choices = list("All","ground_ball", "popup", "fly_ball", "line_drive"))
           ),
           column(2,
                  selectInput("highlight_pitch_hand", "Pitcher Handedness", 
                     choices = list("All","R","L"))
           ),
           column(2,
                  selectInput("highlight_balls", "Balls (Count)", 
                     choices = list("All","0","1","2","3"))
           ),
           column(2,
                  selectInput("highlight_strikes", "Strikes (Count)", 
                     choices = list("All","0","1","2"))
           )
      ),
      fluidRow(
        column(1,
               NULL
        ),
        column(2,
               selectInput("highlight_direction", "Hit Direction", 
                           choices = list("All","pull","center","oppo"))
        ),
        column(2,
               selectInput("highlight_distance", "Distance >=", 
                           choices = list("0","200","300","400"))
        )
      ),
      fluidRow(
        column(1,
               br(),
               div(h4("Options:"),align="center")
        ),
        column(2,
               selectInput("HeatOverlay", "Heat Overlay", 
                           choices = list("No","Yes"), "No")
        ),
        column(2,
               selectInput("distancemarkers", "Distance Markers", 
                           choices = list("No","Yes"), "No")
        )
      ),
      fluidRow(
        column(2,
               br(), 
               div(actionButton("updatespray",tags$h4(tags$b("Update Spray Chart"))),align="center"),
               br()
        )
        
      ),
      column(12,
        div(style="position:absolute; top:80px; left:820px; opacity: 1;", plotOutput("logo3")),
        div(id='header', style="position:absolute;z-index: -1;",ggiraphOutput("Plot3", width = "1200px", height = "600px")),
        br(), br(), br()
      )
  ),
  # tabPanel("Fangraphs Page",
  #    column(12,
  #           br(),
  #           htmlOutput("FGframe")
  #          )
  # ),
  tabPanel("Prospects Live Articles",
           column(12,
                  br(),
                  htmlOutput("PLframe")
           )
  )
 )
 ),
 tabPanel(tags$h4("Pitchers"),
  br(),
  wellPanel(
    fluidRow(
      column(4,
             radioButtons("playerlistchoiceP",
                          "Select from either MiLB or MLB player pool:",
                          choices = list("MiLB" = "MiLB","MLB" = "MLB"), inline = TRUE, selected = "MiLB"),
             # Choose Player
             selectInput("playernameP", "Player (delete & type)", choices = nameselectP$NameTeam, selected = "Forrest Whitley (Astros (AA))"),
             # Choose Metric
             selectInput("metricP", "Metric", choices = list(
               "FIP","ERA","K%","BB%","K-BB%","K/9","BB/9","WHIP","BABIP","BA Against","IP/G", "GS%",
               "FB%", "GB%", "LD%", "PU%", "HR/FB", "Estimated FB Distance","Pull%", "Cent%", "Oppo%" 
             )),
             # slider for number of games for rolling avg
             sliderInput("rollingP",
                         "Games For Rolling Avg:",
                         min = 5, max=162, value = 10),
             # filter season text boxes
             div(style="display:inline-block",
                 textInput("yearlowerP","Filter Season Start:",value="2016")),
             div(style="display:inline-block",
                 textInput("yearupperP","Filter Season End:",value="2018")),
             # graph options check boxes
             checkboxGroupInput("graphControlP", label = "Graph Options (Add/Remove)", 
                                choices = list("Smoothed Line" = 1, "Points" = 2, "Career Avg" = 3),
                                selected = c(1,2,3), inline=T),
             
             # button that creates the graphs
             #bookmarkButton(),
             actionButton("createGraphP",tags$h4(tags$b("Create Graph")))
      )
    )
  ),
  br(),
  # section for output  
  tabsetPanel(type="tabs",
    tabPanel("Trend",
       column(12, 
              div(style="position:absolute; top:60px; left:163px; opacity: 1;", plotOutput("logo1P")),
              # interactive output
              div(id='header',style="position:absolute; top:0px; topwidth:1000px;z-index: -1;",ggiraphOutput("Plot1P", width = "1200px", height = "600px"))
       )
    ),
    # tabPanel("Distribution",
    #    column(12,
    #           br(),
    #           plotOutput("Plot2P")
    #    )
    # ),
    # tabPanel("Spray Chart",
    #          fluidRow(
    #            column(2,
    #                   br(), 
    #                   div(actionButton("updatespray","Update Spray Chart"),align="center"),
    #                   br()
    #            )
    #            
    #          ),
    #          fluidRow(           
    #            column(1,
    #                   br(),
    #                   div(h4("Color:"),align="center")
    #            ),
    #            column(2,
    #                   selectInput("color_by","", choices = list("Hit Trajectory", "Play Result", "Hit Direction"))
    #            )
    #          ),
    #          fluidRow(
    #            column(1,
    #                   br(),
    #                   div(h4("Highlight:"),align="center")
    #            ),
    #            column(2,
    #                   selectInput("highlight_result", "Result", 
    #                               choices = list("All","single", "double", "triple", "home_run", 
    #                                              "field_out", "force_out", "grounded_into_double_play", "double_play", "field_error", "sac_fly", 
    #                                              "sac_bunt", "fielders_choice", "fielders_choice_out"))
    #            ),
    #            column(2,
    #                   selectInput("highlight_trajectory", "Hit Trajectory", 
    #                               choices = list("All","ground_ball", "popup", "fly_ball", "line_drive"))
    #            ),
    #            column(2,
    #                   selectInput("highlight_pitch_hand", "Pitcher Handedness", 
    #                               choices = list("All","R","L"))
    #            ),
    #            column(2,
    #                   selectInput("highlight_balls", "Balls (Count)", 
    #                               choices = list("All","0","1","2","3"))
    #            ),
    #            column(2,
    #                   selectInput("highlight_strikes", "Strikes (Count)", 
    #                               choices = list("All","0","1","2"))
    #            )
    #          ),
    #          fluidRow(
    #            ggiraphOutput("Plot3", width = "1200px", height = "600px"),
    #            br(), br(), br()
    #          )
    # ),
    # tabPanel("Fangraphs Page",
    #          column(12,
    #                 br(),
    #                 htmlOutput("FGframeP")
    #          )
    # ),
    tabPanel("Prospects Live Articles",
             column(12,
                    br(),
                    htmlOutput("PLframeP")
             )
    )
  )
 )
 ),HTML('<div data-iframe-height></div>')
)
