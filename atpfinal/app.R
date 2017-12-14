library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(magrittr)
library(shiny)
library(shinydashboard)
library(shinythemes)

data <- read.csv(file ="ATP.csv", sep = ",", header = TRUE)

##############################
#### Australian Open Data ####
##############################

# Group by date and filter by Australian Open
matches_won <-
  data %>%
  filter(tourney_name == "Australian Open") %>%
  group_by(tourney_date)

# Count the winner names and arrange by descending order
aussie <-
  matches_won %>%
  count(winner_name) %>%
  arrange(desc(n))

# Name the columns in the dataframe
names(aussie)[1] = 'Date'
names(aussie)[2] = 'Name'
names(aussie)[3] = 'Wins'

# Examines only the selected names
aussie_df <- aussie[aussie$Name %in% c("Andre Agassi", "Roger Federer", "Novak Djokovic"), ]

# Changes the date to date in years
aussie_df$Date <- strftime(parse_date_time(as.character(aussie_df$Date), "%Y%m%d"), "%Y")

# Change names to characters
aussie_df$Name <- as.character(aussie_df$Name)

##########################
#### French Open Data ####
##########################

# Group by date and filter by Roland Garros
french_won <-
  data %>%
  filter(tourney_name == "Roland Garros") %>%
  group_by(tourney_date)

# Count the winner names and arrange by descending order
french <-
  french_won %>%
  count(winner_name) %>%
  arrange(desc(n))

# Name the columns in the dataframe
names(french)[1] = 'Date'
names(french)[2] = 'Name'
names(french)[3] = 'Wins'

# Examines only the selected names
french_df <- french[french$Name %in% c("Bjorn Borg", "Ivan Lendl", "Mats Wilander", "Gustavo Kuerten", "Rafael Nadal"), ]

# Changes the date to date in years
french_df$Date <- strftime(parse_date_time(as.character(french_df$Date), "%Y%m%d"), "%Y")

# Change names to characters
french_df$Name <- as.character(french_df$Name)

########################
#### Wimbledon Data ####
########################

# Group by date and filter by Wimbledon
wimby_won <- 
  data %>%
  filter(tourney_name == "Wimbledon") %>%
  group_by(tourney_date)

# Count the winner names and arrange by descending order
wimby <-
  wimby_won %>%
  count(winner_name) %>%
  arrange(desc(n))

# Name the columns in the dataframe
names(wimby)[1] = 'Date'
names(wimby)[2] = 'Name'
names(wimby)[3] = 'Wins'

# Examines only the selected names
wimby_df <- wimby[wimby$Name %in% c("Bjorn Borg", "Boris Becker", "John Mcenroe", "Novak Djokovic", "Pete Sampras", "Roger Federer"),]

# Changes the date to date in years
wimby_df$Date <- strftime(parse_date_time(as.character(wimby_df$Date), "%Y%m%d"), "%Y")

# Change names to characters
wimby_df$Name <- as.character(wimby_df$Name)

######################
#### US Open Data ####
######################

# Group by date and filter by US Open
us_won <- 
  data %>%
  filter(tourney_name == "US Open") %>%
  group_by(tourney_date)

# Count the winner names and arrange by descending order
us <- 
  us_won %>%
  count(winner_name) %>%
  arrange(desc(n))

# Name the columns in the dataframe
names(us)[1] = 'Date'
names(us)[2] = 'Name'
names(us)[3] = 'Wins'

# Examines only the selected names
us_df <- us[us$Name %in% c("Ivan Lendl", "Jimmy Connors", "John Mcenroe", "Pete Sampras", "Roger Federer"),]

# Changes the date to date in years
us_df$Date <- strftime(parse_date_time(as.character(us_df$Date), "%Y%m%d"), "%Y")

# Change names to characters
us_df$Name <- as.character(us_df$Name)

############################
#### Overall Comparison ####
############################

# Filters to titles won only (7 matches)
aussie_comparative <- 
  aussie_df %>%
  filter(aussie_df$Wins == "7")

# Creates a column with Grandslam name
aussie_comparative$Grandslam <- "Australian Open"

# Filters to titles won only (7 matches)
french_comparative <-
  french_df %>%
  filter(french_df$Wins == "7")

# Creates a column with Grandslam name
french_comparative$Grandslam <- "Roland Garros"

# Filters to titles won only (7 matches)
wimby_comparative <- 
  wimby_df %>%
  filter(wimby_df$Wins == "7")

# Creates a column with Grandslam name
wimby_comparative$Grandslam <- "Wimbledon"

# Filters to titles won only (7 matches)
us_comparative <-
  us_df %>%
  filter(us_df$Wins =="7")

# Creates a column with Grandslam name
us_comparative$Grandslam <- "US Open"

# Binds all of the data frames into one
overall_comparison <- rbind(aussie_comparative, french_comparative, wimby_comparative, us_comparative)


ui <- fluidPage(
  theme = shinytheme("paper"),
  titlePanel("ATP Grandslam Dominance Over Time"),
  hr(),
  
  tabsetPanel(
    tabPanel(title = "Australian Open",
             br(),
             p(style= "font-family:Helvetica",
               "*Players are considered 'dominant' in their era if they have won 3 or more Australian Open titles"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   box(selectInput("AussieName", "Name:",
                                   choices = c("All", aussie_df$Name)))),      
                 hr(),
                 helpText("Select a player from the drop-down menu to examine further")),
               
               mainPanel(
                 fluidRow(
                   plotOutput(outputId = "line")))
             )),
    
    
    tabPanel(title = "Roland Garros",
             br(),
             p(style= "font-family:Helvetica",
               "*Players are considered 'dominant' in their era if they have won 3 or more Roland Garros titles"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   box(selectInput("FrenchName", "Name:",
                                   choices = c("All", french_df$Name)))),
                 hr(),
                 helpText("Select a player from the drop-down menu to examine further")),
               
               mainPanel(
                 fluidRow(
                   plotOutput(outputId = "fline")))
             )
    ),
    
    tabPanel(title = "Wimbledon",
             br(),
             p(style = "font-family:Helvetica",
               "*Players are considered 'dominant' in their era if they have won 3 or more Wimbledon titles"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   box(selectInput("WimbyName", "Name:",
                                   choices = c("All", wimby_df$Name)))),
                 hr(),
                 helpText("Select a player from the drop-down menu to examine further")),
               
               mainPanel(
                 fluidRow(
                   plotOutput(outputId = "wline")))
             )
    ),
    
    tabPanel(title = "US Open",
             br(),
             p(style = "font-family:Helvetica",
               "*Players are considered 'dominant' in their era if they have won 3 or more US Open titles"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   box(selectInput("USName", "Name:",
                                   choices = c("All", us_df$Name)))),
                 hr(),
                 helpText("Select a player from the drop-down menu to examine further")),
               
               mainPanel(
                 fluidRow(
                   plotOutput(outputId = "uline")))
             )),
    
    tabPanel(title = "Overall Comparison",
             br(),
             p(style = "font-family:Helvetica",
               "This comparison looks at each Grandslam and which time frame they were most dominated"),
             hr(),
             
             mainPanel(
               fluidRow(
                 plotOutput(outputId = "oline")))
    )))


server <- function(input, output){
  
  output$line <- renderPlot({
    
    chart.colors <- c("royalblue2", "darkturquoise", "midnightblue")
    
    if (input$AussieName == "Andre Agassi"){
      chart.colors <- c("royalblue2", rep("gray",2))
    } 
    
    if (input$AussieName == "Novak Djokovic"){
      chart.colors <- c(rep("gray",1), "darkturquoise", rep("gray",1))
    }
    
    if (input$AussieName == "Roger Federer"){
      chart.colors <- c(rep("gray",2), "midnightblue")
    }
    
    else if (input$AussieName == "All") {
      chart.colors <- c("royalblue2", "darkturquoise", "midnightblue")
    }
    
    theme_set(theme_bw())
    
    aussie_dominance <- ggplot(aussie_df, aes(x=Date,y=Wins,col=Name,group=Name)) + 
      geom_line(size=1.5) +
      geom_point(size=3.5) +
      labs(title="Dominance Over Time", 
           subtitle="AUSTRALIAN OPEN", 
           caption="Source: ATP", 
           y="Matches Won",
           x="Year",
           color=NULL) +  # title and caption
      scale_x_discrete() +  # change to monthly ticks and labels
      scale_color_manual(values = chart.colors) +  # line color
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title=element_text(size=18),
            plot.title = element_text(size=24),
            plot.subtitle = element_text(size=18),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size = 18))
    
    aussie_dominance 
    
  })
  
  output$fline <- renderPlot({
    
    chart.colors2 <- c("hotpink", "palegreen3", "tomato4", "gold", "coral2")
    
    if (input$FrenchName == "Bjorn Borg"){
      chart.colors2 <- c("hotpink", rep("gray",4))
    } 
    
    if (input$FrenchName == "Gustavo Kuerten"){
      chart.colors2 <- c("gray", "palegreen3", rep("gray",3))
    } 
    
    if (input$FrenchName == "Ivan Lendl"){
      chart.colors2 <- c(rep("gray",2), "tomato4", rep("gray",2))
    } 
    
    if (input$FrenchName == "Mats Wilander"){
      chart.colors2 <- c(rep("gray",3), "gold", rep("gray",1))
    } 
    
    if (input$FrenchName == "Rafael Nadal"){
      chart.colors2 <- c(rep("gray",4), "coral2")
    } 
    
    else if (input$FrenchName == "All") {
      chart.colors2 <- c("hotpink", "palegreen3", "tomato4", "gold", "coral2")
    }
    
    theme_set(theme_bw())
    
    french_dominance <- ggplot(french_df, aes(x=Date,y=Wins,col=Name,group=Name)) + 
      geom_line(size=1.5) +
      geom_point(size=3.5) +
      labs(title="Dominance Over Time", 
           subtitle="ROLAND GARROS", 
           caption="Source: ATP", 
           y="Matches Won",
           x="Year",
           color=NULL) +  # title and caption
      scale_x_discrete() +  # change to monthly ticks and labels
      scale_color_manual(values = chart.colors2) +  # line color
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title=element_text(size=18),
            plot.title = element_text(size=24),
            plot.subtitle = element_text(size=18),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size = 18))
    
    french_dominance  
  })
  
  output$wline <- renderPlot({
    
    chart.colors3 <- c("darkgreen", "darkorchid3", "orange", "midnightblue", "mediumseagreen", "salmon")
    
    if (input$WimbyName == "Bjorn Borg"){
      chart.colors3 <- c("darkgreen", rep("gray",5))
    }
    
    if (input$WimbyName == "Boris Becker"){
      chart.colors3 <- c("gray", "darkorchid3", rep("gray",4))
    }
    
    if (input$WimbyName == "John Mcenroe"){
      chart.colors3 <- c(rep("gray",2), "orange", rep("gray",3))
    }
    
    if (input$WimbyName == "Novak Djokovic"){
      chart.colors3 <- c(rep("gray",3), "midnightblue", rep("gray",2))
    }
    
    if(input$WimbyName == "Pete Sampras"){
      chart.colors3 <- c(rep("gray",4), "mediumseagreen", rep("gray",1))
    }
    
    if (input$WimbyName == "Roger Federer"){
      chart.colors3 <- c(rep("gray",5), "salmon")
    }
    
    else if (input$WimbyName == "All"){
      chart.colors3 <- c("darkgreen", "darkorchid3", "orange", "midnightblue", "mediumseagreen", "salmon")
    }
    
    theme_set(theme_bw())
    
    wimby_dominance <- ggplot(wimby_df, aes(x=Date,y=Wins,col=Name,group=Name)) +
      geom_line(size=1.5) +
      geom_point(size=3.5) +
      labs(title="Dominance Over Time",
           subtitle="WIMBLEDON",
           caption="Source: ATP",
           y="Matches Won",
           x="Year",
           color=NULL) +
      scale_x_discrete() +
      scale_color_manual(values = chart.colors3) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title=element_text(size=18),
            plot.title = element_text(size=24),
            plot.subtitle = element_text(size=18),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size = 18))
    
    wimby_dominance
  })
  
  output$uline <- renderPlot({
    
    chart.colors4 <- c("dodgerblue", "midnightblue", "orange", "orangered", "skyblue4")
    
    if (input$USName == "Ivan Lendl"){
      chart.colors4 <- c("dodgerblue", rep("gray",4))
    }
    
    if (input$USName == "Jimmy Connors"){
      chart.colors4 <- c("gray", "midnightblue", rep("gray",3))
    }
    
    if (input$USName == "John Mcenroe"){
      chart.colors4 <- c(rep("gray",2), "orange", rep("gray",2))
    }
    
    if (input$USName == "Pete Sampras"){
      chart.colors4 <- c(rep("gray",3), "orangered", rep("gray",1))
    }
    
    if (input$USName == "Roger Federer"){
      chart.colors4 <- c(rep("gray",4), "skyblue4")
    }
    
    else if (input$USName == "All"){
      chart.colors4 <- c("dodgerblue", "midnightblue", "orange", "orangered", "skyblue4")
    }
    
    theme_set(theme_bw())
    
    us_dominance <- ggplot(us_df, aes(x=Date,y=Wins,col=Name,group=Name)) +
      geom_line(size=1.5) +
      geom_point(size=3.5) +
      labs(title="Dominance Over Time",
           subtitle="US OPEN",
           caption="Source: ATP",
           y="Matches Won",
           x="Year",
           color=NULL) +
      scale_x_discrete() +
      scale_color_manual(values = chart.colors4) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title=element_text(size=18),
            plot.title = element_text(size=24),
            plot.subtitle = element_text(size=18),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size = 18))
    
    us_dominance
  })
  
  output$oline <- renderPlot({
    
    theme_set(theme_classic())
    
    comparisons <- ggplot(overall_comparison, aes(x=Date,group=Grandslam)) +
      geom_density(aes(fill=factor(Grandslam)), alpha=0.7) + 
      labs(title="Grandslam Dominance Over Time",
           caption="Source: ATP",
           x="Year",
           fill="Grandslam") +
      theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title=element_text(size=18),
            plot.title = element_text(size=24),
            plot.caption = element_text(size = 12),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 14)) # rotate x axis text
    
    comparisons
  })
}
shinyApp(ui = ui, server = server)
