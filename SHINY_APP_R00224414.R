#setting the working directory
setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Data Visuakisation Assignment")


#reading the asiacup data which contains all the matches in the dataset
data<-read.csv("C:\\Users\\shash\\OneDrive\\Desktop\\Data Visuakisation Assignment\\asiacup.csv")

str(data)
#Reading the champions of Asia Cup dataset
champion <- read.csv("C:\\Users\\shash\\OneDrive\\Desktop\\Data Visuakisation Assignment\\champion.csv")
str(champion)
#Reading the ODI batsman  dataset
batsmanodi <- read.csv("C:\\Users\\shash\\OneDrive\\Desktop\\Data Visuakisation Assignment\\batsman data odi.csv")
str(batsmanodi)
#Reading the T20 batsman datset
batsmant20 <- read.csv("C:\\Users\\shash\\OneDrive\\Desktop\\Data Visuakisation Assignment\\batsman data t20i.csv")
str(batsmant20)
#Reading the bowler Odi dataset
bowlerodi <- read.csv("C:\\Users\\shash\\OneDrive\\Desktop\\Data Visuakisation Assignment\\bowler data odi.csv")
str(bowlerodi)
#copying the data into other varibales for safety of original data 
data1 <- data
champion1 <- champion
bat <- batsmanodi
bat20 <- batsmant20
bowl <- bowlerodi
#converting the datset into data frame
data1 <- data.frame(data1)
champion1 <- data.frame(champion1)
bat <- data.frame(bat)
bat20 <- data.frame(bat20)
bowl <- data.frame(bowl)
#converting the variables to factors 
data1$Team <- as.factor((data1$Team))
data1$Opponent <- as.factor(data1$Opponent)
data1$Format <- as.factor(data1$Format)
data1$Toss <- as.factor(data1$Toss)
data1$Selection <- as.factor(data1$Selection)
data1$Result <- as.factor(data1$Result)

str(data1$Result)
#reducing the factor variables in Toss variable
data1$Toss <- ifelse(data1$Toss=='win','Win',ifelse(data1$Toss=='Win','Win','Lose'))
#reducing the factor variables in Result variable
data1$Result <- ifelse(data1$Result=='win','Win',
                       ifelse(data1$Result=='Win','Win',
                              ifelse(data1$Result==' Lose D/L','Lose D/L',
                                     ifelse(data1$Result==' Win D/L', 'Win D/L',
                                            ifelse(data1$Result=='Lose','Lose',
                                                   ifelse(data1$Result=='No Result','No Result','Na'))))))

#reducing the factor variables in Ground variable
data1$Ground <- ifelse(data1$Ground=='Sharjah','UAE',
                       ifelse(data1$Ground=='Colombo(PSS)','Sri Lanka',
                              ifelse(data1$Ground=='Moratuwa','Sri Lanka',
                                     ifelse(data1$Ground=='Kandy','Sri Lanka',
                                            ifelse(data1$Ground=='Dhaka','Sri Lanka',
                                                   ifelse(data1$Ground=='Chattogram','Sri Lanka',
                                                          ifelse(data1$Ground=='Chandigarh','India',
                                                                 ifelse(data1$Ground=='Cuttack','India',
                                                                        ifelse(data1$Ground=='Kolkata','India',
                                                                               ifelse(data1$Ground=='Dambulla','Sri Lanka',
                                                                                      ifelse(data1$Ground=='Lahore','Pakistan',
                                                                                             ifelse(data1$Ground=='Karachi','Pakistan',
                                                                                                    ifelse(data1$Ground=='Mirpur','Pakistan',
                                                                                                           ifelse(data1$Ground=='Fatullah','Pakistan',
                                                                                                                  ifelse(data1$Ground=='Dubai(DSC)','UAE',
                                                                                                                         ifelse(data1$Ground=='Abu Dhabi','UAE',
                                                                                                                                ifelse(data1$Ground=='Colombo(RPS)','Sri Lanka',
                                                                                                                                       ifelse(data1$Ground=='Colombo(SSC)','Sri Lanka','NA'))))))))))))))))))


###################### Visualizations of the data ###################################
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)



# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Add a background image to the header
  tags$div(
    titlePanel(
      tags$h1("Cricket Data Visualization", style = "color: white; text-shadow: 2px 2px #000000;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Choose a plot type", style = "margin-top: 20px;"),
      
      # Add some styling to the radio buttons
      tags$style(HTML("
        .radio-inline {
          display: inline-block;
          margin-right: 10px;
        }
        .radio-inline input[type='radio'] {
          margin-top: 3px;
        }
      ")),
      
      radioButtons(
        inputId = "plot_type",
        label = "",
        choices = list(
          "Scatterplot of Runs Scored vs Wickets Lost" = 1, 
          "Boxplot of Runs Scored by Each Team" = 2,
          "Distribution of Runs Scored by Each Player" = 3,
          "Distribution of Bowling Economy Rate" = 4,
          "Team India's Winning and Losing in Different Grounds" = 5,
          "Champions of the Tournament" = 6,
          "Win and loss percentage based on toss and selection" = 7,
          "Sixes Scored by Each Team" = 8,
          "India Win Percentage against Pakistan" = 9,
          "The players having the best average > 40  in indian team" =10,
          "highest no of wicket takers in odi with best economy" = 11,
          "Number of wins and loseses by each country in Asia cup" = 12,
          "Total Runs scored by each player in Asia Cup (Top 10)" = 13,
          "Correlation between Bowling Average and Economy Rate in ODI Cricket" = 14,
          "Average runs scored by team and year" = 15,
          "Participating Countries in Asia Cup" = 16
        ),
        selected = 1,
        inline = TRUE
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "my_plot")
    )
  ),
  
  # Add some styling to the navigation bar
  tags$head(
    tags$style(HTML("
      .navbar-default {
        background-color: #357EC7;
        border-color: #357EC7;
        margin-bottom: 0px;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-brand:focus {
        color: #ffffff;
      }
      .navbar-default .navbar-nav > li > a {
        color: #ffffff;
      }
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        background-color: #4D4D4D;
      }
      .btn-default {
        background-color: #357EC7;
        border-color: #357EC7;
        color: #ffffff;
      }
      .btn-default:hover,
      .btn-default:focus {
        background-color: #4D4D4D;
        border-color: #4D4D4D;
        color: #ffffff;
      }
    "))
  )
)

# Server
server <- function(input, output) {
  
  output$my_plot <- renderPlot({
    if(input$plot_type == 1) {
      #plotting the relationship between the number of wickets lost and the total runs scored for each team in the Asia Cup tournament
      ggplot(data = data1, aes(x = Wicket.Lost, y = Run.Scored, color = Team)) +
        geom_point(size = 4, alpha = 0.7) + 
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Number of Wickets Lost", y = "Total Runs Scored", 
             title = "Relationship between Runs Scored and Wickets Lost for Each Team in Asia Cup 2018") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              legend.position = "bottom",
              panel.grid.major = element_line(color = "gray", linetype = "dashed")) +
        scale_color_manual(values = c("#003366", "#FF8200", "#FFD700", "#006633", "#FF0000", "#B8860B", "#800080")) +
        guides(color = guide_legend(ncol = 2)) +
        expand_limits(x = c(0, max(data1$Wicket.Lost)+1), y = c(0, max(data1$Run.Scored)+50))
      
    } else if(input$plot_type == 2) {
      #Dplotting the runs scored by each team in Asia Cup
      data1 %>%
        ggplot(aes(x = Team, y = Run.Scored, fill = Team)) +
        geom_boxplot() +
        scale_fill_viridis_d() +
        labs(title = "Runs Scored by Each Team in Asia Cup",
             x = "Country",
             y = "Runs") +
        theme_minimal()+
        theme_bw()
      
    } else if(input$plot_type == 3) {
      #DAggregating data by player and innings and calculating total runs
      batsman_runs_by_innings <- batsmanodi %>%
        group_by(Player.Name, Played) %>%
        summarise(Total_Runs = sum(Runs)) %>%
        arrange(desc(Total_Runs))
      #Distrubution of runs score by each player
      # creating histogram
      ggplot(batsmanodi, aes(x = Runs, fill = Country)) +
        geom_histogram(binwidth = 10, alpha = 0.6) +
        # adding average line
        geom_vline(aes(xintercept = mean(Runs)), color = "red", linetype = "dashed", size = 1) +
        # customize plot appearance
        facet_wrap(~Player.Name, ncol = 4) +
        scale_fill_brewer(palette = "Dark2") +
        labs(title = "Distribution of runs scored by each player for their team",
             x = "Runs") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "lightgrey"))
      
    } else if(input$plot_type == 4){
      #DBowling economy rate in odi cricket
      ggplot(bowlerodi, aes(x =Economy.Rate)) +
        geom_histogram(binwidth = 0.5, fill = "#00BFC4", color = "#000000", alpha = 0.7) +
        geom_density(aes(y = ..density..), fill = "#FC4E07", alpha = 0.3) +
        labs(x = "Bowling Economy Rate", y = "Density", 
             title = "Distribution of Bowling Economy Rate in ODI Cricket",
             subtitle = "Based on Data from ODI matches") +
        theme_minimal() +
        scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 1)) +
        coord_cartesian(xlim = c(0, 12)) +
        annotate("text", x = 9, y = 0.13, label = "", size = 3, fontface = "italic")+
        theme_bw()
    } else if(input$plot_type == 5){
      data2 <- filter(data1, Team == "India")
      da <- select(data2, Team, Ground, Result) %>% 
        group_by(Ground, Result) %>% 
        summarize(n = n())
      # Plotting India's wins and losses in different grounds
      ggplot(da, aes(Ground, n, fill = Result)) +
        geom_bar(stat = "identity", position = "stack", color = "white") +
        geom_text(aes(label = n), color = "white",size = 3 , position = position_stack(vjust = 0.5),) +
        scale_fill_manual(values = c("#FF8200", "#006633","gray"), name = "Result", labels = c("Loss", "Win")) +
        facet_wrap(~ Ground, ncol = 2, scales = "free_x") +
        labs(x = "Ground", y = "Count", 
             title = "Team India's Wins and Losses in Different Grounds") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", color = "white"))+
        theme_bw()
      
    }  else if(input$plot_type == 6){
      
      #Dcounting how many times they have become champion
      pltdta <- champion1 %>% 
        group_by(Champion) %>% 
        summarize(n = n())
      
      
      #Dplotting the nof times they have become champion
      
      ggplot(pltdta, aes(x = Champion, y = n, fill = Champion)) + 
        geom_col() +
        geom_text(aes(label = n), vjust = -0.5, size = 4) +
        labs(x = "Champions", y = "Count", title = "Champions of the Tournament") +
        scale_fill_brewer(palette = "Dark2") +
        theme_bw()
      
    } else if(input$plot_type == 7){
      #Dvisualizing the results based on the winning toss and selection
      
      data_summary <- data1 %>%
        group_by(Toss, Selection) %>%
        summarize(total = n(), wins = sum(Result == "Win")) %>%
        mutate(losses = total - wins,
               win_pct = round(wins / total * 100, 2),
               loss_pct = round(losses / total * 100, 2))
      
      
      #Dplot the win and loss percentages
      ggplot(data_summary, aes(x = Toss, y = win_pct, fill = Selection)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = paste0(win_pct, "%")), position = position_stack(vjust = 0.5), size = 4) +
        scale_fill_manual(values = c("#FFC107", "#2196F3"), labels = c("Batting", "Bowling")) +
        labs(x = "Team winning toss", y = "Winning percentage", title = "Win and loss percentage based on toss and selection") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold"), 
              legend.position = "bottom", 
              legend.title = element_blank())
      
    } else if(input$plot_type == 8){
      #Dconsistency of sixes by each team
      library(ggridges)
      ggplot(data1,aes(x = Sixes,y = Team,fill = Team)) +
        geom_density_ridges() + 
        theme_ridges() +
        labs(title="sixes scored by each team") +
        theme(legend.position = "none")+
        theme_bw()
      
    }else if(input$plot_type == 9){
      #Most win percentage against pakistan by india 
      #filtering out the data india vs pakistan matches 
      wins <- filter(data1,Team=="India" & Opponent=="Pakistan")
      #getting the percentgaes of wins and loses
      winper <-  wins %>% count(Result) %>% mutate(percent = n / sum(n),percentagelabel = paste0(round(percent*100), "%")) 
      #plotting the data
      ggplot(winper,aes(x=Result,y=n,fill=Result))+
        geom_bar(stat = "identity")+
        labs(y="Percentage",title="India Win Percentage against pakistan")+
        geom_text(aes(label = percentagelabel),vjust=-0.5) +
        coord_polar("y", start=0)+
        theme_bw()
      
    }else if(input$plot_type == 10){
      #The players having the best average > 40  in indian team
      #filtering out the data
      batavg <- filter(batsmanodi, Country == "India" & Batting.Average > 40) %>%
        arrange(Batting.Average)
      #plotting the graph
      ggplot(batavg, aes(x = Batting.Average, y = reorder(Player.Name, Batting.Average), fill = Player.Name)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = Batting.Average, hjust = -0.1)) +
        labs(x = "Batting Average", y = "Player Name", title = "Best Batting Average") +
        theme_bw()
    }else if(input$plot_type == 11){
      #highest no of wicket takers in odi with best economy
      #filtering out the data
      library(scatterplot3d)
      bowl <- filter(bowlerodi, Economy.Rate<4)
      #plotting the graph
      with(bowl, {scatterplot3d(x = Wickets,y = Played,z = Economy.Rate,, 
                                # filled blue circles
                                color="green", 
                                pch=15, 
                                # lines to the horizontal plane
                                type = "h",main="Highest number of wicket takers with best economy")})
      
    }else if(input$plot_type == 12){
      #Number of wins and loseses by each country in Asia cup
      
      wi_lo <- data1 %>%
        group_by(Team, Result) %>%
        summarize(matches = n()) %>%
        ggplot() +
        geom_bar(aes(x = reorder(Team, -matches), y = matches, fill = Result),
                 stat = "identity", width = 0.6) +
        coord_flip() +
        scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD"),
                          name = "Result") +
        labs(title = "Wins and Losses by Each Country in Asia Cup",
             x = "Country",
             y = "Number of Matches") +
        theme_minimal() +
        theme(plot.title = element_text(size = 18, face = "bold", margin = margin(0, 0, 20, 0)),
              legend.box.margin = margin(0, 0, 10, 0),
              legend.key.size = unit(0.5, "cm")) +
        annotate("text", x = 0.5, y = -0.2, 
                 label = "", size = 3, fontface = "italic")+
        theme_bw()
      
      wi_lo
      
      
    }else if(input$plot_type == 13){
      
      #Analysing the batsman odi dataset
      # Aggregating  data by player and calculating total runs
      str(batsmanodi)
      
      batsman_runs <- batsmanodi %>%
        group_by(Player.Name) %>%
        summarise(Total_Runs = sum(Runs)) %>%
        arrange(desc(Total_Runs)) 
      
      # Create bar chart
      ggplot(batsman_runs[1:10,], aes(x = reorder(Player.Name, Total_Runs), y = Total_Runs, fill = Player.Name)) +
        geom_bar(stat = "identity") +
        labs(title = "Total Runs scored by each player in Asia Cup (Top 10)",
             x = "Player",
             y = "Total Runs") +
        scale_fill_viridis_d() +
        coord_flip() +
        theme_minimal()+
        theme_bw()
    }else if(input$plot_type == 14){
      #correlation between bowling average and Economy rate 
      ggplot(bowlerodi, aes(x = Bowling.Average, y = Economy.Rate)) +
        geom_point(size = 3, color = "#619CFF") +
        geom_smooth(method = "lm", se = FALSE, color = "#F8766D") +
        labs(x = "Bowling Average", y = "Bowling Economy Rate", 
             title = "Correlation between Bowling Average and Economy Rate in ODI Cricket")
      
    }else if(input$plot_type == 15){
      #creating heat maps 
      # Calculate average runs by team and year
      avg_runs_by_team_year <- data1 %>%
        group_by(Team, Year) %>%
        summarise(avg_runs = mean(Run.Scored))
      
      # Create heat map
      ggplot(avg_runs_by_team_year, aes(x = Year, y = Team, fill = avg_runs)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
        labs(x = "Year", y = "Team", title = "Average runs scored by team and year") +
        theme_minimal() +
        theme_bw()+
        theme(plot.title = element_text( size = 18,hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))
    }else if(input$plot_type == 16){
      #creating maps
      # Create a data frame with the latitude and longitude coordinates for each country
      asia_countries <- data.frame(
        country = c("India", "Pakistan", "Bangladesh", "Afghanistan", "Hong Kong", "UAE"),
        lon = c(78.9629, 69.3451, 90.3563, 67.7099, 114.1095, 54.37),
        lat = c(20.5937, 30.3753, 23.685, 33.9391, 22.3964, 24.47)
      )
      
      # Get map data for Asia
      asia_map <- map_data("world2")
      
      # Create the map plot
      ggplot() +
        # Add a gray background and remove axis labels
        theme_void() +
        # Add the map data as a filled polygon
        geom_polygon(data = asia_map, aes(x = long, y = lat, group = group), fill = "gray40", color = "white") +
        # Add the country points with color and size based on country
        geom_point(data = asia_countries, aes(x = lon, y = lat, color = country, size = 4), alpha = 0.9) +
        # Add a title and subtitle
        labs(title = "Participating Countries in Asia Cup",
             subtitle = "India, Pakistan, Bangladesh, Afghanistan, Hong Kong, UAE") +
        # Modify the legend and color scheme
        scale_color_manual(values = c("#FF9933", "#0066CC", "#006600", "#660066", "#FF6600", "#000000")) +
        guides(color = guide_legend(title = NULL, nrow = 2)) +
        # Modify the plot margins and size
        theme(plot.margin = margin(5, 5, 5, 5, "mm"),
              plot.background = element_rect(fill = "white"),
              legend.position = "bottom",
              legend.box.background = element_rect(color = "black", size = 0.5),
              legend.margin = margin(10, 0, 10, 0, "mm"))
         }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

