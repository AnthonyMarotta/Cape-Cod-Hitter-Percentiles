library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DT)
library(scales)
setwd("~/Downloads/CCBLCSV")
# Sample Data - Replace this with actual data loading
FullCCBL <- read.csv("SeasonCCBL.csv")

# Rename the teams in the BatterTeam column
FullCCBL <- FullCCBL %>%
  mutate(BatterTeam = case_when(
    BatterTeam == "BRE_WHI" ~ "Brewster Whitecaps",
    BatterTeam == "BOU_BRA" ~ "Bourne Braves",
    BatterTeam == "CHA_ANG" ~ "Chatham Anglers",
    BatterTeam == "YAR_RED" ~ "Yarmouth-Dennis Red Sox",
    BatterTeam == "FAL_COM" ~ "Falmouth Commodores",
    BatterTeam == "WAR_GAT" ~ "Wareham Gateman",
    BatterTeam == "HAR_MAR" ~ "Harwich Mariners",
    BatterTeam == "HYA_HAR" ~ "Hyannis Harbor Hawks",
    BatterTeam == "ORL_FIR" ~ "Orleans Firebirds",
    BatterTeam == "COT_KET" ~ "Cotuit Kettleers",
    TRUE ~ BatterTeam
  ))

# Data processing for batters
FullCCBL <- FullCCBL %>%
  mutate(
    HardHitCheck = case_when(ExitSpeed >= 95 ~ TRUE, TRUE ~ FALSE),
    InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
    Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
    Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0)
  ) %>%
  group_by(Batter, BatterTeam, AutoPitchType) %>%
  summarise(
    AvgExitSpeed = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1),
    HardHits = sum(HardHitCheck, na.rm = TRUE),
    TotalHits = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
    HardHit_Percentage = round(HardHits / TotalHits * 100, 1),
    Chase_Percentage = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
    Whiff_Percentage = round(sum(PitchCall == "StrikeSwinging") / sum(Swing, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(
    HardHit_Percentile = round(percent_rank(HardHit_Percentage / 100), 2),
    Chase_Percentile = round(1 - percent_rank(Chase_Percentage / 100), 2),
    ExitSpeed_percentile = round(percent_rank(AvgExitSpeed), 2),
    Whiff_Percentile = round(1 - percent_rank(Whiff_Percentage / 100), 2)
  )

# UI
ui <- fluidPage(
  titlePanel("Batter Performance Percentiles"),
  sidebarLayout(
    sidebarPanel(
      selectInput("BatterTeam", "Select Batter Team:", choices = unique(FullCCBL$BatterTeam)),
      selectInput("AutoPitchType", "Select Auto Pitch Type:", choices = c("All", unique(FullCCBL$AutoPitchType))),
      selectInput("Batter", "Select Batter:", choices = NULL)
    ),
    mainPanel(
      plotOutput("Percentiles"),
      dataTableOutput("Percentiles_Data")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Update Batter choices based on selected BatterTeam and AutoPitchType
  observe({
    filteredData <- FullCCBL %>%
      filter(BatterTeam == input$BatterTeam, 
             if (input$AutoPitchType == "All") TRUE else AutoPitchType == input$AutoPitchType)
    
    updateSelectInput(session, "Batter", choices = unique(filteredData$Batter))
  })
  
  # Percentiles plot
  output$Percentiles <- renderPlot({
    # Filtered data for selected batter and AutoPitchType
    selectedBatter <- FullCCBL %>%
      filter(Batter == input$Batter, 
             if (input$AutoPitchType == "All") TRUE else AutoPitchType == input$AutoPitchType)
    
    # Aggregate data if 'All' is selected
    if (input$AutoPitchType == "All") {
      selectedBatter <- selectedBatter %>%
        group_by(Batter, BatterTeam) %>%
        summarise(
          AvgExitSpeed = round(mean(AvgExitSpeed, na.rm = TRUE), 1),
          HardHit_Percentage = round(mean(HardHit_Percentage, na.rm = TRUE), 1),
          Chase_Percentage = round(mean(Chase_Percentage, na.rm = TRUE), 1),
          Whiff_Percentage = round(mean(Whiff_Percentage, na.rm = TRUE), 1),
          HardHit_Percentile = round(mean(HardHit_Percentile, na.rm = TRUE), 2),
          Chase_Percentile = round(mean(Chase_Percentile, na.rm = TRUE), 2),
          ExitSpeed_percentile = round(mean(ExitSpeed_percentile, na.rm = TRUE), 2),
          Whiff_Percentile = round(mean(Whiff_Percentile, na.rm = TRUE), 2)
        )
    }
    
    # Determine color based on percentile for each metric
    selectedBatter <- selectedBatter %>%
      mutate(
        HardHit_Percentile_color = ifelse(HardHit_Percentile <= 0.25, "blue",
                                          ifelse(HardHit_Percentile <= 0.45, "lightblue",
                                                 ifelse(HardHit_Percentile <= 0.55, "grey",
                                                        ifelse(HardHit_Percentile <= 0.75, "lightcoral", "red")))),
        Chase_Percentile_color = ifelse(Chase_Percentile <= 0.25, "blue",
                                        ifelse(Chase_Percentile <= 0.45, "lightblue",
                                               ifelse(Chase_Percentile <= 0.55, "grey",
                                                      ifelse(Chase_Percentile <= 0.75, "lightcoral", "red")))),
        ExitSpeed_color = ifelse(ExitSpeed_percentile <= 0.25, "blue",
                                 ifelse(ExitSpeed_percentile <= 0.45, "lightblue",
                                        ifelse(ExitSpeed_percentile <= 0.55, "grey",
                                               ifelse(ExitSpeed_percentile <= 0.75, "lightcoral", "red")))),
        Whiff_Percentile_color = ifelse(Whiff_Percentile <= 0.25, "blue",
                                        ifelse(Whiff_Percentile <= 0.45, "lightblue",
                                               ifelse(Whiff_Percentile <= 0.55, "grey",
                                                      ifelse(Whiff_Percentile <= 0.75, "lightcoral", "red"))))
      )
    
    # Plotting HardHit Percentage percentiles
    plotHardHit <- ggplot(selectedBatter, aes(x = HardHit_Percentile, y = HardHit_Percentage)) +
      geom_point(size = 9, aes(color = HardHit_Percentile_color)) +
      ggtitle("HardHit Percentage Percentile") + xlim(0, 1) + ylim(min(selectedBatter$HardHit_Percentage), max(selectedBatter$HardHit_Percentage)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedBatter$HardHit_Percentage), yend = max(selectedBatter$HardHit_Percentage)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(HardHit_Percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting Chase Percentage percentiles
    plotChase <- ggplot(selectedBatter, aes(x = Chase_Percentile, y = Chase_Percentage)) +
      geom_point(size = 9, aes(color = Chase_Percentile_color)) +
      ggtitle("Chase Percentage Percentile") + xlim(0, 1) + ylim(min(selectedBatter$Chase_Percentage), max(selectedBatter$Chase_Percentage)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedBatter$Chase_Percentage), yend = max(selectedBatter$Chase_Percentage)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(Chase_Percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting Exit Speed percentiles
    plotExitSpeed <- ggplot(selectedBatter, aes(x = ExitSpeed_percentile, y = AvgExitSpeed)) +
      geom_point(size = 9, aes(color = ExitSpeed_color)) +
      ggtitle("Exit Speed Percentile") + xlim(0, 1) + ylim(min(selectedBatter$AvgExitSpeed), max(selectedBatter$AvgExitSpeed)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedBatter$AvgExitSpeed), yend = max(selectedBatter$AvgExitSpeed)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(ExitSpeed_percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting Whiff Percentage percentiles
    plotWhiff <- ggplot(selectedBatter, aes(x = Whiff_Percentile, y = Whiff_Percentage)) +
      geom_point(size = 9, aes(color = Whiff_Percentile_color)) +
      ggtitle("Whiff Percentage Percentile") + xlim(0, 1) + ylim(min(selectedBatter$Whiff_Percentage), max(selectedBatter$Whiff_Percentage)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedBatter$Whiff_Percentage), yend = max(selectedBatter$Whiff_Percentage)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(Whiff_Percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Arrange the plots in a grid
    grid.arrange(plotHardHit, plotChase, plotExitSpeed, plotWhiff, ncol = 1)
  })
  
  # Data table for percentiles
  output$Percentiles_Data <- DT::renderDataTable({
    selectedData <- FullCCBL %>%
      filter(Batter == input$Batter, 
             if (input$AutoPitchType == "All") TRUE else AutoPitchType == input$AutoPitchType)
    
    if (input$AutoPitchType == "All") {
      selectedData <- selectedData %>%
        group_by(Batter, BatterTeam) %>%
        summarise(
          AvgExitSpeed = round(mean(AvgExitSpeed, na.rm = TRUE), 1),
          HardHit_Percentage = round(mean(HardHit_Percentage, na.rm = TRUE), 1),
          Chase_Percentage = round(mean(Chase_Percentage, na.rm = TRUE), 1),
          Whiff_Percentage = round(mean(Whiff_Percentage, na.rm = TRUE), 1),
          HardHit_Percentile = round(mean(HardHit_Percentile, na.rm = TRUE), 2),
          Chase_Percentile = round(mean(Chase_Percentile, na.rm = TRUE), 2),
          ExitSpeed_percentile = round(mean(ExitSpeed_percentile, na.rm = TRUE), 2),
          Whiff_Percentile = round(mean(Whiff_Percentile, na.rm = TRUE), 2)
        )
    }
    
    selectedData %>%
      select(Batter, BatterTeam, AutoPitchType, HardHit_Percentage, Chase_Percentage, AvgExitSpeed, Whiff_Percentage, HardHit_Percentile, Chase_Percentile, ExitSpeed_percentile, Whiff_Percentile) %>%
      arrange(desc(HardHit_Percentile))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


