library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)
library(dplyr)


setwd('/home/wojtek/Documents/University/Data Visualization/Lab 4')
emissions_df <- read.csv('./EmissionDataViz/resources/co2_emission.csv')
colnames(emissions_df) <- c("Entity", "Code", "Year", "Emission")

entities <- data.frame(unique(emissions_df[!emissions_df$Code=="",]$Entity))
colnames(entities) <- 'Name'


dashboardPage(
  dashboardHeader(title = HTML(paste("World's CO", tags$sub(2), " emission", sep = ""))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("clipboard")),
      menuItem("Country", tabName = "country", icon = icon("flag")),
      menuItem("World", tabName = "world", icon = icon("globe")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "summary",
        fluidPage(
          column(width = 8,
                 box(width = 12,
                     plotOutput("stacked_plot", height = "40em")
                     ),
                 box(width = 12, height = "18.5em",
                     h3("World collectively emitted",
                        HTML("<b>"), emissions_df[emissions_df$Entity == "World" & emissions_df$Year == 2017, "Emission"], HTML("</b>"),
                        HTML(paste("tones of CO", tags$sub(2), sep = ""))),
                     h3("To follow Paris Agreement we would need half this number by 2030 and bring to zero by 2050"),
                     HTML("<br>"),
                     h3("This presentation is created to help you analyse the magnitude of this problem in countrywide and worldwide perspective")
                 )
          ),
          column(width = 4,
                 box(width = 12,
                     plotOutput("pie_plot", height = "60em")
                     )
          )
        )
      ),
      tabItem(
        tabName = "country",
        fluidPage(
          box(width = 7,
            plotOutput("country_line_plot", height = "32em")
          ),
          box(width = 5,
            title = "Set up",
            solidHeader = TRUE,
            status = "warning",
            selectInput("region", "Choose a region to plot:",
                        list(`Region` = entities$Name),
                        selected = "Poland"
            ),
            sliderInput("years", "Year range",
                        min = min(emissions_df[emissions_df$Entity=='Poland', 'Year']), 
                        max = max(emissions_df[emissions_df$Entity=='Poland', 'Year']),
                        value = c(min(emissions_df[emissions_df$Entity=='Poland', 'Year']),
                                  max(emissions_df[emissions_df$Entity=='Poland', 'Year'])),
                        sep = ""
            )
          ),
          valueBoxOutput("test", width = 5)
        )
      ),
      tabItem(
        tabName = "world",
        fluidPage(
          column(width = 7,
                 box(width = 12,
                     plotOutput("bar_plot", height = "30em")
                 ),
                 box(width = 12,
                     plotOutput("bar_plot2", height = "30em")
                 )
          ),
          column(width = 5,
                 box(width = 12,
                     title = "Set up",
                     solidHeader = TRUE,
                     status = "warning",
                     selectInput("top_number", "Select number of countries:",
                                 c(3:10),
                                 selected = 5
                     ),
                     selectInput("top_year", "Select year:",
                                 c(1960:2017),
                                 selected = 1990
                     )),
                 box(width = 12,
                     plotOutput("map")
                 )
          )
        )
      ),
      tabItem(
        tabName = "about",
        fluidPage(
          column(
            width = 7,
            box(
              title = "Author", width = 4, background = "maroon",
              h4("Wojciech Wieczorek (145465)")
            ),
            box(
              title = "Sources", width = 11, background = "navy",
              h4("World's emissions in 1960-2018"),
              h5("https://www.kaggle.com/yoannboyere/co2-ghg-emissionsdata"),
              h4("World's population in 1960-2018"),
              h5("https://www.kaggle.com/muhakabartay/world-population-19602018?select=Use_API_SP.POP.TOTL_DS2_en_csv_v2_1120881.csv"),
            )
          )
        )
      )
    )
  )
)
