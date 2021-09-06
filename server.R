#Resourcees
library(ggplot2)
library(scales)
library(dplyr)
require(maps)
require(viridis)
library(gganimate)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(forcats)


fill_gaps <- function(df) {
  pom_df <- data.frame()
  
  for (row in 2:nrow(df)) {
    if (df[row-1, "Entity"] == df[row, "Entity"] && df[row-1, "Year"] != df[row, "Year"] - 1) {
      Year <- seq(df[row-1, "Year"] + 1, df[row, "Year"] - 1)
      Emission <- seq(df[row-1, "Emission"], df[row, "Emission"], length.out=(length(Year) + 2))[2:(length(Year) + 1)]
      Entity <- rep(df[row, "Entity"], length(Year))
      Code <- rep(df[row, "Code"], length(Year))
      
      pom_df <- rbind(pom_df, data.frame(Year, Emission, Entity, Code))
    }
  }
  
  df <- rbind(df, pom_df)
  return(df)
}


# Emission data
setwd('/home/wojtek/Documents/University/Data Visualization/Lab 4/EmissionDataViz/resources')
emissions_df <- read.csv('./co2_emission.csv')
colnames(emissions_df) <- c("Entity", "Code", "Year", "Emission")
emissions_df <- fill_gaps(emissions_df)
emissions_df <- emissions_df[with(emissions_df, order(Entity, Year)), ]

# Top 5 data
population_df <- read.csv('./population.csv')
population_df <- population_df[, !(colnames(population_df) %in% c("Indicator.Name", "Indicator.Code", "Country.Name", "X2018", "X2019"))]
colnames(population_df) <- c("Code", c(1960:2017))
codes <- population_df[population_df$Code %in% unique(emissions_df[emissions_df$Code != "", "Code"]), "Code"]
percapita_df <- emissions_df[emissions_df$Code %in% codes & emissions_df$Year >= 1960, ]
func <- function(x) { as.numeric(x[4]) / population_df[population_df$Code == x[2], x[3]] }
percapita_df$Emission <- apply(percapita_df, 1, func)

# Stacked data
eu <- subset(emissions_df, subset=(Entity == 'EU-28'))
rownames(eu) <- eu$Year
us <- subset(emissions_df, subset=(Entity == 'United States'))
rownames(us) <- us$Year
mideast <- subset(emissions_df, subset=(Entity == 'Middle East'))
rownames(mideast) <- mideast$Year
india <- subset(emissions_df, subset=(Entity == 'India'))
rownames(india) <- india$Year
china <- subset(emissions_df, subset=(Entity == 'China'))
rownames(china) <- china$Year
stacked_df <- data.frame(eu, us, mideast, india, china)
stacked_df <- stacked_df[c("Emission", "Emission.1", "Emission.2", "Emission.3", "Emission.4")]
colnames(stacked_df) <- c('EU-28', 'United States', 'Middle East', 'India', 'China')
world <- subset(emissions_df, subset=(Entity == 'World'))
world <- world$Emission
stacked_df$`Rest of the world` <- world - rowSums(stacked_df)
Entity <- rep(c('EU-28', 'United States', 'Middle East', 'India', 'China', 'Rest of the world'), each=267)
Year <- rep(seq(1751, 2017), 6)
Emission <- c(stacked_df$`EU-28`, stacked_df$`United States`, stacked_df$`Middle East`,
              stacked_df$`India`, stacked_df$`China`, stacked_df$`Rest of the world`)
stacked_df <- data.frame(Entity, Year, Emission)

function(input, output, session) {
  
  # Changing values on slider
  observe({
    reg <- input$region
    
    updateSliderInput(session, "years",
                      min = min(emissions_df[emissions_df$Entity==reg, 'Year']), 
                      max = max(emissions_df[emissions_df$Entity==reg, 'Year']),
                      value = c(min(emissions_df[emissions_df$Entity==reg, 'Year']),
                                max(emissions_df[emissions_df$Entity==reg, 'Year'])))
  })
  
  
  # Value box
  output$test <- renderValueBox({
    start <- subset(emissions_df, subset=(Entity == input$region & Year == input$years[1]))$Emission
    end <- subset(emissions_df, subset=(Entity == input$region & Year == input$years[2]))$Emission
    
    start_val <- ifelse(start > 0, start, 1)
    end_val <- ifelse(end > 0, end, 1)
    
    val <- round((end_val / start_val - 1) * 100, 2)
    
    if (val > 0){
      valueBox(
        paste0(round((end_val / start_val - 1) * 100, 2), "%"),
        "Change over chosen period",
        icon = icon("angle-double-up"),
        color = "red"
      )
    }
    else if (val < 0){
      valueBox(
        paste0(round((end_val / start_val - 1) * 100, 2), "%"),
        "Change over chosen period",
        icon = icon("angle-double-down"),
        color = "green"
      )
    }
    else{
      valueBox(
        paste0(round((end_val / start_val - 1) * 100, 2), "%"),
        "Change over chosen period",
        icon = icon("minus"),
        color = "yellow"
      )
    }
    
  })
  
  # Country line plot
  output$country_line_plot <- renderPlot({
    line_df <- subset(emissions_df, subset=(Entity == input$region & between(Year, input$years[1], input$years[2])))
    
    line_plot <- ggplot(line_df, aes(x=Year, y=Emission)) +
      geom_line() +
      scale_y_continuous(labels=scales::comma) +
      labs(title="Chosen country's CO2 emission over chosen period", y = "Emission (t)") +
      theme_minimal() +
      theme(plot.title=element_text(size=22, face="bold", hjust = 0.5, margin=margin(0, 0, 20, 0)),
            plot.margin=margin(20, 20, 20, 20),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14, face = "bold"))
    
    line_plot
  })
  
  # Stacked plot
  output$stacked_plot <- renderPlot({
    stacked_plot <- ggplot(stacked_df, aes(x=Year, y=Emission, fill=Entity)) +
      geom_area() +
      scale_fill_brewer(palette = "Set3") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(limits = c(1800, NA)) +
      labs(title = "World's CO2 emission distribution over the years", y = "Emission (t)") +
      theme_minimal() +
      theme(plot.title = element_text(size=22, face="bold", hjust = 0.35, margin = margin(0, 0, 20, 0)),
            plot.margin = margin(20, 20, 20, 20),
            legend.position = 'none',
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14, face = "bold"))
    
    stacked_plot
  })
  
  #Pie plot
  output$pie_plot <- renderPlot({
    pie_plot <- ggplot(subset(stacked_df, subset=(Year == 2017)), aes(x="", y=Emission, fill=Entity)) +
      geom_bar(stat="identity", width=1, color="white") +
      scale_fill_brewer(palette = "Set3") +
      coord_polar("y", start=0) +
      theme_void() +
      labs(title = "World's CO2 emission distribution in 2017", fill = NULL) +
      theme(plot.title = element_text(size=22, face="bold", hjust = 0.5, margin = margin(0, 0, 20, 0)),
            plot.margin = margin(20, 20, 20, 20),
            legend.text = element_text(size=16),
            legend.direction = "vertical",
            legend.position = "bottom",
            legend.margin = margin(50, 0, 20, 0),
            legend.key.size = unit(1.5, 'cm'))
      
    pie_plot
  })
  
  
  #Top 5 (bar plot)
  output$bar_plot <- renderPlot({
    top_df <- subset(emissions_df, subset=(Entity != 'World' & Code != '' & Year == input$top_year))
    top_df <- top_df[order(-top_df$Emission),][1:input$top_number, ]
    
    bar_plot <- top_df %>%
      mutate(Entity = fct_reorder(Entity, Emission)) %>%
      ggplot(aes(x=Entity, y=Emission)) + 
      geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.5) +
      scale_y_continuous(name="Emission (t)", labels=scales::comma) +
      coord_flip() +
      theme_minimal() +
      labs(title = "World's top CO2 emitors", x=NULL) +
      theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(0, 0, 20, 0)),
            plot.margin = margin(20, 80, 20, 20),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14, face = "bold"))
    
    bar_plot
  })
  
  
  #Top 5 per capita (bar plot)
  output$bar_plot2 <- renderPlot({
    top_df2 <- percapita_df[percapita_df$Year == input$top_year, ]
    top_df2 <- top_df2[order(-top_df2$Emission),][1:input$top_number, ]
    
    bar_plot <- top_df2 %>%
      mutate(Entity = fct_reorder(Entity, Emission)) %>%
      ggplot(aes(x=Entity, y=Emission)) + 
      geom_bar(stat="identity", fill="#67a9cf", alpha=.6, width=.5) +
      scale_y_continuous(name="Emission (t)", labels=scales::comma) +
      coord_flip() +
      theme_minimal() +
      labs(title = "World's top CO2 emitors (per capita)", x=NULL) +
      theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(0, 0, 20, 0)),
            plot.margin = margin(20, 80, 20, 20),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14, face = "bold"))
    
    bar_plot
  })
  
  
  #Map
  output$map <- renderPlot({
    theme_set(theme_bw())
    world <- ne_countries(scale = "medium", returnclass = "sf")
    func <- function(x) { ifelse(x[13] %in% percapita_df$Code, percapita_df[percapita_df$Code == x[13] & percapita_df$Year == input$top_year, "Emission"], 0) }
    world$per_capita <- apply(world, 1, func)
    
    map <- ggplot(data = world) +
      geom_sf(aes(fill = per_capita)) +
      scale_fill_viridis_c(option = "plasma", trans = "log", name = "Emission per capita\n(logarithmic scale)", breaks = c(0.1, 1, 10)) +
      labs(title = "World's emission distribution in 2017") +
      theme_minimal() +
      theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(0, 0, 20, 0)),
            plot.margin = margin(20, 0, 20, 0),
            legend.text = element_text(size=10),
            legend.direction = "vertical",
            legend.position = c(0.15, 0.35),
            legend.title = element_text(size=12, margin = margin(0, 0, 10, 0)))
    
    
    map
  })

}
