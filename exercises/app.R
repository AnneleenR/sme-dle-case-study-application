##Required packages: tidyverse, ggplot2, shiny, treemapify

library(tidyverse)
library(ggplot2)
library(shiny)
library(treemapify)

## 

rws_data <- read_csv2("2021_rws_part1.csv", locale=locale(encoding="latin1"))

## Cleaning data

rws_data_select <- rws_data %>%   select(c("gender", "seniority", "metro_regional", "barrier_poor_management", "barrier_skills", "barrier_workspace"))
colnames(rws_data_select) <- c("gender", "seniority", "city_or_rural", "poor_management", "remote_skills", "workspace")

rws_data_select <- rws_data_select %>% mutate(gender = recode(gender, "I would rather not say"="Other"),
                                              city_or_rural = recode (city_or_rural, "Metro" = "City", "Regional" = "Rural"))

rws_data_select <- rws_data_select %>% mutate(seniority = factor(seniority, levels=c("More than 5 years", "Between 1 and 5 years", "Between 6 and 12 months")),
                                              gender = factor(gender, levels=c("Other", "Male", "Female")),
                                              city_or_rural = factor(city_or_rural, levels=c("Rural", "City")),
                                              poor_management = factor(poor_management, levels=c("Significantly worsened", "Somewhat worsened", "Stayed about the same", "Somewhat improved", "Significantly improved",
                                                                                                 "Not a barrier for me")),
                                              remote_skills = factor(remote_skills, levels=c("Significantly worsened", "Somewhat worsened", "Stayed about the same", "Somewhat improved", "Significantly improved",
                                                                                             "Not a barrier for me")),
                                              workspace = factor(workspace, levels=c("Significantly worsened", "Somewhat worsened", "Stayed about the same", "Somewhat improved", "Significantly improved",
                                                                                      "Not a barrier for me")))

grouping_variables <- rws_data_select %>% select(c("gender", "seniority", "city_or_rural"))
barrier_variables <- rws_data_select %>% select(c("poor_management", "remote_skills", "workspace"))

## Shiny app

ui <- fillPage(
  h3("Remote working survey visualizations"),
    inputPanel(
      varSelectInput(inputId = "variable2",
                     label = "Choose barrier", 
                     data = barrier_variables,
                     multiple = F),
      varSelectInput(inputId = "variable", 
                     label = "Choose grouping variable",
                     data = grouping_variables,
                     multiple = F),
      radioButtons(inputId = "plot_type", 
                   label = ("Choose a plot type"),
                   choices = list("Stacked bar chart" = 1, "Treemap" = 2),selected = 1)
      ),
                    
    fillPage(
      plotOutput("plot1", height="75%")
     
    )
  )


server <- function(input, output){
  output$plot1 <- renderPlot({
 
    if (!!input$plot_type == 1) {
    
    data_agg <- rws_data_select %>% 
      group_by(!!input$variable, !!input$variable2) %>% 
      summarise(count = n()) %>% 
      mutate(perc = count/sum(count))
    
      ggplot(data_agg, aes(x=!!input$variable,  y=perc*100, fill=!!input$variable2, label=paste0(round(perc*100, digits=2), "%"))) +
      geom_bar(position = position_stack(reverse=T), stat="identity") +
      geom_text(size = 7, position = position_stack(reverse=T, vjust = 0.5)) +
      coord_flip() +
      theme(text = element_text(size = 20)) + 
      scale_fill_brewer(palette = "RdYlGn") +
      labs(y = "percent") 
     
      
    } else if (!!input$plot_type == 2) {
   
     data_agg <- rws_data_select %>% 
      group_by(!!input$variable, !!input$variable2) %>% 
      summarise(count = n()) %>% 
      mutate(perc = count/sum(count))
    
    ggplot(data_agg, aes(area = perc*100, fill=!!input$variable2, subgroup=!!input$variable, label=paste(!!input$variable2, paste0(round(perc*100, digits=2), "%"), sep="\n"))) +
      geom_treemap() +
      geom_treemap_text(color = "black", size=20) +
      geom_treemap_subgroup_text(place = "centre", alpha = 0.3, color="white", grow=T) +
      geom_treemap_subgroup_border(color = "white") +
      theme(text = element_text(size = 20)) + 
      scale_fill_brewer(palette = "Blues")}
  })
  
  }


shinyApp(ui=ui, server=server)