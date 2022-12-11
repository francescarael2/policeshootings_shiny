setwd("/Users/francescarael/Documents")
#Loading in necessary packages, scripts, and data
library(shiny)
library(usmap)
library(ggplot2)
library(stringr)
library(maps)
library(viridis)
sh_shooting_data <- read.csv("R-Shiny/App-1/data_clean.csv")
sh_state_data <- read.csv("R-Shiny/App-1/state_data.csv")
sh_rolling_data <- read.csv("R-Shiny/App-1/rolling_data.csv")
sh_race_data <- read.csv("R-Shiny/App-1/race_data_clean.csv")

median_male <- median(sh_shooting_data$age[sh_shooting_data$male==1])
median_female <-median(sh_shooting_data$age[sh_shooting_data$male==0])
colourss <- c("#135420","#352735")

h1("my title")


# Define UI for app ----
ui <- fixedPage(
  titlePanel(h1("Police Shootings in the United States"),
             h3("Police are the judge, the jury, and the executioner in many cases across the United States. This dashboard visualizes murders at the hands of police from 2015-2022. Data courtesy of the Washington Post."
             )),
  
column(2,
           radioButtons("radio_choice",
                        h5("Choose Visualization"),
           choices = list("Per Capita (map)" = 1, 
                          "Gender and Age (bar chart)" = 2,
                          "Ethnicity (bar chart)" = 3,
                          "Moving Average (line chart)" = 4), selected = 1))
  ,

  fixedRow(
        column(
          6,
     plotOutput("charts"),
     column(4, sliderInput("map_year",
                 label = "Year:", sep = "",
                 min = 2015, max = 2022, value = 2015)))
  
    ))
    






# Define server logic ----
server <- function(input, output) {
  output$selected_var <- renderText(
    {
      paste("You have selected",input$var)
    }
)
  

  output$charts <- 
    
    if (input$radio_choice == 1) {
      renderPlot(
        {
          data <- sh_state_data %>%
            filter(year == input$map_year)
          
          usmap::plot_usmap(data = data, values = "per_capita", color = "black") +
            scale_fill_continuous(high = "#856084", low = "#DDE1E4", name = "Per Capita", label = scales::scientific) + 
            theme(legend.position = "left") +
            labs(title = "Murders Per Capita (By State)")
          
        })
    }
      
      else if (input$radio_choice == 2)
      {
        renderPlot(
          {
            ggplot(data = sh_shooting_data) + 
              geom_bar(mapping = aes(x = age, y = ..prop.., group = gender, fill = gender)) + 
              scale_y_continuous(labels = scales::percent_format()) + 
              scale_fill_manual(values = c("#84E296","#856084")) +
              labs(title = "Murders by Gender and Age of Victim", x = "Age of Victim",y = "Percentage of Total Murders", fill = "Gender") +
              coord_flip() +
              geom_vline(xintercept = vertical_lines, colour = colourss, size = 1, linetype = "dashed")
          }
        )
      }
      else if (input$radio_choice == 3) {
        renderPlot(
          rolling_data %>%
            filter(year(date) == input$ra_year) %>%
            ggplot(aes(x=date, y=death_10da)) + geom_line() + ylim(0,5) +
            labs(title = "10-Day Moving Average", x="Date",y="Murders")
          
        )
      }
      else if (input$radio_choice==4) {
        renderPlot(
          
          ggplot(race_data_clean, aes(x = race, y=per_million, fill=per_million)) +
            geom_bar(stat= "identity") +
            scale_fill_continuous(high = "#624862", low = "#baa1ba",name="Per Million") + 
            labs(
              title = "Murders Per Million by Ethnicity",
              subtitle = "Ethnicity Data Sourced from 2020 United States Census"
            ) +
            theme(
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)
            ) + 
            xlab("Ethnicity") +
            ylab("Murders per Million") 
        )  }
      
      
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)