# This app examines racial distributions across net worth percentiles using
# Survey of Consumer Finance Data

# Set Up ---------------------
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(scales)

# read in data 
scf <- read_csv("../data/cleaned_scf.csv")

# Sets aesthetic themes and names
theme_scf_dist <-  theme(panel.background= element_blank(),
                         panel.grid.minor.y = element_line(color = "lightgrey"),
                         panel.grid.major.y = element_line(color = "lightgrey"),
                         axis.ticks = element_blank()) 

thp_four <- c("#007363",
              "#A5D867",
              "#6E2585",
              "#427730")


scf$race <- factor(scf$race, levels = c("White, non-mixed race",
                                                "Black/African-American, non-mixed race",
                                                "Hispanic/Latino, non-mixed race",
                                                "Other"))

# Basic data calculations for finding average networth within each bin
# to later be used in app
wealth_lookup <- scf %>% 
  group_by(networth_cat2) %>% 
  summarize(mean = weighted.mean(NETWORTH, WGT))
  
wealth_lookup$mean <- dollar(wealth_lookup$mean)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Racial Wealth Gap"),
   
   h4("Slide along the wealth bar to see how the race distribution changes."),
   
   fluidRow(
     column(6, 
            plotOutput("race_stack")
            ),
     column(6,
            plotOutput("race_bar")
            )
   ),
  
   
   fluidRow(
     column(12, align = "center",
       hr(),
       sliderTextInput("wealth", 
                   label = "Net worth percentile:",
                   choices = levels(factor(scf$networth_cat2)),
                   selected = "0-9.9",
                   width = "150%")
     ),
     fluidRow(
       column(12, align = "center",
              h4(textOutput("average"))
              )
       )
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$race_stack <- renderPlot({
    scf %>% 
      filter(networth_cat2 == input$wealth) %>% 
      group_by(race) %>% 
      summarize(count = n()) %>% 
      mutate(prop = count / sum(count),
             num = 1) %>% 
      select(race, prop, num) %>% 
      ggplot() +
      geom_bar(aes(x = num, y = prop, fill = factor(race)), width = 1, position = "stack", stat = "identity") +
      scale_x_continuous(limits = c(0, 2)) +
      ylab("Racial Makeup of Networth Percentile Group") + 
      xlab("") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      theme_scf_dist +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = thp_four,
                        name = "Race")
   })
  
  output$race_bar <- renderPlot({
    scf %>% 
      filter(networth_cat2 == input$wealth) %>% 
      group_by(race) %>% 
      summarize(count = n(),
                wealth = weighted.mean(NETWORTH, WGT)) %>% 
      mutate(prop = count / sum(count)) %>% 
      ggplot() +
      geom_bar(aes(x = reorder(factor(race), desc(race)), y = prop), stat = "identity", fill = rev(thp_four)) +
      ylab("Racial Makeup of Networth Percentile Group") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      theme(panel.background= element_blank(),
            panel.grid.minor.x = element_line(color = "lightgrey"),
            panel.grid.major.x = element_line(color = "lightgrey"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank()) +
      coord_flip()
  })

output$average <- renderText({
  paste("The average wealth in this bin is",
        wealth_lookup %>% 
          filter(networth_cat2 == input$wealth) %>% 
          select(mean))
})
   }

# Run the application 
shinyApp(ui = ui, server = server)

