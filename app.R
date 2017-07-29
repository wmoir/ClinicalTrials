

library(shiny)
library(tidyverse)


sponsors <- read_csv('sponsors.csv')

ui <- fluidPage(
  titlePanel("Clinical Trials by Primary Sponsor"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("sponsor", 
                         label = h3("Primary Sponsor"),
                         choices = c("Industry" = "Industry",
                                     "NIH" = "NIH", 
                                     "Hospital" = "Hospital", 
                                     "University" = "University", 
                                     "Foundation" = "Foundation", 
                                     "Research Institute" = "Research Institute", 
                                     "U.S. Fed" = "U.S. Fed", 
                                     "Other" = "Other"),
                         selected = c("Industry","NIH","Hospital","University","Other")), 
      sliderInput("numcollabInput", "Number of Collaborators", min = 1, max = 92,
                  value = c(1, 92)),
      sliderInput("year", "Years", min = 1999, max = 2017,
                  value = c(2008, 2016), sep=""),
      br(),
      p("Data was obtained from ",
        a("the Clinical Trials Transformation Initiative",
          href = "http://aact.ctti-clinicaltrials.org/connect",
          target = "_blank")),
      p("Data is originally from ",
        a("ClinicalTrials.gov",
          href = "https://clinicaltrials.gov/",
          target = "_blank")
      ),
      p("The idea for these plots was inspired by",
        a("K. Chiswell",
          href = "http://aact.ctti-clinicaltrials.org/use_cases/2",
          target = "_blank")
      ),
      p("The shiny code was mostly taken from",
        a("Dean Attal's tutorial",
          href = "http://deanattali.com/blog/building-shiny-apps-tutorial/",
          target = "_blank")
      ),
      p("To see the code, check out my",
        a("Github",
          href = "https://github.com/wmoir/ClinicalTrials",
          target = "_blank")
        ),
      br(),
      strong("These are very preliminary analyses and should not be taken seriously -
              there are almost certainly errors. Any mistakes or subpar coding are my responsibility")
    ),
    mainPanel(plotOutput("funderplot"))
  )
)

server <- function(input, output) {
  output$funderplot <- renderPlot({
    filtered <-
      sponsors %>%
      filter(n_collab >= input$numcollabInput[1],
             n_collab <= input$numcollabInput[2],
             year >= input$year[1],
             year <= input$year[2],
             class %in% input$sponsor
      ) %>%
      group_by(year,class) %>%
      summarise(n = n())
    
    ggplot(filtered, aes(x=year, y=n, color=class)) +
      geom_line()
  })
}


shinyApp(ui = ui, server = server)
