library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

##Preapare data
df2 <- read.csv("us-states.txt")
gruop1 <- df2 %>% select(state, deaths) %>%
      group_by(state) %>%
      summarise(mean= mean(deaths))  %>%
      arrange(desc(mean) , .by_group = TRUE)%>%
      slice(1:10)%>% ungroup

x2 <- df2 %>% filter(state %in% as.character (gruop1$state)) %>% ungroup
x2$date <- as.Date(as.character.Date(x2$date), format = "%Y-%m-%d")


# Define UI 
ui <- fluidPage(
  h2(titlePanel("Accumulated Daily COVID deaths in top ten US STATES")),
  sidebarLayout(
         sidebarPanel(
               h3("Select interval of interest"),
            dateRangeInput('dateRange',
            label = 'Filter death rate by date',
            start = as.Date('2020-01-24') , end = as.Date('2021-03-24'),
            min   = "2020-01-24",
            max   = "2021-03-24"
            )
         ),
      # Show the plot
      mainPanel(
         plotOutput("CovidPlot"),
         br(),
         h3(textOutput("death"))
      )
   )
)

# Define server logic required to plot the graph
server <- function(input, output, sesion) {
   output$CovidPlot <- renderPlot({
         df <- dplyr::filter(x2, date >= input$dateRange[1] & date <= input$dateRange[2])
            ggplot() +
            geom_point(data=df, aes(x=date, y=deaths, colour=state)) +
            labs (title="Daily track Covid deaths, top 10 states in US, 2020-2021",
                  y="daily deaths", x= "Months from Jan 1, 2020 to March 2021") +
            scale_x_date(breaks =date_breaks("months"),labels = date_format("%b"))
            })
   output$death <- renderText({ 
         max1  <- x2 %>% filter(date == input$dateRange[1])
         max2  <- x2 %>% filter(date == input$dateRange[2])
         suma1 <- sum(max1$death)
         suma2 <- sum(max2$death)
         total= suma2 - suma1
         paste("Total deaths in the selected period:", total)
         
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

