
library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Shiny Projekt"),
    h3("Vergleiche die Stadt und Land Reichweite von Fahrzeugen"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            #select manufacturer
            selectInput(inputId = "manu",
                        label = "Wähle den Hersteller",
                        choices = c(unique(mpg$manufacturer)),
                        selected = NULL),
            #select model
            selectInput(inputId = "model",
                        label = "Wähle das Model",
                        choices = c(unique(mpg$model)),
                        selected = NULL,
                        multiple = FALSE),
            #selectInput(inputId = "x_var2",
            # label = "Wähle deine zweite unabhängige Variable",
            # choices = c(names(mpg)),
            # selected = NULL,
            # multiple = TRUE),
            
            actionButton(inputId = "button", label = "Los!"),
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("info"),
            
            h4("Highway Reichweite"),
            plotOutput("plot_hwy"),
            
            h4("Stadt Reichweite"),
            plotOutput("plot_cty"),
            
           
            
            
            
            
        )
    )
)

# Define server logic 
server <- function(input, output,session) {
    
    output$info <- renderText({
        paste("Dein Hersteller ist", input$manu, "und dein Model ist", input$model)
    })
    
    
    
    mpg_manu <-  reactive({
        mpg %>% filter(manufacturer == input$manu)
    })
    
    
    observe({
        choices_model <- unique(mpg_manu()$model[mpg_manu()$manufacturer == input$manu])
        updateSelectInput(session, inputId = "model", choices = choices_model)
    })
    
    mpg_model <- reactive({
        mpg_manu() %>% 
            filter(model == input$model)
    })
    
    
    
    
    output$plot_hwy <-  renderPlot({
        ggplot(mpg_model(), aes(model,hwy)) +
            geom_point(aes(size=displ)) +
            facet_grid(cols = vars(year))
    })
    
    output$plot_cty <- renderPlot({
        ggplot(mpg_model(), aes(model,cty)) +
            geom_point(aes(size=displ)) +
            facet_grid(cols = vars(year))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
