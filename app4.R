library(shiny)
library(tidyverse)
source("module/module_login.R")
library(shinyjs)


# Define UI for application that draws a histogram-----
ui <- fluidPage(
  useShinyjs(), # include shinyjs

  # login form as defined in the module
  login_ui(id = "module_login", title = "Please login"),

  # app
  uiOutput(outputId = "display_content_module")
)


# Define server logic -----
server <- function(input, output, session) {

  # create userbase
  user_base_module_tbl <- tibble(
    user_name = "shiny",
    password = "dankefabian"
  )

  # check credentials vs tibble
  validate_password_module <- callModule(
    module = validate_pwd,
    id = "module_login",
    data = user_base_module_tbl,
    user_col = user_name,
    pwd_col = password
  )

  output$display_content_module <- renderUI({
    req(validate_password_module())

    print(validate_password_module())

    fluidPage(
      div(
        class = "bg-success",
        id = "success_module",
        h4("Access confirmed!"),
        p("Welcome to your module-secured application!")
      ),
      # Application title
      titlePanel("Shiny Projekt"),
      h3("Vergleiche die Stadt und Land Reichweite von Fahrzeugen"),

      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(

          # select manufacturer
          selectInput(
            inputId = "manu",
            label = "Wähle den Hersteller",
            choices = c(unique(mpg$manufacturer)),
            selected = NULL
          ),
          # select model
          selectInput(
            inputId = "model",
            label = "Wähle das Model",
            choices = c(unique(mpg$model)),
            selected = NULL,
            multiple = FALSE
          ),
          # selectInput(inputId = "x_var2",
          # label = "Wähle deine zweite unabhängige Variable",
          # choices = c(names(mpg)),
          # selected = NULL,
          # multiple = TRUE),

          actionButton(inputId = "button", label = "Los!"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Info",
              h3("App Beschreibung"),
              p(
                "Mit dieser Shiny App kannst du selber Plots aus dem Datenset mpg", br(),
                "Dieses Datenset ist am Packet", strong("tidyverse"), "angehängt", br(),
                "Im Moment hast du folgende Daten ausgewählt:", br(),
                textOutput("info")
              )
            ),
            tabPanel(
              title = "Plots",
              h4("Highway Reichweite"),
              plotOutput("plot_hwy"),
              h4("Stadt Reichweite"),
              plotOutput("plot_cty")
            ),
            
            tabPanel(
              title = "Table",
              h4("Die Daten von deinem gewählten Fahrzeug"),
              tableOutput("table"),
              h4("Zusammenfassung"),
              tableOutput("summary")
              
            ),
            
            tabPanel(
              title = "Bestes Fahrzeug",
              h4("Die besten Ausführungen der Modelle bezüglich der Reichweite sind:"),
              h5("Highway"),
              tableOutput("besthwy"),
              h5("Stadt"),
              tableOutput("bestcty")
            )
          )
        )
      )
    )
  })

  observe(
    if (validate_password_module() == TRUE) {
      output$info <- eventReactive(input$button, {
        paste("Dein Hersteller ist", input$manu, "und dein Model ist", input$model)
      })

      mpg_manu <- reactive({
        # Du musst in einem reaktiven Kontext checken, ob der Input != null ist,
        # da das UI durch den Server erst erzeugt wird (nach der Authorisierung).
        if (is_null(input$manu)) {
          return(c(unique(mpg$manufacturer)))
        }
        mpg %>% filter(manufacturer == input$manu)
      })

      observe({
        # … also ist auch hier ein Check erfoderlich.
        if (!is_null(input$manu)) {
          choices_model <- unique(mpg_manu()$model[mpg_manu()$manufacturer == req(input$manu)])
          updateSelectInput(session, inputId = "model", choices = choices_model)
        }
      })

      mpg_model <- eventReactive(input$button, {
        mpg_manu() %>%
          filter(model == input$model)
      })

      output$plot_hwy <- renderPlot({
        ggplot(mpg_model(), aes(model, hwy)) +
          geom_point(aes(size = displ, color = as.factor(displ))) +
          facet_grid(cols = vars(year))
      })

      output$plot_cty <- renderPlot({
        ggplot(mpg_model(), aes(model, cty)) +
          geom_point(aes(size = displ, color = as.factor(displ))) +
          facet_grid(cols = vars(year))
      })
      
      output$table <- renderTable({
        mpg_model()
      })
      
      output$summary <- renderTable({
        mpg_model() %>% 
          group_by(year) %>% 
          summarise(Durschnitt_Highway = mean(hwy), Durchschnitt_Stadt = mean(cty)) 
      })
      
      output$besthwy <- renderTable({
        mpg_model() %>% 
          slice_max(hwy, n=1) %>% 
          select(manufacturer, model, year, trans, hwy)
      })
      
      output$bestcty <- renderTable({
        mpg_model() %>% 
          slice_max(cty, n=1) %>% 
          select(manufacturer, model, year, trans, cty)
      })
      
     
      
      
    }
  )
}



# Run the application
shinyApp(ui = ui, server = server)
