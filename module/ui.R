

ui <- fluidPage(
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
