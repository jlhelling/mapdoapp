library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "sidebar map    area2 ",
    "sidebar map    area2 ",
    "sidebar map    area2 ",
    "sidebar profil profil"
  ),
  row_sizes = c(
    "85px",
    "1fr",
    "0.6fr",
    "1.4fr"
  ),
  col_sizes = c(
    "250px",
    "1.7fr",
    "0.3fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Metriques"),
    card_body_fill(
      sliderInput(
        inputId = "bins",
        label = "Number of Bins",
        min = 12,
        max = 100,
        value = 30,
        width = "100%"
      ),
      numericInput(
        inputId = "numRows",
        label = "Number of table rows",
        value = 10,
        min = 1,
        step = 1,
        width = "100%"
      )
    )
  ),
  grid_card_plot(area = "map"),
  grid_card(
    area = "area2",
    full_screen = TRUE,
    card_header(
      "Filtre
      "
    )
  ),
  grid_card(
    area = "profil",
    card_body_fill(
      tabsetPanel(
        tabPanel(
          title = "Empty Tab",
          plotlyOutput(outputId = "plot")
        ),
        tabPanel(
          title = "Empty Tab",
          plotlyOutput(outputId = "plot")
        )
      )
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  

