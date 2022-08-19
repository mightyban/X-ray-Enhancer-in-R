

library(shiny)
library(EBImage)
library(imager)
library(magick)
#library(shinythemes)
#library(waveslim)

ui <- fluidPage( 
                 
                 # Application title
                 titlePanel(h1(strong("X-RAY ENHANCER"))),
                 
                 # Sidebar with inputs
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(12, 
                              fileInput("image", "Upload an Image"),
                              textInput("size", "Size", value = "500x500"),
                       )),
                     
                     fluidRow(
                       column(12,
                              h3(strong("Denoise Image")),
                              h4("Median Filter"),
                              sliderInput("MedianFilter", "Size", 0, 5, 3),
                              ("-----------------------------------------"), 
                              h4("Wavelet Thresholding"),
                              
                              
                       )),
                     
                     
                     fluidRow(
                       column(12, 
                              ("-----------------------------------------"),
                              h4(strong("...via Contrast Limited AHE")),  
                              uiOutput('slider'),
                              sliderInput(inputId = "limit", 
                                          label = "Limit", 
                                          min = 1, 
                                          max = 10,
                                          step = .5,
                                          value = 2),
                              actionButton("clahe.button", 
                                           "ENHANCE", 
                                           icon("hand-sparkles"), 
                                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                           width = 150),
                              br(),
                              br(),
                              br(),
                       )),
                     fluidRow(
                       column(12,
                              ("-----------------------------------------"),
                              h4(strong("...via Histogram Equalization")),
                              actionButton("he.button", "ENHANCE", 
                                           icon("hat-wizard"), 
                                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                           width = 150),
                              br(),
                              br(),
                              actionButton("b", "median filter"),
                              br(),
                              
                       )),
                     
                     fluidRow(
                       column(12,
                              ("-----------------------------------------"), 
                              br(),
                              actionButton("clear.button", "CLEAR", icon("trash"))
                              
                       ))),
                   
                   
                   mainPanel(
                     
                     column(6, h4(strong("Original Image")),
                            plotOutput("original")),
                     
                     column(6, h4(strong("Enhanced Image via CLAHE")),
                            plotOutput("res_CLAHE")),
                     
                     column(6, h4(strong("Enhanced Image via HE")),
                            plotOutput("res_HE")),
                     
                   )
                 )
)
server <- function(input, output) {
  
  img <- reactive({
    f <- input$image
    if (is.null(f))
      return(NULL) 
    readImage(f$datapath)
  })
  
  divs = reactive({
    cat("DIV\n")
    d = req(dim(img()))
    vals <- intersect(
      which(d[1L] %% seq_len(d[1L]) == 0L),
      which(d[2L] %% seq_len(d[2L]) == 0L)
    )[-1L]
    vals[vals<=16L]
  })
  
  output$slider <- renderUI({
    values = req(divs())
    sliderInput(inputId = "nx", label = "nx",
                min = 0, max = length(values) - 1L,
                step = 1, ticks = FALSE,
                value = which(values==8L) - 1L)
  })
  
  CLAHE <- reactive({
    req(input$nx)
    cat("res\n")
    nx = divs()[input$nx+1L]
    limit = input$limit
    
    clahe(img(), nx=nx, ny=nx, limit=limit,keep.range = TRUE)
    
  })
  
  
  HE <- reactive({
    equalize(img())
  })
  
  mf <- reactive({
    
    medianFilter(img(), size = 3)
    
  })
  
  mfc <- eventReactive(input$b, {
    req(mf())
    plot(mf(), all=TRUE)
  })  
  
  CLAHE.click <- eventReactive(input$clahe.button, {
    req(CLAHE())
    plot(CLAHE(), all=TRUE)
  })  
  
  HE.click <-  eventReactive(input$he.button, {
    req(HE())
    plot(HE(), all=TRUE)
  }) 
  
  
  output$res_HE <- renderPlot({
    HE.click()
  })
  
  output$res_HE <- renderPlot({
    mfc()
  })
  
  output$res_CLAHE <- renderPlot({
    CLAHE.click()
    
  })
  
  output$original <- renderPlot({
    req(img())
    plot(img(), all=TRUE)
  })
  
  
  CLEAR.click <- eventReactive(input$clear.button, {
    removeUI(output)
    
  }) 
  # Numeric operators
  tmpfile <- image %>%
    image_rotate(input$rotation) %>%
    image_implode(input$implode) %>%
    image_blur(input$blur, input$blur) %>%
    image_resize(input$size) %>%
    image_write(tempfile(fileext='jpg'), format = 'jpg')
  
  # Return a list
  list(src = tmpfile, contentType = "image/jpeg")
}



shinyApp(ui, server)

