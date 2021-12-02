
### Select input for data set ----
select_dataset <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("select_dt"), label = "Select dataset",
                choices = unlist(dtset),
                selected = 'iris')
  )
}

### Module server for data set description text output ----
description_dataset_outputtext <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ### data description text ----
      output$data_description <- renderText({
        paste(
          Data_description$dataset[Data_description$dataset == input$select_dt],
          ":",
          Data_description$description[Data_description$dataset == input$select_dt]
        )
      })
    }
  )
}

### UI function for Scatter variables : X,Y and Color ----
input_scatter_variables <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('scatt_X')),
    uiOutput(ns('scatt_Y')),
    uiOutput(ns('scatt_COL'))
  )
}

### Define scatter variables X,Y and colors ----
define_scatter_variables <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      data <-  reactive({get(input$select_dt)})
      output$scatt_X <- renderUI({
        selectInput(inputId = ns('scatt_x'), label = "X variable:",
                    choices =  colnames(data())[unlist(lapply(data(), is.numeric))],
                    selected = NULL)
      })
      output$scatt_Y <- renderUI({
        selectInput(inputId = ns('scatt_y'), label = "Y variable:",
                    choices =  colnames(data())[unlist(lapply(data(), is.numeric))],
                    selected = colnames(data())[unlist(lapply(data(), is.numeric))][2])
      })  
      output$scatt_COL <- renderUI({
        selectInput(inputId = ns('scatt_col'), label = "Colored BY:",
                    choices =  c(colnames(data())[unlist(lapply(data(), is.factor))],""),
                    selected = "")
      })
    }
  )
}


### Module server for data set description text output ----
description_dataset_outputtext <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ### data description text ----
      output$data_description <- renderText({
        paste(
          Data_description$dataset[Data_description$dataset == input$select_dt],
          ":",
          Data_description$description[Data_description$dataset == input$select_dt]
        )
      })
    }
  )
}

### UI function for data set description ----
rendertext_description <- function(id) {
  ns <- NS(id)
  tagList(
    h3(verbatimTextOutput(ns("data_description"), placeholder = T))
  )
}
### Module server for value boxes ----
valuebox_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <-  reactive({get(input$select_dt)})
      # Number of rows
      output$nrow <- renderInfoBox({
        infoBox(
          "Number of rows",
          nrow(data()),
          icon = icon("table"),
          color = "navy"
        )
      })
      # Number of columns
      output$ncol <- renderInfoBox({
        infoBox(
          "Number of columns",
          ncol(data()),
          icon = icon("table"),
          color = "navy"
        )
      })
      # Linear correlation coefficient
      output$regression <- renderInfoBox({
        infoBox(
          "Linear correlation coefficient",
          summary(lm(data()[,input$scatt_x]~data()[,input$scatt_y]))$r.squared %>% 
            round(2),
          icon = icon("registered"),
          color = "navy"
        )
      })
    }
  )
}

### UI function for value boxes ----
Valueboxes <- function(id) {
  ns <- NS(id)
  tagList(
    valueBoxOutput(ns("nrow")),
    valueBoxOutput(ns("ncol")) ,
    valueBoxOutput(ns("regression"))
  )%>% withSpinner(color="#3C8DBC",type=4, size = 1.1)
}

### Scatter plot server ----
scatter_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <-  reactive({get(input$select_dt)})
      # scatter plot output
      output$scatt_plot <- renderPlotly({
        if(input$scatt_x != input$scatt_y){
          m <- c(input$scatt_x, input$scatt_y, input$scatt_col)
          x <- data()[, colnames(data()) %in% m]
          if(str_length(input$scatt_col)>0){
            colnames(x) <- c('x', 'y', 'color')
            plt <- ggplot(x, aes(x,y, color = color)) +
              geom_point()+theme_bw() +
              labs(x = paste0('', input$scatt_x), y = paste0('', input$scatt_y),
                   color = paste0('', input$scatt_col))+
              theme(axis.text = element_text(size=12),
                    text = element_text(size=12))
            ggplotly(plt)
          } else {
            colnames(x) <- c('x', 'y')
            plt <- ggplot(x, aes(x,y)) +
              geom_point()+theme_bw() +
              labs(x = paste0('', input$scatt_x), y = paste0('', input$scatt_y))+
              theme(axis.text = element_text(size=12),
                    text = element_text(size=12))
            ggplotly(plt)
          }
        } else {
          shinyalert(text = HTML(
            "<p> Please choose X and Y different, otherwise the scatterplot will be meaningless </p>"),
            type = "info",
            timer = 0,
            animation = "pop",
            showConfirmButton = TRUE,
            html = TRUE,
            immediate = T)
        }
      })
    }
  )
}

### UI function for scatter plot ----
scatter_plot <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("scatt_plot")) %>%
      withSpinner(color="#3C8DBC",type=4, size = 1.1)
  )
}

### UI function for Scatter variables : X,Y and Color ----
input_scatter_variables <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('scatt_X')),
    uiOutput(ns('scatt_Y')),
    uiOutput(ns('scatt_COL'))
  )
}

### Define box plot variables X and Y ----
define_boxplot_variables <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      data <-  reactive({get(input$select_dt)})
      ### box plot X and Y----
      output$box_X <- renderUI({
        selectInput(inputId = ns('box_x'), label = "X variable:",
                    choices =  colnames(data())[unlist(lapply(data(), is.numeric))],
                    selected = NULL)
      })
      output$box_Y <- renderUI({
        selectInput(inputId = ns('box_y'), label = "Y variable:",
                    choices =  colnames(data())[unlist(lapply(data(), is.numeric))],
                    selected = colnames(data())[unlist(lapply(data(), is.numeric))][2])
      })
    }
  )
}

### UI function for box plot variables : X,Y ----
input_boxplot_variables <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('box_X')),
    uiOutput(ns('box_Y'))
  )
}

### box plot server ----
boxplot_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <-  reactive({get(input$select_dt)})
      # box plot output
      output$box_plot <- renderPlotly({
        m <- c(input$box_x, input$box_y)
        x <- data()[, colnames(data()) %in% m]
        if(input$box_x != input$box_y){
          x <- x[,m]
          colnames(x) <- c('x', 'y')
          plt <- ggplot(x, aes(x,y)) +
            geom_boxplot()+theme_bw() + 
            labs(x = paste0('', input$box_x), y = paste0('', input$box_y))+
            theme(axis.text = element_text(size=12),
                  text = element_text(size=12))
          ggplotly(plt)
        } else{
          shinyalert(text = HTML(
            "<p> Please choose data with more than 1 quantitative variable, then select X and Y as different variables </p>"),
            type = "info",
            timer = 0,
            animation = "pop",
            showConfirmButton = TRUE,
            html = TRUE,
            immediate = T)
        }
      })
    }
  )
}

### UI function for box plot ----
box_plot <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("box_plot")) %>%
      withSpinner(color="#3C8DBC",type=4, size = 1.1)
  )
}

### Define density plot variables X and color ----
define_density_variables <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      data <-  reactive({get(input$select_dt)})
      ### box plot X and Y----
      output$den_X <- renderUI({
        selectInput(inputId = ns('den_x'), label = "X variable:",
                    choices =  colnames(data())[unlist(lapply(data(), is.numeric))],
                    selected = NULL)
      })
      output$den_COL <- renderUI({
        selectInput(inputId = ns('den_col'), label = "Colored BY:",
                    choices =  c(colnames(data())[unlist(lapply(data(), is.factor))],''),
                    selected = '')
      })
    }
  )
}

### UI function for density plot variables : X and color ----
input_density_variables <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('den_X')),
    uiOutput(ns('den_COL'))
  )
}

### density plot server ----
density_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <-  reactive({get(input$select_dt)})
      # scatter plot output
      output$den_plot <- renderPlotly({
        
        if(str_length(input$den_col)>0){
          
          m <- c(input$den_x, input$den_col)
          x <- data()[, colnames(data()) %in% m]
          x <- x[,m]
          colnames(x) <- c('x', 'color')
          plt <- ggplot(x, aes(x,colour = color)) +
            geom_density()+ theme_bw() + 
            labs(x = paste0('', input$den_x))+
            theme(axis.text = element_text(size=12),
                  text = element_text(size=12))
          ggplotly(plt)
        } else {
          m <- c(input$den_x)
          x <- data()[, colnames(data()) %in% m,drop=FALSE]
          x <- x[,m,drop=FALSE]
          colnames(x) <- 'x'
          plt <- ggplot(x, aes(x)) +
            geom_density()+theme_bw() + 
            labs(x = paste0('', input$den_x))+
            theme(axis.text = element_text(size=12),
                  text = element_text(size=12))
          ggplotly(plt)
        }
      })
    }
  )
}

### UI function for density plot ----
density_plot <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("den_plot")) %>%
      withSpinner(color="#3C8DBC",type=4, size = 1.1)
  )
}

### data.table server ----
datatable_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      data <-  reactive({get(input$select_dt)})
      # data table output
      output$Crop_data <-
        DT::renderDataTable(
          datatable(data(),
                    filter = 'top',
                    rownames = F,
                    options = list(scrollX = TRUE)
          )
        )
    }
  )
}

### UI function data.table ----
datatable_output <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("Crop_data")) %>%
      withSpinner(color="#3C8DBC",type=4, size = 1.1)
  )
}

### Styling function ----
styles <- function(){
  tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #222D32;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #222D32;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #222D32;
        }
        .box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#222D32
                    }

.box.box-solid.box-primary{
border-bottom-color:#222D32;
border-left-color:#222D32;
border-right-color:#222D32;
border-top-color:#222D32;
}

"#pred{color: black;
                                 font-size: 20px;
                                 font-family: Source Sans Pro
                                 }"
                              ')))
}