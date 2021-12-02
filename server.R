
### global call
source("global.R")

### Server ----

server <- function(input, output, session) { 
  
  ### Read data from filter ----
  data <-  reactive({get(input$select_dt)})
  
  ### data description text ----
  description_dataset_outputtext("moduled")
  
  ### Value boxes ----
  valuebox_server("moduled")
  
  ### Define Scatter variables : X,Y and color ----
  define_scatter_variables("moduled")
  
  ### Scatter plot server ----
  scatter_server("moduled")
  
  ### Define box variables : X and Y ----
  define_boxplot_variables("moduled")
  
  ### Scatter plot server ----
  boxplot_server("moduled")
  
  ### Define density plot variables: X and color ----
  define_density_variables("moduled")
  
  ### density plot server ----
  density_server("moduled")
  
  ### data table server ----
  datatable_server("moduled")
}
