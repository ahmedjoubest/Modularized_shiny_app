
### 1. HEADER ----

header <- 
  dashboardHeader(title = 'Exploratory Data')


### 2. SIDEBAR ----

sidebar <- 
  dashboardSidebar(
    ### Select Data set
    select_dataset("moduled"),
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Scatterplot",
        tabName = "scatter_pl",
        icon = icon("bar-chart-o")),
      conditionalPanel(
        "input.tabs == 'scatter_pl'",
        input_scatter_variables("moduled")
      ),
      menuItem(
        "Boxplot",
        tabName = "box_pl",
        icon = icon("bar-chart-o")),
      conditionalPanel(
        "input.tabs == 'box_pl'",
        input_boxplot_variables("moduled")
      ),
      menuItem(
        "Density plot",
        tabName = "dens_pl",
        icon = icon("bar-chart-o")),
      conditionalPanel(
        "input.tabs == 'dens_pl'",
        input_density_variables("moduled")
      ),
      menuItem("Table", tabName = "datatable", icon = icon("bar-chart-o"))
    )
  )


### 3. DASHBOARD BODY ----

body <- dashboardBody(
  styles(),
  useShinyalert(),
  rendertext_description("moduled"),
  fluidRow(
    ### Value boxes ----
    Valueboxes("moduled")
  ),
  
  tabItems(
    tabItem(tabName = 'scatter_pl',
            box(
              title = HTML("<b> Scatter Plot </b>"),
              br(),
              # scatter output
              scatter_plot("moduled"),
              width=12,status="primary", solidHeader = T)
    ),
    tabItem(tabName = 'box_pl',
            box(
              title = HTML("<b> Box Plot </b>"),
              br(),
              # boxplot output
              box_plot("moduled"),
              width=12,status="primary", solidHeader = T)
    ),
    tabItem(tabName = 'dens_pl',
            box(
              title = HTML("<b> Density of variables </b>"),
              br(),
              # density output
              density_plot("moduled"),
              width=12,status="primary", solidHeader = T)
    ),
    tabItem(tabName = "datatable",
            fluidRow(
              box(
                title = HTML("<b> Data table </b>"),
                br(),
                # Data table output
                datatable_output("moduled"),
                width=12,status="primary", solidHeader = T)
            ))
  )
)


### 3.3 put UI together ----

ui <- 
  dashboardPage(header, sidebar, body)
