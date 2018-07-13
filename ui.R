library(shiny)
library(plotly)
library(ggplot2)
library(shinydashboard)


header <- dashboardHeader(title = "Linear Algebgra Tutorial")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Vector Basics", tabName = "vector_basic"),
              menuItem("Vector Spaces",tabName = "space"),
              menuItem("Eigenvectors",tabName = "eigen"
              ),
              menuItem("Determinants",tabName = "determinant"),
              
              menuItem("Transformations",tabName = "transformation")
              
              
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "vector_basic",
      fluidRow(
        column(width = 12,
               uiOutput("vector_basic_math_title")
               
        )
      ),
      br(),
      br(),
      fluidRow(
        column(width = 12,
               splitLayout(cellWidths = c("5%", "10%","70%"),
                           uiOutput("vector_basic_math_v"),
                           column(width = 12,
                                  textInput("vector_basic_v1","",value = 3),
                                  textInput("vector_basic_v2","",value = 2)
                           ),
                           uiOutput("vector_basic_math_vector_text")

               )
               
        )
        
        
        
        
        
        
        
      ),
      fluidRow(
        column(width = 2,
               actionButton("vector_basic_plot_button","Animate")),
        column(width = 12,
                 plotlyOutput("vector_basic_plot")

        )
      )
      
      
    )
    
  )
  
  
  
)

dashboardPage(header, sidebar, body,skin = "black")