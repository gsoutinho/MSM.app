shinyUI(
  
  fluidPage(
    fluidRow(
    column(12,
           titlePanel("MULTI-STATE MODEL APPLICATION TOOL"),
           align='center')),
   navbarPage("MSM.App",
              
                   tabPanel("Introduction",
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                  column(12,h3("Methodology background"),
                                         checkboxGroupInput("concepts", "",
                                                            c('MSM.app','Survival analysis','Multi-stade models',
                                                              'Shiny applications architecture'),selected = 'MSM.app'))
                               
                              )),
                              mainPanel(
                                uiOutput('MSM.app'),
                                uiOutput('survival.analysis'),
                                uiOutput('MSM.analysis'),
                                uiOutput('Shiny.arch')
                              )))
             
             


)
)
)
