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
                              ))),
             
              tabPanel("Input files",
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                            uiOutput('manage.files1'),
                            uiOutput('variables.choice'),
                            uiOutput('manage.files2'),
                            verbatimTextOutput('tipo.variaveis'))
                                               ),
                         mainPanel(
                           verbatimTextOutput('view.transitions'),
                          
                           #textOutput('surv.confirm'),
                           #uiOutput('resp.files.surv'),
                           #dataTableOutput('view.data_files.surv')
                           uiOutput('resp.files.manage'),
                           #textOutput('view.data_files.manage_text'),
                           dataTableOutput('view.data_files.manage')
                         ))),
             
             navbarMenu("Survival Analysis",
                      
                      tabPanel("Kaplan-Meier estimator",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          fluidRow(
                            uiOutput('KM') 
                            
                          )),
                        
                        mainPanel(
                          
                          verbatimTextOutput('summary.KM'),
                          plotOutput('KM.plot',width = '60%')
                          
                          
                        )
                      )),
                      tabPanel("Compare curves",
                               sidebarLayout(
                                 
                                 sidebarPanel(
                                   
                                   fluidRow(
                                     uiOutput('LR') 
                                     
                                   )),
                                 
                                 mainPanel(
                                   
                                   verbatimTextOutput('summary.LR')
                                   
                                   
                                 )
                               )),
                      tabPanel("Cox PH Model",
                               sidebarLayout(
                                 
                                 sidebarPanel(
                                   
                                   fluidRow(
                                     uiOutput('cox') 
                                     
                                   )),
                                 
                                 mainPanel(
                                   
                                   verbatimTextOutput('summary.cox')
                                   
                                   
                                 )
                               )),
                      tabPanel("Parametric models",
                               sidebarLayout(
                                 
                                 sidebarPanel(
                                   
                                   fluidRow(
                                     uiOutput('parametric') 
                                     
                                   )),
                                 
                                 mainPanel(
                                   
                                   verbatimTextOutput('summary.parametric')
                                   
                                   
                                 )
                               ))
                      
                      
             ),
             #tabPanel("IDM - Data File",
             #         sidebarLayout(
             #         sidebarPanel(
             #          fluidRow(
             #           uiOutput('files'))
             #),
             # mainPanel(
             #   textOutput('tipoAcao'),
             #   uiOutput('resp.files'),
             #   dataTableOutput('view.data_files')
        
             #))),
             navbarMenu("IDM - Analysis",
                        
                        tabPanel("Number of events",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                        uiOutput('type_of_event') 
                                        
                                   )),
                                   
                                   mainPanel(
                                     
                                     verbatimTextOutput('summary.type') #se tivesse posto renderText apenas dava os valores
                                     
                                     
                                   )
                                 )),
                        tabPanel("Regression models",
                                           sidebarLayout(
                                            
                                            sidebarPanel(
                                              
                                              fluidRow(
                                                uiOutput('regression') 
                                                
                                              )),
                                            
                                            mainPanel(
                                              
                                              verbatimTextOutput('regression.summary')#,
                                              #textOutput('variables.selected') 
                                            )
                                          )),
                        tabPanel("Transition Probabilities",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                       uiOutput('tp') 
                                       
                                     )),
                                   
                                   mainPanel(
                                     
                                     verbatimTextOutput('tp.summary'),
                                     #textOutput('tp.confirm'),
                                     plotOutput('tp.plot',width = '80%')
                                   )
                                 )),
                        tabPanel("Cumulative Incidence Function (CIF)",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                       uiOutput('cif') 
                                       
                                     )),
                                   
                                   mainPanel(
                                     
                                     verbatimTextOutput('cif.summary') 
                                    
                                     
                                   )
                                 ))
                        #,tabPanel("Sojourn distribution",
                        #         
                        #                  sidebarLayout(
                        #                    
                        #                    sidebarPanel(
                        #                      
                        #                      fluidRow(
                        #                        uiOutput('sojourn') 
                        #                        
                        #                      )),
                        #                    
                        #                    mainPanel(
                        #                      
                        #                     verbatimTextOutput('sojourn.summary') 
                        #                      
                        #                    )
                        #                  ))
                        ),
            # tabPanel("MSM - Data File",
            #          sidebarLayout(
            #            sidebarPanel(
            #              fluidRow(
            #                uiOutput('files.msm'))
            #            ),
            #            mainPanel(
            #              #textOutput('tipoAcao'),
            #              uiOutput('resp.files.msm'),
            #              dataTableOutput('view.data_files.msm')
            #              
            #            ))),
             navbarMenu("MSM - Analysis",
                        
                        tabPanel("Transition Probabilities",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                       uiOutput('tp.msm') 
                                       
                                     )),
                                   
                                   mainPanel(
                                     verbatimTextOutput('view.variables'),
                                     verbatimTextOutput('tp.summary.msm')
                                     #textOutput('tp.confirm'),
                                     #plotOutput('tp.plot',width = '80%')
                                   )
                                 ))  
             ),
             navbarMenu("Tests for the Markov condition",
                        
                        tabPanel("Local tests",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                       uiOutput('localTests1'),
                                       uiOutput('localTests2')
                                     )),
                                   
                                   mainPanel(
                                     
                                     verbatimTextOutput('localTest.summary')
                                     #verbatimTextOutput('localTest.confirm')
                                     #plotOutput('tp.plot',width = '80%')
                                   )
                                 )),
                        tabPanel("Global tests",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     fluidRow(
                                       uiOutput('globalTests1'),
                                       uiOutput('globalTests2')
                                     )),
                                   
                                   mainPanel(
                                     
                                     verbatimTextOutput('globalTest.summary')
                                     #textOutput('localTest.confirm'),
                                     #plotOutput('tp.plot',width = '80%')
                                   )
                                 ))
             )
             
             


)
)
)
