#########################################################################################

shinyServer(function(input, output) {
  
  #Surv - Data File
  setwd("C:/Users/asus/Desktop/PEN/_LA_6/35_app.MSM/appMSM")
  
  
  output$MSM.app<- renderUI({
    
      if('MSM.app' %in% input$concepts){
       
        fluidRow(
          
          column(12, h3("MSM.app:"),
                 
                 paste('The development of applications for obtaining interpretable results in a simple and
summarized manner in multi-state models is a research field with a large potential,
namely in what refers to use open source tools that can be easily implemented in
biomedical applications. This paper introduces MSM.app, an interactive web appli-
cation, using shiny package for R language, which enables any users, regardless the
previous informatics experience, to perform a dynamic analysis involving the most
important topics in multi-state models. Among these, the MSM.app aims to improve
the communication and knowledge sharing of the estimation of transition probabil-
ities, study of the effect of different predictors on the outcome or the application of
recent methods for checking the Markov assumption. The classical survival analysis
is also included concerning the estimation of survival curves, comparison of group
curves or the inference of regression models. The MSM.app application comprises a
set of dynamic web forms, tables and graphics whose usability is illustrated using
real data examples.')))
        
      }else{
        return(NULL)

      }
    
  })
    
  output$survival.analysis<- renderUI({
    
    if('Survival analysis'  %in% input$concepts ){
   
      fluidRow(
        column(12, h3("Survival analysis:"),paste('Survival analysis can be seen as a set of statistical
    procedures for data analysis for
which the outcome variable of interest is time until an event occurs. In biomedical
applications, some examples of events may be time to death or time to first recurrence
of a tumor after an initial treatment. Regarding the dynamic nature of survival data,
in practice, time to an event cannot be observed due to a deliberate design or random
censoring. In particular, right censoring occurs if the event of interest has not been observed
when the data was evaluated. Some reasons for that may be a loss of follow-up,
drop out or termination of study. The survival function S(t) gives the probability that
a person survives longer than some specified time t. The nonparametric Kaplan-Meier
estimator is the classical method for estimating the survival functions for one or more
groups of individuals which is a generalization of the empirical estimator for censored
data. To derive a confidence interval for all time points we can use the Greenwood
estimator for the variance. To compare curves between groups two nonparametric tests
are usually used: the log-rank is powerful to test differences that fit the proportional
hazard models. This test is appropriate to be used when the data are right skewed
and censored, giving equal weight to early and late failures. The Gehan-Wilcoxon test
is the other which is more sensitive to differences at earlier time points. Among the
models to assess the effect of a vector of covariates the Cox proportional hazard model
(Cox, 1972) is the most frequently used in the literature. It is also possible to consider
parametric models of the baseline hazard from standard survival distributions such as
exponential, weibull or gamma.')))
    }else{
      return(NULL)
   
    }
    
  })
    

  
  output$MSM.analysis<- renderUI({
    
    if('Multi-stade models'  %in% input$concepts ){
      
      fluidRow(
        column(12, h3("Multi-state models:"),paste('A multi-state model is a model for a time continuous stochastic process 
        (Y (t); t >= 0) which at any time occupies one of a few possible states. They can be seen as a generalization of survival 
        analysis in which
survival is the ultimate outcome of interest but where information is available about
intermediate events which individuals may experience during the study period. They
have been widely used to analyzed complex longitudinal and survival data allowing to
obtain a detailed view of the progression of the disease (Andersen, Borgan, Gill and
Keiding, 1993; Hougaard, 2000; Putter, Fiocco and Geskus, 2007; Meira-Machado,
de Una-Alvarez, Cadarso-Suarez and Andersen, 2009; Meira-Machado and Sestelo,
2019). The multi-state process can
be fully characterized through transition intensities or transition probabilities. The transition intensities are the instantaneous
hazards for movement from one state to
another. Covariates may be incorporated in the models in order to explain differences
among individuals in the course of the illness. In fact, an important goal in multistate
modeling is to study the relationships between the different predictors and the
outcome. The most common models are characterized through one of the two model
assumptions that can be made about the dependence of the transition intensities and
time. The transition intensities may be modeled using separated Cox models assuming
the process to be Markovian or using a semi-Markov model in which the future of the
process does not depend on the current time but rather on the duration in the current
state.
For two states $h$, $j$ and two time points s < t, introduce the so-called transition probabilities
p_{hj}(s,t)=P(Y(t)=j|Y(s)=h). In the progressive illness-death model there are five different transition probabilities
to estimate: p_{00}(s,t), p_{01}(s,t), p_{02}(s,t), p_{11}(s,t) and p_{12}(s,t). Since p_{00}(s,t)+p_{01}(s,t)+p_{02}(s,t)=1 and 
p_{11}(s,t)+p_{12}(s,t)=1, in practice one only needs to estimate three of these quantities. Estimating these quantities is
interesting, since they allow for long-term predictions of the process. When the multi-state model is Markovian, 
the Aalen-Johansen estimator gives consistent estimators of the transition probabilities. When the process is not Markovian, 
the Aalen-Johansen estimator of the transition probabilities may introduce some bias and therefore they may be inappropriate. 
The paper by de Una-Alvarez and Meira-Machado (2015) introduces the landmark estimators (LM) based on subsampling  which do not
rely on the Markov assumption. The idea behind subsampling, also referred to as landmarking (Van Houwelingen, 2007), is to consider
                                                   the subset of individuals observed in State h by time s. Subsampling, was later 
                                                   used by (Putter and Spitoni, 2018) to derive a landmark Aalen-Johansen estimator
                                                   (LMAJ) of the transition probabilities.')))
    }else{
      return(NULL)
      
    }
    
  })
  
  
  
  output$Shiny.arch<- renderUI({
    
    if('Shiny applications architecture' %in% input$concepts ){
      
      fluidRow(
        column(12, h3("Shiny applications architecture:"),paste('The shiny package, developed for R language by RStudio, has became a popular way to create and deploy web server applications (Walker, 2016). Through the combination of the structure of shiny framework with R codes, one can easily to build interactive web applications with minimal effort allowing to obtain results or graphics without being necessary to run R codes or have any knowledge of HTML, CSS or JavaScript (Chang, 2017; Kaushik, 2016). Shiny applications also provide the integration with other R packages, JavaScript libraries or CSS customization and they are under the GPL-2 open source license (Seal and Wild, 2016).
The structure of shiny applications build upon two components: the user-interface scripts for the layout of the application where the outputs are displayed (ui.R); and the other given by the server scripts with the instructions of the application (server.R) (Govan, 2016).
Some of the interactive elements existing in ui.R file are buttons, sliders, selection boxes or check boxes that allow the user to introduce values to be executed from server.R file. This way, any user, regardless of their knowledge of R language, can interactively analyse data sets.
All codes for the presentation of the application must be included within the layouts functions "fluidPage" which allows to create canvas for the interface. This "fluidPage" element also enables the page layout to be adjusted to the dimensions of the browser. For example, other layouts functions are "tabsetPanel" and "navlistPanel", which allow the user-interface to be divided into discrete sections. Some of these elements take names equivalents to the HTML, such as "hr()" or "br()", respectively, to create division lines or breaks (Wojciechowski, Hopkins and Upton, 2015).
All instructions used to build the web application are required to be in server.R file, more precisely, within the function "serverShiny". This function requires an input object (values called from ui.R) and an output object (values to be called by ui.R. This way, when a widget is changed, the value for the reactive object is updated to reflect this new situation. 
Rendering functions are used to return reactive outputs such as "plot", "table" or "text" to be run in "ui.R". For instance, with ggplot2 graphics package, the code in "renderPlot" should return a ggplot object (RStudio, 2021). 
A shiny application can be run on a localhost where R and shiny package are installed. This is the easiest way to share, when the users are familiar with R, being only necessary execute the instructions to run the app (Varma and Virmani, 2017). 
When using the RStudio services, shiny applications can be host of three ways to be accessed by internet: ShinyApps.io, Shiny Server, and Shiny Server Pro (Wojciechowski, Hopkins and Upton, 2015).
Shinyapps.io is a self-service platform that makes it easy to share shiny applications on the web. The service runs in the cloud on shared servers that are operated by RStudio. Each application is self-contained and operates on either data that is uploaded with the application, or data that the code pulls from third-party data stores, such as databases or web services (RStudio, 2021b). 
Shiny server is totally free and open source, stable and well featured (Beeley, 2013). Shiny server pro is a commercial version with enhanced security (possibility allowing confidential web sharing of proprietary material) and additional features (Wojciechowski, Hopkins and Upton, 2015).
')))
    }else{
      return(NULL)
      
    }
    
  })
  
  
tipoAcao<- reactiveValues(data = NULL) 

tipoAcao_surv.time<-reactiveValues(data=NULL)
tipoAcao_surv.status<-reactiveValues(data=NULL)

tipoAcao_idm.time1<-reactiveValues(data=NULL)
tipoAcao_idm.event1<-reactiveValues(data=NULL)
tipoAcao_idm.Stime<-reactiveValues(data=NULL)
tipoAcao_idm.event<-reactiveValues(data=NULL)

tipoAcao_trans<- reactiveValues(data = NULL) 
tipoAcao_name.states<- reactiveValues(data = NULL) 
tipoAcao_num.states<- reactiveValues(data = NULL) 
tipoAcao_variables.times<- reactiveValues(data = NULL) 
tipoAcao_variables.events<- reactiveValues(data = NULL)

                                   
observeEvent(input$files.upload.manage, {
                
    if(input$type.file=='Survival data'){
                      tipoAcao$data <- 'upload.surv'  
                      tipoAcao_surv.time$data<-input$Survival.data.time
                      tipoAcao_surv.status$data<-input$Survival.data.status
    }
    if(input$type.file=='Illness-death model (IDM)'){
                      tipoAcao$data <- 'upload' 
                      tipoAcao_idm.time1$data<-input$Illness.death.model.time1
                      tipoAcao_idm.event1$data<-input$Illness.death.model.event1
                      tipoAcao_idm.Stime$data<-input$Illness.death.model.Stime
                      tipoAcao_idm.event$data<-input$Illness.death.model.event
    }
                    
    if(input$type.file=='Other multi-state model (MSM)'){
      
                      tipoAcao$data <- 'upload.msm'  
                     
                      tipoAcao_trans$data<-unlist(strsplit(input$msm.to, ";"))
                      
                      tipoAcao_num.states$data<-as.numeric(input$num.states)
                      
                      tipoAcao_name.states$data<-unlist(strsplit(input$name.states, ","))
                      
                      tipoAcao_variables.times$data<-unlist(strsplit(input$variables.times, ","))
                
                      tipoAcao_variables.events$data<-unlist(strsplit(input$variables.events, ","))
                      
    }
  

})


output$tipoAcao<-renderPrint({ 
  
  paste(tipoAcao$data,'-',tipoAcao3$data)
  
})

              #observeEvent(input$files.upload.surv, {
              #  tipoAcao$data <- 'upload.surv'
              #})
              
              
tipoAcao2<- reactiveValues(data = NULL) 
  
output$manage.files1<- renderUI({
    
    column(12, h3("Input files"),
           fileInput("file.upload",label = ''),
           radioButtons("type.file",
                        label = h4("Type of data:"),
                        choices = c('Survival data','Illness-death model (IDM)', 'Other multi-state model (MSM)'), selected = 'Survival data',
                        inline = TRUE),
        
    )
    
  })
  

output$variables.choice<- renderUI({
  
  if (is.null(tipoAcao$data) & is.null(input$file.upload)){
    
    return(NULL)
    
  }else{
    
    
    if (!is.null(input$file.upload) & input$type.file=='Survival data'){
      
      inFile<-paste(getwd(),input$file.upload[1],sep='/')
      dataB<-read.csv(inFile, sep=';',dec=',')
      nomes<-names(dataB)
      
      column(12, h4('Select variables:'),
             selectInput("Survival.data.time",label = h5('Event time:'),choices = c(nomes),selected = nomes[1]),
             selectInput("Survival.data.status",label = h5('Status (The status indicator, normally 0=right censored, 1=event):'),
                         choices = c(nomes),selected = nomes[1]),
             h4('Select the variable position you wish to change the class*:'),
             textInput("num.numeric",label = h5('From categorical to numeric:'),width = 250),
             textInput("num.categorical",label = h5('From numeric to categorical:'),width = 250),
             h4('Select the variable position you wish to delete*:'),
             textInput("num.delete",label = h5(''),width = 250),
             h5('*Text without spaces between semicolons or commas.'))
    }else{
      
      
      if (!is.null(input$file.upload) & input$type.file=='Illness-death model (IDM)'){
        
        inFile<-paste(getwd(),input$file.upload[1],sep='/')
        dataB<-read.csv(inFile, sep=';',dec=',')
        nomes<-names(dataB)
        
        column(12, h4('Select variables for time and status:'),
               selectInput("Illness.death.model.time1",label = h5('Time to the intermediate state:'), choices = c(nomes),selected = nomes[1]),
               selectInput("Illness.death.model.event1",label = h5('The status indicator of entering
                                                                   in the intermediate state (0=right censored, 1=event):'),choices = c(nomes),selected = nomes[1]),
               selectInput("Illness.death.model.Stime",label = h5('Time to the ultimate state:'),choices = c(nomes),selected = nomes[1]),
               selectInput("Illness.death.model.event",label = h5('The status indicator of entering
                                                                   in the ultimate state (0=right censored, 1=event):'),choices = c(nomes),selected = nomes[1]),
               h4('Select the variable position you wish to change the class*:'),
               textInput("num.numeric",label = h5('From categorical to numeric:'),width = 250),
               textInput("num.categorical",label = h5('From numeric to categorical:'),width = 250),
               h4('Select the variable position you wish to delete*:'),
               textInput("num.delete",label = h5(''),width = 250),
               h5('*Text without spaces between commas.'))
        
      }else{
        
        column(12,
               textInput("num.states", label = h5('Number of states:'),width = 150),
               
               textInput("msm.to", label = h5('Transitions schema (e.g. for ebmt4 data set: 2,3,5,6;4,5,6;4,5,6;5,6;0)*:'),width = 600),
               
               textInput("name.states", label = h5('Name of states (e.g. for ebmt4 data set: Tx,Rec,AE,Rec+AE,Rel,Death)*:'),width = 600),
               textInput("variables.times", label = h5('Variables for event times (e.g. for ebmt4 data set: rec,ae,recae,rel,srv)*:'),width = 600),
               textInput("variables.events", label = h5('Variables for events status (e.g. for ebmt4 data set: rec.s,ae.s,recae.s,rel.s,srv.s)*:'),width = 600),
               h4('Select the variable position you wish to change the class*:'),
               textInput("num.numeric", label = h5('Numeric variables (e.g. for ebmt4 data set: 2,4,6,8,10):'),width = 600),
               textInput("num.categorical", label = h5('Categorical variables (e.g. for ebmt4 data set: 3,5,7,9,11):'),width = 600),
               h4('Select the variable position you wish to delete*:'),
               textInput("num.delete",label = h5(''),width = 250),
               h5('*Text without spaces between commas.'))
        
      }
      
      
    }
  }
})


output$manage.files2<- renderUI({
  
  column(12,

         actionButton("files.upload.manage", label = "Upload"),
         radioButtons("type.view",
                      label = h4("View structure of data:"),
                      choices = c('Data set','Loaded'), selected = 'Data set',
                      inline = TRUE)
  )
  
})



  output$tipo.variaveis<- renderPrint({  #ou render PRint -- conforme o q quiser apresentar
    
    if (is.null(tipoAcao$data) & is.null(input$file.upload)){
      
      return(NULL)
      
    }else{
      
      
      if (!is.null(tipoAcao$data) & !is.null(input$file.upload) & input$type.view=='Loaded'){
        
        dataB<-loadData.manage()
        str(dataB)
      }
      
      if (!is.null(input$file.upload) & input$type.view=='Data set'){
        
        inFile<-paste(getwd(),input$file.upload[1],sep='/')
        dataB<-read.csv(inFile, sep=';',dec=',')
        str(dataB)
      }
      
    }
      
   })
  
  
  
  output$resp.files.manage<- renderUI({
    
    if (is.null(tipoAcao$data)){
      
      column(12, h3("FILE DESCRIPTION:"),
             
             paste('Data has not inserted'))
      
    }else{
      
      
      if(tipoAcao$data =='upload.surv' | tipoAcao$data =='upload' | tipoAcao$data =='upload.msm'){
        
        #column(12, h3("FILES DESCRIPTION:"),paste('Data sucefully loaded'))
        #column(12, input$file[1])
        h3("Data set currently loaded:")
        
      }
    }
    
  })

  
                 # output$files.surv<- renderUI({
                    
                 #  column(12, h3("Survival data file"),
                 #          fileInput("file.surv",label = ''),
                 #           actionButton("files.upload.surv", label = "Upload")
                 #   )
                    
                 #})
  
                  
                  #output$resp.files.surv<- renderUI({
                    
                  #  if (is.null(tipoAcao$data)){
                      
                  #    column(12, h3("FILE DESCRIPTION:"),
                             
                  #           paste('Data has not inserted'))
                      
                  #  }else{
                      
                      
                  #    if(tipoAcao$data =='upload.surv'){
                        
                        #column(12, h3("FILES DESCRIPTION:"),paste('Data sucefully loaded'))
                        #column(12, input$file[1])
                        
                        
                   #   }
                   #}
                    
                   #})
  

          
  output$view.data_files.manage<- renderDataTable({
                    
                    if (is.null(tipoAcao$data)){
                      
                      return(NULL)
                      
                    }else{
                      
                      if(tipoAcao$data =='upload.surv' | tipoAcao$data =='upload' | tipoAcao$data =='upload.msm'){
                        
                        column(12, paste('Data set currently loaded:'))
                        
                        loadData.manage()
                      }
                      
                      #if(tipoAcao$data =='upload'){
                        
                      #  
                      #  loadData.manage()$dataIDM
                      #}
                      
         
                    }
                  })                
                  
                          
  output$view.transitions<-renderPrint({ 
    
    if (is.null(input$msm.to) & is.null(input$name.states)){
      
      return(NULL)
      
    }else{
      
    
    #paste(unlist(strsplit(input$msm.to, ";")))
    
    #() #usado sem ser com formulario shiny
    
    trans<-unlist(strsplit(input$msm.to, ";"))
    
    nchar(trans)
    
    (impares<-seq(1,25,2))
    
    #rm(vect)
    
    (imp<-impares[impares<=nchar(trans)[1]])
    
    num<-NULL
    
    for (i in 1:length(imp)){
      
      num<-c(num,as.numeric(substr(trans[1],imp[i],imp[i])))
      
    }
    
    (vect<-list(num))
    
    for (j in 2:length(nchar(trans))){
      
      #j<-2
      
      (imp2<-impares[impares<=nchar(trans)[j]])
      
      num2<-NULL
      
      for (z in 1:length(imp2)){
        
        #z<-1
        num2<-c(num2,as.numeric(substr(trans[j],imp2[z],imp2[z])))
  
      }
      
      vect[j]<-list(num2)
      
    }
    
    #paste(vect)
    
    if(j==5){
      
      vect[6]<-list(NULL) 
      
      positions=vect
      
      #namesStates =  c("Tx", "Rec", "AE", "Rec+AE", "Rel", "Death")
      
      namesStates=unlist(strsplit(input$name.states, ","))
      
      tmat <-transMatMSM(positions, names = namesStates)
      
      #tmat<-transMat(x=positions, names = namesStates)
      
      tmat
      
    }else
      return(NULL)
      
    }
    
  })
  
  output$view.variables<-renderPrint({ 
    
    if (is.null(input$variables.times) & is.null(input$variables.events)){
      
      return(NULL)
      
    }else{
      
      #var.times.msm<-c(NA, "rec", "ae","recae", "rel", "srv")
      #var.status.msm<-c(NA, "rec.s", "ae.s", "recae.s", "rel.s", "srv.s")
      
      #var.times.msm<-c(NA, unlist(strsplit(input$variables.times, ",")))
      var.times.msm<-  c(NA,tipoAcao_variables.times$data)
      #var.times.msm==c(NA, "rec", "ae","recae", "rel", "srv")
      #var.times.msm
      #length(var.times.msm)
      #class(var.times.msm)
      var.status.msm<-c(NA, tipoAcao_variables.events$data)  
      var.status.msm
    }
    
  }) 
  
  
                  
                #output$view.data_files.surv<- renderDataTable({
                  
                # if (is.null(tipoAcao$data)){
                    
                #    return(NULL)
                    
                # }else{
                    
                #    if(tipoAcao$data =='upload.surv'){
                      
                    
                #      loadData.surv()
                #    }
                #  }
                #})
                
  
  upData.manage<- reactive({
    
    if(is.null(input$file.upload)){
      
      return(NULL)
    }
    
    else{
      
      if(tipoAcao$data =='upload.surv'){
        
      #data("aml")
      #aml2<-aml
      #inFile<-paste(getwd(),'aml.csv',sep='/')
      inFile<-paste(getwd(),input$file.upload[1],sep='/')
      aml2<-read.csv(inFile, sep=';',dec=',')
      
      nomes<-names(aml2)

      
      if(input$Survival.data.time!='time'){
        
        index<-which(nomes==input$Survival.data.time)
        
        aml2$time<-as.integer(aml2[,index])
      }
      
      
      if(input$Survival.data.status!='status'){
        
        index<-which(nomes==input$Survival.data.status)
        
        aml2$status<-aml2[,index]
      }
      
      
      if(input$num.numeric=="" & input$num.categorical=="" & input$num.delete==""){
        
        return(aml2)
        
      }else{
        
        #num.numeric<-as.numeric(input$num.numeric)
        num.numeric<-as.numeric(unlist(strsplit(input$num.numeric, ",")))
        
        if(input$num.numeric!=""){
          
          for(i in 1:length(num.numeric)){
            
            aml2[,num.numeric[i]]<-as.integer(aml2[,num.numeric[i]])
          }
          
        }
        
        
        #num.categorical<-as.numeric(input$num.categorical)
        num.categorical<-as.numeric(unlist(strsplit(input$num.categorical, ",")))
        
        if(input$num.categorical!=""){
          for(j in 1:length(num.categorical)){
            aml2[,num.categorical[j]]<-as.character(aml2[,num.categorical[j]])
          }
        }
        
        
        num.delete<-as.numeric(unlist(strsplit(input$num.delete, ",")))
        
        if(input$num.delete!=""){
          aml2<-aml2[,-num.delete]
          #for(i in 1:length(num.delete)){
            
            #aml2<-aml2[,-c(num.delete[i])]
            #aml2<-subset(aml2, select = -num.delete[i])
          #}
          
        }
        
        
        return(aml2)
        
      }
      
  
      }
      if(tipoAcao$data =='upload'){
        
        inFile<-paste(getwd(),input$file.upload[1],sep='/')
        dataIDM<-read.csv(inFile, sep=';',dec=',')
        #my_list <-list("dataIDM"=dataIDM)
        #return(my_list)
        
        nomes<-names(dataIDM)
        
        
        if(input$Illness.death.model.time1!='time1'){
          
          index<-which(nomes==input$Illness.death.model.time1)
          
          dataIDM$time1<-as.numeric(dataIDM[,index])
        }
        
        
        if(input$Illness.death.model.event1!='event1'){
          
          index<-which(nomes==input$Illness.death.model.event1)
          
          dataIDM$event1<-as.numeric(dataIDM[,index])
        }
        
        if(input$Illness.death.model.Stime!='Stime'){
          
          index<-which(nomes==input$Illness.death.model.Stime)
          
          dataIDM$Stime<-as.numeric(dataIDM[,index])
        }
        
        
        if(input$Illness.death.model.event!='event'){
          
          index<-which(nomes==input$Illness.death.model.event)
          
          dataIDM$event<-as.numeric(dataIDM[,index])
        }
        
        
        if(input$num.numeric=="" & input$num.categorical=="" & input$num.delete==""){
          
          #PASSEI TUDO PARA NUMERIC E RESOLVEI O PROBLEMA para depois no breslow method
          dataIDM$time1<-as.numeric(dataIDM$time1)
          dataIDM$event1<-as.numeric(dataIDM$event1)
          dataIDM$Stime<-as.numeric(dataIDM$Stime)
          dataIDM$event<-as.numeric(dataIDM$event)
          dataIDM$rx<-as.factor(dataIDM$rx)
          dataIDM$sex<-as.numeric(dataIDM$sex)
          dataIDM$age<-as.numeric(dataIDM$age)
          dataIDM$obstruct<-as.numeric(dataIDM$obstruct)
          dataIDM$perfor<-as.numeric(dataIDM$perfor)
          dataIDM$adhere<-as.numeric(dataIDM$adhere)
          dataIDM$nodes<-as.numeric(dataIDM$nodes)
          #data("colonIDM")
          #dat1<-colonIDM
          
          return(dataIDM)
          
        }else{
          
          #num.numeric<-as.numeric(input$num.numeric)
          num.numeric<-as.numeric(unlist(strsplit(input$num.numeric, ",")))
          
          #num.categorical<-as.numeric(input$num.categorical)
          num.categorical<-as.numeric(unlist(strsplit(input$num.categorical, ",")))
          
          if(input$num.numeric!=""){
            
            for(i in 1:length(num.numeric)){
            
            dataIDM[,num.numeric[i]]<-as.integer(dataIDM[,num.numeric[i]])
          }
          
          }
          
          if(input$num.categorical!=""){
          for(j in 1:length(num.categorical)){
            dataIDM[,num.categorical[j]]<-as.character(dataIDM[,num.categorical[j]])
          }
          }
          
          num.delete<-as.numeric(unlist(strsplit(input$num.delete, ",")))
          
          if(input$num.delete!=""){
            dataIDM<-dataIDM[,-num.delete]
                   }
          
          return(dataIDM)
          
        }
        
        
        
      }
      
      if(tipoAcao$data =='upload.msm'){
        #data("aml")
        #aml2<-aml
        #inFile<-paste(getwd(),'aml.csv',sep='/')
        inFile<-paste(getwd(),input$file.upload[1],sep='/')
        dat.msm<-read.csv(inFile, sep=';',dec=',')
        
        #dat.msm <- as.data.frame(apply(dat.msm1, 2, as.numeric))
        #return(dat.msm)
        
    
        if(input$num.numeric=="" & input$num.categorical=="" & input$num.delete==""){
          
          return(dat.msm)
          
        }else{
          
          #num.numeric<-as.numeric(input$num.numeric)
          num.numeric<-as.numeric(unlist(strsplit(input$num.numeric, ",")))
          
          #num.categorical<-as.numeric(input$num.categorical)
          num.categorical<-as.numeric(unlist(strsplit(input$num.categorical, ",")))
          
          if(input$num.numeric!=""){
            
            for(i in 1:length(num.numeric)){
              
              dat.msm[,num.numeric[i]]<-as.integer(dat.msm[,num.numeric[i]])
            }
            
          }
          
          if(input$num.categorical!=""){
            for(j in 1:length(num.categorical)){
              dat.msm[,num.categorical[j]]<-as.character(dat.msm[,num.categorical[j]])
            }
          }
          
          num.delete<-as.numeric(unlist(strsplit(input$num.delete, ",")))
          
          if(input$num.delete!=""){
            dat.msm<-dat.msm[,-num.delete]
          }
          
          return(dat.msm)
          
        }
        
      }
    }
    
  })
  
  
  observeEvent(input$files.upload.manage,{
    
    if(is.null(input$file.upload)){
      
      return(NULL)
      
    }else{
      
      saveData.manage(upData.manage())
    }
  })
  
  
      #observeEvent(input$files.upload.surv,{
        
      #  if(is.null(input$file.surv)){
          
      #    return(NULL)
          
      #  }else{
          
      #    saveData.surv(upData.surv())
      #  }
      #})
      
  
  saveData.manage <- function(data) {
    
    
    responses.manage<<-data
    
  }
      #saveData.surv <- function(data) {
        
        
      #  responses.surv<<-data
        
      #}
  
  
  loadData.manage <- function() {
    
    if (exists("responses.manage")) {
      
      responses.manage
      
    }
  }
  
  
  
  #loadData.surv <- function() {
    
  #  if (exists("responses.surv")) {
      
  #    responses.surv
      
  #  }
  #}
  
  
  #output$surv.confirm<- renderPrint({  #ou render PRint -- conforme o q quiser apresentar
  #  setwd("C:/Users/asus/Desktop/PEN/_LA_5/35_app.MSM/appMSM")
    
  #  inFile<-paste(getwd(),input$file.surv[1],sep='/')
  #  inFile
    #aml2<-read.csv(inFile, sep=';',dec='.')
    #aml2
    
  #})
  
  
  output$KM<- renderUI({
  
    #data("aml")
    
    #str(aml)
    
    #db<-loadData.surv()
    
    db<-loadData.manage()

    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    index<-which(nomes %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
    column(12,h3("Kaplan-Meier estimator"),
                  radioButtons("conf.radio.KM",
                               label = h4("Interval of confidence:"),
                               choices = c('No','Yes'), selected = 'No',
                               inline = TRUE),
           selectInput("KM.variable",label = h5('Select variable'),choices = c(nomes,'None'),selected = 'None'))
  
  
  })
    
    
  output$summary.KM<- renderPrint({ 
    
    #db<-loadData.surv()
    db<-loadData.manage()
    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    index<-which(nomes  %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
    if(input$KM.variable=='None'){
    
      fit<-survfit(Surv(time, status)~1, data=db)
    
    summary(fit)
    
    }else{
      
      #formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~ ",input$single.variable))
      
      #fit2<-survfit(Surv(time, status)~x, data=db)
      
      formula.km<-as.formula(paste("Surv(time, status)~",input$KM.variable))
      
      fit2<-survfit(formula.km, data=db)
      
      summary(fit2)
      
    }
  })
  
  
  output$KM.plot<- renderPlot({
    
    db<-loadData.manage()
    
   
    
    if(input$conf.radio.KM=='No')
      
      conf.value<-FALSE
    else
      conf.value<-TRUE

    if(input$KM.variable=='None'){
      
      fit<-survfit(Surv(time, status)~1, data=db)
      
      plot(fit, xlab='Times', ylab='Survival', mark.time = T, conf.int = conf.value)
      
    }else{
      
      nomes<-names(db)
      
      index2<-which(nomes  %in% input$KM.variable)
      
      num<-length(unique(db[,index2]))
      
      #fit2<-survfit(Surv(time, status)~x, data=db)
      
      formula.km.plot<-as.formula(paste("Surv(time, status)~",input$KM.variable))
      
      fit2<-survfit(formula.km.plot, data=db)
      
      #plot(fit2)
      
      plot(fit2,lty = 1:2,xlab='Times', ylab='Surival', col=1:num, conf.int = conf.value)
      
    }
  })         
  
  
  output$LR<- renderUI({
    
    #db<-loadData.surv()
    db<-loadData.manage()
    #nomes<-names(db)[3]
    
    nomes<-names(db)
    
    index<-which(nomes %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    column(12,h3("Compare survival curves"),
           radioButtons("conf.radio.LR",
                        label = h4("Type of method:"),
                        choices = c('Log-rank','Gehan-wilcoxon'), selected = 'Log-rank',
                        inline = TRUE),
           selectInput("LR.variable",label = h5('Select variable'), choices = nomes))
  })
  
  output$summary.LR<- renderPrint({ 
    db<-loadData.manage()
    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    #index<-which(nomes==c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
   
    if(input$conf.radio.LR=='Log-rank'){
      
      formula.LR<-as.formula(paste("Surv(time, status)~",input$LR.variable))
      survdiff(formula.LR, data=db,rho = 0)
      
    }else{
      
      formula.LR<-as.formula(paste("Surv(time, status)~",input$LR.variable))
      survdiff(formula.LR, data=db,rho = 1) 
    }
  })
  
  output$cox<- renderUI({
    
    db<-loadData.manage()
    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    index<-which(nomes %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
    column(12,h3("Cox PH model"),
               checkboxGroupInput("cox.variables", "Select variables",
                              nomes))
    
    
  })
  
  output$summary.cox<- renderPrint({ 
    db<-loadData.manage()
    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    index<-which(nomes %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
    if(is.null(input$cox.variables)){
      
      
      fit3b<-coxph(Surv(time,status)~1, data=db)
      
      fit3b
      
      
    }else{
      
      formula.cox<-as.formula(paste("Surv(time,status)~",
                                paste(input$cox.variables, collapse = "+")))
      
      
      #fit3<-coxph(Surv(time,status)~x, data=aml)
      
      fit3<-coxph(formula.cox, data=db)
      fit3
      
      
    }
  })
  
  
  output$parametric<- renderUI({
    
    db<-loadData.manage()
    
    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    index<-which(nomes %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
    column(12,h3("Parametric models"),
           radioButtons("type.radio.parametric",
                        label = h4("Type of distributions:"),
                        choices = c('exponential','weibull','gaussian','logistic',
                                    'lognormal','loglogistic'), selected = 'exponential',
                        inline = TRUE),
           checkboxGroupInput("parametric.variables", "Select variables",
                              nomes))
    
    
  })
  
  
  output$summary.parametric<- renderPrint({ 
    db<-loadData.manage()
    #nomes<-names(db)[3]
    
    #nomes_f<-c(nomes, 'time')
    nomes<-names(db)
    
    index<-which(nomes %in% c('time','status',tipoAcao_surv.time$data,tipoAcao_surv.status$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]

    
    if(is.null(input$parametric.variables)){
      
      
      fit4<- survreg(Surv(time, status) ~ 1, data=db, dist = input$type.radio.parametric)
      
      fit4
      
    }else{
      
      formula.parametric<-as.formula(paste("Surv(time,status)~",
                                    paste(input$parametric.variables, collapse = "+")))
      
      
      fit4<- survreg(formula.parametric, data=db, dist = input$type.radio.parametric)
      
      summary(fit4)
      
    }
  })
  
  ######################################################################################################
  #IDM:
  
  #observeEvent(input$files.upload, {
  #  tipoAcao$data <- 'upload'
  #})
  

  #IDM - Data File

  #output$files<- renderUI({
    #setwd("C:/Users/asus/Desktop/PEN/_LA_5/35_app.MSM/appMSM")
  #  column(12,h3("Illness-death model data file"),
  #         fileInput("file",label = ''),
  #         actionButton("files.upload", label = "Upload")
  #  )
  #})
  
  
  #output$resp.files<- renderUI({
    
  #  if (is.null(tipoAcao$data)){
      
  #    column(12, h3("FILE DESCRIPTION:"),
             
  #           paste('Data has not inserted'))
      
  #  }else{
      
      
  #    if(tipoAcao$data =='upload'){
        
        #column(12, h3("FILES DESCRIPTION:"),paste('Data sucefully loaded'))
        #column(12, input$file[1])
        
   #   }
   # }
  #})
  

  
  #upData<- reactive({
    
  #  if(is.null(input$file)){
      
  #    return(NULL)
  #  }
    
  #  else{

      
  #    inFile<-paste(getwd(),input$file[1],sep='/')
  #    dataIDM<-read.csv(inFile, sep=';',dec='.')
  #    my_list <-list("dataIDM"=dataIDM)
  #    return(my_list)

  #   }
  
  #})
  

  # When the Submit button is clicked, save the form data
  
  #observeEvent(input$files.upload,{
    
  #  if(is.null(input$file)){
      
  #    return(NULL)
      
  #  }else{
      
  #    saveData(upData())
  #  }
  #})
  
  #observeEvent(input$upload.surv,{
    
  #  if(is.null(input$file.surv)){
      
  #    return(NULL)
      
  #  }else{
      
  #    saveData(upData())
  #  }
  #})
  
  
  #saveData <- function(data) {
    

  #  responses<<-data
    
  #}
  
  #loadData <- function() {
    
  #  if (exists("responses")) {
     
  #     responses
      
  #  }
  #}
  
  

  #output$view.data_files<- renderDataTable({
    
  #  if (is.null(tipoAcao$data)){
      
  #    return(NULL)
      
  #  }else{
      
  #    if(tipoAcao$data =='upload'){
        
  #      loadData()$dataIDM
        
  #    }
  #  }
  #})
  
  
  output$type_of_event<- renderUI({
    
    column(12,
           
           radioButtons("type.event.radio",
                        label = h3("Type of event:"),
                        choices = c('Count','Proportion'), selected = 'Count',
                        inline = TRUE))
  })
  output$type_of_event2<- renderUI({
    
    column(12,
           
           radioButtons("type.event.radio",
                        label = h3("Type of event:"),
                        choices = c('Count','Proportion'), selected = 'Count',
                        inline = TRUE)
    )
  })
  
  
  output$summary.type.event<- renderTable({
    
    if (is.null(tipoAcao$data)){
      
      return(NULL)
    }
    
    else{ 
      
      if(input$type.event.radio=='Count'){
       
        #dat<-loadData()$dataIDM
        dat<-loadData.manage()
        #dat$Stime<-as.integer(dat$Stime) #COLOCANDO EM INTEGER DEIXA DE FAZER SENTIDO ESTA LINHA DE CODIGO
        dat[,c('time1','event1','Stime','event')]
        }
     
      else{
        loadData.manage[,c('time1','event1','Stime','event')] 
        #loadData()$dataIDM[,1:4] 
      }
}
      
  })
  
  output$summary.type<- renderPrint({ ##se tivesse posto renderText apenas dava os valores (no verbatimTextOutput) mas na consola 
    #aparecia a tabela
    
 
    
    if (is.null(tipoAcao$data)){
      
      return(NULL)
    }
    
    else{ 
      
      if(input$type.event.radio=='Count'){
        
        dat<-loadData.manage()
        
        dat<-dat[!is.na(dat$Stime),]
        nevents(dat)
        
        #nevents(with(dat[,c('time1','event1','Stime','event')], survIDM(time1, event1, Stime, event)), 
        #        state.names = c("healthy", "recurrence", "death"))
        
        }
      
      else{

        dat<-loadData.manage()
        
        round(nevents(dat)/nrow(dat),3)
        
        #round(nevents(with(dat[,c('time1','event1','Stime','event')], survIDM(time1, event1, Stime, event)), 
        #             state.names = c("healthy", "recurrence", "death"))/nrow(dat),3)
        
      }
    }
    
  })
  
  
  #regression models:
  
  output$regression<- renderUI({
    
    #dat<-loadData()$dataIDM
    dat<-loadData.manage()
    #nomes<-names(dat)[-c(1:5)]
    
    nomes<-names(dat)
    
    index<-which(nomes %in% c('time1','event1','Stime','event', tipoAcao_idm.time1$data,
                                 tipoAcao_idm.event1$data, tipoAcao_idm.Stime$data,tipoAcao_idm.event$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
  
    column(12,
           
           h4("Regression models"),
                        radioButtons("type.model",
                        label = h3("Type of model"),
                        choices = c('Markovian','semi-Markovian'), selected = 'Markovian',
                        inline = TRUE),
                        radioButtons("type.regression",
                        label = h3("Type of Regression"),
                        choices = c('Cox models','ANOVA','PH'), selected = 'Cox models',
                        inline = TRUE),
                        checkboxGroupInput("variables.include", "Variables to include:",
                                   nomes,selected = nomes[1:length(nomes)]))

  })


  output$regression.summary<- renderPrint({ 
    
    if (is.null(tipoAcao$data)){
      
      return(NULL)
    }
    
    else{ 
   
      dat<-loadData.manage()
      

      if(input$type.model=='Markovian'){
        

         formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",
                                   paste(input$variables.include, collapse = "+"))) 
       
         fit.cmm <-coxidm(formula, data = dat)

  
       if(input$type.regression=='Cox models')
       summary(fit.cmm)
       if(input$type.regression=='ANOVA')
         summary(fit.cmm,type='anova')
       if (input$type.regression=='PH')
         summary(fit.cmm,type='ph')
      }else{
        
      
        formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",
                                  paste(input$variables.include, collapse = "+")))
        
        fit.cmm <-coxidm(formula, data = dat, semiMarkov = T)
        
        
        if(input$type.regression=='Cox models')
          summary(fit.cmm)
        if(input$type.regression=='ANOVA')
          summary(fit.cmm,type='anova')
        if (input$type.regression=='PH')
          summary(fit.cmm, type='ph')
        
        }
      
      
  
    }
  })
  
  #TP
  
  output$tp<- renderUI({
    
    #dat<-loadData()$dataIDM
    dat<-loadData.manage()
    #nomes<-names(dat)[-c(1:5)]
    
    nomes<-names(dat)
    
    index<-which(nomes %in% c('time1','event1','Stime','event', tipoAcao_idm.time1$data,
                              tipoAcao_idm.event1$data, tipoAcao_idm.Stime$data,tipoAcao_idm.event$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    
    column(12,
           h3("Transition probabilities"),
           textInput("s",label = 's'),
           textInput("times",label = 'Times (e.g. 400,600):'),
           radioButtons("conf.radio",
                        label = h3("Interval of confidence:"),
                        choices = c('No','Yes'), selected = 'No',
                        inline = TRUE),
           h3("Type of methods"), selectInput("type.method.nonparametric",
                              
                              label = h4("Nonparametric:"),
                              
                              choices = c("AJ", "LM", "PLM", "LMAJ","None"), selected = 'None'),
           
           selectInput("type.single.variable",
                       
                       label = h4("One single covariate:"),
                       
                       choices = c("AJ", "LM", "PLM", "LMAJ","IPCW","None"), selected = 'None'),
           selectInput("single.variable",label = h5('Select variable'),choices = c(nomes, "None"), selected = 'None'),
           selectInput("trans.number",label = h5('Transition for plots (for AJ, LM, PLM or LMAJ methods:'),choices = c("1->2","1->3","2->3"), selected = "1->2"),
           textInput("value.single",label = h5('Value (for IPCW method):'), value = NULL),
           h4("Breslow method:"),
           checkboxGroupInput("variables.breslow", "Variables to include in the model:",
                              nomes,inline = TRUE),
           textInput("values.breslow",label = 'Value(s)'))

  })
  
  
  output$tp.summary<- renderPrint({ 
    
    if (is.null(tipoAcao$data)|is.null(input$s)|is.null(input$times)){
      
      return()
    }
    
    else{ 

      dat<-loadData.manage()

      
      times_split<-as.numeric(unlist(strsplit(input$times, ",")))
      
      if(input$conf.radio=='No')
        conf.value<-'FALSE'
      else
        conf.value<-'TRUE'
      
      if(input$type.method.nonparametric %in% c("AJ",  "LM", "PLM", "LMAJ") & input$type.single.variable=='None' & input$values.breslow==''){
   
       res <- tprob(survIDM(time1, event1, Stime, event) ~ 1, s = as.numeric(input$s), method = input$type.method.nonparametric, 
                   conf = conf.value, data = dat)
       
       summary(res, time=times_split)
      }
      

        

      if(input$type.single.variable %in% c("AJ", "LM", "PLM", "LMAJ") & input$type.method.nonparametric=='None' &  input$values.breslow==''){
          
          
          if (input$single.variable=='None'){
            
            return(NULL)
            
          }else{
            
          formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~ factor(",input$single.variable,")")) #desnecessario 
          
          #formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~ ",input$single.variable))
          
          tp.single <- tprob(formula, s= as.numeric(input$s),
                             method = input$type.single.variable, conf = conf.value, data =dat)
          
          
          summary(tp.single, time=times_split)
          
          }
        }
      
      
      
      if(input$type.single.variable %in% c("IPCW") & input$type.method.nonparametric=='None' &  input$values.breslow==''){
        
        
        if (input$value.single=='None'){
          
          return()
          
        }else{
          
          formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",input$single.variable))
          tp.single <- tprob(formula, s= as.numeric(input$s),
                             method = "IPCW", z.value = input$value.single, conf = conf.value, data =dat)
          
          summary(tp.single, time=times_split)
          
        }
      }
      

      if(input$type.method.nonparametric=='None' &  input$type.single.variable=='None' & input$values.breslow!=''){
        
        values.breslow<-unlist(strsplit(input$values.breslow, ","))
        
        if(length(values.breslow)>1 & length(values.breslow)==length(input$variables.breslow)){
          
          
          formula.breslow<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",
                                            paste(input$variables.breslow,collapse = "+")))
          
          #formula.breslow<-as.formula(paste("survIDM(time1, event1, Stime, event) ~age+obstruct"))
          
          tp.breslow <- tprob(formula.breslow, s = as.numeric(input$s), method = "breslow", 
                              z.value =values.breslow, data = dat) #colonIDM ja da
          
          summary(tp.breslow, time=times_split)
          
          
        }else{
          
          #formula.breslow<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",input$variables.breslow))
          
          if(is.character(input$variables.breslow)){
            
            formula.breslow<-as.formula(paste("survIDM(time1, event1, Stime, event) ~factor(",input$variables.breslow,")"))
            
            tp.breslow <- tprob(formula.breslow, s = as.numeric(input$s), method = "breslow", 
                                z.value =values.breslow, conf = conf.value, data = dat)
            
            summary(tp.breslow, time=times_split)
            
          }else{
            
            formula.breslow<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",input$variables.breslow)) 
            
            tp.breslow <- tprob(formula.breslow, s = as.numeric(input$s), method = "breslow", 
                                z.value =as.numeric(values.breslow), conf = conf.value, data = dat)
            
            summary(tp.breslow, time=times_split)
            
          }
          
        }
      }
     
    }
  })
  
  
  output$tp.plot<- renderPlot({
    
    if (is.null(tipoAcao$data)|is.null(input$s)|is.null(input$times)){
      
      return()
      
    }else{
      

      dat<-loadData.manage()
  
      times_split<-as.numeric(unlist(strsplit(input$times, ",")))
      
      if(input$conf.radio=='No'){
        conf.value<-'FALSE'
      }else{
        conf.value<-'TRUE'
      }
       
      
      
      if(input$type.method.nonparametric %in% c("AJ", "LM", "PLM", "LMAJ") & input$type.single.variable=='None'  & 
         input$values.breslow==''){
        
        res1 <- tprob(survIDM(time1, event1, Stime, event) ~ 1, s = as.numeric(input$s), method = input$type.method.nonparametric, 
                     conf = conf.value, data = dat)
        autoplot(res1)
        
      }else{
        
        if(input$type.method.nonparametric=='None'& input$type.single.variable %in% c("AJ", "LM", "PLM", "LMAJ") &
           input$values.breslow==''){
          
          formula<-as.formula(paste("survIDM(time1, event1, Stime, event) ~factor(",input$single.variable,")")) 
          
          res2<- tprob(formula, s = as.numeric(input$s),
                      method = input$type.single.variable, conf =conf.value, data = dat) 
          
          res2
         
          if(input$trans.number=="1->2"){
            
            autoplot(res2, trans="01", ylim=c(0,0.5)) 
            
          }else{
            if(input$trans.number=="1->3"){
              autoplot(res2, trans="02", ylim=c(0,0.5))
              
            }else{
              
              autoplot(res2, trans="12", ylim=c(0,0.5))
              
            }
            
          }
          
       
         
        }else{
          
          if(input$type.single.variable %in% c("IPCW") &  input$single.variable!='None'  & input$values.breslow==''){
            
          
          #formula2<-as.formula(paste("survIDM(time1, event1, Stime, event) ~factor(",input$type.single.variable,")")) 
          formula2<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",input$single.variable))  
          
          res3<- tprob(formula2 , s = as.numeric(input$s), method = input$type.single.variable, 
                      conf = conf.value, z.value = input$value.single, data = dat)
          
          autoplot(res3)
          
          }else{
          
            return(NULL)
        }
      }
      }
    }
})         
  
  
  
  output$tp.confirm<- renderPrint({  #ou render PRint -- conforme o q quiser apresentar
  

  dat<-loadData.manage()

  if(input$values.breslow!=''){

    #values.breslow2<-unlist(strsplit(input$values.breslow, ","))
    
    #values.breslow2<-as.vector(values.breslow2)
    #values.breslow2
    
    
    formula.breslow<-as.formula(paste("survIDM(time1, event1, Stime, event) ~",
                                      paste(input$variables.breslow, collapse = "+")))
    
    formula.breslow

}
  })
  
  
  #cif
  
  
  output$cif<- renderUI({
  
    #dat<-loadData()$dataIDM
    dat<-loadData.manage()
    #nomes<-names(dat)[-c(1:5)]
    
    nomes<-names(dat)
    
    index<-which(nomes %in% c('time1','event1','Stime','event', tipoAcao_idm.time1$data,
                              tipoAcao_idm.event1$data, tipoAcao_idm.Stime$data,tipoAcao_idm.event$data))
    #index<-which(nomes %in% c('time','status','time1','status1'))
    nomes<-nomes[-index]
    column(12,
           h3("Cumulative Incidence Function"),
           textInput("times.cif",label = 'Times'),
           radioButtons("conf.radio.cif",
                        label = h3("Interval of confidence:"),
                        choices = c('No','Yes'), selected = 'No',
                        inline = TRUE),
           selectInput("cif.variable",label = h5('Select variable'),choices = c(nomes,'None'), selected = 'None'),
           textInput("cif.value.variable",label = 'Value', value = NULL))
    
  })
  
  
  output$cif.summary<- renderPrint({ 
    
   
    if (input$times.cif==''){
      
      return()
    }
    
    else{ 
      
      times_split<-as.numeric(unlist(strsplit(input$times.cif, ",")))
      
      dat<-loadData.manage()
      
      if(input$conf.radio.cif=='No'){
        conf.value<-'FALSE'
      }else{
        conf.value<-'TRUE'
        
        
      }
        
    
      if(input$cif.variable=='None'){
        
        
        
      cif <- CIF(survIDM(time1, event1, Stime, event) ~ 1, data = dat, conf = conf.value)
      
      summary(cif, time=times_split)
      
      }else{
        
        formula.cif<-as.formula(paste("survIDM(time1, event1, Stime, event) ~ ",input$cif.variable))
        
        cif.cov <- CIF(formula.cif, data = dat,
                       conf = conf.value, z.value = input$cif.value.variable)
        
        summary(cif.cov, time=times_split)
        
      }
    }
})
  
  #sojourn
  
  
  output$sojourn<- renderUI({
    
    #dat<-loadData()$dataIDM
    dat<-loadData.manage()
    
    nomes<-names(dat)[-c(1:5)]
    
    column(12,
           textInput("times.soj",label = 'Times'),
           h3("Sojourn"),
           radioButtons("conf.radio.soj",
                        label = h3("Interval of confidence:"),
                        choices = c('No','Yes'), selected = 'No',
                        inline = TRUE),
           selectInput("soj.type",label = h5('Method'), choices = c("LM", "Satten-Datta"), selected = 'LM'),
           selectInput("soj.variable", label = h5('Select variable'), choices = c(nomes,'None')),
           textInput("soj.value.variable", label = 'Value', value = NULL))
    
  })
  
  
  output$sojourn.summary<- renderPrint({ 
    
    times_split<-as.numeric(unlist(strsplit(input$times.soj, ",")))

    if (is.null(tipoAcao$data)){
      
      return(NULL)
    }
    
    else{ 
      
      if(input$conf.radio.soj=='No')
        conf.value<-'FALSE'
      else
        conf.value<-'TRUE'
      
      #dat<-loadData()$dataIDM
      dat<-loadData.manage()
      
      dat$Stime<-as.numeric(as.character(dat$Stime)) #continua com os mesmos 7 NA#COLOCANDO EM INTEGER DEIXA DE FAZER SENTIDO ESTA LINHA DE CODIGO
   
      dat<-dat[!is.na(dat$Stime),]
 
      
      if(input$soj.variable=='None'){
        
        soj <- sojourn(survIDM(time1, event1, Stime, event) ~ 1,
                       data = dat, method =input$soj.type, conf = conf.value)
        
        
        summary(soj, time=times_split)
        
        
      }else{
        
        formula.soj<-as.formula(paste("survIDM(time1, event1, Stime, event) ~ ",input$soj.variable))
        
        res4 <- sojourn(formula.soj, data = dat,z.value = input$soj.value.variable, conf = conf.value, conf.level = 0.95)
 
        summary(res4, time=times_split)
        
      }
      
  }
    
  })
  
  
  #output$sojourn<- renderText({
  #  cat(tipoAcao$data)
  #})

  
  ######################################################################################################
  #MORE COMPLEX - MSM
  
  #observeEvent(input$files.upload.msm, {
  #  tipoAcao$data <- 'upload.msm'
  #})
  
  
  #output$files.msm<- renderUI({
    
  #  column(12,h3("Multi-state model data file"),
  #         fileInput("file.msm",label = ''),
  #         actionButton("files.upload.msm", label = "Upload")
  #  )
    
  #})
  
  
  #output$resp.files.msm<- renderUI({
    
  #  if (is.null(tipoAcao$data)){
      
  #    column(12, h3("FILE DESCRIPTION:"),
  #           
  #           paste('Data has not inserted'))
      
  # }else{
      
      
   #   if(tipoAcao$data =='upload.msm'){
        
        #column(12, h3("FILES DESCRIPTION:"),paste('Data sucefully loaded'))
        #column(12, input$file.msm[1])
        #column(12, paste(getwd(),input$file.msm[1],sep='/'))
        
  #   }
  #  }
    
  #})
  
  
  #output$view.data_files.msm<- renderDataTable({
    
  #  if (is.null(tipoAcao$data)){
      
  #    return(NULL)
      
  #  }else{
      
  #    if(tipoAcao$data =='upload.msm'){
        
  #      loadData.msm()
  #    }
  #  }
  #})
  
  
  #upData.msm<- reactive({
    
  #  if(is.null(input$file.msm)){
      
  #    return(NULL)
  #  }
    
  #  else{
      
      
      #data("aml")
      #aml2<-aml
      #inFile<-paste(getwd(),'aml.csv',sep='/')
  #    inFile<-paste(getwd(),input$file.msm[1],sep='/')
  #    dat.msm<-read.csv(inFile, sep=';',dec='.')
      
      #dat.msm <- as.data.frame(apply(dat.msm1, 2, as.numeric))
  #    return(dat.msm)
  #  }
    
  #})
  
  
  #observeEvent(input$files.upload.msm,{
    
  #  if(is.null(input$file.msm)){
      
  #    return(NULL)
      
  #  }else{
      
  #    saveData.msm(upData.msm())
  #  }
  #})
  
  
  
  #saveData.msm <- function(data) {
    
    
  #  responses.msm<<-data
    
  #}
  
  #loadData.msm <- function() {
    
  #  if (exists("responses.msm")) {
      
  #    responses.msm
      
  #  }
  #}
  
  
  output$tp.msm<- renderUI({
    
    #datMSM<-loadData.msm()
    
    #num_states<-tipoAcao_num.states$data-1
    
    column(12,
           h3("Transition probabilities - MSM"),
           textInput("sMSM",label = 's'),
           textInput("timesMSM_ini",label = 'Inicial time'),
           textInput("timesMSM_last",label = 'Last time'),
           textInput("timesMSM_by",label = 'By'),
           radioButtons("conf.radioMSM",
                        label = h3("Interval of confidence:"),
                        choices = c('No','Yes'), selected = 'No',
                        inline = TRUE),
           h3("Type of methods"), selectInput("type.method.MSM",
                                              
                                              label = h4("Type of methods:"),
                                              
                                              choices = c("AJ", "LMAJ"), selected = 'AJ'),
           selectInput("fromMSM",
                       
                       label = h4("From:"),
                       
                       choices = 1:(tipoAcao_num.states$data-1), selected = '1')) #c("1", "2", "3","4","5")
    
  })
  
  
  
  output$tp.summary.msm<- renderPrint({ 
    
    if (is.null(tipoAcao$data)|is.null(input$sMSM)|is.null(input$timesMSM_ini)){
      
      return(NULL)
    }
    
    else{ 
      
      if(input$conf.radioMSM=='No')
        conf.value<-'FALSE'
      else
        conf.value<-'TRUE'
      
      
      #datMSM<-loadData.msm()
      #ebmt<-loadData.manage()
      data("ebmt4")
      ebmt <- ebmt4
      #class(ebmt)
      
      
      #for (i in c(2,4,6,8,10)){ #c(1,3,5,7,9,11)1:11
      
      #   ebmt[,i]<-as.character(ebmt[,i])
      # }
      
      
      #str(ebmt) 
      
      #for (i in c(2,4,6,8,10)){ #c(1,3,5,7,9,11)1:11
      
      #  datMSM[,i]<-as.integer(as.character(datMSM[,i]))
      #}
      
      #for (i in 12:15){ #
      
      #  datMSM[,i]<-as.factor(datMSM[,i])
      #}
      #str(ebmt)
      
      #-----------------------------------------------------------------
      ##obtencao da matriz de transicao:
      
      
      #trans<-unlist(strsplit(input$msm.to, ";")) #tenho ir buscar ao que ficou guardado atras ao submeter
      
      trans<-tipoAcao_trans$data
      
      nchar(trans)
      
      (impares<-seq(1,25,2))
      
      #rm(vect)
      
      (imp<-impares[impares<=nchar(trans)[1]])
      
      num<-NULL
      
      for (i in 1:length(imp)){
        
        num<-c(num,as.numeric(substr(trans[1],imp[i],imp[i])))
        
      }
      
      (vect<-list(num))
      
      for (j in 2:length(nchar(trans))){
        
        #j<-2
        
        (imp2<-impares[impares<=nchar(trans)[j]])
        
        num2<-NULL
        
        for (z in 1:length(imp2)){
          
          #z<-1
          num2<-c(num2,as.numeric(substr(trans[j],imp2[z],imp2[z])))
          
        }
        
        vect[j]<-list(num2)
        
      }
      
      #paste(vect)
      
      if(j==5){
        
        vect[6]<-list(NULL) 
        
        positions=vect
        
        #namesStates =  c("Tx", "Rec", "AE", "Rec+AE", "Rel", "Death")
        
        namesStates=tipoAcao_name.states$data
        
        tmat <-transMatMSM(positions, names = namesStates)
        
        #tmat<-transMat(x=positions, names = namesStates)
        
        #tmat
        
      }else{
        
        return(NULL)
        
      }
        
      
      #-----------------------------------------------------------------
      
      
      #print(datMSM[datMSM[,2]!=as.integer(as.character(datMSM[,2])),])
      
      #for(i in 1:15)
      #  print(table(ebmt[,i]==datMSM[,i]))
      
      #var.times.msm<-c(NA, "rec", "ae","recae", "rel", "srv")
      #var.status.msm<-c(NA, "rec.s", "ae.s", "recae.s", "rel.s", "srv.s")
      
      #tipoAcao_variables.times$data
      #var.times.msm<-c(NA, )
      #var.status.msm<-c(NA, )
        
      msebmt <- msprep(data =  ebmt, trans = tmat, time =  c(NA,tipoAcao_variables.times$data), 
                       status = c(NA, tipoAcao_variables.events$data), 
                       keep = c("match", "proph", "year", "agecl"))
      
      
      if(input$type.method.MSM=="AJ"){
        
        c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data = msebmt)   
        
        msf0 <- msfit(object = c0, vartype = "greenwood", trans = tmat)
        
        # Aalen-Johansen
        
        pt0 <- probtrans(msf0, predt = as.numeric(input$sMSM), method = "greenwood")
        
        #summary(pt0, from = as.numeric(input$fromMSM),times=seq(as.numeric(input$timesMSM_ini), 
         #                                                       as.numeric(input$timesMSM_last), 
          #                                                      as.numeric(input$timesMSM_by))) #p1, p2, p3, p4, p5, p6
       
        #est<-pt0[1][[1]]
       # est[est$time %in% seq(102,110,1),]
    
        
        if(conf.value==FALSE){
          
        est<-pt0[as.numeric(input$fromMSM)][[1]]
        est[est$time %in% seq(as.numeric(input$timesMSM_ini), 
                                   as.numeric(input$timesMSM_last),as.numeric(input$timesMSM_by)),1:7]
        }else{
          summary(pt0, from = as.numeric(input$fromMSM),times=seq(as.numeric(input$timesMSM_ini),  as.numeric(input$timesMSM_last), 
                                                                 as.numeric(input$timesMSM_by)))

          
        }
       
              
      }else{#lmaj:
        
        if(conf.value==FALSE){
          LMpt0 <- LMAJ(msebmt, s=100, from=as.numeric(input$fromMSM))
          
          LMpt0[LMpt0$time %in% seq(as.numeric(input$timesMSM_ini), 
                                  as.numeric(input$timesMSM_last),as.numeric(input$timesMSM_by)),1:7]
      
        }else{
          LMpt0 <- LMAJ(msebmt, s=100, from=as.numeric(input$fromMSM))
          
          LMpt0[LMpt0$time %in% seq(as.numeric(input$timesMSM_ini), 
                                    as.numeric(input$timesMSM_last),as.numeric(input$timesMSM_by)),]



      }
      
}
      
    }
  
  })
  
  
  ######################################################################################################
  #MARKOV TESTS
  
  output$localTests1<- renderUI({
    dat<-loadData.manage()
    #dat<-loadData()$dataIDM
    
    column(12,
           h3("Local tests"),
           radioButtons("typeData.radio",
                        label = h3("Type of data:"),
                        choices = c("IDM","Other MSM"), selected = 'IDM',
                        inline = TRUE),
           textInput("timesLT",label = 'Times'),
    
      radioButtons("typeLT.radio",
                        label = h3("Type of method:"),
                        choices = c("AUC","Log-rank"), selected = 'AUC',
                        inline = TRUE))
  })
  output$localTests2<- renderUI({
    
    
    if(input$typeData.radio=='IDM'){
      
       column(12,selectInput("fromLT",
                       
                       label = h4("From:"),
                       
                       choices = c("1", "2"), selected = '1'),
           selectInput("toLT",
                       
                       label = h4("To:"),
                       
                       choices = c("2", "3"), selected = '2'),
           textInput("replicasLT",label = 'Replicas'),
           actionButton("localTest.button", label = "View"))
    }else{
      
           column(12,selectInput("fromLT",
                              
                              label = h4("From:"),
                              
                              choices = c("1", "2","3","4","5"), selected = '1'),
               selectInput("toLT",
                           
                           label = h4("To:"),
                           
                           choices = c("2", "3","4","5","6"), selected = '2'),
               textInput("replicasLT",label = 'Replicas'),
               actionButton("localTest.button", label = "View"))
    }
    
   
    
  })
  
  
  observeEvent(input$localTest.button, {
    tipoAcao$data2 <- 'localTest'
  })
  
  
  observeEvent(input$typeLT.radio, {
    tipoAcao$data2 <- NULL
  })
  
  output$localTest.summary<-renderPrint({ 
    
    if (is.null(tipoAcao$data2)){
      
      return(NULL)
    }
    
    else{ 
      
      if(input$typeData.radio=='IDM'){
        
        dat<-loadData.manage()
        #dat<-loadData()$dataIDM
        
        dat$Stime<-as.numeric(as.character(dat$Stime)) #continua com os mesmos 7 NA
        dat$time1<-as.numeric(dat$time1)
        dat$event1<-as.numeric(dat$event1)
        dat$event<-as.numeric(dat$event)
        dat<-dat[!is.na(dat$Stime),]
        
        db_wide<-dat
        
        positions<-list(c(2, 3), c(3), c())
        namesStates =  c("Alive", "Rec",  "Death")
        
        tmat <-transMatMSM(positions, namesStates)
        timesNames = c(NA, "time1","Stime")
        
        status=c(NA, "event1","event")
        
      }else{ #other MSM:
       
        #datMSM<-loadData.msm()
        
        data("ebmt4")
        db_wide <- ebmt4
        positions=list(c(2, 3, 5, 6), c(4, 5, 6), c(4, 5, 6), c(5, 6), c(), c())
        namesStates =  c("Tx", "Rec", "AE", "Rec+AE", "Rel", "Death")
        
        tmat <-transMatMSM(positions, namesStates)
        timesNames = c(NA, "rec", "ae","recae", "rel", "srv")
        status=c(NA, "rec.s", "ae.s", "recae.s","rel.s", "srv.s")
        
      }
        trans = tmat
        
        db_long<- prepMSM(data=db_wide, trans, timesNames, status)
        
        times_splitLT<-as.numeric(unlist(strsplit(input$timesLT, ",")))
        
        #db_long$Tstart<-as.numeric(db_long$Tstart)
        
        #db_long$Tstop<-as.numeric(db_long$Tstop)
        
        #db_long$time<-as.numeric(db_long$time)
        
        #print(names(db_long))
        
        if(input$typeLT.radio=='AUC'){
          #print(db_long)  
          res<-local.test(db_long, db_wide, times=times_splitLT, from=as.numeric(input$fromLT), 
                          to=as.numeric(input$toLT),  tmat = tmat,replicas = 5, positions=positions, namesStates=namesStates, 
                          timesNames=timesNames,status=status)
          
          print(res$localTest)
      
        }
        
        if(input$typeLT.radio=='Log-rank'){
          
          res1<-LR.tests(db_long=db_long, times=times_splitLT, transition = 1, replicas = 500)
          
          res2<-LR.tests(db_long=db_long, times=times_splitLT, transition = 2, replicas = 500)
          
          res3<-LR.tests(db_long=db_long, times=times_splitLT, transition = 3, replicas = 500)
          
          c(res1$localTestLR, res2$localTestLR,res3$localTestLR)
          
        }
    

    } #caso nao tenha return null
  })
  
  
  output$localTest.confirm<- renderPrint({  #ou render PRint -- conforme o q quiser apresentar
    
    dat<-loadData.manage()
    #dat<-loadData()$dataIDM
    
    dat$Stime<-as.numeric(as.character(dat$Stime)) #continua com os mesmos 7 NA
    dat$time1<-as.numeric(dat$time1)
    dat$event1<-as.numeric(dat$event1)
    dat$event<-as.numeric(dat$event)
    dat<-dat[!is.na(dat$Stime),]
    
    #cat(as.numeric(input$toLT))
    
    if(input$typeLT.radio=='AUC'){
      
      #(times_splitLT<-as.numeric(unlist(strsplit(input$timesLT, ","))))
      #input$typeLT.radio
      #input$timesLT
      
      
      db_wide<-dat
      
      positions<-list(c(2, 3), c(3), c())
      namesStates =  c("Alive", "Rec",  "Death")
      
      tmat <-transMatMSM(positions, namesStates)
      timesNames = c(NA, "time1","Stime")
      
      status=c(NA, "event1","event")
      trans = tmat
      
      db_long<- prepMSM(data=db_wide, trans, timesNames, status)
      
      times_splitLT<-as.numeric(unlist(strsplit(input$timesLT, ",")))
      
      #res<-local.test(db_long, db_wide, times=times_splitLT, from=as.numeric(input$fromLT), 
      #                to=as.numeric(input$toLT), replicas=as.numeric(input$replicasLT), tmat = tmat)
      #times=c(365,500)
      
      input$replicasLT
      
      res<-local.test(db_long, db_wide, times=times_splitLT, from=as.numeric(input$fromLT), 
                      to=as.numeric(input$toLT), replicas=10, tmat = tmat)
      res$localTest
      
    }
  })
  
  
  
  output$globalTests1<- renderUI({
    
    dat<-loadData.manage()
    #dat<-loadData()$dataIDM
    
    column(12,
           h3("Global tests"),
           radioButtons("typeData.radio",
                        label = h3("Type of data:"),
                        choices = c("IDM","Other MSM"), selected = 'IDM',
                        inline = TRUE))
  })
    
    
    output$globalTests2<- renderUI({
      
      if(input$typeData.radio=='IDM'){
        
      column(12,
           radioButtons("typeGT.radio",
                        label = h3("Type of method:"),
                        choices = c('AUC','Log-rank','Cox PH model'), selected = 'AUC',
                        inline = TRUE),
           textInput("timesGT",label = 'Times (Log-rank test and Cox PH model)'),
           selectInput("fromGT",
                       
                       label = h4("From:"),
                       
                       choices = c("1", "2"), selected = '1'),
           selectInput("toGT",
                       
                       label = h4("To:"),
                       
                       choices = c("2", "3"), selected = '2'),
           textInput("ReplicasGT",label = 'Replicas'),
           actionButton("globalTest.button", label = "View"))
      }else{
        
        column(12,
               radioButtons("typeGT.radio",
                            label = h3("Type of method:"),
                            choices = c('AUC','Log-rank'), selected = 'AUC',
                            inline = TRUE),
               textInput("timesGT",label = 'Times (Log-rank test)'),
               selectInput("fromGT",
                           
                           label = h4("From:"),
                           
                           choices = c("1", "2","3","4","5"), selected = '1'),
               selectInput("toGT",
                           
                           label = h4("To:"),
                           
                           choices = c("2", "3","4","5","6"), selected = '2'),
               textInput("ReplicasGT",label = 'Replicas'),
               actionButton("globalTest.button", label = "View"))
      }
    
  })
  
  
  
  observeEvent(input$globalTest.button, {
    tipoAcao$data3<- 'globalTest'
  })
  
  
  #observeEvent(input$typeGT.radio, {
  #  tipoAcao$data3<-NULL
  #})
  
  output$globalTest.summary<-renderPrint({ 
    
    
    if (is.null(tipoAcao$data3)){
      
      return(NULL)
    }
    
    else{ 
      
      dat<-loadData.manage()
      #dat<-loadData()$dataIDM
      
      dat$Stime<-as.numeric(as.character(dat$Stime)) #continua com os mesmos 7 NA
      dat$time1<-as.numeric(dat$time1)
      dat$event1<-as.numeric(dat$event1)
      dat$event<-as.numeric(dat$event)
      dat<-dat[!is.na(dat$Stime),]
      
      db_wide<-dat
      
      positions<-list(c(2, 3), c(3), c())
      namesStates =  c("Alive", "Rec",  "Death")
      
      tmat <-transMatMSM(positions, namesStates)
      timesNames = c(NA, "time1","Stime")
      
      status=c(NA, "event1","event")
      trans = tmat
      
      db_long<- prepMSM(data=db_wide, trans, timesNames, status)
      
      times_splitGT<-as.numeric(unlist(strsplit(input$timesGT, ",")))
      
      if(input$typeGT.radio=='AUC'){
        
        res<-global.test(db_long, db_wide, from=as.numeric(input$fromGT), to=as.numeric(input$toGT), replicas = 3, tmat=tmat)
        print(res$globalTest)
        print(res$localTests)
        
      }
      if(input$typeGT.radio=='Log-rank'){
        
        
        # res1<-LR.tests(db_long=db_long, times=c(90,180,200), transition = 1, replicas = 50)
        #res2<-LR.tests(db_long=db_long, times=c(90,180,200), transition = 2, replicas = 50)
        res3<-LR.tests(db_long=db_long, times=c(90,180,200), transition = 3, replicas = 50)
        
        #c(res1$glovalTestLR, res2$glovalTestLR, res3$glovalTestLR)
        
        print(res3$globalTestLR)
        print(res3$localTestLR)
      }
      
      if(input$typeGT.radio=='Cox PH model'){
        
        mk <- markov.test(survIDM(time1,event1,Stime,event)~1, s=times_splitGT, nm.method = "LM", data=dat)
        print(mk$cox.markov.test)
        #mk$TPestimates
        #mk$nm.method
        
      }
      
    }
    
  })
  
  
  
})