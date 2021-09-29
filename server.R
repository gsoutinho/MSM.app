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
  
  
  
})