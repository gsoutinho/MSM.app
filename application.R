rm(list=ls())
setwd("C:/Users/asus/Desktop/PEN/_LA_6/35_app.MSM/")
#install.packages("shiny")
library(shiny)
require(shiny)

library("survidm")
require("survidm")
library("survival")
library("mstate")
library("markovMSM")

runApp("appMSM") 

#-----------------------------------------------------------------------------
install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='gsoutinho',
                          token='160A403E1666E6129FF1421A5339F531',
                          secret='iXmA8fHyvc7oqMcUFmW6WoHzxLqI1+tOy8zgQ+NT')

deployApp("appMSM")