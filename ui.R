library(tidycensus)
library(acs)
library(dplyr)
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(lubridate)
library(maptools)
library(readr)
library(rgeos)
library(GISTools)
library(rgdal)
library(data.table)

navbarPage("COVID Demographic Mapping", id="nav",
                 tabPanel("Attribute Map", tags$style(HTML('
#type {background-color: rgba(255,255,255,0.8);;}'
                 ), type = "text/css", "html, body {width:100%;height:100%}"),
                 leafletOutput("plot", width = "100%", height = "700px"),
                 absolutePanel(top = 200, left = 10,
                               radioButtons("type", "Attribute Graph Type:",
                                            list('Percent with Alcoholism' = 'alc',
                                                 'Percent in Management Job Positions' = 'mang',
                                                 'Percent with Diabetes' = 'diabetes',
                                                 'Percent Smokers' = 'csmoking',
                                                 'Percent Obesity' = 'obesity',
                                                 'Percent Male' = 'male',
                                                 'Percent White' = 'white',
                                                 'Percent Hispanic' = 'hispanic',
                                                 'Percent Elderly' = 'over_65',
                                                 'Percent with High School Degree' = 'hs',
                                                 'Percent with Phone Service' = 'phone',
                                                 'Percent with No Vehicle' = 'novehicle',
                                                 'Polution Concentration' = 'polution',
                                                 'Percent with Internet' = 'int',
                                                 'Percent Young Childeren ' = 'young',
                                                 'Percent in Poverty' = 'poverty',
                                                 'Proportion of Crowded Areas' = 'crowd',
                                                 'Median Income' = 'minc'
                                            )))),
                 tabPanel("COVID", tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                          leafletOutput("plot1", width = "100%", height = "700px"),
                          absolutePanel(bottom = 0, left = 10,
                                        sliderInput("day", label = "Days After First Recorded US Case:", 
                                                    min=0, max= (length(date)-1), value= (length(date)-1)))),
                 tabPanel("Latent Variable Relationships", 
                          sidebarPanel(position = 'left', radioButtons("scale", "Scale:",
                                                                       list('National' = 'nat',
                                                                            'By County and State' = 'coun'
                                                                       )),
                                       conditionalPanel(
                                         condition = "input.scale == 'nat'",
                                         sliderInput("day1", "Days After March 31:", 
                                                     min=0, max= (length(date)-1), value= (length(date)-1))),
                                       conditionalPanel(
                                         condition = "input.scale == 'coun'",
                                         selectizeInput("searchc", label = "County Search", choices =  unique(shape_C$identity), selected = "Orange County", options = list(maxItems = 1))),
                                       conditionalPanel(condition = "input.scale == 'nat'",
                                                        radioButtons("type1", "Attribute Graph Type:",
                                                                     list('Percent with Alcoholism' = 'alc',
                                                                          'Percent in Management Job Positions' = 'mang',
                                                                          'Percent with Diabetes' = 'diabetes',
                                                                          'Percent Smokers' = 'csmoking',
                                                                          'Percent Obesity' = 'obesity',
                                                                          'Percent Male' = 'male',
                                                                          'Percent White' = 'white',
                                                                          'Percent Hispanic' = 'hispanic',
                                                                          'Percent Elderly' = 'over_65',
                                                                          'Percent with High School Degree' = 'hs',
                                                                          'Percent with Phone Service' = 'phone',
                                                                          'Percent with No Vehicle' = 'novehicle',
                                                                          'Polution Concentration' = 'polution',
                                                                          'Percent with Internet' = 'int',
                                                                          'Percent Young Childeren ' = 'young',
                                                                          'Percent in Poverty' = 'poverty',
                                                                          'Proportion of Crowded Areas' = 'crowd',
                                                                          'Median Income' = 'minc'
                                                                     )))
                          ),
                          mainPanel(
                            conditionalPanel(condition = "input.scale == 'nat'", plotOutput("plot2")),
                            conditionalPanel(condition = "input.scale == 'coun'",
                                             fluidRow(
                                               column(8,plotOutput("plot3", width = "100%", height = "600px"),plotOutput('plot4', width = "100%", height = "75px"))
                                             ))
                          ))
)