library(shiny)
library(shinythemes)
library(tile)  # Currently using for lighten function

# Retrieve Data ####
jcode<-read.delim("C:/Users/cmp53/OneDrive/Documents/R/Party-Codex.csv",sep=",",stringsAsFactors=FALSE)
print.table(jcode)
# SHINY CODE ####

shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  

  sidebarLayout(
    sidebarPanel(h1("Electoral Cooperation in Japan: 1996-2017"),
      fluidRow(
        actionButton("save","Save"),
        actionButton("map12","Change Map")),        
      selectInput("year", 
                  label = "Choose a year to display",
                  choices = c(1996,2000,2003,2005,2009,2012,2014,2017),
                  selected = 2005),
      sliderInput("eff",
                  "Alliance Efficiency",
                  min=0,max=1,value=0),
# 2017 does not work...unsure why   
    fluidRow(
      column(4,
      checkboxGroupInput("a1", 
                  label = "Alliance 1 Parties:",
                  choices = "Liberal Democratic Party",
                  selected = "Liberal Democratic Party")),
      column(4,
      checkboxGroupInput("a2", 
                         label = "Alliance 2 Parties:",
                         choices = "Democratic Party of Japan",
                         selected = "Democratic Party of Japan")),
      column(4,
             checkboxGroupInput("a3", 
                                label = "Alliance 3 Parties:",
                                choices = "Japanese Communist Party",
                                selected = character(0)))
      ),
    helpText("This app allows the user to see how up to 3 hypothetical alliances could have performend 
             in Japanese lower house elections from 1996 to 2017.  By default, the first two alliances 
             are set to historic alliances for that year.  If no members are selected for the third alliance, 
             it defaults to the best outside candidate.  By default, alliance efficiency is set to zero.  
             This does not mean that the alliance does not work, but rather that in each district where 
             two alliance candidates ran against one another, neither stepped down (the historic case).  
             For all positive values, alliance efficiency represents the net share of their votes that go 
             to the strongest candidate.  The save function is for comparing the current plot to future plots.")
),
    
    mainPanel(
      
      fluidRow(
        column(6,
          plotOutput("ternaryPlot",height=600,width=600)),
        column(6,
          plotOutput("ternaryPlot.saved",height=600,width=600))
      ),
#      plotOutput("map.plot",height=600,width=600)
      fluidRow(
        column(6,
               plotOutput("map.plot",height=600,width=600)),
        column(6,
               plotOutput("map.plot.saved",height=600,width=600))
      )
    )
  )
))

