library(shinythemes)
library(shiny)
library(DT)
library(bcp)


shinyUI(navbarPage("Bayesian Change Point", id="nav", theme = shinytheme("paper"),
tabPanel("Poll Analysis",
div(class="outer",
sidebarLayout(
sidebarPanel(

tags$style(type="text/css",
".shiny-output-error { visibility: hidden; }",
".shiny-output-error:before { visibility: hidden; }"
),

selectInput("choosepoll", label="Choose Dataset", choices=c("Trump", "Congress2018", "PartisanAffiliation", "Obamacare", "Obama", "ClintonTrump2016", "ObamaRomney2012"), selected="PartisanAffiliation"),
actionButton("pollbayes", label = "Go"),
downloadButton('downloadplot', label="Plot"),
downloadButton('downloadtable', label="Table"),

tags$p("Click 'Go' to start analysis"),



tags$hr(),

checkboxInput('advanced', "Advanced", value=FALSE),
uiOutput('advancedslider'),
uiOutput('advancedburnin'),
uiOutput('advancedmcmc'),

#tags$p("A smaller prior is better for big data sets"),

tags$hr(),
uiOutput('pollchooserpartisan'),
uiOutput('selectPeople'),
uiOutput('selectMode'),
uiOutput('selectPartisan'),
uiOutput('selectPollster'),

tags$p("Choose your inputs, but keep in mind live telephone interviews are more reliable")


),



mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Plot',


# this is an extra div used ONLY to create positioned ancestor for tooltip
# we don't change its position
div(
style = "position:relative",
plotOutput("approvalOutput", height = 400,
hover = hoverOpts("plot_hoverapproval", delay = 100, delayType = "debounce")),
uiOutput("hover_infoapproval")
),

div(
style = "position:relative",
plotOutput("posteriorProbOutput", height = 200,
hover = hoverOpts("plot_hoverposteriorprob", delay = 100, delayType = "debounce")),
uiOutput("hover_infoposteriorprob")
)



),

tabPanel("Bayes Table", DT::dataTableOutput('trumpchangepointtable')),


tabPanel("Poll Table", DT::dataTableOutput('trumppolltable'))


)
)
)
)
)



)
)
