library(ggplot2)
library(gridExtra)
library(DT)
library(shiny)
library(bcp)
library(dplyr)

options(shiny.maxRequestSize=180000*1024^2)

shinyServer(function(input, output, session) {



dataFetchTrump <- reactive({
    read.table(file="http://elections.huffingtonpost.com/pollster/api/v2/questions/00c%20-Pres-45-Trump%20-%20Job%20Approval%20-%20National/poll-responses-clean.tsv", header=TRUE, sep="\t", na.strings="/", quote = "")
})

dataFetchCongress2018 <- reactive({
    read.table(file="http://elections.huffingtonpost.com/pollster/api/v2/questions/18-US-House/poll-responses-clean.tsv", header=TRUE, sep="\t", na.strings="/", quote = "")
})

dataFetchObamacare <- reactive({
    read.table(file="http://elections.huffingtonpost.com/pollster/api/v2/questions/00c%20-US-Health-Bill/poll-responses-clean.tsv", header=TRUE, sep="\t", na.strings="/", quote = "")
})

dataFetchClintonTrump2016 <- reactive({
    read.table(file="http://elections.huffingtonpost.com/pollster/api/v2/questions/16-US-Pres-GE%20TrumpvClinton/poll-responses-clean.tsv", header=TRUE, sep="\t", na.strings="/", quote = "")
})

dataFetchObama <- reactive({
    read.table(file="http://elections.huffingtonpost.com/pollster/api/v2/questions/00c%20-Pres-44-Obama%20-%20Job%20Approval%20-%20National/poll-responses-clean.tsv", header=TRUE, sep="\t", na.strings="/", quote = "")
})

dataFetchObamaRomney2012 <- reactive({
    read.table(file="http://elections.huffingtonpost.com/pollster/api/v2/questions/12-US-Pres-GE%20RvO/poll-responses-clean.tsv", header=TRUE, sep="\t", na.strings="/", quote = "")
})

dataFetch <- reactive({
    input$choosepoll

    isolate(data <- if(input$choosepoll=="Trump"){
        dataFetchTrump()
    } else if(input$choosepoll=="Congress2018"){
        dataFetchCongress2018()
    } else if(input$choosepoll=="Obamacare"){
        dataFetchObamacare()
    } else if(input$choosepoll=="Obama"){
        dataFetchObama()
    } else if(input$choosepoll=="ClintonTrump2016"){
        dataFetchClintonTrump2016()
    } else if(input$choosepoll=="ObamaRomney2012"){
        dataFetchObamaRomney2012()
    })
    

    
})



selectPeopleUnique <- reactive({
    unique(dataFetch()$sample_subpopulation)
})

selectPollsterUnique <- reactive({
    unique(as.factor(dataFetch()$survey_house))
})

selectModeUnique <- reactive({
    unique(dataFetch()$mode)
})

selectPartisanUnique <- reactive({
    unique(dataFetch()$partisan_affiliation)
})

output$selectPeople <- renderUI({
    
    selectInput(inputId = "people_vars", label = h4("Population"), choices = selectPeopleUnique(), selected=c("Likely Voters", "Registered Voters", "Adults"), multiple=TRUE)
    
    
})

output$selectPollster <- renderUI({
    
    selectInput(inputId = "pollster_vars", label = h4("Pollster"), choices =  selectPollsterUnique(), selected=selectPollsterUnique(), multiple=TRUE)
    
    
})

output$selectMode <- renderUI({
    
    selectInput(inputId = "mode_vars", label = h4("Mode"), choices =  selectModeUnique(), selected="Live Phone", multiple=TRUE)
    
    
})

output$selectPartisan <- renderUI({
    
    selectInput(inputId = "partisan_vars", label = h4("Partisan Affiliation"), choices =  selectPartisanUnique(), selected="None", multiple=TRUE)
    
    
})


dataSubset <- reactive({
    
    all.data <- dataFetch()
    
    
    filter(all.data,
    sample_subpopulation %in% c(input$people_vars),
    mode %in% c(input$mode_vars),
    survey_house %in% c(input$pollster_vars),
    partisan_affiliation %in% c(input$partisan_vars)
    )
    

    
})

output$trumppolltable <- renderDataTable({
    
    DT::datatable(dataSubset())
    
})







ApproveBCP <- reactive({
    
    input$pollbayes

 
    isolate(approval.table <- dataSubset())
    
    
    
    approval.table$Date <- as.Date(approval.table$end_date, format="%Y-%m-%d")

    approve.bc <- bcp(y=approval.table$Approve, burnin=2000, mcmc=10000, w0=mean(approval.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    
    approve.posterior.mean <- approve.bc$posterior.mean
    approve.posterior.prob <- approve.bc$posterior.prob
    approve.posterior.var <- approve.bc$posterior.var
    approve.posterior.sd <- sqrt(approve.posterior.var)
    
    approve.bayes.dataframe <- data.frame(approval.table$Date, approval.table$Approve, approve.posterior.mean, approve.posterior.prob, approve.posterior.sd)
    colnames(approve.bayes.dataframe) <- c("Date", "Approve", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    notsure.frame <- approval.table[complete.cases(approval.table["Undecided"]),]
    
    notsure.bc <- bcp(y=notsure.frame$Undecided,  burnin=2000, mcmc=10000,  w0=mean(notsure.frame$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    notsure.posterior.mean <- notsure.bc$posterior.mean
    notsure.posterior.prob <- notsure.bc$posterior.prob
    notsure.posterior.var <- notsure.bc$posterior.var
    notsure.posterior.sd <- sqrt(notsure.posterior.var)
    
    notsure.bayes.dataframe <- data.frame(notsure.frame$Date, notsure.frame$Undecided, notsure.posterior.mean, notsure.posterior.prob, notsure.posterior.sd)
    colnames(notsure.bayes.dataframe) <- c("Date", "Undecided", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    disapprove.frame <- approval.table[complete.cases(approval.table["Disapprove"]),]

    
   disapprove.bc <- bcp(y=disapprove.frame$Disapprove,  burnin=2000, mcmc=10000, w0=mean(disapprove.frame$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    disapprove.posterior.mean <- disapprove.bc$posterior.mean
    disapprove.posterior.prob <- disapprove.bc$posterior.prob
    disapprove.posterior.var <- disapprove.bc$posterior.var
    disapprove.posterior.sd <- sqrt(disapprove.posterior.var)
    
    disapprove.bayes.dataframe <- data.frame(disapprove.frame$Date, disapprove.frame$Disapprove, disapprove.posterior.mean, disapprove.posterior.prob, disapprove.posterior.sd)
    colnames(disapprove.bayes.dataframe) <- c("Date", "Disapprove", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    total.frame <- data.frame(
    c(disapprove.frame$Date, notsure.frame$Date, approval.table$Date),
    c(disapprove.frame$Disapprove, notsure.frame$Undecided, approval.table$Approve),
    c(disapprove.posterior.mean, notsure.posterior.mean, approve.posterior.mean),
    c(disapprove.posterior.prob, notsure.posterior.prob, approve.posterior.prob),
    c(disapprove.posterior.sd, notsure.posterior.sd, approve.posterior.sd),
    c(rep("3. Disapprove", length(disapprove.frame$Disapprove)), rep("2. Undecided", length(notsure.frame$Undecided)), rep("1. Approve", length(approval.table$Approve))),
    c(disapprove.frame$survey_house, notsure.frame$survey_house, approval.table$survey_house),
    c(disapprove.frame$mode, notsure.frame$mode, approval.table$mode)
    )
    colnames(total.frame) <- c("Date", "Rating", "PosteriorMean", "PosteriorProb", "PosteriorSd", "Type", "Pollster", "Mode")
    
    total.frame$Hodder <- Hodder(total.frame$PosteriorMean)
    total.frame$PosteriorProb <- total.frame$PosteriorProb*(total.frame$Hodder/abs(total.frame$Hodder+0.00001))*-1
    
    total.frame
    
    
    
})

SupportBCP <- reactive({
    
    input$pollbayes
    
    
    isolate(approval.table <- dataSubset())
    
    
    
    approval.table$Date <- as.Date(approval.table$end_date, format="%Y-%m-%d")
    
    approve.bc <- bcp(y=approval.table$Favor, burnin=2000, mcmc=10000, w0=mean(approval.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    
    approve.posterior.mean <- approve.bc$posterior.mean
    approve.posterior.prob <- approve.bc$posterior.prob
    approve.posterior.var <- approve.bc$posterior.var
    approve.posterior.sd <- sqrt(approve.posterior.var)
    
    approve.bayes.dataframe <- data.frame(approval.table$Date, approval.table$Favor, approve.posterior.mean, approve.posterior.prob, approve.posterior.sd)
    colnames(approve.bayes.dataframe) <- c("Date", "Favor", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    notsure.frame <- approval.table[complete.cases(approval.table["Undecided"]),]

    notsure.bc <- bcp(y=notsure.frame$Undecided,  burnin=2000, mcmc=10000,  w0=mean(notsure.frame$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    notsure.posterior.mean <- notsure.bc$posterior.mean
    notsure.posterior.prob <- notsure.bc$posterior.prob
    notsure.posterior.var <- notsure.bc$posterior.var
    notsure.posterior.sd <- sqrt(notsure.posterior.var)
    
    notsure.bayes.dataframe <- data.frame(notsure.frame$Date, notsure.frame$Undecided, notsure.posterior.mean, notsure.posterior.prob, notsure.posterior.sd)
    colnames(notsure.bayes.dataframe) <- c("Date", "Undecided", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    disapprove.bc <- bcp(y=approval.table$Oppose,  burnin=2000, mcmc=10000,  w0=mean(approval.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    disapprove.posterior.mean <- disapprove.bc$posterior.mean
    disapprove.posterior.prob <- disapprove.bc$posterior.prob
    disapprove.posterior.var <- disapprove.bc$posterior.var
    disapprove.posterior.sd <- sqrt(disapprove.posterior.var)
    
    disapprove.bayes.dataframe <- data.frame(approval.table$Date, approval.table$Oppose, disapprove.posterior.mean, disapprove.posterior.prob, disapprove.posterior.sd)
    colnames(disapprove.bayes.dataframe) <- c("Date", "Oppose", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    total.frame <- data.frame(
    c(approval.table$Date, notsure.frame$Date, approval.table$Date),
    c(approval.table$Oppose, notsure.frame$Undecided, approval.table$Favor),
    c(disapprove.posterior.mean, notsure.posterior.mean, approve.posterior.mean),
    c(disapprove.posterior.prob, notsure.posterior.prob, approve.posterior.prob),
    c(disapprove.posterior.sd, notsure.posterior.sd, approve.posterior.sd),
    c(rep("3. Oppose", length(approval.table$Oppose)), rep("2. Undecided", length(notsure.frame$Undecided)), rep("1. Favor", length(approval.table$Favor))),
    c(approval.table$survey_house, notsure.frame$survey_house, approval.table$survey_house),
    c(approval.table$mode, notsure.frame$mode, approval.table$mode)
    )
    colnames(total.frame) <- c("Date", "Rating", "PosteriorMean", "PosteriorProb", "PosteriorSd", "Type", "Pollster", "Mode")
    
    total.frame$Hodder <- Hodder(total.frame$PosteriorMean)
    total.frame$PosteriorProb <- total.frame$PosteriorProb*(total.frame$Hodder/abs(total.frame$Hodder+0.00001))*-1
    
    total.frame
    
    
    
})


Congress2018BCP <- reactive({
    
    input$pollbayes
    
    
    isolate(congress.table <- dataSubset())
    
    
    
    congress.table$Date <- as.Date(congress.table$end_date, format="%Y-%m-%d")
    
    approve.bc <- bcp(y=congress.table$Democrat, burnin=2000, mcmc=10000, w0=mean(congress.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    
    approve.posterior.mean <- approve.bc$posterior.mean
    approve.posterior.prob <- approve.bc$posterior.prob
    approve.posterior.var <- approve.bc$posterior.var
    approve.posterior.sd <- sqrt(approve.posterior.var)
    
    approve.bayes.dataframe <- data.frame(congress.table$Date, congress.table$Democrat, approve.posterior.mean, approve.posterior.prob, approve.posterior.sd)
    colnames(approve.bayes.dataframe) <- c("Date", "Democrat", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    notsure.frame <- congress.table[complete.cases(congress.table["Undecided"]),]

    notsure.bc <- bcp(y=notsure.frame$Undecided,  burnin=2000, mcmc=10000,  w0=mean(notsure.frame$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    notsure.posterior.mean <- notsure.bc$posterior.mean
    notsure.posterior.prob <- notsure.bc$posterior.prob
    notsure.posterior.var <- notsure.bc$posterior.var
    notsure.posterior.sd <- sqrt(notsure.posterior.var)
    
    notsure.bayes.dataframe <- data.frame(notsure.frame$Date, notsure.frame$Undecided, notsure.posterior.mean, notsure.posterior.prob, notsure.posterior.sd)
    colnames(notsure.bayes.dataframe) <- c("Date", "Undecided", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    disapprove.bc <- bcp(y=congress.table$Republican,  burnin=2000, mcmc=10000,  w0=mean(congress.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    disapprove.posterior.mean <- disapprove.bc$posterior.mean
    disapprove.posterior.prob <- disapprove.bc$posterior.prob
    disapprove.posterior.var <- disapprove.bc$posterior.var
    disapprove.posterior.sd <- sqrt(disapprove.posterior.var)
    
    disapprove.bayes.dataframe <- data.frame(congress.table$Date, congress.table$Republican, disapprove.posterior.mean, disapprove.posterior.prob, disapprove.posterior.sd)
    colnames(disapprove.bayes.dataframe) <- c("Date", "Republican", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    total.frame <- data.frame(
    c(congress.table$Date, notsure.frame$Date, congress.table$Date),
    c(congress.table$Republican, notsure.frame$Undecided, congress.table$Democrat),
    c(disapprove.posterior.mean, notsure.posterior.mean, approve.posterior.mean),
    c(disapprove.posterior.prob, notsure.posterior.prob, approve.posterior.prob),
    c(disapprove.posterior.sd, notsure.posterior.sd, approve.posterior.sd),
    c(rep("3. Republican", length(congress.table$Republican)), rep("2. Undecided", length(notsure.frame$Undecided)), rep("1. Democrat", length(congress.table$Democrat))),
    c(congress.table$survey_house, notsure.frame$survey_house, congress.table$survey_house),
    c(congress.table$mode, notsure.frame$mode, congress.table$mode)
    )
    colnames(total.frame) <- c("Date", "Rating", "PosteriorMean", "PosteriorProb", "PosteriorSd", "Type", "Pollster", "Mode")
    
    total.frame$Hodder <- Hodder(total.frame$PosteriorMean)
    total.frame$PosteriorProb <- total.frame$PosteriorProb*(total.frame$Hodder/abs(total.frame$Hodder+0.00001))*-1
    
    total.frame
    
    
    
})

election2016BCP <- reactive({
    
    input$pollbayes
    
    
    isolate(election2016.table <- dataSubset())
    
    
    
    election2016.table$Date <- as.Date(election2016.table$end_date, format="%Y-%m-%d")
    
    approve.bc <- bcp(y=election2016.table$Clinton, burnin=2000, mcmc=10000, w0=mean(election2016.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    
    approve.posterior.mean <- approve.bc$posterior.mean
    approve.posterior.prob <- approve.bc$posterior.prob
    approve.posterior.var <- approve.bc$posterior.var
    approve.posterior.sd <- sqrt(approve.posterior.var)
    
    approve.bayes.dataframe <- data.frame(election2016.table$Date, election2016.table$Clinton, approve.posterior.mean, approve.posterior.prob, approve.posterior.sd)
    colnames(approve.bayes.dataframe) <- c("Date", "Clinton", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    notsure.frame <- election2016.table[complete.cases(election2016.table["Undecided"]),]
    
    notsure.bc <- bcp(y=notsure.frame$Undecided,  burnin=2000, mcmc=10000,  w0=mean(notsure.frame$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    notsure.posterior.mean <- notsure.bc$posterior.mean
    notsure.posterior.prob <- notsure.bc$posterior.prob
    notsure.posterior.var <- notsure.bc$posterior.var
    notsure.posterior.sd <- sqrt(notsure.posterior.var)
    
    notsure.bayes.dataframe <- data.frame(notsure.frame$Date, notsure.frame$Undecided, notsure.posterior.mean, notsure.posterior.prob, notsure.posterior.sd)
    colnames(notsure.bayes.dataframe) <- c("Date", "Undecided", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    disapprove.bc <- bcp(y=election2016.table$Trump,  burnin=2000, mcmc=10000,  w0=mean(election2016.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    disapprove.posterior.mean <- disapprove.bc$posterior.mean
    disapprove.posterior.prob <- disapprove.bc$posterior.prob
    disapprove.posterior.var <- disapprove.bc$posterior.var
    disapprove.posterior.sd <- sqrt(disapprove.posterior.var)
    
    disapprove.bayes.dataframe <- data.frame(election2016.table$Date, election2016.table$Trump, disapprove.posterior.mean, disapprove.posterior.prob, disapprove.posterior.sd)
    colnames(disapprove.bayes.dataframe) <- c("Date", "Trump", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    total.frame <- data.frame(
    c(election2016.table$Date, notsure.frame$Date, election2016.table$Date),
    c(election2016.table$Trump, notsure.frame$Undecided, election2016.table$Clinton),
    c(disapprove.posterior.mean, notsure.posterior.mean, approve.posterior.mean),
    c(disapprove.posterior.prob, notsure.posterior.prob, approve.posterior.prob),
    c(disapprove.posterior.sd, notsure.posterior.sd, approve.posterior.sd),
    c(rep("3. Trump", length(election2016.table$Trump)),rep("2. Undecided", length(notsure.frame$Undecided)),rep("1. Clinton", length(election2016.table$Clinton))),
    c(election2016.table$survey_house, notsure.frame$survey_house, election2016.table$survey_house),
    c(election2016.table$mode, notsure.frame$mode, election2016.table$mode)
    )
    colnames(total.frame) <- c("Date", "Rating", "PosteriorMean", "PosteriorProb", "PosteriorSd", "Type", "Pollster", "Mode")
    
    total.frame$Hodder <- Hodder(total.frame$PosteriorMean)
    total.frame$PosteriorProb <- total.frame$PosteriorProb*(total.frame$Hodder/abs(total.frame$Hodder+0.00001))*-1
    
    total.frame
    
    
    
})

election2012BCP <- reactive({
    
    input$pollbayes
    
    
    isolate(election2012.table <- dataSubset())
    
    
    
    election2012.table$Date <- as.Date(election2012.table$end_date, format="%Y-%m-%d")
    
    approve.bc <- bcp(y=election2012.table$Obama, burnin=2000, mcmc=10000, w0=mean(election2012.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    
    approve.posterior.mean <- approve.bc$posterior.mean
    approve.posterior.prob <- approve.bc$posterior.prob
    approve.posterior.var <- approve.bc$posterior.var
    approve.posterior.sd <- sqrt(approve.posterior.var)
    
    approve.bayes.dataframe <- data.frame(election2012.table$Date, election2012.table$Obama, approve.posterior.mean, approve.posterior.prob, approve.posterior.sd)
    colnames(approve.bayes.dataframe) <- c("Date", "Obama", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    notsure.frame <- election2012.table[complete.cases(election2012.table["Undecided"]),]
    
    notsure.bc <- bcp(y=notsure.frame$Undecided,  burnin=2000, mcmc=10000,  w0=mean(notsure.frame$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    notsure.posterior.mean <- notsure.bc$posterior.mean
    notsure.posterior.prob <- notsure.bc$posterior.prob
    notsure.posterior.var <- notsure.bc$posterior.var
    notsure.posterior.sd <- sqrt(notsure.posterior.var)
    
    notsure.bayes.dataframe <- data.frame(notsure.frame$Date, notsure.frame$Undecided, notsure.posterior.mean, notsure.posterior.prob, notsure.posterior.sd)
    colnames(notsure.bayes.dataframe) <- c("Date", "Undecided", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    disapprove.bc <- bcp(y=election2012.table$Romney,  burnin=2000, mcmc=10000,  w0=mean(election2012.table$margin_of_error, na.rm=TRUE)/100, p0=input$prior)
    disapprove.posterior.mean <- disapprove.bc$posterior.mean
    disapprove.posterior.prob <- disapprove.bc$posterior.prob
    disapprove.posterior.var <- disapprove.bc$posterior.var
    disapprove.posterior.sd <- sqrt(disapprove.posterior.var)
    
    disapprove.bayes.dataframe <- data.frame(election2012.table$Date, election2012.table$Romney, disapprove.posterior.mean, disapprove.posterior.prob, disapprove.posterior.sd)
    colnames(disapprove.bayes.dataframe) <- c("Date", "Romney", "PosteriorMean", "PosteriorProb", "PosteriorSD")
    
    total.frame <- data.frame(
    c(election2012.table$Date, notsure.frame$Date, election2012.table$Date),
    c(election2012.table$Romney, notsure.frame$Undecided, election2012.table$Obama),
    c(disapprove.posterior.mean, notsure.posterior.mean, approve.posterior.mean),
    c(disapprove.posterior.prob, notsure.posterior.prob, approve.posterior.prob),
    c(disapprove.posterior.sd, notsure.posterior.sd, approve.posterior.sd),
    c(rep("3. Romney", length(election2012.table$Romney)), rep("2. Undecided", length(notsure.frame$Undecided)), rep("1. Obama", length(election2012.table$Obama))),
    c(election2012.table$survey_house, notsure.frame$survey_house, election2012.table$survey_house),
    c(election2012.table$mode, notsure.frame$mode, election2012.table$mode)
    )
    colnames(total.frame) <- c("Date", "Rating", "PosteriorMean", "PosteriorProb", "PosteriorSd", "Type", "Pollster", "Mode")
    
    total.frame$Hodder <- Hodder(total.frame$PosteriorMean)
    total.frame$PosteriorProb <- total.frame$PosteriorProb*(total.frame$Hodder/abs(total.frame$Hodder+0.00001))*-1
    
    total.frame
    
    
    
})


BCPTable <- reactive({
    
    if(input$choosepoll=="Trump"){
        ApproveBCP()
    } else if(input$choosepoll=="Congress2018"){
        Congress2018BCP()
    } else if(input$choosepoll=="Obamacare"){
        SupportBCP()
    } else if(input$choosepoll=="Obama"){
        ApproveBCP()
    } else if(input$choosepoll=="ClintonTrump2016"){
        election2016BCP()
    } else if(input$choosepoll=="ObamaRomney2012"){
        election2012BCP()
    }
})


###Create Plots

ratingPlot <- reactive({
    
    bcp.table <- BCPTable()
    
    ggplot(bcp.table, aes(Date, Rating)) +
    geom_point(alpha=0.5, aes(colour=Type)) +
    geom_line(aes(as.Date(Date, format="%Y-%m-%d"), PosteriorMean, colour=Type)) +
    theme_light() +
    scale_x_date("Date", date_minor_breaks = "1 month") +
    scale_y_continuous("Rating %") +
    scale_color_manual(values = rev(cols))
    
    
})

posteriorProbPlot <- reactive({
    bcp.table <- BCPTable()

    ggplot(bcp.table, aes(Date, PosteriorProb)) +
    geom_line(aes(colour=Type))+
    theme_bw() +
    scale_x_date("Date", date_minor_breaks = "1 month") +
    scale_y_continuous("Probability", limits = c(-1.05, 1.05), breaks=seq(-1, 1, 0.25)) +
    scale_color_manual(values = rev(cols))
})



output$approvalOutput <- renderPlot({
    ratingPlot()
    
})


output$hover_infoapproval <- renderUI({
    
    point.table <- BCPTable()
    
    hover <- input$plot_hoverapproval
    point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    
    approval.only <- filter(point, Rating=="Approval")
    disapproval.only <- filter(point, Rating=="Disapproval")
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    
    # actual tooltip created as wellPanel
    wellPanel(
    style = style,
    p(HTML(paste0("<b> Date: </b>", point$Date, "<br/>"))),
    p(HTML(paste0("<b> Pollster: </b>", point$Pollster, "<br/>"))),
    p(HTML(paste0("<b> Mode: </b>", point$Mode, "<br/>"))),
    p(HTML(paste0("<b> Rating: </b>", point$Rating, "<b>%</b>", "<br/>")))


    )
})


output$posteriorProbOutput <- renderPlot({
    posteriorProbPlot()
    
})


output$hover_infoposteriorprob <- renderUI({
    
    point.table <- BCPTable()
    
    hover <- input$plot_hoverposteriorprob
    point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
    style = style,
    p(HTML(paste0("<b> Date: </b>", point$Date, "<br/>",
    "<b> Posterior Probability: </b>", point$PosteriorProb, "<br/>"
    
    )))
    )
})




output$downloadplot <- downloadHandler(
filename = function() { paste(input$choosepoll, '.jpg',  sep='') },
content = function(file) {
    ggsave(file,grid.arrange(ratingPlot(), posteriorProbPlot(), nrow=2), device='jpeg', dpi=300, width=12, height=7)
}
)


output$trumpchangepointtable <- renderDataTable({
    
    DT::datatable(BCPTable())
    
})

output$downloadtable <- downloadHandler(
filename = function() { paste(input$choosepoll, '.csv', sep='') },
content = function(file
) {
    write.csv(BCPTable(), file)
}
)


})
