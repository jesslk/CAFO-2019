library(shiny)
library(shinyBS)
library(dplyr)
library(knitr)
library(DT)
library(readxl)
library(timevis)
library(ggplot2)
library(leaflet)
library(metafor)
library(gridExtra)
library(stringr)
library(reshape2)
library(RColorBrewer)
library(RGraphics)


shinyServer(function(output, input, session)({
  
  source("selectiveSideBar.R", local = TRUE)
  source("selectDataSet.R", local = TRUE)
  source("reactive.R", local = TRUE)
  source("forestPlotPrepare.R", local = TRUE)
  source("forestROBPlot.R",  local = TRUE)
  source("forestROBPlotExperiment.R", local = TRUE)
 
  
  ## First descriptive RMD  
output$descriptive <- renderUI({
  HTML(markdown::markdownToHTML(knit("../scripts/descriptive.Rmd", quiet = TRUE)))
  
  
})

## Leaflet map w/ customized tiles 

output$map <- renderLeaflet({
  cafo <- readxl::read_xlsx("../datasets/cafo.xlsx", sheet = "Treatment-Outcome data")
 ## Create location data 
   cafo <- cafo %>% mutate(Country = ifelse(Refid == 648 | Refid == 690 | Refid == 743|Refid == 288, "Germany",
                                           ifelse(Refid == 81 | Refid == 203, "Netherlands", "United States"))
                          )
   cafo <- cafo %>% mutate(`State` = ifelse(Refid == 64 | Refid == 690 | Refid == 743 | Refid == 288, NA,
                                                     ifelse(Refid == 81 | Refid == 203, NA, "North Carolina")))
   cafo <- cafo %>% mutate(long = ifelse(Country == "Germany", 13.404954,
                                        ifelse(Country == "Netherlands", 4.899431, -78.644257)))
   cafo <- cafo %>% mutate(lat = ifelse(Country == "Germany", 52.520008,
                                        ifelse(Country == "Netherlands", 52.379189, 35.787743)))
   cafoo <- distinct(cafo, Refid, .keep_all = TRUE)
   cafoo <- cafoo %>% group_by(Country, long, lat) %>% summarise(`Number of Studies` = n())
   leaflet(data=cafoo) %>%  addProviderTiles(providers$Stamen.TonerLite,
                                           options = providerTileOptions(noWrap = TRUE)
  ) %>%  setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
     addCircleMarkers(lng = cafoo$long, lat = cafoo$lat,radius = log(cafoo$`Number of Studies`)*8, popup = ~paste("Country:", cafoo$Country, "<br>",
                                                                          
                                                                          "Number of Studies:", cafoo$`Number of Studies`) )})
## Country bar plot

output$bar <- renderPlot({
  cafo <- readxl::read_xlsx("../datasets/cafo2.xlsx") %>% select(Refid, `Author(s)`, Year, Country, State, City, lng, lat, Title, `Journal Name`)
  unique(cafo) %>% ggplot(aes(x = Country)) + geom_bar(aes(fill = Country)) + geom_text(stat = "count", aes(label = ..count..), vjust = -1) ## Display number of counts
})


## Timeline of the studies 
## Need to be automated

output$time <- renderTimevis({
  ## Only select authors and year information columns
  timedata <- unique(dataset %>% select(paperInfo, paperYear))
  
  ## Extract only author names from paperInfo column. Extract string comes before the period 
  sub("\\..*", "", timedata$paperInfo) -> timedata$Author
 # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
  timedata2 <- timedata %>% select(paperYear, Author)
  ## Insert into a dataframe 
  datt <- data.frame(
    ## make it reactive
    id = 1:nrow(timedata2),   
    content = timedata2$Author,
    start = timedata2$paperYear,
    end = NA
  )
  timevis(datt)
})

## How many recorded outcomes per study?

output$multiple <- renderPlot({
  ## Rearrange the studies by increasing order
  dataset2 <- within(dataset, 
                     paperInfo <- factor(paperInfo, 
                                         levels=names(sort(table(paperInfo), 
                                                           increasing=TRUE))))
  ##Recode some of the variables
  
  dataset2$Effect.measure[dataset2$Effect.measure=="beta"] <- "Beta"
  dataset2$Effect.measure[dataset2$Effect.measure=="beta p value"] <- "Beta P Value"
  dataset2$Effect.measure[dataset2$Effect.measure=="OR"] <- "Odds Ratio"
  dataset2$Effect.measure[dataset2$Effect.measure=="OR p value"] <- "Odds Ratio P Value"
  dataset2$Effect.measure[dataset2$Effect.measure=="PR"] <- "Prevalence Ratio"
  dataset2 %>% ggplot(aes(x = paperInfo)) + geom_bar(aes(fill = Effect.measure)) + coord_flip() + geom_text(stat = "count", aes(label = ..count..), vjust = 0) +
    labs(x = "", fill = "Effect Measure") + ylab("Number of Reported Outcomes")
  
})

## Reactive UI for 2nd selectinput because 2nd selectinput depends on the first selectinput
### Figuring out how to create selection as a reactive statement
selection <- data.frame("Body" = c(rep("Nervous", 3),rep("Upper Respiratory", 3), rep("Lower Respiratory", 6), rep("Other", 1) ), 
                        Outcome = c("All", "Psychological Distress", "Neurologic", "All", "Allergic rhinitis", "Other", "Asthma", "Other Types of Asthma", "Chronic obstructive pulmonary disease (COPD)", "Bronchial Hyperresponsiveness to Methacholine (BHM)", "Pneumonia", "Wheezing without a cold", "All"))
observeEvent(
  input$Body,
  updateSelectInput(session, "Outcome", "Outcome", 
                    choices = selection$Outcome[selection$Body==input$Body]))

##Metafor forest plot
## Rows and columns for the plot have to be specified. 
## If unspecified, the function tries to set the limits to some sensible values
## However, sometimes you have to set them manually


output$metafor <- renderPlot({
  library(metafor)
  ## Only select records with Odds Ratio
  
  #if(input$Body == "Nervous"){
   # dataset3 <- dataset %>% 
    #  filter(Effect.measure == "OR") %>% filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress"|Categorized.class == "Neurologic")
    #select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    ## Again, recoding variables. Could be changed in the future?
    #dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry  grouchy  or bad-tempered"] <- "Angry grouchy or bad-tempered"
    #dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy  blue  or unhappy"] <- "Gloomy blue or unhappy"
    #dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy blue or unhappy"] <- "Gloomy blue or unhappy"
    ##dataset3$Outcome.variable[dataset3$Outcome.variable=="Confused or unable to concentrate"] <- "Psychological Distress"
    ##dataset3$Outcome.variable[dataset3$Outcome.variable=="Nervous or anxious"] <- "Psychological Distress"
    ##dataset3$Outcome.variable[dataset3$Outcome.variable=="Stressed or annoyed"] <- "Psychological Distress"
    ## Fit the linear model. I calculated sd manually 
    #forest(dataset3$Effect.measure.1, ilab.pos = 4, ci.lb = dataset3$Lower, ci.ub = dataset3$Upper, refline = 1, slab = paste(dataset3$Refid, dataset3$paperInfo),xlim = c(-7,4),ilab.xpos=c(-5.2,-2.5,-0.4), ilab=cbind(dataset3$Outcome.variable, dataset3$Exposure.measure, dataset3$ROB_confounding_paige))
    #text(c(-6, -4,-1.9,-0.3), 22, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    #text(4,                  22, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Body == "Nervous" & input$Outcome == "All" ){
    dataset3 <- dataset %>% 
      filter(Effect.measure == "OR") %>% filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress"|Categorized.class == "Neurologic")
    ## Again, recoding variables. Could be changed in the future?
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry  grouchy  or bad-tempered"] <- "Angry grouchy or bad-tempered"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy  blue  or unhappy"] <- "Gloomy blue or unhappy"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy blue or unhappy"] <- "Gloomy blue or unhappy"
    #dataset3$Outcome.variable[dataset3$Outcome.variable=="Confused or unable to concentrate"] <- "Psychological Distress"
    #dataset3$Outcome.variable[dataset3$Outcome.variable=="Nervous or anxious"] <- "Psychological Distress"
    #dataset3$Outcome.variable[dataset3$Outcome.variable=="Stressed or annoyed"] <- "Psychological Distress"
    ## Fit the linear model. I calculated sd manually 
    forest(dataset3$Effect.measure.1, ilab.pos = 4, ci.lb = dataset3$Lower, ci.ub = dataset3$Upper, refline = 1, slab = paste(dataset3$Refid, dataset3$paperInfo),xlim = c(-7,4),ilab.xpos=c(-5.2,-2.5,-0.4), ilab=cbind(dataset3$Outcome.variable, dataset3$Exposure.measure, dataset3$ROB_confounding_paige))
    text(c(-6, -4,-1.9,-0.3), 22, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(4,                  22, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Psychological Distress"){
    dataset3 <- dataset %>% 
      filter(Effect.measure == "OR") %>% filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress"|Categorized.class == "Neurologic")
    dataset31 <- dataset3 %>% 
      filter(Categorized.class == "Psychological"|Categorized.class == "Stress")
    forest(dataset31$Effect.measure.1, ilab.pos = 4, ci.lb = dataset31$Lower, ci.ub = dataset31$Upper, refline = 1, slab = paste(dataset31$Refid, dataset31$paperInfo),xlim = c(-17,6.5),ilab.xpos=c(-13,-6.7,-2), ilab=cbind(dataset31$Outcome.variable, dataset31$Exposure.measure, dataset31$ROB_confounding_paige))
    text(c(-14.6, -10.5,-5,-1.4), 18, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(6.4,                  18, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Neurologic"){
    dataset3 <- dataset %>% 
      filter(Effect.measure == "OR") %>% filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress"|Categorized.class == "Neurologic")
    dataset32 <- dataset3 %>% 
      filter(Categorized.class == "Stress")
    forest(dataset32$Effect.measure.1, ilab.pos = 4, ci.lb = dataset32$Lower, ci.ub = dataset32$Upper, refline = 1, slab = paste(dataset32$Refid, dataset32$paperInfo),xlim = c(-18.5,6.5),ilab.xpos=c(-13,-8.3,-2), ilab=cbind(dataset32$Outcome.variable, dataset32$Exposure.measure, dataset32$ROB_confounding_paige))
    text(c(-16.5, -11,-6,-1.4), 5.5, c("ID and Year","Outcome Variable", "Exposure", "Confounding"))
    text(6.5,                  5.5, "Odds Ratio [95% CI]", pos=2)}

  
  
  #if(input$Body == "Upper Respiratory"){
   # dataset4 <- dataset %>% filter(Effect.measure == "OR") %>% 
    #  filter(Categorized.class == "Upper Respiratory") %>%
     # select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    #forest(dataset4$Effect.measure.1, ilab.pos = 4, ci.lb = dataset4$Lower, ci.ub = dataset4$Upper, clim= c(0, 5) , refline = 1, slab = paste(dataset4$Refid, dataset4$paperInfo),xlim = c(-17,6.5),ilab.xpos=c(-13,-7.3,-2), ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige))
    #text(c(-15, -11,-7,-2), 28, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    #text(6.2,                  28, "Odds Ratio [95% CI]", pos=2)}
  
  ## One confidence interval is too wide so I put a limit on the confidence interval using clim. 
  if(input$Body == "Upper Respiratory" & input$Outcome == "All"){
    dataset4 <- dataset %>% filter(Effect.measure == "OR") %>% 
      filter(Categorized.class == "Upper Respiratory") 
    forest(dataset4$Effect.measure.1, ilab.pos = 4, ci.lb = dataset4$Lower, ci.ub = dataset4$Upper, clim= c(0, 5) , refline = 1, slab = paste(dataset4$Refid, dataset4$paperInfo),xlim = c(-17,6.5),ilab.xpos=c(-13,-7.3,-2), ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige))
    text(c(-15, -11,-7,-2), 28, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(6.2,                  28, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Allergic rhinitis"){
    dataset4 <- dataset %>% filter(Effect.measure == "OR") %>% 
      filter(Categorized.class == "Upper Respiratory")
    dataset41 <- dataset4 %>% 
      filter(Outcome.variable == "Allergic rhinitis")
    forest(dataset41$Effect.measure.1, ilab.pos = 4, ci.lb = dataset41$Lower, ci.ub = dataset41$Upper, refline = 1, slab = paste(dataset41$Refid, dataset41$paperInfo),xlim = c(-14,6.5),ilab.xpos=c(-11,-8.5,-2), ilab=cbind(dataset41$Outcome.variable, dataset41$Exposure.measure, dataset41$ROB_confounding_paige))
    text(c(-12.5, -10.5,-8,-1.5), 21, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(6,                  21, "Odds Ratio [95% CI]", pos=2)}

  if(input$Outcome == "Other"){
    dataset4 <- dataset %>% filter(Effect.measure == "OR") %>% 
      filter(Categorized.class == "Upper Respiratory")
    dataset42 <- dataset4 %>% 
      filter(Outcome.variable == "Sensitization against ubiquitous allergens"| Outcome.variable == "Specific IgE to Common Allergens >0.35IU/mL"|Outcome.variable == "IgE")
    forest(dataset42$Effect.measure.1, ilab.pos = 4, clim = c(0,5),ci.lb = dataset42$Lower, ci.ub = dataset42$Upper, refline = 1, slab = paste(dataset42$Refid, dataset42$paperInfo),xlim = c(-17,6.5),ilab.xpos=c(-13.5,-7.2,-1.5), ilab=cbind(dataset42$Outcome.variable, dataset42$Exposure.measure, dataset42$ROB_confounding_paige))
    text(c(-15.2, -12,-7,-1), 8.5, c("Refid and Author","Outcome Variable", "Exposure Measure", "Confounding"))
    text(6,                  8.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Body == "Lower Respiratory" & input$Outcome == "Asthma"){
    dataset5 <- dataset %>% filter(Effect.measure == "OR")%>%
      filter(Categorized.class == "Lower Respiratory") 
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Chronic obstructive pulmonary disease (COPD)"] <- "COPD"
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Bronchial Hyperresponsiveness to Methacholine"] <- "BHM"
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Wheezing without cold"] <- "Wheezing without a cold"
    dataset51 <- dataset5 %>% filter(Outcome.variable == "Asthma")
    forest(dataset51$Effect.measure.1, ilab.pos = 4, clim = c(0,5),ci.lb = dataset51$Lower, ci.ub = dataset51$Upper, refline = 1, slab = paste(dataset51$Refid, dataset51$paperInfo),xlim = c(-7,3),ilab.xpos=c(-5, -4, -1), ilab=cbind(dataset51$Outcome.variable, dataset51$Exposure.measure, dataset51$ROB_confounding_paige))
    text(c(-6.3, -4.8, -3.2, -1), 25, c("Refid and Author","Outcome Variable", "Exposure Measure", "Confounding"))
    text(5,                  25, "Odds Ratio [95% CI]", pos=2)}

  if(input$Body == "Lower Respiratory" & input$Outcome == "Other Types of Asthma"){
    dataset5 <- dataset %>% filter(Effect.measure == "OR")%>%
      filter(Categorized.class == "Lower Respiratory") 
    dataset52 <- dataset5 %>% 
      filter(Outcome.variable == "Physician-Diagnosed Asthma"|Outcome.variable =="Allergic asthma"|Outcome.variable == "Non-allergic asthma")
    forest(dataset52$Effect.measure.1, ilab.pos = 4, ci.lb = dataset52$Lower, ci.ub = dataset52$Upper, refline = 1, slab = paste(dataset52$Refid, dataset52$paperInfo),xlim = c(-8,5),ilab.xpos=c(-6, -3.5, -0.6), ilab=cbind(dataset52$Outcome.variable, dataset52$Exposure.measure, dataset52$ROB_confounding_paige))
    text(c(-7.2, -5.3, -3, -0.3), 7.5, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(4.8,                  7.5, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Body == "Lower Respiratory" & input$Outcome == "Chronic obstructive pulmonary disease (COPD)"){
    dataset5 <- dataset %>% filter(Effect.measure == "OR")%>%
      filter(Categorized.class == "Lower Respiratory") 
    dataset53 <- dataset5 %>% 
      filter(Outcome.variable == "COPD")
    forest(dataset53$Effect.measure.1, ilab.pos = 4, ci.lb = dataset53$Lower, ci.ub = dataset53$Upper, refline = 1, slab = paste(dataset53$Refid, dataset53$paperInfo),xlim = c(-8,5),ilab.xpos=c(-6, -4.5, -0.6), ilab=cbind(dataset53$Outcome.variable, dataset53$Exposure.measure, dataset53$ROB_confounding_paige))
    text(c(-7.2, -5.3, -3.3, -0.3), 13.5, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(4.8,                  13.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Bronchial Hyperresponsiveness to Methacholine (BHM)"){
    dataset5 <- dataset %>% filter(Effect.measure == "OR")%>%
      filter(Categorized.class == "Lower Respiratory") 
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Bronchial Hyperresponsiveness to Methacholine"] <- "BHM"
    dataset54 <- dataset5 %>% 
      filter(Outcome.variable == "BHM")
    forest(dataset54$Effect.measure.1, ilab.pos = 4, ci.lb = dataset54$Lower, ci.ub = dataset54$Upper, refline = 1, slab = paste(dataset54$Refid, dataset54$paperInfo),xlim = c(-10,5),ilab.xpos=c(-7.5,-6,-2), ilab=cbind(dataset54$Outcome.variable, dataset54$Exposure.measure, dataset54$ROB_confounding_paige))
    text(c(-9.2, -7,-5,-1.4), 7.5, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(5,                  7.5, "Odds Ratio [95% CI]", pos=2)
  }

  
  if(input$Outcome == "Pneumonia"){
    dataset5 <- dataset %>% filter(Effect.measure == "OR")%>%
      filter(Categorized.class == "Lower Respiratory") 
    dataset55 <- dataset5 %>% 
      filter(Outcome.variable == "Pneumonia")
    forest(dataset55$Effect.measure.1, ilab.pos = 4, ci.lb = dataset55$Lower, ci.ub = dataset55$Upper, refline = 1, slab = paste(dataset55$Refid, dataset55$paperInfo),xlim = c(-8,5),ilab.xpos=c(-6,-4.3,-1.5), ilab=cbind(dataset55$Outcome.variable, dataset55$Exposure.measure, dataset55$ROB_confounding_paige))
    text(c(-7, -5,-3.3,-1), 9.5, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(5,                  9.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Wheezing without a cold"){
    dataset5 <- dataset %>% filter(Effect.measure == "OR")%>%
      filter(Categorized.class == "Lower Respiratory") 
    dataset56 <- dataset5 %>% 
      filter(Outcome.variable == "Wheezing without a cold")
    forest(dataset56$Effect.measure.1, ilab.pos = 4, ci.lb = dataset56$Lower, ci.ub = dataset56$Upper, refline = 1, slab = paste(dataset56$Refid, dataset56$paperInfo),xlim = c(-15, 5),ilab.xpos=c(-12, -8, -2), ilab=cbind(dataset56$Outcome.variable, dataset56$Exposure.measure, dataset56$ROB_confounding_paige))
    text(c(-13.5, -10.5, -6, -1.5), 2.3, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(5,                  2.3, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Body == "Other" & input$Outcome == "All"){
    dataset6 <- dataset %>% filter(Effect.measure == "OR") %>% 
      filter(Categorized.class == "Antimicrobial resistance" | Categorized.class == "Other") 
    forest(dataset6$Effect.measure.1, ilab.pos = 4, clim = c(0, 10),ci.lb = dataset6$Lower, ci.ub = dataset6$Upper, refline = 1, slab = paste(dataset6$Refid, dataset6$paperInfo),xlim = c(-20,9),ilab.xpos=c(-16.3,-10,-1), ilab=cbind(dataset6$Outcome.variable, dataset6$Exposure.measure, dataset6$ROB_confounding_paige))
    text(c(-18, -13.3,-8,-0.7), 21, c("Refid and Author","Outcome Variable ", "Exposure Measure", "Confounding"))
    text(12,                  21, "Odds Ratio [95% CI]", pos=2)}

  
  })

## Plot
output$forestROBPlot <- renderPlot({ 
    
 #  if(is.null(input$goButton) || input$goButton==0) return()
    df <- db()
    if(nrow(df) == 0) return(NULL)
    
    isContainExperimt <- isExperimt()
    measure_method <- measureMethod()
    study_type <- studyType()
    
    xlogScale <- FALSE
    
    if (measure_method == "OR") {
      xlab = "Odds ratio (95% confidence interval)"
      xlogScale <- TRUE
    } else if (measure_method == "beta") {
      xlab = "Beta (95% confidence interval)"
    } else if (measure_method == "Mean difference") {
      xlab = "Mean difference"
    } else if (measure_method == "PR") {
      xlab = "Prevalence ratio"
      xlogScale <- TRUE
    } else if ( measure_method == "beta p value") {
      xlab = "Beta p value"
    } else if (measure_method == "OR p value") {
      xlab = "Odds ratio p value"
    }
    
    if (!isContainExperimt) {
      ## include Experiment 
      if ("Experimental studies" %in% study_type) {
        ## only experimental study 
        experimtDf <- getOnlyExperimtDB()
        auxForPlot <-   getInputsForForestplot(experimtDf, measure_method, 7)
        ROBNms <-  c("Random sequence generation", "Allocation concealment", "Blinding owners/personnel", 
                     "Blinding outcome assessors", "Incomplete outcome data", "Selective reporting", 
                     "Other sources of bias")
        
        forest_ROB_plot(auxForPlot$M_first, auxForPlot$M_second,auxForPlot$labeltext, 
                        auxForPlot$mean, auxForPlot$lower, auxForPlot$upper,
                        auxForPlot$is.summary, xlab, ROBNms, measure_method,
                        xlog = xlogScale)
        
      } else {
        ## non-experiment study 
        auxForPlot <- getInputsForForestplot(df, measure_method, 6)
        ROBNms <-  c("Confounding", "Selection of participants", "Measurement of exposures/interventions", 
                     "Missing data", "Measurement of outcome", "Selection of reported result")   
        forest_ROB_plot(auxForPlot$M_first, auxForPlot$M_second,auxForPlot$labeltext, 
                        auxForPlot$mean, auxForPlot$lower, auxForPlot$upper,
                        auxForPlot$is.summary, xlab, ROBNms,measure_method,
                        xlog = xlogScale)
      }
    } else {
      ## mixed case 
      currentStudyType = unique(df$studyType)
      experimeIdx = which(df$studyType == "Experimental studies")
      auxForPlot <- getInputsForForestplot(df, measure_method, 6)
      ROBNms <-  c("Confounding", "Selection of participants", "Measurement of exposures/interventions", 
                   "Missing data", "Measurement of outcome", "Selection of reported result")   
      forest_ROB_plot(auxForPlot$M_first, auxForPlot$M_second,auxForPlot$labeltext, 
                      auxForPlot$mean, auxForPlot$lower, auxForPlot$upper,
                      auxForPlot$is.summary, xlab, ROBNms, measure_method,
                      xlog = xlogScale)

    }
  }
# , height = function(){ifelse(nrow(db()) < numRow, heightDefault, maxHeight)}
)

## dynamically input the width and height for plot 
output$forestROBPlot2 <- renderUI({
  plotOutput("forestROBPlot", 
             width = paste0(input$width, "%"),
             height = input$height)
})
# 
# plotInput <- reactive({
# #  if(input$returnpdf){
#    plotOutput("forestROBPlot", 
#                width = paste0(input$width, "%"),
#                height = input$height)
# #  }
# })
# 
# output$forestROBPlot2 <- renderUI({
#    plotInput()
# })
# 
# ## save the plot 
# output$downloadForestROBPlot <- downloadHandler(
#   filename <- function(){
#     'plot.pdf'
#   },  
#   content <- function(file){
#     file.copy("plot.pdf", file)
#   }
# )
###################### data summuary panel #######################
output$distTable <- DT::renderDataTable({
    
    input$tabBut
    
    if(nrow(db()) == 0) return(NULL)
    
    isContainExperimt <- isExperimt()
    study_type <- studyType()
    flag <- 0
    if(!isContainExperimt){
      ## include Experiment 
      if("Experimental studies" %in% study_type){
        ## only experimental study 
        flag <- 1
        aux <- as.data.frame(getOnlyExperimtDB())
        db <- getROBClass_Expr(aux)
      }else{
        ## non-experimental studies 
        aux <- as.data.frame(db())
        db <- getROBClass_nonExpr(aux)
      }
    }else{
      ## mixing case
      aux <- as.data.frame(db())
      db <- aux
      db <- getROBClass_nonExpr(aux)
    }

    ## replace studyID with associated paper info  
    RefNms = rep("", nrow(db))
    for(i in 1:nrow(db)){
      current_id = db$studyID[i]
      refNms = aux  %>% 
               filter(studyID == current_id) %>% 
               select(paperInfo) %>%
               unique() %>%
               .[, "paperInfo"]     
      RefNms[i] <- gsub("\\,", ";", toString(refNms))
    }
    
    db$studyID <- RefNms
    if(flag == 1) {    
      colnames(db) <- c("Study(s)",
                        "Risk of Bias class",
                        "Study Type",
                        "Rationale allocation method",
                        "Rationale allocation concealment",
                        "Rationale blinding owners/personnel",
                        "Rationale blinding outcome assessors",
                        "Rationale incomplete outcome data",
                        "Rationale selective reporting",
                        "Rationale other sources of bias")
    }else{
      colnames(db) <- c("Study(s)",
                        "Risk of Bias class",
                        "Study Type",
                        "Due to confounding",
                        "Due to selection",
                        "Due to measurement of exposure/intervention",
                        "Due to missing data",
                        "Due to measurement of outcome",
                        "Support for judement",
                        "Overall ROB")
    }
    rownames(db) <- NULL
    return(db)
}, option = list(
                 bAutoWidth = TRUE, 
                 sScroll = T,
                 sScrollX = '200px',
                 sScrollY = '800px',
                 scrollCollapse = TRUE,
                 iDisplayLength = 10,
                 xtable.include.rownames = F))


output$exprimetROBPlot <- renderPlot({
  ## if experiment study mixed with other type of studies, plot ROB of experiment study seperately 
  input$experimentGoButton
  
  forest_ROB_plot_expriment()
  
})

## This is Revman Plot. Need to be automated.
output$bias <- renderPlot({
  library(ggplot2)
  library(tidyr)
  rob <- read_xlsx("../datasets/cafo.xlsx", sheet = "Risk of bias")
  rob <- as.data.frame(rob)
  
  ## Risk of Bias RavMan Table
  
  r2 <- rob %>% select(Refid, ROB_confounding_paige, ROB_selection_paige, ROB_measurementExposure_paige, ROB_missingData_paige, ROB_measureOutcome_paige,ROB_SelectionofReportedResult_paige, ROB_overall_paige)
  names(r2) <- c("Refid", "Confounding", "Selection", "Measurement of Exposure", "Missing Data", "Measurement of Outcome", "Selection of Report", "Overall")
  r22 <- r2 %>% gather(key = `Type of Bias`, value = Bias, Confounding:Overall)
  r22$Bias <- as.factor(r22$Bias)
  r22$Bias <- factor(r22$Bias, levels = c("Critical","Serious", "Moderate", "Low", "Uncertain"))
  r22$`Type of Bias` <- factor(r22$`Type of Bias`, levels = c("Selection of Report", "Selection", "Missing Data", "Measurement of Outcome", "Measurement of Exposure", "Confounding", "Overall"))
  color_table <- tibble(
    Bias = c("Critical","Serious", "Moderate", "Low", "Uncertain"),
    Color = c("firebrick2", "darkorange","yellow", "green2", "floralwhite")
  )
  ## Code NAs
  r22[is.na(r22)] <- "Uncertain"
  r22 %>% ggplot(aes(x = `Type of Bias`, fill = Bias)) + geom_bar(position = "fill") + scale_fill_manual(values = color_table$Color) +coord_flip()+ scale_x_discrete(labels = c("Overall", "Confounding", "Measurement of Exposure", "Measurement of Outcome", "Missing Data", "Selection", "Selection of Report")) + ylab("Ratio")
})

## Reference page
output$references <- renderUI({
  HTML(markdown::markdownToHTML(knit("../scripts/references.Rmd", quiet = TRUE)))
  
  
})



}))


