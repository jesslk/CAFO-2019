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
  HTML(markdown::markdownToHTML(knit("../scripts/gettingstarted.Rmd", quiet = TRUE)))
  
  
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
## Should be automated
selection <- data.frame("Body" = c(rep("Nervous", 4),rep("Upper Respiratory", 4), rep("Lower Respiratory", 7), rep("Other", 3) ), Outcome = c("All","Psychological Distress", "Eye irritation", "Difficulty hearing", "All","Allergic rhinitis", "Sensitization against ubiquitous allergens", "Specific IgE to Common Allergens >0.35IU/mL", "All","Asthma", "Bronchial Hyperresponsiveness to Methacholine (BHM)", "Chronic obstructive pulmonary disease (COPD)", "Physician-Diagnosed Asthma", "Pneumonia", "Wheezing without a cold", "All","Activity limitations from asthma symptoms", "Other infectious disease"))
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
  
  if(input$Body == "Nervous"){
    dataset3 <- dataset %>% 
      filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    ## Again, recoding variables. Could be changed in the future?
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry  grouchy  or bad-tempered"] <- "Angry grouchy or bad-tempered"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry grouchy or bad-tempered"] <- "Psychological Distress"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy  blue  or unhappy"] <- "Gloomy blue or unhappy"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy blue or unhappy"] <- "Gloomy blue or unhappy"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Itching eyes"] <- "Eye irritation"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Burning eyes"] <- "Eye irritation"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Nervous or anxious"] <- "Psychological Distress"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Stressed or annoyed"] <- "Psychological Distress"
    ## Fit the linear model. I calculated sd manually 
    res <- rma(data = dataset3, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
   
    ## Below is the actual forest plot
    forest(res, xlim=c(-8, 4), at=log(c(0.25, 1, 3, 7)), addfit = F, atransf=exp,
           ilab=cbind(dataset3$Outcome.variable, dataset3$Exposure.measure, dataset3$ROB_confounding_paige),
           ilab.xpos=c(-7,-5.3,-3), ilab.pos = 4 , cex=0.75, ylim=c(-1, 48),
           order=order(dataset3$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-7.5, -6.3,-4.3,-3), 27, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.8,                  27, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Body == "Nervous" & input$Outcome == "All" ){
    dataset3 <- dataset %>% 
      filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry  grouchy  or bad-tempered"] <- "Angry grouchy or bad-tempered"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry grouchy or bad-tempered"] <- "Psychological Distress"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy  blue  or unhappy"] <- "Gloomy blue or unhappy"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy blue or unhappy"] <- "Gloomy blue or unhappy"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Itching eyes"] <- "Eye irritation"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Burning eyes"] <- "Eye irritation"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Nervous or anxious"] <- "Psychological Distress"
    dataset3$Outcome.variable[dataset3$Outcome.variable=="Stressed or annoyed"] <- "Psychological Distress"
    res <- rma(data = dataset3, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-8, 4), at=log(c(0.25, 1, 3, 7)), addfit = F, atransf=exp,
           ilab=cbind(dataset3$Outcome.variable, dataset3$Exposure.measure, dataset3$ROB_confounding_paige),
           ilab.xpos=c(-7,-5.3,-3), ilab.pos = 4 , cex=0.75, 
           #ylim=c(0,48),
           order=order(dataset3$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-7.5, -6.3,-4.3,-3), 47, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.8,                  47, "Odds Ratio [95% CI]", pos=2)
    text( 0,                   0 , "Increasing")}
  
  if(input$Outcome == "Psychological Distress"){
    dataset36 <- dataset3 %>% 
      filter(Outcome.variable == "Psychological Distress")
    res <- rma(data = dataset36, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-8, 4), at=log(c(0.25, 0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset36$Outcome.variable, dataset36$Exposure.measure, dataset36$ROB_confounding_paige),
           ilab.xpos=c(-6.5,-4.8,-2), ilab.pos = 4 , cex=1, #ylim=c(0,18),
           order=order(dataset36$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-7.6, -6.4,-4.4,-1.7), 17, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.8,                  17, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Eye irritation"){
    dataset34 <- dataset3 %>% 
      filter(Outcome.variable == "Eye irritation")
    res <- rma(data = dataset34, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-6, 2), at=log(c(0.25, 0.5, 1, 2.5, 5)), addfit = F, atransf=exp,
           ilab=cbind(dataset34$Outcome.variable, dataset34$Exposure.measure, dataset34$ROB_confounding_paige),
           ilab.xpos=c(-5,-3.8,-2), ilab.pos = 4 , cex=1, #ylim=c(-1, 21),
           order=order(dataset34$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-5.6, -4.7,-3.5,-1.7), 19.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(1.8,                  19.5, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Difficulty hearing"){
    dataset35 <- dataset3 %>% 
      filter(Outcome.variable == "Difficulty hearing")
    res <- rma(data = dataset35, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-8, 4), at=log(c(0.25, 0.5, 1, 2.5, 5)), addfit = F, atransf=exp,
           ilab=cbind(dataset35$Outcome.variable, dataset35$Exposure.measure, dataset35$ROB_confounding_paige),
           ilab.xpos=c(-6.5,-4.8,-2), ilab.pos = 4 , cex=1, #ylim=c(-1, 10),
           order=order(dataset35$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-7.6, -6.4,-4.4,-1.7), 8.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.6,                  8.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Body == "Upper Respiratory"){
    dataset4 <- dataset %>% 
      filter(Categorized.class == "Upper Respiratory" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    res <- rma(data = dataset4, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
           ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige),
           ilab.xpos=c(-5.8,-3.5,-1), cex=0.85, #ylim=c(0, 30),
           order=order(dataset4$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-6.5, -5.5,-3.2,-0.6), 29.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.9,                  29.5, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Body == "Upper Respiratory" & input$Outcome == "All"){
    dataset4 <- dataset %>% 
      filter(Categorized.class == "Upper Respiratory" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    res <- rma(data = dataset4, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
           ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige),
           ilab.xpos=c(-5.8,-3.5,-1), cex=0.85, #ylim=c(0, 30),
           order=order(dataset4$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-6.5, -5.5,-3.2,-0.6), 29.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.9,                  29.5, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Allergic rhinitis"){
    dataset41 <- dataset4 %>% 
      filter(Outcome.variable == "Allergic rhinitis")
    res <- rma(data = dataset41, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
           ilab=cbind(dataset41$Outcome.variable, dataset41$Exposure.measure, dataset41$ROB_confounding_paige),
           ilab.xpos=c(-5.8,-3.5,-1), cex=0.85, #ylim=c(0, 22),
           order=order(dataset41$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-6.5, -5.5,-3.2,-0.6), 20.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.9,                  20.5, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Sensitization against ubiquitous allergens"){
    dataset42 <- dataset4 %>% 
      filter(Outcome.variable == "Sensitization against ubiquitous allergens")
    res <- rma(data = dataset42, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
           ilab=cbind(dataset42$Outcome.variable, dataset42$Exposure.measure, dataset42$ROB_confounding_paige),
           ilab.xpos=c(-5.8,-3.5,-1), cex=0.85, #ylim=c(0, 3.5),
           order=order(dataset42$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-6.5, -5.5,-3.2,-0.6), 2.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.9,                  20.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Specific IgE to Common Allergens >0.35IU/mL"){
    dataset43 <- dataset4 %>% 
      filter(Outcome.variable == "Specific IgE to Common Allergens >0.35IU/mL")
    res <- rma(data = dataset43, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
           ilab=cbind(dataset43$Outcome.variable, dataset43$Exposure.measure, dataset43$ROB_confounding_paige),
           ilab.xpos=c(-6,-3.5,-1), cex=0.85, #ylim=c(0, 9),
           order=order(dataset43$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-6.5, -5.7,-3.2,-0.6), 7.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.9,                  7.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  
  
  if(input$Body == "Lower Respiratory"){
    dataset5 <- dataset %>% 
      filter(Categorized.class == "Lower Respiratory" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Chronic obstructive pulmonary disease (COPD)"] <- "COPD"
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Bronchial Hyperresponsiveness to Methacholine"] <- "BHM"
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Wheezing without cold"] <- "Wheezing without a cold"
    
    res <- rma(data = dataset5, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset5$Outcome.variable, dataset5$Exposure.measure, dataset5$ROB_confounding_paige),
           ilab.xpos=c(-4,-2.5,-1), cex=0.70, #ylim=c(0, 65),
           order=order(dataset5$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.6,-2,-0.7), 64, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.5,                  64, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Body == "Lower Respiratory" & input$Outcome == "All"){
    dataset5 <- dataset %>% 
      filter(Categorized.class == "Lower Respiratory" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Chronic obstructive pulmonary disease (COPD)"] <- "COPD"
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Bronchial Hyperresponsiveness to Methacholine"] <- "BHM"
    dataset5$Outcome.variable[dataset5$Outcome.variable=="Wheezing without cold"] <- "Wheezing without a cold"
    
    res <- rma(data = dataset5, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset5$Outcome.variable, dataset5$Exposure.measure, dataset5$ROB_confounding_paige),
           ilab.xpos=c(-4,-2.5,-1), cex=0.70, #ylim=c(0, 65),
           order=order(dataset5$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.6,-2,-0.7), 64, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.5,                  64, "Odds Ratio [95% CI]", pos=2)}
  
  
  
  if(input$Outcome == "Asthma"){
    dataset51 <- dataset5 %>% 
      filter(Outcome.variable == "Asthma")
    res <- rma(data = dataset51, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset51$Outcome.variable, dataset51$Exposure.measure, dataset51$ROB_confounding_paige),
           ilab.xpos=c(-4,-3.3,-1), cex=1, #ylim=c(0, 26),
           order=order(dataset51$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.8,-2.8,-0.7), 24.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3.5,                  24.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Bronchial Hyperresponsiveness to Methacholine (BHM)"){
    dataset52 <- dataset5 %>% 
      filter(Outcome.variable == "BHM")
    res <- rma(data = dataset52, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset52$Outcome.variable, dataset52$Exposure.measure, dataset52$ROB_confounding_paige),
           ilab.xpos=c(-4,-3.3,-1), cex=1, #ylim=c(0,8.3),
           order=order(dataset52$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.8,-2.8,-0.7), 6.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3,                  6.5, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Chronic obstructive pulmonary disease (COPD)"){
    dataset53 <- dataset5 %>% 
      filter(Outcome.variable == "COPD")
    res <- rma(data = dataset53, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset53$Outcome.variable, dataset53$Exposure.measure, dataset53$ROB_confounding_paige),
           ilab.xpos=c(-4,-3.3,-1), cex=1, #ylim=c(0,15),
           order=order(dataset53$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.8,-2.8,-0.7), 13.3, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3,                  13.3, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Physician-Diagnosed Asthma"){
    dataset54 <- dataset5 %>% 
      filter(Outcome.variable == "Physician-Diagnosed Asthma")
    res <- rma(data = dataset54, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset54$Outcome.variable, dataset54$Exposure.measure, dataset54$ROB_confounding_paige),
           ilab.xpos=c(-4,-2.4,-1), cex=1, #ylim=c(0,8.6),
           order=order(dataset54$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.3,-2.1,-0.7), 7.2, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3,                  7.2, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Outcome == "Pneumonia"){
    dataset55 <- dataset5 %>% 
      filter(Outcome.variable == "Pneumonia")
    res <- rma(data = dataset55, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset55$Outcome.variable, dataset55$Exposure.measure, dataset55$ROB_confounding_paige),
           ilab.xpos=c(-4,-2.4,-1), cex=1, #ylim=c(0,11),
           order=order(dataset55$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.7,-2.1,-0.7), 9.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3,                  9.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Wheezing without a cold"){
    dataset56 <- dataset5 %>% 
      filter(Outcome.variable == "Wheezing without a cold")
    res <- rma(data = dataset56, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    par(mar=c(4,4,1,2))
    forest(res, xlim=c(-5, 3), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F, atransf=exp,
           ilab=cbind(dataset56$Outcome.variable, dataset56$Exposure.measure, dataset56$ROB_confounding_paige),
           ilab.xpos=c(-4,-2.8,-1), cex=1, #ylim=c(0,9.5),
           order=order(dataset56$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    
    text(c(-4.7, -3.7,-2.9,-0.7), 8.2, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(3,                  8.2, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Body == "Other"){
    dataset6 <- dataset %>% 
      filter(Categorized.class == "Live style" | Categorized.class == "Other" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    dataset6$Outcome.variable[dataset6$Outcome.variable=="Activity limitations in past year as a result of asthma symptoms"] <- "Activity limitations from asthma symptoms"
    dataset6$Exposure.measure[dataset6$Exposure.measure=="Livestock Odor Reported Outside or Inside School Building Versus No Reported Odor"] <- "Livestock oder reported VS None"
    res <- rma(data = dataset6, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-19, 2), ilab.pos = 4, at=log(c(0.5, 1, 2.5, 5, 15)), addfit = F,atransf=exp,
           ilab=cbind(dataset6$Outcome.variable, dataset6$Exposure.measure, dataset6$ROB_confounding_paige),
           ilab.xpos=c(-16,-10.5,-2.6), cex=1, #ylim=c(-1, 13),
           order=order(dataset6$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-18, -14.6,-8,-2.5), 12, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(12,                  12, "Odds Ratio [95% CI]", pos=2)}
  
  if(input$Body == "Other" & input$Outcome == "All"){
    dataset6 <- dataset %>% 
      filter(Categorized.class == "Live style" | Categorized.class == "Other" & Effect.measure == "OR") %>%
      select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
    dataset6$Outcome.variable[dataset6$Outcome.variable=="Activity limitations in past year as a result of asthma symptoms"] <- "Activity limitations from asthma symptoms"
    dataset6$Exposure.measure[dataset6$Exposure.measure=="Livestock Odor Reported Outside or Inside School Building Versus No Reported Odor"] <- "Livestock oder reported VS None"
    res <- rma(data = dataset6, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-19, 2), ilab.pos = 4, at=log(c(0.5, 1, 2.5, 5, 15)), addfit = F,atransf=exp,
           ilab=cbind(dataset6$Outcome.variable, dataset6$Exposure.measure, dataset6$ROB_confounding_paige),
           ilab.xpos=c(-16,-10.5,-2.6), cex=1, #ylim=c(-1, 13),
           order=order(dataset6$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-18, -14.6,-8,-2.5), 12, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(12,                  12, "Odds Ratio [95% CI]", pos=2)}
  
  
  
  if(input$Outcome == "Activity limitations from asthma symptoms"){
    dataset61 <- dataset6 %>% 
      filter(Outcome.variable == "Activity limitations from asthma symptoms") 
    res <- rma(data = dataset61, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-22, 5), ilab.pos = 4, at=log(c(0.5, 1, 1.5, 2.5, 3)), addfit = F,atransf=exp,
           ilab=cbind(dataset61$Outcome.variable, dataset61$Exposure.measure, dataset61$ROB_confounding_paige),
           ilab.xpos=c(-19,-12.5,-4), cex=1, #ylim=c(-1, 4.4),
           order=order(dataset61$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-20.8, -17,-12,-3), 2.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(4.7,                  2.5, "Odds Ratio [95% CI]", pos=2)}
  
  
  if(input$Outcome == "Other infectious disease"){
    dataset62 <- dataset6 %>% 
      filter(Outcome.variable == "Other infectious disease") 
    res <- rma(data = dataset62, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
    forest(res, xlim=c(-20, 12), ilab.pos = 4, at=log(c(0.5, 1, 5, 10)), addfit = F,atransf=exp,
           ilab=cbind(dataset62$Outcome.variable, dataset62$Exposure.measure, dataset62$ROB_confounding_paige),
           ilab.xpos=c(-17.5,-12.5,-4), cex=1, #ylim=c(-1, 10.7),
           order=order(dataset62$ROB_confounding_paige),
           xlab="Odds Ratio", mlab="", psize = 1)
    text(c(-19, -16.6,-12,-3.8), 9, c("ID and Year","Outcome", "Exposure", "Confounding"))
    text(11,                  9, "Odds Ratio [95% CI]", pos=2)}
  
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


