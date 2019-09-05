##Nervous system

dataset3 <- dataset %>% 
  filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress" & Effect.measure == "OR") %>%
  select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
dataset3$Outcome.variable[dataset3$Outcome.variable=="Angry  grouchy  or bad-tempered"] <- "Angry grouchy or bad-tempered"
dataset3$Outcome.variable[dataset3$Outcome.variable=="Gloomy  blue  or unhappy"] <- "Gloomy blue or unhappy"
dataset3$Outcome.variable[dataset3$Outcome.variable=="Itching eyes"] <- "Eye irritation"
dataset3$Outcome.variable[dataset3$Outcome.variable=="Burning eyes"] <- "Eye irritation"
dataset3$Outcome.variable[dataset3$Outcome.variable=="Nervous or anxious"] <- "Psychological Distress"
dataset3$Outcome.variable[dataset3$Outcome.variable=="Stressed or annoyed"] <- "Psychological Distress"
res <- rma(data = dataset3, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
forest(res, xlim=c(-8, 4), at=log(c(0.25, 1, 3, 7)), addfit = F, atransf=exp,
       ilab=cbind(dataset3$Outcome.variable, dataset3$Exposure.measure, dataset3$ROB_confounding_paige),
       ilab.xpos=c(-7,-5.3,-3), ilab.pos = 4 , cex=0.75, 
       order=order(dataset3$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)
text(c(-7.5, -6.3,-4.3,-3), nrow(dataset3)*0.87, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(3.8,                  nrow(dataset3)*0.87, "Odds Ratio [95% CI]", pos=2)

## Upper Respiratory

dataset4 <- dataset %>% 
  filter(Categorized.class == "Upper Respiratory" & Effect.measure == "OR") %>%
  select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
res <- rma(data = dataset4, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
par(mar=c(4,4,1,2))
forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
       ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige),
       ilab.xpos=c(-5.9,-3.5,-1), cex=0.95,
       order=order(dataset4$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)

text(c(-6.5, -5.5,-3.2,-0.6), nrow(dataset4)*0.9, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(3.9,                  nrow(dataset4)*0.9, "Odds Ratio [95% CI]", pos=2)


## Lower Respiratory

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
       ilab.xpos=c(-4,-2.5,-1), cex=0.70, ylim=c(0, 65),
       order=order(dataset5$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)

text(c(-4.7, -3.6,-2,-0.7), 64, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(3.5,                  64, "Odds Ratio [95% CI]", pos=2)

## Other

dataset6 <- dataset %>% 
  filter(Categorized.class == "Live style" | Categorized.class == "Other" & Effect.measure == "OR") %>%
  select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
dataset6$Outcome.variable[dataset6$Outcome.variable=="Activity limitations in past year as a result of asthma symptoms"] <- "Activity limitations from asthma symptoms"
dataset6$Exposure.measure[dataset6$Exposure.measure=="Livestock Odor Reported Outside or Inside School Building Versus No Reported Odor"] <- "Livestock oder reported VS None"
res <- rma(data = dataset6, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
forest(res, xlim=c(-19, 2), ilab.pos = 4, at=log(c(0.5, 1, 2.5, 5, 15)), addfit = F,atransf=exp,
       ilab=cbind(dataset6$Outcome.variable, dataset6$Exposure.measure, dataset6$ROB_confounding_paige),
       ilab.xpos=c(-16,-10.5,-2.6), cex=0.75, ylim=c(-1, 13),
       order=order(dataset6$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)
text(c(-18, -14.6,-8,-2.5), 12, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(12,                  12, "Odds Ratio [95% CI]", pos=2)



### PRactice more

## Update choices
selection <- data.frame("Body" = c(rep("Nervous", 4),rep("Upper Respiratory", 4), rep("Lower Respiratory", 7), rep("Other", 3) ), Outcome = c("All","Psychological Distress", "Eye irritation", "Difficulty hearing", "All","Allergic rhinitis", "Sensitization against ubiquitous allergens", "Specific IgE to Common Allergens >0.35IU/mL", "All","Asthma", "Bronchial Hyperresponsiveness to Methacholine (BHM)", "Chronic obstructive pulmonary disease (COPD)", "Physician-Diagnosed Asthma", "Pneumonia", "Wheezing without a cold", "All","Activity limitations from asthma symptoms", "Other infectious disease"))

## Outcome reactive


if(input$Outcome == "Psychological Distress"){
  dataset36 <- dataset3 %>% 
    filter(Outcome.variable == "Psychological Distress")
  res <- rma(data = dataset36, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
  forest(res, xlim=c(-8, 4), at=log(c(0.25, 0.5, 1, 5, 10)), addfit = F, atransf=exp,
         ilab=cbind(dataset36$Outcome.variable, dataset36$Exposure.measure, dataset36$ROB_confounding_paige),
         ilab.xpos=c(-6.5,-4.8,-2), ilab.pos = 4 , cex=1, ylim=c(-1, 18),
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
         ilab.xpos=c(-5,-3.8,-2), ilab.pos = 4 , cex=1, ylim=c(-1, 21),
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
         ilab.xpos=c(-6.5,-4.8,-2), ilab.pos = 4 , cex=1,
         order=order(dataset35$ROB_confounding_paige),
         xlab="Odds Ratio", mlab="", psize = 1)
  text(c(-7.6, -6.4,-4.4,-1.7), nrow(dataset35)-2, c("ID and Year","Outcome", "Exposure", "Confounding"))
  text(3.6,                  nrow(dataset35)-2, "Odds Ratio [95% CI]", pos=2)}

## Upper Respiratory

##All

if(input$Body == "Upper Respiratory" & input$Outcome == "All"){
  dataset4 <- dataset %>% 
    filter(Categorized.class == "Upper Respiratory" & Effect.measure == "OR") %>%
    select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
  res <- rma(data = dataset4, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
  par(mar=c(4,4,1,2))
  forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
         ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige),
         ilab.xpos=c(-5.8,-3.5,-1), cex=0.85,
         order=order(dataset4$ROB_confounding_paige),
         xlab="Odds Ratio", mlab="", psize = 1)
  
  text(c(-6.5, -5.5,-3.2,-0.6), nrow(dataset4)-2, c("ID and Year","Outcome", "Exposure", "Confounding"))
  text(3.9,                  nrow(dataset4)-2, "Odds Ratio [95% CI]", pos=2)}

if(input$Outcome == "Allergic rhinitis"){
  dataset41 <- dataset4 %>% 
    filter(Outcome.variable == "Allergic rhinitis")
  res <- rma(data = dataset41, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
  par(mar=c(4,4,1,2))
  forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
         ilab=cbind(dataset41$Outcome.variable, dataset41$Exposure.measure, dataset41$ROB_confounding_paige),
         ilab.xpos=c(-5.8,-3.5,-1), cex=0.85, ylim=c(0, 22),
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
         ilab.xpos=c(-5.8,-3.5,-1), cex=0.85, ylim=c(0, 3.5),
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
         ilab.xpos=c(-6,-3.5,-1), cex=0.85, ylim=c(0, 9),
         order=order(dataset43$ROB_confounding_paige),
         xlab="Odds Ratio", mlab="", psize = 1)
  
  text(c(-6.5, -5.7,-3.2,-0.6), 7.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
  text(3.9,                  7.5, "Odds Ratio [95% CI]", pos=2)}


## Lower 

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
         ilab.xpos=c(-4,-2.5,-1), cex=0.70, ylim=c(0, 65),
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
         ilab.xpos=c(-4,-3.3,-1), cex=0.70, ylim=c(0, 26),
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
         ilab.xpos=c(-4,-3.3,-1), cex=0.80, ylim=c(0,8.3),
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
         ilab.xpos=c(-4,-3.3,-1), cex=0.80, ylim=c(0,15),
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
         ilab.xpos=c(-4,-2.4,-1), cex=0.80, ylim=c(0,8.6),
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
         ilab.xpos=c(-4,-2.4,-1), cex=0.80, ylim=c(0,11),
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
         ilab.xpos=c(-4,-2.8,-1), cex=0.80, ylim=c(0,9.5),
         order=order(dataset56$ROB_confounding_paige),
         xlab="Odds Ratio", mlab="", psize = 1)
  
  text(c(-4.7, -3.7,-2.9,-0.7), 8.2, c("ID and Year","Outcome", "Exposure", "Confounding"))
  text(3,                  8.2, "Odds Ratio [95% CI]", pos=2)}

## Other

if(input$Body == "Other" & input$Outcome == "All"){
  dataset6 <- dataset %>% 
    filter(Categorized.class == "Live style" | Categorized.class == "Other" & Effect.measure == "OR") %>%
    select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
  dataset6$Outcome.variable[dataset6$Outcome.variable=="Activity limitations in past year as a result of asthma symptoms"] <- "Activity limitations from asthma symptoms"
  dataset6$Exposure.measure[dataset6$Exposure.measure=="Livestock Odor Reported Outside or Inside School Building Versus No Reported Odor"] <- "Livestock oder reported VS None"
  res <- rma(data = dataset6, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
  forest(res, xlim=c(-19, 2), ilab.pos = 4, at=log(c(0.5, 1, 2.5, 5, 15)), addfit = F,atransf=exp,
         ilab=cbind(dataset6$Outcome.variable, dataset6$Exposure.measure, dataset6$ROB_confounding_paige),
         ilab.xpos=c(-16,-10.5,-2.6), cex=1, ylim=c(-1, 13),
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
         ilab.xpos=c(-19,-12.5,-4), cex=1, ylim=c(-1, 4.4),
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
         ilab.xpos=c(-17.5,-12.5,-4), cex=1, ylim=c(-1, 10.7),
         order=order(dataset62$ROB_confounding_paige),
         xlab="Odds Ratio", mlab="", psize = 1)
  text(c(-19, -16.6,-12,-3.8), 9, c("ID and Year","Outcome", "Exposure", "Confounding"))
  text(11,                  9, "Odds Ratio [95% CI]", pos=2)}



## Multiple

dataset2 <- within(dataset, 
                   paperInfo <- factor(paperInfo, 
                                       levels=names(sort(table(paperInfo), 
                                                         increasing=TRUE))))

dataset2$Effect.measure[dataset2$Effect.measure=="beta"] <- "Beta"
dataset2$Effect.measure[dataset2$Effect.measure=="beta p value"] <- "Beta P Value"
dataset2$Effect.measure[dataset2$Effect.measure=="OR"] <- "Odds Ratio"
dataset2$Effect.measure[dataset2$Effect.measure=="OR p value"] <- "Odds Ratio P Value"
dataset2$Effect.measure[dataset2$Effect.measure=="PR"] <- "Prevalence Ratio"
dataset2 %>% ggplot(aes(x = paperInfo, fill = Effect.measure)) + geom_bar() + coord_flip() + geom_text(stat='count',aes(label=..count..)) +
  labs(x = "", fill = "Effect Measure") + ylab("Number of Reported Outcomes")



