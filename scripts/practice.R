##Nervous system

dataset3 <- dataset %>% 
  filter(Categorized.class == "Eye" | Categorized.class == "Otologic" | Categorized.class == "Psychological" | Categorized.class == "Stress" & Effect.measure == "OR") %>%
  select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
res <- rma(data = dataset3, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
forest(res, xlim=c(-8, 4), at=log(c(0.25, 1, 3, 7)), addfit = F, atransf=exp,
       ilab=cbind(dataset3$Outcome.variable, dataset3$Exposure.measure, dataset3$ROB_confounding_paige),
       ilab.xpos=c(-7,-5.3,-3), ilab.pos = 4 , cex=0.75, ylim=c(-1, 48),
       order=order(dataset3$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)
text(c(-7.5, -6.3,-4.3,-3), 47, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(3.8,                  47, "Odds Ratio [95% CI]", pos=2)

## Upper Respiratory

dataset4 <- dataset %>% 
  filter(Categorized.class == "Upper Respiratory" & Effect.measure == "OR") %>%
  select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
res <- rma(data = dataset4, yi = Effect.measure.1, vi = ((Effect.measure.1-Lower)/1.96)^2, slab=paste(Refid,paperYear, sep=",  "),measure = "OR", method = "REML")
par(mar=c(4,4,1,2))
forest(res, xlim=c(-7, 4), ilab.pos = 4 ,at=log(c(0.5, 0.8, 1, 4, 8)), addfit = F, atransf=exp,
       ilab=cbind(dataset4$Outcome.variable, dataset4$Exposure.measure, dataset4$ROB_confounding_paige),
       ilab.xpos=c(-5.9,-3.5,-1), cex=0.95, ylim=c(0, 30),
       order=order(dataset4$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)

text(c(-6.5, -5.5,-3.2,-0.6), 29.5, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(3.9,                  29.5, "Odds Ratio [95% CI]", pos=2)


## Lower Respiratory

dataset5 <- dataset %>% 
  filter(Categorized.class == "Lower Respiratory" & Effect.measure == "OR") %>%
  select(paperInfo, Refid, paperYear, Outcome.variable, Exposure.measure, ROB_confounding_paige, Effect.measure.1, Lower, Upper)
dataset5$Outcome.variable[dataset5$Outcome.variable=="Chronic obstructive pulmonary disease (COPD)"] <- "COPD"
dataset5$Outcome.variable[dataset5$Outcome.variable=="Bronchial Hyperresponsiveness to Methacholine"] <- "BHM"
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
forest(res, xlim=c(-19, 2), at=log(c(0.5, 1, 2.5, 5, 15)), addfit = F,atransf=exp,
       ilab=cbind(dataset6$Outcome.variable, dataset6$Exposure.measure, dataset6$ROB_confounding_paige),
       ilab.xpos=c(-14,-7,-2.6), cex=0.75, ylim=c(-1, 13),
       order=order(dataset6$ROB_confounding_paige),
       xlab="Odds Ratio", mlab="", psize = 1)
text(c(-18, -14.6,-8,-2.5), 12, c("ID and Year","Outcome", "Exposure", "Confounding"))
text(12,                  12, "Odds Ratio [95% CI]", pos=2)