
library(readr)
hsptldata <- read_delim("california-hospital-performance-ratings-for-coronary-artery-bypass-graft-cabg-surgery-2011-2017.csv", "," , escape_double = FALSE, trim_ws = TRUE)



#checking summary
summary(object=hsptldata)


#removing na values
hsptldata.cleaned <- na.omit(hsptldata)


#checking summary after removing na values
summary(hsptldata.cleaned)
View(hsptldata.cleaned)






#converting variables into as factors

hsptldata.cleaned$`Performance Measure` <- as.factor(hsptldata.cleaned$`Performance Measure`)






hsptldata.cleaned$`# of Adverse Events`<-as.factor(hsptldata.cleaned$`# of Adverse Events`)




hsptldata.cleaned$`Performance Rating`<-as.factor(hsptldata.cleaned$`Performance Rating`)


summary(hsptldata.cleaned)
view(hsptldata.cleaned)

#Multiple Linear regression models

lmcases.1 <- lm(`Risk-adjusted Rate` ~ `# of Cases` ,data = hsptldata.cleaned)
summary(lmcases.1)






lmcases.2 <- lm(`Risk-adjusted Rate`~ `# of Cases` + County, data = hsptldata.cleaned)
summary(lmcases.2)



lmcases.3 <- lm(`Risk-adjusted Rate`~ `# of Cases` + County + `Performance Measure`, data = hsptldata.cleaned)
summary(lmcases.3)



lmcases.4 <- lm(`Risk-adjusted Rate` ~ `# of Cases` + County + `Performance Measure` + `# of Adverse Events` , data = hsptldata.cleaned)
summary(lmcases.4)

#Performing anova test to show the best fit model

anova(lmcases.1, lmcases.2, lmcases.3, lmcases.4)


#determining whether the association between # of Adverse Events and Risk-adjusted Rate varies significantly across different levels of Performance Measure or not.
lmcases.05 <- lm(`Risk-adjusted Rate` ~ `# of Cases` + County + `Performance Measure` +  `# of Adverse Events`+  `# of Adverse Events`*`Performance Measure`, data = hsptldata.cleaned)
summary(lmcases.05)


