setwd("~/Master/HS 2022/natural experiments")

#Libraries
library(ivreg)
library(haven)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)
library(reshape2)

#Import Data
data <- read.csv("oltmans_2011.csv", sep =";")
head(data)

#Descriptive Statistics
str(data)
summary(data)

# Boxplot Inequality 
gini <- data[c("lngini_w","lngini_b")]
gini$ID <- seq.int(nrow(gini))
data_long <- melt(gini, id.vars = "ID")
ggplot(data_long, aes(x = variable, y = exp(value))) +
  geom_boxplot()+
  ylim ( 0, 1) +
  ylab("Level of Inequality in Income") +
  xlab(" ")+
  scale_x_discrete(labels=c("White","Black")) 

#Boxplot Poverty
povrate <- data[c("povrate_w","povrate_b")]
povrate$ID <- seq.int(nrow(povrate))
pov_long <- melt(povrate, id.vars = "ID")
ggplot(pov_long, aes(x = variable, y = value)) +
  geom_boxplot()+
  ylim ( 0, 1) +
  ylab("Poverty Rate") +
  xlab(" ")+
  scale_x_discrete(labels=c("White","Black")) 

# Effect of Effect of 1990 dissimilarity index
povrate_w <-  lm(povrate_w ~ dism1990, data = data)
povrate_b <-  lm(povrate_b ~ dism1990, data = data)
lngini_w <-  lm(lngini_w ~ dism1990, data = data)
lngini_b <-  lm(lngini_b ~ dism1990, data = data)

stargazer(
  povrate_w, povrate_b, lngini_w, lngini_b,
  type = ,
  title = "html",
  colnames = FALSE,
  model.numbers = TRUE,
  dep.var.caption="Dependent variables:",
  dep.var.labels = c("Povrate white"," Povrate black","Inequality white", "Inequality black"),
  covariate.labels = c("Effect"),
  keep.stat = c("n","rsq","f"),
  notes.align = "l",
  omit.table.layout = "n", 
  out = "Effect of Effect of 1990 dissimilarity index.htm"
)

# Standardized Effect of 1990 dissimilarity index
firststage <- lm(dism1990 ~ herf + lenper, data = data)
summary(firststage)
data$dismhat <- predict(firststage)

secondstage_povrate_w <- lm(povrate_w~dismhat, data = data)
secondstage_povrate_b <- lm(povrate_b~dismhat, data = data)
secondstage_lngini_w <- lm(lngini_w~dismhat, data = data)
secondstage_lngini_b <- lm(lngini_b~dismhat, data = data)

stargazer(
  secondstage_povrate_w, secondstage_povrate_b, secondstage_lngini_w, secondstage_lngini_b,
  type = ,
  title = "html",
  colnames = FALSE,
  model.numbers = TRUE,
  dep.var.caption="Dependent variables:",
  dep.var.labels = c("Povrate white"," Povrate black","Inequality white", "Inequality black"),
  covariate.labels = c("Effect"),
  keep.stat = c("n","rsq","f"),
  notes.align = "l",
  omit.table.layout = "n", 
  out = "2SLS.htm"
)


# z-standardizing
data$dismhat <- (data$dismhat- mean(data$dismhat))/sd(data$dismhat)
sd(data$dism1990)

second_stand_povrate_w <- lm(povrate_w~dismhat_r, data = data)
second_stand_povrate_b <- lm(povrate_b~dismhat_r, data = data)
second_stand_gini_w <- lm(lngini_w~dismhat_r, data = data)
second_stand_gini_b <- lm(lngini_b~dismhat_r, data = data)

stargazer(
  second_stand_povrate_w, second_stand_povrate_b, second_stand_gini_w, second_stand_gini_b,
  type = ,
  title = "html",
  colnames = FALSE,
  model.numbers = TRUE,
  dep.var.caption="Dependent variables:",
  dep.var.labels = c("Povrate white"," Povrate black","Inequality white", "Inequality black"),
  covariate.labels = c("Effect"),
  keep.stat = c("n","rsq","f"),
  notes.align = "l",
  omit.table.layout = "n", 
  out = "2SLS Standardized Effect.htm"
)



#ivreg
iv_secondstage_povrate_w <- ivreg(povrate_w ~ dism1990 | herf+lenper, data = data)
iv_secondstage_povrate_b <- ivreg(povrate_b ~ dism1990 | herf+lenper, data = data)
iv_secondstage_lngini_w <- ivreg(lngini_w ~ dism1990 | herf+lenper, data = data)
iv_secondstage_lngini_b <- ivreg(lngini_b ~ dism1990 | herf+lenper, data = data)

summary(iv_secondstage_povrate_w)
summary(iv_secondstage_povrate_b)
summary(iv_secondstage_lngini_w)
summary(iv_secondstage_lngini_b)


