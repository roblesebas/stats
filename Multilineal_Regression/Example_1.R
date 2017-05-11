
setwd("C:/Users/Sebastian/Google Drive/CURSOS/DATA SCIENCE/stats/Multilineal_Regression")

# Uploading libraries

# http://personal.us.es/vararey/adatos2/multiple.pdf

library(foreign)  # read spss files
library(QuantPsyc)  # standirize coeficients 

# Creating scripts

# Getting Data ####

dataset <- read.spss("notas.sav", to.data.frame = TRUE )
dataset$nsocial <- factor(dataset$nsocial)
dataset$sexo <- factor(dataset$sexo, labels = c("m", "f"))

to.data.frame = # Descriptive Analysis ####

# Inferential Analysis #### 

# Model 

m1 <- lm(calif ~ int + horas + nsocial, data = dataset)

summary(m1)

lm.beta(m1)

# ANOVA

fit <- aov(calif ~ int + horas + nsocial, data = dataset)

summary(fit)

anova(m1)

