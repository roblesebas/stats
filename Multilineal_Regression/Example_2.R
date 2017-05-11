# Source 

#http://www.uv.es/uriel/5%20Analisis%20de%20regresion%20multiple%20con%20informacion%20cualitativa.pdf


setwd("C:/Users/Sebastian/Google Drive/CURSOS/DATA SCIENCE/stats/Multilineal_Regression")


# exercise 5.3 ####

# Uploading libraries

library(xlsx)

# Getting data

dataset.raw.1 <- read.xlsx2("wage02sp.xlsx", sheetIndex = 1, stringsAsFactors = FALSE)
names(dataset.raw.1) <- tolower(names(dataset.raw.1))

# Cleaning data ####

dataset.raw.2 <- dataset.raw.1[,c(50, 22, 17)]
dataset.raw.2$female <- as.numeric(dataset.raw.2$female)
dataset.raw.2$female <- factor(dataset.raw.2$female, labels = c("m", "f"))
dataset.raw.2$wage <- as.numeric(dataset.raw.2$wage)
dataset.raw.2$educ <- as.numeric(dataset.raw.2$educ)
dataset.raw.2$wage.t <- log(dataset.raw.2$wage)

# Inferential Analysis

m1 <- lm(wage.t ~ female + educ, data = dataset.raw.2)
summary(m1)


# exercise 5.3 ####

# Getting data ####

mydata_5.3.raw <- read.csv("demand.txt", sep = "\t", stringsAsFactors = FALSE)

# Cleaning data ####

names(mydata_5.3.raw) <- tolower(names(mydata_5.3.raw))

mydata_5.3.raw.1 <- mydata_5.3.raw[, c("fish", "urban", "inc")]
mydata_5.3.raw.1$urban <- factor(mydata_5.3.raw.1$urban, labels = c("rural", "urban"))

# Tidying data ####

mydata_5.3.raw.1$fish.t <- log(mydata_5.3.raw.1$fish)
mydata_5.3.raw.1$inc.t <- log(mydata_5.3.raw.1$inc)

# Inferential Analysis ####

m1 <- lm(fish.t ~ urban + inc.t, data = mydata_5.3.raw.1)
summary(m1)


