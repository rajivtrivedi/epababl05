# library loads -----------------------------------------------------------

setwd("c:/users/rajiv.trivedi/downloads")
library("readxl")
#install.packages("arules")
# install.packages("arulesViz")

library(arules)
library(arulesViz)

# Read Data ---------------------------------------------------------------

data <- read_excel("Market Basket Analysis of Cyber Security Perception.xlsx", sheet = "for_mba_analysis")
head(data)
# Generate Rules ----------------------------------------------------------
# support = 0.20,
# confidence = 0.8,

rules <- apriori(
  data,
  parameter = list(
    support = 0.20,
    confidence = 0.70,
    target = "rules"          # Maximum length of the rules
  ))
concerned_rules <- subset(rules, subset = rhs %in% "Concern=Very much concerned")
inspect(concerned_rules)
plot(concerned_rules, method = "graph",  
     measure = "confidence", shading = "lift") 

quality(concerned_rules)[0:5]


# another experiment ------------------------------------------------------
cyber_security_data <- read_excel("Market Basket Analysis of Cyber Security Perception.xlsx", sheet = "cyber_security_data")

cyber_security_rules <- apriori(
  cyber_security_data,
  parameter = list(
    support = 0.02,
    confidence = 0.80,
    target = "rules"          # Maximum length of the rules
  ))
# inspect(cyber_security_rules)

fraud_awareness_rules <- subset(cyber_security_rules, subset = rhs %in% "fraud_awareness=No")
inspect(fraud_awareness_rules)
plot(fraud_awareness_rules, method = "graph",  
     measure = "confidence", shading = "lift") 

quality(fraud_awareness_rules)[0:5]

