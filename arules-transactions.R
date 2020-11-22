setwd("/mnt/c/Users/X1C6/OneDrive/R Programs Info Learning/NUS-ISS Equipment/")

library(arules)

# data_equip <- read.csv("arule-input-transactions.csv", stringsAsFactor = FALSE)
# dim(data_equip)
# summary(data_equip)
# str(data_equip)

# read into transaction data format
data_trans <- read.transactions("arule-input-transactions.csv",rm.duplicates=T, sep=",", format="single", cols=c(1,2))
inspect(data_trans)
summary(data_trans)

#requency of equipment
itemFrequencyPlot(data_trans, support = 0.05, cex.names=0.8)
itemFrequencyPlot(data_trans, topN = 11, cex.names=0.8)
# by absolute value
itemFrequencyPlot(data_trans, topN=10, type="absolute", main="Item Frequency") 

#find frequent itemsets
frequentEqp <-  eclat(data_trans, parameter = list(support = 0.01, maxlen = 15), control = list(verbose=FALSE))
inspect(frequentEqp)
summary(frequentEqp)

#get association rules
# support = 0.01 gives only 3 rules. 0.06 gives 18.
Brules <- apriori(data_trans, parameter=list(support=0.003, confidence=0.5))
confBrules <- sort(Brules, by="confidence", decreasing = TRUE)
inspect(head(confBrules))
liftBrules <- sort(Brules, by="lift", decreasing = TRUE)
inspect(head(liftBrules))

library(arulesViz)
plot(Brules)
plot(Brules, method = "grouped", control = list(k = 5))
plot(Brules, method="graph", control=list(type="items"))

# This takes too long
# plot(Brules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))

# have to click on "end" in the plot to end session
plot(Brules,measure=c("support","lift"),shading="confidence",interactive=T)
