setwd("/mnt/c/Users/X1C6/OneDrive/R Programs Info Learning/NUS-ISS Equipment/")

library(reshape2)
df <- read.csv("arule-input-transactions.csv", stringsAsFactor = FALSE)

# Convert to basket format using reshape2
newdata <- dcast(data = melt(df, id.vars = "TRX"), TRX ~ variable + value, length)
write.csv(newdata, "reshape-arules-input-transactions.csv", row.names = FALSE)

df2 <- list()
for(i in unique(df$TRX)){
  df2[[i]] <- unlist(df$EQP[df$TRX==i])
}

ibrary(arules)

newTransc <- as(df2, "transactions")
inspect(newTransc)
summary(newTransc)

itemFrequencyPlot(newTransc, support = 0.05, cex.names=0.8)
itemFrequencyPlot(newTransc, topN = 11, cex.names=0.8)

rules <- apriori(newTransc, parameter = list(supp = 0.05, conf = 0.8))
options(digits=2)
inspect(rules[1:5])
inspect(rules)
