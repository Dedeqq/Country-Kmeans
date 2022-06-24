# load the data
data <- read.csv('data.csv')
data_describtion <- read.csv('data-dictionary.csv')

View(data_describtion)
View(data)

# data exploration and visualization
empty <- apply(data,2, function(x) is.na(x))
summary(empty) # no missing data

str(data)
summary(data)

library(corrplot)
M = cor(data[3:9])
corrplot(M, method = 'number')

library(rworldmap)
data.map <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "country")

mapCountryData(data.map, nameColumnToPlot="const_form", catMethod='categorical',colourPalette='rainbow', mapTitle='Constitutional form')

boxplot(data$child_mort)
hist(data$child_mort, col="peachpuff",  border="black", prob = TRUE, xlab = "Child mortality", main = "Child mortality distribution")
mapCountryData(data.map, nameColumnToPlot="child_mort",colourPalette='heat', mapTitle='Child mortality')

boxplot(data$exports)
hist(data$exports, col="peachpuff",  border="black", prob = TRUE, xlab = "Export", main = "Export distribution")
mapCountryData(data.map, nameColumnToPlot="exports",colourPalette='heat', mapTitle='Export')

boxplot(data$health)
hist(data$health, col="peachpuff",  border="black", prob = TRUE, xlab = "Health spending", main = "Health spending distribution")
mapCountryData(data.map, nameColumnToPlot="health",colourPalette='heat', mapTitle='Health spending')

boxplot(data$imports)
hist(data$imports, col="peachpuff",  border="black", prob = TRUE, xlab = "Import", main = "Import distribution")
mapCountryData(data.map, nameColumnToPlot="imports",colourPalette='heat', mapTitle='Import')

boxplot(data$income)
hist(data$income, col="peachpuff",  border="black", prob = TRUE, xlab = "Income", main = "Income per capita distribution")
mapCountryData(data.map, nameColumnToPlot="income",colourPalette='heat', mapTitle='Income per capita')

boxplot(data$inflation)
hist(data$inflation, col="peachpuff",  border="black", prob = TRUE, xlab = "Inflation", main = "Inflation distribution")
mapCountryData(data.map, nameColumnToPlot="inflation",colourPalette='heat', mapTitle='Inflation')

boxplot(data$life_expec)
hist(data$life_expec, col="peachpuff",  border="black", prob = TRUE, xlab = "Life expectancy", main = "Life expectancy distribution")
mapCountryData(data.map, nameColumnToPlot="life_expec",colourPalette='heat', mapTitle='Life expectancy')

boxplot(data$total_fer)
hist(data$total_fer, col="peachpuff",  border="black", prob = TRUE, xlab = "Fertility", main = "Fertility distribution")
mapCountryData(data.map, nameColumnToPlot="total_fer",colourPalette='heat', mapTitle='Fertility')

boxplot(data$gdpp)
hist(data$gdpp, col="peachpuff",  border="black", prob = TRUE, xlab = "GDP", main = "GDP per capita istribution")
mapCountryData(data.map, nameColumnToPlot="gdpp",colourPalette='heat', mapTitle='GDP per capita')


#data processing
data$republic <- ifelse(data$const_form == "Republic", 1, 0)
data$absolute_monarchy <- ifelse(data$const_form == "Absolute monarchy", 1, 0)
data$constitutional_monarchy <- ifelse(data$const_form == "Constitutional monarchy", 1, 0)
data$provisional <- ifelse(data$const_form == "Provisional", 1, 0)
View(data)

processed_data <- scale(data[3:15])
View(processed_data)

# choosing number of clusters with elbow rule
library('factoextra')
fviz_nbclust(processed_data,kmeans,method = 'wss')

# k means with 5 clusters
library('cluster')
cluster <- kmeans(processed_data,5)
data$cluster <- cluster$cluster
cluster$size
cluster$centers

data.map <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData(data.map, nameColumnToPlot="cluster",catMethod='categorical',colourPalette='rainbow')

# we decide to ignore constitutional form since it is a bad indicator of a country's economic development
new_processed_data <- scale(data[3:11])
View(new_processed_data)

# choosing number of clusters with elbow rule
fviz_nbclust(new_processed_data,kmeans,method = 'wss')

# k means with 3 clusters
better_cluster <- kmeans(new_processed_data,3)
data$result <- better_cluster$cluster
better_cluster$size
better_cluster$centers

data.map <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData(data.map, nameColumnToPlot="result",catMethod='categorical',colourPalette='rainbow')



