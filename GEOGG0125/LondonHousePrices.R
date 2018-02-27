#load required libraries
library(rgdal)
library(classInt)
library(maptools)
library(RColorBrewer)
library(scales)
library(prettymapr)
library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(grid)
library(gridExtra)
library(tmap)
library(maptools)

#Set working directory
setwd("/Users/nicolacritten/Documents/UNI/MASTERS/125 - Principles of Spatial Analysis/Coursework 2 (Nick)")

#import the shape file
boroughShape <- readOGR(".", "London_Borough_Excluding_MHW")

#import the csv files data and reformat the postcodes
pricesPaid<-read.csv("PricePaidData.csv", header = T, sep = ",", 
                     stringsAsFactors = F, check.names = T)
pricesPaid$Postcode<-str_replace_all(pricesPaid$Postcode, fixed(" "), "")

postcodeLookup<-read.csv("PostcodeLookup.csv", header = T, sep = ",", 
                         stringsAsFactors = F, check.names = T)
postcodeLookup$Postcode<-str_replace_all(postcodeLookup$Postcode, fixed(" "), "")

#working out the mean for each borough
districtMean <- data.frame(aggregate(as.double(pricesPaid[, "Price"]), 
                                     list(District=pricesPaid$District), mean))
districtMean[,"District"] <- as.character(districtMean[,"District"])
districtMean <- rename(districtMean, Mean=x)




#working out the median for each borough
districtMed <- data.frame(aggregate(as.double(pricesPaid[, "Price"]), list(District=pricesPaid$District), median))
districtMed[,"District"] <- as.character(districtMed[,"District"])
districtMed <- rename(districtMed, Median=x)

#working out the range for each borough
districtRange <- data.frame(aggregate(as.double(pricesPaid[, "Price"]), list(District=pricesPaid$District), range))
districtRange[,"District"] <- as.character(districtRange[,"District"])
RangeValues <- districtRange$x[,2]
districtRange <- data.frame(districtRange$District, RangeValues)

#converting the name of each borough to all caps for merging
boroughShape@data$NAME <- toupper(boroughShape@data$NAME)

#merging the districs with the borough shape
joinedMean <- merge(boroughShape, districtMean, by.x="NAME", by.y="District")
joinedAll <- merge(joinedMean, districtMed, by.x="NAME", by.y="District")
joinedAll <- merge(joinedAll, districtRange, by.x="NAME", by.y="districtRange.District")
pricesMean <- as.numeric(joinedAll@data$Mean)
pricesMed <- as.numeric(joinedAll@data$Median)
pricesRange <- as.numeric(joinedAll@data$RangeValues)

#accounting for Westminter NoData
westminster <- joinedAll[joinedAll[["NAME"]] == "WESTMINSTER", ]

#plotting and exporting the map for mean values
breaksMean <- classIntervals(pricesMean, n=6, style = "fisher") #setting the break value
my_colours <- brewer.pal(6, "Reds") #setting the colour scheme

pdf(file="Mean.pdf", width = 9.6, height = 8) #exporting as a PDF

prettymap(plot(boroughShape, col = my_colours[findInterval(pricesMean, breaksMean$brks, 
                                                           all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE, 
          title = 'Mean price paid per borough') #plotting the map
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, 
               border = "#404040"), drawscale = FALSE) #plotting Westminster (no data)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMean$brks, digits = 1) #reformatting the breaks value
breaks2 <- format(breaksMean$brks, big.mark=",", scientific=FALSE, trim = TRUE)
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")

dev.off()

#set colours & breaks for the median map
breaksMed <- classIntervals(pricesMed, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

#output to PDF
pdf(file="Median.pdf", width = 9.6, height = 8)

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(pricesMed, breaksMed$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE, title = 'Median price paid per borough')
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, border = "#404040"), drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMed$brks, digits = 1)
breaks2 <- format(breaksMed$brks, big.mark=",", scientific=FALSE, trim = TRUE)
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")

#stop saving
dev.off()

#set colours & breaks for the range map
breaksRange <- classIntervals(pricesRange, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

#output to PDF
pdf(file="Range.pdf", width = 9.6, height = 8)


#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(pricesRange, breaksRange$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE, title = 'Range of price paid per borough')
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, border = "#404040"), drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksRange$brks, digits = 1)
breaks2 <- format(breaksRange$brks, big.mark=",", scientific=FALSE, trim = TRUE)
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")

#stop saving
dev.off()

#merging the price paid with the postcodes
pricesPaid2 <- left_join(pricesPaid, postcodeLookup, by=c("Postcode"="Postcode"))

#Creating a new column for the colour of the point depending on price
pricesPaid2$Colour = "#0000008C"
pricesPaid2$Colour[pricesPaid2$Price<521810.8]="#fef0d9BF"
pricesPaid2$Colour[pricesPaid2$Price<763569.3 & pricesPaid2$Price>=521810.8]="#fdd49eBF"
pricesPaid2$Colour[pricesPaid2$Price<1237192.3 & pricesPaid2$Price>=763569.3]="#fdbb84BF"
pricesPaid2$Colour[pricesPaid2$Price<1797972.2 & pricesPaid2$Price>=1237192.3]="#fc8d59BF"
pricesPaid2$Colour[pricesPaid2$Price<3873737.1 & pricesPaid2$Price>=1797972.2]="#e34a33BF"
pricesPaid2$Colour[pricesPaid2$Price>=3873737.1]="#b30000BF"

pdf(file="Points plotted.pdf", width = 9.6, height = 8) #export to PDF

#plot the points of prices on a map
prettymap(plot(boroughShape), drawscale = FALSE, 
          title = 'Each sold property within London coloured by price paid') #plot map
prettymap(plot(westminster,density=15, angle=45, add = TRUE, border = "#404040"),
          drawscale = FALSE) #plot Westminster (no data)
points(pricesPaid2$Eastings, pricesPaid2$Nothings, col = pricesPaid2$Colour, 
       cex = .3, pch = 4) #overlay the points in the assigned colouur
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

dev.off()


#-----Plotting Hillingdon (biggest range)-----#
onlyHillingdonPoints <- pricesPaid2 %>% filter(District == "HILLINGDON")
onlyHillingdonShape <- boroughShape[boroughShape[["NAME"]] == "HILLINGDON",]

onlyHillingdonPointsRe <- onlyHillingdonPoints[order(onlyHillingdonPoints$Price, decreasing = TRUE),]
first500 <- onlyHillingdonPointsRe[1:500,]         #500 most expensive
first100 <- onlyHillingdonPointsRe[1:100,]         #100 most expensive
minusHighestAndLowest500 <- onlyHillingdonPointsRe[500:3377,] #total points: 3877

allPoints_plot <- qplot(data = onlyHillingdonPoints, x = Price) + ylab("Number") + xlab("Price") #all data
first500_plot <- qplot(data = first500, x = Price) + ylab("Number") + xlab("Price")
first100_plot <- qplot(data = first100, x = Price) + ylab("Number") + xlab("Price") #all data
minusHighestAndLowest_plot <- qplot(data = minusHighestAndLowest500, x = Price) + ylab("Number") + xlab("Price") #all data

pdf(file="HillingdonPlots.pdf", width = 9.6, height = 8)
#create grid of the plots made
grid.newpage()
pushViewport(viewport(layout=grid.layout(5,2, heights = unit(c(0.5,0.3,5,0.3,5), "null"))))

# prints a map object into a defined cell   
print(allPoints_plot, vp=viewport(layout.pos.col = 1, layout.pos.row = 3))
print(first500_plot, vp=viewport(layout.pos.col = 1, layout.pos.row =5))
print(first100_plot, vp=viewport(layout.pos.col = 2, layout.pos.row =3))
print(minusHighestAndLowest_plot, vp=viewport(layout.pos.col = 2, layout.pos.row = 5))
grid.text("Price paid for Borough of Hillingdon", vp = viewport(layout.pos.row = 1, 
                                                                layout.pos.col = 1:2))
grid.text("All points", vp = viewport(layout.pos.row = 2, layout.pos.col = 1, 
                                      gp=gpar(fontsize=9)))
grid.text("Most expensive 500 points", vp = viewport(layout.pos.row = 2, 
                                                     layout.pos.col = 2, gp=gpar(fontsize=9)))
grid.text("Most expensive 100 points", vp = viewport(layout.pos.row = 4, 
                                                     layout.pos.col = 1, gp=gpar(fontsize=9)))
grid.text("Not including the highest and lowest 500 points", vp = 
            viewport(layout.pos.row = 4, layout.pos.col = 2, gp=gpar(fontsize=9)))

dev.off()




#LEGEND NOT DRAWING AND COLOURS STILL TRANSPARENT
#Reassigning colours without transparency

pdf(file="Hillingdon points.pdf", width = 9.6, height = 8)
#plot Hillingdon
prettymap(plot(onlyHillingdonShape), drawscale = FALSE, 
          title = "Each sold property within borough of Hillingdon") 
prettymap(points(onlyHillingdonPoints$Eastings, onlyHillingdonPoints$Nothings,
                   cex = .7, col = onlyHillingdonPoints$Colour), drawscale = FALSE, 
          pch = 4, lwd = 2)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
legend(x=549034.8, y=170656.5, legend = leglabs(breaks2), 
       fill = c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", 
                "#e34a33", "#b30000"), bty = "black", bg = "#FFFFFFA6", 
       border = "black", cex = 0.75, title = "Price (£)")

dev.off()


#----plotting the points of the 500 most and 500 least expensive prices paid on a map-----#

#assign variables for the points
pricesPaidReordered <- pricesPaid2[order(pricesPaid2$Price, decreasing = TRUE),]
first500 <- pricesPaidReordered[1:500,]         #500 most expensive
last500 <- pricesPaidReordered[120018:120518,]  #500 least expensive

pdf(file="MostAndLeast.pdf", width = 9.6, height = 8) #export to PDF

#plotting the most and least expensive points
prettymap(plot(boroughShape), drawscale = FALSE,
          title = 'The 500 most and 500 least expensive properies sold')
prettymap(plot(westminster,density=15, angle=45, add = TRUE, border = "#404040"),
          drawscale = FALSE)
prettymap(points(first500$Eastings, first500$Nothings, col = "red", cex = .8), 
          drawscale = FALSE)
prettymap(points(last500$Eastings, last500$Nothings, col = "dark green", cex = .8), 
          drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
legend(x=551787.9, y=169345, legend = c("500 Most", "500 Least"), 
       col = c("red","dark green"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, pch = c(1,1))

dev.off()


#--------Removing Outliers---------#
#visalising the data before outliers removed
pricesPaidNew <- pricesPaid
qplot(data = pricesPaidNew, x = Price) + ylab("Price") #all data
qplot(data = pricesPaidNew, x = Price) + ylab("Price") + 
  xlim(c(10000000, 330000000)) #only the highest values - above 10,000,000

hist(pricesPaidNew$Price, breaks = 500)

#replacing outliers
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

#makes outliers a N/A value
outlierReplace(pricesPaidNew, "Price", which(pricesPaidNew$Price > 50000000), NA)

#visalising without outliers
qplot(data = pricesPaidNew, x = Price) + ylab("Price")
hist(pricesPaidNew$Price, breaks = 500)
hist(pricesPaidNew$Price, breaks = 50)



#--------WITHOUT OUTLIERS plotting the mean / median of the prices for each borough---------#
#working out the mean for each district
districtMean <- data.frame(aggregate(as.double(pricesPaidNew[, "Price"]), list(District=pricesPaidNew$District), mean, na.rm = TRUE))
districtMean[,"District"] <- as.character(districtMean[,"District"])
districtMean <- rename(districtMean, Mean=x)
districtMean

#working out the median for each district
districtMed <- data.frame(aggregate(as.double(pricesPaidNew[, "Price"]), list(District=pricesPaidNew$District), median, na.rm = TRUE))
districtMed[,"District"] <- as.character(districtMed[,"District"])
districtMed <- rename(districtMed, Median=x)
districtMed

#working out the range for each district
districtRange <- data.frame(aggregate(as.numeric(pricesPaidNew[, "Price"]), list(District=pricesPaidNew$District), range, na.rm = TRUE))
districtRange[,"District"] <- as.character(districtRange[,"District"])
RangeValues <- districtRange$x[,2]
districtRange <- data.frame(districtRange$District, RangeValues)
districtRange

#converting the name of each borough to all caps for merging
boroughShape@data$NAME <- toupper(boroughShape@data$NAME)

#merging the districs with the borough shape
joinedMean <- merge(boroughShape, districtMean, by.x="NAME", by.y="District")
joinedAll <- merge(joinedMean, districtMed, by.x="NAME", by.y="District")
joinedAll <- merge(joinedAll, districtRange, by.x="NAME", by.y="districtRange.District")
pricesMean <- as.numeric(joinedAll@data$Mean)
pricesMed <- as.numeric(joinedAll@data$Median)
pricesRange <- as.numeric(joinedAll@data$RangeValues)

View(joinedAll@data)

#creating tables for top 5 for each
onlyNeeded <- joinedAll@data %>% select(NAME, Mean, Median, RangeValues)
onlyNeeded <- rename(onlyNeeded, Range = RangeValues)

onlyNeededMean <- onlyNeeded %>% select(NAME, Mean)
onlyNeededMean <- onlyNeededMean[order(onlyNeededMean$Mean, decreasing = TRUE),]
onlyNeededMean$Mean <- format(onlyNeededMean$Mean, big.mark=",", scientific=FALSE, trim = TRUE)
onlyNeededMean <- onlyNeededMean[1:5,]

onlyNeededMed <- onlyNeeded %>% select(NAME, Median)
onlyNeededMed <- onlyNeededMed[order(onlyNeededMed$Median, decreasing = TRUE),]
onlyNeededMed$Median <- format(onlyNeededMed$Median, big.mark=",", scientific=FALSE, trim = TRUE)
onlyNeededMed <- onlyNeededMed[1:5,]

onlyNeededR <- onlyNeeded %>% select(NAME, Range)
onlyNeededR <- onlyNeededR[order(onlyNeededR$Range, decreasing = TRUE),]
onlyNeededR$Range <- format(onlyNeededR$Range, big.mark=",", scientific=FALSE, trim = TRUE)
onlyNeededR <- onlyNeededR[1:5,]

pdf("tableMeans.pdf")
grid.table(onlyNeededMean)
dev.off()

pdf("tableMed.pdf")
grid.table(onlyNeededMed)
dev.off()

pdf("tableR.pdf")
grid.table(onlyNeededR)
dev.off()

#accounting for Westminter NoData
westminster <- joinedAll[joinedAll[["NAME"]] == "WESTMINSTER", ]

#finding centroids for adding labels
boroughCentroids <- coordinates(boroughShape)

#set colours & breaks for the mean map
breaksMean <- classIntervals(pricesMean, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

#output to PDF
pdf(file="Mean_withoutOutliers2.pdf", width = 9.6, height = 8)

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(pricesMean, breaksMean$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE)
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, border = "#404040"), drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMean$brks, digits = 1)
breaks2 <- format(breaksMean$brks, big.mark=",", scientific=FALSE, trim = TRUE)
title('Mean price paid per borough (without outliers)')
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.35, font = 2)

dev.off()

#set colours & breaks for the median map
breaksMed <- classIntervals(pricesMed, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

pdf(file="Median_withoutOutliers2.pdf", width = 9.6, height = 8)

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(pricesMed, breaksMed$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE)
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, border = "#404040"), drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMed$brks, digits = 1)
breaks2 <- format(breaksMed$brks, big.mark=",", scientific=FALSE, trim = TRUE)
title('Median price paid per borough (without outliers)')
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.35, font = 2)

dev.off()

#set colours & breaks for the range map
breaksRange <- classIntervals(pricesRange, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

pdf(file="Range_withoutOutliers2.pdf", width = 9.6, height = 8)

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(pricesRange, breaksRange$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE)
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, border = "#404040"), drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksRange$brks, digits = 1)
breaks2 <- format(breaksRange$brks, big.mark=",", scientific=FALSE, trim = TRUE)
title('Range of price paid per borough (without outliers)')
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.35, font = 2)

dev.off()



#------picking out certain boroughs with a high range------#

#picking out Southwark, Enfield, Camden
onlySouthwarkPoints <- pricesPaid2 %>% filter(District == "SOUTHWARK")
onlySouthwarkShape <- boroughShape[boroughShape[["NAME"]] == "SOUTHWARK",]

#picking out Southwark, Enfield, Camden values
southwarkAll <- joinedAll@data %>% filter(NAME == "SOUTHWARK")
southwarkRange <- as.character(southwarkAll$RangeValues)
southwarkMean <- as.character(round(southwarkAll$Mean, digits = 1))
southwarkMed <- as.character(round(southwarkAll$Median, digits = 1))

enfieldAll <- joinedAll@data %>% filter(NAME == "ENFIELD")
enfieldRange <- as.character(enfieldAll$RangeValues)
enfieldMean <- as.character(round(enfieldAll$Mean, digits = 1))
enfieldMed <- as.character(round(enfieldAll$Median, digits = 1))

camdenAll <- joinedAll@data %>% filter(NAME == "CAMDEN")
camdenRange <- as.character(camdenAll$RangeValues)
camdenMean <- as.character(round(camdenAll$Mean, digits = 1))
camdenMed <- as.character(round(camdenAll$Median, digits = 1))

startingTextR <- "Range: "
startingTextM <- "Mean: "
startingTextMed <- "Median: " 

southwarkText1 <- paste(startingTextR,southwarkRange,sep="")
southwarkText2 <- paste(startingTextM,southwarkMean,sep="")
southwarkText3 <- paste(startingTextMed,southwarkMed,sep="")
  
enfieldText1 <- paste(startingTextR,enfieldRange,sep="")
enfieldText2 <- paste(startingTextM,enfieldMean,sep="")
enfieldText3 <- paste(startingTextMed,enfieldMed,sep="")

camdenText1 <- paste(startingTextR,camdenRange,sep="")
camdenText2 <- paste(startingTextM,camdenMean,sep="")
camdenText3 <- paste(startingTextMed,camdenMed,sep="")

s1 <- qplot(data = onlySouthwarkPoints, x = Price) + xlab("Price") + ylab("Number") #all data
s2<- qplot(data = onlySouthwarkPoints, x = Price) + xlab("Price") + ylab("Number") +
  xlim(c(100000, 3000000)) #only selection of points in the middle

#create grid of the plots made
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))

# prints a map object into a defined cell   
print(s1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(s2, vp=viewport(layout.pos.col = 1, layout.pos.row =2))


onlyEnfieldPoints <- pricesPaid2 %>% filter(District == "ENFIELD")
onlyEnfieldShape <- boroughShape[boroughShape[["NAME"]] == "ENFIELD",]

qplot(data = onlyEnfieldPoints, x = Price) + ylab("Price") #all data
qplot(data = onlyEnfieldPoints, x = Price) + ylab("Price") + 
  xlim(c(100000, 3000000)) #only the higher values - above 1,000,000

onlyCamdenPoints <- pricesPaid2 %>% filter(District == "CAMDEN")
onlyCamdenShape <- boroughShape[boroughShape[["NAME"]] == "CAMDEN",]

qplot(data = onlyCamdenPoints, x = Price) + ylab("Price") #all data
qplot(data = onlyCamdenPoints, x = Price) + ylab("Price") + 
  xlim(c(100000, 3000000)) #only the higher values - above 1,000,000

pdf(file="Southwark points.pdf", width = 9.6, height = 8)

#plot Southwark
prettymap(plot(onlySouthwarkShape), drawscale = FALSE,
          title = 'Each sold property within borough of Southwark')
prettymap(points(onlySouthwarkPoints$Eastings, onlySouthwarkPoints$Nothings,
                   cex = .7, col = onlySouthwarkPoints$Colour), lwd = 2, pch =4, drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
mtext(southwarkText1, side = 3, col = "black", line = 1, adj = 1)
mtext(southwarkText2, side = 3, col = "black", line = 2, adj = 1)
mtext(southwarkText3, side = 3, col = "black", line = 3, adj = 1)
dev.off()


pdf(file="Enfield points.pdf", width = 9.6, height = 8)
#plot Enfield
prettymap(plot(onlyEnfieldShape), drawscale = FALSE,
          title = 'Each sold property within borough of Enfield')
prettymap(points(onlyEnfieldPoints$Eastings, onlyEnfieldPoints$Nothings,
                   cex = .5, col = onlyEnfieldPoints$Colour), lwd =2, pch = 4, drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
mtext(enfieldText1, side = 3, col = "black", line = 1, adj = 1)
mtext(enfieldText2, side = 3, col = "black", line = 2, adj = 1)
mtext(enfieldText3, side = 3, col = "black", line = 3, adj = 1)
dev.off()

pdf(file="Camden points.pdf", width = 9.6, height = 8)
#plot Camden
prettymap(plot(onlyCamdenShape), drawscale = FALSE,
          title = 'Each sold property within Camden')
prettymap(points(onlyCamdenPoints$Eastings, onlyCamdenPoints$Nothings,
                   cex = .5, col = onlyCamdenPoints$Colour), lwd = 2, pch = 4, drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
mtext(camdenText1, side = 3, col = "black", line = 1, adj = 1)
mtext(camdenText2, side = 3, col = "black", line = 2, adj = 1)
mtext(camdenText3, side = 3, col = "black", line = 3, adj = 1)
dev.off()


#------HOUSEHOLD INCOME-------#
#NOT USING AS MISSING TWO BOROUGHS
boroughProfiles<-read.csv("london-borough-profiles.csv", header = T, sep = ",", stringsAsFactors = F, check.names = T, fileEncoding="latin1")
names(boroughProfiles)

#converting the name of each borough to all caps for merging
boroughProfiles$Area.name <- toupper(boroughProfiles$Area.name)

boroughProfiles$Modelled.Household.median.income.estimates.2012.13 = gsub("[£]", "", boroughProfiles$Modelled.Household.median.income.estimates.2012.13)
boroughProfiles$Modelled.Household.median.income.estimates.2012.13 = gsub("[,]", "", boroughProfiles$Modelled.Household.median.income.estimates.2012.13)

#merging the districs with the borough shape
joinedProfiles <- merge(boroughShape, boroughProfiles, by.x="NAME", by.y="Area.name")

annualPay <- as.numeric(joinedProfiles@data$Gross.Annual.Pay..2016.)

householdMedIncome <- as.numeric(joinedProfiles@data$Modelled.Household.median.income.estimates.2012.13)

#set colours & breaks for the annual pay map
breaksPay <- classIntervals(annualPay, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

#plot map
#output to PDF
#pdf(file="Household income.pdf", width = 9.6, height = 8)

prettymap(plot(boroughShape, col = my_colours[findInterval(annualPay, breaksPay$brks, 
                                                           all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE, title = 'Gross Annual Pay per Borough')
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMean$brks, digits = 1)
breaks2 <- format(breaksMean$brks, big.mark=",", scientific=FALSE, trim = TRUE)
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")

#stop saving
#dev.off()


#USING THIS ONE AS HAS ALL BOROUGHS

#set colours & breaks for the household median income map
breaksHouseholdMedIncome <- classIntervals(householdMedIncome, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

#plot map
#output to PDF
pdf(file="Household median income.pdf", width = 9.6, height = 8)

prettymap(plot(boroughShape, col = my_colours[findInterval(householdMedIncome, breaksHouseholdMedIncome$brks, 
                                                           all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE, 
          title = 'Household Median Income per Borough')
addnortharrow(pos = "topright", padin = c(0.8, 0.9), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMean$brks, digits = 1)
breaks2 <- format(breaksMean$brks, big.mark=",", scientific=FALSE, trim = TRUE)
legend(x=549034.8, y=170656.5, legend = c(leglabs(breaks2)), 
       fill = c(my_colours), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Income (£)")

#stop saving
dev.off()


#----Correlation of median income per borough and median price paid----#
#make a data frame
corDataFrame <- data.frame(MedianIncome=householdMedIncome, MedianPricePaid=pricesMed)
names(corDataFrame)
print(corDataFrame)

cor(corDataFrame, use="pairwise.complete.obs") #Pearson correlation
cor(corDataFrame, method="kendall", use="pairwise.complete.obs")
cor(corDataFrame, method="spearman", use="pairwise.complete.obs")

#graph of med income vs med price paid
pdf(file="Med Income vs Med Price Plot.pdf", width = 9.6, height = 8)

par(mfrow=c(1,1))
with(corDataFrame, plot(MedianIncome,MedianPricePaid, pch = 4, cex = 0.75, 
                        xlab = "Median Household Income (£)",
                        ylab = "Median Property Price Paid (£)"))
title(main = list("Median Household Income by Median Property Price Paid \n for the Boroughs of London", cex = 0.8,
                  col = "black", font = 2, cex = 1))
mtext("Pearson correlation: 0.7799", side = 1, col = "black", line = 3, adj = 0, font = 3)
with(corDataFrame,abline(lm(MedianPricePaid ~ MedianIncome), col = "red"))
dev.off()


#CORRECT ONE

pdf(file="Med Price Plot vs Med Income.pdf", width = 9.6, height = 8)
with(corDataFrame, plot(MedianPricePaid,MedianIncome, pch = 4, cex = 0.75, 
                        ylab = "Median Household Income (£)",
                        xlab = "Median Property Price Paid (£)"))
title(main = list("Median Household Income by Median Property Price Paid \n for the Boroughs of London", cex = 0.8,
                  col = "black", font = 2, cex = 1))
mtext("Pearson correlation: 0.7799", side = 1, col = "black", line = 3, adj = 0, font = 3)
with(corDataFrame,abline(lm(MedianIncome~ MedianPricePaid), col = "red"))
dev.off()


#----Geographically weighted Regression----#
#runs a linear model 
joinedProfiles2 <- joinedProfiles[!(joinedProfiles@data$NAME == "WESTMINSTER"),]
joinedAll2 <- joinedAll[!(joinedAll@data$NAME == "WESTMINSTER"),]

model <- lm(joinedProfiles2@data$Modelled.Household.median.income.estimates.2012.13 
            ~ joinedAll2@data$Median)

# we can use the par function to plot them in a 2x2 frame
par(mfrow=c(2,2))
plot(model)

resids<-residuals(model)
map.resids <- cbind(joinedProfiles2, resids) 
names(map.resids)[13] <- "resids"
#qtm(map.resids, fill = "resids")

#A residual is the difference between the predicted and observed values for an 
#observation in the model. Models with lower r-squared values would have greater 
#residuals on average as the data would not fit the modelled regression line as well. 
#Standardised residuals are represented as Z-scores where 0 represent the predicted values.


#----Better map----#
par(mfrow=c(1,1))




#generates the residual values for the median income and house price
model <- lm(joinedProfiles2@data$Modelled.Household.median.income.estimates.2012.13 
            ~ joinedAll2@data$Median)
resids<-residuals(model)
map.resids <- cbind(joinedProfiles2, resids) 
names(map.resids)[13] <- "resids"
var <- map.resids@data$resids

breaks <- classIntervals(var, n = 5, style = "pretty") #set break values
my_colours <- rev(brewer.pal(5, "RdYlBu")) #set colour scheme

pdf(file="Residuals.pdf", width = 9.6, height = 8) #output to PDF

#plot map
prettymap(plot(map.resids, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)], 
     axes = FALSE, border = "#404040"), drawscale =FALSE,
     title = "Residuals for Median Household Income by Median Property Price Paid for the Boroughs of London")
prettymap(plot(westminster, col = "#dbdbdb", add = TRUE, border = rgb(0.8,0.8,0.8,0)),
          drawscale = FALSE)
addnortharrow(pos = "topright", padin = c(0.15, 0.8), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
legend(x=553034.8, y=170656.5, legend = c(leglabs(breaks$brks), "NoData"), 
       fill = c(my_colours,"#dbdbdb"), bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Residual value")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.35, font = 2) #adds labels

dev.off()




View(map.resids@data)
#----Average house price data----#
housePricesMean<-read.csv("house-prices-borough-mean.csv", header = T, sep = ",", stringsAsFactors = F, check.names = T, fileEncoding="latin1")

#converting the name of each borough to all caps for merging
housePricesMean$Area <- toupper(housePricesMean$Area)

#merging the districs with the borough shape
joinedHousePriceMean <- merge(boroughShape, housePricesMean, by.x="NAME", by.y="Area")


#set colours & breaks for the average house price data
breaksMed <- classIntervals(joinedHousePriceMean@data$X2016, n=6, style = "fisher")
my_colours <- brewer.pal(6, "Reds") #hexidecimal colours

#output to PDF
pdf(file="AverageHousePrice.pdf", width = 9.6, height = 8)

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(joinedHousePriceMean@data$X2016,
                                                           breaksMed$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale = FALSE, 
          title = 'Average house price per borough')
addnortharrow(pos = "topright", padin = c(0.5, 0.9), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.55, 1.2),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
breaks$brks <- round(breaksMed$brks, digits = 1)
breaks2 <- format(breaksMed$brks, big.mark=",", scientific=FALSE, trim = TRUE)
legend(x=549034.8, y=172356.5, legend = leglabs(breaks2), 
       fill = my_colours, bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Price (£)")

dev.off()


#----Compared "average house price" data with "mean property price"----#
only2016Mean <- housePricesMean %>% select(Area, X2016)
bothAveragePrice <- merge(districtMean, only2016Mean, by.x="District", by.y="Area")
bothAveragePrice <- rename(bothAveragePrice, Borough = District, "Mean price paid 2016" = Mean, "Average house price 2016" = X2016)

bothAveragePrice$`Mean price paid 2016` <- round(bothAveragePrice$`Mean price paid 2016`, digits = 1)
bothAveragePrice$`Mean price paid 2016` <- format(bothAveragePrice$`Mean price paid 2016`, big.mark=",", scientific=FALSE, trim = TRUE)

bothAveragePrice$`Average house price 2016` <- format(bothAveragePrice$`Average house price 2016`, big.mark=",", scientific=FALSE, trim = TRUE)

pdf("tableAverages3.pdf", height=11, width=10)
grid.table(bothAveragePrice)
dev.off()




install.packages("zoo")
library(gstat)
library(xts)

# define sample grid based on the extent of the House.Points file
grid <-spsample(joinedAll, type = 'regular', n = 10000)

# runs the idw for the Price variable of House.Points
idw <- idw(joinedAll@data$Mean ~ 1, joinedAll, newdata= grid)
