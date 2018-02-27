#Install code for required libraries if needed
#install.packages("rgdal")
#install.packages("maptools")
#install.packages("RColorBrewer")
#install.packages("dplyr")
#install.packages("classInt")
#install.packages("ggsn")
#install.packages("sp")
#install.packages("prettymapr")
#install.packages("tmap")
#install.packages("gridExtra")
#install.packages("grid")

#Required libraries
library(rgdal)
library(maptools)
library(RColorBrewer)
library(dplyr)
library(classInt)
library(ggsn)
library(sp)
library(prettymapr)
library(tmap)
library(gridExtra)
library(grid)

#Set working directory - must be changed if running the code elsewhere
#Insert the file path where the R code file is saved in the place below
setwd("/Users/nicolacritten/Documents/UNI/MASTERS/125 - Principles of Spatial Analysis/Coursework 1 (Micro)")

#------SECTION 1------#
#Read in the shape file of the London boroughs, which should be in the working directory
boroughShape <- readOGR(".", "London_Borough_Excluding_MHW")

#Read in the data as CSV from the London Data Store
#Files are the general London Borough Data, Access to Nature and Access to Public Open Space
brghData<-read.csv("LondonBoroughsData.csv", header = T, sep = ",", stringsAsFactors = F, check.names = T)
accessToNature <- read.csv("AccessToNature.csv", header = T, sep = ",", stringsAsFactors = F, check.names = T)
accessToPublicOpenSpace <- read.csv("AccessToPublicOpenSpace.csv", header = T, sep = ",", stringsAsFactors = F, check.names = T)

#Rename the columns that will be used so they are easier to reference
brghData <- rename(brghData, greenSpace=X._of_area_that_is_Greenspace._2005)
brghData <- rename(brghData, anxiety=Anxiety_score_2011.14_.out_of_10.)
accessToNature <- rename(accessToNature, goodAccess=X..homes.with.good.access.to.nature)

#Calculate out a mean for each borough's access to nature and then add it as a new column
accessToNatureMean <- data.frame(aggregate(as.double(accessToNature[, "goodAccess"]), list(District=accessToNature$Borough), mean))
accessToNature <- rename(accessToNatureMean, goodAccess=x)

#Filter out only the borough means from access to public open space data
#This data source also includes means for each ward, which are not required
onlyBoroughs <- accessToPublicOpenSpace %>% filter(Ward.name == "Boroughs")

#Rename the columns regarding access to parks to make it easier to reference
onlyBoroughs <- rename(onlyBoroughs, accessToLocalPark=Local.Parks)
onlyBoroughs <- rename(onlyBoroughs, accessToDistPark=District.Parks)
onlyBoroughs <- rename(onlyBoroughs, accessToMetroPark=Metropolitan.Parks)

#Select only the needed columns
brghData <- brghData %>% select(Area_name,greenSpace,anxiety)

#Join each of the requried data items with the borough shape file
boroughShape@data <- left_join(boroughShape@data, brghData, by = c('NAME' = 'Area_name'))
boroughShape@data <- left_join(boroughShape@data, accessToNature, by = c('NAME' = 'District'))
boroughShape@data <- left_join(boroughShape@data, onlyBoroughs, by = c('NAME' = 'Borough.name'))

#Select only the needed columns
boroughShape@data <- boroughShape@data %>% select(NAME,greenSpace,anxiety,
                                                  goodAccess,accessToLocalPark,
                                                  accessToDistPark,accessToMetroPark)

#Assign the required data items so they are easier to reference
greenSpaceVar <- as.numeric(boroughShape@data[,"greenSpace"])
anxietyVar <- as.numeric(boroughShape@data[,"anxiety"])
goodAccessVar <- as.numeric(boroughShape@data[,"goodAccess"])
accessToLocalParkVar <- as.numeric(boroughShape@data[,"accessToLocalPark"])
accessToDistParkVar <- as.numeric(boroughShape@data[,"accessToDistPark"])
accessToMetroParkVar <- as.numeric(boroughShape@data[,"accessToMetroPark"])

#Find the centroids for each borough for adding labels after map creation
boroughCentroids <- coordinates(boroughShape)

#------SECTION 2: GREEN SPACE------#

#Output the created map to a PDF saved in the working directory
pdf(file="Percentage that is green space map.pdf", width = 9.6, height = 8)

#set colours & breaks for better map
breaks <- classIntervals(greenSpaceVar, n=6, style = "fisher")
my_colours <- brewer.pal(6, "YlOrRd") #hexidecimal colours

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(greenSpaceVar, breaks$brks, all.inside = TRUE)], axes = FALSE, 
               border = rgb(0.8,0.8,0.8)),
          title = 'Percentage of Borough Area that is Green Space (2005)')
addnortharrow(pos = "topright", padin = c(0.8, 0.9), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
legend(x=553070.5, y=169574.6, legend = leglabs(breaks$brks), fill = my_colours, 
       bty = "n", border = "black", cex = 0.75, title = "Percentage (%)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.5, font = 2)

#stop PDF outout
dev.off()

#------SECTION 3: ANXIETY------#

#Output the created map to a PDF saved in the working directory
pdf(file="Anxiety score map.pdf", width = 9.6, height = 8)

#set colours & breaks for better map
breaks <- classIntervals(anxietyVar, n=6, style = "fisher")
my_colours <- brewer.pal(6, "YlOrRd") #hexidecimal colours

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(anxietyVar, breaks$brks, all.inside = TRUE)], axes = FALSE, 
               border = rgb(0.8,0.8,0.8)),
          title = 'Anxiety score per borough in London (2011-14)')
addnortharrow(pos = "topright", padin = c(0.8, 0.9), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
legend(x=553070.5, y=169574.6, legend = leglabs(breaks$brks), fill = my_colours, 
       bty = "n", border = "black", cex = 0.75, title = "Score (Out of 10)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.5, font = 2)

dev.off()

#------SECTION 4: ACCESS TO NATURE------#

#Output the created map to a PDF saved in the working directory
pdf(file="Good access to nature map.pdf", width = 9.6, height = 8)

#set colours & breaks for better map
breaks <- classIntervals(goodAccessVar, n=6, style = "fisher")
my_colours <- brewer.pal(6, "YlOrRd") #hexidecimal colours

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(goodAccessVar, breaks$brks, all.inside = TRUE)], axes = FALSE, 
               border = rgb(0.8,0.8,0.8)),
          title = 'Percentage of Households with Good Access to Nature per borough in London (2012)')
addnortharrow(pos = "topright", padin = c(0.8, 0.9), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
breaks$brks <- round(breaks$brks, digits = 1)
legend(x=553070.5, y=169574.6, legend = leglabs(breaks$brks), fill = my_colours, 
       bty = "n", border = "black", cex = 0.75, title = "Percentage (%)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.5, font = 2)

dev.off()

#------SECTION 5: ACCESS TO LOCAL PARKS------#

#Output the created map to a PDF saved in the working directory
pdf(file="Access to local park map.pdf", width = 9.6, height = 8)

#set colours & breaks for better map
breaks <- classIntervals(accessToLocalParkVar, n=6, style = "fisher")
my_colours <- brewer.pal(6, "YlOrRd") #hexidecimal colours

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(accessToLocalParkVar, breaks$brks, all.inside = TRUE)], axes = FALSE, 
               border = rgb(0.8,0.8,0.8)),
          title = "Percentage of Households with Access to a Local Park per borough in London (2013)")
addnortharrow(pos = "topright", padin = c(0.8, 0.9), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
breaks$brks <- round(breaks$brks, digits = 1)
legend(x=553070.5, y=169574.6, legend = leglabs(breaks$brks), fill = my_colours, 
       bty = "n", border = "black", cex = 0.75, title = "Percentage (%)")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.5, font = 2)

dev.off()

#------SECTION 6: CORRELATION------#

#make a data frame for green space vs anxiety
dat_GA <- data.frame(GreenSpace=greenSpaceVar, Anxiety=anxietyVar)
GAcor <- cor(dat_GA) #Pearson correlation

#anxiety vs good access to nature
dat_NA <- data.frame(GoodNatureAccess=goodAccessVar, Anxiety=anxietyVar)
NAcor <- cor(dat_NA) #Pearson correlation

#anxiety vs good access to a local park
dat_LPA <- data.frame(LocalParkAccess=accessToLocalParkVar, Anxiety=anxietyVar)
LPAcor <- cor(dat_LPA) #Pearson correlation

#anxiety vs good access to a district park
dat_DPA <- data.frame(DistrictParkAccess=accessToDistParkVar, Anxiety=anxietyVar)
DPAcor <- cor(dat_DPA) #Pearson correlation

#anxiety vs good access to a metropolitan park
dat_MPA <- data.frame(MetropolitianParkAccess=accessToMetroParkVar, Anxiety=anxietyVar)
MPAcor <- cor(dat_MPA) #Pearson correlation

items <- c("% of Green Space"=round(GAcor[1,2], digits=3), 
           "Access To Nature" =round(NAcor[1,2],digits=3), 
           "Local Park Access" =round(LPAcor[1,2],digits=3), 
           "District Park Access" =round(DPAcor[1,2],digits=3),
           "Metropolitan Park Access" =round(MPAcor[1,2],digits=3))

corTable <- data.frame("Anxiety Score"=items)
corTable <- tableGrob(corTable)

#Output the created table of correlation values to a PDF saved in the working directory
pdf("Correlations table.pdf")
grid.draw(corTable)
dev.off()

#------SECTION 7: PLOTS FOR CORRELATION------#

#Output the created scatterplot to a PDF saved in the working directory
pdf(file="Green space by Anxiety score scatterplot.pdf", width = 9.6, height = 8)
#plot of green space % vs anxiety
with(dat_GA,plot(GreenSpace,Anxiety, xlab = "Borough Area that is Green Space (%)",
                 ylab = "Anxiety Score (out of 10)"))
title(main = list("Percentage of Green Space by Anxiety Score for the Boroughs of London",
                  col = "black", font = 2))
with(dat_GA,abline(lm(Anxiety ~ GreenSpace), col = "red"))
mtext("Pearson correlation: -0.433", side = 1, col = "black", line = 3, adj = 0, font = 3)
dev.off()

#Output the created scatterplot to a PDF saved in the working directory
pdf(file="Access to Nature by Anxiety score scatterplot.pdf", width = 9.6, height = 8)
#plot of anxiety vs good access to nature
with(dat_NA,plot(GoodNatureAccess,Anxiety, xlab = "Household Acccess to Nature (%)",
                 ylab = "Anxiety Score (out of 10)"))
title(main = list("Good Access to Nature by Anxiety score for the Boroughs of London",
                  col = "black", font = 2))
with(dat_NA,abline(lm(Anxiety ~ GoodNatureAccess), col = "red"))
mtext("Pearson correlation: -0.5681", side = 1, col = "black", line = 3, adj = 0, font = 3)
dev.off()

#Output the created scatterplot to a PDF saved in the working directory
pdf(file="Access to Local Park by Anxiety score scatterplot.pdf", width = 9.6, height = 8)
#plot of anxiety vs good access to a local park
with(dat_LPA,plot(LocalParkAccess,Anxiety, xlab = "Household Access to a Local Park (%)",
                  ylab = "Anxiety Score (out of 10)"))
title(main = list("Access to a Local Park by Anxiety score for the Boroughs of London",
                  col = "black", font = 2))
with(dat_LPA,abline(lm(Anxiety ~ LocalParkAccess), col = "red"))
mtext("Pearson correlation: 0.471", side = 1, col = "black", line = 3, adj = 0, font = 3)
dev.off()

#Output the created scatterplot to a PDF saved in the working directory
pdf(file="Access to District Park by Anxiety score scatterplot.pdf",width = 9.6, height = 8)
#plot of anxiety vs good access to a district park
with(dat_DPA,plot(DistrictParkAccess,Anxiety, xlab = "Household Access to a District Park (%)",
                  ylab = "Anxiety Score (out of 10)"))
title(main = list("Access to a Dictrict Park by Anxiety score for the Boroughs of London",
                  col = "black", font = 2))
with(dat_DPA,abline(lm(Anxiety ~ DistrictParkAccess), col = "red"))
mtext("Pearson correlation: -0.3602", side = 1, col = "black", line = 3, adj = 0, font = 3)
dev.off()

#Output the created scatterplot to a PDF saved in the working directory
pdf(file="Access to Metro Park by Anxiety score scatterplot.pdf",width = 9.6, height = 8)
#plot of anxiety vs good access to a metropolitian park
with(dat_MPA,plot(MetropolitianParkAccess,Anxiety, xlab = "Household Access to a Metropolitan Park (%)",
                  ylab = "Anxiety Score (out of 10)"))
title(main = list("Access to a Metropolitian Park by Anxiety score for the Boroughs of London",
                  col = "black", font = 2))
with(dat_MPA,abline(lm(Anxiety ~ MetropolitianParkAccess), col = "red"))
mtext("Pearson correlation: -0.103", side = 1, col = "black", line = 3, adj = 0, font = 3)
dev.off()

#------SECTION 8: RESIDUALS FOR ANXIETY SCORE BY ACCESS TO NATURE------#

#Runs a linear model, which gives information about residuals
model <- lm(dat = dat_NA,Anxiety ~ GoodNatureAccess)
summary(model)

#Creates a variable for the redidual values and adds them to the borough data
resids<-residuals(model)
map.resids <- cbind(boroughShape, resids) 
names(map.resids)[8] <- "resids"

#Maps the residual values
residualsVar <- map.resids@data$resids
#set colours & breaks
breaks <- classIntervals(residualsVar, n = 6, style = "pretty")
my_colours <- rev(brewer.pal(6, "RdYlBu"))

#Output the created map to a PDF saved in the working directory
pdf(file="Residuals map.pdf", width = 9.6, height = 8)

#plot map
prettymap(plot(boroughShape, col = my_colours[findInterval(residualsVar, breaks$brks, all.inside = TRUE)], 
               axes = FALSE, border = "#404040"), drawscale =FALSE,
          title = "Residuals for Good Nature Access by Anxiety Score for the Boroughs of London")
addnortharrow(pos = "topright", padin = c(0.8, 1.1), scale = 0.7,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.25,
            unitcategory = "metric", htin = 0.1, padin = c(0.15, 0.15),
            style = "bar", bar.cols = c("black", "white"), lwd = 1,
            linecol = "black", tick.cex = 0.7, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomright")
legend(x=553034.8, y=170656.5, legend = leglabs(round(breaks$brks,digits=3)), 
       fill = my_colours, bty = "black", bg = "#FFFFFFA6",
       border = "black", cex = 0.75, title = "Residual value")
text(boroughCentroids, labels = boroughShape$NAME, cex=0.5, font = 2)

dev.off()

#------SECTION 9: EXPORTING A TABLE TO COMPARE THE DIFFERENT VARIABLES------#

#Selects the needed variables for the table and formats them corrrectly
neededVars <- boroughShape@data %>% select(NAME, greenSpace, goodAccess, 
                                           accessToMetroPark, anxiety)
neededVars$goodAccess <- round(neededVars$goodAccess, digits = 1)
neededVars <- rename(neededVars, Borough = NAME)
neededVars <- rename(neededVars, "% of Area is \nGreen Space" = greenSpace)
neededVars <- rename(neededVars, "% of Households \n with Good Access \n to Nature" 
                     = goodAccess)
neededVars <- rename(neededVars, "% of Households \n with Access to \n a Metropolitan Park" 
                     = accessToMetroPark)
neededVars <- rename(neededVars, "Anxiety Score" = anxiety)

#Orders the values by anxiety score
neededVars <- neededVars[order(neededVars$`Anxiety Score`, decreasing = FALSE),]

#Output the created table to a PDF saved in the working directory
pdf("Variables table simple.pdf", height=11, width=10)
grid.table(neededVars)
dev.off()


#Creates another table with the highest and lowest three boroughs highlighed for each variable
neededVars2 <- tableGrob(neededVars)

#A function to find the cell within the table that will be coloured
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

#Creates variables for the cell locations of the highest 3 boroughs for each variable
high1 <- find_cell(neededVars2, 17, 3, "core-bg")   #% of Area is Green Space
high12 <- find_cell(neededVars2, 16, 3, "core-bg")  #% of Households with Good Access to Nature
high13 <- find_cell(neededVars2, 12, 3, "core-bg")  #% of Households with Access to a Metropolitan Park

high2 <- find_cell(neededVars2, 30, 4, "core-bg")
high22 <- find_cell(neededVars2, 8, 4, "core-bg")
high23 <- find_cell(neededVars2, 12, 4, "core-bg")

high3 <- find_cell(neededVars2, 8, 5, "core-bg")
high32 <- find_cell(neededVars2, 24, 5, "core-bg")
high33 <- find_cell(neededVars2, 30, 5, "core-bg")

#Colours each of the cells with the 3 highest values green 
neededVars2$grobs[high1][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
neededVars2$grobs[high12][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
neededVars2$grobs[high13][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)

neededVars2$grobs[high2][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
neededVars2$grobs[high22][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
neededVars2$grobs[high23][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)

neededVars2$grobs[high3][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
neededVars2$grobs[high32][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)
neededVars2$grobs[high33][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)

#Creates variables for the cell locations of the lowest 3 boroughs for each variable
low1 <- find_cell(neededVars2, 32, 3, "core-bg")  #% of Area is Green Space
low12 <- find_cell(neededVars2, 34, 3, "core-bg") #% of Households with Good Access to Nature
low13 <- find_cell(neededVars2, 8, 3, "core-bg")  #% of Households with Access to a Metropolitan Park

low2 <- find_cell(neededVars2, 2, 4, "core-bg")
low22 <- find_cell(neededVars2, 6, 4, "core-bg")
low23 <- find_cell(neededVars2, 17, 4, "core-bg")

low3 <- find_cell(neededVars2, 21, 5, "core-bg")
low32 <- find_cell(neededVars2, 3, 5, "core-bg")
low33 <- find_cell(neededVars2, 4, 5, "core-bg")

#Colours each of the cells with the 3 lowest values red 
neededVars2$grobs[low1][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)
neededVars2$grobs[low12][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)
neededVars2$grobs[low13][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)

neededVars2$grobs[low2][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)
neededVars2$grobs[low22][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)
neededVars2$grobs[low23][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)

neededVars2$grobs[low3][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)
neededVars2$grobs[low32][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)
neededVars2$grobs[low33][[1]][["gp"]] <- gpar(fill="firebrick1", col = "firebrick4", lwd=5)

#Output the created table to a PDF saved in the working directory
pdf("Variables table with selections.pdf", height=11, width=10)
grid.draw(neededVars2)
dev.off()