setwd("C:/Users/dhardy/Dropbox/sesync/data/R")

# load emf maker
library(devEMF)

# set colors
rgb(0, 169, 230, names = "mooreablue", maxColorValue = 255)
rgb(0, 197, 255, names ="delftblue", maxColorValue = 255)
rgb(115, 223, 255, names = "apatiteblue", maxColorValue = 255)
rgb(190, 232, 255, names = "sodaliteblue", maxColorValue = 255)
rgb(190, 255, 232, names = "indicoliteblue", maxColorValue = 255)

# create data and label vectors for property number affected by SLR
prcl <- c(36, 132, 205, 283)
lbls.prcl <- c("36", "132", "205", "283")
x <- c(1, 2, 3, 4)

# prints tif
tiff('figures/hh.prop.tif', width=3, height=3, units="in", res=300)
par(cex=0.7)

# plots data in bar chart
cols1 <- c("#BEFFE8",  "#73DFFF", "#00C5FF", "#00A9E6")
midpoints <- barplot(prcl, names.arg=x, ylim=c(0, 300), xlab="Sea-level rise (feet)", ylab="Number of properties", col=cols1)
text(midpoints, 20, labels=prcl)
##title("Number of Hog Hammock properties \naffected by each foot of sea-level rise")
dev.off()

# prints transparent png for slide
png('figures/hh.prop.png', width=5, height=5, units="in", res=300, bg="transparent")
par(cex=1.2)

# plots data in bar chart
cols1 <- c("#BEFFE8",  "#73DFFF", "#00C5FF", "#00A9E6")
midpoints <- barplot(prcl, names.arg=x, ylim=c(0, 300), xlab="Sea-level rise (feet)", 
                     ylab="Number of properties", col=cols1, col.lab="black", col.axis="black")
text(midpoints, 20, labels=prcl)
##title("Number of Hog Hammock properties \naffected by each foot of sea-level rise")
dev.off()


##########
# creates vector for percent area affected by SLR
area <- c(2, 20, 27, 30, 21)
lbls.area <- c("2", "20", "27", "30", "21")

# # prints tif
# tiff('figures/hh.area.bar.tif', width=5, height=5, units="in", res=300)

#this returns transparent png
png('figures/hh.area.png',width=5,height=5,units="in", res=300, bg = "transparent")
par(cex=1)

# Specify axis options within plot(); type="n" tells plot () not to graph anything yet 
par(xpd=TRUE, 
    mar=c(10,2,0,2)+0.1) # cex() sets size of text and tick marks relative to tiff; mar() sets margin of bottom, left, top, and right

# plots horizontal bar chart, single stacked column
cols2 <- c( "#BEFFE8",  "#73DFFF", "#00C5FF", "#00A9E6", "white")
area.1 <- rbind(2, 20, 27, 30)
lgnd.1 <- c("1 foot","2 feet", "3 feet", "4 feet")
barplot (area.1, horiz=TRUE, col=cols2, xlim=c(0,100), ylim=c(0,8), xlab="Percent area")
##title("Percentage area of Hog Hammock \naffected by each foot of sea-level rise", line=-6)
legend(11, -3, lgnd.1, fill=cols2, bty = "n", ncol=4)
dev.off()

################################################
## barplot of parcel status
################################################

# create data and label vectors for property number affected by SLR
stat <- c(182, 42, 72)
acre <- c(219, 144, 57)
ha <- c(89, 59, 23)
lbls.stat <- c("182", "42", "72")
lgnd.1 <- c("Geechee Descendant", "Heritage Authority", "Non-traditional")

# prints transparent png for slide
png('figures/hh.stat.png', width=5, height=5, units="in", res=300, bg="transparent")
par(cex=1.3)

# plots data in bar chart
cols1 <- c("#4e4e4e",  "grey", "white")
midpoints <- barplot(stat, ylim=c(0, 200), ylab="Parcels", col=cols1)
text(midpoints, 20, labels=stat)
dev.off()


# prints transparent png for slide
png('figures/hh.hectare.png', width=5, height=5, units="in", res=300, bg="transparent")
par(cex=1.3)

# plots data in bar chart
cols1 <- c("#4e4e4e",  "grey", "white")
midpoints <- barplot(ha, ylim=c(0, 100), ylab="Hectares", col=cols1)
text(midpoints, 10, labels=ha)
legend(11, -3, lgnd.1, fill=cols1, bty = "n", ncol=4)
dev.off()


##########################################################
## analyses of parcel inundation by social group status
##########################################################

## import data
inund <- read.csv("data/slr/prclhh_inund_status.csv", header=T)
ha <- (inund[,2:9])* 9.2903e-6 ## conversion of 9 square feet grid cells to hectare area measure
sum(rowSums(ha)) ## check that the sum equals areas of Hog Hammock (~166 ha)


##################################################
## calculations based on total area
in.perc <- (ha/(sum(rowSums(ha))))*100 ## percentage of each social group affected relative to total area

## percentage sums by one foot increments (x#) of SLR
x6 <- sum(rowSums(in.perc[,2:7]))
x5 <- sum(rowSums(in.perc[,2:6]))
x4 <- sum(rowSums(in.perc[,2:5]))
x3 <- sum(rowSums(in.perc[,2:4]))
x2 <- sum(rowSums(in.perc[,2:3]))
x1 <- colSums(in.perc[2])

## percentage of sg's area inundated by one foot increments (#) of SLR relative to total area (t)
sgt.6 <- rowSums(in.perc[,2:7])
sgt.5 <- rowSums(in.perc[,2:6])
sgt.4 <- rowSums(in.perc[,2:5])
sgt.3 <- rowSums(in.perc[,2:4])
sgt.2 <- rowSums(in.perc[,2:3])
sgt.1 <- in.perc[2]


##################################################
## calculations based on social group area owned
sg.area <- rowSums(ha[,2:8]) ## total area owned by each social group (sg)
sgin.perc <- (ha/sg.area)*100 ## percentage area inundated of each social group's total holdings

## percentage of social group's area inundated by one foot increments (#) of SLR relative to sg's total area
sg6 <- rowSums(sgin.perc[,2:7])
sg5 <- rowSums(sgin.perc[,2:6])
sg4 <- rowSums(sgin.perc[,2:5])
sg3 <- rowSums(sgin.perc[,2:4])
sg2 <- rowSums(sgin.perc[,2:3])
sg1 <- sgin.perc[2]

sg.inund <- cbind(sg1, sg2, sg3, sg4, sg5, sg6) ## combine all into single data.frame
names(sg.inund)[1] <- "sg1"
sg.inund <- t(sg.inund) ## transpose data.frame into matrix with social groups as columns
colnames(sg.inund) <- c("Geechee", "Heritage Authority","Non-traditional") ## add column names
sg.inund <- t(sg.inund) ## transpose data.frame into matrix with social groups as rows
colnames(sg.inund) <- c("0.3", "0.6","0.9", "1.2", "1.5", "1.8") ## add column names

cols1 <- c("#4e4e4e",  "grey", "white") ## setting colors for graphing barplots

# prints tif
tiff('figures/hh.sg.inund.tif', width=5, height=5, units="in", res=300)
par(cex=1)
barplot((sg.inund[,1:4]), beside=TRUE, ylim=c(0,100), xlab="Inundation (meters)", 
        ylab="Percent Area of Social Group Holdings")
dev.off()

# prints png
png('figures/hh.sg.inund.png', width=5, height=5, units="in", res=300, bg="transparent")
par(cex=1.3)
barplot((sg.inund[,1:4]), beside=TRUE, ylim=c(0,100), xlab="Inundation (meters)", 
        ylab="Percent Area", col=cols1, main="Relative Parcel Inundation \nby Owner Group")
dev.off()


