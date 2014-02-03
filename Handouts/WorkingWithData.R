

options(width = 80)
opts_chunk$set(comment = NA, cache = TRUE, fig.height = 5, fig.width = 5, message = FALSE, warning = FALSE, fig.align = 'center')
require(ggplot2)
require(e1071)
require(plyr)



site <- "http://www1.appstate.edu/~arnholta/PASWR/CD/data/Poplar3.CSV"
poplar <- read.csv(file = url(site))
head(poplar, n = 5)  # show first five rows



str(poplar)
summary(poplar)



poplarC <- read.csv(file = url(site), na.strings = "-99", 
    colClasses = c(rep("factor", 3), rep("numeric", 3), "factor"))
str(poplarC)



## DF <- read.table(file=url(site), header=TRUE)
## df <- DF
## df[df$var1==999,  "var1"] = NA
## df[df$var2==99,   "var2"] = NA
## df[df$var3==9999, "var3"] = NA



levels(poplarC$Site) <- c("Moist", "Dry")
TreatmentLevels <- c("Control", "Fertilizer", "Irrigation", "FertIrriga")
levels(poplarC$Treatment) <- TreatmentLevels
str(poplarC$Treatment)



poplarC$Site <- factor(poplarC$Site, labels = c("Moist", "Dry"))
str(poplarC$Site)



poplar$Site <- factor(poplar$Site, levels = c("Moist", "Dry"))
str(poplar$Site)


