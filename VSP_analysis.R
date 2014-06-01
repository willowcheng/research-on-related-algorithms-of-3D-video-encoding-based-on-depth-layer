##########################################################
#This script is used for analizing effect of VSP algorithm
##########################################################


## Step 1: read raw data from work directory
# Uncomment next line if xlsx package haven't been installed
#install.packages("xlsx")
# Load dependent library so that .xlsx file can be read
library(xlsxjars)
library(xlsx)
RawData <- read.xlsx(file="./RawData.xlsx", sheetIndex=1)


## Step 2: Subset by observations
# With DMVP enable and ALC enable 
# Part of quantization parameter (QP) values are chosen
SubData <- RawData[RawData$DepthBaseMVP=="Enable" & 
                           RawData$AdaptiveLuminanceCompensation=="Enable", ]
VSP_SubData <- SubData[SubData$Texture_QPISlice==SubData$ Depth_QPISlice, ]


## Step 3: Subset by variables
VSP_Data <- data.frame(OnOff=VSP_SubData$VSP_Enable, 
                       Rate=VSP_SubData$SUM_Rate, 
                       PSNR=VSP_SubData$AVE_PSNR)


## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
VSP_OnOff <- aggregate(PSNR ~ Rate + OnOff, data = VSP_Data, FUN = sum)
with(VSP_OnOff, qplot(Rate, PSNR, col = OnOff, 
                              geom = c("point", "line"),
                              xlab = "Rate (Kbps)",
                              ylab = "PSNR (dB)",
                              asp = 0.85, 
                              main = "VIEW SYNTHESIS PREDICTION"))


## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="VSP_analysis.png")
dev.off()



