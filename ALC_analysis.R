##########################################################
#This script is used for analizing effect of ALC algorithm
##########################################################


## Step 1: read raw data from work directory

# Uncomment next line if xlsx package haven't been installed
#install.packages("xlsx")

# Load dependent library so that .xlsx file can be read
library(xlsxjars)
library(xlsx)
RawData <- read.xlsx(file="./RawData.xlsx", sheetIndex=1)




## Step 2: Subset by observations

# With VSP enable and DMVP enable
# Part of quantization parameters (QP) are chosen
SubData <- RawData[RawData$VSP_Enable=="Enable" & 
                           RawData$DepthBaseMVP=="Enable", ]
ALC_SubData <- SubData[
        SubData$Texture_QPISlice==SubData$Depth_QPISlice, ]



## Step 3: Subset by variables

ALC_Data <- data.frame(OnOff=ALC_SubData$AdaptiveLuminanceCompensation, 
                       Rate=ALC_SubData$SUM_Rate, 
                       PSNR=ALC_SubData$AVE_PSNR)


## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
ALC_OnOff <- aggregate(PSNR ~ Rate + OnOff, data = ALC_Data, FUN = sum)
with(ALC_OnOff, qplot(Rate, PSNR, col = OnOff, 
                       geom = c("point", "line"),
                       xlab = "Rate (Kbps)",
                       ylab = "PSNR (dB)",
                       asp = 0.85, 
                       main = "ADAPTIVE LUMINANCE COMPENSATION"))


## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="ALC_analysis.png")
dev.off()