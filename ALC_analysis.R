##########################################################
#This script is used for analyzing effect of ALC algorithm
##########################################################

## Step 1: read raw data from work directory
# Uncomment next line if xlsx package haven't been installed
#install.packages("xlsx")
# Load dependent library so that .xlsx file can be read
library(rJava)
library(xlsxjars)
library(xlsx)
RawData <- read.xlsx(file="./RawData.xlsx", sheetIndex=1)

## Step 2: Subset by observations
# With VSP enable and DMVP enable
# Part of quantization parameters (QP) are chosen
SubData <- RawData[RawData$VSP_Enable=="Enable" & 
                           RawData$DepthBaseMVP=="Enable" & 
                           RawData$FrameToBeEncoded==100, ]
ALC_SubData <- SubData[
        SubData$Texture_QPISlice==SubData$Depth_QPISlice, ]

## Step 3: Subset by variables
ALC_sData <- data.frame(OnOff=ALC_SubData$AdaptiveLuminanceCompensation, 
                       Rate=ALC_SubData$SUM_Rate, 
                       PSNR=ALC_SubData$AVE_PSNR)
# Order by OnOff variable for precise observation
ALC_Data <- ALC_sData[order(ALC_sData$OnOff),]

## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
ALC_OnOff <- aggregate(PSNR ~ Rate + OnOff, data = ALC_Data, FUN = sum)
with(ALC_OnOff, qplot(Rate, PSNR, col = OnOff, 
                       geom = c("point", "line"),
                       xlab = "Bit Rate (Kbps)",
                       ylab = "PSNR (dB)",
                       main = "ADAPTIVE LUMINANCE COMPENSATION"))

## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="ALC_analysis.png")
dev.off()

## Step 6: Analize the average value of PSNR based on difference QP_texture
AVE_ALC_Difference_PSNR <- mean(ALC_Data[ALC_Data$OnOff=="Enable",]$PSNR) - 
        mean(ALC_Data[ALC_Data$OnOff=="Disable",]$PSNR)

## Step 7: Compare the difference when ALC is enable or disable
ALC_Difference_Rate <- vector()
for (i in 1:(length(ALC_Data$OnOff)/2)) {
        ALC_Difference_Rate <- cbind(ALC_Difference_Rate, (ALC_Data[ALC_Data$OnOff=="Disable",]$Rate[i] - 
                                                           ALC_Data[ALC_Data$OnOff=="Enable",]$Rate[i]) / 
                                         ALC_Data[ALC_Data$OnOff=="Disable",]$Rate[i])
}
AVE_ALC_Difference_Rate <- mean(ALC_Difference_Rate)

# For optional format for analysis
# Txt format of raw data as well as organized data is supplied
write.table(RawData, file="./RawData.txt")
write.table(ALC_Data, file="./ALC_Data.txt")
