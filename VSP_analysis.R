##########################################################
#This script is used for analyzing effect of VSP algorithm
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
# With DMVP enable and ALC enable 
# Part of quantization parameter (QP) values are chosen
SubData <- RawData[RawData$DepthBaseMVP=="Enable" & 
                           RawData$AdaptiveLuminanceCompensation=="Enable" & 
                           RawData$FrameToBeEncoded==100, ]
VSP_SubData <- SubData[SubData$Texture_QPISlice==SubData$ Depth_QPISlice, ]

## Step 3: Subset by variables
VSP_sData <- data.frame(OnOff=VSP_SubData$VSP_Enable, 
                       Rate=VSP_SubData$SUM_Rate, 
                       PSNR=VSP_SubData$AVE_PSNR)
# Order by OnOff variable for precise observation
VSP_Data <- VSP_sData[order(VSP_sData$OnOff),]

## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
VSP_OnOff <- aggregate(PSNR ~ Rate + OnOff, data = VSP_Data, FUN = sum)
with(VSP_OnOff, qplot(Rate, PSNR, col = OnOff, 
                              geom = c("point", "line"),
                              xlab = "Bit Rate (Kbps)",
                              ylab = "PSNR (dB)",
                              main = "VIEW SYNTHESIS PREDICTION"))

## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="VSP_analysis.png")
dev.off()

## Step 6: Analize the average value of PSNR based on difference QP_texture
AVE_VSP_Difference_PSNR <- mean(VSP_Data[VSP_Data$OnOff=="Enable",]$PSNR) - 
        mean(VSP_Data[VSP_Data$OnOff=="Disable",]$PSNR)

## Step 7: Compare the difference when VSP is enable or disable
VSP_Difference_Rate <- vector()
for (i in 1:(length(VSP_Data$OnOff)/2)) {
        VSP_Difference_Rate <- cbind(VSP_Difference_Rate, (VSP_Data[VSP_Data$OnOff=="Disable",]$Rate[i] - 
                                                                   VSP_Data[VSP_Data$OnOff=="Enable",]$Rate[i]) / 
                                             VSP_Data[VSP_Data$OnOff=="Disable",]$Rate[i])
}
AVE_VSP_Difference_Rate <- mean(VSP_Difference_Rate)

# For optional format for analysis
# Txt format of raw data as well as organized data is supplied
write.table(RawData, file="./RawData.txt")
write.table(VSP_Data, file="./VSP_Data.txt")
