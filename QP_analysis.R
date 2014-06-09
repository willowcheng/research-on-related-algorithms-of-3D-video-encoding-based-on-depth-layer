##########################################################
#This script is used for analyzing effect of different QP values
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
SubData <- RawData[RawData$DepthBaseMVP== "Enable" & 
                           RawData$AdaptiveLuminanceCompensation== "Enable" &
                           RawData$VSP_Enable== "Enable" & 
                           RawData$FrameToBeEncoded==100, ]
QP_SubData <- SubData[SubData$Depth_QPISlice!=40 & 
                              SubData$Depth_QPISlice!=26, ]

## Step 3: Subset by variables
QP_Data <- data.frame(PSNR=QP_SubData$AVE_PSNR, 
                      QP_texture=QP_SubData$Texture_QPISlice,
                      Rate=QP_SubData$SUM_Rate)

## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
QP_value <- aggregate(PSNR ~ Rate + QP_texture, data = QP_Data, FUN = sum)
with(QP_value, qplot(Rate, PSNR, col = QP_texture, 
                     geom = "point",
                     xlab = "Bit Rate (Kbps)",
                     ylab = "PSNR (dB)",
                     main = "QUANTIZATION PARAMETER"))

## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="QP_analysis.png")
dev.off()

## Step 6: Analize the average value of PSNR depending on difference QP_texture
PSNR_Statistic <- c(mean(QP_SubData[QP_SubData$Texture_QPISlice==26,]$AVE_PSNR), 
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==28,]$AVE_PSNR),
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==30,]$AVE_PSNR),
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==32,]$AVE_PSNR),
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==34,]$AVE_PSNR),
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==36,]$AVE_PSNR),
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==38,]$AVE_PSNR),
                        mean(QP_SubData[QP_SubData$Texture_QPISlice==40,]$AVE_PSNR))
PSNR_Difference <- vector()

## Step 7: Get the mean value of PSNR_Difference
for (i in 1:(length(PSNR_Statistic)-1)) {
        PSNR_Difference <- cbind(PSNR_Difference, PSNR_Statistic[i] - PSNR_Statistic[i+1])
}
AVE_QP_Difference_PSNR <- mean(PSNR_Difference)

## Step 8: Analizing rate of average based on different QP
Rate_Statistic <- c(mean(QP_SubData[QP_SubData$Texture_QPISlice==26,]$SUM_Rate), 
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==28,]$SUM_Rate),
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==30,]$SUM_Rate),
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==32,]$SUM_Rate),
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==34,]$SUM_Rate),
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==36,]$SUM_Rate),
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==38,]$SUM_Rate),
                    mean(QP_SubData[QP_SubData$Texture_QPISlice==40,]$SUM_Rate))

## Step 10: Get the mean value of Rate_Difference
QP_Rate_Difference <- vector()
for (i in 1:(length(Rate_Statistic)-1)) {
        QP_Rate_Difference <- cbind(QP_Rate_Difference, 
                                 (Rate_Statistic[i] - Rate_Statistic[i+1])/Rate_Statistic[i])
}
AVE_QP_Rate_Difference <- mean(QP_Rate_Difference)

# For optional format for analysis
# Txt format of raw data as well as organized data is supplied
write.table(RawData, file="./RawData.txt")
write.table(QP_Data, file="./QP_Data.txt")
