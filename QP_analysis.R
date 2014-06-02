##########################################################
#This script is used for analizing effect of different QP values
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
                           RawData$VSP_Enable== "Enable", ]
QP_SubData <- SubData[SubData$Depth_QPISlice!=40 & 
                              SubData$Depth_QPISlice!=26, ]



## Step 3: Subset by variables

QP_Data <- data.frame(PSNR=QP_SubData$AVE_PSNR, 
                      Ratio=QP_SubData$Ratio,
                      QP_texture=QP_SubData$Texture_QPISlice)


## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
QP_value <- aggregate(PSNR ~ Ratio + QP_texture, data = QP_Data, FUN = sum)
with(QP_value, qplot(Ratio, PSNR, col = QP_texture, 
                      geom = "point",
                      xlab = "Ratio",
                      ylab = "PSNR (dB)",
                      asp = 0.85, 
                      main = "QUANTIZATION PARAMETER"))


## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="QP_analysis.png")
dev.off()



# For optional format for analysis
# Txt format of raw data as well as organized data is supplied
write.table(RawData, file="./RawData.txt")
write.table(QP_Data, file="./QP_Data.txt")
