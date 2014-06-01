##########################################################
#This script is used for analizing effect of DMVP algorithm
##########################################################


## Step 1: read raw data from work directory

# Uncomment next line if xlsx package haven't been installed
#install.packages("xlsx")

# Load dependent library so that .xlsx file can be read
library(xlsxjars)
library(xlsx)
RawData <- read.xlsx(file="./RawData.xlsx", sheetIndex=1)




## Step 2: Subset by observations

# With VSP disable
DMVP_SubData <- RawData[RawData$VSP_Enable=="Disable", ]



## Step 3: Subset by variables

DMVP_Data <- data.frame(OnOff=DMVP_SubData$DepthBaseMVP, 
                        Rate=DMVP_SubData$SUM_Rate, 
                        PSNR=DMVP_SubData$AVE_PSNR)


## Step 4: Use ggplot package to plot graph
# If ggplot2 package haven't been installed, please uncomment next line
#install.packages("ggplot2")
# Load ggplot2 library, which implements the grammar of graphics
library(ggplot2)
DMVP_OnOff <- aggregate(PSNR ~ Rate + OnOff, data = DMVP_Data, FUN = sum)
with(DMVP_OnOff, qplot(Rate, PSNR, col = OnOff, 
                      geom = c("point", "line"),
                      xlab = "Rate (Kbps)",
                      ylab = "PSNR (dB)",
                      asp = 0.85, 
                      main = "DEPTH BASED MOTION VECTOR PREDICTION"))


## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="DMVP_analysis.png")
dev.off()