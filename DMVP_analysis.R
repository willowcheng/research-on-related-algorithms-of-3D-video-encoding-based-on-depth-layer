##########################################################
#This script is used for analizing effect of DMVP algorithm
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
                      main = "DEPTH BASED MOTION VECTOR PREDICTION"))


## Step 5: Copy graph from device into hard disk in work directory
dev.copy(png, file="DMVP_analysis.png")
dev.off()


## Step 6: Analize the average value of PSNR depending on difference QP_texture
DMVP_Difference_PSNR <- mean(DMVP_Data[DMVP_Data$OnOff=="Enable",]$PSNR) - 
        mean(DMVP_Data[DMVP_Data$OnOff=="Disable",]$PSNR)

## Step 7: Compare the difference when DMVP is enable or disable
DMVP_Difference_Rate <- vector()
for (i in 1:(length(DMVP_Data$OnOff)/2)) {
        DMVP_Difference_Rate <- cbind(DMVP_Difference_Rate, (DMVP_Data[DMVP_Data$OnOff=="Disable",]$Rate[i] - 
                                         DMVP_Data[DMVP_Data$OnOff=="Enable",]$Rate[i]) / 
                                         DMVP_Data[DMVP_Data$OnOff=="Disable",]$Rate[i])
}
AVE_DMVP_Difference_Rate <- mean(DMVP_Difference_Rate)
# For optional format for analysis
# Txt format of raw data as well as organized data is supplied
write.table(RawData, file="./RawData.txt")
write.table(DMVP_Data, file="./DMVP_Data.txt")
