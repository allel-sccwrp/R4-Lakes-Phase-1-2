#### Notes ####
# 2023 01 11 AL





#################### Set up ######################

library(stringr)
library(dplyr)
library(ggplot2)
library(scales)
library(Cairo)
library(gridExtra)



#Mac
#setwd("/Users/allel/OneDrive - SCCWRP/00 R/R4/Chl vs MC/")
#Windows
setwd("C:/Users/Fenri/OneDrive - SCCWRP/00 R/R4/Chl vs MC")




###################### Data load in and processing ###################

#NLA.raw <- read.csv("dat.merge.all.tp.ca.csv", stringsAsFactors = FALSE, header =TRUE)
#df.NLA <- NLA.raw

#Subset to only contain NLA data
#df.NLA.noCA <- subset(df.NLA, !(datasource == "CA_public_data"))
#columns that matter: chl and mc; mc column has a bunch of NA

#write.csv(df.NLA.noCA, "NLA TP.csv", row.names=FALSE, quote=FALSE)

data.raw <- read.csv("R4 NLA Chl MC Compiled.csv", stringsAsFactors = FALSE, header = TRUE)


data <- data.raw

#Turn MC data to numeric, below detection data changed to NA
data$MC <- as.numeric(data$MC)

#There are a number of NLA Chl data that's NA, to display them:
#data[is.na(data$Chl),]
#Display max value ignoring NA
#data[which.max(data$Chl),]
#max = 3299.18




#Remove rows with NA values in the Chl column
data.noNA <- na.omit(data) #This removes rows with any NA = 1492

#For some reason these commands don't work at removing NAs
#data.noNA <- data[!is.na(data$Chl),] #From 5033 to 5014
#data.noNA <- data[!is.na(data.noNA$MC),]


data.MCto0 <- data
data.MCto0$MC[is.na(data.MCto0$MC)] <- 0

data.MCto0 <- na.omit(data.MCto0)




data.shoreonly <- data.noNA[data.noNA$Location == "" | data.noNA$Location == "IX" | data.noNA$Location == "IX2",]



#################### Plotting ##############################




#All R4 data (phase 1 + 2)
#This version wasn't used
#ggplot(data=data, aes(x=Chl, y=MC, color=Data_Source)) +
#  geom_point() +
#  scale_color_manual(values = c("gray","blue","dark green")) +
#  scale_x_continuous(trans = "log10") +
#  scale_y_continuous(trans = "log10") +
  #geom_errorbar(aes(ymin=ug.L_mean-ug.L_sd, ymax=ug.L_mean+ug.L_sd), width=0.2, position=position_dodge(0.9)) +
#  ylab("Microcystins concentration (μg/L)") +
#  xlab("Chlorophyll a concentration (μg/L)") +
  #scale_x_continuous(n.breaks=7) +
#  theme_classic() +
#  theme(text=element_text(size=15))


CairoPDF("R4 NLA Chl vs MC.pdf", width = 8, height = 6)

ggplot(data=data.noNA, aes(x=Chl, y=MC, color=Data_Source, shape=Data_Source)) +
  geom_point(size=2) +
  scale_color_manual(values = c("gray","gray","blue","dark green")) +
  scale_shape_manual(values = c(1,1,16,16)) +
  scale_x_continuous(trans = "log10", labels=label_comma()) +
  scale_y_continuous(trans = "log10") +
  geom_vline(xintercept=c(2.6, 7.3, 56), linetype='dashed') +
  #geom_errorbar(aes(ymin=ug.L_mean-ug.L_sd, ymax=ug.L_mean+ug.L_sd), width=0.2, position=position_dodge(0.9)) +
  ylab("Microcystins concentration (μg/L)") +
  xlab("Chlorophyll a concentration (μg/L)") +
  #scale_x_continuous(n.breaks=7) +
  theme_classic() +
  theme(text=element_text(size=15))

dev.off()


#Final version used
#Inkscape modification: Dragged down the x-axis -> Took the row of 0 MC data, copied, and vertically flipped the copied version (because the circles weren't complete) -> gave this row a special 'below detection' label on the y-axis
CairoPDF("R4 NLA Chl vs MC (MC to 0).pdf", width = 8, height = 6)

#MC changed to 0 instead of removing
ggplot(data=data.MCto0, aes(x=Chl, y=MC, color=Data_Source, shape=Data_Source)) +
  geom_point(size=2) +
  scale_color_manual(values = c("gray","gray","gray","blue","dark green")) +
  scale_shape_manual(values = c(1,1,1,16,16)) +
  scale_x_continuous(trans = "log10", labels=label_comma()) +
  scale_y_continuous(trans = "log10") +
  geom_vline(xintercept=c(2.6, 7.3, 56), linetype='dashed') +
  #geom_errorbar(aes(ymin=ug.L_mean-ug.L_sd, ymax=ug.L_mean+ug.L_sd), width=0.2, position=position_dodge(0.9)) +
  ylab("Microcystins concentration (μg/L)") +
  xlab("Chlorophyll a concentration (μg/L)") +
  #scale_x_continuous(n.breaks=7) +
  theme_classic() +
  theme(text=element_text(size=15))

dev.off()







#Shore only
ggplot(data=data.shoreonly, aes(x=Chl, y=MC, color=Data_Source, shape=Data_Source)) +
  geom_point(size=2) +
  scale_color_manual(values = c("gray","gray","gray","blue","dark green")) +
  scale_shape_manual(values = c(1,1,1,16,16)) +
  scale_x_continuous(trans = "log10", labels=label_comma()) +
  scale_y_continuous(trans = "log10") +
  geom_vline(xintercept=c(2.6, 7.3, 56), linetype='dashed') +
  #geom_errorbar(aes(ymin=ug.L_mean-ug.L_sd, ymax=ug.L_mean+ug.L_sd), width=0.2, position=position_dodge(0.9)) +
  ylab("Microcystins concentration (μg/L)") +
  xlab("Chlorophyll a concentration (μg/L)") +
  #scale_x_continuous(n.breaks=7) +
  theme_classic() +
  theme(text=element_text(size=15))








#With labels just for info
ggplot(data=data.noNA, aes(x=Chl, y=MC, color=Data_Source)) +
  geom_point() +
  scale_color_manual(values = c("gray","blue","dark green")) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  #geom_errorbar(aes(ymin=ug.L_mean-ug.L_sd, ymax=ug.L_mean+ug.L_sd), width=0.2, position=position_dodge(0.9)) +
  ylab("Microcystins concentration (μg/L)") +
  xlab("Chlorophyll a concentration (μg/L)") +
  geom_text(aes(label = Station), size = 5, nudge_y = -0.1, nudge_x = 0.1) +
  #scale_x_continuous(n.breaks=7) +
  theme_classic() +
  theme(text=element_text(size=15))



