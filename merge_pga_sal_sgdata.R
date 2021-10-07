cd /host-rootfs/sdcard/Download
sudo R

library(dplyr)
library(xml2)
library(stringi)
library(stringr)
library(rvest)
library('lpSolve')


sg_data <- read.csv('dg_performance_2021.csv', stringsAsFactors=FALSE)
FDSal <- read.csv('FanDuel-PGA-2021 ET-08 ET-12 ET-62531-players-list.csv', stringsAsFactors=FALSE)


FDSal$player_name <- paste(FDSal$Last.Name, ", ", FDSal$First.Name, sep="")

FDSal$player_name <- gsub("[',]", "", FDSal$player_name)
FDSal$player_name <- gsub("[.-]", "", FDSal$player_name)
FDSal$player_name <- gsub(" Jr| Sr| II| III| IV", "", FDSal$player_name)
FDSal2

sg_data$player_name <- gsub("[',]", "", sg_data$player_name)
sg_data$player_name <- gsub("[.-]", "", sg_data$player_name)
sg_data$player_name <- gsub(" Jr| Sr| II| III| IV", "", sg_data$player_name)
sg_data$player_name <- gsub("Fitzpatrick Matthew", "Fitzpatrick Matt", sg_data$player_name)
sg_data$player_name <- gsub("Lee Kyounghoon", "Lee KyoungHoon", sg_data$player_name)
sg_data

joined2FD <- left_join(FDSal, sg_data, by = "player_name")
joined2FD$Name <- as.character(joined2FD$Name)

head(joined2FD)
write.csv(joined2FD, file='8_12_21_FDpga_merge_roughh2.csv', row.names=FALSE)
#


joined2FDc <- right_join(sg_data, FDSal, by = "player_name")
#joined2FDc
#joined2FDc <- left_join(joinedxFDc, f5x53FD, by = "Name")
#joinedzFDc$Name <- as.character(joinedzFDc$Name)
#jz <- str_split_fixed(joinedzFDc$Name, " ", 2)
#nm <- substr(jz[,1], 0, 1)
#joinedzFDc$Name <- paste(nm, jz[,2])
#joined2FDc <- left_join(joinedzFDc, dff4, by = "Name")
df <- joined2FDc
#df <- joined2FDc
new_DF <- df[is.na(df$putt_true) > 0,]
#head(new_DF, n=1)
DF  <- subset(new_DF, select = c(player_name, Salary))
DF
