cd /host-rootfs/sdcard/Download
sudo R
library(dplyr)
library(xml2)
library(stringi)
library(stringr)
library(rvest)
library('lpSolve')
library(corrplot)
library(lpSolveAPI)

FDSal2 <- read.csv('8_12_21_FDpga_merge_roughh2.csv', stringsAsFactors=FALSE)
FDSal2$Name <- FDSal2$Nickname 
#FDSal$Nickname <- paste(FDSal$First.Name, FDSal$Last.Name)
#FDSal2 <-subset(FDSal, select = c(Nickname, Salary, Team, Id))
#names(FDSal2) <- c("Name", "Salary", "Team", "ID")

FDSal2$Name <- gsub("[',]", "", FDSal2$Name)
FDSal2$Name <- gsub("[.-]", "", FDSal2$Name)
FDSal2$Name <- gsub(" Jr| Sr| II| III| IV", "", FDSal2$Name)
FDSal2$Name <- as.character(FDSal2$Name)

uninamesx <- data.frame(table(FDSal2$Name))
uninamesx
write.csv(uninamesx, file='uninamesx.csv', row.names=FALSE)
plyr_sd <- read.csv('FD Top Golfers.csv')

plyr_sd$Name <- gsub("\\(PG\\)|\\(SG\\)|\\(SF\\)|\\(PF\\)|\\(C\\)", "", plyr_sd$Name)
plyr_sd$Name <- gsub("[\n]", "", plyr_sd$Name)
plyr_sd$Name <- gsub("[',]", "", plyr_sd$Name)
plyr_sd$Name <- gsub("[.-]", "", plyr_sd$Name)
plyr_sd$Name <- gsub("^\\s+|\\s+$", "", plyr_sd$Name)
plyr_sd$Name <- gsub(" Jr| Sr| II| III| IV", "", plyr_sd$Name)

rztar <- read.csv('pga2021bogeyavoidance (1).csv')
rztar$Name <- gsub("\\(PG\\)|\\(SG\\)|\\(SF\\)|\\(PF\\)|\\(C\\)", "", rztar$Name)
rztar$Name <- gsub("[\n]", "", rztar$Name)
rztar$Name <- gsub("[',]", "", rztar$Name)
rztar$Name <- gsub("[.-]", "", rztar$Name)
rztar$Name <- gsub("^\\s+|\\s+$", "", rztar$Name)
rztar$Name <- gsub(" Jr| Sr| II| III| IV", "", rztar$Name)

rzrush <- read.csv('pga2021drivingdistance (1).csv')
rzrush$Name <- gsub("\\(PG\\)|\\(SG\\)|\\(SF\\)|\\(PF\\)|\\(C\\)", "", rzrush$Name)
rzrush$Name <- gsub("[\n]", "", rzrush$Name)
rzrush$Name <- gsub("[',]", "", rzrush$Name)
rzrush$Name <- gsub("[.-]", "", rzrush$Name)
rzrush$Name <- gsub("^\\s+|\\s+$", "", rzrush$Name)
rzrush$Name <- gsub(" Jr| Sr| II| III| IV", "", rzrush$Name)

nfjoin <- read.csv('pga2021appfromgt200yd (1).csv', stringsAsFactors=FALSE)

nfjoin$Name <- gsub("[\n]", "", nfjoin$Name)
nfjoin$Name <- gsub("[',]", "", nfjoin$Name)
nfjoin$Name <- gsub("[.-]", "", nfjoin$Name)
nfjoin$Name <- gsub("^\\s+|\\s+$", "", nfjoin$Name)
nfjoin$Name <- gsub(" Jr| Sr| II| III| IV", "", nfjoin$Name)

nfjoin2 <- read.csv('1623736280599_datagolf_trends.csv', stringsAsFactors=FALSE)

nfjoin2$Name <- gsub("[\n]", "", nfjoin2$Name)
nfjoin2$Name <- gsub("[',]", "", nfjoin2$Name)
nfjoin2$Name <- gsub("[.-]", "", nfjoin2$Name)
nfjoin2$Name <- gsub("^\\s+|\\s+$", "", nfjoin2$Name)
nfjoin2$Name <- gsub(" Jr| Sr| II| III| IV", "", nfjoin2$Name)

nfjoin3 <- read.csv('CHTorreyPines_FarmersInsurance (1).csv', stringsAsFactors=FALSE)

nfjoin3$Name <- gsub("[\n]", "", nfjoin3$Name)
nfjoin3$Name <- gsub("[',]", "", nfjoin3$Name)
nfjoin3$Name <- gsub("[.-]", "", nfjoin3$Name)
nfjoin3$Name <- gsub("^\\s+|\\s+$", "", nfjoin3$Name)
nfjoin3$Name <- gsub(" Jr| Sr| II| III| IV", "", nfjoin3$Name)

nfjoin4 <- read.csv('EventHistoryUSOpen (1).csv', stringsAsFactors=FALSE)

nfjoin4$Name <- gsub("[\n]", "", nfjoin4$Name)
nfjoin4$Name <- gsub("[',]", "", nfjoin4$Name)
nfjoin4$Name <- gsub("[.-]", "", nfjoin4$Name)
nfjoin4$Name <- gsub("^\\s+|\\s+$", "", nfjoin4$Name)
nfjoin4$Name <- gsub(" Jr| Sr| II| III| IV", "", nfjoin4$Name)


joinedFD <- left_join(FDSal2, plyr_sd, by = "Name")
joinedFD$Name <- as.character(joinedFD$Name)
joinedyFD <- left_join(joinedFD, rztar, by = "Name")
joinedzFD <- left_join(joinedyFD, rzrush, by = "Name")
joinedqFD <- left_join(joinedzFD, nfjoin, by = "Name")
joinedxFD$Name <- as.character(joinedxFD$Name)
joined2FD <- left_join(joinedqFD, nfjoin2, by = "Name")
jz <- str_split_fixed(joined2FD$Name, " ", 2)
jz
nm <- substr(jz[,1], 0, 1)
joined2FD$Name <- paste(nm, jz[,2])
nfjoin3$Name <- as.character(nfjoin3$Name)
joined3FD <- left_join(joined2FD, nfjoin3, by = "Name")
joined4FD <- left_join(joined3FD, nfjoin4, by = "Name")
uninames <- data.frame(table(joinedxFD$Name))
uninames
write.csv(uninames, file='nfluninames.csv', row.names=FALSE)
write.csv(joinedFD, file='joinedFD.csv', row.names=FALSE)

#joinedxFD <- read.csv('joinedxFD (1).csv', stringsAsFactors=FALSE)
#joined2FD <- left_join(joinedxFD, nfjoin2, by = "Name")

joinedFDc <- left_join(FDSal2, plyr_sd, by = "Name")
joined2FDc <- left_join(joinedFDc, nfjoin, by = "Name")
df <- joined2FDc
new_DF <- df[rowSums(is.na(df)) > 0,]
head(new_DF, n=1)
new_DF$Ceil <- as.numeric(new_DF$Ceil)
DF <- new_DF
DF

#write.csv(joined2FD, file='nfldatajoinwk16snf.csv', row.names=FALSE)
write.csv(DF, file='DFwk16snf.csv', row.names=FALSE)

#54232

