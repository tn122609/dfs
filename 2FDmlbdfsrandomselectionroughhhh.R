cd /host-rootfs/sdcard/Download
sudo R

library(dplyr)
library(xml2)
library(stringi)
library(stringr)
library(rvest)
library('lpSolve')

plistx <- read.csv('FanDuel-MLB-2020-06-04-46016-players-list.csv', stringsAsFactors=FALSE)
head(plistx)

#plistx$FPPG[plistx$FPPG == 0] <- 6.9

plistx$FPPGpSal <- plistx$FPPG/(plistx$Salary/1000)



plist <- plistx[complete.cases(plistx[ ,16]),]
plist

pl1t8 <-plist[!(plist$Batting.Order > 8),]
pl1t4 <-plist[!(plist$Batting.Order > 4),]
length(pl1t4[,1])
length(pl1t8[,1])

tmlist <- unique(plist$Team)

agg1t8 <- aggregate(pl1t8$FPPGpSal, list(pl1t8$Team), mean)
agg1t8 <- agg1t8[order(agg1t8$Group.1),]

aggFP1t8 <- aggregate(pl1t8$FPPG, list(pl1t8$Team), mean)
aggFP1t8 <- aggFP1t8[order(aggFP1t8$Group.1),]



agg1t4 <- aggregate(pl1t4$FPPGpSal, list(pl1t4$Team), mean)
agg1t4 <- agg1t4[order(agg1t4$Group.1),]

pplist <-plistx[!(plistx$Probable.Pitcher != "Yes"),]
head(pplist)
pplist$FPPGpSal <- pplist$FPPG/(pplist$Salary/1000)

pplist <- pplist[order(pplist[,11]),]

plist$pitFPPG <- 0
#team col 10 plist, opp column 11
#pplist same

for(cc in 1:length(plist[,1])){
plist[cc,18] <- pplist[which(pplist[,10] == plist[cc,11]),6]
}


pplist <- dataframe(cbind(pplist$Id, agg1t8, pplist$Nickname, agg1t4[,2], pplist$FPPG, pplist$Played, pplist$Salary, pplist$FPPGpSal, pplist$Team, aggFP1t8[,2]), stringsAsFactors = F)
pplist <- pplist[order(pplist[,6]),]
pitcut <- pplist[round(.55*length(pplist[,1])),6]
pplist$Count <- 0

pplist <- pplist[order(-pplist[,9]),]
cut <- round(.55*length(pplist[,1]))

for (ee in cut:length(pplist[,1])){

pplist[ee,12] <- pplist[ee,12] + 1
}

pplist <- pplist[order(pplist[,11]),]
cut <- round(.55*length(pplist[,1]))

for (ee in cut:length(pplist[,1])){
pplist[ee,12] <- pplist[ee,12] + 1
}

pplist <-pplist[!(pplist$Count== 2),]


plist <- plist[order(-plist$FPPGpSal),]
plist$Count <- 0

cut <- round(.55*length(plist[,1]))

for (ff in cut:length(plist[,1])){

plist[ff,19] <- plist[ff,19] + 1
}

for (gg in 1:length(plist[,1])){
if(plist[gg,18] < pitcut){
plist[gg,19] <- plist[gg,19] + 1
}
}

plist <-plist[!(plist$Count== 2),]

c1b <- 0
sec <- 0
ss <- 0
th <- 0
of <- 0
ut <- 0
pp <- 0
lineups <- matrix(, nrow=0, ncol=9)
players <- matrix(, nrow=0, ncol=9)
botched <- matrix(, nrow=0, ncol=9)
system.time(for(bb in 1:9){ #a
salrange <- 0
tries <- 0
while(salrange < 1){ #b


c1bdf <- plist[which(plist$Position == "C" | plist$Position == "1B"),]
secdf <- plist[which(plist$Position == "2B"),]
ssdf <- plist[which(plist$Position == "SS"),]
thdf <- plist[which(plist$Position == "3B"),]
ofdf <- plist[which(plist$Position == "OF"),]

#while(pp < 1){ #a
row<- sample(1:length(pplist[,1]), 1)


stack <- .subset2(pplist, 1)[row]
teams <- .subset2(pplist, 11)[row]
sals <- .subset2(pplist, 8)[row]
guys <- .subset2(pplist, 4)[row]
#pplist<- ppdf[-row,]
pp <- pp +1


while(c1b < 1){ #a
row<- sample(1:length(c1bdf[,1]), 1)

stack <- cbind(stack, .subset2(c1bdf, 1)[row])
teams <- cbind(teams, .subset2(c1bdf, 10)[row])
sals <- cbind(sals, .subset2(c1bdf, 8)[row])
guys <- cbind(guys, .subset2(c1bdf, 4)[row])

c1bdf<- c1bdf[-row,]
c1b <- c1b +1

} #a

while(sec < 1){ #a
row<- sample(1:length(secdf[,1]), 1)


stack <- cbind(stack, .subset2(secdf, 1)[row])
teams <- cbind(teams, .subset2(secdf, 10)[row])
sals <- cbind(sals, .subset2(secdf, 8)[row])
guys <- cbind(guys, .subset2(secdf, 4)[row])
secdf<- secdf[-row,]
sec <- sec +1

} #a



while(th < 1){ #a
row<- sample(1:length(thdf[,1]), 1)


stack <- cbind(stack, .subset2(thdf, 1)[row])
teams <- cbind(teams, .subset2(thdf, 10)[row])
sals <- cbind(sals, .subset2(thdf, 8)[row])
guys <- cbind(guys, .subset2(thdf, 4)[row])
thdf<- thdf[-row,]
th <- th +1

} #a

while(ss < 1){ #a
row<- sample(1:length(ssdf[,1]), 1)


stack <- cbind(stack, .subset2(ssdf, 1)[row])
teams <- cbind(teams, .subset2(ssdf, 10)[row])
sals <- cbind(sals, .subset2(ssdf, 8)[row])
guys <- cbind(guys, .subset2(ssdf, 4)[row])
ssdf<- ssdf[-row,]
ss <- ss +1

} #a

while(of < 3){ #a
row<- sample(1:length(ofdf[,1]), 1)


stack <- cbind(stack, .subset2(ofdf, 1)[row])
teams <- cbind(teams, .subset2(ofdf, 10)[row])
sals <- cbind(sals, .subset2(ofdf, 8)[row])
guys <- cbind(guys, .subset2(ofdf, 4)[row])
ofdf<- ofdf[-row,]
of <- of +1

} #a

utdf <- rbind(c1bdf, secdf, ssdf, thdf, ofdf)
while(ut < 1){ #a
row<- sample(1:length(utdf[,1]), 1)


stack <- cbind(stack, .subset2(utdf, 1)[row])
teams <- cbind(teams, .subset2(utdf, 10)[row])
sals <- cbind(sals, .subset2(utdf, 8)[row])
guys <- cbind(guys, .subset2(utdf, 4)[row])
utdf<- utdf[-row,]
ut <- ut +1

} #a


#} #a
teem <- 0
for(tt in 2:9){
if(teams[tt] == teams[1]){
teem <- teem + 1
}
}

tmtbl <- data.frame(table(teams[1:9]))

#for(tt in 1:9){
#assign(paste("p",tt,sep=""), joined2FD[which(joined2FD[,1] == stack[1,tt]),7])
#}
#sp2 <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9)


if(teem < 2){ #c
if(max(tmtbl$Freq) < 5){ #d
if(any(is.na(stack)) == "FALSE"){  #e
totsal <- sum(sals)
print(totsal)
if(totsal > 32500 & totsal <= 35000){ #f
lineups <- rbind(lineups, stack)
players <- rbind(players, guys)
salrange <- 1
} else if(tries > 50){ #f #g
if(totsal > 31000 & totsal <= 35000){ #h
lineups <- rbind(lineups, stack)
players <- rbind(players, guys)
salrange <- 1
} #h
} else if(tries > 100){ #g #i
salrange <- 1
} #i
} else { #e #j
botched <- rbind(botched, guys)
} #j
tries <- tries + 1
} 
}
#reset these values for the next stack
c1b <- 0
sec <- 0
ss <- 0
th <- 0
of <- 0
ut <- 0
pp <- 0
}
write.csv(lineups, file='6_4_20_FDmlb_lineups_roughh2.csv', row.names=FALSE)

}) #a
write.csv(lineups, file='6_4_20_FDmlb_lineups_roughh2.csv', row.names=FALSE)

#for (i in 1:9){
#assign(paste("SM",i,sep=""), data.frame(players[,i]))
#}
#listFD <- do.call("rbind", list(SM1, SM2, SM3, SM4, SM5, SM6, SM7, SM8, SM9))
#listFD <- sort(listFD)
#listFD
#playertableFD <- table(listFD)
#playertableFD





#write.csv(plist, file='battersheet4_10.csv', row.names=FALSE)
#write.csv(pplist, file='pitchstix4_10.csv', row.names=FALSE)





