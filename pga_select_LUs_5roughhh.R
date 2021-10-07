cd /host-rootfs/sdcard/Download
sudo R

library(dplyr)
library(xml2)
library(stringi)
library(stringr)
library(rvest)
library('lpSolve')

pgdf2 <- read.csv('pga_tier_data_8_12.csv', stringsAsFactors=FALSE)
num <- 1000
lineups <- matrix(, nrow=0, ncol=6)
top3 <- matrix(, nrow=0, ncol=6)
for(h in 1:num){
pgdf <- pgdf2

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 4)[ww]/sum(pgdf$Tier)
pgdf[ww,5] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 5)[row]){ #d
aa <- 1

stack <- .subset2(pgdf, 1)[row]
teams <- .subset2(pgdf, 2)[row]

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 4)[ww]/sum(pgdf$Tier)
pgdf[ww,5] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 5)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
teams <- cbind(teams, .subset2(pgdf, 2)[row])

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 4)[ww]/sum(pgdf$Tier)
pgdf[ww,5] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 5)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
teams <- cbind(teams, .subset2(pgdf, 2)[row])

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 4)[ww]/sum(pgdf$Tier)
pgdf[ww,5] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 5)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
teams <- cbind(teams, .subset2(pgdf, 2)[row])


pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 4)[ww]/sum(pgdf$Tier)
pgdf[ww,5] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 5)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
teams <- cbind(teams, .subset2(pgdf, 2)[row])


pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 4)[ww]/sum(pgdf$Tier)
pgdf[ww,5] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 5)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
teams <- cbind(teams, .subset2(pgdf, 2)[row])


pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

totsal <- pgdf2[which(pgdf2[,1] == stack[1]),3] + pgdf2[which(pgdf2[,1] == stack[2]),3] + pgdf2[which(pgdf2[,1] == stack[3]),3] + pgdf2[which(pgdf2[,1] == stack[4]),3] + pgdf2[which(pgdf2[,1] == stack[5]),3] + pgdf2[which(pgdf2[,1] == stack[6]),3] 


if(totsal > 51400 & totsal <= 60000){ 
#comb <- c(stack[1], stack[2], stack[3], stack[4], stack[5], stack[6])
#comb
#comb2 <- c(teams[1], teams[2], teams[3], teams[4], teams[5], teams[6])
#comb2
top3 <- rbind(top3, stack)
lineups <- rbind(lineups, teams)
}
print(h)
write.csv(lineups, file='2golflineups_8_12.csv', row.names=FALSE)
}

playertableFD <- table(top3)
playertableFD <- playertableFD/length(lineups[,1])
playertableFD


write.csv(playertableFD, file='golfertable.csv', row.names=FALSE)
write.csv(lineups, file='2golflineups_8_12.csv', row.names=FALSE)











