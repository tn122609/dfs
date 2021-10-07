cd /host-rootfs/sdcard/Download
sudo R

library(dplyr)
library(xml2)
library(stringi)
library(stringr)
library(rvest)
library('lpSolve')

#belmontodds.csv
num <- 50000
top3 <- matrix(, nrow=0, ncol=1)
for(h in 1:num){
pgdf <- read.csv('belmontodds.csv', stringsAsFactors=FALSE)
pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 2)[ww]/sum(pgdf$Odds)
pgdf[ww,3] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 3)[row]){ #d
aa <- 1

stack <- .subset2(pgdf, 1)[row]
#teams <- .subset2(pgdf, 5)[row]

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 2)[ww]/sum(pgdf$Odds)
pgdf[ww,3] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 3)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
#teams <- .subset2(pgdf, 5)[row]

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 2)[ww]/sum(pgdf$Odds)
pgdf[ww,3] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 3)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
#teams <- .subset2(pgdf, 5)[row]

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 2)[ww]/sum(pgdf$Odds)
pgdf[ww,3] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 3)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
#teams <- .subset2(pgdf, 5)[row]

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

pgdf$cm <- 0
yyz <- 0
for(ww in 1:length(pgdf[,1])){ #b
yyz <- yyz + .subset2(pgdf, 2)[ww]/sum(pgdf$Odds)
pgdf[ww,3] <- yyz
} #b
crn <- runif(1)
aa <- 0
row <- 1
while(aa < 1){ #c
if(crn < .subset2(pgdf, 3)[row]){ #d
aa <- 1

stack <- cbind(stack, .subset2(pgdf, 1)[row])
#teams <- .subset2(pgdf, 5)[row]

pgdf<- pgdf[-row,]

}else{ #d #g 
row <- row + 1
} #g
} #c
pgdf

comb <- paste(stack[1], stack[2], stack[3], stack[4], stack[5])
comb
top3 <- rbind(top3, comb)
}

playertableFD <- table(top3)
playertableFD <- playertableFD/num
playertableFD


write.csv(playertableFD, file='belmonttable2.csv', row.names=FALSE)
