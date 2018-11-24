cd /sdcard/Download
sudo R
#########rules of thumb
#########if a wr or te as captain, need a qb from same team 
#####if qb as cpt neef at least 1 wr/te from same team
##### if d/st, then only 2 players from opposing teams
####no kicker as cpt or 2 kicker lineups

library(dplyr)
library(xml2)
library(stringi)
library(stringr)
library(rvest)
library('lpSolve')


#setwd("C:/Users/Ty/Desktop/dfs")
Salaries <- read.csv('DKSalaries (45).csv')
Salaries
options(digits=9) ######this doesn't seem to work

Salaries$Name <- gsub("Patriots", "New England D/ST", Salaries$Name)
Salaries$Name <- gsub("Panthers", "Carolina D/ST", Salaries$Name)
Salaries$Name <- gsub("Titans", "Tennessee D/ST", Salaries$Name)
Salaries$Name <- gsub("49ers", "San Francisco D/ST", Salaries$Name)
Salaries$Name <- gsub("Bengals", "Cincinnati D/ST", Salaries$Name)
Salaries$Name <- gsub("Rams", "Los Angeles Rams D/ST", Salaries$Name)
Salaries$Name <- gsub("Eagles", "Philadelphia D/ST", Salaries$Name)
Salaries$Name <- gsub("Ravens", "Baltimore D/ST", Salaries$Name)
Salaries$Name <- gsub("Texans", "Houston D/ST", Salaries$Name)
Salaries$Name <- gsub("Saints", "New Orleans D/ST", Salaries$Name)
Salaries$Name <- gsub("Jets", "New York Jets D/ST", Salaries$Name)
Salaries$Name <- gsub("Chargers", "Los Angeles Chargers D/ST", Salaries$Name)
Salaries$Name <- gsub("Chiefs", "Kansas City D/ST", Salaries$Name)
Salaries$Name <- gsub("Broncos", "Denver D/ST", Salaries$Name)
Salaries$Name <- gsub("Cardinals", "Arizona D/ST", Salaries$Name)
Salaries$Name <- gsub("Bills", "Buffalo D/ST", Salaries$Name)
Salaries$Name <- gsub("Packers", "Green Bay D/ST", Salaries$Name)
Salaries$Name <- gsub("Steelers", "Pittsburgh D/ST", Salaries$Name)
Salaries$Name <- gsub("Vikings", "Minnesota D/ST", Salaries$Name)
Salaries$Name <- gsub("Lions", "Detroit D/ST", Salaries$Name)
Salaries$Name <- gsub("Browns", "Cleveland D/ST", Salaries$Name)
Salaries$Name <- gsub("Cowboys", "Dallas D/ST", Salaries$Name)
Salaries$Name <- gsub("Jaguars", "Jacksonville D/ST", Salaries$Name)
Salaries$Name <- gsub("Giants", "New York Giants D/ST", Salaries$Name)
Salaries$Name <- gsub("Dolphins", "Miami D/ST", Salaries$Name)
Salaries$Name <- gsub("Raiders", "Oakland D/ST", Salaries$Name)
Salaries$Name <- gsub("Falcons", "Atlanta D/ST", Salaries$Name)
Salaries$Name <- gsub("Redskins", "Washington D/ST", Salaries$Name)
Salaries$Name <- gsub("Seahawks", "Seattle D/ST", Salaries$Name)
Salaries$Name <- gsub("Colts", "Indianapolis D/ST", Salaries$Name)
Salaries$Name <- gsub("Buccaneers", "Tampa Bay D/ST", Salaries$Name)
Salaries$Name <- gsub("Bears", "Chicago D/ST", Salaries$Name)
Salaries$Name

numf<- read_html("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections")
numf2 <- numf %>%
html_table(fill = TRUE)
numf2[[2]]

numf3x <- data.frame(numf2[[2]])
numf4x <- data.frame(numf2[[1]])
numf3 <- numf3x[-1,]
numf4 <- data.frame(numf4x[-1,])
head(numf3, n=6)
head(numf4, n=6)

dnumf<- read_html("http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/d")
dnumf2 <- dnumf %>%
html_table(fill = TRUE)
dnumf2[[1]]
dnumf2[[2]]

dnumf3x <- data.frame(dnumf2[[2]])
dnumf4x <- data.frame(dnumf2[[1]])
dnumf3 <- dnumf3x[-1,]
dnumf4 <- data.frame(dnumf4x[-1,])
names(dnumf4) <- "numf4x..1..."
head(dnumf3, n=6)
head(dnumf4, n=6)

numf4 <- rbind(numf4, dnumf4)
head(numf4, n=6)

numfName <- str_split_fixed(numf4[,1], "\n ", 3)
numfName2 <- str_split_fixed(numfName[,3], ",", 2)
head(numfName, n=6)
head(numfName2, n=6)

numfp1 <- data.frame(cbind(numfName[,1], numfName2))
head(numfp1, n=6)

names(numfp1) <- c("Name", "Pos", "Team")
numfp1$Pos <- gsub("^\\s+|[(]", "", numfp1$Pos)
numfp1$Team <- gsub("^\\s+|[)]", "", numfp1$Team)
#i believe 1st 3 columns are ready now
numfp2 <- subset(numf3, select = c(Opp, DraftKings, DraftKings.1))
names(numfp2) <- c("Opp", "Proj.Pts", "Salary")
dnumfp2 <- subset(dnumf3, select = c(Opp, DraftKings, DraftKings.1))
names(dnumfp2) <- c("Opp", "Proj.Pts", "Salary")
numfp2 <- rbind(numfp2, dnumfp2)
nfjoin <- cbind(numfp1, numfp2)
head(nfjoin, n=6)

cur_max <- 147.9976
solutionsMatrix2 <- matrix(, nrow=0, ncol=9)
dkuploadMatrix2 <- matrix(, nrow=0, ncol=15)
#colnames(dkuploadMatrix2) <- c("CPT", "FLEX", "FLEX", "FLEX", "FLEX", "FLEX")
for(p in 1:230){ #aa

s <- 1
while(s < 2){ #bb
nfjoin$Salary <- gsub("[$]", "", nfjoin$Salary)
head(nfjoin, n=6)

#Big Ben?, Le'Veon12/22, Eli Rogers?, Elijah Hood, T. Smith 43/38, Switzer?
Salaries[38,3] <- gsub("Smith", "", Salaries[38,3])
Salaries[43,3] <- gsub("Smith", "", Salaries[43,3])
Salaries[22,3] <- gsub("Bell", "", Salaries[22,3])
Salaries[12,3] <- gsub("Bell", "", Salaries[12,3])
#Salaries[1,3] <- gsub("Elliott", "", Salaries[1,3])
#Salaries[4,3] <- gsub("Elliott", "", Salaries[4,3])
#Salaries[79,3] <- gsub("Rogers", "", Salaries[79,3])
#Salaries[67,3] <- gsub("Rogers", "", Salaries[67,3])

nfjoin$Name
Salaries$Name
nfjoin$Name <- gsub("[\n]", "", nfjoin$Name)
nfjoin$Name <- gsub("[',]", "", nfjoin$Name)
nfjoin$Name <- gsub("[.-]", "", nfjoin$Name)
nfjoin$Name <- gsub("^\\s+|\\s+$", "", nfjoin$Name)
nfjoin$Name <- gsub(" Jr| Sr| II| III| IV", "", nfjoin$Name)

Salaries$Name <- gsub("[\n]", "", Salaries$Name)
Salaries$Name <- gsub("[',]", "", Salaries$Name)
Salaries$Name <- gsub("[.-]", "", Salaries$Name)
Salaries$Name <- gsub("^\\s+|\\s+$", "", Salaries$Name)
Salaries$Name <- gsub(" Jr| Sr| II| III| IV", "", Salaries$Name)

nfjoin2 <- inner_join(nfjoin, Salaries, by = "Name")
nfjoin2$Salary <- nfjoin2$Salary.y
nfjoin3 <- subset(nfjoin2, select = c("Name", "Pos", "Salary", "Team", "Opp", "Proj.Pts", "Roster.Position", "ID"))

datax <- nfjoin3

k1 <- data.frame("Boswell", "K", 3800, "PIT", "CAR", 8.37, "FLEX", 11574542)
names(k1)<-c("Name", "Pos", "Salary", "Team", "Opp", "Proj.Pts", "Roster.Position", "ID")

k2 <- data.frame("Gano", "K", 3400, "CAR", "PIT", 8.09, "FLEX", 11574543)
names(k2)<-c("Name", "Pos", "Salary", "Team", "Opp", "Proj.Pts", "Roster.Position", "ID")

k3 <- data.frame("Boswell", "K", 5700, "PIT", "CAR", 8.37, "CPT", 11574580)
names(k3)<-c("Name", "Pos", "Salary", "Team", "Opp", "Proj.Pts", "Roster.Position", "ID")

k4 <- data.frame("Gano", "K", 5100, "CAR", "PIT", 8.09, "CPT", 11574581)
names(k4)<-c("Name", "Pos", "Salary", "Team", "Opp", "Proj.Pts", "Roster.Position", "ID")
data <- rbind(datax, k1, k2, k3, k4)


data$Proj.Pts <- as.numeric(data$Proj.Pts)
for(b in 1:length(data[,1])){
if(data[b,2] == "QB"){
data[b,6] <- data[b,6]*1.348
}
if(data[b,2] == "RB"){
data[b,6] <- data[b,6]*1.465
}
if(data[b,2] == "WR"){
data[b,6] <- data[b,6]*1.53
}
if(data[b,2] == "TE"){
data[b,6] <- data[b,6]*1.554
}
if(data[b,2] == "D"){
data[b,6] <- data[b,6]*1.665
}
if(data[b,2] == "K"){
data[b,6] <- data[b,6]*1.521
}
}

data$Proj.Pts <- as.numeric(data$Proj.Pts)
for(h in 1:length(data$Proj.Pts)){
if(data$Roster.Position[h]=="CPT"){
data$Proj.Pts[h] <- 1.5*data$Proj.Pts[h]
}
}
#data <- nfjoin3

#this should already be handled in inner join
#target <- c("CLE", "NYJ")
#data <- filter(data, Team %in% target)  

#Convert salary to numeric
data$Salary <- as.numeric(data$Salary)
data$Proj.Pts <- as.numeric(data$Proj.Pts)
data <- data.frame(na.omit(data))

head(data, n=12)
data
data$Name
#write.csv(data, file='2projections.csv', row.names=FALSE) 

#Add binary valeus for positions  'Constraint Vectors'
data <- cbind(data, X=ifelse(data$Roster.Position=="CPT",1,0))
data <- cbind(data, Y=ifelse(data$Roster.Position=="FLEX", 1,0))

#Objective Function. sum of proj pts
f.obj <- data$Proj.Pts

#Constraints
num_X <- 1
num_Y <- 5
max_from_a_name <- 1
max_team_cost <- 50000
max_player_from_a_team <- 5

guys <- sort(unique(data$Name))

name_constraint_vector <- c()
name_constraint_dir <- c()
name_constraint_rhs <- c()

for(i in 1:length(guys)){
  tempguys <- data$Name==as.character(guys[i])
  tempguys[tempguys==T] <- 1
  tempguys[tempguys==F] <- 0
  
  name_constraint_vector <- c(name_constraint_vector, tempguys)
  name_constraint_dir <- c(name_constraint_dir, "<=")
  name_constraint_rhs <- c(name_constraint_rhs, max_from_a_name)
}

#Constraints for max players from team
clubs <- sort(unique(data$Team))

team_constraint_vector <- c()
team_constraint_dir <- c()
team_constraint_rhs <- c()

for(i in 1:length(clubs)){
  temp <- data$Team==as.character(clubs[i])
  temp[temp==T] <- 1
  temp[temp==F] <- 0
  
  team_constraint_vector <- c(team_constraint_vector, temp)
  team_constraint_dir <- c(team_constraint_dir, "<=")
  team_constraint_rhs <- c(team_constraint_rhs, max_player_from_a_team)
}

  f.con <- matrix(c(data$X, data$Y, data$Salary, data$Proj.Pts, team_constraint_vector, name_constraint_vector), nrow=(4+length(clubs)+ length(guys)), byrow=TRUE)
  f.dir <- c("=", "=", "<=", "<=", team_constraint_dir, name_constraint_dir)
  f.rhs <- c(num_X, num_Y, max_team_cost, cur_max, team_constraint_rhs, name_constraint_rhs)
  
  x <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE, num.bin.soln=90)
 
print(x$num.bin.solns)
numcols <- length(data$Pos)
numsols <- 90

dkuploadMatrix <- matrix(, nrow=0, ncol=15)
solutionsMatrix <- matrix(, nrow=0, ncol=9)
solutions <- list()
solutionsz <- matrix(head(x$solution, numcols*numsols), nrow=numsols, byrow=TRUE)
for(g in 1:x$num.bin.solns){ #ee
solutions<- data[which(solutionsz[g,]==1),]
solutions<- solutions[order(solutions$Y, solutions$X),]
solutions2 <-data.frame(solutions)
print(sum(solutions$Proj.Pts))
#given 3+ wr/te from same team, need to have QB from same team
zz <- 0
  t <- 0
  d <- 0
  q <- 0
  rb <- 0
  wrt <- 0
 for(z in 1:6){ #a
 
 if(solutions2[z,2] == "QB"){
 for(o in 1:6){
  if(solutions2[o,2] == "QB"){
   if(solutions2[o,4] == solutions2[z,4] & solutions2[o,6] != solutions2[z,6]){
   q <- q + 1
   }
   }
   }
   }
   
    if(solutions2[z,2] == "RB"){
 for(o in 1:6){
  if(solutions2[o,2] == "RB"){
   if(solutions2[o,4] == solutions2[z,4] & solutions2[o,6] != solutions2[z,6]){
   rb <- rb + 1
   }
   }
   }
   }
   
    if(solutions2[z,2] == "WR" | solutions2[z,2] == "TE"){
 for(o in 1:6){
  if(solutions2[o,2] == "WR" | solutions2[o,2] == "TE"){
   if(solutions2[o,4] == solutions2[z,4] & solutions2[o,6] != solutions2[z,6]){
   wrt <- wrt + 1
   }
   }
   }
   }
   #   if(solutions2[z,2] == "K"){#b
 #  zz <- zz + 1
 #  }#b
   if(solutions2[z,2] == "D"){#c
   d <- d + 1
   ds <- 0
    for(l in 1:6){#d
    
    if(solutions2[z,4]  == solutions2[l,5]){#e
    if(solutions2[l,2] == "QB"){#f
    ds <- ds + 2
    }#f
      if(solutions2[l,7] == "CPT"){#g
    ds <- ds + 2
    } else { #h (& closed g)
    ds <- ds + 1
    } #h
    } #e
    } #d
    if(ds < 4){ #i
    d <- d - 1
    } #i
    } #c
   if(solutions2[z,2] == "QB"){ #j
  t <- t + 1
  ts <- 0
  for(j in 1:6){ #k
  if(solutions2[j,2] == "WR" | solutions2[j,2] == "TE"){ #l
  if(solutions2[z,4] == solutions2[j,4] & solutions2[j,6] > 2.85){ #m
    ts <- ts +1
  } #m
  } #l
} #k
if(ts > 0){
t <- t - 1
}
} #j
  } #a
  tw <- 0
    if(solutions2[1,2] == "WR" | solutions2[1,2] == "TE"){ #n
    tw <- 1
    for(f in 1:5){ #o
    if(solutions2[1+f,2] == "QB"){ #p
    if(solutions2[1,4] == solutions2[1+f,4]){
    tw <- 0
    }
    }  #p
    } #o
    } #n
     cnt <- matrix(c(t, d, tw, q, rb, wrt))
print(cnt)
###t must = 0; zz must be < 2; d must = 0; tw must = 0, ki must = 0
if(t == 0){ #r
#if(zz < 2){ #s allowing for 2 kickers for now
if( d== 0){ #t
if(tw == 0){ #u
if(q == 0){
if(rb < 5){
if(wrt < 9){
s <- s + 1
solutionsMatrix <- rbind(solutionsMatrix, c(x$num.bin.solns, sum(solutions$Proj.Pts), sum(solutions$Salary), toString(solutions$Name[1]), toString(solutions$Name[2]), toString(solutions$Name[3]), toString(solutions$Name[4]), toString(solutions$Name[5]), toString(solutions$Name[6])))
dkuploadMatrix <- rbind(dkuploadMatrix, c(solutions$ID[1], solutions$ID[2], solutions$ID[3], solutions$ID[4], solutions$ID[5], solutions$ID[6], x$num.bin.solns, sum(solutions$Proj.Pts), sum(solutions$Salary), toString(solutions$Name[1]), toString(solutions$Name[2]), toString(solutions$Name[3]), toString(solutions$Name[4]), toString(solutions$Name[5]), toString(solutions$Name[6])))

}
}
}
} #u
} #t
#} #s
} #r

}#ee
rni <- sample(1:length(solutionsMatrix[,1]), 1, replace=TRUE)
if(length(solutionsMatrix[,1]) > 0){
if(length(solutionsMatrix[,1]) > 1){
solutionsMatrix2 <- rbind(solutionsMatrix2, solutionsMatrix[rni,])
dkuploadMatrix2 <- rbind(dkuploadMatrix2, dkuploadMatrix[rni,])
} else {
solutionsMatrix2 <- rbind(solutionsMatrix2, solutionsMatrix)
dkuploadMatrix2 <- rbind(dkuploadMatrix2, dkuploadMatrix)
}
}
print(sum(solutions[1:6,6]))
print(sum(solutions$Proj.Pts))
cur_max <-   x$objval - .000005
} #bb
print(solutionsMatrix2)
write.csv(dkuploadMatrix2, file='2topprojlines_pit_car.csv', row.names=FALSE) 
} #aa
solutionsMatrix2
dkuploadMatrix2
write.csv(dkuploadMatrix2, file='2topprojlines.csv', row.names=FALSE) 

#could probably revise to and generate #'s outside of for loop if im bored...
dkuploadMatrix3 <- matrix(, nrow=0, ncol=15)
for(y in 1:20){
rn <- rnorm(1, 0, 322.6)
print(rn)
abrd <- abs(round(rn, digits =0))
dkuploadMatrix3 <- rbind(dkuploadMatrix3, dkuploadMatrix2[abrd,])
}
dkuploadMatrix3
write.csv(dkuploadMatrix3, file='dkshowdown11_8_car_pit.csv', row.names=FALSE) 
l1 <- read.csv('topprojlines_pit_car.csv')
l2 <- read.csv('2topprojlines_pit_car.csv')
dkuploadMatrix2 <- rbind(l1, l2)

dkuploadMatrix4 <- dkuploadMatrix3
table()
for (i in 1:6){
assign(paste("SM",i,sep=""), data.frame(dkuploadMatrix4[,i+9]))
}
listFD <- do.call("rbind", list(SM1, SM2, SM3, SM4, SM5, SM6))
playertableFD <- table(listFD)
playertableFD

table()
MVPtable <- table(dkuploadMatrix4[,10])
MVPtable
