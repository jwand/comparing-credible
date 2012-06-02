library(anchors)
library(foreign)
#load("../data/nes88.Rdata")
#anes <- nes88
#rm(nes88)

anes <- as.data.frame(read.csv("../data/nes04dat.txt",quote=""))
dim(anes)

# V043203     Q1a. Who does R think will/would vote for President
# 1. John Kerry
# 2. George W. Bush
## Recode:
# dem == 1
# rep ==0
# all else: NA
anes$vote2  <- ifelse( anes$V043203 == 1, 1,
                       ifelse(anes$V043203 == 2, 0, NA))
xtabs( ~ V043203 + vote2 , data=anes)

# V043136     N1a. Spending and Services - 7-point scale self-placement
# =============================================================================
# PRE-ELECTION SURVEY:
# QUESTION:
# Where would you place YOURSELF on this scale, or haven't
# you thought much about this?
# 01. Govt should provide many fewer services
# 02.
# 03.
# 04.
# 05.
# 06.
# 07. Govt should provide many more services

## Recode: 1=more services, 7=fewer
ff <- function(x) { y <- ifelse(x >=1 & x<= 7, 8-x, NA); print(table(x,y)); y }
anes$posslf <- ff(as.numeric(anes$V043136))
anes$posdem <- ff(as.numeric(anes$V043139))
anes$posrep <- ff(as.numeric(anes$V043138))
anes$pospdem<- ff(as.numeric(anes$V043140))
anes$posprep<- ff(as.numeric(anes$V043141))

xtabs( ~ posslf+vote2, data=anes)

## missing...
anes$miss.any <- is.na(anes$posdem) | is.na(anes$posslf) | is.na(anes$posrep)
anes$miss.pty <- is.na(anes$posdem) | is.na(anes$posrep)
anes$miss.slf <- is.na(anes$posslf)


## subset
dim(anes)
anes4 <- subset(anes,subset=!miss.slf)# , select = c()
dim(anes4)


anes4$x <- ""
anes4$x[!is.na(anes4$posdem) & anes4$posslf <  anes4$posdem   &  (!is.na(anes4$posrep) & anes4$posdem <   anes4$posrep) ] <-"a.A,1-2"
anes4$x[!is.na(anes4$posdem) & anes4$posslf == anes4$posdem   &  (!is.na(anes4$posrep) & anes4$posslf <   anes4$posrep) ] <-"b.{A,1},2"
anes4$x[!is.na(anes4$posdem) & !is.na(anes4$posrep) & anes4$posdem <   anes4$posslf  &  anes4$posslf <   anes4$posrep  ] <-"c.1,A,2"
anes4$x[!is.na(anes4$posrep) & anes4$posrep ==  anes4$posslf  &  (!is.na(anes4$posdem) & anes4$posdem <   anes4$posslf) ] <-"d.1,{2,A}"
anes4$x[!is.na(anes4$posrep) & anes4$posrep <   anes4$posslf  &  (!is.na(anes4$posdem) & anes4$posdem <   anes4$posrep) ] <-"e.1-2,A"
#anes4$x[!is.na(anes4$posdem) & anes4$posdem == anes4$posslf   &  !is.na(anes4$posrep) &  anes4$posslf == anes4$posrep  ] <-"f.{1,A,2}"

##
anes4$y <- ""
anes4$y[anes4$posdem >   anes4$posslf  &  anes4$posdem  <  anes4$posrep ] <-1
anes4$y[anes4$posdem ==  anes4$posslf  &  anes4$posdem  <  anes4$posrep ] <-2
anes4$y[anes4$posdem <   anes4$posslf  &  anes4$posslf  <  anes4$posrep ] <-3
anes4$y[anes4$posrep ==  anes4$posslf  &  anes4$posdem  <  anes4$posrep ] <-4
anes4$y[anes4$posrep <   anes4$posslf  &  anes4$posdem  <  anes4$posrep ] <-5
#anes4$y[anes4$posrep ==   anes4$posslf  &  anes4$posdem  ==  anes4$posrep ] <-0

anes5<-subset(anes4,
              subset= y!="" & x!="",
              select=c(vote2,x,y,posslf,posdem,posrep,pospdem,posprep,miss.any,miss.pty,miss.slf))

write.table(anes5,"anes.csv",sep=",",na="",row.names=FALSE)
