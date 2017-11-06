## SOURCE: data/surveys/anes/nes04/R/services4.R
##
## replication code for:
## 
##  Credible Comparisons Using Interpersonally Incomparable Data: Nonparametric Scales with Anchoring Vignettes
##  Jonathan Wand
##  American Journal of Political Science, 57(1), 249--262 

library(anchors)
library(foreign)

anes4 <- read.csv("anes.csv")

xtabs( ~ x + y , data=anes4)

## GOOD:
## note: return to original order for posslf (7=more service)
## counts by xtab:
(tb <- xtabs( ~ posslf + x, data=anes4, subset=!is.na(vote2) & x != "",drop=TRUE))
# N=776
sum(tb)

## reject no association
chisq.test(tb,sim=TRUE,B=10000)

## look at missing: missordering
## N=98 missording
sum(ix <- anes4$y!=1 & anes4$x=="" & !is.na(anes4$posrep) & !is.na(anes4$posdem))
as.matrix(table(paste( anes4$posslf, anes4$posdem, anes4$posrep)[ix]))
sum(ix <- is.na(anes4$posrep) & is.na(anes4$posdem))

## vote percents by xtabs
tb
(tc <-  xtabs(vote2 ~ I(posslf) + x, data=anes4, subset=!is.na(vote2) & x != ""))
round(tc/tb,2)



####################################################################
## test row v column homog
####################################################################
## column 1:
(a <- cbind(tb[1:5,1,1], tb[1:5,1,2] ))
chisq.test(a,sim=TRUE,B=10000)
## col 2:
(a <- cbind(tb[1:4,2,1], tb[1:4,2,2] ))
chisq.test(a,sim=TRUE,B=10000)
## col 3:
(a <- cbind(tb[2:6,3,1], tb[2:6,3,2] ))
chisq.test(a,sim=TRUE,B=10000)
## col 4: 
(a <- cbind(tb[4:7,4,1], tb[4:7,4,2] ))
chisq.test(a,sim=TRUE,B=10000)
## col 5: 
(a <- cbind(tb[4:7,5,1], tb[4:7,5,2] ))
chisq.test(a,sim=TRUE,B=10000)

## row 1
(a <- cbind(tb[1,c(1:2),1], tb[1,c(1:2),2] ))
chisq.test(a,sim=TRUE, B=10000)

## row 2:
(a <- cbind(tb[2,c(1,3),1], tb[2,c(1,3),2] ))
chisq.test(a,sim=TRUE, B=10000)
(a <- cbind(tb[2,c(1:3),1], tb[2,c(1:3),2] ))
chisq.test(a,sim=TRUE, B=10000)

## row 3:
(a <- cbind(tb[3,c(1,3),1], tb[3,c(1,3),2] ))
chisq.test(a,sim=TRUE, B=10000)
(a <- cbind(tb[3,c(1:3),1], tb[3,c(1:3),2] ))
chisq.test(a,sim=TRUE)

## row 4:
(a <- cbind(tb[4,c(1,3,5),1], tb[4,c(1,3,5),2] ))
chisq.test(a,sim=TRUE,B=10000)
(a <- cbind(tb[4,c(1:5),1], tb[4,c(1:5),2] ))
chisq.test(a,sim=TRUE,B=10000)

## row 5:
(a <- cbind(tb[5,c(3,5),1], tb[5,c(3,5),2] ))
chisq.test(a,sim=TRUE,B=10000)
(a <- cbind(tb[5,c(3:5),1], tb[5,c(3:5),2] ))
chisq.test(a,sim=TRUE,B=10000)

## row 6:
(a <- cbind(tb[6,c(3,5),1], tb[6,c(3,5),2] ))
chisq.test(a,sim=TRUE,B=10000)
(a <- cbind(tb[6,c(3:5),1], tb[6,c(3:5),2] ))
chisq.test(a,sim=TRUE,B=10000)

## row 7:
(a <- cbind(tb[7,c(4:5),1], tb[7,c(4:5),2] ))
chisq.test(a,sim=TRUE,B=10000)



## good:
(a <- matrix( c(tb[4,5,1], tb[4,5,2],
               tb[5,3,1], tb[5,3,2]), byrow=TRUE,ncol=2))
chisq.test(a,sim=TRUE,B=10000)

### other
#a <- matrix( c(tb[4,5,1], tb[4,5,2],
#               tb[6,3,1], tb[6,3,2]), byrow=TRUE,ncol=2)
#chisq.test(a,sim=TRUE)
#
#a <- matrix( c(tb[5,5,1], tb[5,5,2],
#               tb[6,3,1], tb[6,3,2]), byrow=TRUE,ncol=2)
#chisq.test(a,sim=TRUE)
#
#a <- matrix( c(tb[5,5,1], tb[5,5,2],
#               tb[5,3,1], tb[5,3,2]), byrow=TRUE,ncol=2)
#chisq.test(a,sim=TRUE)
#
### anchors
#
#z <- anchors( posslf ~ posdem + posrep, data=anes)
#summary(z)
#anesz <- anchors.subset(anes,z)
#ftable(xtabs( ~ Bs + Be + posslf, anesz))
#anesz$B <- paste(anesz$Bs,anesz$Be,sep=",")
#(tb <- xtabs( ~ posslf + B, data=anesz))
#round(tb / apply(tb,1,sum),2)*100

######################

## test fit: B
## corrolorary 1
idx <- anes$posdem > anes$posslf & anes$posrep < anes$posslf | anes$miss.any
(a <- sum(idx, na.rm=TRUE))
1- a /nrow(anes)

idx <- anes4$posdem > anes4$posslf & anes4$posrep < anes4$posslf | anes4$miss.any
(a <- sum(idx, na.rm=TRUE))
1- a /nrow(anes4)

idx <- anes4$posdem > anes4$posslf & anes4$posrep < anes4$posslf
(a <- sum(idx, na.rm=TRUE))
1- a /length(idx)

# test fit: C
(tb <- table(anes$posrep - anes$posdem  ))
(a <- sum(tb))
round(tb /sum(tb),2)
round(tb /sum(tb),2)[which.max(tb /sum(tb))]

(tb <- table(anes4$posrep - anes4$posdem  ))
(a <- sum(tb))
round(tb /sum(tb),3)
round(tb /sum(tb),3)[which.max(tb /sum(tb))]

(tb <- xtabs(~ I(posrep - posdem) , data=anes, subset=!posrep %in% 2:6 | !posdem %in% 2:6 ))
(b <- sum(tb))
b/a
(tb <- xtabs(~ I(posrep - posdem) , data=anes, subset=posrep %in% 2:6 & posdem %in% 2:6 ))
sum(tb)
round(tb /sum(tb),2)
round(tb /sum(tb),2)[which.max(tb /sum(tb))]
## modal category is: 
.21 * .69
