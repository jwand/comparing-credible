## replication code for:
## 
##  Credible Comparisons Using Interpersonally Incomparable Data: Nonparametric Scales with Anchoring Vignettes
##  Jonathan Wand
##  American Journal of Political Science, 57(1), 249--262 

library(anchors)
data(mexchn)

## embedFont only needed for figs for article
embedFont <- FALSE


tmp <- subset(mexchn, select=c(xsay1,xsay2,xsay3,xsay4,xsay5))
idx <- apply(tmp,1, function(x) all(x==0))
sum(idx)
mexchn.del <- subset(mexchn, subset=xsayself>0 & !idx)
dim(mexchn.del)


### figures 1(a) and 1(B)
if (embedFont) source("z_embed_fonts.R")
fname <- "mexchn_xsayself.pdf"
pdf(fname,height=3,width=3.5)
par(cex.lab=1,cex=.75,mar=c(2,4,1,.5)) # par(cex.lab=1,cex=1.2,mar=c(4,4,1,4))
par(family="CM")
## basic histograms
(tb <- xtabs( ~ china + xsayself, data=mexchn.del))
tp <- tb / apply(tb,1,sum)
b <- barplot(tp, beside=TRUE, col=c("black","red"),ylim=c(0,.7),
        ylab="Proportion",
        names.arg=c("No say","Little","Some","Lot","Unlimited"),cex.names=1.1,cex.axis=1.1)

zm <- tp[1,]
zc <- tp[2,]
x <-b[1,1]
y <- zm[1]
#arrows( x, y+.05, x, y+.01,length=.1)
text( x-.05, y+.07, "Mexico", srt=90)
x <-b[2,1] 
y <- zc[1]
#arrows( x,  y+.05 , x, y+.01,length=.1)
text( x-.05, y+.07, "China", srt=90)
dev.off();
if (embedFont) embedCM(fname)

fname <- "mexchn_xsay5.pdf"
pdf(fname,height=3,width=3.5)
par(cex.lab=1,cex=.75,mar=c(2,4,1,.5)) # par(cex.lab=1,cex=1.2,mar=c(4,4,1,4))
par(family="CM")
(tb <- xtabs( ~ china + xsay5, data=mexchn.del, subset=xsay5>0))
tp <- tb / apply(tb,1,sum)
b <- barplot(tp, beside=TRUE, col=c("black","red"),ylim=c(0,.7),
        ylab="Proportion",
        names.arg=c("No say","Little","Some","Lot","Unlimited"),cex.names=1.1,cex.axis=1.1)

zm <- tp[1,]
zc <- tp[2,]
x <-b[1,1]
y <- zm[1]
#arrows( x, y+.05, x, y+.01,length=.1)
text( x-.05, y+.07, "Mexico", srt=90)
x <-b[2,1] 
y <- zc[1]
#arrows( x,  y+.05 , x, y+.01,length=.1)
text( x-.05, y+.07, "China", srt=90)

dev.off();
if (embedFont) embedCM(fname)


## Figure 6:

fname <- "mexchn_C5orig.pdf"

#pdf(fname,height=4,width=8)
pdf(fname,height=3,width=6.5)
par(cex.lab=1,cex=.75,mar=c(2,4,.5,.5)) # par(cex.lab=1,cex=1.2,mar=c(4,4,1,4))
par(family="CM")
                                        #par(cex.lab=1,cex=1.2,mar=c(4,4,0,0))

a1c <- anchors(xsayself ~ xsay5 + xsay4 + xsay3 + xsay2 + xsay1,method="C",
               data = subset(mexchn,subset=china==1))
a1m <- anchors(xsayself ~ xsay5 + xsay4 + xsay3 + xsay2 + xsay1,method="C",
               data = subset(mexchn,subset=china==0))
zm <- summary(a1m)
zc <- summary(a1c)
b <- barplot(rbind(zm$uniform,
              zc$uniform),
        beside=TRUE, ylim=c(0,.55), col=c("black","red"),
        ylab="Proportion",xlab="C",
        names.arg=c(1:11))
x <-b[1,1] -.05
y <- zm$uniform[1]
#arrows( x, y+.05, x, y+.01,length=.1)
text( x-.1, y+.06, "Mexico", srt=90)
x <-b[2,1] -.05
y <- zc$uniform[1]
#arrows( x,  y+.05 , x, y+.01,length=.1)
text( x-.1, y+.05, "China", srt=90)

dev.off()
if (embedFont) embedCM(fname)

## TABLE 2:
a1c <- anchors(xsayself ~ xsay5 ,method="B",
               data = subset(mexchn,subset=china==1))
summary(a1c)
a1m <- anchors(xsayself ~ xsay5 ,method="B",
               data = subset(mexchn,subset=china==0))
summary(a1m)
