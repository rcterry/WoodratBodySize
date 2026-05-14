#----------------------------------------
# Track Standard Deviation by Species by time period
# Resampling to account for unequal sample sizes per time bin
#-----------------------------------------

data <- read.csv(paste0(output_dir, "/4.0-PaisleyFossils-PredictedSize.csv"))
data <- subset(data,select=c("RCTID","Taxon","Crown.Height","Bchron.Age","Age.Bin","Weight","wt.nls.used","Length","l.nls.used"))

species <- c("Neotoma cinerea","Neotoma lepida")

for (s in 1:length(species)) {
df <- data[which(data$Taxon==species[s]),]
  
## Plot Mean and SD
mn.w <- aggregate(df$Weight,by=list(df$Age.Bin),mean)
colnames(mn.w) <- c("TimeBin","Mean.W")
sd.w <- aggregate(df$Weight,by=list(df$Age.Bin),sd)
colnames(sd.w) <- c("TimeBin","SD.W")

mn.l <- aggregate(df$Length,by=list(df$Age.Bin),mean)
colnames(mn.l) <- c("TimeBin","Mean.L")
sd.l <- aggregate(df$Length,by=list(df$Age.Bin),sd)
colnames(sd.l) <- c("TimeBin","SD.L")

out <- merge(mn.w,sd.w)
out <- merge(out,mn.l)
out <- merge(out,sd.l)

out$Species <- rep(species[s],nrow(out))

w.ylim.m=c(round((min(out$Mean.W)-min(out$Mean.W)*.02),0),
         round((max(out$Mean.W)+max(out$Mean.W)*.02),0))
w.ylim.sd=c(round((min(out$SD.W)-min(out$SD.W)*.02),0),
         round((max(out$SD.W)+max(out$SD.W)*.02),0))

l.ylim.m=c(round((min(out$Mean.L)-min(out$Mean.L)*.02),0),
           round((max(out$Mean.L)+max(out$Mean.L)*.02),0))
l.ylim.sd=c(round((min(out$SD.L)-min(out$SD.L)*.02),0),
            round((max(out$SD.L)+max(out$SD.L)*.02),0))
#-----------------------
# Order rows by time bin
if (nrow(out)==3) {
 x <-c(which(out$TimeBin=="Pleistocene"),
       which(out$TimeBin=="Holocene"),
       which(out$TimeBin=="Modern"))
 out <- out[x,]
 x <- seq(1:3)
 xlim=c(0.5,3.5)
 }

if (nrow(out)==2){
  x <- c(which(out$TimeBin=="Holocene"),
         which(out$TimeBin=="Modern"))
  out <- out[x,]
  x <- seq(1:2)
  xlim=c(0.5,2.5)}

#------------------------
# Plot Weight
pdf(paste0(output_dir, "/", paste("6-OverTime-Mean-StDev-Weight-",species[s],".pdf",sep="")),height=5,width=4)
plot(x,out$Mean.W,
     type="n",
     axes=FALSE,
     ylim=w.ylim.m,
     xlim=xlim,
     xlab="",
     ylab="Mean Weight (g)",
     main=paste(species[s]," Mean and SD - Weight",sep=""))
lines(x,out$Mean.W,
      lwd=1.5,
      col=alpha("black",0.2))
points(x,out$Mean.W,
       pch=19,
       cex=1)
axis(1,lwd=1.5,tcl=-0.3,labels=out$TimeBin,at=x,las=2)
axis(2,lwd=1.5,tcl=-0.3)

par(new=TRUE)

plot(x,out$SD.W,
     type="n",
     axes=FALSE,
     ylim=w.ylim.sd,
     xlim=xlim,
     xlab="",
     ylab="")
lines(x,out$SD.W,
      lwd=1.5,
      col=alpha("red",0.2))
points(x,out$SD.W,
       pch=19,
       cex=1,
       col="red")

axis(4,lwd=1.5,tcl=-0.3)
mtext("Std. Dev.", side = 4, line = 3)

legend("topright",
       legend = c("Weight", "SD"),
       col    = c("black", "red"),
       lwd    = 2,
       bty    = "n")

box(lwd=2)
dev.off()

#------------------------
# Plot Length
pdf(paste0(output_dir, "/", paste("6-OverTime-Mean-StDev-Length-",species[s],".pdf",sep="")),height=5,width=4)
plot(x,out$Mean.L,
     type="n",
     axes=FALSE,
     ylim=l.ylim.m,
     xlim=xlim,
     xlab="",
     ylab="Mean Length (mm)",
     main=paste(species[s]," Mean and SD - Length",sep=""))
lines(x,out$Mean.L,
      lwd=1.5,
      col=alpha("black",0.2))
points(x,out$Mean.L,
       pch=19,
       cex=1)
axis(1,lwd=1.5,tcl=-0.3,labels=out$TimeBin,at=x,las=2)
axis(2,lwd=1.5,tcl=-0.3)

par(new=TRUE)

plot(x,out$SD.L,
     type="n",
     axes=FALSE,
     ylim=l.ylim.sd,
     xlim=xlim,
     xlab="",
     ylab="")
lines(x,out$SD.L,
      lwd=1.5,
      col=alpha("red",0.2))
points(x,out$SD.L,
       pch=19,
       cex=1,
       col="red")
axis(4,lwd=1.5,tcl=-0.3)
mtext("Std. Dev.", side = 4, line = 3)
legend("topright",
       legend = c("Weight", "SD"),
       col    = c("black", "red"),
       lwd    = 2,
       bty    = "n")
box(lwd=2)
dev.off()

}

########################################################################
# Resampling to track differences in means and SDs between time periods
# Just doing for N. cinerea
########################################################################
df <- data[which(data$Taxon=="Neotoma cinerea"),]

n <- min(table(df$Age.Bin)) # Identify size of smallest time bin

pl <- df[which(df$Age.Bin=="Pleistocene"),]
ho <- df[which(df$Age.Bin=="Holocene"),]
md <- df[which(df$Age.Bin=="Modern"),]

means.wt.out <- dim(0)
means.l.out <- dim(0)
wt.diffs.out <- dim(0)
l.diffs.out <- dim(0)

for (i in 1:10000) { # Loop through resample iterations
  temp.pl <- pl[sample(row.names(pl),n,replace=TRUE),]
  temp.ho <- ho[sample(row.names(ho),n,replace=TRUE),]
  mn.pl.w <- mean(temp.pl$Weight)
  sd.pl.w <- sd(temp.pl$Weight)
  mn.pl.l <- mean(temp.pl$Length)
  sd.pl.l <- sd(temp.pl$Length)
  
  mn.ho.w <- mean(temp.ho$Weight)
  sd.ho.w <- sd(temp.ho$Weight)
  mn.ho.l <- mean(temp.ho$Length)
  sd.ho.l <- sd(temp.ho$Length)
  
  mn.mo.w <- mean(md$Weight)
  sd.mo.w <- sd(md$Weight)
  mn.mo.l <- mean(md$Length)
  sd.mo.l <- sd(md$Length)
  
  wt.means <- c(i,mn.pl.w, mn.ho.w, mn.mo.w, sd.pl.w, sd.ho.w, sd.mo.w)
  l.means <- c(i,mn.pl.l, mn.ho.l, mn.mo.l, sd.pl.l, sd.ho.l, sd.mo.l)
  means.wt.out <- rbind(means.wt.out,wt.means)
  means.l.out <- rbind(means.l.out,l.means)
    
  diff.pl.ho.w <- mn.pl.w - mn.ho.w
  diff.pl.ho.w.sd <- sd.pl.w - sd.ho.w
  diff.pl.ho.l <- mn.pl.l - mn.ho.l
  diff.pl.ho.l.sd <- sd.pl.l - sd.ho.l
  
  diff.ho.mo.w <- mn.ho.w - mn.mo.w
  diff.ho.mo.w.sd <- sd.ho.w - sd.mo.w
  diff.ho.mo.l <- mn.ho.l - mn.mo.l
  diff.ho.mo.l.sd <- sd.ho.l - sd.mo.l

  wt.diffs <- c(i,diff.pl.ho.w, diff.ho.mo.w, diff.pl.ho.w.sd, diff.ho.mo.w.sd)
  l.diffs <- c(i,diff.pl.ho.l, diff.ho.mo.l, diff.pl.ho.l.sd, diff.ho.mo.l.sd)
  wt.diffs.out <- rbind(wt.diffs.out,wt.diffs)
  l.diffs.out <- rbind(l.diffs.out,l.diffs)
}
colnames(means.wt.out) <- c("Iteration","Pl.mn","Ho.mn","Mo.mn","Pl.sd","Ho.sd","Mo.sd")
colnames(means.l.out) <- c("Iteration","Pl.mn","Ho.mn","Mo.mn","Pl.sd","Ho.sd","Mo.sd")
means.wt.out <- data.frame(means.wt.out)
means.l.out <- data.frame(means.l.out)

colnames(wt.diffs.out) <- c("Iteration","Pl.Ho","Ho.Mo","SD.pl.ho","SD.ho.mo")
colnames(l.diffs.out) <- c("Iteration","Pl.Ho","Ho.Mo","SD.pl.ho","SD.ho.mo")
wt.diffs.out <- data.frame(wt.diffs.out)
l.diffs.out <- data.frame(l.diffs.out)

# Create output object with mean of the means, and mean of the SDs
wt <- apply(means.wt.out,2,mean)
l <- apply(means.l.out,2,mean)

Resampled.Out <- data.frame(rbind(wt,l))
Resampled.Out$Iteration <- rep(10000,nrow(Resampled.Out))
row.names(Resampled.Out) <- c("Weight","Length")
write.csv(Resampled.Out,paste0(output_dir, "/6-OverTime-Resampled_Means_SDs.csv"),row.names=TRUE)

pdf(paste0(output_dir, "/6-OverTime-Resampled-Means-StDevs-Weight.pdf"),height=5,width=4)
x <- c(1:3)
plot(x,Resampled.Out[1,c(2:4)],
     type="n",
     axes=FALSE,
     ylim=c(245,305),
     xlim=c(0.5,3.5),
     xlab="",
     ylab="Mean Weight (g)",
     main="Mean and SD - Weight - Resampled")
lines(x,Resampled.Out[1,c(2:4)],
      lwd=1.5,
      col=alpha("black",0.2))
points(x,Resampled.Out[1,c(2:4)],
       pch=19,
       cex=1)
axis(1,lwd=1.5,tcl=-0.3,labels=c("Pleistocene","Holocene","Modern"),at=x,las=2)
axis(2,lwd=1.5,tcl=-0.3)

par(new=TRUE)

plot(x,Resampled.Out[1,c(5:7)],
     type="n",
     axes=FALSE,
     ylim=c(35,75),
     xlim=c(0.5,3.5),
     xlab="",
     ylab="")
lines(x,Resampled.Out[1,c(5:7)],
      lwd=1.5,
      col=alpha("red",0.2))
points(x,Resampled.Out[1,c(5:7)],
       pch=19,
       cex=1,
       col="red")

axis(4,lwd=1.5,tcl=-0.3)
mtext("Std. Dev.", side = 4, line = 3)

legend("topright",
       legend = c("Weight", "SD"),
       col    = c("black", "red"),
       lwd    = 2,
       bty    = "n")
box(lwd=2)
dev.off()

pdf(paste0(output_dir, "/6-OverTime-Resampled-Means-StDevs-Length.pdf"),height=5,width=4)
x <- c(1:3)
plot(x,Resampled.Out[2,c(2:4)],
     type="n",
     axes=FALSE,
     xlim=c(0.5,3.5),
     ylim=c(355,370),
     xlab="",
     ylab="Mean Length (mm)",
     main="Mean and SD - Length - Resampled")
lines(x,Resampled.Out[2,c(2:4)],
      lwd=1.5,
      col=alpha("black",0.2))
points(x,Resampled.Out[2,c(2:4)],
       pch=19,
       cex=1)
axis(1,lwd=1.5,tcl=-0.3,labels=c("Pleistocene","Holocene","Modern"),at=x,las=2)
axis(2,lwd=1.5,tcl=-0.3)

par(new=TRUE)

plot(x,Resampled.Out[2,c(5:7)],
     type="n",
     axes=FALSE,
     ylim=c(15,25),
     xlim=c(0.5,3.5),
     xlab="",
     ylab="")
lines(x,Resampled.Out[2,c(5:7)],
      lwd=1.5,
      col=alpha("red",0.2))
points(x,Resampled.Out[2,c(5:7)],
       pch=19,
       cex=1,
       col="red")

axis(4,lwd=1.5,tcl=-0.3)
mtext("Std. Dev.", side = 4, line = 3)

legend("topright",
       legend = c("Weight", "SD"),
       col    = c("black", "red"),
       lwd    = 2,
       bty    = "n")
box(lwd=2)
dev.off()

# Calculate difference percentages
wt.pr.pl.ho <- length(which(wt.diffs.out$Pl.Ho<=0))/nrow(wt.diffs.out)
wt.pr.ho.mo <- length(which(wt.diffs.out$Ho.Mo<=0))/nrow(wt.diffs.out)
wt.sd.pr.pl.ho <- length(which(wt.diffs.out$SD.pl.ho<=0))/nrow(wt.diffs.out)
wt.sd.pr.ho.mo <- length(which(wt.diffs.out$SD.ho.mo<=0))/nrow(wt.diffs.out)

l.pr.pl.ho <- length(which(l.diffs.out$Pl.Ho<=0))/nrow(l.diffs.out)
l.pr.ho.mo <- length(which(l.diffs.out$Ho.Mo<=0))/nrow(l.diffs.out)
l.sd.pr.pl.ho <- length(which(l.diffs.out$SD.pl.ho<=0))/nrow(l.diffs.out)
l.sd.pr.ho.mo <- length(which(l.diffs.out$SD.ho.mo<=0))/nrow(l.diffs.out)

wt.pr <- rbind(wt.pr.pl.ho,wt.pr.ho.mo,wt.sd.pr.pl.ho,wt.sd.pr.ho.mo)
l.pr <- rbind(l.pr.pl.ho,l.pr.ho.mo,l.sd.pr.pl.ho,l.sd.pr.ho.mo)

pdf(paste0(output_dir, "/6-Histograms-Resampling-Wt.pdf"),height=7,width=9)
breaks=30
par(mfrow=c(2,2))
hist(wt.diffs.out$Pl.Ho, breaks=breaks,
     main="Weight Pleist - Holo",
     xlab="Weight Difference (g)")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% P-H <=0: ",wt.pr.pl.ho,sep=""))

hist(wt.diffs.out$Ho.Mo,breaks=breaks,
     main="Weight Holo - Mod",
     xlab="Weight Difference (g)")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% H-M <=0: ",wt.pr.ho.mo,sep=""))

hist(wt.diffs.out$SD.pl.ho,breaks=breaks,
     main="SD Weight Pleist - Holo",
     xlab="SD Difference")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% P-H <=0: ",wt.sd.pr.pl.ho,sep=""))

hist(wt.diffs.out$SD.ho.mo,breaks=breaks,
     main="SD Weight Holo - Mod",
     xlab = "SD Difference")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% H-M <=0: ",wt.sd.pr.ho.mo,sep=""))
dev.off()

###############################################
pdf(paste0(output_dir, "/6-Histograms-Resampling-L.pdf"),height=7,width=9)
breaks=30
par(mfrow=c(2,2))
hist(l.diffs.out$Pl.Ho, breaks=breaks,
     main="Length Pleist - Holo",
     xlab="Length Difference (mm)")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% P-H <=0: ",l.pr.pl.ho,sep=""))

hist(l.diffs.out$Ho.Mo,breaks=breaks,
     main="Length Holo - Mod",
     xlab="SD Difference (mm)")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% H-M <=0: ",l.pr.ho.mo,sep=""))

hist(l.diffs.out$SD.pl.ho,breaks=breaks,
     main="SD Length Pleist - Holo",
     xlab="Length Difference (mm)")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% P-H <=0: ",l.sd.pr.pl.ho,sep=""))

hist(l.diffs.out$SD.ho.mo,breaks=breaks,
     main="SD Length Holo - Mod",
     xlab = "SD Difference (mm)")
abline(v=0,col="orange",lwd=2)
legend("topright",legend=paste("% H-M <=0: ",l.sd.pr.ho.mo,sep=""))
dev.off()
