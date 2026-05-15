#################################################
# CHECK FOR SEX DIFFERENCES IN BODY SIZE
#################################################

data <- read.csv(paste0(data_dir, "/Museum-Neotoma-MAXILLA.csv"))

## Remove rows where no SEX info recorded
data <- data[-which(data$SEX==""),]

# --- Subset to only Male and Female ---
species <- unique(data$SPECIES)
sex <- unique(data$SEX)


pdf(paste0(output_dir, "/2.4-SexDiffs-Boxplots-Length.pdf"),height=10,width = 3)
par(mfrow=c(4,1))
T <- t.test(Total.length..TL.~SEX,data=data)
boxplot(Total.length..TL. ~ SEX, 
        data = data,
        border = "black",
        main = "Total Length by Sex (All Species Combined)",
        xlab = "",
        ylab = "Total Length (mm)"
)
legend("bottomright",legend=paste("p = ",round(T$p.value,3),sep=""))

for (s in 1:length(species)){
    temp <- data[which(data$SPECIES==species[s]),]
    T <- t.test(Total.length..TL.~SEX,data=temp)
    boxplot(Total.length..TL. ~ SEX, 
            data = temp,
            border = "black",
            main = paste("Total Length by Sex (",species[s],")",sep=""),
            xlab = "",
            ylab = "Total Length (mm)"
    )
    legend("bottomright",legend=paste("p = ",round(T$p.value,3),sep=""))
}
dev.off()

## REPEAT FOR WEIGHT
species <- unique(data$SPECIES)
sex <- unique(data$SEX)


pdf(paste0(output_dir, "/2.4-SexDiffs-Boxplots-Weight.pdf"),height=10,width = 3)
par(mfrow=c(4,1))
T <- t.test(Weight~SEX,data=data)
boxplot(Weight ~ SEX, 
        data = data,
        border = "black",
        main = "Weight by Sex (All Species Combined)",
        xlab = "",
        ylab = "Weight (g)"
)
legend("bottomright",legend=paste("p = ",round(T$p.value,3),sep=""))

for (s in 1:length(species)){
  temp <- data[which(data$SPECIES==species[s]),]
  T <- t.test(Weight~SEX,data=temp)
  boxplot(Weight ~ SEX, 
          data = temp,
          border = "black",
          main = paste("Weight by Sex (",species[s],")",sep=""),
          xlab = "",
          ylab = "Weight (g)"
  )
  legend("bottomright",legend=paste("p = ",round(T$p.value,3),sep=""))
}
dev.off()

#######################
# Species by Sex Means
#########################
w <- aggregate(data$Weight,by=list(data$SPECIES,data$SEX),mean,na.rm=TRUE)
wsd <- aggregate(data$Weight,by=list(data$SPECIES,data$SEX),sd,na.rm=TRUE)
colnames(w) <- c("Species","Sex","Weight")
colnames(wsd) <- c("Species","Sex","Weight SD")
out <- merge(w,wsd)

l <- aggregate(data$Total.length..TL.,by=list(data$SPECIES,data$SEX),mean,na.rm=TRUE)
lsd <- aggregate(data$Total.length..TL.,by=list(data$SPECIES,data$SEX),sd,na.rm=TRUE)
colnames(l) <- c("Species","Sex","Length")
colnames(lsd) <- c("Species","Sex","Length SD")
out <- merge(out,l)
out <- merge(out,lsd)

write.csv(out,
          paste0(output_dir,'/2.4-SpeciesBySex-Means.csv'))

