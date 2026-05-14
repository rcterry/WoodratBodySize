#---------------------------------------------
# Evaluate Crown Height vs. Weight and Length
#----------------------------------------------

# --- Load your dataset ---
data <- read.csv(paste0(output_dir, "/4.0-PaisleyFossils-PredictedSize.csv"))

# --- Correlation test for Mean.Weight and Crown.height ---
cor_weight <- cor.test(data$Weight, data$Crown.Height, method = "pearson")
# print(cor_weight)

# --- Correlation test for Mean.TL and Crown.height ---
cor_tl <- cor.test(data$Length, data$Crown.Height, method = "pearson")
# print(cor_tl)

pdf(paste0(output_dir, "/4.3-CrownHeight-v-Size.pdf"),height=8,width=4)
par(mfrow=c(2,1))
plot(Weight ~ Crown.Height, data = data,
     pch = 19, col = alpha("black",0.5), cex = 1.2)
model <- lm(Weight ~ Crown.Height, data = data)
abline(model, col = "black")
legend("topright",
       legend=paste("cor = ",
                    round(cor_weight$estimate,2),
                    "\np = ",round(cor_weight$p.value,2),
                    sep=""))

# --- Plot Mean.TL vs. Crown.height ---
plot(Length ~ Crown.Height, data = data,
     pch = 19, col = alpha("black",0.5), cex = 1.2)
model <- lm(Length ~ Crown.Height, data = data)
abline(model, col = "blue")
legend("topright",
       legend=paste("cor = ",
                    round(cor_tl$estimate,2),
                    "\np = ",round(cor_tl$p.value,2),
                    sep=""))
dev.off()

#####################
# Look Within Species
species <- unique(data$Taxon)

for (s in 1:length(species)) {
  temp <- data[which(data$Taxon==species[s]),]
  temp <- temp[-which(is.na(temp$Crown.Height)),]
  temp$Bchron.Age <- -temp$Bchron.Age
  
  # --- Correlation test for Mean.Weight and Crown.height ---
  cor_weight <- cor.test(temp$Weight, temp$Crown.Height, method = "pearson")
  cor.p <- cor_weight$p.value
  cor.r <- cor_weight$estimate
  
  x <- lm(Length ~ Crown.Height, data=temp)
  xx <- summary(x)
  rx <- xx$adj.r.squared
  px <- xx$coefficients[2,4]
  
  a <- lm(Crown.Height ~ Bchron.Age, data=temp)
  aa <- summary(a)
  r <- aa$adj.r.squared
  p <- aa$coefficients[2,4]
  
     pdf(paste0(output_dir, "/", paste("4.3-CrownHeight-Stats-",species[s],"-Weight.pdf",sep="")),height=4.5,width=8.5)
  par(mfrow=c(1,2))
  plot(temp$Crown.Height,temp$Weight,
       xlab="Crown Height (mm)",
       ylab="Weight (g)",
       pch = 19, cex = 1.2, col=alpha("black",0.5))
  abline(x)
  legend("topright",legend=c("Cor Stats:",
                             paste("r = ",round(cor.r,2)),
                             paste("p = ",round(cor.p,2))))
  legend("bottomleft",legend=c("Reg Stats:",
                             paste("r2 = ",round(rx,2)),
                             paste("p = ",round(px,2))))

  plot(temp$Bchron.Age,temp$Crown.Height,
       main = species[s],
       xlab = "Cal Years BP",
       ylab = "Crown Height (mm)",
       ylim=c(0,6),
       pch = 19, cex = 1.2, col=alpha("black",0.5))
  abline(a)
  legend("topright",legend=c("Regr. Stats:",
                             paste("adj.r = ", round(r,4)),
                             paste("p = ", round(p,2))))
  
  dev.off()
  
  # --- Correlation test for Mean.TL and Crown.height ---
  cor_length <- cor.test(temp$Length, temp$Crown.Height, method = "pearson")
  cor.p <- cor_length$p.value
  cor.r <- cor_length$estimate
 
  x <- lm(Length ~ Crown.Height, data=temp)
  xx <- summary(x)
  rx <- xx$adj.r.squared
  px <- xx$coefficients[2,4]
  
  a <- lm(Crown.Height ~ Bchron.Age, data=temp)
  aa <- summary(a)
  r <- aa$adj.r.squared
  p <- aa$coefficients[2,4]
  
     pdf(paste0(output_dir, "/", paste("4.3-CrownHeight-Stats-",species[s],"-Length.pdf",sep="")),height=4.5,width=8.5)
  par(mfrow=c(1,2))
  plot(temp$Crown.Height,temp$Length,
       xlab="Crown Height (mm)",
       ylab="Length (mm)",
       pch = 19, cex = 1.2, col=alpha("black",0.5))
  abline(x)
  legend("topright",legend=c("Cor Stats:",
                             paste("r = ",round(cor.r,2)),
                             paste("p = ",round(cor.p,2))))
  legend("bottomleft",legend=c("Reg Stats:",
                               paste("r2 = ",round(rx,2)),
                               paste("p = ",round(px,2))))
  
  plot(temp$Bchron.Age,temp$Crown.Height,
       main = species[s],
       xlab = "Cal Years BP",
       ylab = "Crown Height (mm)",
       ylim=c(0,6),
       pch = 19, cex = 1.2, col=alpha("black",0.5))
  abline(a)
  legend("topright",legend=c("Regr. Stats:",
                             paste("adj.r = ", round(r,4)),
                             paste("p = ", round(p,2))))
  dev.off()

}






