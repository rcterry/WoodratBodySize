# -------------------------
# Fit Models - WEIGHT
# -------------------------
fig_dir <- paste0(output_dir, "/3-Allometries/Weight/")
combined_pdf <- paste0(fig_dir, "3.1-Allometries-Weight-Combined.pdf")
pdf(combined_pdf, height = 10.5, width = 7.5, onefile = TRUE)
combined_dev <- dev.cur()
par(mfrow = c(4, 3), mar = c(4, 4, 3, 1), cex = 0.6)


# MAXILLA ################################################################
# --------------------------
# Read in Data
data <- read.csv(paste0(data_dir, "/Museum-Neotoma-MAXILLA.csv"))
# colnames(data)[which(colnames(data)=="Total.length..TL.")] <- "Length"

# ~~~~~ Loop for Weight ~~~~~~~~~~~~~~~~~~~~~~~
species <- unique(data$SPECIES)
measures <- c("Alveolar.length",
              "Diastema.length",
              "M1.length",
              "M2.Length",
              "M3.Length",
              "Premax.to.frontal",
              "M1.Width",
              "M1M2.Length")

nls.out <- dim(0)
ls.out <- dim(0)

for (s in 1:length(species))  {
  ls.model.out <- list()
  nls.model.out <- list()
  
  temp <- data[which(data$SPECIES==species[s]),]

  for (m in 1:length(measures)) {
    xy <- cbind(temp[,measures[m]],temp[, 'Weight'])
    xy_clean <- xy[complete.cases(xy), ]
    x <- xy_clean[,1]
    y <- xy_clean[,2]

    main = paste0("Maxilla-Weight-",species[s],"-",measures[m],sep="")
  
    # Linear log-log least squares ~~~~~~~~~~~~~~~~~~~~~
    # Define the model
    model.ls <- lm(log(y) ~ log(x),
                   na.action = 'na.omit')
    
    # Save the model to a list that will contain all the models
    ls.model.out[[paste0("ls_",main)]] <- model.ls
    
    # Pull out the model parameters
    z <- summary(model.ls)
    a <- exp(coef(model.ls)[1])
    b <- coef(model.ls)[2] 
    
    # Pull out R2 and pvalue
    r <- z$r.squared
    pv <- z$coefficients[2,4]
    
    # %PE calculation
    p <- exp(predict(model.ls)) # predict and then exponentiate to convert back to linear scale
    pe <- abs(y - p)/p
    pe.mn <- mean(pe)
    
    # AIC and loglikelihood calculation; AIC = -2 * log_likelihood + 2 * num_params
    aic <- AIC(model.ls)
    aicc <- AICc(model.ls)
    lglk <- logLik(model.ls)
    
    # Output model info to collection object
    ls <- c("maxilla",species[s],measures[m],nrow(xy_clean),r,pv,pe.mn,aic,aicc,lglk,a,b)
    ls.out <- rbind(ls.out,ls)
    
    pdf(file = paste0(fig_dir, main, '-ls.pdf'),
        height = 3,
        width = 4)
    par(cex = 0.6)
    plot(log(y) ~ log(x),
         pch = 1,
         cex = 1.2,
         main = main,
         xlab = paste('log', measures[m]),
         ylab = "log Weight (g)")
    abline(coef(model.ls),
           col = 'blue')
    legend("topleft", 
           legend = c(
             paste0("N = ", nrow(xy_clean)),
             paste0("R^2 = ", round(r, 2)),
             paste0("p = ", round(pv, 5)),
             paste0("pe = ", round(pe.mn,2)),
             paste0("aic = ", round(aic,3)),
             paste0("aicc = ", round(aicc,3)),
             paste0("lglk = ",round(lglk,3)),
             paste0('a = ', round(exp(coef(model.ls)[1]), 3)),
             paste0('b = ', round(coef(model.ls)[2], 3)))
    )
    dev.off()

    # Nonlinear least squares ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define the model
    # Use estimates from log-log linear model as start for nls
    # a.start <- exp(coef(model.ls)[1])
    b.start <- coef(model.ls)[2]
    
    # model.nls <- nls(y ~ a * x^b,
    #                  start = list(a = 1, b = 1),
    #                  na.action = 'na.omit')

     model.nls <- nls(y ~ a * x^b,
                     start = list(a = 1, b = b.start),
                     na.action = 'na.omit')
    
    # Pull out the model parameters
    z <- summary(model.nls)
    
    # Save the model to a list that will contain all the models
    nls.model.out[[paste0("nls_",main)]] <- model.nls
    
    # r2 Calculation
    sst2 <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    sse2 <- sum(residuals(model.nls)^2, na.rm = TRUE)
    r_squared <- 1 - (sse2 / sst2)
    
    # Pull pvalue
    pv <- z$coefficients[2,4]
    
    # %PE calculation
    p <- predict(model.nls)
    pe <- abs(y - p)/p
    pe.mn <- mean(pe)
    
    # calculate AIC and log likelihood
    aic <- AIC(model.nls)   #AIC = -2 * log_likelihood + 2 * num_params
    aicc <- AICc(model.nls)
    lglk <- logLik(model.nls)
  
    # Output model info to collection object
    nls <- c("maxilla",species[s],measures[m],nrow(xy_clean),r_squared,pv,pe.mn,aic,aicc,lglk,z$coefficients[,1])
    nls.out <- rbind(nls.out,nls)
    
    # Plotting
    pdf(file = paste0(fig_dir, main, '-nls.pdf'),
        height = 3,
        width = 4)
    par(cex = 0.6)
      plot(y ~ x,
           pch = 1,
           cex = 1.2,
           main = main, 
           xlab = measures[m],
           ylab = "Weight (g)")
      curve(predict(model.nls, 
                    newdata = data.frame(x = x)), 
            add = TRUE, 
            col = "blue")
      legend("topleft", 
             legend = c(
               paste0('N = ',nrow(xy_clean)),
               paste0('R^2 =', round(r_squared, 2)),
               paste0('p = ', round(pv,5)),
               paste0('pe = ', round(pe.mn,2)),
               paste0('aic = ', round(aic,3)),
               paste0('aicc = ', round(aicc,3)),
               paste0('lglk = ', round(lglk,3)),
               paste0('a = ', round(coef(model.nls)[1], 3)),
               paste0('b = ', round(coef(model.nls)[2], 3)))
      )
    dev.off()

    # Add the same figure to the combined multi-panel PDF
     dev.set(combined_dev)
    par(cex = 0.6)
    plot(y ~ x,
         pch = 1,
         cex = 1.2,
         main = main, 
         xlab = measures[m],
         ylab = "Weight (g)")
    curve(predict(model.nls, 
                  newdata = data.frame(x = x)), 
          add = TRUE, 
          col = "blue")
  }
  
  nls.model.out
  ls.model.out
  
  
}
   
nls.out <- data.frame(nls.out)
colnames(nls.out) <- c("Bone","Species","Measurement","N","nls.r2","nls.p","nls.pe","nls.aic","nls.aicc","nls.lglk","nls.a","nls.b")
for(c in 4:ncol(nls.out)){nls.out[,c]<-as.numeric(nls.out[,c])}

ls.out <- data.frame(ls.out)
colnames(ls.out) <- c("Bone","Species","Measurement","N","ls.r2","ls.p","ls.pe","ls.aic","ls.aicc","ls.lglk","ls.a","ls.b")
for(c in 4:ncol(ls.out)){ls.out[,c]<-as.numeric(ls.out[,c])}

Max.Models <- merge(nls.out,ls.out)

# ~~~~~ Output data file with model fits ~~~~~~~~
write.csv(Max.Models,paste0(output_dir, "/3.1-Models-Maxilla-Weight.csv"),row.names=F)

##############################
# NLS vs LS Model Performance
sink(paste0(output_dir, "/3.1-Maxilla-NLS_vs_LS-Model_Estimates-Weight.txt"))
cat("a\n")
print(t.test(Max.Models$nls.a,Max.Models$ls.a))
cat("\n")
cat("b\n")
print(t.test(Max.Models$nls.b,Max.Models$ls.b))
cat("\n")
cat("r2\n")
print(t.test(Max.Models$nls.r2,Max.Models$ls.r2))
cat("\n")
cat("pe\n")
print(t.test(Max.Models$nls.pe,Max.Models$ls.pe))
sink()

pdf(paste0(output_dir, "/3.1-Maxilla-NLS_vs_LS-Model-PE-Weight.pdf"),height=4,width=5)
plot(Max.Models$nls.pe,Max.Models$ls.pe,
     main = "Maxillary Models",
     xlab = "NLS prediction error",
     ylab = "LS prediction error",
     pch = 19)
abline(0,1)
dev.off()

pdf(paste0(output_dir, "/3.1-Maxilla-NLS_vs_LS-Model-R2-Weight.pdf"),height=4,width=5)
plot(Max.Models$nls.r2,Max.Models$ls.r2,
     main = "Maxillary Models",
     xlab = "NLS R^2",
     ylab = "LS R^2",
     pch = 19)
abline(0,1)
dev.off()


############################################################################
# MANDIBLES ################################################################
############################################################################
fig_dir <- paste0(output_dir, "/3-Allometries/Weight/")

# --------------------------
# Read in Data
data <- read.csv(paste0(data_dir, "/Museum-Neotoma-MANDIBLE.csv"))
# colnames(data)[which(colnames(data)=="Total.length..TL.")] <- "Length"

# ~~~~~ Loop for Weight ~~~~~~~~~~~~~~~~~~~~~~~
species <- unique(data$SPECIES)
measures <- c("Alveolar.length",
              "Diastema.length",
              "M1.length",
              "M2.Length",
              "M3.Length",
              "M1.Width",
              "M1M2.Length")

nls.out <- dim(0)
ls.out <- dim(0)

for (s in 1:length(species))  {
  ls.model.out <- list()
  nls.model.out <- list()
  
  temp <- data[which(data$SPECIES==species[s]),]
  
  for (m in 1:length(measures)) {
    xy <- cbind(temp[,measures[m]],temp[, 'Weight'])
    xy_clean <- xy[complete.cases(xy), ]
    x <- xy_clean[,1]
    y <- xy_clean[,2]
    
    main = paste0("Mandible-Weight-",species[s],"-",measures[m],sep="")
    
    # Linear log-log least squares ~~~~~~~~~~~~~~~~~~~~~
    # Define the model
    model.ls <- lm(log(y) ~ log(x),
                   na.action = 'na.omit')
    
    # Pull out the model parameters
    z <- summary(model.ls)
    a <- exp(coef(model.ls)[1])
    b <- coef(model.ls)[2] #why is this not exponentiated?  Because it's the slope?
    
    # Pull out R2 and pvalue
    r <- z$r.squared
    pv <- z$coefficients[2,4]
    
    # %PE calculation
    p <- exp(predict(model.ls)) # predict and then exponentiate to convert back to linear scale
    pe <- abs(y - p)/p
    pe.mn <- mean(pe)
    
    # AIC and log likelihood
    aic <- AIC(model.ls)
    aicc <- AICc(model.ls)
    lglk <- logLik(model.ls)
    
    # Output model info to collection object
    ls <- c("mandible",species[s],measures[m],nrow(xy_clean),r,pv,pe.mn,aic,aicc,lglk,a,b)
    ls.out <- rbind(ls.out,ls)
    
    pdf(file = paste0(fig_dir, main, '-ls.pdf'),
        height = 3,
        width = 4)
    par(cex = 0.6)
    plot(log(y) ~ log(x),
         pch = 1,
         cex = 1.2,
         main = main,
         xlab = paste('log', measures[m]),
         ylab = "log Weight (g)")
    abline(coef(model.ls),
           col = 'blue')
    legend("topleft", 
           legend = c(
             paste0("N = ", nrow(xy_clean)),
             paste0("R^2 = ", round(r, 2)),
             paste0("p = ", round(pv, 5)),
             paste0("pe = ", round(pe.mn,2)),
             paste0("aic = ", round(aic,3)),
             paste0("aicc = ", round(aicc,3)),
             paste0("lglk = ", round(lglk,3)),
             paste0('a = ', round(exp(coef(model.ls)[1]), 3)),
             paste0('b = ', round(coef(model.ls)[2], 3)))
    )
    dev.off()

    # Nonlinear least squares ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define the model
    
    # Use estimates from log-log linear model as start for nls
    # a.start <- exp(coef(model.ls)[1])
    b.start <- coef(model.ls)[2]
    
    # model.nls <- nls(y ~ a * x^b,
    #                  start = list(a = 1, b = 1),
    #                  na.action = 'na.omit')
    
     model.nls <- nls(y ~ a * x^b,
                     start = list(a = 1, b = b.start),
                     na.action = 'na.omit')
    
    # Pull out the model parameters
    z <- summary(model.nls)
    
    # r2 Calculation
    sst2 <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    sse2 <- sum(residuals(model.nls)^2, na.rm = TRUE)
    r_squared <- 1 - (sse2 / sst2)
    
    # Pull pvalue
    pv <- z$coefficients[2,4]
    
    # %PE calculation
    p <- predict(model.nls)
    pe <- abs(y - p)/p
    pe.mn <- mean(pe)
    
    # Calculate AIC and log likelihood
    aic <- AIC(model.nls)   #AIC = -2 * log_likelihood + 2 * num_params
    aicc <- AICc(model.nls)
    lglk <- logLik(model.nls)
    
    # Output model info to collection object
    nls <- c("mandible",species[s],measures[m],nrow(xy_clean),r_squared,pv,pe.mn,aic,aicc,lglk,z$coefficients[,1])
    nls.out <- rbind(nls.out,nls)
    
    # Plotting
    pdf(file = paste0(fig_dir, main, '-nls.pdf'),
        height = 3,
        width = 4)
    par(cex = 0.6)
    plot(y ~ x,
         pch = 1,
         cex = 1.2,
         main = main, 
         xlab = measures[m],
         ylab = "Weight (g)")
    curve(predict(model.nls, 
                  newdata = data.frame(x = x)), 
          add = TRUE, 
          col = "blue")
    dev.off()

    # Add the same figure to the combined multi-panel PDF
     dev.set(combined_dev)
    par(cex = 0.6)
    plot(y ~ x,
         pch = 1,
         cex = 1.2,
         main = main, 
         xlab = measures[m],
         ylab = "Weight (g)")
    curve(predict(model.nls, 
                  newdata = data.frame(x = x)), 
          add = TRUE, 
          col = "blue")
  }
}

# Close combined allometries PDF after all maxilla and mandible figures
if (combined_dev %in% dev.list()) {
     dev.off(combined_dev)
}

nls.out <- data.frame(nls.out)
colnames(nls.out) <- c("Bone","Species","Measurement","N","nls.r2","nls.p","nls.pe","nls.aic","nls.aicc","nls.lglk","nls.a","nls.b")
for(c in 4:ncol(nls.out)){nls.out[,c]<-as.numeric(nls.out[,c])}

ls.out <- data.frame(ls.out)
colnames(ls.out) <- c("Bone","Species","Measurement","N","ls.r2","ls.p","ls.pe","ls.aic","ls.aicc","ls.lglk","ls.a","ls.b")
for(c in 4:ncol(ls.out)){ls.out[,c]<-as.numeric(ls.out[,c])}

Max.Models <- merge(nls.out,ls.out)

# ~~~~~ Output data file with model fits ~~~~~~~~
write.csv(Max.Models,paste0(output_dir, "/3.1-Models-Mandible-Weight.csv"),row.names=F)

##############################
# NLS vs LS Model Performance

sink(paste0(output_dir, "/3.1-Mandible-NLS_vs_LS-Model_Estimates-Weight.txt"))
cat("a\n")
print(t.test(Max.Models$nls.a,Max.Models$ls.a))
cat("\n")
cat("b\n")
print(t.test(Max.Models$nls.b,Max.Models$ls.b))
cat("\n")
cat("r2\n")
print(t.test(Max.Models$nls.r2,Max.Models$ls.r2))
cat("\n")
cat("pe\n")
print(t.test(Max.Models$nls.pe,Max.Models$ls.pe))
sink()

pdf(paste0(output_dir, "/3.1-Mandible-NLS_vs_LS-Model-PE-Weight.pdf"),height=4,width=5)
plot(Max.Models$nls.pe,Max.Models$ls.pe,
     main = "Mandibular Models",
     xlab = "NLS prediction error",
     ylab = "LS prediction error",
     pch = 19)
abline(0,1)
dev.off()

pdf(paste0(output_dir, "/3.1-Mandible-NLS_vs_LS-Model-R2-Weight.pdf"),height=4,width=5)
plot(Max.Models$nls.r2,Max.Models$ls.r2,
     main = "Mandibular Models",
     xlab = "NLS R^2",
     ylab = "LS R^2",
     pch = 19)
abline(0,1)
dev.off()







