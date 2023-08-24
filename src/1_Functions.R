#load bootstrap files #############
load_bootstrap_files <- function(x){
  bootstrap_files <<- list.files(x, full.names=T, recursive=FALSE)
  print("Files found:")
  print(bootstrap_files)
  females_bootstrap <<- bootstrap_files[grepl("Female", bootstrap_files)&!grepl("combined", bootstrap_files)]
  males_bootstrap <<- bootstrap_files[grepl("/Male", bootstrap_files)&!grepl("combined", bootstrap_files)]
  print("")
  print("Files that will not be merged into the combined file (because they are combined already):")
  print(bootstrap_files[grepl("combined", bootstrap_files)])
  }

# Select a subset of male donors ##################
select_males_only<-function(){
  sel<-with(data, which(Geslacht =="M" & Donatiesoortcode =="Volbloed"))
  length(sel) 
  # I don't think we have volume data in FinDonor
  # sel<-sel[data$AfgenomenVolume[sel]>400]
  # length(sel) 
  sel<-sel[!is.na(data$Hb[sel])]
  length(sel) 
  table(round(data$Hb[sel],1))
  sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  length(sel) 
  
  # only donations with ferritin values and full history
  sel<-sel[!is.na(data$Ferritine[sel])]
  length(sel) 
  sel<-sel[data$Hb[sel]>=8.4 & data$Hb[sel]<15]
  length(sel) 
  
  with(data[sel,],table(numdon))
  # Identify donors with first ferritin measurement at first intake
  # TODO: Find how to deal with this, is this necessary? We don't have this in Findonor.
  # selnewfer<-with(data, which(!is.na(Ferritine) & Donatiesoortcode =="Nieuwe donorkeuring"))
  # 
  # sel<-sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  length(sel)
  
  # Create sub-dataset
  datt<-with(data,as.data.frame(cbind(KeyID[sel], Leeftijd[sel], Hb[sel], BMI[sel], numdon[sel], Hb[sel]-RefHb[sel], Ferritine[sel])))
  colnames(datt)<-c("KeyID", "age", "Hb", "BMI", "numdon", "dRefHb" , "Ferritin")
  datt <- datt %>% 
    mutate(age = as.numeric(age),
           Hb = as.numeric(Hb),
           BMI = as.numeric(BMI),
           numdon = as.numeric(numdon),
           dRefHb = as.numeric(dRefHb),
           Ferritin = as.numeric(Ferritin))
  datt$lFer<-log10(datt$Ferritin)
  
  return(datt)
  
}

# Select a subset of female donors #############################
select_females_only<-function(){
  sel<-with(data, which(Geslacht =="F" & Donatiesoortcode =="Volbloed"))
  length(sel) 
  # I don't think we have volume data in FinDonor
  # sel<-sel[data$AfgenomenVolume[sel]>400]
  # length(sel) 
  sel<-sel[!is.na(data$Hb[sel])]
  length(sel) 
  table(round(data$Hb[sel],1))
  sel<-sel[data$Hb[sel]>5 & data$Hb[sel]<15]
  length(sel) 
  
  # only donations with ferritin values and full history
  sel<-sel[!is.na(data$Ferritine[sel])]
  length(sel) 
  sel<-sel[data$Hb[sel]>=7.8 & data$Hb[sel]<15]
  length(sel) 
  
  with(data[sel,],table(numdon))
  # Identify donors with first ferritin measurement at first intake
  # TODO: Find how to deal with this, is this necessary? We don't have this in Findonor.
  # selnewfer<-with(data, which(!is.na(Ferritine) & Donatiesoortcode =="Nieuwe donorkeuring"))
  # 
  # sel<-sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  length(sel)
  
  # Create sub-dataset
  datt<-with(data, as.data.frame(cbind(KeyID[sel], Leeftijd[sel], Hb[sel], BMI[sel], numdon[sel], Hb[sel]-RefHb[sel], Ferritine[sel])))
  colnames(datt)<-c("KeyID", "age", "Hb", "BMI", "numdon", "dRefHb" , "Ferritin")
  datt <- datt %>% 
    mutate(age = as.numeric(age),
           Hb = as.numeric(Hb),
           BMI = as.numeric(BMI),
           numdon = as.numeric(numdon),
           dRefHb = as.numeric(dRefHb),
           Ferritin = as.numeric(Ferritin))
  datt$lFer<-log10(datt$Ferritin)
  
  return(datt)
  
}

# Plot hb Fer ###################
plotHbFer<-function(CIs) {
  set.seed(1)
  with(fitdata,plot(jitter(lFer,amount=.1),dRefHb, xlim=c(0.2,3), ylim=c(-2.5,2.5), main=title, 
                    xaxt="n", xlab="Ferritin [ng/mL]", ylab="Hb change from baseline value [mmol/L]"))
  xvals<-log10(15*2^(-8:8))
  xlabs<-(15*2^(-8:8))
  axis(1, at=xvals,labels=xlabs,las=1)
  abline(h=0,col=8)
  if (!missing(CIs)) { # Add information on confidence intervals
    lines(CIs$x,CIs$ulimfit, col=8, lty=3)
    lines(CIs$x,CIs$llimfit, col=8, lty=3)
    # 95% confidence intervals for the changepoint
    abline(v=CIs$CIcp, col=8, lty=3)
  }
  xm<-plotsol()
  print(paste("Changepoint:",10^xm[3]))
  with(fitdata[order(fitdata$lFer),], 
       lines(lFer, rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)),  col = 3,  lwd = 2))
  abline(v=xm[3],col=7, lwd=2, lty=2)
  print(paste("SolutionOk:",SolutionOk(xm)))
  if (missing(CIs)) { # Add information on confidence intervals
    legend("topright", c("Fitted association", paste0("Changepoint level (", round(10^xm[3],1), " ng/mL)"), paste0("Rolling mean (n=", rollmean_nr,")")), 
           lty=c(1,2,1), lwd=c(2,2,2), col=c(2,7,3), cex=0.7)
  } else {
    legend("topright", c("Fitted association", paste0("Changepoint level (", round(10^xm[3],1), " ng/mL)"), paste0("Rolling mean (n=", rollmean_nr,")"), "95% Confidence interval"), 
           lty=c(1,2,1,3), lwd=c(2,2,2,1), col=c(2,7,3,8), cex=0.7)
  }
  return(xm)
}

# function to estimate and plot the changepoint on the fitdata dataset ##################
plotsol<-function(col=2, lwd=2){
  # set initial values vor the changepoint using the linear regression results
  lfit<-lm(dRefHb~lFer, data=fitdata)
  x<-as.numeric(c(lfit$coefficients, -lfit$coefficients[1]/lfit$coefficients[2], sd(lfit$residuals)))
  # x<-as.numeric(c(lfit$coefficients, 1.5, sd(lfit$residuals)))
  
  # calculate the parameter estimates which maximize the likelihood function
  sol<-fminsearch(fitf, x, lower=ll, upper=ul, method="Hooke-Jeeves")
  
  # plot and print the results
  xs<-c(0, sol$xmin[3],5)
  lines(xs, yvalsfitted(xs, sol$xmin), col=col, lwd=lwd)
  print(paste(paste(sol$xmin, collapse = " "), sol$convergence, sol$info$iterations), SolutionOk(sol$xmin))
  return(sol$xmin)
}

# Rough check on the minimum found #######################
SolutionOk<-function(x){
  o<-c()
  for (i in 1:4) {
    x2<-x
    x2[i]<-x[i]+1e-5
    o<-c(o,fitf(x2)-fitf(x))
  }
  # return(paste(paste(o, collapse=" "), ifelse(length(which(o<0))>0, "ERROR", "")))
  # if false then not a minimum
  return(ifelse(length(which(o<0))>0, F, T))
}

# Function to calculate likelihood ##################
fitf<-function(x) {
  # function to calculate the likelihood of a function with a linear increase with intercept x[1]  
  # and slope x[2], a subsequent changepoint at x[3] after which the y-value remains constant
  # and random error with standard deviation x[4]
  # this function makes use of the fitdata
  bt<-fitdata$lFer>x[3]
  -sum(dnorm(fitdata$dRefHb[!bt]-(x[1]+x[2]*fitdata$lFer[!bt]), mean=0, sd=x[4], log=T))-
    sum(dnorm(fitdata$dRefHb[ bt]-(x[1]+x[2]*x[3]             ), mean=0, sd=x[4], log=T))
}

# Calculate y values for given x and changepoint ###############
yvalsfitted<-function(x, sol){
  # function to calculate y values for a given set of x values and changepoint parameters (sol)
  ifelse(x>sol[3], sol[1]+sol[2]*sol[3], sol[1]+sol[2]*x)
}
