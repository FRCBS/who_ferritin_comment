#load bootstrap files #############
load_bootstrap_files <- function(x){
  bootstrap_files <<- list.files(x, full.names=T, recursive=FALSE)
  print("Files found:")
  print(bootstrap_files)
  females_bootstrap <<- bootstrap_files[grepl("Female", bootstrap_files)&!grepl("combined", bootstrap_files)]
  males_bootstrap <<- bootstrap_files[grepl("Male", bootstrap_files)&!grepl("combined", bootstrap_files)]
  if(menopausal_status){
  premenopausal_bootstrap <<- bootstrap_files[grepl("Premenopausal", bootstrap_files)&!grepl("combined", bootstrap_files)]
  postmenopausal_bootstrap <<- bootstrap_files[grepl("Postmenopausal", bootstrap_files)&!grepl("combined", bootstrap_files)]
  }
  print("")
  print("Files that will not be merged into the combined file (because they are combined already):")
  print(bootstrap_files[grepl("combined", bootstrap_files)])
  }

# Select a subset of male donors ##################
select_males_only<-function(){
  sel<-with(data, which(Geslacht =="M" & Donatiesoortcode =="Volbloed"))
  length(sel) 
  #sel<-sel[data$AfgenomenVolume[sel]>400]
  #length(sel) 
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
  #selnewfer<-with(data, which(!is.na(Ferritine) & Donatiesoortcode =="Nieuwe donorkeuring"))
  
  #sel<-sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  #length(sel)
  
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
  #sel<-sel[data$AfgenomenVolume[sel]>400]
  #length(sel) 
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
  #selnewfer<-with(data, which(!is.na(Ferritine) & Donatiesoortcode =="Nieuwe donorkeuring"))
  
  #sel<-sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  #length(sel)
  
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

# Select a subset of premenopausal female donors #############################
select_premenopausal_only<-function(){
  sel<-with(data, which(Geslacht =="F" & Donatiesoortcode =="Volbloed" & premenopausal == 1))
  length(sel) 
  #sel<-sel[data$AfgenomenVolume[sel]>400]
  #length(sel) 
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
  #selnewfer<-with(data, which(!is.na(Ferritine) & Donatiesoortcode =="Nieuwe donorkeuring"))
  
  #sel<-sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  #length(sel)
  
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

# Select a subset of premenopausal female donors #############################
select_postmenopausal_only<-function(){
  sel<-with(data, which(Geslacht =="F" & Donatiesoortcode =="Volbloed" & premenopausal == 0))
  length(sel) 
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
  # selnewfer<-with(data, which(!is.na(Ferritine) & Donatiesoortcode =="Nieuwe donorkeuring"))
  # 
  # sel<-sel[!(data$KeyID[sel] %in% data$KeyID[selnewfer])]
  # length(sel)
  
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

# Plot hb Fer ###################
plotHbFer<-function(CIs) {
  set.seed(1)
  with(fitdata,plot(jitter(lFer,amount=.1),jitter(dRefHb, amount=.1), xlim=c(0.2,3), ylim=c(-2.5,2.5), main=title, 
                    xaxt="n", xlab="Ferritin [ng/mL]", ylab="Hb change from baseline value [mmol/L]", col = alpha("black", alpha=0.1)))
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
  fitdata$lFerjit <- jitter(log10(fitdata$Ferritin), amount = 0.01)
  fitdata$rollmean <- with(fitdata[order(fitdata$lFerjit),], rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)))
  rollmean_data <<- fitdata %>% select(lFerjit, rollmean)
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
  # set initial values for the changepoint using the linear regression results
  lfit<-lm(dRefHb~lFer, data=fitdata)
  x<-as.numeric(c(lfit$coefficients, -lfit$coefficients[1]/lfit$coefficients[2], sd(lfit$residuals)))
  # x<-as.numeric(c(lfit$coefficients, 1.5, sd(lfit$residuals)))
  
  if(all(x>ll) & all(x<ul)){
    sol<-fminsearch(fitf, x, lower=ll, upper=ul, method="Hooke-Jeeves")
    
    # plot and print the results
    xs<-c(0, sol$xmin[3],5)
    lines(xs, yvalsfitted(xs, sol$xmin), col=col, lwd=lwd)
    print(paste(paste(sol$xmin, collapse = " "), sol$convergence, sol$info$iterations), SolutionOk(sol$xmin))
    sol_data <<- as.data.frame(cbind(xs, yvalsfitted(xs, sol$xmin)))
    names(sol_data) <<- c("x", "y")
    
    return(sol$xmin)
    
  } else {print("Can't conduct changepoint analysis because (some) parameters are outside limits you defined")
    limits<-(x>ul)+(x<ll)
  limits <- case_when(limits==1 ~ "outside", limits==0 ~"inside")
    return(limits)
    }
  # calculate the parameter estimates which maximize the likelihood function
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

# function to evaluate and plot the fit for various subgroups #################
# the subgroups parameter is defined by the covariate input,
# the cutpoints (cuts) can be either specified by an input vector
# or by an integer value indicating the number of cutpoints
plotCovariateDependency<-function(covariate, cuts, sex) {
  set.seed(1)
  if(sex=="M"){title <- "Male donors"} else if (sex == "F"){title<- "Female donors"}else{title <- "All donors"}
  xlab <- "Ferritin (ng/mL)"
  ylab <- "Hb change from reference value (mmol/L)"
  xaxt <- "n"
  eval(parse(text=paste0("plot<-with(refdata",sex,",plot(jitter(lFer,amount=.1),dRefHb, xlim=c(0.2,3), ylim=c(-2.5,2.5), 
                    xaxt=xaxt, xlab= xlab, ylab= ylab, main=title))")))
  xvals<-log10(15*2^(-8:8))
  xlabs<-(15*2^(-8:8))
  axis(1, at=xvals,labels=xlabs,las=1)
  abline(h=0,col=8)
  
  if (length(cuts)==1) { # by ncuts quantiles
    # set quantiles
    #eval(parse(text=paste0("quantiles <- quantile(refdata",sex,"$",covariate,", prob = seq(0, 1, length = cuts+1))")))
    # add quantile info to the data
    eval(parse(text=paste0("refdata",sex,"$",covariate,"cuts <- cut2(refdata",sex,"$",covariate,", g = cuts)")))
  } else { # by directly specifying cutpoints
    eval(parse(text=paste0("refdata",sex,"$",covariate,"cuts <- cut2(refdata",sex,"$",covariate,", cuts = cuts)")))
  }
  eval(parse(text=paste0("dist<-table(refdata",sex,"$",covariate,"cuts)")))
  print(dist)
  ncuts<-length(dist)
  
  xa<-c()
  for (i in 1:length(dist))  {
    # fitdata<-refdata[refdata$agecuts==levels(refdata$agecuts)[i],]
    eval(parse(text=paste0("fitdata<<-refdata",sex,"[refdata",sex,"$",covariate,"cuts==levels(refdata", sex,"$",covariate,"cuts)[i],]")))
    
    xa<-rbind(xa,plotsol(col=i+1))
    with(fitdata,lines(lFer, rollmean(dRefHb, rollmean_nr, fill = list(NA, NULL, NA)),  col = i+1,  lwd = 2))
  }
  paste0(levels(fitdata$agecuts), " n=" ,dist, "")
  # legend("topleft", levels(fitdata$agecuts), col=2:(ncuts+1),lwd=rep(2,ncuts), title="age groups")
  eval(parse(text=paste0("legend(\"topright\",cex=0.5, paste0(levels(fitdata$",covariate,"cuts), \" n=\" ,dist), col=2:(ncuts+1),lwd=rep(2,ncuts), title=\"",covariate," groups\")")))
  return(xa)
}

# function to export the results #################
exportData <- function(data){
  set.seed(1)
  if(exists("export_data")&&is.data.frame(get("export_data"))){
    export_data2<-data[sample(nrow(data), proportion*nrow(data)), ] %>% mutate(dRefHbjit = jitter(dRefHb, amount = 0.01), lFerjit = jitter(log10(Ferritin), amount = 0.01)) %>% select(dRefHbjit, lFerjit)
    if(grepl("Male", title)){
      export_data2$sex <- "M"
    } else if (grepl("Female", title)){
      export_data2$sex <- "F"
    } else if (grepl("Premenopausal", title)){
      export_data2$sex <- "preF"
    } else if (grepl("Postmenopausal", title)){
      export_data2$sex <- "postF"
    }
    export_data <<- rbind(export_data, export_data2)
    #rm(export_data2)
    }else{
    export_data <<- data[sample(nrow(data), proportion*nrow(data)), ] %>% mutate(dRefHbjit = jitter(dRefHb, amount = 0.01), lFerjit = jitter(log10(Ferritin), amount = 0.01)) %>% select(dRefHbjit, lFerjit)
    if(grepl("Male", title)){
      export_data$sex <<- "M"
    } else if (grepl("Female", title)){
      export_data$sex <<- "F"
    } else if (grepl("Premenopausal", title)){
      export_data$sex <<- "preF"
    } else if (grepl("Postmenopausal", title)){
      export_data$sex <<- "postF"
    }
    }
  rownames(export_data) <- NULL
  saveRDS(export_data, file = paste0(file.path(main_dir, export_folder,currentDate,FolderDataset), "/export_data.rds"))
}

exportRollmean <- function(data){
  if(exists("rollmean_export")&&is.data.frame(get("rollmean_export"))){
    rollmean_export2 <- data
    if(grepl("Male", title)){
      rollmean_export2$sex <- "M"
    } else if (grepl("Female", title)){
      rollmean_export2$sex <- "F"
    } else if (grepl("Premenopausal", title)){
      rollmean_export2$sex <- "preF"
    } else if (grepl("Postmenopausal", title)){
      rollmean_export2$sex <- "postF"
    }
    rollmean_export <<- rbind(rollmean_export, rollmean_export2)

  }else{
    rollmean_export <<- data
    if(grepl("Male", title)){
      rollmean_export$sex <<- "M"
    } else if (grepl("Female", title)){
      rollmean_export$sex <<- "F"
    } else if (grepl("Premenopausal", title)){
      rollmean_export$sex <<- "preF"
    } else if (grepl("Postmenopausal", title)){
      rollmean_export$sex <<- "postF"
    }
  }
  rownames(rollmean_export) <- NULL
  saveRDS(rollmean_export, file = paste0(file.path(main_dir, export_folder,currentDate,FolderDataset), "/rollmean_export.rds"))
}

exportSolution <- function(data){
  if(exists("sol_export")&&is.data.frame(get("sol_export"))){
    sol_export2 <- data
    if(grepl("Male", title)){
      sol_export2$sex <- "M"
    } else if (grepl("Female", title)){
      sol_export2$sex <- "F"
    } else if (grepl("Premenopausal", title)){
      sol_export2$sex <- "preF"
    } else if (grepl("Postmenopausal", title)){
      sol_export2$sex <- "postF"
    }
    sol_export <<- rbind(sol_export, sol_export2)
    
  }else{
    sol_export <<- data
    if(grepl("Male", title)){
      sol_export$sex <<- "M"
    } else if (grepl("Female", title)){
      sol_export$sex <<- "F"
    } else if (grepl("Premenopausal", title)){
      sol_export$sex <<- "preF"
    } else if (grepl("Postmenopausal", title)){
      sol_export$sex <<- "postF"
    }
  }
  rownames(sol_export) <- NULL
  saveRDS(sol_export, file = paste0(file.path(main_dir, export_folder,currentDate,FolderDataset), "/sol_export.rds"))
}

conform <- function(data) {
  
  # Rename columns to conform
  data <- data %>% 
    rename(EINnummer = donationID,
           KeyID = donor,
           Geslacht = gender,
           Geboortedatum = dob,
           Donatiedatum = date,
           Ferritine = Ferritin,
           Donatiesoortcode = donat_phleb,
           Lengte = height,
           Gewicht = weight)
  
  # Transform variables for conformity
  data <- data %>% 
    mutate(Hb = Hb * 0.06195) %>%  # Our Hb measurements are in g/L. This conversion to mmol/L comes from Google + calculator!
    mutate(Geslacht = as.factor(ifelse(Geslacht == "Men", "M", "F"))) %>% 
    mutate(Donatiesoortcode = ifelse(Donatiesoortcode == "K", "Volbloed", NA)) %>% 
    filter(Donatiesoortcode == "Volbloed") %>% mutate(Donatiesoortcode = as.factor(Donatiesoortcode))
  
  # Create numdon, prev_fer, and RefDonDate
  data <- data %>% 
    arrange(KeyID, as.Date(Donatiedatum, format = "%Y-%m-%d")) %>% 
    mutate(numdon = seq_along(KeyID)) %>% 
    group_by(KeyID) %>% 
    arrange(KeyID, by_group = T) %>% 
    mutate(prev_fer = lag(Ferritine, order_by = KeyID), RefDonDate = min(Donatiedatum), prev_DonDate = lag(Donatiedatum, order_by = KeyID))
  
  # Create RefHb
  ReferenceHb <- data %>%
    filter(numdon == 1 | numdon == 2) %>%
    group_by(KeyID) %>% mutate(RefHb = mean(Hb)) %>%
    ungroup() %>%
    filter(numdon == 1 & !is.na(RefHb)) %>%
    dplyr::select(c(KeyID, RefHb))
  
  data_2 <- merge(data, ReferenceHb, by = "KeyID")
  
  # Create numfer
  data_3 <- data_2 %>% filter(!is.na(Ferritine)) 
  numfer <- data_3 %>% 
    arrange(KeyID, as.Date(data_3$Donatiedatum, format = "%Y-%m-%d")) %>% 
    mutate(numfer = with(data_3, ave(rep(1, nrow(data_3)), KeyID, FUN = seq_along))) %>% 
    select(c(EINnummer, numfer))
  
  data <- merge(data_3, numfer, by = "EINnummer")
  
  # Adjust length, width
  # Create BMI, Age, LogFer, Dhb, premenopausal
  data_1 <- data%>%
    filter((Donatiesoortcode == "Nieuwe donorkeuring" | Donatiesoortcode == "Volbloed") & !is.na(Hb) & !is.na(Ferritine) & Hb > 0) %>%
    mutate(Lengte = replace(Lengte, Lengte < 130, NA), 
           Gewicht = replace(Gewicht, Gewicht > 150 | Gewicht < 51, NA)) %>%
    mutate(BMI = Gewicht / ((Lengte/100)*(Lengte/100)), 
           Leeftijd = round(as.numeric((Donatiedatum-Geboortedatum)/365.25), 0), 
           LogFer = log10(Ferritine), 
           DHb = Hb - RefHb,
           premenopausal = case_when(Geslacht == "F" & Leeftijd <=50 ~1, Geslacht == "F" & Leeftijd >50 ~0))%>%
    filter(!is.na(BMI))
  
  data <- data_1
  
  return(data)
}