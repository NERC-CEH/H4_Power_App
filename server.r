#load all libraries required
library(shiny)
library(nlme)
library(shinydashboard)
library(DT)
library(ggplot2)
library(lme4)
theme_set(theme_classic())
library(plotly)

sim_data_ind <- function(data0, SD, k, tslope.ind, var3ind){
  #individual means
  data0$int <- rnorm(dim(data0)[1],k,SD)
  
  #simulating the response
  data0$response <- rnorm(dim(data0)[1], mean = data0$int+(tslope.ind*data0$year1), 
                          sd = var3ind) 
  
  return(data0)
}

sim_data <- function(data0, nosite, SD, k, var3, tslope, var4, response){
  #site-specific intercept
  int1 <- rnorm(nosite,k,SD)
  int.df <- data.frame(int = int1, site = c(1:nosite))
  
  #including the site-specific intercept to the main data set
  data0$int <- int.df$int[match(data0$site, int.df$site)]
  # print(anyNA(int.df$int))
  
  #adding plot-specific variation for each plot
  data0$rep_v <- data0$site_and_reps
  
  levels(data0$rep_v) <- as.numeric(rnorm(length(levels(data0$rep_v)),0,var3))
  data0$int <- data0$int + as.numeric(levels(data0$rep_v))[data0$rep_v]
  
  
  #simulating the response
  if(response == "norm"){
  data0$response <- rnorm(dim(data0)[1], mean = data0$int+tslope*data0$year1,
                          sd = var4)
  } else if(response == "binom"){
    mn=(data0$int+tslope*data0$year1)
    data0$response <- rbinom(dim(data0)[1], 1, probit(mn))
  }
  data0
}



# sim_data_binom <- function(data0, nosite, SD, k, var3, tslope, var4){
#   
#   #site-specific intercept
#   int1 <- rnorm(nosite,k,SD)
#   int.df <- data.frame(int = int1, site = c(1:nosite))
#   
#   #including the site-specific intercept to the main data set
#   data0$int <- int.df$int[match(data0$site, int.df$site)]
#   
#   #adding plot-specific variation for each plot
#   data0$rep_v <- data0$site_and_reps
#   
#   levels(data0$rep_v) <- as.numeric(rnorm(length(levels(data0$rep_v)),0,var3))
#   data0$int <- data0$int + as.numeric(levels(data0$rep_v))[data0$rep_v]
#   
#   #simulating the response
#   
#   mn=(data0$int+tslope*data0$year1)
#   data0$response <- rbinom(dim(data0)[1], 1, probit(mn))
#   
#   data0
#   
# }

logit=function(x){log(x/(1-x))}
probit=function(x){exp(x)/(1+exp(x))}


## all code sits within a function of inputs and outputs 
function(input, output, session) {
  # 
  # reactive({validate(
  #   need(nchar(gsub("[0-9]|,|;|\\.| ","",input$nosite.yr)==0,
  #              "Include only numbers separated by , or ; in No. of sites per year")),
  #   need(nchar(gsub("[0-9]|,|;|\\.| ","",input$deschg.yr)==0,
  #              paste("Include only numbers separated by , or ; in Years in which", 
  #                    "change occurs"))),
  #   need(nchar(gsub("[0-9]|,|;|\\.| ","",input$noreps)==0,
  #              paste("Include only numbers separated by , or ; in No. of within",
  #                    "site replicates per year"))),
  #   need(nchar(gsub("[0-9]|,|;|\\.| ","",input$samfreq)==0,
  #              paste("Include only numbers separated by , or ; in How often sites",
  #                    "are repeated (years)")))
  # )})
  
  #Check how many multiple scenario boxes are ticked for the site based analysis. 
  check.input <- reactive({
    
    tot.true <- as.integer(c(input$mult_yr=="TRUE",input$mult_ef=="TRUE",input$mult_st=="TRUE"))
    flag <- character()
    
    flaga <- ifelse(nchar(gsub("[0-9]|,|;| ","",input$nosite.yr))==0 & 
                      nchar(input$nosite.yr)>0,"",
                    paste("Include only numbers separated by , or ; in No. of", 
                          "sites per year"))
    flagb <- ifelse(nchar(gsub("[0-9]|,|;| |-","",input$deschg.yr))==0 & 
                      nchar(input$deschg.yr)>0,"",
                    paste("Include only numbers separated by , or ; in", 
                          "Years in which change occurs"))
    flagc <- ifelse(nchar(gsub("[0-9]|,|;| ","",input$noreps))==0 & 
                      nchar(input$noreps)>0,"",
                    paste("Include only numbers separated by , or ; in", 
                          "No. of within site replicates per year"))
    flagd <- ifelse(nchar(gsub("[0-9]|,|;| ","",input$samfreq))==0 & 
                      nchar(input$samfreq)>0,"",
                    paste("Include only numbers separated by , or ; in", 
                          "How often sites are repeated (years)"))
    
    if(input$deschg){
      chg.tm <- as.numeric(unlist(strsplit(input$deschg.yr,",|;")))
      chg.num <- length(chg.tm) + 1
      
      if(input$hist & input$hist.yr>0){chg.tm = chg.tm + input$hist.yr}
      
      i.nosite.yr <- as.numeric(unlist(strsplit(input$nosite.yr,",|;")))
      smfreq <- as.numeric(unlist(strsplit(input$samfreq,",|;")))
      rept <- as.numeric(unlist(strsplit(input$noreps,",|;")))
      
      # check if any of these are of length one in which case repeat
      if(length(i.nosite.yr) == 1)
        i.nosite.yr <- rep(i.nosite.yr, chg.num)
      if(length(smfreq) == 1)
        smfreq <- rep(smfreq, chg.num)
      if(length(rept) == 1)
        rept <- rep(rept, chg.num)
      
      flag1 <- ifelse(length(i.nosite.yr) != chg.num, 
                      paste("Total entries in number of sites per year needs to",
                            "be 1 or", chg.num), "")
      flag2 <- ifelse(length(rept) != chg.num,
                      paste("Total entries in number of within site replicates needs", 
                            "to be 1 or", chg.num), "")
      flag3 <- ifelse(length(smfreq) != chg.num, 
                      paste("Total entries in how often sites are repeated needs to", 
                            "be 1 or", chg.num), "")
      flag4 <- ifelse(sum(tot.true)>1,"Only one multiple scenario allowed", "")
      flag5 <- ifelse(max(chg.tm)>(input$noyear+input$hist.yr), 
                      "Please keep years in which change occurs within No. of Years",
                      "")
      flag_list <- list(flag1,flag2,flag3,flag4,flag5,flaga,flagb,flagc,flagd)
      flag <- ifelse(all(sapply(flag_list, function(x) x == "")), "",
                     paste(flag_list[sapply(flag_list,
                                            function(x) !(x == ""))], 
                           collapse = "\n")
      )
    } else {
      flag4 <- ifelse(sum(tot.true)>1,"Only one multiple scenario allowed","")
      flag_list <- list(flag4,flaga,flagb,flagc,flagd)
      flag <- ifelse(all(sapply(flag_list, function(x) x == "")), "",
                     paste(flag_list[sapply(flag_list,
                                            function(x) !(x == ""))], 
                           collapse = "\n")
      )
    }
    
    return(flag)
    
  })
  
  #Check how many multiple scenario boxes are ticked for the data from individuals
  #analysis
  check.input.ind <- reactive({
    
    tot.true <- as.integer(c(input$noyr_ind=="TRUE",input$mult_efind=="TRUE",input$noind_ind=="TRUE"))
    
    flag <- character()
    flaga <- ifelse(nchar(gsub("[0-9]|,|;| ","",input$noind.yr))==0 & 
                      nchar(input$noind.yr)>0,"",
                    paste("Include only numbers separated by , or ; in No. of", 
                          "Individuals per year"))
    flagb <- ifelse(nchar(gsub("[0-9]|,|;| |-","",input$deschg_ind.yr))==0 & 
                      nchar(input$deschg_ind.yr)>0,"",
                    paste("Include only numbers separated by , or ; in", 
                          "Years in which change occurs"))
    flagc <- ifelse(nchar(gsub("[0-9]|,|;| ","",input$rep.ind))==0 & 
                      nchar(input$rep.ind)>0,"",
                    paste("Include only numbers separated by , or ; in Frequency", 
                          "of sampling in years"))
    
    if(isTRUE(input$deschg_ind)){
      chg.tm <- as.numeric(unlist(strsplit(input$deschg_ind.yr,",|;")))
      chg.num <- length(chg.tm) + 1
      
      if(input$histind & input$hist.yrind>0){chg.tm = chg.tm + input$hist.yrind}
      
      i.noind.yr <- as.numeric(unlist(strsplit(input$noind.yr,",|;")))
      repind <- as.numeric(unlist(strsplit(input$rep.ind,",|;")))
      
      # check if any of these are of length one in which case repeat
      if(length(i.noind.yr) == 1)
        i.noind.yr <- rep(i.noind.yr, chg.num)
      if(length(repind) == 1)
        repind <- rep(repind, chg.num)
      
      flag1 <- ifelse(length(i.noind.yr) != chg.num, 
                      paste("Total entries in number of individuals per year needs", 
                            "to be 1 or", chg.num), "")
      flag2 <- ifelse(length(repind) != chg.num,
                      paste("Total entries in number of within site replicates", 
                            "needs to be 1 or", chg.num), "")
      flag4 <- ifelse(sum(tot.true)>1,"Only one multiple scenario allowed", "")
      flag5 <- ifelse(max(chg.tm)>input$noyear.ind, 
                      "Please keep years in which change occurs within No. of Years",
                      "")
      flag_list <- list(flag1,flag2,flag4,flag5,flaga,flagb,flagc)
      flag <- ifelse(all(sapply(flag_list, function(x) x == "")), "",
                     paste(flag_list[sapply(flag_list,
                                            function(x) !(x == ""))], 
                           collapse = "\n")
      )
    } else {
      flag4 <- ifelse(sum(tot.true)>1,"Only one multiple scenario allowed","")
      flag_list <- list(flag4,flaga,flagb,flagc)
      flag <- ifelse(all(sapply(flag_list, function(x) x == "")), "",
                     paste(flag_list[sapply(flag_list,
                                            function(x) !(x == ""))], 
                           collapse = "\n")
      )
    }
    
    return(flag)
    
  })
  
  noind.yr.nm <- reactive({
    
    validate(
      need(check.input.ind() == "",
           check.input.ind())
    )
    return(as.numeric(unlist(strsplit(input$noind.yr,",|;"))))
  })
  
  #define the length of the time series by adding the historical data length to the
  #number of years
  yrdef <- reactive({ 
    if(input$mult_yr=="TRUE"){
      #if we are looking at the multiple year scenario, then the number of years is a
      #sequence.
      i.noyear <- seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5) +
        input$hist.yr
    }else{
      i.noyear <- input$noyear + input$hist.yr
    }
    if(i.noyear[1]<0){i.noyear=abs(i.noyear)}
    
    return(i.noyear) 	 
  })
  
  
  #Calculate the total number of sites using the number of sites per year, number of
  #years and the sampling frequency
  nosite.r <- reactive({	
    
    validate(
      need(check.input() == "",
           check.input())
    )
    
    i.nosite.yr <- as.numeric(unlist(strsplit(input$nosite.yr,",|;")))
    smfreq <- as.numeric(unlist(strsplit(input$samfreq,",|;")))
    i.noyear <- yrdef()[1]
    
    if(i.noyear<0){i.noyear <- abs(i.noyear)}
    
    # if(check.input()[[1]]<=1){
    
    if(input$deschg){
      nosite = max(i.nosite.yr) * max(smfreq)
    }else{	
      if(i.noyear >= input$samfreq){
        nosite = i.nosite.yr * smfreq
      }else{
        isamfreq <- i.noyear
        nosite = i.nosite.yr * isamfreq 
      } 
      
    }
    nosite
    # }
  })
  
  
  #specify the structure of the simulated data in terms of unique site and sample
  #identifiers and year
  selectData <- eventReactive(input$update,{
    validate(
      need(check.input() == "",
           check.input())
    )
    #selectData <- reactive({
    # if(check.input()[[1]]<=1){
    
    i.noyear <- yrdef()[1]
    
    chg.tm <- as.numeric(unlist(strsplit(input$deschg.yr,",|;")))
    chg.num <- length(chg.tm) + 1
    
    if(input$hist & input$hist.yr>0){chg.tm = chg.tm + input$hist.yr}
    
    i.nosite.yr <- as.numeric(unlist(strsplit(input$nosite.yr,",|;")))
    smfreq <- as.numeric(unlist(strsplit(input$samfreq,",|;")))
    rept <- as.numeric(unlist(strsplit(input$noreps,",|;")))
    
    
    if(input$deschg){
      # check if any of these are of length one in which case repeat
      if(length(i.nosite.yr) == 1)
        i.nosite.yr <- rep(i.nosite.yr, chg.num)
      if(length(smfreq) == 1)
        smfreq <- rep(smfreq, chg.num)
      if(length(rept) == 1)
        rept <- rep(rept, chg.num)
      
      tps <- diff(c(0,chg.tm,i.noyear))
      nositemx = max(i.nosite.yr) * max(smfreq)
      #establish which sites need to be removed from a full exhaustive set of every
      #replicate at every site in every year in order to conform to sampling
      #frequency
      thin.id=c()
      for(ks in 1:(length(chg.tm)+1)){
        nositep = i.nosite.yr[ks] * smfreq[ks] 
        if(ks==1){
          thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                     rep((1:tps[ks]),
                                         each=floor(nositep/smfreq[ks])),
                                     sep="_"))
        }else{
          thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                     rep(tps[(ks-1)]+(1:tps[ks]),
                                         each=floor(nositep/smfreq[ks])),
                                     sep="_"))
        }
      }
    }else{
      
      nositemx <- nosite.r()
      thin.id <- paste(rep(1:nositemx,floor(i.noyear/smfreq)),
                       rep(1:i.noyear,each=floor(nositemx/smfreq)),sep="_")
    }
    #create data frame representing exhasistive set of all reps and all sites in
    #all years
    data0 <- expand.grid(reps=1:max(rept), site=1:nositemx, year=1:i.noyear)
    
    #define a unique identifier for sites in particular years
    data0$site.yr <- paste(data0$site, data0$year, sep="_")
    
    #thin the full data set to conform to sampling frequency under investigation
    data0 <- data0[is.element(data0$site.yr,thin.id),]
    
    #convert all identifiers to factors
    data0$site <- as.factor(data0$site)
    data0$reps <- as.factor(data0$reps)
    data0$site_and_reps = interaction(data0$site,data0$reps)
    data0$year_and_reps = interaction(data0$year,data0$reps)
    
    if(length(rept)>1){
      tpid=c()
      for(rpi in 1:length(rept)){
        if(rpi==1){
          tpid=c(tpid,apply(expand.grid(1:chg.tm[rpi], 1:rept[rpi]), 1, 
                            paste, collapse="."))
        }else{
          if(rpi==length(rept)){
            tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):i.noyear,
                                          1:rept[rpi]), 1, paste, collapse="."))
          }else{
            tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):chg.tm[rpi], 
                                          1:rept[rpi]), 1, paste, collapse="."))
          }
          #}
          
        }
      }
      data0 <- data0[is.element(data0$year_and_reps,tpid),]
    }
    data0$year1 <- data0$year-1
    
    data0
    
    # }
  })
  
  #simulate data according to the design and distribution specified	
  sim.data <- reactive({
    
    # if(check.input()[[1]]<=1){
    
    prv <- paramvals()
    
    i.noyear <- yrdef()[1]
    data0 <- selectData()
    
    nosite <- nosite.r()
    
    # if(input$data_distr=='binom'){
    #   sim_data_binom(data0, nosite, prv$var1, prv$var2, prv$var3, input$tslope, prv$var4)
    # }else{
      sim_data(data0, nosite, prv$var1, prv$var2, prv$var3, input$tslope, prv$var4,
               input$data_distr)
    # }
    
    # }
  })
  
  
  #simulate data according to the design and distribution specified for the data from
  #individuals
  simdata.ind <- eventReactive(input$updateind,{	
    
    
    prv <- paramvals.ind()
    
    i.noyear.ind <- input$noyear.ind + input$hist.yrind
    
    repind <- as.numeric(unlist(strsplit(input$rep.ind,",|;")))
    
    noind_yr <- noind.yr.nm()
    noindmx <- max(noind_yr)
    
    if(input$deschg_ind){
      chg.tmind <- as.numeric(unlist(strsplit(input$deschg_ind.yr,",|;")))
      chg.num <- (length(chg.tmind)+1)
      
      if(input$histind & input$hist.yrind>0){
        chg.tmind = chg.tmind + input$hist.yrind
      }
      
      tps <- diff(c(0,chg.tmind,i.noyear.ind))
      
      if(length(repind)==1){
        repind <- rep(repind, chg.num)
      }
      if(length(noind_yr)==1){
        noind_yr <- rep(noind_yr, chg.num)
      }
      
      #establish which sites need to be removed from a full exhaustive set of every
      #replicate at every site in every year in order to conform to sampling
      #frequency
      thin.id=c()
      for(ks in 1:chg.num){
        nositep = noind_yr[ks] 
        if(ks==1){
          
          thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                     rep(seq(1,tps[ks],by=repind[ks]),each=nositep),
                                     sep="_"))
        }else{
          
          thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                     rep(seq((tps[(ks-1)]+1),tps[(ks-1)]+(tps[ks]),
                                             by=repind[ks]),each=nositep),
                                     sep="_"))
        }
      }
    }else{
      
      thin.id <- paste(rep(1:noindmx,ceiling(i.noyear.ind/min(repind))),
                       rep(seq(1,i.noyear.ind,by=min(repind)),each=noindmx),sep="_")
    }
    
    
    data0 <- expand.grid(ind=1:max(noind.yr.nm()), 
                         year=seq(1,i.noyear.ind,by=min(repind)))	
    
    data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
    data0 <- data0[is.element(data0$ind.yr,thin.id),]
    
    data0$ind <- as.factor(data0$ind)
    
    data0$year1 <- data0$year-1	
    
    
    data0 <- sim_data_ind(data0, prv$var1, prv$var2, 
                          input$tslope.ind, prv$var3)		
    
    return(data0)
    
  })	
  
  ###################### #
  # Run site analysis ####
  ###################### #
  
  #run the power analysis for site based data by simulating multiple data sets under
  #the individual scenerio specified
  run.scen <- eventReactive(input$update,{
    
    prv <- paramvals()
    
    pval <- replicate(input$nsims, {
      
      ### simulate data #####
      
      data0 <- selectData()
      
      nosite <- nosite.r()
      
      if(input$data_distr=='binom'){

        data0 <- sim_data(data0, nosite = nosite, SD = prv$var1, k = prv$var2,
                          var3 = prv$var3, tslope = input$tslope, var4 = prv$var4,
                          response = input$data_distr)

        #### model data ####
        mod.boot <- glmer(response~year + (1|site/reps),family=binomial,
                          data=data0,na.action=na.omit)
        #store the p value corresponding to the estimated trend
        summary(mod.boot)$coefficients[2,4]


      }else{

        data0 <- sim_data(data0, nosite = nosite, SD = prv$var1, k = prv$var2, 
                          var3 = prv$var3, tslope = input$tslope, var4 = prv$var4,
                          response = input$data_distr)
        
        #### model data ####	 
        mod.boot <- lme(response~year,random=~1|site/reps,
                        data=data0,na.action=na.omit)
        #store the p value corresponding to the estimated trend
        summary(mod.boot)$tTable[2,5]
        
      }
    })
    
    return(pval)
    
    
  })
  
  #run the power analysis for site based data by simulating multiple data sets under
  #the multiple year scenerio specified
  run.mult.scen <- eventReactive(input$update,{
    
    prv <- paramvals()
    
    mult.pval=matrix(ncol=input$nsims,nrow=5)
    
    if(input$hist & input$hist.yr>0){
      yr.rng <- round(seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5)) +
        input$hist.yr
    }else{
      yr.rng <- round(seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5)) 
    }
    
    if(yr.rng[1]<0){yr.rng=abs(yr.rng)}
    
    chg.tm <- as.numeric(unlist(strsplit(input$deschg.yr,",|;")))
    
    if(input$hist & input$hist.yr>0){chg.tm = chg.tm + input$hist.yr}
    
    i.nosite.yr <- as.numeric(unlist(strsplit(input$nosite.yr,",|;")))
    smfreq <- as.numeric(unlist(strsplit(input$samfreq,",|;")))
    rept <- as.numeric(unlist(strsplit(input$noreps,",|;")))
    
    for(ik in 1:length(yr.rng)){
      
      ### simulate data #####
      
      i.noyear <- yr.rng[ik]
      
      if(input$deschg){
        chg.num <- length(chg.tm)+1
        if(length(i.nosite.yr) == 1)
          i.nosite.yr <- rep(i.nosite.yr, chg.num)
        if(length(smfreq) == 1)
          smfreq <- rep(smfreq, chg.num)
        if(length(rept) == 1)
          rept <- rep(rept, chg.num)
        
        tps <- diff(c(0,chg.tm,i.noyear))
        nositemx = max(i.nosite.yr) * max(smfreq)
        
        #establish which sites need to be removed from a full exhaustive set of
        #every replicate at every site in every year in order to conform to
        #sampling frequency
        thin.id=c()
        for(ks in 1:(length(chg.tm)+1)){
          nositep = i.nosite.yr[ks] * smfreq[ks] 
          if(ks==1){
            thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                       rep((1:tps[ks]),
                                           each=floor(nositep/smfreq[ks])),
                                       sep="_"))
          }else{
            thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                       rep(tps[(ks-1)]+(1:tps[ks]),
                                           each=floor(nositep/smfreq[ks])),
                                       sep="_"))
          }
        }
        
      }else{
        
        nositemx <- nosite.r()
        thin.id <- paste(rep(1:nositemx,floor(i.noyear/smfreq)),
                         rep(1:i.noyear,each=floor(nositemx/smfreq)),sep="_")
      }
      
      #create data frame representing exhasistive set of all reps and all sites in
      #all years
      data0 <- expand.grid(reps=1:max(rept), site=1:nositemx, year=1:i.noyear)
      
      #define a unique identifier for sites in particular years
      data0$site.yr <- paste(data0$site, data0$year, sep="_")
      
      #thin the full data set to conform to sampling frequency under investigation
      data0 <- data0[is.element(data0$site.yr,thin.id),]
      
      #convert all identifiers to factors
      data0$site <- as.factor(data0$site)
      data0$reps <- as.factor(data0$reps)
      data0$site_and_reps = interaction(data0$site,data0$reps)
      data0$year_and_reps = interaction(data0$year,data0$reps)
      
      if(length(rept)>1){
        tpid=c()
        for(rpi in 1:length(rept)){
          if(rpi==1){
            tpid=c(tpid,apply(expand.grid(1:chg.tm[rpi], 1:rept[rpi]), 1, paste, 
                              collapse="."))
          }else{
            if(rpi==length(rept)){
              tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):i.noyear, 
                                            1:rept[rpi]), 1, paste, collapse="."))
            }else{
              tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):chg.tm[rpi],
                                            1:rept[rpi]), 1, paste, collapse="."))
            }
            #}
            
          }
        }
        data0 <- data0[is.element(data0$year_and_reps,tpid),]
      }
      
      data0$year1 <- data0$year-1
      
      nosite <- nosite.r()
      
      mult.pval[ik,] <- replicate(input$nsims,{
        
        data0 <- sim_data(data0, nosite, prv$var1, prv$var2, prv$var3, 
                          input$tslope, prv$var4, response = input$data_distr)
        
        if(input$data_distr == "norm"){
        #### model data ####
        mod.boot <- lme(response~year,random=~1|site/reps,
                        data=data0,na.action=na.omit)
        #store the p value corresponding to the estimated trend
        summary(mod.boot)$tTable[2,5]
        } else if(input$data_distr == "binom"){
          #### model data ####
          mod.boot <- glmer(response~year + (1|site/reps),family=binomial,
                            data=data0,na.action=na.omit)
          #store the p value corresponding to the estimated trend
          summary(mod.boot)$coefficients[2,4]
        } else stop("run.mult.scen family error")
      })
      
    }
    
    return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
    
    
  })
  
  
  
  
  
  
  #run the power analysis for site based data by simulating multiple data sets under
  #the multiple no. of sites scenerio specified
  run.mult.st.scen <- eventReactive(input$update,{
    
    prv <- paramvals()
    
    #define matrix to store p values in
    mult.pval=matrix(ncol=input$nsims,nrow=5)
    
    #define a sequence covering the range of sites of interest
    st.rng <- round(seq(input$nosite.st.rng[1],input$nosite.st.rng[2],len=5))
    
    chg.tm <- as.numeric(unlist(strsplit(input$deschg.yr,",|;")))
    
    if(input$hist & input$hist.yr>0){chg.tm = chg.tm + input$hist.yr}
    
    i.noyear <- input$noyear  + input$hist.yr
    
    smfreq <- as.numeric(unlist(strsplit(input$samfreq,",|;")))
    rept <- as.numeric(unlist(strsplit(input$noreps,",|;")))
    
    for(ij in 1:length(st.rng)){
      
      ### simulate data #####
      
      i.nosite.yr <- rep(st.rng[ij],(length(chg.tm)+1))
      
      if(input$deschg){
        chg.num <- length(chg.tm)+1
        if(length(i.nosite.yr) == 1)
          i.nosite.yr <- rep(i.nosite.yr, chg.num)
        if(length(smfreq) == 1)
          smfreq <- rep(smfreq, chg.num)
        if(length(rept) == 1)
          rept <- rep(rept, chg.num)
        
        tps <- diff(c(0,chg.tm,i.noyear))
        nositemx = max(i.nosite.yr) * max(smfreq)
        
        #establish which sites need to be removed from a full exhaustive set of
        #every replicate at every site in every year in order to conform to
        #sampling frequency
        thin.id=c()
        for(ks in 1:(length(chg.tm)+1)){
          nositep = i.nosite.yr[ks] * smfreq[ks] 
          if(ks==1){
            thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                       rep((1:tps[ks]),
                                           each=floor(nositep/smfreq[ks])),
                                       sep="_"))
          }else{
            thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                       rep(tps[(ks-1)]+(1:tps[ks]),
                                           each=floor(nositep/smfreq[ks])),
                                       sep="_"))
          }
        }
        
      }else{
        
        nositemx = max(i.nosite.yr) * max(smfreq)
        thin.id <- paste(rep(1:nositemx,floor(i.noyear/smfreq)),
                         rep(1:i.noyear,each=floor(nositemx/smfreq)),sep="_")
      }
      
      #create data frame representing exhasistive set of all reps and all sites in
      #all years
      data0 <- expand.grid(reps=1:max(rept), site=1:nositemx, year=1:i.noyear)
      
      #define a unique identifier for sites in particular years
      data0$site.yr <- paste(data0$site, data0$year, sep="_")
      
      #thin the full data set to conform to sampling frequency under investigation
      data0 <- data0[is.element(data0$site.yr,thin.id),]
      
      #convert all identifiers to factors
      data0$site <- as.factor(data0$site)
      data0$reps <- as.factor(data0$reps)
      data0$site_and_reps = interaction(data0$site,data0$reps)
      data0$year_and_reps = interaction(data0$year,data0$reps)
      
      if(length(rept)>1){
        tpid=c()
        for(rpi in 1:length(rept)){
          if(rpi==1){
            tpid=c(tpid,apply(expand.grid(1:chg.tm[rpi], 1:rept[rpi]), 1,
                              paste, collapse="."))
          }else{
            if(rpi==length(rept)){
              tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):i.noyear, 
                                            1:rept[rpi]), 1, paste, collapse="."))
            }else{
              tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):chg.tm[rpi], 
                                            1:rept[rpi]), 1, paste, collapse="."))
            }
            #}
            
          }
        }
        data0 <- data0[is.element(data0$year_and_reps,tpid),]
      }
      
      
      data0$year1 <- data0$year-1
      
      # nosite <- nosite.r()
      
      mult.pval[ij,] <- replicate(input$nsims,{
        
        data0 <- sim_data(data0, nositemx, prv$var1, prv$var2, prv$var3, 
                          input$tslope, prv$var4, input$data_distr)
        
        if(input$data_distr == "norm"){
          #### model data ####
          mod.boot <- lme(response~year,random=~1|site/reps,
                          data=data0,na.action=na.omit)
          #store the p value corresponding to the estimated trend
          summary(mod.boot)$tTable[2,5]
        } else if(input$data_distr == "binom"){
          #### model data ####
          mod.boot <- glmer(response~year + (1|site/reps),family=binomial,
                            data=data0,na.action=na.omit)
          #store the p value corresponding to the estimated trend
          summary(mod.boot)$coefficients[2,4]
        } else stop("run.mult.st.scen family error")
        
      }) 
      
    }
    
    return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
    
    
  })
  
  #run the power analysis for site based data by simulating multiple data sets under
  #the multiple effects scenerios specified
  run.mult.eff.scen <- eventReactive(input$update,{
    
    prv <- paramvals()
    
    #define matrix to store p values in
    mult.pval=matrix(ncol=input$nsims,nrow=5)
    
    #define a sequence covering the range of sites of interest
    ef.rng <- (seq(input$nosite.ef.rng[1],input$nosite.ef.rng[2],len=5))
    
    chg.tm <- as.numeric(unlist(strsplit(input$deschg.yr,",|;")))
    
    if(input$hist & input$hist.yr>0){chg.tm = chg.tm + input$hist.yr}
    
    i.noyear <- input$noyear  + input$hist.yr
    
    i.nosite.yr <- as.numeric(unlist(strsplit(input$nosite.yr,",|;")))
    smfreq <- as.numeric(unlist(strsplit(input$samfreq,",|;")))
    rept <- as.numeric(unlist(strsplit(input$noreps,",|;")))
    
    ### simulate data #####
    
    if(input$deschg){
      chg.num <- length(chg.tm)+1
      if(length(i.nosite.yr) == 1)
        i.nosite.yr <- rep(i.nosite.yr, chg.num)
      if(length(smfreq) == 1)
        smfreq <- rep(smfreq, chg.num)
      if(length(rept) == 1)
        rept <- rep(rept, chg.num)
      
      tps <- diff(c(0,chg.tm,i.noyear))
      nositemx = max(i.nosite.yr) * max(smfreq)
      
      #establish which sites need to be removed from a full exhaustive set of every
      #replicate at every site in every year in order to conform to sampling
      #frequency
      thin.id=c()
      for(ks in 1:(length(chg.tm)+1)){
        nositep = i.nosite.yr[ks] * smfreq[ks] 
        if(ks==1){
          thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                     rep((1:tps[ks]),
                                         each=floor(nositep/smfreq[ks])),
                                     sep="_"))
        }else{
          thin.id <- c(thin.id,paste(rep(1:nositep,floor(tps[ks]/smfreq[ks])),
                                     rep(tps[(ks-1)]+(1:tps[ks]),
                                         each=floor(nositep/smfreq[ks])),sep="_"))
        }
      }
      
    }else{
      
      nositemx <- nosite.r()
      #nosite <- nosite.r()
      thin.id <- paste(rep(1:nositemx,floor(i.noyear/smfreq)),
                       rep(1:i.noyear,each=floor(nositemx/smfreq)),sep="_")
    }
    
    #create data frame representing exhasistive set of all reps and all sites in
    #all years
    data0 <- expand.grid(reps=1:max(rept), site=1:nositemx, year=1:i.noyear)
    
    #define a unique identifier for sites in particular years
    data0$site.yr <- paste(data0$site, data0$year, sep="_")
    
    #thin the full data set to conform to sampling frequency under investigation
    data0 <- data0[is.element(data0$site.yr,thin.id),]
    
    #convert all identifiers to factors
    data0$site <- as.factor(data0$site)
    data0$reps <- as.factor(data0$reps)
    data0$site_and_reps = interaction(data0$site,data0$reps)
    data0$year_and_reps = interaction(data0$year,data0$reps)
    
    if(length(rept)>1){
      
      tpid=c()
      for(rpi in 1:length(rept)){
        if(rpi==1){
          tpid=c(tpid,apply(expand.grid(1:chg.tm[rpi], 1:rept[rpi]), 1, 
                            paste, collapse="."))
        }else{
          if(rpi==length(rept)){
            tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):i.noyear,
                                          1:rept[rpi]), 1, paste, collapse="."))
          }else{
            tpid=c(tpid,apply(expand.grid((1+chg.tm[rpi-1]):chg.tm[rpi],
                                          1:rept[rpi]), 1, paste, collapse="."))
          }
          #}
          
        }
      }
      data0 <- data0[is.element(data0$year_and_reps,tpid),]
    }
    
    data0$year1 <- data0$year-1
    
    nosite <- nosite.r()
    
    
    for(ik in 1:length(ef.rng)){
      
      mult.pval[ik,] <- replicate(input$nsims, {
        
        data0 <- sim_data(data0, nosite, prv$var1, prv$var2, prv$var3, 
                          ef.rng[ik], prv$var4, input$data_distr)
        if(input$data_distr == "norm"){
          #### model data ####
          mod.boot <- lme(response~year,random=~1|site/reps,
                          data=data0,na.action=na.omit)
          #store the p value corresponding to the estimated trend
          summary(mod.boot)$tTable[2,5]
        } else if(input$data_distr == "binom"){
          #### model data ####
          mod.boot <- glmer(response~year + (1|site/reps),family=binomial,
                            data=data0,na.action=na.omit)
          #store the p value corresponding to the estimated trend
          summary(mod.boot)$coefficients[2,4]
        } else stop("run.mult.eff.scen family error")
        
      })
      
    }
    
    return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
    
  })
  
  ##################### #
  # Run ind analysis ####
  ##################### #
  
  #run the power analysis for data from individuals by simulating multiple data sets
  #under the scenerio specified
  run.ind.scen <- eventReactive(input$updateind,{	
    
    prv <- paramvals.ind()
    
    i.noyear.ind <- input$noyear.ind + input$hist.yrind
    
    
    repind <- as.numeric(unlist(strsplit(input$rep.ind,",|;")))
    
    noind_yr <- noind.yr.nm() 
    noindmx <- max(noind_yr)
    
    if(input$deschg_ind){
      chg.tmind <- as.numeric(unlist(strsplit(input$deschg_ind.yr,",|;")))
      chg.num <- length(chg.tmind)+1
      
      if(input$histind & input$hist.yrind>0){
        chg.tmind = chg.tmind + input$hist.yrind
      }
      tps <- diff(c(0,chg.tmind,i.noyear.ind))
      
      if(length(repind)==1){
        repind <- rep(repind, chg.num)
      }
      
      if(length(noind_yr)==1){
        noind_yr <- rep(noind_yr, chg.num)
      }
      
      #establish which sites need to be removed from a full exhaustive set of every
      #replicate at every site in every year in order to conform to sampling
      #frequency
      thin.id=c()
      for(ks in 1:chg.num){
        nositep = noind_yr[ks] 
        if(ks==1){
          
          thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                     rep(seq(1,tps[ks],by=repind[ks]),each=nositep),
                                     sep="_"))
        }else{
          thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                     rep(seq((tps[(ks-1)]+1),tps[(ks-1)]+(tps[ks]),
                                             by=repind[ks]),each=nositep),sep="_"))
        }
      }
    }else{
      
      thin.id <- paste(rep(1:noindmx,ceiling(i.noyear.ind/min(repind))),
                       rep(seq(1,i.noyear.ind,by=min(repind)),each=noindmx),sep="_")
    }
    
    
    data0 <- expand.grid(ind=1:max(noind.yr.nm()), 
                         year=seq(1,i.noyear.ind,by=min(repind)))	
    
    data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
    data0 <- data0[is.element(data0$ind.yr,thin.id),]
    
    data0$ind <- as.factor(data0$ind)
    
    data0$year1 <- data0$year-1	
    
    pvali <- replicate(input$nsimi,{
      
      
      data0 <- sim_data_ind(data0, prv$var1, prv$var2, 
                            input$tslope.ind, prv$var3)
      
      #fit model to simulated data
      mod.bt.i <- lm(response~year,data=data0)
      
      #store the p value corresponding to the estimated trend
      summary(mod.bt.i)$coefficients[2,4]
      
    })
    
    return(100*(length(pvali[pvali<0.05])/length(pvali)))
    
    
  })	
  
  
  #run the power analysis for data from individuals by simulating multiple data sets
  #under the multiple effects scenerios specified
  run.ind.multef.scen <- eventReactive(input$updateind,{	
    
    # if(check.input.ind()[[1]]>0){
    
    mult.pval=matrix(ncol=input$nsimi,nrow=5)
    
    ef.rngi <- (seq(input$ef.rng.ind[1],input$ef.rng.ind[2],len=5))
    
    prv <- paramvals.ind()
    
    i.noyear.ind <- input$noyear.ind + input$hist.yrind
    
    #totno.ind=noind.yr.nm()*(i.noyear.ind/input$rep.ind)
    
    repind <- as.numeric(unlist(strsplit(input$rep.ind,",|;")))
    
    noind_yr <- noind.yr.nm() 
    noindmx <- max(noind_yr)
    
    if(input$deschg_ind){
      chg.tmind <- as.numeric(unlist(strsplit(input$deschg_ind.yr,",|;")))
      chg.num <- length(chg.tmind)+1
      
      if(input$histind & input$hist.yrind>0){
        chg.tmind = chg.tmind + input$hist.yrind
      }
      
      tps <- diff(c(0,chg.tmind,i.noyear.ind))
      
      if(length(repind)==1){
        repind <- rep(repind, chg.num)
      }
      
      if(length(noind_yr)==1){
        noind_yr <- rep(noind_yr, chg.num)
      }
      
      #establish which sites need to be removed from a full exhaustive set of every
      #replicate at every site in every year in order to conform to sampling
      #frequency
      thin.id=c()
      for(ks in 1:chg.num){
        nositep = noind_yr[ks] 
        if(ks==1){
          
          thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                     rep(seq(1,tps[ks],by=repind[ks]),each=nositep),
                                     sep="_"))
        }else{
          
          thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                     rep(seq((tps[(ks-1)]+1),tps[(ks-1)]+(tps[ks]),
                                             by=repind[ks]),each=nositep),
                                     sep="_"))
        }
      }
    }else{
      
      thin.id <- paste(rep(1:noindmx,ceiling(i.noyear.ind/min(repind))),
                       rep(seq(1,i.noyear.ind,by=min(repind)),each=noindmx),
                       sep="_")
    }
    
    
    data0 <- expand.grid(ind=1:max(noind.yr.nm()), 
                         year=seq(1,i.noyear.ind,by=min(repind)))	
    
    data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
    data0 <- data0[is.element(data0$ind.yr,thin.id),]
    
    data0$ind <- as.factor(data0$ind)
    
    data0$year1 <- data0$year-1	
    
    
    for(ijk in 1:length(ef.rngi)){
      
      mult.pval[ijk,] <- replicate(input$nsimi, {
        
        data0 <- sim_data_ind(data0, prv$var1, prv$var2, 
                              ef.rngi[ijk], prv$var3)
        
        #fit model to simulated data
        mod.bt.i <- lm(response~year,data=data0)
        
        #store the p value corresponding to the estimated trend
        summary(mod.bt.i)$coefficients[2,4]
        
      })
    }
    
    return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
    # }
  })	
  
  #run the power analysis for data from individuals by simulating multiple data sets
  #under the multiple no. of individuals per year scenerios specified
  run.ind.multind.scen <- eventReactive(input$updateind,{	
    
    # if(check.input.ind()[[1]]>0){
    
    mult.pval=matrix(ncol=input$nsimi,nrow=5)
    i.noyear.ind <- input$noyear.ind + input$hist.yrind		
    ind.rngi <- round((seq(input$ind.rng.ind[1],input$ind.rng.ind[2],len=5)))
    prv <- paramvals.ind()
    repind <- as.numeric(unlist(strsplit(input$rep.ind,",|;")))
    
    for(ijk in 1:length(ind.rngi)){
      
      
      noindmx <- ind.rngi[ijk]
      
      if(input$deschg_ind){
        
        chg.tmind <- as.numeric(unlist(strsplit(input$deschg_ind.yr,",|;")))
        chg.num <- length(chg.tmind)+1
        noind.yr.i = rep(ind.rngi[ijk],chg.num)
        if(length(repind)==1){
          repind <- rep(repind, chg.num)
        }
        if(input$histind & input$hist.yrind>0){
          chg.tmind = chg.tmind + input$hist.yrind
        }
        
        tps <- diff(c(0,chg.tmind,i.noyear.ind))
        #establish which sites need to be removed from a full exhaustive set of
        #every replicate at every site in every year in order to conform to
        #sampling frequency
        
        thin.id=c()
        for(ks in 1:(length(chg.tmind)+1)){
          nositep = noind.yr.i[ks] 
          if(ks==1){
            
            thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                       rep(seq(1,tps[ks],by=repind[ks]),each=nositep),
                                       sep="_"))
          }else{
            
            thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                       rep(seq((tps[(ks-1)]+1),tps[(ks-1)]+(tps[ks]),
                                               by=repind[ks]),each=nositep),
                                       sep="_"))
          }
        }
        
      }else{
        
        thin.id <- paste(rep(1:noindmx,ceiling(i.noyear.ind/min(repind))),
                         rep(seq(1,i.noyear.ind,by=min(repind)),each=noindmx),sep="_")
      }
      
      
      data0 <- expand.grid(ind=1:noindmx, year=seq(1,i.noyear.ind,by=min(repind)))	
      
      data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
      data0 <- data0[is.element(data0$ind.yr,thin.id),]
      
      data0$ind <- as.factor(data0$ind)
      
      data0$year1 <- data0$year-1	
      
      
      mult.pval[ijk,] <- replicate(input$nsimi, {
        
        data0 <- sim_data_ind(data0, prv$var1, prv$var2, 
                              input$tslope.ind, prv$var3)
        
        #fit model to simulated data
        mod.bt.i <- lm(response~year,data=data0)
        
        #store the p value corresponding to the estimated trend
        summary(mod.bt.i)$coefficients[2,4]
        
      })
    }
    
    return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
    # }
  })	
  
  
  #run the power analysis for data from individuals by simulating multiple data sets
  #under the multiple no. of years scenarios specified
  run.ind.multyr.scen <- eventReactive(input$updateind,{	
    # if(check.input.ind()[[1]]>0){
    
    mult.pval=matrix(ncol=input$nsimi,nrow=5)
    
    yr.rngi <- round((seq(input$yr.rng.ind[1],input$yr.rng.ind[2],len=5))) + input$hist.yrind
    
    prv <- paramvals.ind()
    repind <- as.numeric(unlist(strsplit(input$rep.ind,",|;")))
    
    noind_yr <- noind.yr.nm()
    noindmx <- max(noind_yr)
    
    for(ijk in 1:length(yr.rngi)){
      
      i.noyear.ind <- yr.rngi[ijk]
      
      
      if(input$deschg_ind){
        chg.tmind <- as.numeric(unlist(strsplit(input$deschg_ind.yr,",|;")))
        if(input$histind & input$hist.yrind>0){
          chg.tmind = chg.tmind + input$hist.yrind
        }
        
        tps <- diff(c(0,chg.tmind,i.noyear.ind))
        
        if(length(repind)==1){
          repind <- rep(repind, chg.num)
        }
        if(length(noind_yr)==1){
          noind_yr <- rep(noind_yr, chg.num)
        }
        
        #establish which sites need to be removed from a full exhaustive set of
        #every replicate at every site in every year in order to conform to
        #sampling frequency
        thin.id=c()
        for(ks in 1:(length(chg.tmind)+1)){
          nositep = noind_yr[ks] 
          if(ks==1){
            
            thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                       rep(seq(1,tps[ks],by=repind[ks]),
                                           each=nositep),
                                       sep="_"))
          }else{
            
            thin.id <- c(thin.id,paste(rep(1:nositep,ceiling(tps[ks]/repind[ks])),
                                       rep(seq((tps[(ks-1)]+1),
                                               tps[(ks-1)]+(tps[ks]),
                                               by=repind[ks]),each=nositep),
                                       sep="_"))
          }
        }
      }else{
        
        thin.id <- paste(rep(1:noindmx,ceiling(i.noyear.ind/min(repind))),
                         rep(seq(1,i.noyear.ind,by=min(repind)),each=noindmx),
                         sep="_")
      }
      
      
      data0 <- expand.grid(ind=1:max(noind.yr.nm()), 
                           year=seq(1,i.noyear.ind,by=min(repind)))	
      
      data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
      data0 <- data0[is.element(data0$ind.yr,thin.id),]
      
      data0$ind <- as.factor(data0$ind)
      
      data0$year1 <- data0$year-1	
      
      
      mult.pval[ijk,] <- replicate(input$nsimi,{
        
        data0 <- sim_data_ind(data0, prv$var1, prv$var2, 
                              input$tslope.ind, prv$var3)
        
        #fit model to simulated data
        mod.bt.i <- lm(response~year,data=data0)
        
        #store the p value corresponding to the estimated trend
        summary(mod.bt.i)$coefficients[2,4]
        
      })
    }
    
    return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
    # }
  })		
  
  
  ##################### #
  ##################### #
  
  test.yr <- reactive({
    if(check.input()==""){		
      yr.rng <- seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5)
      return(yr.rng)
    }
  })
  
  
  ef.rng <- reactive({seq(input$nosite.ef.rng[1],input$nosite.ef.rng[2],len=5)})		
  yr.rng <- reactive({seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5) + 
      input$hist.yr})		
  st.rng <- reactive({seq(input$nosite.st.rng[1],input$nosite.st.rng[2],len=5)})	
  ef.rngi <- reactive({seq(input$ef.rng.ind[1],input$ef.rng.ind[2],len=5)})		
  yr.rngi <- reactive({seq(input$yr.rng.ind[1],input$yr.rng.ind[2],len=5)})		
  ind.rngi <- reactive({seq(input$ind.rng.ind[1],input$ind.rng.ind[2],len=5)})		
  
  
  
  ##################################### #
  ###
  ###  Produce plots                #####
  ###
  ##################################### #
  
  
  #boxplot of simulated data under the site based scenarios
  output$plot1 <- renderPlotly({
    if(input$update > 0) {
      if(input$data_distr == "norm"){
      if(input$hist & input$hist.yr>0){
        ggplotly(ggplot(sim.data(), aes(x = year, y = response)) +
                   stat_summary_bin(aes(colour = site), fun.data = mean_se) +
                   stat_summary(aes(colour = site), fun = mean, geom= "line") +
                   stat_summary_bin(fun.data = mean_se, size = 2) +
                   stat_summary(fun = mean, geom= "line") +
                   scale_x_continuous(labels = c((-1*(input$hist.yr-1)):0,
                                                 1:input$noyear),
                                      breaks = 1:(input$hist.yr + input$noyear)) +
                   labs(x = "Year", y = "log MEAS",
                        title = "Example simulated data") +
                   NULL)
      }else{
        ggplotly(ggplot(sim.data(), aes(x = year, y = response)) +
                   stat_summary_bin(aes(colour = site), fun.data = mean_se) +
                   stat_summary(aes(colour = site), fun = mean, geom= "line") +
                   stat_summary_bin(fun.data = mean_se, size = 2) +
                   stat_summary(fun = mean, geom= "line") +
                   labs(x = "Year", y = "log MEAS", 
                        title = "Example simulated data"))
      }
      } else if(input$data_distr == "binom"){
        if(input$hist & input$hist.yr>0){
          ggplotly(ggplot(sim.data(), aes(x = year, y = response)) +
                     # geom_jitter(aes(colour = site), width = 0.1, height = 0.1) +
                     geom_point(aes(colour = site, group = site), 
                                position = position_jitterdodge(dodge.width = 0.2,
                                                                jitter.height = 0.1,
                                                                jitter.width = 0)) +
                     # stat_summary_bin(aes(colour = site), fun.data = mean_se) +
                     stat_summary(aes(colour = site), fun = mean, geom= "line") +
                     stat_summary_bin(fun.data = mean_se, size = 2) +
                     stat_summary(fun = mean, geom= "line") +
                     scale_x_continuous(labels = c((-1*(input$hist.yr-1)):0,
                                                   1:input$noyear),
                                        breaks = 1:(input$hist.yr + input$noyear)) +
                     labs(x = "Year", y = "log MEAS",
                          title = "Example simulated data") +
                     NULL)
        }else{
          ggplotly(ggplot(sim.data(), aes(x = year, y = response)) +
                     # geom_jitter(aes(colour = site), width = 0.1, height = 0.1) +
                     geom_point(aes(colour = site, group = site), 
                                position = position_jitterdodge(dodge.width = 0.2,
                                                                jitter.height = 0.1,
                                                                jitter.width = 0)) +
                     # stat_summary_bin(aes(colour = site), fun.data = mean_se) +
                     stat_summary(aes(colour = site), fun = mean, geom= "line") +
                     stat_summary_bin(fun.data = mean_se, size = 2) +
                     stat_summary(fun = mean, geom= "line") +
                     labs(x = "Year", y = "log MEAS", 
                          title = "Example simulated data"))
        }
      }
    }
  })
  
  #plot of multiple year scenarios for site based data 
  output$plot2 <- renderPlot({
    if(input$update > 0) {
      plot(yr.rng(),run.mult.scen(),pch=15,cex=1.75,ylab="Power",
           xlab="Number of Years",ylim=c(0,100))
      lines(yr.rng(),run.mult.scen(),col="grey")
      abline(h=80,col="red")
      
    }
  })
  
  #plot of multiple effects scenarios for site based data
  output$plot3 <- renderPlot({
    if(input$update > 0){
      plot(ef.rng(),run.mult.eff.scen(),pch=15,cex=1.75,ylab="Power",
           xlab="Effect Size (Coefficient of Year)",ylim=c(0,100))
      lines(ef.rng(),run.mult.eff.scen(),col="grey")
      abline(h=80,col="red")
    }
  })
  
  #plot of multiple site number scenarios for site based data
  output$plot4 <- renderPlot({
    if(input$update > 0) {
      plot(st.rng(),run.mult.st.scen(),pch=15,cex=1.75,ylab="Power",
           xlab="Number of Sites per Year",ylim=c(0,100))
      lines(st.rng(),run.mult.st.scen(),col="grey")
      abline(h=80,col="red")
    }
  })
  
  #boxplot of simulated data under the individual observation scenarios
  output$indplot <- renderPlotly({
    # if(check.input.ind()[[1]]<=1 & check.input.ind()[[2]]==""){
    if(input$histind & input$hist.yrind>0){
      ggplotly(ggplot(simdata.ind(), aes(x = year, y = response)) +
                 geom_jitter(height = 0, width = 0.1, colour = "grey") +
                 stat_summary_bin(fun.data = mean_se) +
                 stat_summary(fun = mean, geom= "line") +
                 labs(x = "Year", y = "log MEAS",
                      title = "Example simulated data") +
                 scale_x_continuous(labels = c((-1*(input$hist.yrind-1)):0,
                                               1:input$noyear.ind),
                                    breaks = 1:(input$hist.yrind + 
                                                  input$noyear.ind)) +
                 NULL)
    }else{
      ggplotly(ggplot(simdata.ind(), aes(x = year, y = response)) +
                 geom_jitter(height = 0, width = 0.1, colour = "grey") +
                 stat_summary_bin(fun.data = mean_se) +
                 stat_summary(fun = mean, geom= "line") +
                 labs(x = "Year", y = "log MEAS",
                      title = "Example simulated data") +
                 NULL)
    }
    # }else{
    #   plotly_empty(type = "scatter", mode = "markers") %>%
    #     config(
    #       displayModeBar = FALSE
    #     ) %>%
    #     layout(
    #       title = list(
    #         text = check.input.ind()[[2]],
    #         yref = "paper",
    #         y = 0.5
    #       )
    #     )
    # }
    
  })
  
  #plot of multiple effect scenarios for individual observation data 
  output$indplot.me <- renderPlot({
    # if(check.input.ind()[[1]]<=1 & check.input.ind()[[2]] == ""){
    plot(ef.rngi(),run.ind.multef.scen(),pch=15,
         xlab="Effect Size (Coefficient of Year)",ylab="Power %",ylim=c(0,100))
    lines(ef.rngi(),run.ind.multef.scen(),col="grey")
    abline(h=80,col="red")
    # }else{
    #   plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
    #   text(0,0,check.input.ind()[[2]],cex=2.8)
    # }
    
  })
  
  #plot of multiple number of individuals per year scenarios for individual observation data 
  output$indplot.mi <- renderPlot({
    # if(check.input.ind()[[1]]<=1&check.input.ind()[[2]] == ""){
    plot(ind.rngi(),run.ind.multind.scen(),pch=15,
         xlab="No. of Individuals per year",ylab="Power %",ylim=c(0,100))
    lines(ind.rngi(),run.ind.multind.scen(),col="grey")
    abline(h=80,col="red")
    # }else{
    #   plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
    #   text(0,0,check.input.ind()[[2]],cex=2.8)
    # }
    
  })
  
  #plot of multiple year scenarios for individual observation data 
  output$indplot.my <- renderPlot({
    # if(check.input.ind()[[1]]<=1 & check.input.ind()[[2]]==""){
    plot(yr.rngi(),run.ind.multyr.scen(),pch=15,
         xlab="No. of Years",ylab="Power %",ylim=c(0,100))
    lines(yr.rngi(),run.ind.multyr.scen(),col="grey")
    abline(h=80,col="red")
    # }else{
    #   plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
    #   text(0,0,check.input.ind()[[2]],cex=2.8)
    # }
    
  })
  
  
  #################### #
  #################### #
  #################### #
  
  
  output$test <- renderPrint({nosite.r()})
  output$test2 <- renderPrint({simdata.ind()})
  
  #output$test.ind <- renderPrint({run.ind.scen()})
  
  output$pow <- renderText({
    if(input$update > 0) {
      pvl=run.scen()
      return(paste("Estimated Power = ",
                   round(100*(length(pvl[pvl<0.05])/length(pvl))),"%",sep=""))
    }
  })
  
  
  output$powind <- renderText({
    if(input$updateind > 0) {
      pvl=run.ind.scen()
      return(paste("Estimated Power = ",round(pvl),"%",sep=""))
    }
  })
  
  
  #################################################### #	
  ##### specify inputs parameters that can be reset #### 
  #################################################### #
  
  #define dynamic inputs for scenarios defining site-based data
  output$resetable_input <- renderUI({
    times <- input$reset_input
    div(id=letters[(times %% length(letters)) + 1],
        #numericInput('noyear', 'No. of Years', 5,min=1,max=20,step=1),
        fluidRow(align="bottom",
                 column(9,style = "margin-top: -25px;",
                        numericInput('noyear', 'No. of Years', 5,
                                     min=1,max=20,step=1)),
                 column(3,actionButton("show1", "",icon = icon("info")))),	
        fluidRow(align="bottom",
                 column(9,
                        checkboxInput("mult_yr", label = "Multiple Year Scenarios",
                                      value = FALSE)),
                 column(3,actionButton("show2", "",icon = icon("info")))),		  
        conditionalPanel("input.mult_yr==true",
                         sliderInput("nosite.yr.rng", 
                                     label = "No. of Years Scenario Range", 
                                     min = 2, max = 20, value = c(5,20),ticks=FALSE)  
        ),
        fluidRow(align = "bottom",
                 column(9,
                        checkboxInput("hist", label = "Include historic data", 
                                      value = FALSE)),
                 column(3,actionButton("show3", "", icon = icon("info")))),		  
        conditionalPanel("input.hist==true",
                         sliderInput("hist.yr", 
                                     label = "No. of years of legacy data", 
                                     min = 0, max = 10, value = c(0),ticks=FALSE)  
        ),
        fluidRow(align = "bottom",
                 column(9,
                        checkboxInput("deschg", label = "Include change in design", 
                                      value = FALSE)),
                 column(3, actionButton("show4","",icon=icon("info")))),		  
        conditionalPanel("input.deschg==true",
                         textInput('deschg.yr', 
                                   'Years in which change occurs (comma delimited)',
                                   "0")  
        ),
        fluidRow(align="bottom",
                 column(9,
                        textInput('nosite.yr', 'No. of sites per Year', "10")),
                 column(3,actionButton("show5", "",icon = icon("info")))),	
        fluidRow(align="bottom",
                 column(9,
                        checkboxInput("mult_st", label = "Multiple Site Scenarios", 
                                      value = FALSE)),
                 column(3,actionButton("show6", "",icon = icon("info")))),
        conditionalPanel("input.mult_st==true",
                         sliderInput("nosite.st.rng", 
                                     label = "No. of Sites Scenario Range", 
                                     min = 2, max = 100, value = c(5,25),ticks=FALSE)  
        ),  
        fluidRow(align="bottom",
                 column(9,
                        textInput('noreps', 'No. of within site replicates per year',
                                  "3")),
                 column(3,actionButton("show7", "",icon = icon("info")))),	
        fluidRow(align="bottom",
                 column(9,
                        textInput('samfreq', 'How often sites are repeated (years)',
                                  "1")),
                 column(3,actionButton("show8", "",icon = icon("info")))),
        fluidRow(align="bottom",
                 column(9,
                        numericInput('tslope', 'Year on year change', 
                                     input$tslope_intro, 
                                     min = 0, max = 0.25,step=0.01)),
                 column(3,actionButton("show9", "",icon = icon("info")))),	
        fluidRow(align="bottom",
                 column(9,
                        checkboxInput("mult_ef", label = "Multiple change scenarios", 
                                      value = FALSE)),	
                 column(3,actionButton("show10", "",icon = icon("info")))),
        conditionalPanel("input.mult_ef==true",
                         sliderInput("nosite.ef.rng", label = "Effect Size Range", 
                                     min = 0, max = 0.25, value = c(0,0.1),
                                     ticks=FALSE)  
        ),
        
        fluidRow(
          column(9,sliderInput('nsims', 'Number of Simulations', 10, min = 1,
                               max = 1000,step=50)),
          column(3,actionButton("show11","",icon=icon("info"))))
    )
  })
  
  #define dynamic inputs for scenarios defining individual observation data
  output$resinputind <- renderUI({
    times <- input$reset_inputind
    div(id=letters[(times %% length(letters)) + 1],
        fluidRow(
          column(9,
                 numericInput('noyear.ind', 'No. of Years', 5,min=1,max=20,step=1)),
          column(3, actionButton("ishow1","",icon=icon("info")))),
        fluidRow(
          column(9,
                 checkboxInput("noyr_ind", label = "Multiple Year Scenarios",
                               value = FALSE)),
          column(3, actionButton("ishow2","",icon=icon("info")))),
        conditionalPanel("input.noyr_ind==true",
                         sliderInput("yr.rng.ind", label = "Multiple Years Range",
                                     min = 2, max = 25, value = c(2,10),ticks=FALSE)  
        ),
        fluidRow(
          column(9,checkboxInput("histind", label = "Include historic data", 
                                 value = FALSE)),
          column(3, actionButton("ishow3","",icon=icon("info")))),
        conditionalPanel("input.histind==true",
                         sliderInput("hist.yrind", 
                                     label = "No. of years of legacy data", 
                                     min = 0, max = 10, value = c(0),ticks=FALSE)  
        ),
        fluidRow(
          column(9,checkboxInput("deschg_ind", label = "Include change in design",
                                 value = FALSE)),
          column(3, actionButton("ishow4","",icon=icon("info")))),
        conditionalPanel("input.deschg_ind==true",
                         textInput('deschg_ind.yr', 
                                   'Years in which change occurs (comma delimited)',
                                   "1,5,10")  
        ),
        fluidRow(
          column(9,textInput('noind.yr',
                             'No. of Individuals per Year (comma delimited)', "10")),
          column(3,actionButton("ishow5","",icon=icon("info")))),
        fluidRow(
          column(9,checkboxInput("noind_ind", 
                                 label = "Multiple Individual Scenarios",
                                 value = FALSE)),
          column(3, actionButton("ishow6","",icon=icon("info")))),
        conditionalPanel("input.noind_ind==true",
                         sliderInput("ind.rng.ind",
                                     label = "No. of Individuals Range", 
                                     min = 10, max = 250, value = c(10,100),
                                     ticks=FALSE)  
        ),
        fluidRow(
          column(9,textInput('rep.ind', 
                             'Frequency of sampling in years (comma delimited)',
                             "1")),
          column(3, actionButton("ishow7","",icon=icon("info")))),
        fluidRow(
          column(9,numericInput('tslope.ind', 'Effect Size', input$tslope_intro, 
                                min = 0, max = 2,step=0.05)),
          column(3, actionButton("ishow8","",icon=icon("info")))),
        fluidRow(
          column(9,checkboxInput("mult_efind", label = "Multiple Effect Scenarios", 
                                 value = FALSE)),
          column(3, actionButton("ishow9","",icon=icon("info")))),
        conditionalPanel("input.mult_efind==true",
                         sliderInput("ef.rng.ind", label = "Effect Size Range", 
                                     min = 0, max = 0.25, value = c(0,0.1),
                                     ticks=FALSE)  
        ),
        fluidRow(
          column(9,
                 sliderInput('nsimi', 'Number of Simulations', 10,
                             min = 1, max = 1000,step=50)),
          column(3, actionButton("ishow10","",icon=icon("info"))))
        
    )
  })
  
  
  #define dynamic parameter inputs for data distributions for site-based data	
  output$resetable_inputp <- renderUI({
    times <- input$reset_inputp
    div(id=letters[(times %% length(letters)) + 1],
        fluidRow(
          column(9,
                 numericInput('var1', 'Between Site Variation', 
                              switch(input$presets,
                                     "fish" = 1.5,
                                     "honey" = 0.15,
                                     "lead" = 2),
                              min=1,max=20,step=0.1)),
          column(3, actionButton("showpv1","",icon=icon("info")))),
        fluidRow(
          column(9,
                 numericInput('var2', 'Average Site values',
                              switch(input$presets,
                                     "fish"=1,
                                     "honey" = 0.16,
                                     "lead" = 1.23),
                              min=1,max=20,step=0.1)),
          column(3,actionButton("showpv2","",icon=icon("info")))),
        fluidRow(
          column(9,
                 numericInput('var3', 'Between replicate variation', 
                              switch(input$presets,
                                     "fish" = 0.25,
                                     "honey" = 0.34,
                                     "lead" = 0.35),
                              min=1,max=20,step=0.1)),
          column(3,actionButton("showpv3","",icon=icon("info")))),
        fluidRow(
          column(9,
                 numericInput('var4', 'Residual Variation', 
                              switch(input$presets,
                                     "fish" = 0.5,
                                     "honey" = 0,
                                     "lead" = 0.8),
                              min=1,max=20,step=0.1)),
          column(3,actionButton("showpv4","",icon=icon("info"))))
    )
    
  })
  
  observe(updateRadioButtons(session, "data_distr", 
                             selected = switch(input$presets,
                                               "fish" = "norm",
                                               "honey" = "binom",
                                               "lead" = "norm")))
  
  paramvals <- reactive({
    validate(
      need(check.input() == "",
           check.input())
    )
    outpar <- list()
    if(input$presets=='fish'){
      outpar$var1=1.5 ; outpar$var2=1 ; outpar$var3=0.25 ; outpar$var4=0.5 ;
    }else{
      if(input$presets=='honey'){
        outpar$var1=0.15 ; outpar$var2=0.16 ; outpar$var3=0.34 ; outpar$var4=0 ;
      }else{
        if(input$presets=='lead'){
          outpar$var1=2 ; outpar$var2=1.23 ; outpar$var3=0.35 ; outpar$var4=0.8 ;
        }
      }
    }
    if(input$param_spec=='val'){
      outpar$var1=input$var1 
      outpar$var2=input$var2
      outpar$var3=input$var3
      outpar$var4=input$var4
    }
    outpar
    
  })
  
  
  paramvals.ind <- reactive({
    validate(
      need(check.input.ind() == "",
           check.input.ind())
    )
    outparind <- list()
    if(input$presetind=='bird'){
      outparind$var1=0.2 ; outparind$var2=0.5 ; outparind$var3=0.8 ; 
    }else{
      if(input$presetind=='otters'){
        outparind$var1=0.2; outparind$var2=0.5; outparind$var3=0.8 ; 
      }
    }
    if(input$param_specind=='val'){
      outparind$var1=input$var1ind
      outparind$var2=input$var2ind 
      outparind$var3=input$var3ind 
    }
    outparind
    
  })
  
  
  #define dynamic parameter inputs for data distributions for individual observation
  #data
  output$resetable_inputpind <- renderUI({
    times <- input$reset_inputpind
    div(id=letters[(times %% length(letters)) + 1],
        fluidRow(
          column(9,
                 numericInput('var1ind', 'Between Individual Variation',
                              switch(input$presetind,
                                     "bird" = 0.2,
                                     "otters" = 0.2),min=0.1,max=1.5,step=0.1)),
          column(3, actionButton("ishowpv1","",icon=icon("info")))),
        fluidRow(
          column(9,
                 numericInput('var2ind', 'Average value per individual',
                              switch(input$presetind,
                                     "bird" = 0.5,
                                     "otters" = 0.5),min=0.01,max=10,step=0.1)),
          column(3, actionButton("ishowpv2","",icon=icon("info")))),
        fluidRow(
          column(9,
                 numericInput('var3ind', 'Residual Variation', 
                              switch(input$presetind,
                                     "bird" = 0.8,
                                     "otters" = 0.8),min=0.1,max=2,step=0.1)),
          column(3,actionButton("ishowpv3","",icon=icon("info"))))
    )
    
  })
  
  observeEvent(input$show, {
    showModal(modalDialog(title = "Set scenario",
                          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/60FWIU4sgCU" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')                   
    ))
  })
  
  
  # Info buttons ####
  observeEvent(input$show1, {
    showModal(modalDialog(title = "No. of Years",
                          "The number of years the survey takes place over."                         
    ))
  })	
  
  observeEvent(input$show2, {
    showModal(modalDialog(title = "Multiple Year Scenarios",
                          paste("If you want to look at the results of a range", 
                                "of years the you can use this option. The results",
                                "of varying the numbers of years across the range",
                                "specified will be plotted in the graph beneath the",
                                "table. The graph at the top and the table only",
                                "show the results of the number of years specified",
                                "above.")
    ))
  })
  
  observeEvent(input$show3, {
    showModal(modalDialog(title = "Include historic data",
                          paste("If you want to look at the impacts of including",
                                "data from historic surveys click here and use the",
                                "slider to state how many years of historic",
                                "survey you want to include.")
    ))
  })
  
  observeEvent(input$show4, {
    showModal(modalDialog(title = "Include change in design",
                          paste("If you want to look at the impacts of changing",
                                "survey design then use this option. You can",
                                "specify the years in which change occurs by",
                                "entering numbers separated by commas or",
                                "semicolons into the dialogue that appears below",
                                "when this option is selected. Use negative",
                                "years to have change occur in the historic",
                                "survey period if you wish. For specifying the",
                                "change in the survey enter multiple numbers",
                                "into the appropriate survey design option",
                                "below. Separate numbers by commas or semicolons",
                                "If only one option is given in the survey design",
                                "entry then it is assumed to be constant over",
                                "the entire survey period.")
    ))
  })
  
  observeEvent(input$show5, {
    showModal(modalDialog(title = "No. of sites per Year",
                          paste("Use this option to specify the number of sites",
                                "surveyed per year. Sites are assumed to be areas",
                                "within which there could be multiple replicates",
                                "(see below).",
                                "If you want this to change",
                                "over the survey period use the above change in",
                                "design option and enter multiple numbers here",
                                "separated by commas or semi-colons, e.g. 20,50",
                                "or 10;30;80")
    ))
  })
  
  observeEvent(input$show6, {
    showModal(modalDialog(title = "Multiple Site Scenarios",
                          paste("If you want to look at the results of a range",
                                "of number of sites you can use this option.", 
                                "The results",
                                "of varying the numbers of sites across the range",
                                "specified will be plotted in the graph beneath the",
                                "table. The graph at the top and the table only",
                                "show the results of the number of sites specified",
                                "above.")
    ))
  })
  
  observeEvent(input$show7, {
    showModal(modalDialog(title = "No. of within site replicates per year",
                          paste("Use this option to set how many within site",
                                "replicates per year you want your survey",
                                "to have.", 
                                "If you want this to change",
                                "over the survey period use the above change in",
                                "design option and enter multiple numbers here",
                                "separated by commas or semi-colons, e.g. 3,5",
                                "or 1;3;8")
    ))
  })
  
  observeEvent(input$show8, {
    showModal(modalDialog(title = "How often sites are repeated (years)",
                          paste("Use this option to set how often sites",
                                "are repeated. A value of 1 means each site",
                                "is surveyed every year while a value higher",
                                "than 1 means that there will be longer gaps",
                                "between resurveys, with different sites being",
                                "surveyed each year in a rolling programme.",
                                "If you want this to change",
                                "over the survey period use the above change in",
                                "design option and enter multiple numbers here",
                                "separated by commas or semi-colons, e.g. 3,5",
                                "or 1;3;8")
    ))
  })
  
  observeEvent(input$show9, {
    showModal(modalDialog(title = "Year on year change",
                          paste("Use this option to set how much the property",
                                "of interest is changing over time.",
                                "A value of 0 means no change over time.",
                                "For the purposes of this app only positive change",
                                "is allowed but the results of the power analysis",
                                "would be the same if negative change occurred.",
                                "If you are unsure of what levels of change you",
                                "might expect then use the Multiple change scenarios",
                                "option below to explore what change you'd be able",
                                "to detect at a given survey structure, or enter",
                                "values into this box and click Update Analysis",
                                "to see an example of how that data might look in",
                                "the figure to the top right. Note that while",
                                "the figure changes when you change this number",
                                "the new number does not come into effect until you",
                                "press Update Analysis.")
    ))
  })
  observeEvent(input$show10, {
    showModal(modalDialog(title = "Multiple change scenarios",
                          paste("If you want to look at the results of a range",
                                "of change scenarios you can use this option.", 
                                "The results",
                                "of varying the change scenarios across the range",
                                "specified will be plotted in the graph beneath the",
                                "table. The graph at the top and the table only",
                                "show the results of the Year on year change",
                                "specified above.")
    ))
  })
  observeEvent(input$show11, {
    showModal(modalDialog(title = "Number of simulations",
                          paste("The number of simulations from which the expected",
                                "power (entered in table) is derived. The greater",
                                "the number of simulations the more accurate the",
                                "estimated power, but also the longer it takes",
                                "to compute. The figure to the top right is just",
                                "a single simulation while the multiple scenario",
                                "graphs which appear below the table have each",
                                "point in the graph representing the number of",
                                "simulations (i.e. total number of simulations",
                                "run = number of simulations x 5 in multiple",
                                "scenario options).")
    ))
  })
  
  observeEvent(input$showp1, {
    showModal(modalDialog(title = "What type of data?",
                          paste("Is your response variable continuous, e.g.",
                                "concentration of some pollutant, or is it binary",
                                "e.g. above/below some detection limit?")
    ))
  })
  observeEvent(input$showp2, {
    showModal(modalDialog(title = "How to specify input parameters?",
                          paste("We have given you the option of using parameters",
                                "from specific surveys which you can do by",
                                "clicking 'Using presets' or you can specify your",
                                "own by clicking 'Specify values'. If you go",
                                "from a specific preset to 'Specify values' the",
                                "entries in those boxes will be already filled",
                                "with the values from that preset.")
    ))
  })
  observeEvent(input$showp3, {
    showModal(modalDialog(title = "Parameterise according to what data?",
                          paste("We have given you the options of using parameters",
                                "from three surveys, the Marine Fish survey,",
                                "the Honey Monitoring Scheme, and",
                                "the EA lead scheme.",
                                "The honey monitoring scheme parameters are",
                                "appropriate for use with binary data, the",
                                "others are appropriate for use with continuous",
                                "data.")
    ))
  })
  observeEvent(input$showpv1, {
    showModal(modalDialog(title = "Between Site Variation",
                          paste("How different on average is each site from",
                                "the other sites? Higher values indicate more",
                                "differences between sites.")
    ))
  })
  observeEvent(input$showpv2, {
    showModal(modalDialog(title = "Average Site Values",
                          paste("This is the average value for each site at the",
                                "starting point of survey.")
    ))
  })
  observeEvent(input$showpv3, {
    showModal(modalDialog(title = "Between Replicate Variation",
                          paste("How different on average are the replicates within",
                                "each site from each other?")
    ))
  })
  observeEvent(input$showpv4, {
    showModal(modalDialog(title = "Residual Variation",
                          paste("This is the average amount of random variation",
                                "in the outcome that",
                                "cannot be explained by the parameters above.")
    ))
  })
  
  # Individual info buttons ####
  observeEvent(input$ishow1, {
    showModal(modalDialog(title = "No. of Years",
                          "The number of years the survey takes place over."                         
    ))
  })	
  
  observeEvent(input$ishow2, {
    showModal(modalDialog(title = "Multiple Year Scenarios",
                          paste("If you want to look at the results of a range", 
                                "of years the you can use this option. The results",
                                "of varying the numbers of years across the range",
                                "specified will be plotted in the graph beneath the",
                                "table. The graph at the top and the table only",
                                "show the results of the number of years specified",
                                "above.")
    ))
  })
  
  observeEvent(input$ishow3, {
    showModal(modalDialog(title = "Include historic data",
                          paste("If you want to look at the impacts of including",
                                "data from historic surveys click here and use the",
                                "slider to state how many years of historic",
                                "survey you want to include.")
    ))
  })
  
  observeEvent(input$ishow4, {
    showModal(modalDialog(title = "Include change in design",
                          paste("If you want to look at the impacts of changing",
                                "survey design then use this option. You can",
                                "specify the years in which change occurs by",
                                "entering numbers separated by commas or",
                                "semicolons into the dialogue that appears below",
                                "when this option is selected. Use negative",
                                "years to have change occur in the historic",
                                "survey period if you wish. For specifying the",
                                "change in the survey enter multiple numbers",
                                "into the appropriate survey design option",
                                "below. Separate numbers by commas or semicolons",
                                "If only one option is given in the survey design",
                                "entry then it is assumed to be constant over",
                                "the entire survey period.")
    ))
  })
  
  observeEvent(input$ishow5, {
    showModal(modalDialog(title = "No. of Individuals per Year",
                          paste("Use this option to specify the number of individuals",
                                "surveyed per year.",
                                "If you want this to change",
                                "over the survey period use the above change in",
                                "design option and enter multiple numbers here",
                                "separated by commas or semi-colons, e.g. 20,50",
                                "or 10;30;80")
    ))
  })
  
  observeEvent(input$ishow6, {
    showModal(modalDialog(title = "Multiple Individual Scenarios",
                          paste("If you want to look at the results of a range",
                                "of number of individuals you can use this option.", 
                                "The results",
                                "of varying the numbers of individuals across the range",
                                "specified will be plotted in the graph beneath the",
                                "table. The graph at the top and the table only",
                                "show the results of the number of individuals specified",
                                "above.")
    ))
  })
  
  observeEvent(input$ishow7, {
    showModal(modalDialog(title = "Frequency of sampling in years",
                          paste("?",
                                "If you want this to change",
                                "over the survey period use the above change in",
                                "design option and enter multiple numbers here",
                                "separated by commas or semi-colons, e.g. 3,5",
                                "or 1;3;8")
    ))
  })
  
  observeEvent(input$ishow8, {
    showModal(modalDialog(title = "Effect Size",
                          paste("Use this option to set how much the property",
                                "of interest is changing over time.",
                                "A value of 0 means no change over time.",
                                "For the purposes of this app only positive change",
                                "is allowed but the results of the power analysis",
                                "would be the same if negative change occurred.",
                                "If you are unsure of what levels of change you",
                                "might expect then use the Multiple change scenarios",
                                "option below to explore what change you'd be able",
                                "to detect at a given survey structure, or enter",
                                "values into this box and click Update Analysis",
                                "to see an example of how that data might look in",
                                "the figure to the top right. Note that while",
                                "the figure changes when you change this number",
                                "the new number does not come into effect until you",
                                "press Update Analysis.")
    ))
  })
  observeEvent(input$ishow9, {
    showModal(modalDialog(title = "Multiple effect scenarios",
                          paste("If you want to look at the results of a range",
                                "of effect scenarios you can use this option.", 
                                "The results",
                                "of varying the effect scenarios across the range",
                                "specified will be plotted in the graph beneath the",
                                "table. The graph at the top and the table only",
                                "show the results of the Effect Size",
                                "specified above.")
    ))
  })
  observeEvent(input$ishow10, {
    showModal(modalDialog(title = "Number of simulations",
                          paste("The number of simulations from which the expected",
                                "power (entered in table) is derived. The greater",
                                "the number of simulations the more accurate the",
                                "estimated power, but also the longer it takes",
                                "to compute. The figure to the top right is just",
                                "a single simulation while the multiple scenario",
                                "graphs which appear below the table have each",
                                "point in the graph representing the number of",
                                "simulations (i.e. total number of simulations",
                                "run = number of simulations x 5 in multiple",
                                "scenario options).")
    ))
  })
  
  observeEvent(input$ishowp1, {
    showModal(modalDialog(title = "How to specify input parameters?",
                          paste("We have given you the option of using parameters",
                                "from specific surveys which you can do by",
                                "clicking 'Using presets' or you can specify your",
                                "own by clicking 'Specify values'. If you go",
                                "from a specific preset to 'Specify values' the",
                                "entries in those boxes will be already filled",
                                "with the values from that preset.")
    ))
  })
  observeEvent(input$ishowp2, {
    showModal(modalDialog(title = "Parameterise according to what data?",
                          paste("We have given you the options of using parameters",
                                "from two surveys, one using birds and one using otters")
    ))
  })
  observeEvent(input$ishowpv1, {
    showModal(modalDialog(title = "Between Individual Variation",
                          paste("How different on average is each individual from",
                                "the other individuals? Higher values indicate more",
                                "differences between individuals.")
    ))
  })
  observeEvent(input$ishowpv2, {
    showModal(modalDialog(title = "Average Value per Individual",
                          paste("This is the average value per individual",
                                "at the starting point of the survey.")
    ))
  })
  observeEvent(input$ishowpv3, {
    showModal(modalDialog(title = "Residual Variation",
                          paste("This is the average amount of random variation",
                                "in the outcome that",
                                "cannot be explained by the parameters above.")
    ))
  })
  
  ############################################################### #
  ####
  ####   output tables ####
  ####
  ######################################### #
  
  ### table of parameters and power that can be appended for site-based data
  
  dtp <- reactiveValues()
  dtp$df <- data.frame(No.Year= numeric(0),No.Legacy.Years= numeric(0),
                       Year.of.Change=numeric(0),No.Sites= numeric(0),
                       No.Reps= numeric(0),RepeatFreq= numeric(0),
                       Change= numeric(0),Power= numeric(0))
  
  newEntry <- observe({
    
    pvl=run.scen()
    pvl=round(100*(length(pvl[pvl<0.05])/length(pvl)))
    
    isolate(dtp$df[nrow(dtp$df) + 1,] <- c(input$noyear,input$hist.yr,
                                           input$deschg.yr,input$nosite.yr,
                                           input$noreps,input$samfreq,
                                           input$tslope,pvl))
    
  })
  
  output$ptab <- renderDT({dtp$df})
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0("data-",Sys.Date(),".csv")
    },
    content = function(con){
      write.csv(dtp$df, con)
    }
  )
  
  
  ### table of parameters and power that can be appended for individual observation data
  dtpind <- reactiveValues()
  dtpind$df <- data.frame(No.Year= numeric(0),No.Legacy.Years= numeric(0),
                          No.Individuals= numeric(0),Frequency= numeric(0),
                          Change= numeric(0),Power= numeric(0))
  
  newEntryind <- observe({
    
    pvl=run.ind.scen()
    pvl=round(pvl)
    
    isolate(dtpind$df[nrow(dtpind$df) + 1,] <- c(input$noyear.ind,input$hist.yrind,
                                                 input$noind.yr,input$rep.ind,
                                                 input$tslope.ind,pvl))
    
  })
  
  output$ptabind <- renderDT({dtpind$df})
  
  output$downloadTableInd <- downloadHandler(
    filename = function() {
      paste0("data-",Sys.Date(),".csv")
    },
    content = function(con){
      write.csv(dtpind$df, con)
    }
  )
  
  
  #################################################### #
  ###
  ###
  ###     END CODE                                  ####
  ###
  #################################################### #
  
  
} # close function 






















