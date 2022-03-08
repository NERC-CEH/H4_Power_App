#load all libraries required
library(shiny)
library(nlme)

sim_data_ind <- function(data0, SD, k, tslope.ind, var3ind){
  #individual means
  data0$int <- rnorm(dim(data0)[1],k,SD)
  
  #simulating the response
  data0$response <- rnorm(dim(data0)[1], mean = data0$int+(tslope.ind*data0$year1), sd = var3ind) 
  
  return(data0)
}

sim_data <- function(data0, nosite, SD, k, var3, tslope, var4){
  #site-specific intercept
  int1 <- rnorm(nosite,k,SD)
  int.df <- data.frame(int = int1, site = c(1:nosite))
  
  #including the site-specific intercept to the main data set
  data0$int <- int.df$int[match(data0$site, int.df$site)]
  
  #adding plot-specific variation for each plot
  data0$rep_v <- data0$site_and_reps
  
  levels(data0$rep_v) <- as.numeric(rnorm(length(levels(data0$rep_v)),0,var3))
  data0$int <- data0$int + as.numeric(levels(data0$rep_v))[data0$rep_v]
  
  #simulating the response
  
  data0$response <- rnorm(dim(data0)[1], mean = data0$int+tslope*data0$year1, sd = var4)
  
  data0
}

## all code sits within a function of inputs and outputs 
function(input, output, session) {

  
	#Check how many multiple scenario boxes are ticked for the site based analysis. 
	check.input <- reactive({
  
		tot.true <- as.integer(c(input$mult_yr=="TRUE",input$mult_ef=="TRUE",input$mult_st=="TRUE"))

		return(sum(tot.true))
		
	})
  
	#Check how many multiple scenario boxes are ticked for the data from individuals analysis. 
	check.input.ind <- reactive({
  
		tot.true <- as.integer(c(input$noyr_ind=="TRUE",input$mult_efind=="TRUE",input$noind_ind=="TRUE"))

		return(sum(tot.true))
		
	})
  
	
	#define the length of the time series by adding the historical data length to the number of years
	yrdef <- reactive({ 
		if(input$mult_yr=="TRUE"){
			#if we are looking at the multiple year scenario, then the number of years is a sequence. 
			i.noyear <- seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5) + input$hist.yr
		}else{
			i.noyear <- input$noyear + input$hist.yr
		}
		if(i.noyear[1]<0){i.noyear=abs(i.noyear)}
		
		return(i.noyear) 	 
	})


	#Calculate the total number of sites using the number of sites per year, number of years and the sampling frequency
	nosite.r <- eventReactive(input$update,{
			i.noyear <- yrdef()[1]
			
			if(i.noyear<0){i.noyear <- abs(i.noyear)}
			
			if(check.input()<=1){
				if(i.noyear >= input$samfreq){
					nosite = input$nosite.yr * input$samfreq
				}else{
					isamfreq <- i.noyear
					nosite = input$nosite.yr * isamfreq 
				} 
				nosite
			}
	})

	#specify the structure of the simulated data in terms of unique site and sample identifiers and year 
	selectData <- eventReactive(input$update,{
	
		if(check.input()<=1){
			
			i.noyear <- yrdef()[1]
			
			nosite <- nosite.r()
			
			#establish which sites need to be removed from a full exhaustive set of every replicate at every site in every year in order to conform to sampling frequency
			thin.id <- paste(rep(1:nosite,floor(i.noyear/input$samfreq)),rep(1:i.noyear,each=floor(nosite/input$samfreq)),sep="_")
			
			#create data frame representing exhasistive set of all reps and all sites in all years  
			data0 <- expand.grid(reps=1:input$noreps, site=1:nosite, year=1:i.noyear)
		  
			#define a unique identifier for sites in particular years
			data0$site.yr <- paste(data0$site, data0$year, sep="_")
			
			#thin the full data set to conform to sampling frequency under investigation
			data0 <- data0[is.element(data0$site.yr,thin.id),]

			#convert all identifiers to factors
			data0$site <- as.factor(data0$site)
			data0$reps <- as.factor(data0$reps)
			data0$site_and_reps = interaction(data0$site,data0$reps)
			data0$year1 <- data0$year-1

			data0#[,1:2]
		}
	})

	#simulate data according to the design and distribution specified	
	sim.data <- eventReactive(input$update,{

		if(check.input()<=1){
			
			i.noyear <- yrdef()[1]
			data0 <- selectData()
			
			nosite <- nosite.r()
			
			sim_data(data0, nosite, input$var1, input$var2, input$var3, input$tslope, input$var4)
		
		}
	})
 
 
 	#simulate data according to the design and distribution specified for the data from individuals		
	simdata.ind <- eventReactive(input$updateind,{	

		i.noyear.ind <- input$noyear.ind + input$hist.yrind
		
		totno.ind=input$noind.yr*(i.noyear.ind/input$rep.ind)

		data0 <- expand.grid(ind=1:input$noind.yr, year=seq(1,i.noyear.ind,by=input$rep.ind))	
		data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
		data0$ind <- as.factor(data0$ind)

		data0$year1 <- data0$year-1	
		
	
		data0 <- sim_data_ind(data0, input$var1, input$var2, input$tslope.ind, input$var3ind)
		
		return(data0)
	
	})	
	
	######################
	######################
 
	#run the power analysis for site based data by simulating multiple data sets under the individual scenerio specified
	run.scen <- eventReactive(input$update,{
		
		if(check.input()<=1){
					
			
			pval <- replicate(input$nsims, {

				### simulate data #####
				  
				data0 <- selectData()
			
				nosite <- nosite.r()
				
				data0 <- sim_data(data0, nosite = nosite, SD = input$var1, k = input$var2, 
				                  var3 = input$var3, tslope = input$tslope, var4 = input$var4)


				#### model data ####	 
				mod.boot <- lme(response~year,random=~1|site/reps,data=data0,na.action=na.omit)
				#store the p value corresponding to the estimated trend
				summary(mod.boot)$tTable[2,5]
				

			})

			return(pval)
			
		}
	})
 
	#run the power analysis for site based data by simulating multiple data sets under the multiple year scenerio specified
   run.mult.scen <- eventReactive(input$update,{
			if(check.input()>0){	
			
			mult.pval=matrix(ncol=input$nsims,nrow=5)
			
			yr.rng <- round(seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5)) + input$hist.yr
			
			if(yr.rng[1]<0){yr.rng=abs(yr.rng)}
			
			
			
			for(ik in 1:length(yr.rng)){

				### simulate data #####
				  
				i.noyear <- yr.rng[ik]
			
				if(i.noyear >= input$samfreq){
					nosite = input$nosite.yr * input$samfreq
				}else{
					input$samfreq <- i.noyear
					nosite = input$nosite.yr * input$samfreq 
				} 
					
				#establish which sites need to be removed from a full exhaustive set of every replicate at every site in every year in order to conform to sampling frequency
				thin.id <- paste(rep(1:nosite,floor(i.noyear/input$samfreq)),
				                 rep(1:i.noyear,each=floor(nosite/input$samfreq)),sep="_")
				
				#create data frame representing exhasistive set of all reps and all sites in all years 					
				data0 <- expand.grid(reps=1:input$noreps, site=1:nosite, year=1:i.noyear)
				#define a unique identifier for sites in particular years
				data0$site.yr <- paste(data0$site, data0$year, sep="_")
				
				#thin the full data set to conform to sampling frequency under investigation
				data0 <- data0[is.element(data0$site.yr,thin.id),]
				
				#convert all identifiers to factors
				data0$site <- as.factor(data0$site)
				data0$reps <- as.factor(data0$reps)
				data0$site_and_reps = interaction(data0$site,data0$reps)
				data0$year1 <- data0$year-1
					
				mult.pval[ik,] <- replicate(input$nsims,{

					
					data0 <- sim_data(data0, nosite, input$var1, input$var2, input$var3, 
					                  input$tslope, input$var4)

					#### model data ####
					mod.boot <- lme(response~year,random=~1|site/reps,data=data0,na.action=na.omit)
					#store the p value corresponding to the estimated trend
					summary(mod.boot)$tTable[2,5]
					


				})
			
			}
			
			return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
			}
		
	})
	
	#run the power analysis for site based data by simulating multiple data sets under the multiple no. of sites scenerio specified
   run.mult.st.scen <- eventReactive(input$update,{
   
			if(check.input()>0){	
			
			#define matrix to store p values in
			mult.pval=matrix(ncol=input$nsims,nrow=5)
			
			#define a sequence covering the range of sites of interest
			st.rng <- round(seq(input$nosite.st.rng[1],input$nosite.st.rng[2],len=5))
			
			i.noyear <- input$noyear  + input$hist.yr
					
			for(ij in 1:length(st.rng)){

					### simulate data #####
					  
					i.nosite.yr <- st.rng[ij]
				
					if(i.noyear >= input$samfreq){
						nosite = i.nosite.yr * input$samfreq
					}else{
						input$samfreq <- i.noyear
						nosite = i.nosite.yr * input$samfreq 
					} 
							
					#establish which sites need to be removed from a full exhaustive set of every replicate at every site in every year in order to conform to sampling frequency
					thin.id <- paste(rep(1:nosite,floor(i.noyear/input$samfreq)),
					                 rep(1:i.noyear,each=floor(nosite/input$samfreq)),sep="_")
					
					#create data frame representing exhasistive set of all reps and all sites in all years 					
					data0 <- expand.grid(reps=1:input$noreps, site=1:nosite, year=1:i.noyear)
					#define a unique identifier for sites in particular years
					data0$site.yr <- paste(data0$site, data0$year, sep="_")
					
					#thin the full data set to conform to sampling frequency under investigation
					data0 <- data0[is.element(data0$site.yr,thin.id),]
					
					#convert all identifiers to factors
					data0$site <- as.factor(data0$site)
					data0$reps <- as.factor(data0$reps)
					data0$site_and_reps = interaction(data0$site,data0$reps)
					data0$year1 <- data0$year-1
					
					
					mult.pval[ij,] <- replicate(input$nsims,{

						data0 <- sim_data(data0, nosite, input$var1, input$var2, input$var3, 
						                  input$tslope, input$var4)
						
						#### model data ####
						mod.boot <- lme(response~year,random=~1|site/reps,data=data0,na.action=na.omit)

						#store the p value corresponding to the estimated trend
						summary(mod.boot)$tTable[2,5]
						
					}) 
			
			}
			
			return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
			
			}
		
	})

	#run the power analysis for site based data by simulating multiple data sets under the multiple effects scenerios specified
 	run.mult.eff.scen <- eventReactive(input$update,{
			
			if(check.input()>0){	
			
				#define matrix to store p values in
				mult.pval=matrix(ncol=input$nsims,nrow=5)
				
				#define a sequence covering the range of sites of interest
				ef.rng <- (seq(input$nosite.ef.rng[1],input$nosite.ef.rng[2],len=5))
	

				### simulate data #####
				  
				i.noyear <- input$noyear + input$hist.yr
			
				if(i.noyear >= input$samfreq){
					nosite = input$nosite.yr * input$samfreq
				}else{
					input$samfreq <- i.noyear
					nosite = input$nosite.yr * input$samfreq 
				} 
						
				#establish which sites need to be removed from a full exhaustive set of every replicate at every site in every year in order to conform to sampling frequency
				thin.id <- paste(rep(1:nosite,floor(i.noyear/input$samfreq)),
				                 rep(1:i.noyear,each=floor(nosite/input$samfreq)),sep="_")
				
				#create data frame representing exhaustive set of all reps and all sites in all years 					
				data0 <- expand.grid(reps=1:input$noreps, site=1:nosite, year=1:i.noyear)
				#define a unique identifier for sites in particular years
				data0$site.yr <- paste(data0$site, data0$year, sep="_")
				
				#thin the full data set to conform to sampling frequency under investigation
				data0 <- data0[is.element(data0$site.yr,thin.id),]
				
				#convert all identifiers to factors
				data0$site <- as.factor(data0$site)
				data0$reps <- as.factor(data0$reps)
				data0$site_and_reps = interaction(data0$site,data0$reps)
				data0$year1 <- data0$year-1
					
				for(ik in 1:length(ef.rng)){
				
					mult.pval[ik,] <- replicate(input$nsims, {
						
						data0 <- sim_data(data0, nosite, input$var1, input$var2, input$var3, 
						                  input$tslope, input$var4)

						#### model data ####			 
						mod.boot <- lme(response~year,random=~1|site/reps,data=data0,na.action=na.omit)

						#store the p value corresponding to the estimated trend
						summary(mod.boot)$tTable[2,5]
							
					})
			
				}
			
			return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
		
			}	
	})
	
	######################
	######################
			
	#run the power analysis for data from individuals by simulating multiple data sets under the scenerio specified		
	run.ind.scen <- eventReactive(input$updateind,{	

		i.noyear.ind <- input$noyear.ind + input$hist.yrind
		
		totno.ind=input$noind.yr*(i.noyear.ind/input$rep.ind)

		data0 <- expand.grid(ind=1:input$noind.yr, year=seq(1,i.noyear.ind,by=input$rep.ind))	
		data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
		data0$ind <- as.factor(data0$ind)

		data0$year1 <- data0$year#-1	
		
		pvali <- replicate(input$nsimi,{
		
				
			data0 <- sim_data_ind(data0, input$var1ind, input$var2ind, 
			                      input$tslope.ind, input$var3ind)
			
			#fit model to simulated data
			mod.bt.i <- lm(response~year,data=data0)
				
			#store the p value corresponding to the estimated trend
			summary(mod.bt.i)$coefficients[2,4]
		
		})

	return(100*(length(pvali[pvali<0.05])/length(pvali)))
	
	})	
	
	
	#run the power analysis for data from individuals by simulating multiple data sets under the multiple effects scenerios specified	
	run.ind.multef.scen <- eventReactive(input$updateind,{	

		mult.pval=matrix(ncol=input$nsimi,nrow=5)
				
		ef.rngi <- (seq(input$ef.rng.ind[1],input$ef.rng.ind[2],len=5))
		
		i.noyear.ind <- input$noyear.ind + input$hist.yrind
		
		totno.ind=input$noind.yr*input$noyear.ind

		data0 <- expand.grid(ind=1:input$noind.yr, year=1:i.noyear.ind)	
		data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
		data0$ind <- as.factor(data0$ind)

		data0$year1 <- data0$year-1	
		
		
		for(ijk in 1:length(ef.rngi)){
		
			mult.pval[ijk,] <- replicate(input$nsimi, {
				
				data0 <- sim_data_ind(data0, input$var1ind, input$var2ind, 
				                      input$tslope.ind, input$var3ind)
				
				#fit model to simulated data
				mod.bt.i <- lm(response~year,data=data0)
					
				#store the p value corresponding to the estimated trend
				summary(mod.bt.i)$coefficients[2,4]
				
			})
		}
		
		return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
	
	})	
	
	#run the power analysis for data from individuals by simulating multiple data sets under the multiple no. of individuals per year scenerios specified	
	run.ind.multind.scen <- eventReactive(input$updateind,{	

		mult.pval=matrix(ncol=input$nsimi,nrow=5)
		i.noyear.ind <- input$noyear.ind + input$hist.yrind		
		ind.rngi <- round((seq(input$ind.rng.ind[1],input$ind.rng.ind[2],len=5)))
	
		for(ijk in 1:length(ind.rngi)){
		
			totno.ind=ind.rngi[ijk]*i.noyear.ind

			data0 <- expand.grid(ind=1:ind.rngi[ijk], year=1:i.noyear.ind)	
			data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
			data0$ind <- as.factor(data0$ind)

			data0$year1 <- data0$year-1	
					
			
			mult.pval[ijk,] <- replicate(input$nsimi, {
			
				data0 <- sim_data_ind(data0, input$var1ind, input$var2ind, 
				                      input$tslope.ind, input$var3ind)
				
				#fit model to simulated data
				mod.bt.i <- lm(response~year,data=data0)
					
				#store the p value corresponding to the estimated trend
				summary(mod.bt.i)$coefficients[2,4]
				
			})
		}
	
	return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
	
	})	
	
	
	#run the power analysis for data from individuals by simulating multiple data sets under the multiple no. of years scenerios specified	
	run.ind.multyr.scen <- eventReactive(input$updateind,{	

		mult.pval=matrix(ncol=input$nsimi,nrow=5)
				
		yr.rngi <- round((seq(input$yr.rng.ind[1],input$yr.rng.ind[2],len=5))) + input$hist.yrind
	
		for(ijk in 1:length(yr.rngi)){
		
			totno.ind=input$noind.yr*yr.rngi[ijk]

			data0 <- expand.grid(ind=1:input$noind.yr, year=1:yr.rngi[ijk])	
			data0$ind.yr <- paste(data0$ind, data0$year, sep="_")		
			data0$ind <- as.factor(data0$ind)

			data0$year1 <- data0$year-1	
				
			mult.pval[ijk,] <- replicate(input$nsimi,{
			
				data0 <- sim_data_ind(data0, input$var1ind, input$var2ind, 
				                      input$tslope.ind, input$var3ind)
				
				#fit model to simulated data
				mod.bt.i <- lm(response~year,data=data0)
					
				#store the p value corresponding to the estimated trend
				summary(mod.bt.i)$coefficients[2,4]
			
			})
		}
		
		return(apply(mult.pval,1,function(x){100*(length(x[x<0.05])/length(x))}))
	
	})		
	
	
	######################
	######################
	
	test.yr <- reactive({
		if(check.input()>0){		
			yr.rng <- seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5)
			return(yr.rng)
		}
	})
		
		
	ef.rng <- reactive({seq(input$nosite.ef.rng[1],input$nosite.ef.rng[2],len=5)})		
	yr.rng <- reactive({seq(input$nosite.yr.rng[1],input$nosite.yr.rng[2],len=5) + input$hist.yr})		
	st.rng <- reactive({seq(input$nosite.st.rng[1],input$nosite.st.rng[2],len=5)})	
	ef.rngi <- reactive({seq(input$ef.rng.ind[1],input$ef.rng.ind[2],len=5)})		
	yr.rngi <- reactive({seq(input$yr.rng.ind[1],input$yr.rng.ind[2],len=5)})		
	ind.rngi <- reactive({seq(input$ind.rng.ind[1],input$ind.rng.ind[2],len=5)})		
				
	
	
	######################################
	###
	###  Produce plots
	###
	######################################
	
	
	#boxplot of simulated data under the site based scenarios
	output$plot1 <- renderPlot({
		 if(input$update > 0) {	 
			if(check.input()<=1){
				if(input$hist & input$hist.yr>0){
					boxplot(response~year,data=sim.data(),names=c((-1*(input$hist.yr-1)):0,1:input$noyear),ylab="log MEAS",xlab="Year",main="Example simulated data",cex.main=2)
				}else{
					boxplot(response~year,data=sim.data(),ylab="log MEAS",xlab="Year",main="Example simulated data",cex.main=2)
				}
			}else{
				plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
				text(0,0,"Only one multiple scenario allowed",cex=2.8)
			}
		}
	})
	
	#plot of multiple year scenarios for site based data 
	output$plot2 <- renderPlot({
		if(input$update > 0) {
			if(check.input()==1){
				plot(yr.rng(),run.mult.scen(),pch=15,cex=1.75,ylab="Power",xlab="Number of Years",ylim=c(0,100))
				lines(yr.rng(),run.mult.scen(),col="grey")
				abline(h=80,col="red")
			}else{
				plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
				text(0,0,"Only one multiple scenario allowed",cex=2.8)
			}
		}
	})
	
	#plot of multiple effects scenarios for site based data
	output$plot3 <- renderPlot({
		if(check.input()==1){
			plot(ef.rng(),run.mult.eff.scen(),pch=15,cex=1.75,ylab="Power",xlab="Effect Size (Coefficient of Year)",ylim=c(0,100))
			lines(ef.rng(),run.mult.eff.scen(),col="grey")
			abline(h=80,col="red")
		}else{
			plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
			text(0,0,"Only one multiple scenario allowed",cex=2.8)
	
		}	
	})
	
	#plot of multiple site number scenarios for site based data
	output$plot4 <- renderPlot({
		if(input$update > 0) {
			if(check.input()==1){
				plot(st.rng(),run.mult.st.scen(),pch=15,cex=1.75,ylab="Power",xlab="Number of Sites per Year",ylim=c(0,100))
				lines(st.rng(),run.mult.st.scen(),col="grey")
				abline(h=80,col="red")
			}else{
				plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
				text(0,0,"Only one multiple scenario allowed",cex=2.8)
			}
		}
	})
			
	#boxplot of simulated data under the individual observation scenarios
	output$indplot <- renderPlot({
		if(check.input()<=1){
			boxplot(response~year,data=simdata.ind(),ylab="log Hg Conc.",xlab="Year")
		}else{
			plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
			text(0,0,"Only one multiple scenario allowed",cex=2.8)
		}
		
	})
	
	#plot of multiple effect scenarios for individual observation data 
	output$indplot.me <- renderPlot({
		if(check.input.ind()<=1){
			plot(ef.rngi(),run.ind.multef.scen(),pch=15,xlab="Effect Size (Coefficient of Year)",ylab="Power %",ylim=c(0,100))
		lines(ef.rngi(),run.ind.multef.scen(),col="grey")
		abline(h=80,col="red")
		}else{
			plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
			text(0,0,"Only one multiple scenario allowed",cex=2.8)
		}
		
	})
	
	#plot of multiple number of individuals per year scenarios for individual observation data 
	output$indplot.mi <- renderPlot({
		if(check.input.ind()<=1){
			plot(ind.rngi(),run.ind.multind.scen(),pch=15,xlab="No. of Individuals per year",ylab="Power %",ylim=c(0,100))
		lines(ind.rngi(),run.ind.multind.scen(),col="grey")
		abline(h=80,col="red")
		}else{
			plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
			text(0,0,"Only one multiple scenario allowed",cex=2.8)
		}
		
	})

	#plot of multiple year scenarios for individual observation data 
	output$indplot.my <- renderPlot({
		if(check.input.ind()<=1){
			plot(yr.rngi(),run.ind.multyr.scen(),pch=15,xlab="No. of Years",ylab="Power %",ylim=c(0,100))
		lines(yr.rngi(),run.ind.multyr.scen(),col="grey")
		abline(h=80,col="red")
		}else{
			plot(0,0,xaxt="n",yaxt="n",type="n",xlab="",ylab="")
			text(0,0,"Only one multiple scenario allowed",cex=2.8)
		}
		
	})
		
	
	####################
	####################
	####################
	
	
	output$test <- renderPrint({check.input()})
	output$test2 <- renderPrint({yrdef()})

	output$test.ind <- renderPrint({run.ind.scen()})
	
	output$powind <- renderText({
		if(input$update > 0) {
			pvl=run.ind.scen()
			return(paste("Estimated Power = ",round(pvl),"%",sep=""))
		}
	})

	output$pow <- renderText({
		if(input$update > 0) {
			pvl=run.scen()
			return(paste("Estimated Power = ",round(100*(length(pvl[pvl<0.05])/length(pvl))),"%",sep=""))
		}
	})

	
	output$powind <- renderText({
		if(input$updateind > 0) {
			pvl=run.ind.scen()
			return(paste("Estimated Power = ",round(pvl),"%",sep=""))
		}
	})

	
	#################################################	
	##### specify inputs parameters that can be reset 
	#################################################

	#define dynamic inputs for scenarios defining site-based data
	output$resetable_input <- renderUI({
        times <- input$reset_input
        div(id=letters[(times %% length(letters)) + 1],
            numericInput('noyear', 'No. of Years', 5,min=1,max=20,step=1),
					checkboxInput("mult_yr", label = "Multiple Year Scenarios", value = FALSE),		  
					conditionalPanel("input.mult_yr==true",
							sliderInput("nosite.yr.rng", label = "No. of Years Scenario Range", min = 2, max = 20, value = c(5,20),ticks=FALSE)  
						),
					checkboxInput("hist", label = "Include historic data", value = FALSE),		  
					conditionalPanel("input.hist==true",
							sliderInput("hist.yr", label = "No. of years of legacy data", min = 0, max = 10, value = c(0),ticks=FALSE)  
						),
					numericInput('nosite.yr', 'No. of sites per Year',10,min=2,max=100,step=1),
					checkboxInput("mult_st", label = "Multiple Site Scenarios", value = FALSE),		  
					conditionalPanel("input.mult_st==true",
							sliderInput("nosite.st.rng", label = "No. of Sites Scenario Range", min = 2, max = 100, value = c(5,25),ticks=FALSE)  
						),  
					 
					numericInput('noreps', 'No. of within site replicates per year',3,min=1,max=25,step=1),
					numericInput('samfreq', 'How often sites are repeated (years)',input$samfreq_intro,min=1,max=10,step=1),
					numericInput('tslope', 'Year on year change', input$tslope_intro, min = 0, max = 0.25,step=0.01),
					checkboxInput("mult_ef", label = "Multiple change scenarios", value = FALSE),		  
					conditionalPanel("input.mult_ef==true",
							sliderInput("nosite.ef.rng", label = "Effect Size Range", min = 0, max = 0.25, value = c(0,0.1),ticks=FALSE)  
						),
						
					sliderInput('nsims', 'Number of Simulations', 10, min = 1, max = 1000,step=50)
			)
   })
	
	#define dynamic inputs for scenarios defining individual observation data
	output$resinputind <- renderUI({
        times <- input$reset_inputind
        div(id=letters[(times %% length(letters)) + 1],
						numericInput('noyear.ind', 'No. of Years', 5,min=1,max=20,step=1),
						checkboxInput("noyr_ind", label = "Multiple Year Scenarios", value = FALSE),		  
						conditionalPanel("input.noyr_ind==true",
							sliderInput("yr.rng.ind", label = "Multiple Years Range", min = 2, max = 25, value = c(2,10),ticks=FALSE)  
						),
						checkboxInput("histind", label = "Include historic data", value = FALSE),		  
						conditionalPanel("input.histind==true",
							sliderInput("hist.yrind", label = "No. of years of legacy data", min = 0, max = 10, value = c(0),ticks=FALSE)  
						),
						numericInput('noind.yr', 'No. of Individuals per Year',10,min=2,max=200,step=10),
						checkboxInput("noind_ind", label = "Multiple Individual Scenarios", value = FALSE),		  
						conditionalPanel("input.noind_ind==true",
							sliderInput("ind.rng.ind", label = "No. of Individuals Range", min = 10, max = 250, value = c(10,100),ticks=FALSE)  
						),
						numericInput('rep.ind', 'Frequency of sampling (in years)', input$samfreq_ind_intro, min = 1, max = 5,step=1),
						numericInput('tslope.ind', 'Effect Size', input$tslope_intro, min = 0, max = 2,step=0.05),
						checkboxInput("mult_efind", label = "Multiple Effect Scenarios", value = FALSE),		  
						conditionalPanel("input.mult_efind==true",
							sliderInput("ef.rng.ind", label = "Effect Size Range", min = 0, max = 0.25, value = c(0,0.1),ticks=FALSE)  
						),
						sliderInput('nsimi', 'Number of Simulations', 10, min = 1, max = 1000,step=50)
			
			)
    })
	

	#define dynamic parameter inputs for data distributions for site-based data	
	output$resetable_inputp <- renderUI({
						times <- input$reset_inputp
						div(id=letters[(times %% length(letters)) + 1],
							numericInput('var1', 'Between Site Variation', 1.5,min=1,max=20,step=0.1),
							numericInput('var2', 'Average Site values', 1,min=1,max=20,step=0.1),
							numericInput('var3', 'Between replicate variation', 0.25,min=1,max=20,step=0.1),
							numericInput('var4', 'Residual Variation', 0.5,min=1,max=20,step=0.1)					
						)
						
				})
				
	#define dynamic parameter inputs for data distributions for individual observation data	
	output$resetable_inputpind <- renderUI({
						times <- input$reset_inputpind
						div(id=letters[(times %% length(letters)) + 1],
							numericInput('var1ind', 'Between Individual Variation', 0.2,min=0.1,max=1.5,step=0.1),
							numericInput('var2ind', 'Average value per individual', 0.5,min=0.01,max=10,step=0.1),
							numericInput('var3ind', 'Residual Variation', 0.8,min=0.1,max=2,step=0.1)				
						)
						
				})


				
	### table of parameters and power that can be appended for site-based data
	dtp <- reactiveValues()
	dtp$df <- data.frame(No.Year= numeric(0),No.Legacy.Years= numeric(0),No.Sites= numeric(0),No.Reps= numeric(0),RepeatFreq= numeric(0),Change= numeric(0),Power= numeric(0))
	
	newEntry <- observe({

		pvl=run.scen()
		pvl=round(100*(length(pvl[pvl<0.05])/length(pvl)))
		
		isolate(dtp$df[nrow(dtp$df) + 1,] <- c(input$noyear,input$hist.yr,input$nosite.yr,input$noreps,input$samfreq,input$tslope,pvl))

	})
	
	output$ptab <- renderTable({dtp$df})

	
	### table of parameters and power that can be appended for individual observation data
	dtpind <- reactiveValues()
	dtpind$df <- data.frame(No.Year= numeric(0),No.Legacy.Years= numeric(0),No.Individuals= numeric(0),Frequency= numeric(0),Change= numeric(0),Power= numeric(0))
	
	newEntryind <- observe({

		pvl=run.ind.scen()
		pvl=round(pvl)
		
		isolate(dtpind$df[nrow(dtpind$df) + 1,] <- c(input$noyear.ind,input$hist.yrind,input$noind.yr,input$rep.ind,input$tslope.ind,pvl))

	})
	
	output$ptabind <- renderTable({dtpind$df})


	#####################################################
	###
	###
	###     END CODE
	###
	#####################################################
	
	
} # close function 





















