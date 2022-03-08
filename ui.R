library(shiny)
library(shinydashboard)

shinyUI(

  fluidPage(
  
  shinyjs::useShinyjs(),
  
  fluidRow(h5("")),
  
  fluidRow(
	  column(3
	  ),
	  column(6,
		h1(titlePanel("25YEP H4 Power Analysis Tool")),align = 'center'
	  ),        
	  column(3,
		img(src="UKCEH-Logo.png",height=60,width=220),align = 'right'
	  )
  ),
  
  navbarPage("", id="one",
   
	tabPanel("Introduction", 
	
			h1("Simulating Data"),
			
			br(),
			
			h5("This power analysis tool is based on a simulation approach, whereby pseudo data sets are generated according to a hypothetical design, a realistic distribution and some effect (which may by a trend over time, a difference between groups or a driver effect). This pseudo data is then analysed as if the data had been obtained in reality and a formal statistical test is conducted to assess whether the effect (trend or treatment) is significant. This process is repeated, storing the number of times that the effect was detected. 
			This is therefore based on the premise of simulating data that represents the particular metrics under consideration and the different scenarios relating to the monitoring intensity and extent as well as a hypothetical change to detect. Data are simulated using existing data sources to understand the distribution and variability over space and time of particular metrics. Critically, existing data are used to establish the key properties and parameters of the distribution of the data to enable realistic simulations to be derived. Important parameters to be established include the shape of the distribution, any upper or lower limits, variation over space, and variation over time. From these properties, pseudo data sets are generated according to the spatial and temporal monitoring scenarios in a parametric bootstrap approach. 
			The following steps indicate the processes involved in the proposed power analysis:"),
			
			br(), 
			
			h5("1.	Use existing data, reviewed and quality assured to check for outliers and anomalies, to establish parameters for data distribution"),
			h5("2.	Define monitoring intensity and extent (e.g. number of individuals per year, how many years)"),
			h5("3.	Define hypothetical effect to detect (e.g. a change over time of a specific percentage rate) "),
			h5("4.	Simulate data to produce a pseudo data set over the required length of time with the required sample size and effect size"),
			h5("5.	Analyse data using modelling approach to estimate effect size and significance. Any dependence structure present within the data, such as repeats over time or spatial correlation will be accounted for within this step as will the data type – e.g. counts, proportions or concentrations.  "),
			h5("6.	Store the results from the fitted model and repeat steps 4 and 5."),
			h5("7.	Statistical power is then obtained by the proportion of the results in step 6 that infer a significant effect")

			
			,
			br(),
			
			h3("Sampling Design"),
			
					br(),
					
					h5("To simulate realistic data, or data that conforms to a particular hypothetical scenario, there are a number of features of the data that we need to define. 
					We start with considering the structure of the survey design. The two major components of the design are how many observations are there and over what time period is being considered. For individual-based observations, this is simply just the number of individuals per year observed and the total number of years of the (hypothtical) survey. 
					For site-based data, there is another aspect. There is the number of sites per year and the total time period (as with the indivudual case), but there is also the possibility of taking multiple measurements (often called replciates) from the same site. This is shown below. "),
					
					br(),
					
					h5(""),
					
					img(src="site_reps.png",height=300,width=1100),
					
					br(),
					br(),	
					
					h5("The number of replicate observations taken at a particular site can be defined below and will define the default value in the power analyses tab for site-based data."),
					
					numericInput('reps_intro', 'How many samples are taken at each site?', 0, min = 1, max = 10,step=1),
					
					br(),
					br(),
					
					h5("For site-based data, where the same unit (e.g. the site itself) can be repeatedly sampled, we also need to define how often samples are taken. In other words, how often we expect sites to be revisited. In simple cases, this may just be every year, but there may be other designs whereby each site is only repeated every few years but with some sampling of sites undertaken in every year of survey. This is shown in the figure below and is often referred to as a rotating panel design, where the panels in this sense represent a particular subset of sites. 
					In this example sites are revisited every 5 years - shown by the same subset of point coloured 5 years apart - but an equal number of sites is still monitored each year. The total number of unique sites is therefore given by the number of sites per year multiplied by the length of time between revisits. In this case, 20 site in each year multiplied by 5 year repeat frequency equals 100 unique sites."),
					
					img(src="rollling_reps.png",height=200,width=1100),
					
					br(),
					br(),
					
					h5("How often sites are revisited in this manner can be defined below (in years) and will define the default value in the power analyses tab for site based data. A value of 1 corresponds to the same sites being surveyed every year, a value of 5 has a 5 year delay between repeat samples as shown in the figure above."),
					
					numericInput('samfreq_intro', 'How often are sites re-visited?', 1, min = 1, max = 10,step=1),
								
					br(),
					br(),
					
					h5("Whilst the concept if repeat visits does not make sense when considering data from individuals (the same individuals cannot be sampled more than once), we do consider the possibility that observations may not be taken every year. 
					We have therefore included a scenario that allows the user to specify the intensity of monitoring of individuals, be that every year or every few years. The figure below shows the examples of taking measurements from individuals every 2 years.
					This enables a comparison between scenarios of twice the number samples every two years versus a half the number of samples every year, for example."),
					
					img(src="ind.freq.png",height=600,width=900),
					
					h5("How often individuals are sampled in this manner can be defined below (in years) and will define the default value in the power analyses tab for data from individuals. A value of 1 corresponds to sampliong individuals every year, whereas a value of 2 will presume samples are only taken every other year as shown in the figure above."),
					
					numericInput('samfreq_ind_intro', 'How often are sites re-visited?', 1, min = 1, max = 10,step=1),
					
					br(),
					br(),
					
					h5("The particular effect that we are interested in these series of power analyses is a trend over time.
					The power analysis therefore, is the power to detect a change in the response over time. The change is assumed to be a linear trend and when generating the pseudo data sets, we specify the hypothetical year on year change in the response metric that we are interested to test whether it can be significantly detected or not. "),
					
					br(),
					
					img(src="yoyc.png",height=600,width=900),
					
					numericInput('tslope_intro', 'Year on year change', 0, min = 0, max = 0.25,step=0.01),
					
					br(),
	
			h2("Data Distribution"),
			
				br(),
				h3("Site-based Data"),
					
					br(),
					h5("There are a number of key aspects to the site-based data that is simulated within the application. The structure follows the assumption that there are a number of sites monitored over time, within which a number of replicate samples are taken. The sites are then repeatedly surveyed throughout the survey according to a specified frequency. The distribution of the data was assumed to be normally distributed on the log scale. The variance of this distribution represents the residual error in the data. The mean of this distribution was defined in a hierarchical manner with each replicate sample having its own mean value, which depended on the site level mean. This is shown diagrammatically in the figure below showing the structure of the data with site (shown in blue) and replicate samples (shown in grey) following some underlying distribution (i.e. each sample at each site has its own average value). On top of this a trend effect is imposed (red dots) and finally some residual variability is added (black squares). There are four different parameters to simulate the data which starts with an average site value (shown in orange) and some between site variation around this (blue). Repeat observations at each site are then distributed around the corresponding site mean with some additional between replicate variance (grey). The time trend (effect) is then imposed on the data and some residual error around this is included (black). This structure was established based on a key exemplar data set. "),
					img(src="dist_params.png",height=600,width=900),
				
				h3("Data from individuals"),
				
					h5("The type of data considered is assumed to come from individual animals opportunistically sampled over time. There is therefore no structure within the design such as repeated observations or so-called nested values, such as replicates within a site. It is therefore reasonable to assume, in this case, that observations are independent.  There are just three components to the data that is simulated that represent the mean (red line in the figure below) and variance (the spread of the histogram below) of the concentrations in each individual and an additional residual variation terms which adds some random noise around the imposed trend (effect of interest) over time. Concentrations are assumed to follow a log-normal distribution. "),
					img(src="dist.png",height=600,width=900)
		
	),
	
	tabPanel("Site-based Data",   
				
		  pageWithSidebar(
			  headerPanel(''),
			  sidebarPanel(
				  
				  h1("Define Scenario"),
					actionButton("show", "",icon = icon("info")), 
						uiOutput('resetable_input'),
						
						actionButton("update", "Update Analysis",icon = icon("calendar")),
						actionButton("reset_input", "Reset inputs",icon = icon("redo")),
			
						
				  h1("Input Parameters"),
						radioButtons("param_spec", label = "How to specify input parameters?", 
										choices=list("Using presets"="pr","Specify values"="val"),selected=NULL),		  
						conditionalPanel("input.param_spec=='val'",	
						uiOutput('resetable_inputp'),
						actionButton("reset_inputp", "Reset Parameter inputs")
						),
						conditionalPanel("input.param_spec=='pr'",	
						radioButtons("presets", "Parameterise according to what data", 
						choices=list("Marine Fish"="fish","Honey Monitoring"="honey","EA Lead"="lead"), selected = NULL)
						)
			  ),
			  
			  mainPanel(
					plotOutput('plot1'),
					#verbatimTextOutput("test"),
					br(),
					
					span(textOutput("pow"),style="color:red; font-size: 40px; font-style: bold"),
					
					br(),
					br(),
					
					tableOutput('ptab'),
					
					conditionalPanel("input.mult_yr==true",
					plotOutput('plot2')
					),
					
					conditionalPanel("input.mult_ef==true",
					plotOutput('plot3')
					),
					
					conditionalPanel("input.mult_st==true",
					plotOutput('plot4')
					)
			  )
		)
   
	),

	tabPanel("Data from Individuals",   
		 
		  pageWithSidebar(
			  headerPanel(''),
			  sidebarPanel(
				  
					h1("Define Scenario"),
						uiOutput('resinputind'),
						
						actionButton("updateind", "Update Analysis"),
						actionButton("reset_inputind", "Reset inputs"),
						
					h1("Input Parameters"),
						uiOutput('resetable_inputpind'),
						actionButton("reset_inputpind", "Reset Parameter inputs")
						
			  ),
			  
			  mainPanel(
					
					plotOutput('indplot'),
					
					br(),
					
					span(textOutput("powind"),style="color:red; font-size: 40px; font-style: bold"),
					
					br(),
					
					tableOutput('ptabind'),
					
					br(),
					
					conditionalPanel("input.mult_efind==true",
					plotOutput('indplot.me')
					),
					
					conditionalPanel("input.noind_ind==true",
					plotOutput('indplot.mi')
					),
					
					conditionalPanel("input.noyr_ind==true",
					plotOutput('indplot.my')
					)
			  )
		)
	   
	),

	tabPanel("Example",   
	 
		
	   fluidPage(
			
			h2("Run through video demonstration"),
			
			br(),
			
			tags$video(id="video2", type = "video/mp4",height=300,width=500,src = "Example.mp4", controls = "controls"),
			
			br(),
			
			h2("Example from raw data"),
			h5("The figure below shows the real PBDE data observed from freshwater fish monitored at a number of sites across England between 2015 and 2019. From this data, the key distributional parameters were extracted using a model specified according to the form described on the Introduction tab. The parameters were used to simulate a pseudo data set that should adequately represent the real, observed data. This is shown in the right hand box plot below.
			Good agreement between these two boxplots demonstrates the suitability of the simulation procedure and that the parameters extrated describe the key properties well. "),
			img(src="ex_plots.png",height=400,width=900),align = 'center',
			
			h5("Because the parameters extracted from the observed data have proven to provide suitable representation, we can now simulate pseudo data sets under different designs and compare the statistical power. We start with a design similar to the observed data where we assume there is data over 5 years, that 20 sites are surveyed every years and 3 samples are taken from each site. We wish to investigate the power to detect a hypothetical change of 0.02 MEAS (on the log scale) every year. The simulated data under this scenario is shown in the boxplot below."),
			
			img(src="ex_sim1.png",height=400,width=600),align = 'center',
			
			h5("We compare this to the exact same scenario but where we assume that there exists historical data (e.g. archived sampels) that can also be included. We therefore add in an additional 5 years' worth of data and the pseudo data can be visually described by the boxplot below. "),
			img(src="ex_sim2.png",height=400,width=600),align = 'center',
			
			h5("The application computes and stores the statistical power under each of these scenarios. As the summary table is appended, the results can be easily compared across the two scenarios. Showing that, in this case, the additional 5 years of legacy data has increased power from 17% to 79%"),
			img(src="ex_tab.png",height=220,width=900),align = 'center'
			
		
		)
	)

)
)
)



