library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

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
                        h3("Power Analysis Tool"),
                        
                        h5(paste("This tool is designed to be used to simulate",
                                 "data under different survey designs and expected",
                                 "change scenarios and then use this to calculate",
                                 "the expected power of the specified survey design",
                                 "in detecting the specified change.",
                                 "For a full description of how this tool works",
                                 "see the Explanation tab, for a worked example",
                                 "see the Example tab or to get started on using",
                                 "the app go to either the Site-based Data",
                                 "tab or the Data from Individuals tab."))
                        ),
               
               tabPanel("Explanation", 
                        
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
			
			h4("Data types"),
			br(),
			h5(paste("Within the site-based data we consider responses that can be either",
			         "continuous or binary. A continuous variable is shown in the graph",
			         "above, while an example of a binary variable is whether the",
			         "concentration of some pollutant is above or below some",
			         "detection limit. Whether the variable is continuous or binary has",
			         "implications for how we interpret the effect size/year on year",
			         "change. For continuous variables the effect size is simply the",
			         "increase in the mean per year. However, the effect size for",
			         "binary variables is more complicated as it acts upon the log-odds",
			         "ratio rather than the percentage of sites that are in each category",
			         "which means a given effect size will lead to differing impacts",
			         "dependent on the initial conditions. Use the below controls to",
			         "see what change a given effect size causes to different percentages."
			         )),
			
			br(),
			wellPanel(
			  fluidRow(
			    column(4, 
			           sliderInput("exampleperc","Starting percentage of sites:",
			                       1,99,50,step = 1)),
			    column(4,
			           sliderInput("exampleEffect","Effect size:",
			                       0,0.25,0.1,0.01)),
			    column(2,
			           numericInput("exampleYears","Number of years:",
			                        1,0,100)),
			    column(2,
			           radioButtons("exampleSign","Direction of change",
			                        c("Positive","Negative"),"Positive"))
			    
			  ),
			  br(),
			  span(textOutput("exampleRes"),style="font-size: 20px; style:bold")
			),
			br(),
			
			h3("Data from individuals"),
			
			h5("The type of data considered is assumed to come from individual animals opportunistically sampled over time. There is therefore no structure within the design such as repeated observations or so-called nested values, such as replicates within a site. It is therefore reasonable to assume, in this case, that observations are independent.  There are just three components to the data that is simulated that represent the mean (red line in the figure below) and variance (the spread of the histogram below) of the concentrations in each individual and an additional residual variation terms which adds some random noise around the imposed trend (effect of interest) over time. Concentrations are assumed to follow a log-normal distribution. "),
			img(src="dist.png",height=600,width=900)
			
               ),
			
			tabPanel("Site-based Data",   
			         
			         pageWithSidebar(
			           headerPanel(''),
			           sidebarPanel(
			             
			             h1("Define Scenario"),
			             br(),
			             #actionButton("show", "",icon = icon("info")), 
			             uiOutput('resetable_input'),
			             
			             actionButton("update", "Update Analysis",icon = icon("calendar")),
			             actionButton("reset_input", "Reset inputs",icon = icon("redo")),
			             
			             
			             h1("Input Parameters"),
			             fluidRow(
			               column(9,
			                      radioButtons("data_distr", label = "What type of data?", 
			                          choices=list("Continuous data"="norm",
			                                       "Binary data"="binom"),selected="norm")),
			               column(3, actionButton("showp1","",icon =icon("info")))),
			             fluidRow(
			               column(9,
			                      radioButtons("param_spec", 
			                                   label = "How to specify input parameters?", 
			                                   choices=list("Using presets"="pr",
			                                                "Specify values"="val"),
			                                   selected=NULL)),
			               column(3, actionButton("showp2","", icon = icon("info")))),
			             conditionalPanel("input.param_spec=='val'",	
			                              uiOutput('resetable_inputp'),
			                              actionButton("reset_inputp", "Reset Parameter inputs")
			             ),
			             conditionalPanel("input.param_spec=='pr'",	
			                              fluidRow(
			                                column(9,
			                                       radioButtons("presets", 
			                                           "Parameterise according to what data", 
			                                           choices=list("Marine Fish"="fish",
			                                                        "Honey Monitoring"="honey",
			                                                        "EA Lead"="lead"), 
			                                           selected = NULL)),
			                                column(3, actionButton("showp3","", 
			                                                       icon=icon("info"))))
			             )
			           ),
			           
			           mainPanel(
			             plotlyOutput('plot1'),
			             
			             br(),
			             
			             span(textOutput("pow"),style="color:red; font-size: 20px"),
			             
			             br(),
			             br(),
			             
			             DTOutput('ptab'),
			             
			             downloadLink("downloadTable","Download Table"),
			             
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
			             
			             fluidRow(
			               column(9,
			                      radioButtons("param_specind", 
			                                   label = "How to specify input parameters?", 
			                                   choices=list("Using presets"="pr",
			                                                "Specify values"="val"),
			                                   selected="pr")),
			               column(3,actionButton("ishowp1","",icon=icon("info")))),
			             conditionalPanel("input.param_specind=='val'",
			                              uiOutput('resetable_inputpind'),
			                              actionButton("reset_inputpind",
			                                           "Reset Parameter inputs"),
			             ),
			             conditionalPanel("input.param_specind=='pr'",
			                              fluidRow(
			                                column(9,
			                                       radioButtons("presetind", 
			                                                    "Parameterise according to what data", 
			                                                    choices=list("Birds"="bird",
			                                                                 "Otters"="otters"), 
			                                                    selected = NULL)),
			                                column(3, actionButton("ishowp2","",icon=icon("info"))))
			             )
			             
			           ),
			           
			           mainPanel(
			             
			             plotlyOutput('indplot'),
			             
			             br(),
			             
			             span(textOutput("powind"),style="color:red; font-size: 20px"),
			             
			             # verbatimTextOutput("test2"),
			             br(),
			             
			             DTOutput('ptabind'),
			             
			             downloadLink("downloadTableInd","Download Table"),
			             
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
			           h5(paste("Here in this page we go through two examples to demonstrate",
			                    "some different usages of the app. The first using",
			                    "the site-based data where we try and find what change",
			                    "over time can currently be detected by the honey monitoring",
			                    "scheme. The second uses data from individuals where we",
			                    "try and design a scheme based on the effect size we want",
			                    "to detect.")),
			           
			           hr(),
			           
			           h2("Example of site-based data"),
			           
			           fluidRow(
			             column(7,
			                    h5(HTML(paste("Here we use an example from the honey monitoring",
			                                  "scheme where we are interested in what kinds of",
			                                  "effects we can detect given a set sampling scheme.",
			                                  "For more information on the honey monitoring scheme",
			                                  "see their <a href='https://honey-monitoring.ac.uk/'>",
			                                  "website</a>."))),
			                    br(),
			                    h5(paste("For this analysis we are looking at",
			                             "5 years of survey, and",
			                             "say there are 100 sites visited per year, with only",
			                             "1 replicate per site and where each site is visited every",
			                             "year. We want to look at a range of potential change",
			                             "values so we click the 'Multiple Change Scenarios'",
			                             "button and drag the range to go from 0.05 to 0.25.",
			                             "We also want to run a reasonable number of simulations",
			                             "so we can be more confident in the estimated power",
			                             "so we set the number of simulations to 200.",
			                             "See the image on the right to see what the selection panel",
			                             "looks like after all this.")),
			                    br(),
			                    h5(paste("We use the preset Honey Monitoring parameters, and",
			                             "make sure we are simulating binary data:")),
			                    img(src="honey_params2.png",width=250,height=220),
			                    br(),
			                    h5("And now we are ready to click 'Update Analysis'!"),
			                    h5("This now takes a little while to calculate the results."
			                    )),
			             column(5,
			                    img(src="honey_params.png",height=600,width=250))
			           ),
			           br(),
			           h5(paste("The top right figure on the page (and the figure to the",
			                    "left below) represents a visualisation of a",
			                    "single dataset that follows the rules we set out",
			                    "in the left-hand panel.",
			                    "Note that the effect size used within this figure",
			                    "(and also within the table) is the change over",
			                    "time in the box, not from the range of the",
			                    "Multiple Change Scenario slider.",
			                    "Here we can see the",
			                    "trend in the mean (plus standard error) over",
			                    "time in black, and individual sites are",
			                    "represented as coloured dots and lines.",
			                    "To make it clearer what is going on we can",
			                    "double click on one of the sites in the legend",
			                    "and the rest of the sites disappear - shown",
			                    "in the figure to the right for site 3. We can see",
			                    "that the site goes between having",
			                    "measurements above and below the detection limit",
			                    "over time.")),
			           fluidRow(
			             column(6,
			                    img(src="honey_single.png",height=300,width=600)
			                    ),
			             column(6,
			                    img(src="honey_onesite.png",height=300,width=600))
			           ),
			           br(),
			           h5(paste("What we are particularly interested in is what kind of",
			                    "change over time we can reliably detect with this data,",
			                    "and that is shown in the figure below the table:")),
			           img(src="honey_multiple.png",height = 450, width = 800),
			           h5(paste("We can see that this survey design can detect change over",
			                    "time that is above around 0.2 per year around 80% of the time.",
			                    "Unlike in continuous data, as this is binary data",
			                    "this 0.2 per year does not translate",
			                    "simply to percentage change, but we can use the widget",
			                    "below to show the change we would expect based on the",
			                    "initial starting percentage of 16% (given in parameter values).",
			                    "As this power analysis works for both positive and negative",
			                    "change we can say that 80% of the time we would detect",
			                    "either a drop from 16% to 7% of sites being under the limit",
			                    "or an increase from 16% to 34% of sites being over the limit.")),
			           br(),
			           wellPanel(
			             fluidRow(
			               column(4, 
			                      sliderInput("exampleperc2","Starting percentage of sites:",
			                                  1,99,16,step = 1)),
			               column(4,
			                      sliderInput("exampleEffect2","Effect size:",
			                                  0,0.25,0.2,0.01)),
			               column(2,
			                      numericInput("exampleYears2","Number of years:",
			                                   5,0,100)),
			               column(2,
			                      radioButtons("exampleSign2","Direction of change",
			                                   c("Positive","Negative"),"Positive"))
			               
			             ),
			             br(),
			             span(textOutput("exampleRes2"),style="font-size: 20px; style:bold")
			           ),
			           br(),
			           
			           hr(),
			           
			           br(),
			           
			           h2("Example using data from individuals"),
			           
			           h5(paste("We can use this app to not only see the effect size",
			                    "that can be detected by a given survey design",
			                    "but also to see how we should design a survey to",
			                    "detect an effect we are interested in.",
			                    "Here we will use the tab titled 'Data from Individuals'",
			                    "to investigate how we might try and detect a change",
			                    "in the response of 0.05 per year - this is in log",
			                    "concentration of the pollutant in our case but could",
			                    "be any continuous variable.")),
			           h5(paste("We know that we have 8 years in which to detect this change",
			                    "and that we have variable levels of funding available",
			                    "to test individuals for the pollutant so we can try",
			                    "multiple individual scenarios. We set up the controls",
			                    "such that we run multiple individual scenarios from 10",
			                    "to 70 and examine the plot output under the table:")),
			           
			           img(src="bird_multiple.png",width = 600, height = 400),
			           
			           br(),
			           h5(paste("From this we can see that we have to test over 50",
			                    "individuals per year over the 8 year period in order",
			                    "to detect a change of 0.05 80% of the time.")),
			           fluidRow(
			             column(6,
			           h5(paste("Unfortunately, testing 50 individuals a year for 8 years",
			                    "(a total of 400 individuals) will exceed our budget.",
			                    "However, we know that we have individuals in the freezer",
			                    "that we could test now. We have 5 individuals collected",
			                    "at 2 year intervals for the past 7 years (a total of",
			                    "20 individuals). So we can set up the app to account",
			                    "for this historic data, by selecting 'include historic",
			                    "data' and including a change in design at year 0. We",
			                    "can switch from sampling every two years to every year",
			                    "and up the number of samples taken. We can't use the",
			                    "multiple individual scenarios option to do this",
			                    "but we can do it manually by trying out a few different",
			                    "options ranging from 10 to 30 samples per year within",
			                    "the new survey. An example of how we set this up is",
			                    "shown to the right.")),
			              br(),
			              h5(paste("The results of our testing are shown in",
			                    "the table below, we have used the interactive sorting",
			                    "option to order the rows from highest to lowest power.")),
			           ),
			           column(6,
			                  img(src="bird_inputs.png", width = 300, height = 500))),
			           
			           img(src="bird_table.png",width = 800, height = 300),
			           br(),
			           h5(paste("From these results we can see that adding the historic",
			                    "data allows us to detect change of 0.05 units 80% of",
			                    "the time just by sampling 25 individuals per year from",
			                    "now on - a total of 220 individuals to test, and a",
			                    "saving of around 45%."))
			
			
			         )
			),
			
			tabPanel("Extract input parameter values",
			
			 fluidPage(
			 
							 # App title ----
				  titlePanel("Extract parameters from data frame"),

				  # Sidebar layout with input and output definitions ----
				  sidebarLayout(

					# Sidebar panel for inputs ----
					sidebarPanel(

					h5("Upload a csv file with columns for the response variable of interest (named 'response'), the site identifier (named 'site.id') and a repeat or grouping identifier (named 'rep.id'). For individual based data only the response and rep.id are required."),
					
					  # Input: Select a file ----
					  fileInput("file1", "Choose CSV File",
								multiple = FALSE,
								accept = c("text/csv",
										 "text/comma-separated-values,text/plain",
										 ".csv")),

					  # Horizontal line ----
					  tags$hr(),
					  
					  radioButtons("dat_typ", "Type of Data",
								   choices = c(Site_Based = "site",
											   Individual_Based = "ind"),
								   selected = "site")

					  ),

					# Main panel for displaying outputs ----
					mainPanel(

					  # Output: Data file ----
					  tableOutput("estparams")

					)

				  )
							 
			 )
			
			)
			
    )
  )
)



