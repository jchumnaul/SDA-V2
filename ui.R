library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(shinythemes)
library(ggplot2)
library(gplots)
library(purrr)
library(DT)
library(plyr)
library(dplyr)
library(psych)
library(RVAideMemoire)
library(png)
library(stats)
library(agricolae)
library(car)
library(ggpubr)
library(shinyFiles)
library(fs)
library(rmarkdown)
library(tidyverse)
library(rstatix)
library(shinyWidgets)
library(shinydashboard)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(EnvStats)
library(shinydisconnect)
library(htmltools)
library(bsplus)
library(plotly)  

Sys.setlocale("LC_ALL", "thai") 


ui <- fluidPage(theme = shinytheme("flatly"),
                disconnectMessage(
                  text = "Your session has timed out! Please refresh the page.",
                  refresh = "",
                  background = "#646464e6",
                  size = 36,
                  width = "full",
                  top = "center",
                  colour = "white",
                  overlayColour = "#999",
                  overlayOpacity = 0.4
                ),      
                
                
useShinydashboard(),
useShinyalert(),    
navbarPage(h4(strong("SDA-V2")), 
           
############################################################################# Introduction ##################################################################################
                           
tabPanel(h4("Home"), 
     
         
        sidebarPanel(
            
          img(src = "science1.png", width = "100%"),
          img(src = "rstudio2.png", width = "90%"),
          width = 2),
        
         
         mainPanel(
           tags$style(HTML("
    	.tabbable > .nav > li > [data-value ='Welcome'] {background-color: #6B8EB7;   color: white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Overview'] {background-color: #6B8EB7; color: white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Acknowledgements'] {background-color: #6B8EB7; color: white; font-size: 16px}
.tabbable > .nav > li > [data-value ='References'] {background-color: #6B8EB7; color: white; font-size: 16px}
  	")),
           tabsetPanel(
             tabPanel("Welcome", imageOutput('welcome'), align = "center"),
          tabPanel("Overview", br(),
                  tags$blockquote(h5(strong("App Developer 1: "), "  Asst. Prof. Jularat Chumnaul, Ph.D."),
                  h5(strong("Address: "), "  Division of Computational Science, Faculty of Science, Prince of Songkla University,
Hat Yai, Songkhla, 90110 Thailand"),
                   h5(strong("E-mail: "),  "jularat.c@psu.ac.th")), 

                  tags$blockquote(h5(strong("App Developer 2: "), "  Assoc. Prof. Mohammad Sepehrifar, Ph.D."),
                  h5(strong("Address: "), "  Department of Mathematics and Statistics, Mississippi State University, 
                  Mississippi State, MS 39762 USA"),
                  h5(strong("E-mail: "),  "msepehrifar@math.msstate.edu")),                   
                  
                  p(style ="text-align: justify; font-size = 80px", "The Smart Data Analysis V2 (SDA-V2) is an interactive web app that can communicate and allow for 
                    interaction with users. In the first step of using this software,  users must prepare a data table in .csv format using UTF-8 encoding and upload the prepared data table to the application:", a(href = "https://jularatchumnaul.shinyapps.io/SDA-V2/", "SDA-V2"), ". 
                    Then, users have to provide relevant research details such as the variables to be analyzed, type of research hypothesis (two-sided or one-sided), hypothesized value, and significance level, etc. The data and details entered through the SDA-V2 interface are processed on a server hosted by RStudio. 
                    SDA-V2 automatically explores and visualizes the given data, examines the underlying assumptions associated with parametric tests, selects an appropriate statistical method for the given data, and displays analysis results in a convenient and easy-to-understand format."), 
h5(strong("Privacy statements: "), "We assure you that your data and other information will not be recorded. As soon as you exit our web app, all of your data will be permanently erased."), imageOutput("diagram") 
),
tabPanel("Acknowledgements", br(), 

"SDA-V2 is developed using the following R packages.", br(),
                                       
h5(strong("Package 'base'")),
code("IQR"), "function is used to find the inter quartile range of a data set.", br(),
code("max"), "function is used to find the maximum of a data set.", br(),
code("mean"), "function is used to find the mean of a data set.", br(),
code("median"), "function is used to find the median of a data set.", br(),
code("min"), "function is used to find the minimum of a data set.", br(),
code("median"), "function is used to find the median of a data set.", br(),
code("sd"), "function is used to find the standard deviation of a data set.", br(),
code("table"), "function is used for cross babulation and table creation.", br(),
code("var"), "function is used to find the variance of a data set.", br(), 

h5(strong("Package 'car'")),
code("ncvTest"), "function is used to Compute a score test of the hypothesis of constant error variance against the alternative that the error variance changes with the level of the response.", br(),

h5(strong("Package 'corrplot'")),
code("corrplot"), "function is used to create the correlogram to visualize correlation matrix.", br(),

h5(strong("Package 'dplyr'")),
code("mutate"), "function is used to add new variables.", br(),

h5(strong("Package 'DT'")),
code("datatable"), "function is used to create an HTML table.", br(),

h5(strong("Package 'ggplot2'")),
code("ggplot"), "function is used to create plots.", br(),

h5(strong("Package 'gmodels'")),
code("CrossTable"), "function is used to create cross tabulation with tests for factor independence.", br(),

h5(strong("Package 'gplots'")),
code("plotmeans"), "function is used to plot group means and confidence intervals.", br(),

h5(strong("Package 'graphics'")),
code("abline"), "function is used to add straight lines to a plot.", br(),
code("barplot"), "function is used to create bar plots.", br(),
code("box"), "function is used to draw a box.", br(),
code("boxplot"), "function is used to create box plots.", br(),
code("hist"), "function is used to create histograms.", br(),
code("mosaicplot"), "function is used to create mosaic plots.", br(),
code("plot"), "function is used to create a generic X-Y plotting.", br(),

h5(strong("Package 'ltm'")),
code("cronbach.alpha"), "function is used to find the Cronbach's alpha coefficient.", br(),

h5(strong("Package 'plotly'")),
code("ggplotly"), "function is used to convert ggplot2 to plotly.", br(),

h5(strong("Package 'psych'")),
code("describeBy"), "function is used to find the basic summary statistics by group.", br(),

h5(strong("Package 'quantreg'")),
code("rq"), "function is used to perform a quantile regression.", br(),

h5(strong("Package 'rcompanion'")),
code("nagelkerke"), "function is used to produce McFadden, Cox and Snell, and Nagelkerke pseudo r-squared measures, along with p-values, for models.", br(),

h5(strong("Package 'Rfit'")),
code("rfit"), "function is used to perform a rank-based estimation regression.", br(),

h5(strong("Package 'RVAideMemoire'")),
code("byf.shapiro"), "function is used to perform Shapiro-Wilk test for factor levels.", br(),

h5(strong("Package 'shiny'")),
code("shiny"), "package is used to build an interactive web app.", br(),

h5(strong("Package 'shinyalert'")),
code("shinyalert"), "function is used to display a popup message (modal).", br(),

h5(strong("Package 'shinydisconnect'")),
code("disconnectMessage"), "function is used to display a nice message when a shiny app disconnects.", br(),

h5(strong("Package 'shinythemes'")),
code("shinytheme"), "function is used to set up the Shiny theme, and the 'flatly' theme is used for this app.", br(),

h5(strong("Package 'stats'")),
code("addmargins"), "function is used to put arbitrary margins on multidimensional tables.", br(),
code("aov"), "function is used to fit an analysis of variance model.", br(),
code("bartlett.test"), "function is used to perform Bartlett test of homogeneity of variances.", br(),
code("chisq.test"), "function is used to perform Pearson's Chi-squared test for count data.", br(),
code("cor"), "function is used to find the correlation coefficient.", br(),
code("cor.test"), "function is used to perform a test for correlation.", br(),
code("fisher.test"), "function is used to perform Fisher's exact test for count data.", br(),
code("kruskal.test"), "function is used to perform Kruskal-Wallis rank sum test.", br(),
code("lm"), "function is used to fit linear models.", br(),
code("oneway.test"), "function is used to perform a test for equal means in a one-way layout.", br(),
code("pairwise.t.test"), "function is used to perform pairwise t tests.", br(),
code("pairwise.wilcox.test"), "function is used to perform pairwise Wilcoxon rank sum tests.", br(),
code("shapiro.test"), "function is used to perform Shapiro-Wilk normality test.", br(),
code("t.test"), "function is used to perform Student's t-test.", br(),
code("TukeyHSD"), "function is used to compute Tukey Honest Significant Differences.", br(),
code("var.test"), "function is used to perform F test to compare two variances.", br(),
code("wilcox.test"), "function is used to perform Wilcoxon rank sum and signed rank tests.", br(), br()

),

tabPanel("References", br(),
         "Al-Hoorie AH, Vitta JP. The seven sins of L2 research: A review of 30 journals’
statistical quality and their CiteScore, SJR, SNIP, JCR Impact Factors.",
         em("Language Teaching Research"), ". 2018 23(6):727–744.", br(), br(), 
         
         "Asghar G, Saleh Z. Normality tests for statistical analysis: A guide for
non-Statisticians.", em("International Journal of Endocrinology and Metabolism"), ". 2012
10(2):486–489.", br(), br(), 
         
         "Binder A. Considerations of the place of assumptions in correlational analysis.", 
         em("American Psychologist Journal"), ". 1959 14:504–501.", br(), br(),
         
         "Bland JM, Altman DG. Correlation, regression, and repeated data.", em("BMJ"), ". 1994
308:896.", br(), br(),
         
         "Box JF. Guinness, gosset, fisher, and small samples.", em("Statistical science"), ". 1987:45–52.", br(), br(),  
         
         "Camill G, Hopkins KD. Applicability of chi-square to 2 × 2 contingency tables
with small expected cell frequencies.", em("Psychological Bulletin"), ". 1978 85(1):163–167.", br(), br(),
         
         "Celik N. Welch’s ANOVA: Heteroskedastic skew-t error terms.", em("Communications
in Statistics - Theory and Methods"), ". 2022 51(9):3065–3076.", br(), br(), 
         
         "Choi PT. Statistics for the reader: what to ask before believing the results.",
         em("Canadian Journal of Anesthesia"), ". 2005 52:R1–R5.", br(), br(), 
         
         "Chow S-C., Shao J, Wang H.", em("Sample Size Calculations in Clinical Research
(2nd ed.)"), ". Boca Raton, FL: Chapman & Hall/CRC Biostatistics Series; 2008.", br(), br(),
         
         "Cronbach LJ. Coefficient alpha and the internal structure of tests.",
         em("Psychometrika"), ". 1951 16(3):297–334.", br(), br(),
         
         "Delacre M, Leys C, Mora YL, Lakens D. Taking Parametric Assumptions
Seriously: Arguments for the Use of Welch’s F-test instead of the Classical
F-test in One-Way ANOVA.", em("International Review of Social Psychology"), ". 2019
32(1):1–12.", br(), br(), 
         
         "Ernst AF, Albers CJ. Regression assumptions in clinical psychology research
practice: A systematic review of common misconceptions.", em("PeerJ"), ". 2017 5:e3323.", br(), br(), 
         
         "Fisher RA. Student.", em("Annals of Eugenics"), ". 1939 9(1):1–9.", br(), br(),
         
         "Fisher RA.", em("The design of experiments"), ". Edinburgh, UK: Oliver and Boyd; 1935.", br(), br(),
         
         "Foster RC. KR20 and KR21 for Some Nondichotomous Data (It’s Not Just Cronbach’s Alpha).", em("SAGE journals"), ". 2021 81(6):1–33.", br(), br(), 
         
         "Grizzle JE. The Teacher’s Corner: Continuity Correction in the chi-square
for 2 × 2 Tables.", em("The American Statistician"), ". 1967 21(4):28–32.", br(), br(),
         
         "Hayes AF, Cai L. Further evaluating the conditional decision rule for comparing
two independent means.", em("British Journal of Mathematical and Statistical
Psychology"), ". 2007 60(Pt 2):217–244.", br(), br(), 
         
         "Haynes W. Wilcoxon rank sum test.", em("Encyclopedia of systems biology"), ". 2013:2354–2355.", br(), br(), 
         
         "Hazelton ML. A graphical tool for assessing normality.", em("Journal of the American
Statistical Association"), ". 2003 57:285–288.", br(), br(), 
         
         "Headrick TC. A Note on the Relationship between the Pearson
Product-Moment and the Spearman Rank-Based Coefficients of Correlation.",
         em("Open Journal of Statistics"), ". 2016 6:1025–1027.", br(), br(),
         
         "Heuvel EVD, Zhan Z. Myths About Linear and Monotonic Associations:
Pearson’s r, Spearman’s ρ, and Kendall’s τ.", em("The American Statistician"), ". 2022
76(1):44–52.", br(), br(),
         
         "Hoekstra R, Kiers HAL, Ackerman RA, Johnson A. Are assumptions of
well-known statistical techniques checked, and why (not)?.", em("Frontiers in
Psychology"), ". 2012 3(Article 137):1–9.", br(), br(), 
         
         "Hu Y, Plonsky L. Statistical assumptions in L2 research: A systematic review.",
         em("Second Language Research"), ". 2021 37(1):171–184.", br(), br(), 
         
         "Hulley SB, Cummings SR, Browner WS, Grady D, Newman TB.", em("Designing
clinical research: an epidemiologic approach"), ". Philadelphia, PA: Lippincott
Williams & Wilkins.; 2013.", br(), br(),
         
         "Jiang W, Chen H, Yang L, Pan X. moreThanANOVA: A user-friendly Shiny/R
application for exploring and comparing data with interactive visualization.",
         em("PLoS ONE"), ". 2022 17(7):e0271185.", br(), br(), 
         
         "Kalpić D, Hlupić N, Lovrić M. Student’s t-Tests.", em("International Encyclopedia of
Statistical Science"), ". 2011:1559–1563.", br(), br(), 
         
         "Kashy DA, Donnellan MB, Ackerman RA, Russell DW. Reporting and
interpreting research in PSPB: Practices, principles, and pragmatics.",
         em("Personality and Social Psychology Bulletin"), ". 2009 35:1131–1142.", br(), br(), 
         
         "Kowalski CJ. On the effects of non-normality on the distribution of the sample
product-moment correlation coefficient.", em("Journal of the Royal Statistical Society"), ". 1972 21:1–12.", br(), br(),
         
         "Kruskal WH, Wallis WA. Use of ranks in one-criterion variance analysis.",
         em("Journal of the American statistical Association"), ". 1952 47(260):583–621.", br(), br(), 
         
         "Kuder GF, Richardson MW. The theory of the estimation of test reliability.",
         em("Psychometrika"), ". 1937 2(3):151–160.", br(), br(), 
         
         "Lindstromberg S. Inferential statistics in Language Teaching Research: A review 
and ways forward.", em("Language Teaching Research"), ". 2016 20:741–768.", br(), br(), 
         
         "Olsen CH. Review of the use of statistics in infection and immunity.", em("Infection
and Immunity"), ". 2003 Dec;71(12):6689–6692.", br(), br(), 
         
         "Osborne JW, Waters E. Four assumptions of multiple regression that
researchers should always test.", em("Practical Assessment, Research, and Evaluation"), ".
2002 8:Article 2.", br(), br(), 
         
         "Pearson K. On the criterion that a given system of deviations from the probable
in the case of a correlated system of variables is such that it can be reasonably
supposed to have arisen from random sampling.", em("Philosophical Magazine Series
5"), ". 1900 50(302):157–175.", br(), br(),
         
         "Plonsky L, Gass S. Quantitative research methods, study quality, and outcomes: 
The case of interaction research.", em("Language Learning"), ". 2011 61:325–366.", br(), br(), 
         
         "Plonsky L. Study quality in SLA: An assessment of designs, analyses, and reporting practices in quantitative L2 research.", 
         em("Studies in Second Language Acquisition"), ". 2013 35:655–687.", br(), br(), 
         
         "Razali NM, Wah YB. Power comparisons of shapiro-wilk, kolmogorov-smirnov,
lilliefors and anderson-darling tests.", em("Journal of statistical modeling and
analytics"), ". 2011 2(1):21–33.", br(), br(), 
         
         "Rey D, Neuhäuser M. Wilcoxon-Signed-Rank Test.", em("International Encyclopedia of Statistical Science"), ". 2011:1658–1659.", br(), br(), 
         
         "Rosner B.", em("Fundamentals of biostatistics (8ed ed.)"), ". Boston, MA: Cengage
learning; 2015.", br(), br(),
         
         "Rovinelli RJ, Hambleton RK. On the use of content specialists in the
assessment of criterion-referenced test item validity.", em("Dutch Journal of
Educational Research"), ". 1977 2:49–60.", br(), br(),
         
         "Schober P, Boer C, Schwarte LA. Correlation Coefficients: Appropriate Use and
Interpretation.", em("Anesthesia & Analgesia"), ". 2018 126(5):1763–1768.", br(), br(),
         
         "Schucany WR, Ng HKT. Preliminary goodness-of-fit tests for normality do not
validate the one-sample Student t.", em("Communications in Statistics - Theory and
Methods"), ". 2006 35:2275–2286.", br(), br(), 
         
         "Shapiro SS, Wilk MB. An analysis of variance test for normality (complete samples).", 
         em("Biometrika"), ". 1965 52(3/4):591–611.", br(), br(), 
         
         "Spearman C. The proof and measurement of association between two things.",
         em("The American Journal of Psychology"), ". 1904 15(1):72.", br(), br(),
         
         "Turner RC, Carlson L. Indexes of Item-Objective Congruence for
Multidimensional Items.", em("International Journal of Testing"), ". 2003 3(2):163–171.", br(), br(),
         
         "Walker DA. A Comparison of the Spearman-Brown and Flanagan-Rulon
Formulas for Split Half Reliability under Various Variance Parameter
Conditions.", em("Journal of Modern Applied Statistical Methods"), ". 2006 5(2):443–451.", br(), br(),
         
         "Wilcoxon F. Individual comparisons by ranking methods.", em("Biometrics Bulletin"), ".
1945 1(6):80–83.", br(), br(), 
         
         "Yates F. Contingency table involving small numbers and the chi-square test.",
         em("Supplement to the Journal of the Royal Statistical Society"), ". 1934 1(2):217–235.", br(), br(),
         
         "Zimmerman DW. Some properties of preliminary tests of equality of variances
in the two-sample location problem.", em("The Journal of General Psychology"), ". 1996
123:217–231.", br(), br() 

)
), width = 10)                                          
),

############################################################################# Instrument ##################################################################################                           
tabPanel(h4("Validity/Reliability"),

sidebarPanel(
           img(src = "logosmart2.png", width = "100%"),
           
           box(width = "100%", title = span(icon("dashboard"), "Content validity"), solidHeader = TRUE, status = "primary",       
               box(width = "100%", title = "Item Objective Congruence (IOC) Index", solidHeader = TRUE, status = "info",   fileInput("fileioc", "Upload CSV file:", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".c"))%>%
                     shinyInput_label_embed(
                       icon("info-circle") %>%
                         bs_embed_popover(
                           title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                         )
                     )               
                   ),
               actionButton("checkioc", icon = icon("download"), "Check IOC", class = "btn-primary", width = "100%")),
           
           box(width = "100%", title = span(icon("dashboard"), "Reliability"), solidHeader = TRUE, status = "primary",
               box(width = "100%", title = "Internal-consistency methods", solidHeader = TRUE, status = "info", 
               radioButtons("check2", "Select method:", choices = c("Split-Half" = "spearman", "KR-20" = "kr20", "KR-21" = "kr21", "Cronbach's Alpha" = "cron"), selected = "spearman"),
               
              box(width = "100%", fileInput("filerelia", "Upload CSV file:", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
                    shinyInput_label_embed(
                      icon("info-circle") %>%
                        bs_embed_popover(
                          title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                        )
                    )               
                  ),
              
              conditionalPanel(
                condition = "input.check2 == 'spearman'",
                uiOutput('selecteven'),
                uiOutput('selectodd')
              )
               
              ),
              
              actionButton("checkrelia", icon = icon("download"), "Check reliability", class = "btn-primary", width = "100%")
               
              ),
           
           width = 3),  

         mainPanel(
           tags$style(HTML("
     	.tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}     
    	.tabbable > .nav > li > [data-value = 'Quality of Instrument Report'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
           
    tabsetPanel(
       tabPanel("Data Preparation", imageOutput('pageinstru'), align = "center"),
    tabPanel("Quality of Instrument Report",  verbatimTextOutput('check'), DT::dataTableOutput("instrument"), tags$head(tags$style(HTML("#check{font-size: 16px;}"))))),
    width = 9)                                  
),  
         
############################################################################# Descriptive statistics ##################################################################################
                           
tabPanel(h4("Basic Statistics"),
                                    
sidebarPanel(
tags$style(HTML("
	.box.box-solid.box-primary>.box-header { 
	background:#666666
	color:#fff;
	}           
	.box-header .box-title {
	font-weight: bold;
	font-size: 14px;
	}
	.box.box-solid.box-primary {
 	border-bottom-color:#BEBEBE;
	border-left-color:#BEBEBE;
	border-right-color:#BEBEBE;
	border-top-color:#BEBEBE;
	}
	")),
          
     img(src = "logosmart2.png", width = "100%"),
      box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",   
          fileInput("file1", "Upload CSV file:", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
            shinyInput_label_embed(
              icon("info-circle") %>%
                bs_embed_popover(
                  title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                )
            )                                
     ), 


     box(width = "100%", title = span(icon("pen-to-square"), "Variable and parameters"), solidHeader = TRUE, status = "primary", 
         box(width = "100%", uiOutput('select1')),
         box(width = "100%",
             uiOutput('text1'),
    
checkboxInput("mean1", label = "Mean", value = TRUE),
checkboxInput("med1", label = "Median", value = TRUE),
checkboxInput("min1", label = "Minimum", value = TRUE),
checkboxInput("max1", label = "Maximum", value = TRUE),
checkboxInput("r1", label = "Range", value = TRUE),
checkboxInput("sd1", label = "Standard deviation", value = TRUE),
checkboxInput("var1", label = "Variance", value = TRUE),
checkboxInput("cv1", label = "Coefficient of variation (CV)", value = TRUE),
checkboxInput("q1", label = "1st Quartile", value = TRUE),
checkboxInput("q3", label = "3rd Quartile", value = TRUE),
checkboxInput("iqr1", label = "Inter Quartile Range (IQR)", value = TRUE))),
                                      
actionButton("getresult1", icon = icon("download"), "Get results", class = "btn-primary", width = "100%"), width = 3),
                                    
mainPanel(tags$style(HTML("
#	.tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}                                         
    	.tabbable > .nav > li > [data-value = 'Data viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Descriptive statistics'] {background-color:#6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
                                      
tabsetPanel(
  #  tabPanel("Data Preparation", imageOutput('page11'), align = "center"),
tabPanel("Data Viewer", verbatimTextOutput('datadetail1'), DT::dataTableOutput("datatable1"), tags$head(tags$style(HTML("#datadetail1{font-size: 16px;}")))),
tabPanel("Data Visualization", plotOutput('plot11'), align = "center"),
tabPanel("Descriptive Statistics", verbatimTextOutput('summary1'), tags$head(tags$style(HTML("#summary1{font-size: 16px;}")))),
),
width = 9)                                  
), 
                           
########################################################################## One Population Mean ###############################################################################
                           
tabPanel(h4("One Mean"),
                                    
sidebarPanel(
img(src = "logosmart2.png", width = "100%"),                                    
box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",    
fileInput("file2", "Upload CSV file:", multiple = FALSE,
   accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))%>%
  shinyInput_label_embed(
    icon("info-circle") %>%
      bs_embed_popover(
        title = "Data file in .csv format using UTF-8 encoding", placement = "right"
      )
  )               
), 

                                
box(width = "100%", title = span(icon("pen-to-square"), "Variable and parameters"), solidHeader = TRUE, status = "primary",  
    box(width = "100%", uiOutput('select2')),
    box(width = "100%",  radioButtons("typetest2", "Type of hypothesis:", choices = c("Two-sided" = "two2", "One-sided (less than)" = "less2", "One-sided (greater than)" = "great2")),                                  
   autonumericInput(inputId = "val2", label = "Hypothesized value:", decimalPlaces = 2, value = 0)),
   box(width = "100%",   sliderInput(inputId = "sig2", label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01))),
 actionButton("getresult2", "Get results", icon = icon("download"), class = "btn-primary", width = "100%"), 
width = 3),
                                    
mainPanel(
                                      tags$style(HTML("
  	.tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}                                         
    	.tabbable > .nav > li > [data-value = 'Data Viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Descriptive Statistics'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7; color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Assumption Checking'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Method Selection'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Hypothesis Testing'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
                                      
                                      tabsetPanel(
                                        tabPanel("Data Preparation", imageOutput('pageonemean'), align = "center"),
                                        tabPanel("Data Viewer", verbatimTextOutput('datadetail2'), DT::dataTableOutput("datatable2")),
                                        tabPanel("Data Visualization", plotOutput('plot2'), align = "center"),
                                        tabPanel("Descriptive Statistics", verbatimTextOutput('sumVar2'), tags$head(tags$style(HTML("#sumVar2{font-size: 16px;}")))),
                                        tabPanel("Assumption Checking",  verbatimTextOutput('testnorm2'), tags$head(tags$style(HTML("#testnorm2{font-size: 16px;}"))), plotOutput('plot21')),
                                        tabPanel("Method Selection", imageOutput('image2'), align = "center"),
                                        tabPanel("Hypothesis Testing", verbatimTextOutput('testmean2'), tags$head(tags$style(HTML("#testmean2{font-size: 16px;}"))))), 
                                      width = 9)
                                    
                           ), 
                           
######################################################################### Two-Population Means #########################################################################
                           
tabPanel(h4("Two Means"),
                                    
                                    sidebarPanel(
                                      img(src = "logosmart2.png", width = "100%"),
                                      box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",                                       
                                          fileInput("file3", "Upload CSV file:",
                                                    multiple = FALSE,
                                                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
                                            shinyInput_label_embed(
                                              icon("info-circle") %>%
                                                bs_embed_popover(
                                                  title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                                                )
                                            )                                
                                          ),   

                                     
                                      box(width = "100%", title = span(icon("pen-to-square"), "Variables and parameters"), solidHeader = TRUE, status = "primary",  
                                          box(width = "100%",    uiOutput('select31'),
                                          uiOutput('select32'),
                                          radioButtons("paired3", "Two populations are:", choices = c("Independent" = "ind3", "Paired" = "pair3"))),
                                          box(width = "100%", radioButtons("typetest3", "Type of hypothesis:", choices = c("Two-sided" = "two3", "One-sided (less than)" = "less3", "One-sided (greater than)" = "great3")),                             
                                          autonumericInput(inputId = "val3", label = "Hypothesized value:", decimalPlaces = 2, value = 0)),                                
                                          box(width = "100%", sliderInput(inputId = "sig3", label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01))),                                 
                                  
actionButton("getresult3", "Get results", icon = icon("download"), class = "btn-primary", width = "100%"), width = 3),
                                    
                                    mainPanel(
                                      tags$style(HTML("
  .tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}                                         
 	.tabbable > .nav > li > [data-value = 'Data Viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Descriptive Statistics'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7; color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Assumptions Checking'] {background-color: #6B8EB7;   color:white; font-size: 16px}
     	.tabbable > .nav > li > [data-value = 'Method Selection'] {background-color: #6B8EB7;   color:white; font-size: 16px}
      	.tabbable > .nav > li > [data-value = 'Hypothesis Testing'] {background-color: #6B8EB7;   color:white; font-size: 16px}
       	.tabbable > .nav > li[class = active] > a {background-color: #25476D ; color:white; font-size: 16px}
  	")),
                                      
                                      tabsetPanel(
                                        tabPanel("Data Preparation", imageOutput('pagetwomeans'), align = "center"),
                                        tabPanel("Data Viewer", verbatimTextOutput('datadetail3'), DT::dataTableOutput("datatable3")),
                                        tabPanel("Data Visualization", plotOutput('plot33'), align = "center"),
                                        tabPanel("Descriptive Statistics", verbatimTextOutput('sumVar3'), tags$head(tags$style(HTML("#sumVar3{font-size: 16px;}")))),
                                        tabPanel("Assumptions Checking", verbatimTextOutput('testnorm31'), tags$head(tags$style(HTML("#testnorm31{font-size: 16px;}"))), verbatimTextOutput('testnorm32'), tags$head(tags$style(HTML("#testnorm32{font-size: 16px;}"))), plotOutput('plot34')),
                                        tabPanel("Method Selection", imageOutput('imagetwopop'), align = "center"),
                                        tabPanel("Hypothesis Testing", verbatimTextOutput('testmean3'), tags$head(tags$style(HTML("#testmean3{font-size: 16px;}"))))
                                      ), width = 9)
                           ),  
                           
###################################################### More than Two-Population Means (One Factor) #############################################################
                           
                           tabPanel(h4("One-Way ANOVA"),
                                    
                                    sidebarPanel(
                                      img(src = "logosmart2.png", width = "100%"),	
                                      box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",                                       
                                          fileInput("file4", "Upload CSV file:",
                                                    multiple = FALSE,
                                                    accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))%>%
                                            shinyInput_label_embed(
                                              icon("info-circle") %>%
                                                bs_embed_popover(
                                                  title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                                                )
                                            )                                                    
                                      ), 
                                      
           
                                      box(width = "100%", title = span(icon("pen-to-square"), "Variables and parameters"), solidHeader = TRUE, status = "primary", 
                                          box(width = "100%",   uiOutput('select41'),
                                          uiOutput('select42')),
                                          box(width = "100%",    sliderInput(
                                            inputId = "sig4",
                                            label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01))),
                                     
                                      actionButton("getresult4", "Get results", icon = icon("download"), class = "btn-primary", width = "100%"), 
                                                                     
                                      width = 3),
                                    
                                    mainPanel(
                                      tags$style(HTML("
     .tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}                                    
    	.tabbable > .nav > li > [data-value ='Data Viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Descriptive Statistics'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Data Visualization'] {background-color: #6B8EB7; color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Assumptions Checking'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Method Selection'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Hypothesis Testing'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
                                      
                                      tabsetPanel(
                                        tabPanel("Data Preparation", imageOutput('pageanova'), align = "center"),
                                        tabPanel("Data Viewer", verbatimTextOutput('datadetail4'), DT::dataTableOutput("datatable4")),
                                        tabPanel("Data Visualization", plotOutput('plot41'), align = "center"),
                                        tabPanel("Descriptive Statistics", verbatimTextOutput('sumVar4'), tags$head(tags$style(HTML("#sumVar4{font-size: 16px;}")))),
                                        tabPanel("Assumptions Checking", verbatimTextOutput('testnorm4'), tags$head(tags$style(HTML("#testnorm4{font-size: 16px;}")))),
                                        tabPanel("Method Selection", imageOutput('imageanova'), align = "center"),
                                        tabPanel("Hypothesis Testing", verbatimTextOutput('testmean4'), tags$head(tags$style(HTML("#testmean4{font-size: 16px;}"))))
                                      ), width = 9)
                           ),                         
                           
############################################################################# Correlation ##################################################################################
                           
tabPanel(h4("Correlation"),
                                    
sidebarPanel(
     img(src = "logosmart2.png", width = "100%"),
      box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",           
                                          fileInput("file6", "Upload CSV file:",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv"))%>%
            shinyInput_label_embed(
              icon("info-circle") %>%
                bs_embed_popover(
                  title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                )
            )                
                                      ),
                                      
  box(width = "100%", title = span(icon("pen-to-square"), "Variables and parameters"), solidHeader = TRUE, status = "primary", 
  
  box(width = "100%", uiOutput('select61'),
                                          uiOutput('select62')),
                                          box(width = "100%", sliderInput(
                                            inputId = "sig6",
                                            label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01))),
                                      
                                      actionButton("getresult6", "Get results", icon = icon("download"), class = "btn-primary", width = "100%"),
 
                                      width = 3),
                                    
                                    mainPanel(
                                      tags$style(HTML("
      .tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}                                    
    	.tabbable > .nav > li > [data-value = 'Data Viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Descriptive Statistics'] {background-color: #6B8EB7;   color:white; font-size: 16px}
     	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7;   color:white; font-size: 16px}
.tabbable > .nav > li > [data-value ='Assumption Checking'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Method Selection'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Hypothesis Testing'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
                                      tabsetPanel(
                                        tabPanel("Data Preparation", imageOutput('pagecorrelation'), align = "center"),
                                        tabPanel("Data Viewer", DT::dataTableOutput("datatable6")),
                                        tabPanel("Data Visualization", plotOutput('plot61'), align = "center"),
                                        tabPanel("Descriptive Statistics", verbatimTextOutput('sumVar6'), tags$head(tags$style(HTML("#sumVar6{font-size: 16px;}")))),
                                        tabPanel("Assumption Checking", verbatimTextOutput('testnorm66'), tags$head(tags$style(HTML("#testnorm66{font-size: 16px;}"))), plotOutput('plot64')),
                                        tabPanel("Method Selection", imageOutput('imagecor'), align = "center"),
                                        tabPanel("Hypothesis Testing", verbatimTextOutput('cortest'), tags$head(tags$style(HTML("#cortest{font-size: 16px;}"))))
                                      ), width = 9)        
                           ),

######################################################################### Simple Regression ####################################################################################
tabPanel(h4("Simple Regression"),
         
         sidebarPanel(
           img(src = "logosmart2.png", width = "100%"),
           box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",           
               fileInput("filereg", "Upload CSV file:",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))%>%
                 shinyInput_label_embed(
                   icon("info-circle") %>%
                     bs_embed_popover(
                       title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                     )
                 )                
               
           ),
           

           box(width = "100%", title = span(icon("pen-to-square"), "Variables and parameters"), solidHeader = TRUE, status = "primary", 
               
               box(width = "100%", uiOutput('selectreg1'),
                   uiOutput('selectreg2')),
               box(width = "100%", sliderInput(
                 inputId = "sigreg",
                 label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01))),
           
           actionButton("getresultreg", "Get results", icon = icon("download"), class = "btn-primary", width = "100%"),
           
           width = 3),
         
         mainPanel(
           tags$style(HTML("
      .tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}                                    
    	.tabbable > .nav > li > [data-value = 'Data Viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Descriptive Statistics'] {background-color: #6B8EB7;   color:white; font-size: 16px}
     	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7;   color:white; font-size: 16px}
.tabbable > .nav > li > [data-value ='Assumptions Checking'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Transformations Checking'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value ='Model Selection'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Suggestion Model'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
           tabsetPanel(
            tabPanel("Data Preparation", imageOutput('pageregression'), align = "center"),
             tabPanel("Data Viewer", DT::dataTableOutput("datatablereg6")),
             tabPanel("Data Visualization", br(), plotlyOutput('regplot'), align = "center"),
             tabPanel("Descriptive Statistics", verbatimTextOutput('sumVarreg6'), tags$head(tags$style(HTML("#sumVarreg{font-size: 16px;}")))),
             tabPanel("Assumptions Checking", verbatimTextOutput('testnormres'), tags$head(tags$style(HTML("#testnormres{font-size: 16px;}"))), plotOutput('plotres')),
             tabPanel("Transformations Checking", verbatimTextOutput('modelsquare'), tags$head(tags$style(HTML("#modelsquare{font-size: 16px;}"))), plotOutput('plotres1'), verbatimTextOutput('modelsquareroot'), tags$head(tags$style(HTML("#modelsquareroot{font-size: 16px;}"))), plotOutput('plotres2'), verbatimTextOutput('modellog'), tags$head(tags$style(HTML("#modellog{font-size: 16px;}"))), plotOutput('plotres3')),
             tabPanel("Model Selection", imageOutput('imagereg'), align = "center"),
             tabPanel("Suggestion Model", verbatimTextOutput('testnormres2'), tags$head(tags$style(HTML("#testnormres2{font-size: 16px;}"))))
           ), width = 9)        
),

############################################################################# Chi-square test ##################################################################################                           
tabPanel(h4("Association"), 
         sidebarPanel(
           img(src = "logosmart2.png", width = "100%"),
           box(width = "100%", title = span(icon("folder-open"), "Data"), solidHeader = TRUE, status = "primary",           
               fileInput("file7", "Upload CSV file:",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))%>%
                 shinyInput_label_embed(
                   icon("info-circle") %>%
                     bs_embed_popover(
                       title = "Data file in .csv format using UTF-8 encoding", placement = "right"
                     )
                 )                
           ),
           
           box(width = "100%", title = span(icon("pen-to-square"), "Variables and parameters"), solidHeader = TRUE, status = "primary", 
               
               box(width = "100%", uiOutput('select71'),
                   uiOutput('select72')),
               box(width = "100%", sliderInput(
                 inputId = "sig7",
                 label = "Significance level:", min = 0.01, max = 0.10, value = 0.05, step = 0.01))),
           
           actionButton("getresult7", "Get results", icon = icon("download"), class = "btn-primary", width = "100%"),
       
           width = 3),
         
         mainPanel(
           tags$style(HTML("
   .tabbable > .nav > li > [data-value = 'Data Preparation'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Data Viewer'] {background-color: #6B8EB7;   color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Descriptive Statistics'] {background-color: #6B8EB7;   color:white; font-size: 16px}
 	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7;   color:white; font-size: 16px}
     .tabbable > .nav > li > [data-value ='Assumption Checking'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	  	.tabbable > .nav > li > [data-value ='Method Selection'] {background-color: #6B8EB7;  color:white; font-size: 16px}
    	.tabbable > .nav > li > [data-value = 'Hypothesis Testing'] {background-color: #6B8EB7; color:white; font-size: 16px}
  	")),
           tabsetPanel(
             tabPanel("Data Preparation", imageOutput('pageassociation'), align = "center"),
             tabPanel("Data Viewer", DT::dataTableOutput("datatable7")),
             tabPanel("Data Visualization", plotOutput('plotchi'), align = "center"),
             tabPanel("Descriptive Statistics", verbatimTextOutput('chitable'), tags$head(tags$style(HTML("#chitable{font-size: 16px;}")))),
            tabPanel("Assumption Checking", verbatimTextOutput('expected1'), tags$head(tags$style(HTML("#expected1{font-size: 16px;}")))),
            tabPanel("Method Selection", imageOutput('imagechi'), align = "center"),
            tabPanel("Hypothesis Testing", verbatimTextOutput('chitest'), tags$head(tags$style(HTML("#chitest{font-size: 16px;}"))))
           ), width = 9)  
         
),
                  
############################################################################# Sample size ##################################################################################                           
tabPanel(h4("Sample Size"),
  
  sidebarPanel(
           img(src = "logosmart2.png", height = "100%", width = "100%"),
            
           box(width = "100%", title = span(icon("pen-to-square"), "Hypothesis testing"), solidHeader = TRUE, status = "primary",
             radioButtons("test", "Select:", choices = c("One mean" = "onemean", "Two independent means" = "twoind", "Two dependent means" = "twodepen", "Correlation" = "corrn")),
                   
                   conditionalPanel(
                     condition = "input.test == 'onemean'",
                     box(width = "100%",
                     autonumericInput(inputId = "ref", withMathJax(sprintf("Reference value (\\(\\mu_0\\)):")), decimalPlaces = 2, value = 0),
                     autonumericInput(inputId = "meanmu", withMathJax(sprintf("Mean (\\(\\mu\\)):")), decimalPlaces = 2, value = 1),
                     autonumericInput(inputId = "sd", withMathJax(sprintf("S.D. (\\(\\sigma\\)):")), decimalPlaces = 2, value = 1),
                     sliderInput(inputId = "alpha1", label = "Significance level (\\(\\alpha\\)):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                     sliderInput(inputId = "beta1", label = "Prob. of type II error (\\(\\beta\\)):", min = 0.1, max = 0.50, value = 0.1, step = 0.1))
                   ),
                  
             conditionalPanel(
               condition = "input.test == 'twoind'",
               box(width = "100%",
                   autonumericInput(inputId = "mean1", withMathJax(sprintf("Mean of Group 1 (\\(\\mu_1\\)):")), decimalPlaces = 2, value = 1),
                   autonumericInput(inputId = "mean2", withMathJax(sprintf("Mean of Group 2 (\\(\\mu_2\\)):")), decimalPlaces = 2, value = 2),
                   autonumericInput(inputId = "sd1", withMathJax(sprintf("S.D. of Group 1 (\\(\\sigma_1\\)):")), decimalPlaces = 2, value = 1),
                   autonumericInput(inputId = "sd2", withMathJax(sprintf("S.D. of Group 2 (\\(\\sigma_2\\)):")), decimalPlaces = 2, value = 2),
                   autonumericInput(inputId = "ratio", withMathJax(sprintf("Ratio (\\(n_2 / n_1\\)):")), decimalPlaces = 2, value = 1),
                  sliderInput(inputId = "alpha2", label = "Significance level (\\(\\alpha\\)):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                  sliderInput(inputId = "beta2", label = "Prob. of type II error (\\(\\beta\\)):", min = 0.1, max = 0.50, value = 0.1, step = 0.1))
             ),
             
             conditionalPanel(
               condition = "input.test == 'twodepen'",
               box(width = "100%",
                   autonumericInput(inputId = "delta", withMathJax(sprintf("Mean difference (\\(\\Delta\\)):")), decimalPlaces = 2, value = 1),
                   autonumericInput(inputId = "sd3", withMathJax(sprintf("S.D. (\\(\\sigma\\)):")), decimalPlaces = 2, value = 1),
               sliderInput(inputId = "alpha3", label = "Significance level (\\(\\alpha\\)):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
               sliderInput(inputId = "beta3", label = "Prob. of type II error (\\(\\beta\\)):", min = 0.1, max = 0.50, value = 0.1, step = 0.1))
             ),
             
             conditionalPanel(
               condition = "input.test == 'corrn'",
               box(width = "100%",
                   autonumericInput(inputId = "samplecorr", withMathJax(sprintf("Expected correlation coefficient (r):")), decimalPlaces = 2, value = 0.2),
                   sliderInput(inputId = "alpha4", label = "Significance level (\\(\\alpha\\)):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                   sliderInput(inputId = "beta4", label = "Prob. of type II error (\\(\\beta\\)):", min = 0.1, max = 0.50, value = 0.1, step = 0.1))
             )),
                
               
               actionButton("calc", icon = icon("download"), "Calculation", class = "btn-primary", width = "100%"),
           
           width = 3),
  
         
  mainPanel(
    tags$style(HTML("
    	.tabbable > .nav > li > [data-value = 'Sample Size Report'] {background-color: #6B8EB7;   color:white; font-size: 16px}
 	.tabbable > .nav > li > [data-value = 'Data Visualization'] {background-color: #6B8EB7;   color:white; font-size: 16px}
  	")),
    
    tabsetPanel(
       tabPanel("Sample Size Report", verbatimTextOutput('n1'), tags$head(tags$style(HTML("#n1{font-size: 16px;}")))),
       tabPanel("Data Visualization", plotOutput('plottype2'), plotOutput('plotpower'), align = "center")
    ), width = 9)                                         

)

                ) 
                
) 

