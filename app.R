# Load libraries
library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(ggplot2)
library(car)
library(caret)
library(ggpubr)
library(plotly)
library(Hmisc)
library(stats)
library(knitr)
library(reshape2)
library(olsrr)
library(data.table)

# Read in data
ct <- read.csv("red_ct_data.csv")

# Aggregate data and only keep distinct results
newg<-aggregate(ct$Tot.Gross, by=list(Category = ct$EmplId.Empl.Rcd), FUN = sum)
a <- data.table(ct, key = "EmplId.Empl.Rcd")
b <- data.table(newg, key = "Category")
x <- a[b]
ct <- distinct(x, EmplId.Empl.Rcd, .keep_all = TRUE)

# Remove temporary dataholders
rm(a)
rm(b)
rm(newg)
rm(x)

# Rename columns
names(ct)[20] <- "Total.Gross"
names(ct)[9] <- "Salary"

ct$City <- factor(ifelse(ct$City == "Bridgeport", "Bridgeport",
                         ifelse(ct$City == "Cheshire", "Cheshire",
                                ifelse(ct$City == "Danbury", "Danbury",
                                       ifelse(ct$City == "Danielson", "Danielson",
                                              ifelse(ct$City == "Enfield", "Enfield",
                                                     ifelse(ct$City == "Farmington" | ct$City == "FARMINGTON", "Farmington",
                                                            ifelse(ct$City == "GROTON", "Groton",
                                                                   ifelse(ct$City == "Hartford" | ct$City == "HARTFORD" | ct$City == "East Hartford" | ct$City == "WEST HARTFORD", "Hartford",
                                                                          ifelse(ct$City == "Manchester", "Manchester",
                                                                                 ifelse(ct$City == "Middlesex", "Middlesex",
                                                                                        ifelse(ct$City == "Middletown", "Middletown",
                                                                                               ifelse(ct$City == "New Britain", "New Britain",
                                                                                                      ifelse(ct$City == "New Haven", "New Haven",
                                                                                                             ifelse(ct$City == "Newington", "Newington",
                                                                                                                    ifelse(ct$City == "Norwalk", "Norwalk",
                                                                                                                           ifelse(ct$City == "Norwich", "Norwich",
                                                                                                                                  ifelse(ct$City == "Rocky Hill", "Rocky Hill",
                                                                                                                                         ifelse(ct$City == "STAMFORD", "Stamford",
                                                                                                                                                ifelse(ct$City == "STORRS MANSFIELD", "Storrs Mansfield",
                                                                                                                                                       ifelse(ct$City == "Wallingford", "Wallingford",
                                                                                                                                                              ifelse(ct$City == "Waterbury" | ct$City == "WATERBURY", "Waterbury",
                                                                                                                                                                     ifelse(ct$City == "Wethersfield", "Wethersfield",
                                                                                                                                                                            ifelse(ct$City == "Willimantic", "Willimantic",
                                                                                                                                                                                   ifelse(ct$City == "Windsor Locks", "Windsor Locks", "Winsted")))))))))))))))))))))))))
# Create Career Area variable
ct$CarArea <- factor(ifelse(ct$Agency == "CCC - Asnuntuck" | ct$Agency == "CCC - Capital" |
                              ct$Agency == "CCC - Gateway" | ct$Agency == "CCC - Housatonic" |
                              ct$Agency == "CCC - Manchester" | ct$Agency == "CCC - Middlesex" |
                              ct$Agency == "CCC - Naugatuck Valley" | ct$Agency == "CCC - Northwestern" |
                              ct$Agency == "CCC - Norwalk" | ct$Agency == "CCC - Quinebaug Valley" |
                              ct$Agency == "CCC - Three Rivers" | ct$Agency == "CCC - Tunxis" |
                              ct$Agency == "Charter Oak State College" | 
                              ct$Agency == "Comm. College System Office" |
                              ct$Agency == "Connecticut Board of Regents" |
                              ct$Agency == "Connecticut State Universities" |
                              ct$Agency == "Office of Higher Education" |
                              ct$Agency == "State Dept of Education" |
                              ct$Agency == "Teachers' Retirment Board" |
                              ct$Agency == "CSU - Central" | 
                              ct$Agency == "CSU - Eastern" |
                              ct$Agency == "CSU - Southern" | 
                              ct$Agency == "CSU - Western" |
                              ct$Agency == "University of Connecticut", "Education",
                            ifelse(ct$Agency == "Agricultural Experiment Sta" |
                                     ct$Agency == "Dept of Energy & Environ Prot." |
                                     ct$Agency == "Dept. of Agriculture" |
                                     ct$Agency == "Envrionmental Quality Council",
                                   "Environmental",
                                   ifelse(ct$Agency == "Attorney General's Office" |
                                            ct$Agency == "Elections Enforcement" |
                                            ct$Agency == "Governor's Office" |
                                            ct$Agency == "Judicial Branch" |
                                            ct$Agency == "Legislative Management" |
                                            ct$Agency == "Lieutenant Governor's Office" |
                                            ct$Agency == "Office of Policy Management" |
                                            ct$Agency == "Office of State Ethics" |
                                            ct$Agency == "Probate Court Administration" |
                                            ct$Agency == "Secretary of the State", "Judicial",
                                          ifelse(ct$Agency == "Auditors of Public Accounts" |
                                                   ct$Agency == "Capital Rev Dev Authority" |
                                                   ct$Agency == "Connecticut Green Bank" |
                                                   ct$Agency == "Dept. of Banking" |
                                                   ct$Agency == "Dept. of Insurance" |
                                                   ct$Agency == "Dept. of Revenue Services" |
                                                   ct$Agency == "Office of the State Treasurer" |
                                                   ct$Agency == "Worker's Compensation Commiss.",
                                                 "Finance",
                                                 ifelse(ct$Agency == "Chief Medical Examiner's Offc" |
                                                          ct$Agency == "Dept. of Public Health" |
                                                          ct$Agency == "Mental Heath & Addiction Serv." |
                                                          ct$Agency == "Off of Healthcare Advocate" |
                                                          ct$Agency == "Psychiatric Security Review" |
                                                          ct$Agency == "UConn Health Center" |
                                                          ct$Agency == "Dept. of Rehab. Services", "Healthcare",
                                                        ifelse(ct$Agency == "Comm. Women Children & Seniors" |
                                                                 ct$Agency == "COMMISSION OF EQUITY AND OPPOR" |
                                                                 ct$Agency == "Department of Development Serv" |
                                                                 ct$Agency == "Dept of Administrative Svcs" |
                                                                 ct$Agency == "Dept of Social Services" |
                                                                 ct$Agency == "Dept. of Veterans' Affairs" |
                                                                 ct$Agency == "Econ. & Community Development" |
                                                                 ct$Agency == "Dept. of Children and Families" |
                                                                 ct$Agency == "Dept. of Labor" |
                                                                 ct$Agency == "Office of Early Childhood" |
                                                                 ct$Agency == "Human Rights & Opportunities",
                                                               "Social Services",
                                                               ifelse(ct$Agency == "Connecticut Lottery Corp." |
                                                                        ct$Agency == "Connecticut Siting Council" |
                                                                        ct$Agency == "Connecticut State Library" |
                                                                        ct$Agency == "CT Innovations Inc" |
                                                                        ct$Agency == "Dept. of Consumer Protection" |
                                                                        ct$Agency == "Freedom of Information" |
                                                                        ct$Agency == "Office of Consumer Counsel" |
                                                                        ct$Agency == "State Comptroller's Office" |
                                                                        ct$Agency == "State Department of Aging",
                                                                      "State Services",
                                                                      ifelse(ct$Agency == "CT Housing & Fin. Authority" |
                                                                               ct$Agency == "Department of Housing",
                                                                             "Housing",
                                                                             ifelse(ct$Agency == "CT Port Authority" |
                                                                                      ct$Agency == "CT Airport Authority (OPS)" |
                                                                                      ct$Agency == "Dept. of Motor Vehicles" |
                                                                                      ct$Agency == "Dept. of Transportation",
                                                                                    "Transportation",
                                                                                    ifelse(ct$Agency == "Dept. of Emrg Svc & Public Prot" |
                                                                                             ct$Agency == "Dept. of Correction" |
                                                                                             ct$Agency == "Division of Criminal Justice" |
                                                                                             ct$Agency == "Military Dept." |
                                                                                             ct$Agency == "Public Defender Services",
                                                                                           "Law Enforcement", "Other")))))))))))

# Take log of total gross
ct$logGross <- log(ct$Total.Gross)

# Subset data for use in linear model
tmp <- ct[,c(11,13,15:17:18,20:21)]

attach(tmp)

# Create test and training datasets
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(tmp), 0.8*nrow(tmp))  # row indices for training data
trainData <- tmp[trainingRowIndex, ]  # model training data
testData  <- tmp[-trainingRowIndex, ]

# Create log of Total.Gross
trainData$logGross <- log(trainData$Total.Gross)
testData$logGross <- log(testData$Total.Gross)

# Remove total.gross
trainData <- trainData[,c(1:6,8:9)]
testData <- testData[,c(1:6,8:9)]

# Create linear model based on suggested variables
mod <- lm(logGross ~ Age + City + Ethnic.Grp + Sex + CarArea + EE.Class.Descr + Union.Descr, data = trainData)

# Remove a union description value
trainData <- trainData[which(trainData$Union.Descr != "DivPublicDefSrvs - Statutory"),]
testData <- testData[which(testData$Union.Descr != "DivPublicDefSrvs - Statutory"),]

#------------------------------------------------------------------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- 
  dashboardPage(
    dashboardHeader(title = "CT Salary Calculator"),
    dashboardSidebar(
      numericInput('ageInput', 'Age', 35,
                   min = 20, max = 100),
      selectInput("ethnicInput", "Ethnicity",
                  choices = c("AMIND", "ASIAN", "BLACK", "HISPA", "NSPEC", "PACIF", "WHITE")),
      selectInput("classInput", "Job Time Status",
                  choices = c("Appointed Initial 6 Months", "Durational", "Emerg/ProvPending Opn Comp",
                              "Emergency 2 months or less", "Emergency Unclassified", "Graduate Assistants UCHC",
                              "Intermittent", "Judicial Temporary", "OLM Temp and Legislator", "Permanent", 
                              "Probation Initial Period Class", "Probation Initial Period Uncls", "Probation Period Classified",
                              "Probation Period Unclassified", "Prov Open Comp to 6 months", "Prov Pending Agency", "Seasonal",
                              "Special Appointment Higher Ed", "Student Laborer", "Temp 6 months or less", "Temp Day No End Date",
                              "Temp Serv in a Higher Class", "Tenured", "Trainee-Initial WTP complete", "UConn Pending Tenure",
                              "UConn Tenure pending Visa", "University Assistant CSU")),
      selectInput("cityInput", "City",
                  choices = c("Bridgeport", "Cheshire", "Danbury", "Danielson", "Enfield", "Farmington",
                              "Groton", "Hartford", "Manchester", "Middlesex", "Middletown", "New Britain", "New Haven",
                              "Newington", "Norwalk", "Norwich", "Rocky Hill", "Stamford", "Storrs Mansfield",
                              "Wallingford", "Waterbury", "Wethersfield", "Willimantic", "Windsor Locks", "Winsted")),
      selectInput("carInput", "Career Area",
                  choices = c("Education", "Environmental", "Finance", "Healthcare", "Housing", "Judicial", "Law Enforcement",
                              "Other", "Social Services", "State Services", "Transportation")),
      selectInput("unionInput", "Union",
                  choices = c("Admin and Residual (P-5)", "Administrative Clerical (NP-3)", "Amercan Fed of School Admin", 
                              "Assistant Attys General", "Auditors of Public Accounts", "Bd State Acad Awards Prof",
                              "Comm College Admin - AFSCME", "Comm College Admin - CCCC", 
                              "Comm College AFT Couns/Lib", "Comm College Confid Exclusions", "Comm College Faculty - AFT",
                              "Comm College Faculty CCCC", "Comm College Mgmt Exclusions", "Confidential",
                              "Conn Assoc Prosecutors", "Conn Development Authority", "Conn Lottery Exclusion",
                              "Connecticut Innovations Inc", "Correctional Officers (NP-4)" ,"Correctional Supervisor (NP-8)",
                              "Crim Justice Managerial Exempt", "Crim Justice Non-Mgmt Exempts", "Criminal Justice Inspectors",
                              "Criminal Justice Residual", "CSCU MC Exempt", "CT Housing and Finance Auth", "Deputy Wardens NP8Temp",
                              "DPDS Chief Public Defenders", "DPDS Exempt", "DPDS Public Defenders",
                              "DPDS Public Defenders - AFSCME", "DPDS Sprvsng Atty AFSCME", "Education A (P-3A)", "Education B (P-3B)",           
                              "Engineer  Scien  Tech (P-4)", "Exempt/Elected/Appointed", "Health NonProfessional (NP-6)", 
                              "Health Professional (P-1)", "Higher Ed - Confidential", "Higher Ed - Professional Emp",
                              "Judicial - Judicial Marshals", "Judicial - Law Clerks", "Judicial - Mgr and Conf", "Judicial - Non-Professional",
                              "Judicial - Professional", "Judicial - Supvr Jud Marshals", "Legislative Management", "Managerial",
                              "No Designated Unit", "Other Non-Bargaining", "Protective Services (NP-5)", "Service/Maintenance (NP-2)",
                              "Social and Human Services(P-2)", "St Vocation Federation Teacher", "State Police (NP-1)",
                              "State University Exempt", "State University Faculty", "State University Non-Fac Prof",
                              "StatePoliceLts&Captains (NP-9)", "Tax Attorneys A&RTemp", "UCHC - Exempt", "UCHC - Faculty",                
                              "UCHC - Faculty - AAUP", "UCHC Univ Hlth Professionals", "UConn - Exempt", "UConn - Faculty",              
                              "UConn - Law School Faculty", "UConn - Non-Faculty", "UHC- Faculty - Non-Bargaining" )),
      actionButton("genButton", "Predict Salary", style = "color: #000000; background-color: #42f44e; border-color: #2e6da4"),
      # Sidebar Menu
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard"),
        menuItem("Help", tabName = "help")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem("dashboard",
                fluidRow(
                  valueBoxOutput("femalepred", width = 3),
                  valueBoxOutput("malepred", width = 3),
                  valueBoxOutput("diff", width = 3),
                  valueBoxOutput("pctval", width = 3)
                ),
                fluidRow(
                  box(
                    title = "Distributions of Total Gross by Gender for Selected Career Area",
                    status = "primary", solidHeader = TRUE,
                    width = 12,
                    plotOutput("dens", width = "100%", height = 600)
                  )
                ),
                fluidRow(
                  box(
                    title = "Distributions of Ethnicity for Selected Career Area",
                    status = "primary", solidHeader = TRUE,
                    width = 12,
                    plotOutput("bp", width = "100%", height = 600)
                  )
                )
        ),
        tabItem("help",
                fluidRow(
                  mainPanel(
                    h1("Help Page"),
                    h3("Using the Calculator"),
                    p("Enter the credentials based on your desired position and then click submit salary to view the predicted salary for
                      an employee with the given credentials. The calculator displays both male and female predictions, along with the difference
                      between the two and the percentile in which the predicted employee is in for their respective career area."),
                    h3("Density Plot"),
                    p("The density plot shows the distribution of male and female salaries for the selected career area. The dashed red 
                      line indicates where the predicted salary falls."),
                    h3("Boxplot"),
                    p("The boxplot displays the distributions of salary broken down by ethnic group. The red dashed line displays where
                       the predicted salary falls among all ethinic groups.")
                  )
                ))
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  malepredvalue <- reactive({
    # Create new data frame with input variables
    temp <- as.data.frame(cbind(input$ageInput, input$cityInput, input$ethnicInput, "M", input$carInput, input$classInput,
                                input$unionInput))
    
    # Rename variables
    colnames(temp) <- c("Age", "City", "Ethnic.Grp", "Sex", "CarArea", "EE.Class.Descr", "Union.Descr")
    
    # Refactor age to be numeric
    temp$Age <- as.numeric(as.character(temp$Age))
    
    # Make individual prediction based on inputs
    ind.pred <- mod %>% predict(temp)
    
    # Undo log
    ind.pred <- round(exp(1)^(ind.pred))
  })
  
  femalepredvalue <- reactive({
    # Create new data frame with input variables
    temp <- as.data.frame(cbind(input$ageInput, input$cityInput, input$ethnicInput, "F", input$carInput, input$classInput,
                                input$unionInput))
    
    # Rename variables
    colnames(temp) <- c("Age", "City", "Ethnic.Grp", "Sex", "CarArea", "EE.Class.Descr", "Union.Descr")
    
    # Refactor age to be numeric
    temp$Age <- as.numeric(as.character(temp$Age))
    
    # Make individual prediction based on inputs
    ind.pred <- mod %>% predict(temp)
    
    # Undo log
    ind.pred <- round(exp(1)^(ind.pred))
  })
  
  output$dens <- renderPlot({
    # Run when button pressed
    input$genButton
    
    filtered <-
      ct %>%
      filter(CarArea == input$carInput)
    
    ggplot(filtered, aes(Total.Gross, fill = Sex)) +
      geom_density(alpha = 0.3) +
      geom_vline(aes(xintercept = isolate(femalepredvalue())), linetype="dashed", lwd=1.5, colour="red") +
      xlim(0,200000) +
      labs(x="Total Gross", y="Frequency")
    
  })
  
  output$bp <- renderPlot({
    # Run when button pressed
    input$genButton
    
    # Filter data by career area
    filtered <-
      ct %>%
      filter(CarArea == input$carInput)
    
    # Filter data by total gross
    filtered <- filtered[which(filtered$Total.Gross < 200000 & filtered$Ethnic.Grp != "unknown"),]
    
    ggplot(filtered, aes(Ethnic.Grp, Total.Gross, colour = Ethnic.Grp)) +
      geom_boxplot() +
      geom_hline(aes(yintercept = isolate(femalepredvalue())), linetype="dashed", lwd=1.5, colour="red") +
      labs(x="Ethnic Group", y="Total Gross")
  })
  
  output$femalepred <- renderValueBox({
    input$genButton
    
    valueBox(
      value = paste0("$",isolate(femalepredvalue())),
      icon = icon("dollar"),
      subtitle = "Predicted Female Salary"
    )
  })
  
  output$malepred <- renderValueBox({
    input$genButton
    
    valueBox(
      value = paste0("$",isolate(malepredvalue())),
      icon = icon("dollar"),
      subtitle = "Predicted Male Salary"
    )
  })
  
  output$diff <- renderValueBox({
    input$genButton
    
    valueBox(
      value = paste0("$",isolate(malepredvalue()-femalepredvalue())),
      icon = icon("dollar"),
      subtitle = "Difference in Salary"
    )
  })
  
  output$pctval <- renderValueBox({
    input$genButton
    
    valueBox(
      value = paste0(isolate(pct()),"%"),
      icon = icon("percent"),
      subtitle = "Salary Percentile"
    )
  })
  
  pct <- reactive({
    # Run whenever button is pressed
    input$genButton
    
    # Generate predicted salary
    pred <- isolate(femalepredvalue())
    
    # Filter data by career area
    filtered <-
      ct %>%
      filter(CarArea == input$carInput)
    
    # Create df of values less than prediction
    tmp <- filtered[which(filtered$Total.Gross < pred),]
    # Get number of obs less than pred
    tmpcnt <- as.numeric(count(tmp))
    # Get overall count
    cnt <- as.numeric(count(filtered))
    # Calculate percentile
    pct <- round((tmpcnt/cnt)*100)
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

