#library(shiny)
library(shinyjs)

# Input values for alqpurpose
alqpurpose <- c("DNA extraction",
                "RNA extraction",
                "WB",
                "Protein mass spectrometry",
                "Metabolism",
                "Drug screen-Pilot67",
                "Drug screen-IC50",
                "Drug screen-VP5C1",
                "Drug screen-EMBL stepI",
                "Drug screen-EMBL stepII",
                "Drug screen-Synergy",
                "Drug screen-Phenotype 1000",
                "Drug screen-Cytokine",
                "Drug screen-Tri12 Combi",
                "ncRNA screen",
                "cell culture",
                "FACS viability",
                "MACS",
                "sent",
                "unknown")

fluidPage(
  
  useShinyjs(), 
  
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           position:fixed;
           top: calc(55%);;
           left: calc(50%);;")
      )),
  
  navbarPage(
    
    title = "Tumorbank", inverse = TRUE, id = "tabs",
    
    tabPanel("Biobank",
             #####
             tabsetPanel(
               
               tabPanel("Patients",
                        # ----
                        h3("Enter patient data"),
                        sidebarLayout(
                          sidebarPanel(
                            numericInput("patid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, textInput("patpatientid", "Patient ID*", NULL)),
                              column(6, textInput("patpseudoid", "HIPO ID", NULL))
                            ),
                            fluidRow(
                              column(6, uiOutput("patprjidref")),
                              column(6, uiOutput("patdgnidref"))
                            ),
                            fluidRow(
                              column(6, selectInput("patsex", "Gender", choices = c("", "f", "m") ))
                            ),
                            textInput("patcomment", "Comment", NULL),
                            h6("Mandatory fields are marked with *."),
                            br(),
                            fluidRow(
                              column(3, actionButton("submitPat", "Submit", class = "btn-primary")),
                              column(3, actionButton("resetPat", "Reset", class = "btn-success")),
                              column(3, actionButton("newPat", "New", class = "btn-warning")),
                              column(3, actionButton("deletePat", "Delete", class = "btn-danger"))
                            )
                          ),
                          
                          mainPanel(DT::dataTableOutput("tbl_pat"))
                        )
               ), # ----
               
               
               tabPanel("Samples",
                        # ----
                        h3("Enter sample data"),
                        sidebarLayout(
                          sidebarPanel(
                            numericInput("smpid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, uiOutput("smppatidref")),
                              column(6, textInput("smpsampleid", "Sample ID*", NULL))
                            ),
                            fluidRow(
                              column(6, dateInput("smpsampledate", "Sample date", "", format = "yyyy-mm-dd", weekstart = 1) ),
                              column(6, dateInput("smpdatereceived", "Date received", "", format = "yyyy-mm-dd", weekstart = 1) )
                            ),
                            fluidRow(
                              column(6, numericInput("smpleukocytes", "Leukocyte count", NULL)),
                              column(6, numericInput("smppblymphocytes", "%-PB-lymphocytes", NULL, min = 0, max = 100, step = 1))
                            ),
                            textInput("smpcomment", "Comment", NULL),
                            h6("Mandatory fields are marked with *."),
                            fluidRow(
                              column(3, actionButton("submitSmp", "Submit", class = "btn-primary") ),
                              column(3, actionButton("resetSmp", "Reset", class = "btn-success") ),
                              column(3, actionButton("newSmp", "New", class = "btn-warning")),
                              column(3, actionButton("deleteSmp", "Delete", class = "btn-danger") )
                            )
                          ),
                          mainPanel(DT::dataTableOutput("tbl_smp"))
                        )
                        # ----
               ),
               
               tabPanel("Aliquots",
                        # ----
                        h3("Enter aliquots"),
                        
                        sidebarLayout(
                          sidebarPanel(
                            numericInput("alqid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, uiOutput("alqsmpidref")),
                              column(6, selectInput("alqsampletype", "Sample type*", choices = c('', 'bone marrow', 'peripheral blood', 'lymph node', 'serum', 'saliva', 'DNA', 'RNA', 'protein'), selected = ''))
                            ),
                            fluidRow(
                              column(6, dateInput("alqdate", "Prepared on", "", format = "yyyy-mm-dd", weekstart = 1)),
                              column(6, uiOutput("alqusridref"))
                            ),
                            fluidRow(
                              column(6,
                                     conditionalPanel(
                                       condition = "input.alqsampletype == 'bone marrow' || input.alqsampletype == 'peripheral blood' || input.alqsampletype == 'lymph node' || input.alqsampletype == 'DNA' || input.alqsampletype == 'RNA'", 
                                       selectInput("alqcelltype", "Cell type", choices = c('', 'MNC viable', 'MNC pellet', 'CD19pos', 'CD19neg', 'CD3neg', 'CD3pos', 'granulocytes'))
                                     )
                              ),
                              column(6,
                                     conditionalPanel(
                                       condition = "input.alqsampletype == 'bone marrow' || input.alqsampletype == 'peripheral blood' || input.alqsampletype == 'lymph node'", 
                                       numericInput("alqcellnumber", "Cell number", NULL, min = 100000, max = 100000000, step = 1000)
                                     )
                              )
                            ),
                            
                            fluidRow(
                              column(6, 
                                     conditionalPanel(
                                       condition = "input.alqsampletype == 'serum' || input.alqsampletype == 'saliva' || input.alqsampletype == 'DNA' || input.alqsampletype == 'RNA'",
                                       numericInput("alqvolume", "Volume (in µl)", NULL, min = 100, max = 1500, step = 50)
                                     )
                              ),
                              column(6, 
                                     conditionalPanel(
                                       condition = "input.alqsampletype == 'DNA' || input.alqsampletype == 'RNA'",
                                       numericInput("alqconc", "Conc(ng/ul)", NULL)
                                     )
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(6, selectInput("alqstored", "Stored at", choices = c("", "USZ", "NCT", "USZ", "OMZ"), selected = "")),
                              column(6, selectInput("alqfreezer", "Freezer", choices = c("", "-20°C_1", "-80°C_1", "N2"), selected = ""))
                            ),
                            fluidRow(
                              column(4, numericInput("alqtower", "Tower/Rack", NULL)),
                              column(4, numericInput("alqbox", "Box", NULL)),
                              column(4, selectInput("alqposition", "Position", choices = c("", sort(paste0(rep(LETTERS[1:9], times = 9), rep(1:9, each = 9)))), selected = ""))
                            ),
                            checkboxInput("alqempty", "Empty", 0),
                            
                            hr(),
                            checkboxInput("alqused", "Show used fields"),
                            conditionalPanel(
                              condition = "input.alqused == true",
                              fluidRow(
                                column(6, dateInput("alqdateused", "Date of use", "", format = "yyyy-mm-dd", weekstart = 1)),
                                column(6, uiOutput("alqusedusridref"))
                              ),
                              selectInput("alqpurpose", "Used for", choices = c("", alqpurpose) )
                            ),
                            
                            hr(),
                            textInput("alqcomment", "Comment", NULL),
                            h6("Mandatory fields are marked with *."),
                            br(),
                            fluidRow(
                              column(4, actionButton("submitAlq", "Submit", class = "btn-primary") ),
                              column(4, actionButton("resetAlq", "Reset", class = "btn-success") ),
                              column(4, actionButton("deleteAlq", "Delete", class = "btn-danger") )
                            )
                          ),
                          mainPanel(DT::dataTableOutput("tbl_alq"))
                        )
                        # ----
               )
             )
    ),
    
    tabPanel("Analysis",
             # ----
             br(),
             tabsetPanel(
               
               tabPanel("Add new analysis",
                        
                        sidebarPanel(
                          
                          # samples
                          uiOutput("anlsmpidref"),
                          fileInput('anlSampleUpload', 'Upload excel file with sample IDs', accept = c('.xlsx', '.xls')),
                          hr(),
                          
                          # analysis
                          uiOutput("anlanuidref"),
                          selectInput("anlstatus", "Status*", choices = c( '', 'selected', 'submitted', 'screened', 'data received', 'failed'), selected = ''),
                          conditionalPanel(
                            condition = "input.anlstatus == 'submitted' | input.anlstatus == 'screened'",
                            dateInput("anldate", "Submission/screening date", "", format = "yyyy-mm-dd", weekstart = 1),
                            fluidRow(column(6, textInput("anlrun", "Run/Batch", "")),
                                     column(6, selectInput("anltype", "Type (C=control;T=tumor)", choices = c("", "C", "T", "C/T"), selected = "")))
                          ),
                          textInput("anlcomment", "Comment", ""),
                          h6("Mandatory fields are marked with *."),

                          br(),
                          fluidRow(
                            column(3, actionButton("submitAnl", "Submit", class = "btn-primary") ),
                            column(3, actionButton("resetAnl", "Reset", class = "btn-success") ),
                            column(3)
                          )
                          
                        ),
                        
                        mainPanel(tableOutput("tbl_smp_upload"))
                        
               ),
               
               tabPanel("Edit analysis",
                        
                        sidebarPanel(
                          
                          uiOutput("edit_anlanuidref"),
                          selectInput("edit_filter_anlstatus", "Current status", choices = c('', 'selected', 'submitted', 'screened', 'data received', 'failed'), selected = ''),
                          
                          h4("Update analysis status"),
                          numericInput("edit_anlid", "Auto-ID", NULL),
                          textInput("edit_anlsmpidref", "Sample ID", ""),
                          selectInput("edit_anlstatus",  "Status", choices = c('', 'selected', 'submitted', 'screened', 'data received', 'failed'), selected = ''),
                          dateInput("edit_anldate", "Submission date", "", format = "yyyy-mm-dd", weekstart = 1),
                          textInput("edit_anlrun", "Run/Batch", ""),
                          selectInput("edit_anltype", "Type", choices = c("", "C", "T", "C/T"), selected = ""),
                          textInput("edit_anlcomment", "Comment", ""),
                          h6("Mandatory fields are marked with *."),
                          
                          br(),
                          fluidRow(
                            column(3, actionButton("submitEditAnl", "Submit", class = "btn-primary") ),
                            column(3, actionButton("resetEditAnl", "Reset", class = "btn-success") ),
                            column(3, actionButton("deleteEditAnl", "Delete", class = "btn-danger"))
                          )
                          
                        ),
                        mainPanel(
                          DT::dataTableOutput("edit_tbl_anl")
                        )
                        
               )
               
             ) # End of tabsetPanel()
             
             # ----
    ),
    
    tabPanel("More",
             
             br(),
             
             tabsetPanel(
               tabPanel("Diagnosis",
                        # ----
                        sidebarLayout(
                          sidebarPanel(
                            id = "setDgn", # Needed to re-set input values to default
                            br(),
                            numericInput("dgnid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, textInput("dgnname", "Diagnosis*", NULL)),
                              column(6)
                            ),
                            textInput("dgnfullname", "Full name", NULL),
                            h6("Mandatory fields are marked with *."),
                            br(),
                            actionButton("submitDgn", "Submit", class = "btn-primary"),
                            actionButton("resetDgn", "Reset", class = "btn-success"),
                            actionButton("deleteDgn", "Delete", class = "btn-danger")
                          ),
                          mainPanel(br(), DT::dataTableOutput("tbl_dgn"))
                        )
                        # ----
               ),
               
               tabPanel("Projects",
                        # ----
                        sidebarLayout(
                          sidebarPanel(
                            id = "setPrj", # Needed to re-set input values to default 
                            br(),
                            numericInput("prjid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, textInput("prjname", "Project*", NULL)),
                              column(6)
                            ),
                            fluidRow(
                              column(6, selectInput("prjmaterial", "Material", choices = c("", "cells", "DNA", "RNA", "mixed"))),
                              column(6, textInput("prjdisease", "Disease", NULL))
                            ),
                            fluidRow(
                              column(6, textInput("prjfirstname", "Contact first name", NULL)),
                              column(6, textInput("prjlastname", "Contact last name", NULL))
                            ),
                            textInput("prjdepartment", "Department", NULL),
                            textInput("prjinstitute", "Institute", NULL),
                            fluidRow(
                              column(6, textInput("prjcity", "City", NULL)),
                              column(6, textInput("prjcountry", "Country", NULL))
                            ),
                            textInput("prjdescription", "Description", NULL),
                            h6("Mandatory fields are marked with *."),
                            br(),
                            fluidRow(
                              column(3, actionButton("submitPrj", "Submit", class = "btn-primary") ),
                              column(3, actionButton("resetPrj", "Reset", class = "btn-success") ),
                              column(3, actionButton("deletePrj", "Delete", class = "btn-danger") )
                            )
                          ),
                          mainPanel(br(), DT::dataTableOutput("tbl_prj"))
                        )
                        # ----
               ),
               
               tabPanel("Users",
                        # ----
                        sidebarLayout(
                          sidebarPanel(
                            id = "setUsr", # Needed to re-set input values to default 
                            br(),
                            numericInput("usrid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, textInput("usrinitials", "User*", NULL)),
                              column(6)
                            ),
                            fluidRow(
                              column(6, textInput("usrfirstname", "First name", NULL)),
                              column(6, textInput("usrlastname", "Last name", NULL))
                            ),
                            selectInput("usrposition", "Position", choices = c('','TA', 'Trainee', 'Master student', 'Medical student', 'PhD student', 'PostDoc') ),
                            fluidRow(
                              column(6, dateInput("usrstartdate", "Start date", "", format = "yyyy-mm-dd", weekstart = 1)),
                              column(6, dateInput("usrenddate", "End date", "", format = "yyyy-mm-dd", weekstart = 1) )
                            ),
                            h6("Mandatory fields are marked with *."),
                            br(),
                            fluidRow(
                              column(3, actionButton("submitUsr", "Submit", class = "btn-primary") ),
                              column(3, actionButton("resetUsr", "Reset", class = "btn-success") ),
                              column(3, actionButton("deleteUsr", "Delete", class = "btn-danger") )
                            )
                          ),
                          mainPanel(br(), DT::dataTableOutput("tbl_usr"))
                        )
                        # ----
               ),
               
               tabPanel("Analysis",
                        # ----
                        sidebarLayout(
                          
                          sidebarPanel(
                            id = "setAnu",
                            br(),
                            numericInput("anuid", "Auto-ID", NULL),
                            fluidRow(
                              column(6, textInput("anuname", "Analysis*", "")),
                              column(6, uiOutput("anuusridref"))
                            ),
                            textInput("anudescription", "Description", ""),
                            h6("Mandatory fields are marked with *."),
                            br(),
                            fluidRow(
                              column(3, actionButton("submitAnu", "Submit", class = "btn-primary") ),
                              column(3, actionButton("resetAnu", "Reset", class = "btn-success") ),
                              column(3, actionButton("deleteAnu", "Delete", class = "btn-danger") )
                            )
                          ),
                          
                          mainPanel(br(), DT::dataTableOutput("tbl_anu"))
                          
                        )
                        # ----
               )
             )
    )
    
    
  ) # NavbarPage
      ) # fluidPage
