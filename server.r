library(shiny)
library(shinyjs)
library(DT)
library(RPostgreSQL)
library(pool)
library(dplyr)
library(stringr)
library(readxl)

# Connect to the database
# ----
pool <- dbPool(
  drv = dbDriver("PostgreSQL"),
  dbname = "test",
  host = "huber-vm01.embl.de",
  user = "admin",
  password = "bloodcancertumorbank"
)

onStop(function() {
  poolClose(pool)
})

# ----


# Get the tables
# ----

# 1. Function to get the column names
getColNames <- function(table) {
  qyr <- paste0("SELECT * FROM information_schema.columns WHERE table_name = '", table, "';")
  colNames <- dbGetQuery(pool, qyr)$column_name
}

# 2. Function to get the tables with exchanged fk
getTbl <- function(table) {
  if (table == "patient") {
    query <- dbGetQuery(pool, "SELECT p.patid, p.patpatientid, p.patpseudoid, pr.prjname, d.dgnname, p.patsex, p.patcomment
                        FROM patient p
                        LEFT OUTER JOIN diagnosis d ON p.patdgnidref = d.dgnid
                        LEFT OUTER JOIN project pr ON p.patprjidref = pr.prjid
                        ORDER BY p.patpatientid;")
  } else if (table == "sample") {
    query <- dbGetQuery(pool, "SELECT s.smpid, p.patpatientid, s.smpsampleid, s.smpsampledate, s.smpdatereceived,  
                        s.smpleukocytes, s.smppblymphocytes, s.smpcomment
                        FROM sample s
                        LEFT OUTER JOIN patient p ON s.smppatidref = p.patid
                        ORDER BY p.patpatientid, s.smpsampleid;")
  } else if (table == "analysis") {
    query <- dbGetQuery(pool,"SELECT a.anlid, s.smpsampleid, o.anuname, a.anlstatus, a.anldate, a.anlrun, a.anltype, a.anlcomment
                        FROM analysis a 
                        LEFT OUTER JOIN sample s ON a.anlsmpidref = s.smpid
                        LEFT OUTER JOIN analysislookup o ON a.anlanuidref = o.anuid
                        ORDER BY anuname;")
  } else if (table == "analysislookup") {
    query <- dbGetQuery(pool,"SELECT anuid, anuname, anudescription
                        FROM analysislookup
                        ORDER BY anuname;")
  } else if (table == "aliquot") {
    query <- dbGetQuery(pool,"SELECT a.alqid, s.smpsampleid, a.alqdate, users1.usrinitials AS prepared_by, a.alqsampletype, a.alqcelltype, a.alqcellnumber, a.alqvolume, a.alqconc, a.alqstored, a.alqfreezer, a.alqtower, a.alqbox, a.alqposition, a.alqempty, a.alqdateused, users2.usrinitials AS used_by, a.alqpurpose, a.alqcomment 
                        FROM aliquot a
                        LEFT OUTER JOIN sample s ON a.alqsmpidref = s.smpid
                        LEFT OUTER JOIN users AS users1 ON a.alqusridref = users1.usrid 
                        LEFT OUTER JOIN users AS users2 ON a.alqusedusridref = users2.usrid
                        ORDER BY alqid;")
  } else if (table == "diagnosis") {
    query <- dbGetQuery(pool,"SELECT dgnid, dgnname, dgnfullname
                        FROM diagnosis
                        ORDER BY dgnname;")
  } else if (table == "project") {
    query <- dbGetQuery(pool, "SELECT prjid, prjname, prjdisease, prjmaterial, prjfirstname, prjlastname, prjdepartment, prjinstitute, prjcity, prjcountry, prjdescription
                        FROM project
                        ORDER BY prjname;")
  } else if (table == "users") {
    query <- dbGetQuery(pool, "SELECT usrid, usrinitials, usrfirstname, usrlastname, usrposition, usrstartdate, usrenddate 
                        FROM users 
                        ORDER BY usrinitials;")
  }
  }

# 3. Generate a dataframe that matches the DB column names with more readible column names.
tblPat_fields <- getColNames("patient")
tblPat_names <- c("AutoID", "Patient ID", "Pseudonym", "Project", "Diagnosis", "Gender", "Comment")
tblPat_matchNames <- as.data.frame(cbind(Fields = tblPat_fields, Names = tblPat_names))

tblSmp_fields <- getColNames("sample")
tblSmp_names <- c("AutoID", "Patient ID", "Sample ID", "Date", "Received on", "Leukocyte count", "%-PB-lymphocytes", "Comment")
tblSmp_matchNames <- as.data.frame(cbind(Fields = tblSmp_fields, Names = tblSmp_names))

tblAlq_fields <- getColNames("aliquot")
tblAlq_names <- c("AutoID", "Sample ID", "Prepared on", "Prepared by", "Sample type", "Cell type", "Cell number", "Volume", "Concentration", "Stored at", "Freezer", "Tower/Rack", "Box", "Position", "Empty", "Used on", "Used by", "Used for", "Comment")
tblAlq_matchNames <- as.data.frame(cbind(Fields = tblAlq_fields, Names = tblAlq_names))

tblAnl_fields <- getColNames("analysis")
tblAnl_names <- c("AutoID", "Sample ID", "Analysis", "Status", "Date", "Run", "Specification", "Comment")
tblAnl_matchNames <- as.data.frame(cbind(Fields = tblAnl_fields, Names = tblAnl_names))

tblAnu_fields <- getColNames("analysislookup")
tblAnu_names <- c("AutoID", "Analysis", "Description")
tblAnu_matchNames <- as.data.frame(cbind(Fields = tblAnu_fields, Names = tblAnu_names))

tblDgn_fields <- getColNames("diagnosis")
tblDgn_names <- c("AutoID", "Diagnosis", "Diagnosis full name")
tblDgn_matchNames <- as.data.frame(cbind(Fields = tblDgn_fields, Names = tblDgn_names))

tblPrj_fields <- getColNames("project")
tblPrj_names <- c("AutoID", "Project", "Disease", "Material", "First name", "Last name", "Department", "Institute", "City", "Country", "Description")
tblPrj_matchNames <- as.data.frame(cbind(Fields = tblPrj_fields, Names = tblPrj_names))

tblUsr_fields <- getColNames("users")
tblUsr_names <- c("AutoID", "Initials", "First name", "Last name", "Position", "Start date", "End date")
tblUsr_matchNames <- as.data.frame(cbind(Fields = tblUsr_fields, Names = tblUsr_names))

tbl_matchNames <- unique(rbind(tblPat_matchNames, tblSmp_matchNames, tblAnl_matchNames, tblAnu_matchNames, tblAlq_matchNames, tblDgn_matchNames, tblPrj_matchNames, tblUsr_matchNames))
tbl_matchNames$Fields <- as.character(tbl_matchNames$Fields)
tbl_matchNames$Names <- as.character(tbl_matchNames$Names)
# ----


function(input, output, session) {
  
  # General functions
  
  # Close the shiny app when browser window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ## Reactive tables that can be updated when new data is added
  rvtbl <- reactiveValues()
  rvtbl$pat <- getTbl("patient")
  rvtbl$smp <- getTbl("sample")
  rvtbl$alq <- getTbl("aliquot")
  rvtbl$anl <- getTbl("analysis")
  rvtbl$dgn <- getTbl("diagnosis")
  rvtbl$prj <- getTbl("project")
  rvtbl$usr <- getTbl("users")
  rvtbl$anu <- getTbl("analysislookup")
  
  
  ## Define foreign key (fk) values
  fk <- reactiveValues(
    # Reactive values used in patient table:
    patprjidref = "", patdgnidref = "", 
    # Reactive values used in sample table
    smppatidref = "",
    # Reactive values used in aliquot table
    alqsmpidref = "", alqusridref = "", alqusedusridref = "",
    # Reactive values used in the analysis table:
    anlsmpidref = "", anlanuidref = "", anlpath = NULL 
  )
  
  ## Activate submit button when all mandatory fields are filled
  mandFilled <- function(mandatoryFields, btn, session) {
    mandatoryFilled <- vapply(mandatoryFields,
                              function(x) {
                                !is.null(input[[x]]) && input[[x]] != ""
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    # enable/disable the submit/delete button
    shinyjs::toggleState(id = btn, condition = mandatoryFilled)
  }
  
  
  # Tumorbank
  
  ## Patient
  # ----
  
  # Disable fields
  disable("patpatientid")
  disable("patid")
  hide("patid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled(c("patpatientid", "patprjidref"), "submitPat", session) })
  observe({ mandFilled("patid", "deletePat", session) })
  
  # Get reactive fk
  output$patprjidref <- renderUI({
    tbl <- rvtbl$prj
    value <- tbl[["prjname"]]
    selectInput("patprjidref", "Project*", c("", value), selected = fk$patprjidref)
  })
  
  output$patdgnidref <- renderUI({
    tbl <- rvtbl$dgn
    value <- tbl[["dgnname"]]
    selectInput("patdgnidref", "Diagnosis", c("", value), selected = fk$patdgnidref)
  })
  
  # Update input fields when row is selected
  updateInputsPat <- function(data, session) {
    fk$patprjidref <- data[["patprjidref"]]
    fk$patdgnidref <- data[["patdgnidref"]]
    updateNumericInput(session, "patid", value = as.integer(data[["patid"]]))
    updateTextInput(session, "patpatientid", value = data[["patpatientid"]])
    updateTextInput(session, "patpseudoid", value = data[["patpseudoid"]])
    updateSelectInput(session, "patsex", selected = data[["patsex"]])
    updateTextInput(session, "patcomment", value = data[["patcomment"]])
  }
  
  observeEvent(input$tbl_pat_rows_selected, {
    if (length(input$tbl_pat_rows_selected) > 0) {
      dataSelected <- rvtbl$pat[input$tbl_pat_rows_selected, ]
      updateInputsPat(dataSelected, session)
    } 
  })
  
  # RESET values
  resetInputPat <- function() {
    fk$patprjidref <- ""
    fk$patdgnidref <- ""
    updateTextInput(session, "patprjidref", value = fk$patprjidref)
    updateTextInput(session, "patdgnidref", value = fk$patdgnidref)
    setInputFields <- c("patid", "patpatientid", "patpseudoid", "patsex", "patcomment")
    for(i in setInputFields) {
      reset(i)
    }
  }
  
  observeEvent(input$resetPat, {
    resetInputPat()
  })
  
  # New patient
  observeEvent(input$newPat, {

    pat <- rvtbl$pat
    
    # If no patient is in the database yet, insert P0001, then add up
    if( length(pat$patpatientid) > 0) {
      NewPatPatientID <- sprintf("P%04d", max(as.numeric(str_sub(pat$patpatientid, -4))) +1)
    } else {
      NewPatPatientID <- "P0001"
    }
    
    # Empty all fields
    fk$patprjidref <- ""
    fk$patdgnidref <- ""
    setInputFields <- c("patid", "patpseudoid", "patsex", "patcomment")
    for(i in setInputFields) {
      reset(i)
    }
    
    # Add new patientID
    updateTextInput(session, "patpatientid", value = NewPatPatientID)
  })
  
  
  # DELETE a record
  ## Show modal when button deletePat is clicked
  observeEvent(input$deletePat, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      footer = tagList(modalButton("No"),
                       actionButton("deletePatYes", "Yes"))))
  })
  
  # DELETE a record after confirmation
  observeEvent(input$deletePatYes, {
    query <- sprintf("DELETE FROM patient WHERE patid = '%s';",
                     input$patid)
    dbGetQuery(pool, query)
    
    removeModal()
    resetInputPat()
    # Update patient table
    rvtbl$pat <- getTbl("patient")
  })
  
  
  # INSERT/UPDATE a record
  observeEvent(input$submitPat, {
    
    # Get all input values
    data <- sapply(getColNames("patient"), function(x) input[[x]], simplify = FALSE)
    data <- lapply(data, trimws, which = "both")

    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data)] <- "NULL"
    
    # Add '' around filled text fields
    dataChr <- c("patpatientid", "patpseudoid", "patsex", "patcomment")
    for(i in dataChr) {
      if(data[i] != "NULL") {
        data[i] <- paste0("'", data[i], "'")
      }  
    }
    
    # Exchange values with fk
    if(data[names(data) == "patprjidref"] != "NULL") {
      data[names(data) == "patprjidref"] <- rvtbl$prj$prjid[rvtbl$prj$prjname == data[["patprjidref"]] ]
    } 
    if(data[names(data) == "patdgnidref"] != "NULL") {
      data[names(data) == "patdgnidref"] <- rvtbl$dgn$dgnid[rvtbl$dgn$dgnname == data[["patdgnidref"]] ]
    } 
    
    # Separate autoID
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Get all data to check update values
    dataSelected <- rvtbl$pat[input$tbl_pat_rows_selected, ]
    
    
    # INSERT values, if autoID == NA, else UPDATE
    
    if( dataAutoID == "NULL" ) {
      insertNames <- paste0(names(dataInsert), collapse = ", ")
      insertValues <- paste0(unname(dataInsert), collapse = ", ")
      insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", "patient", insertNames, insertValues)
      print(insertQuery)
      dbGetQuery(pool, insertQuery)
      } else {
        updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
        updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID) )
        updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;", "patient", updateData, updateAutoID)
        print(updateQuery)
        dbGetQuery(pool, updateQuery)
      } 

    resetInputPat()
    rvtbl$pat <- getTbl("patient")

  })

  
  # Get output table
  output$tbl_pat <- DT::renderDataTable({
    rvtbl$pat
    pat <- rvtbl$pat
    
    if(nrow(pat)>0) {
      
      names(pat) <- tbl_matchNames$Names[match(names(pat),tbl_matchNames$Fields)]
      return(pat)
      
    } else NULL
  }, selection = "single", filter = 'top', rownames = FALSE, options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100), autoWidth = TRUE, columnDefs = list(list(visible = FALSE, targets = 0))))
  
  # ----
  
  ## Sample
  # ----
  
  # Disable autoID
  disable("smpid")
  disable("smppatidref")
  disable("smpsampleid")
  hide("smpid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled(c("smppatidref", "smpsampleid"), "submitSmp", session) })
  observe({ mandFilled(c("smppatidref"), "newSmp", session) })
  observe({ mandFilled("smpid", "deleteSmp", session) })
  
  # Get reactive fk inputUI value
  output$smppatidref <- renderUI({
    if( !is.na(input$patid)) {
      fk$smppatidref <- input$patpatientid
      disabled(textInput("smppatidref", "Patient ID*", fk$smppatidref))
    } else {
      disabled(textInput("smppatidref", "Patient ID*", NULL))
    }
  })
  
  # Generate sampleID for new samples
  observeEvent(input$newSmp, {

    year <- format(Sys.Date(), "%y")
    smp <- rvtbl$smp
    
    # For the first entry:
    if(nrow(smp) == 0){
      newID <- paste0(year, "S0001")
    } else {
      
      # For follow-up samples
      sampleid_max <- max(smp$smpsampleid)
      sampleid_max_year <- str_sub(sampleid_max, 1,2)
      sampleid_max_number <- as.numeric(str_sub(sampleid_max, 4,7))
      if(year == sampleid_max_year) {
        nextID <- sprintf("%04d", as.numeric(sampleid_max_number+1))
        newID <- paste0(year, "S", nextID)
      } else {
        newID <- paste0(year, "S0001")
      }
    } 

    updateTextInput(session, "smpsampleid", value = newID)
    
    setInputFields <- c("smpid", "smpsampledate", "smpdatereceived", "smpleukocytes", "smppblymphocytes", "smpcomment")
    for(i in setInputFields) {
      reset(i)
    }
    
  })
  
  # Update input fields when row is selected
  updateInputsSmp <- function(data, session) {
    updateNumericInput(session, "smpid", value = as.integer(data$smpid))
    updateTextInput(session, "smpsampleid", value = data$smpsampleid)
    updateDateInput(session, "smpsampledate", value = data$smpsampledate)
    updateDateInput(session, "smpdatereceived", value = data$smpdatereceived)
    updateNumericInput(session, "smpleukocytes", value = data$smpleukocytes )
    updateNumericInput(session, "smppblymphocytes", value = data$smppblymphocytes )
  }
  
  observeEvent(input$tbl_smp_rows_selected, {
    if( length(input$tbl_smp_rows_selected) > 0) {
      smp <- rvtbl$smp
      smpFiltered <- filter(smp, smppatidref == input$patid)
      dataSelected <- smpFiltered[input$tbl_smp_rows_selected, ]
      updateInputsSmp(dataSelected, session)
    }
  })
  
  # RESET values
  resetInputSmp <- function() {
    setInputFields <- c("smpid", "smpsampleid", "smpsampledate", "smpdatereceived", "smpleukocytes", "smppblymphocytes", "smpcomment")
    for(i in setInputFields) {
      reset(i)
    }
  }
  observeEvent(input$resetSmp, {
    resetInputSmp()
  })
  observeEvent(is.na(input$patid), {
    fk$smppatidref <- ""
    resetInputSmp()
  })
  
  # DELETE a record
  ## Show modal when button deletePat is clicked
  observeEvent(input$deleteSmp, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      h5("All associated aliquots and analysis entries will also be deleted. This action cannot be undone."),
      footer = tagList(modalButton("No"),
                       actionButton("deleteSmpYes", "Yes"))))
  })
  
  # DELETE a record after confirmation
  observeEvent(input$deleteSmpYes, {
    query <- sprintf("DELETE FROM sample WHERE smpid = '%s'", input$smpid)
    dbGetQuery(pool, query)
    
    removeModal()
    resetInputSmp()
    # Update sample table
    rvtbl$smp <- getTbl("sample")
  })
  
  
  # INSERT/UPDATE a record
  observeEvent(input$submitSmp, {
    
    # Get all input values
    data <- sapply(getColNames("sample"), function(x) input[[x]], simplify = FALSE)
    data <- lapply(data, trimws, which = "both")
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data) ] <- "NULL"
    
    # Remove empty date field
    if( length(data[["smpsampledate"]]) == 0 ) {
      data[["smpsampledate"]] <- "NULL"
    } else {
      data[["smpsampledate"]] <- as.character(data[["smpsampledate"]])
    }
    if( length(data[["smpdatereceived"]]) == 0 ) {
      data[["smpdatereceived"]] <- "NULL"
    } else {
      data[["smpdatereceived"]] <- as.character(data[["smpdatereceived"]])
    }
    
    # Exchange values with fk
    data[["smppatidref"]] <- as.character(rvtbl$pat$patid[match(data[["smppatidref"]], rvtbl$pat$patpatientid)])
    
    # Add '' around filled text fields
    dataChr <- c("smppatidref", "smpsampleid", "smpsampledate", "smpdatereceived", "smpcomment")
    for(i in dataChr) {
      if(data[i] != "NULL") {
        data[i] <- paste0("'", data[i], "'")
      }  
    }

    # Separate autoID
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Get all data to check update values
    dataSelected <- rvtbl$smp[input$tbl_smp_rows_selected, ]
    
    # INSERT values, if autoID == NA, else UPDATE
    if(dataAutoID == "NULL"){
      
      # Generate INSERT query
      insertNames <- paste0(names(dataInsert), collapse = ", ")
      insertValues <- paste0(dataInsert, collapse = ", ")
      insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", 
                             "sample",
                             insertNames,
                             insertValues)
      print(insertQuery)
      dbGetQuery(pool, insertQuery)
      
    } else {
      
      # Generate the UPDATE query
      updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
      updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID))
      updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;",
                             "sample",
                             updateData,
                             updateAutoID)
      print(updateQuery)
      dbGetQuery(pool, updateQuery)
      
    }
    
    resetInputSmp()
    rvtbl$smp <- getTbl("sample")
    
  })
  
  # Create output table
  output$tbl_smp <- DT::renderDataTable({
    smp <- rvtbl$smp
    
    if( !is.na(input$patid) & nrow(smp) > 0 ) {
      smpSelected <- smp %>%
        filter(smppatidref == input$patid)
      names(smpSelected) <- tbl_matchNames$Names[match(names(smpSelected),tbl_matchNames$Fields)]
      
      smpSelected

    } else NULL
    
  }, selection = "single", filter = 'top', rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = c(0)))))
  
  
  # ----
  
  ## Aliquot
  # -----
  disable("alqid")
  #hide("alqid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled(c("alqsmpidref", "alqsampletype"), "submitAlq", session) })
  observe({ mandFilled("alqid", "deleteAlq", session) })

  ## Get reactive FK input field
  output$alqsmpidref <- renderUI({
    fk$alqsmpidref <- input$smpsampleid
    disabled(textInput("alqsmpidref", "Sample ID*", fk$alqsmpidref))
  })
  
  output$alqusridref <- renderUI({
    usr <- rvtbl$usr[["usrinitials"]]
    selectInput("alqusridref", "Prepared by", c("", usr), selected = fk$alqusridref)
  })
  
  output$alqusedusridref <- renderUI({
    usr <- rvtbl$usr[["usrinitials"]]
    selectInput("alqusedusridref", "Used by", c("", usr), selected = fk$alqusedusridref)
  })
  
  # Update input fields when row is selected
  updateInputsAlq <- function(data, session) {
    usr <- rvtbl$usr
    fk$alqusridref <- usr$usrinitials[match(data[["alqusridref"]], usr$usrid)]
    fk$alqusedusridref <- usr$usrinitials[match(data[["alqusedusridref"]], usr$usrid)]
    
    updateNumericInput(session, "alqid", value = as.integer(data[["alqid"]]))
    updateSelectInput(session, "alqsampletype", selected = data[["alqsampletype"]])
    updateDateInput(session, "alqdate", value = data[["alqdate"]])
    updateSelectInput(session, "alqcelltype", selected = data[["alqcelltype"]])
    updateNumericInput(session, "alqcellnumber", value = data[["alqcellnumber"]] )
    updateNumericInput(session, "alqvolume", value = data[["alqvolume"]] )
    updateNumericInput(session, "alqconc", value = data[["alqconc"]] )
    updateSelectInput(session, "alqstored", selected = data[["alqstored"]])
    updateSelectInput(session, "alqfreezer", selected = data[["alqfreezer"]])
    updateNumericInput(session, "alqtower", value = data[["alqtower"]])
    updateNumericInput(session, "alqbox", value = data[["alqbox"]])
    updateTextInput(session, "alqposition", value = data[["alqposition"]])
    updateCheckboxInput(session, "alqempty", "Empty", 0)
    updateDateInput(session, "alqdateused", value = data[["alqdateused"]])
    updateSelectInput(session, "alqpurpose", selected = data[["alqpurpose"]])
    updateTextInput(session, "alqcomment", value = data[["alqcomment"]])
  }
  
  observeEvent(input$tbl_alq_rows_selected, {
    if( length(input$tbl_alq_rows_selected) > 0) {
      alq <- rvtbl$alq
      alqFiltered <- filter(alq, alqsmpidref == input$smpid)
      dataSelected <- alqFiltered[input$tbl_alq_rows_selected, ]
      updateInputsAlq(dataSelected, session)
    }
  })
  
  # RESET values
  resetInputAlq <- function() {
    fk$alqusridref <- ""
    fk$alqusedusridref <- ""
    
    alq_cols <- getColNames("aliquot")
    setInputFields <- alq_cols[!alq_cols %in% c("alqsmpidref", "alqusridref", "alqusedusridref")]
    for(i in setInputFields) {
      reset(i)
    }
  }
  observeEvent(input$resetAlq, {
    resetInputAlq()
  })
  observeEvent(is.na(input$smpid), {
    fk$alqsmpidref <- ""
    resetInputAlq()
  })
  
  # INSERT/UPDATE a record
  observeEvent(input$submitAlq, {
    
    # Get all input values
    data <- sapply(getColNames("aliquot"), function(x) input[[x]], simplify = FALSE)
    data <- lapply(data, trimws, which = "both")
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data) ] <- "NULL"
    
    # Remove empty date field
    if( length(data[["alqdate"]]) == 0 ) {
      data[["alqdate"]] <- "NULL"
    } else {
      data[["alqdate"]] <- as.character(data[["alqdate"]])
    }
    if( length(data[["alqdateused"]]) == 0 ) {
      data[["alqdateused"]] <- "NULL"
    } else {
      data[["alqdateused"]] <- as.character(data[["alqdateused"]])
    }
    # If alqusridref is empty, it is returned as "" and exchanged with "NULL" from the function before. However, alqusedusridref returns as character(0) and will therefore be exchanged with this function.
    if( length(data[["alqusedusridref"]]) == 0 ) {
      data[["alqusedusridref"]] <- "NULL"
    } else {
      data[["alqusedusridref"]] <- as.character(data[["alqusedusridref"]])
    }
    
    # Exchange values with fk
    data[["alqsmpidref"]] <- as.character(rvtbl$smp$smpid[match(data[["alqsmpidref"]], rvtbl$smp$smpsampleid)])
    
    if( data[["alqusridref"]] != "NULL" ) {
      data[["alqusridref"]] <- rvtbl$usr$usrid[match(data[["alqusridref"]], rvtbl$usr$usrinitials)]
    }
    
    if( data[["alqusedusridref"]] != "NULL" ) {
      data[["alqusedusridref"]] <- rvtbl$usr$usrid[match(data[["alqusedusridref"]], rvtbl$usr$usrinitials)]
    }
    
    # Add '' around filled text fields
    dataChr <- c("alqdate", "alqsampletype", "alqcelltype", "alqstored", "alqfreezer", "alqposition", "alqdateused", "alqpurpose", "alqcomment")
    for(i in dataChr) {
      if(data[i] != "NULL") {
        data[i] <- paste0("'", data[i], "'")
      }  
    }
    # Separate autoID
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Get all data to check update values
    dataSelected <- rvtbl$alq[input$tbl_alq_rows_selected, ]
    
    # INSERT values, if autoID == NA, else UPDATE
    if(dataAutoID == "NULL"){
      
      # Generate INSERT query
      insertNames <- paste0(names(dataInsert), collapse = ", ")
      insertValues <- paste0(dataInsert, collapse = ", ")
      insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", 
                             "aliquot",
                             insertNames,
                             insertValues)
      print(insertQuery)
      dbGetQuery(pool, insertQuery)
      
    } else {
      
      # Generate the UPDATE query
      updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
      updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID))
      updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;",
                             "aliquot",
                             updateData,
                             updateAutoID)
      print(updateQuery)
      dbGetQuery(pool, updateQuery)
      
    }
    
      fk$alqusedusridref <- ""
      setInputFields <- c("alqid", "alqposition", "alqempty", "alqdateused",  "alqpurpose", "alqcomment")
      for(i in setInputFields) {
        reset(i)
      }
    rvtbl$alq <- getTbl("aliquot")

  })
  
  # Create output table
  output$tbl_alq <- DT::renderDataTable({
    
    alq <- rvtbl$alq
    usr <- rvtbl$usr
    
    if( !is.na(input$smpid) & nrow(alq) > 0 ) {
      alqSelected <- alq %>%
        filter(alqsmpidref == input$smpid) 
      names(alqSelected) <- tbl_matchNames$Names[match(names(alqSelected),tbl_matchNames$Fields)]
      
      alqSelected
      
    } else NULL
    
  }, selection = "single", filter = 'top', rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = c(0)))))
  
  # -----
  
  
  # Analysis
  # ----
  
  # New analysis
  observe({ mandFilled(c("anlsmpidref", "anlanuidref", "anlstatus"), "submitnAnl", session) })
  
  ## UI output
  output$anlsmpidref <- renderUI({
    tbl <- dbGetQuery(pool, "SELECT smpsampleid FROM sample;")
    values <- tbl[["smpsampleid"]]
    selectInput("anlsmpidref", "Select sample IDs", choices = c('', values), multiple = TRUE, selected = "")
  })
  
  output$anlanuidref <- renderUI({
    tbl <- rvtbl$anu
    values <- sort(tbl[["anuname"]])
    selectInput("anlanuidref", "Analysis*", c("", values), selected = "")
  })
  
  # Get file input & reset file input
  observeEvent(input$anlSampleUpload, {
    fk$anlpath <- 'uploaded'
  })
  
  observeEvent(input$resetAnl, {
    fk$anlpath <- 'reset'
  })
  
  input_filepath <- reactive({
    if (is.null(fk$anlpath)) {
      return(NULL)
    } else if (fk$anlpath == 'uploaded') {
      return(input$anlSampleUpload)
    } else if (fk$anlpath == 'reset') {
      return(NULL)
    }
  })
  
  # Combine select samples and input samples
  combine_samples <- function() {
    # Get samples from input field
    input_select <- input$anlsmpidref
    input_filepath <- input_filepath()
    input_file <- if(!is.null(input_filepath)) read_excel(input_filepath$datapath)
    input_samples <- c(input_select, input_file[[1]])
    
    if(!is.null(input_samples)) {
      tbl <- as.data.frame(input_samples)
      names(tbl) <- "sampleID"
      tbl
    } else NULL
    
  }
  
  # RESET values
  resetInputAnl <- function() {
    setInputFields <- c("anlsmpidref", "anlSampleUpload", "anlanuidref", "anlstatus", "anldate", "anlrun", "anltype", "anlcomment")
    for(i in setInputFields) {
      reset(i)
    }
  }
  
  observeEvent(input$resetAnl, {
    resetInputAnl()
  })
  
  # Construct the INSERT query
  observeEvent(input$submitAnl, {
    
    # Get the sample IDs
    anlUpload <- combine_samples()
    
    if(!is.null(anlUpload)) {
      smp <- dbGetQuery(pool, "SELECT smpid, smpsampleid FROM sample;")
      anu <- dbGetQuery(pool, "SELECT anuid, anuname FROM analysislookup;")
      
      if(all(anlUpload$sampleID %in% smp$smpsampleid)) {
        
        # Add analysis and status
        anlUpload$analysis <- input$anlanuidref
        anlUpload$anlstatus <- input$anlstatus
        anlUpload$anldate <- ifelse(length(input$anldate)==0, "NULL", paste0("'", input$anldate, "'"))
        
        # Exchange fk
        anlUpload$anlsmpidref <- smp$smpid[match(anlUpload[,1], smp$smpsampleid)]
        anlUpload$anlanuidref <- anu$anuid[match(anlUpload$analysis, anu$anuname)]
        
        # Add '' around filled text fields
        anlUpload$anlstatus <- paste0("'", anlUpload$anlstatus, "'")

        # Get input values
        anlUpload <- select(anlUpload, anlsmpidref, anlanuidref, anlstatus, anldate)
        insertNames <- paste0(names(anlUpload), collapse = ", ")
        
        anlUpload <- mutate(anlUpload, combined = do.call(paste, c(anlUpload, sep = ", ")) )
        insertValues <- paste0("(", anlUpload$combined, ")", collapse = ", ")
        
        insertQuery <- sprintf("INSERT INTO %s (%s) VALUES %s;",
                               "analysis",
                               insertNames,
                               insertValues)
        dbGetQuery(pool, insertQuery) # Run the insert query
        resetInputAnl() # Reset the input fields
        fk$anlpath <- 'reset'
        rvtbl$anu <- getTbl("analysislookup")

      } else {
        falseSampleID <- anlUpload$sampleID[!anlUpload$sampleID %in% smp$smpsampleid]
        showNotification(paste0("Error: The following sample IDs do not exist: ", paste0(falseSampleID, collapse = ", ")), type  = "error", duration = NULL)
      }
      
    } else return(NULL)
  })
  
  # Show table as a control
  output$tbl_smp_upload <- renderTable({
    combine_samples()
  }, filter = 'top', rownames = FALSE)
  
  
  # Edit analysis
  
  # Disable fields
  disable("edit_anlid")
  hide("edit_anlid")
  disable("edit_anlsmpidref")
  
  observe({ mandFilled(c("edit_anlsmpidref"), "submitEditAnl", session) })
  observe({ mandFilled("edit_anlid", "deleteEditAnl", session) })
  

  # Get UI output
  output$edit_anlanuidref <- renderUI({
    # When reactive table is updated (after delete or submit) ...
    rvtbl$anu
    tbl <- rvtbl$anu
    values <- tbl[["anuname"]]
    selectInput("edit_anlanuidref", "Analysis", c("", values), selected = "")
  })
  
  # UPDATE input fields
  observeEvent(input$edit_tbl_anl_rows_selected, {
    if(length(input$edit_tbl_anl_rows_selected) > 0) {
      anl <- rvtbl$anl
      anl$anlanuidref <- rvtbl$anu$anuname[match(anl$anlanuidref, rvtbl$anu$anuid)] 
      anl_filtered <- filter(anl, anlanuidref == input$edit_anlanuidref, anlstatus == input$edit_filter_anlstatus)
      anl_selected <- anl_filtered[input$edit_tbl_anl_rows_selected, ]

      updateNumericInput(session, "edit_anlid", value = as.integer(anl_selected$anlid))
      updateTextInput(session, "edit_anlsmpidref", value = anl_selected$smpsampleid)
      fk$anlanuidref <- anl_selected$anuname
      updateSelectInput(session, "edit_anlstatus", selected = anl_selected$anlstatus)
      updateDateInput(session, "edit_anldate", value = anl_selected$anldate)
      updateTextInput(session, "edit_anlrun", value = anl_selected$anlrun)
      updateSelectInput(session, "edit_anltype", selected = anl_selected$anltype)
      updateTextInput(session, "edit_anlcomment", value = anl_selected$anlcomment)
    }
  })
  
  # RESET input fields
  resetInputEditAnl <- function() {
    setInputFields <- c("edit_anlid", "edit_anlsmpidref", "edit_anlstatus", "edit_anldate", "edit_anlrun", "edit_anltype", "edit_anlcomment")
    for(i in setInputFields) {
      reset(i)
    }
  }
  observeEvent(input$resetEditAnl, {
    resetInputEditAnl()
  })
  
  # DELETE a record
  ## Show modal when button deletePat is clicked
  observeEvent(input$deleteEditAnl, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      footer = tagList(modalButton("No"),
                       actionButton("deleteEditAnlYes", "Yes"))))
  })
  
  # DELETE a record after confirmation
  observeEvent(input$deleteEditAnlYes, {
    query <- sprintf("UPDATE analysis SET anldeleted = 'yes' WHERE anlid = '%s'", input$edit_anlid)
    dbGetQuery(pool, query)
    removeModal()
    resetInputEditAnl()
    rvtbl$anl <- getTbl("analysis")
  })
  
  # UPDATE a record
  observeEvent(input$submitEditAnl, {
    
    # Get values from the input fields
    anlColNames <- c("edit_anlid", "edit_anlsmpidref", "edit_anlanuidref", "edit_anlstatus", "edit_anldate", "edit_anlrun", "edit_anltype", "edit_anlcomment")
    data <- sapply(anlColNames, function(x) input[[x]], simplify = FALSE)
    names(data) <- gsub("edit_", "", names(data))
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data)] <- "NULL"
    
    # Remove empty date fields
    if (length(data[["anldate"]]) == 0 ) {
      data[["anldate"]] <- "NULL"
    } else {
      data[["anldate"]] <- as.character(data[["anldate"]])
    }
    
    # Add '' around filled text fields
    dataChr <- c("anlstatus", "anldate", "anlrun", "anltype", "anlcomment")
    for(i in dataChr) {
      if(data[i] != "NULL") {
        data[i] <- paste0("'", data[i], "'")
      }
    }
    
    # Exchange fk values with auto-ID
    ## Sample table
    dataSmp <- dbGetQuery(pool, "SELECT smpid, smpsampleid FROM sample;")
    data[names(data)=="anlsmpidref"] <- as.character(dataSmp$smpid[match(data[names(data)=="anlsmpidref"], dataSmp$smpsampleid)])
    
    ## Analysis lookup table
    dataAnu <- dbGetQuery(pool, "SELECT anuid, anuname FROM analysislookup;")
    data[names(data) == "anlanuidref"] <- as.character(dataAnu$anuid[match(data[names(data)=="anlanuidref"], dataAnu$anuname)])
    
    # Get filled fields without autoID and construct INSERT/UPDATE SQL syntax
    dataFilled <- data[!names(data) == "anlid"]
    dataAutoID <- data[names(data) == "anlid"]
    
    updateData <- paste0(names(dataFilled), " = ", dataFilled, collapse = ", ")
    updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID))
    updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;",
                           "analysis",
                           updateData,
                           updateAutoID)
      dbGetQuery(pool, updateQuery)
      resetInputEditAnl()
      rvtbl$anl <- getTbl("analysis")

  })
  
  # Get analysis table
  output$edit_tbl_anl <- DT::renderDataTable({
    if(!is.null(input$edit_filter_anlstatus) & !is.null(input$edit_anlanuidref)) {
      anl <- rvtbl$anl
      
      anl_filtered <- anl %>%
        filter(anlanuidref == input$edit_anlanuidref, anlstatus == input$edit_filter_anlstatus) %>%
        arrange(anlsmpidref)
      names(anl_filtered) <- tbl_matchNames$Names[match(names(anl_filtered), tbl_matchNames$Fields)]
      return(anl_filtered)
    } else NULL
    
  }, selection = "single", filter = 'top', rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = c(0)))))
  
  # ----
  
  # More
  
  # Diagnosis
  # ----
  
  # Disable fields
  disable("dgnid")
  hide("dgnid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled("dgnname", "submitDgn", session) })
  observe({ mandFilled("dgnid", "deleteDgn", session) })
  
  # Update input fields when row is selected
  updateInputsDgn <- function(data, session) {
    updateNumericInput(session, "dgnid", value = as.integer(data[["dgnid"]]))
    updateTextInput(session, "dgnname", value = data[["dgnname"]])
    updateTextInput(session, "dgnfullname", value = data[["dgnfullname"]])
  }
  observeEvent(input$tbl_dgn_rows_selected, {
    if (length(input$tbl_dgn_rows_selected) > 0) {
      dataSelected <- rvtbl$dgn[input$tbl_dgn_rows_selected, ]
      updateInputsDgn(dataSelected, session)
    }
  })
  
  ## RESET values
  observeEvent(input$resetDgn, {
    shinyjs::reset("setDgn")
  })
  
  ## DELETE a record
  ### Show modal when button deletePat is clicked
  observeEvent(input$deleteDgn, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      
      footer = tagList(modalButton("No"),
                       actionButton("deleteDgnYes", "Yes"))))
  })
  
  ### DELETE a record after confirmation
  observeEvent(input$deleteDgnYes, {
    #pat <- dbGetQuery(pool, "SELECT * FROM patient;")
    if(input$dgnid %in% rvtbl$pat$patdgnidref) {
      showNotification("Error: There are still patients assigned to this diagnosis! Change the diagnosis for all patients with this diagnosis first.", type = "error", duration = NULL)
    } else {
      query <- sprintf("DELETE FROM diagnosis WHERE dgnid = '%s'",
                       input$dgnid)
      dbGetQuery(pool, query)
      
      # Update diagnosis table
      rvtbl$dgn <- getTbl("diagnosis")
      shinyjs::reset("setDgn")
    }
    removeModal()
    
  })
  
  
  # INSERT/UPDATE a record
  observeEvent(input$submitDgn, {
    
    # Get input data and trim whitespaces
    data <- sapply(getColNames("diagnosis"), function(x) input[[x]], simplify = FALSE)
    data <- lapply(data, trimws, which = "both")
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data)] <- "NULL"
    
    # Separate auto-ID
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Add '' around filled text fields
    dataChr <- c("dgnname", "dgnfullname")
    for(i in dataChr) {
      if(dataInsert[i] != "NULL") {
        dataInsert[i] <- paste0("'", dataInsert[i], "'")
      }  
    }

    # Get all data to check update values
    dataSelected <- rvtbl$dgn[input$tbl_dgn_rows_selected, ]
    
    # INSERT values, if autoID == "NULL", else UPDATE
    if( dataAutoID == "NULL" ) {
      
      if( toupper(data[["dgnname"]]) %in% toupper(rvtbl$dgn$dgnname) | toupper(data[["dgnname"]]) %in% rvtbl$dgn$dgnname) {
        showNotification("Error: This value alread exists!", type  = "error", duration = NULL)
        updateInputsDgn(dataSelected, session)
      } else {
        insertNames <- paste0(names(dataInsert), collapse = ", ")
        insertValues <- paste0(unname(dataInsert), collapse = ", ")
        insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", "diagnosis", insertNames, insertValues)
        dbGetQuery(pool, insertQuery)
      }
    } else {
      if( !toupper(data[["dgnname"]]) %in% toupper(rvtbl$dgn$dgnname) | nrow(rvtbl$dgn[rvtbl$dgn$dgnid == input$dgnid & toupper(rvtbl$dgn$dgnname) == toupper(input$dgnname), ]) > 0) {
        updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
        updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID) )
        updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;", "diagnosis", updateData, updateAutoID)
        dbGetQuery(pool, updateQuery)
        
      } else {
        showNotification("Error: This value alread exists!", type  = "error", duration = NULL)
        updateInputsDgn(dataSelected, session)
        
      }
    }
    
    shinyjs::reset("setDgn")
    rvtbl$dgn <- getTbl("diagnosis")

  })
  
  # Create output table
  output$tbl_dgn <- DT::renderDataTable({
    tbl <- rvtbl$dgn
    names(tbl) <- tbl_matchNames$Names[match(names(tbl),tbl_matchNames$Fields)]
    tbl
  }, select = "single", filter = "top", rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0))) )
  
  # ----
  
  
  # Project
  # ----
  
  # Disable fields
  disable("prjid")
  hide("prjid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled("prjname", "submitPrj", session)})
  observe({ mandFilled("prjid", "deletePrj", session)})
  
  # Update input fields when row is selected
  updateInputsPrj <- function(data, session) {
    updateNumericInput(session, "prjid", value = as.integer(data[["prjid"]]) )
    updateTextInput(session, "prjname", value = data[["prjname"]])
    updateSelectInput(session, "prjmaterial", selected = data[["prjmaterial"]])
    updateTextInput(session, "prjdisease", value = data[["prjdisease"]])
    updateTextInput(session, "prjfirstname", value = data[["prjfirstname"]])
    updateTextInput(session, "prjlastname", value = data[["prjlastname"]])
    updateTextInput(session, "prjdepartment", value = data[["prjdepartment"]])
    updateTextInput(session, "prjinstitute", value = data[["prjinstitute"]])
    updateTextInput(session, "prjcity", value = data[["prjcity"]])
    updateTextInput(session, "prjcountry", value = data[["prjcountry"]])
    updateTextInput(session, "prjdescription", value = data[["prjdescription"]])
  }
  observeEvent(input$tbl_prj_rows_selected, {
    if (length(input$tbl_prj_rows_selected) > 0) {
      dataSelected <- rvtbl$prj[input$tbl_prj_rows_selected, ]
      updateInputsPrj(dataSelected, session)
    }
  })
  
  # RESET values
  observeEvent(input$resetPrj, {
    shinyjs::reset("setPrj")
  })
  
  # DELETE a record
  ## Show modal when button deletePat is clicked
  observeEvent(input$deletePrj, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      
      footer = tagList(modalButton("No"),
                       actionButton("deletePrjYes", "Yes"))))
  })
  
  ## DELETE a record after confirmation
  observeEvent(input$deletePrjYes, {

    if(input$prjid %in% rvtbl$pat$patprjidref) {
      showNotification("Error: There are still patients assigned to this project!", type = "error", duration = NULL)
      removeModal()
    } else {
      query <- sprintf("DELETE FROM project WHERE prjid = '%s'",
                       input$prjid)
      dbGetQuery(pool, query)
      removeModal()
      shinyjs::reset("setPrj")
      rvtbl$prj <- getTbl("project")
      
    }
  })
  
  # INSERT/UPDATE a record
  observeEvent(input$submitPrj, {
    
    # Get input data and trim whitespaces
    data <- sapply(getColNames("project"), function(x) input[[x]])
    data <- lapply(data, trimws, which = "both")
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data)] <- "NULL"
    
    # Separate auto-id
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Add '' around filled text fields
    dataChr <- c("prjname", "prjdisease", "prjmaterial", "prjfirstname", "prjlastname", "prjdepartment", "prjinstitute", "prjcity", "prjcountry", "prjdescription")
    for(i in dataChr) {
      if(dataInsert[i] != "NULL") {
        dataInsert[i] <- paste0("'", dataInsert[i], "'")
      }  
    }
    
    # Get all data to check update values
    dataSelected <- rvtbl$prj[input$tbl_prj_rows_selected, ]
    
    # INSERT values, if autoID == NA, else UPDATE
    if( dataAutoID == "NULL" ) {
      
      if( toupper(data[["prjname"]]) %in% toupper(rvtbl$prj$prjname) | toupper(data[["prjname"]]) %in% rvtbl$prj$prjname) {
        showNotification("Error: This value alread exists!", type  = "error", duration = NULL)
        updateInputsPrj(dataSelected, session)
        
      } else {
        insertNames <- paste0(names(dataInsert), collapse = ", ")
        insertValues <- paste0(unname(dataInsert), collapse = ", ")
        insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", "project", insertNames, insertValues)
        print(insertQuery)
        dbGetQuery(pool, insertQuery)
        
      }
    } else {
      if( !toupper(data[["prjname"]]) %in% toupper(rvtbl$prj$prjname) | nrow(rvtbl$prj[rvtbl$prj$prjid == input$prjid & toupper(rvtbl$prj$prjname) == toupper(input$prjname), ]) > 0) {
        
        updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
        updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID) )
        updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;", "project", updateData, updateAutoID)
        print(updateQuery)
        dbGetQuery(pool, updateQuery)
        
      } else {
        showNotification("Error: This value alread exists!", type  = "error", duration = NULL)
        updateInputsPrj(dataSelected, session)
      }
    }
    
    shinyjs::reset("setPrj")
    rvtbl$prj <- getTbl("project")

  })
  
  
  # Create output table
  output$tbl_prj <- DT::renderDataTable({
    tbl <- rvtbl$prj
    names(tbl) <- tbl_matchNames$Names[match(names(tbl),tbl_matchNames$Fields)]
    tbl
  }, selection = "single", rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0)))
  )
  
  # ----
  
  
  # User
  # ----
  
  # Disable fields
  disable("usrid")
  hide("usrid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled("usrinitials", "submitUsr", session) })
  observe({ mandFilled("usrid", "deleteUsr", session) })
  
  # Update input fields when row is selected
  updateInputsUsr <- function(data, session) {
    updateNumericInput(session, "usrid", value = as.integer(data[["usrid"]]))
    updateTextInput(session, "usrinitials", value = data[["usrinitials"]])
    updateTextInput(session, "usrfirstname", value = data[["usrfirstname"]])
    updateTextInput(session, "usrlastname", value = data[["usrlastname"]])
    updateSelectInput(session, "usrposition", selected = data[["usrposition"]])
    updateDateInput(session, "usrstartdate", value = data[["usrstartdate"]])
    updateDateInput(session, "usrenddate", value = data[["usrenddate"]])
  }
  observeEvent(input$tbl_usr_rows_selected, {
    if (length(input$tbl_usr_rows_selected) > 0) {
      dataSelected <- rvtbl$usr[input$tbl_usr_rows_selected, ]
      updateInputsUsr(dataSelected, session)
      disable("usrinitials")
      
    }
  })
  

  # RESET values
  observeEvent(input$resetUsr, {
    enable("usrinitials")
    shinyjs::reset("setUsr")
  })
  
  # DELETE a record
  ## Show modal when button deletePat is clicked
  observeEvent(input$deleteUsr, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      
      footer = tagList(modalButton("No"),
                       actionButton("deleteUsrYes", "Yes"))))
  })
  
  # DELETE a record after confirmation
  observeEvent(input$deleteUsrYes, {
    
    if(input$usrid %in% c(rvtbl$alq$alqusridref, rvtbl$alq$alqusedusridref, rvtbl$anl$anlanuidref) ) {
      showNotification("Error: There are records assigned to this user! Remove or change all records, before you can delete this user.", type = "error", duration = NULL)
      removeModal()
    } else {
      query <- sprintf("DELETE FROM users WHERE usrid = '%s'",
                       input$usrid)
      dbGetQuery(pool, query)
      removeModal()
      shinyjs::reset("setUsr")
      rvtbl$usr <- getTbl("users")
    }

  })

  
  # INSERT/UPDATE a record
  observeEvent(input$submitUsr, {
    
    data <- sapply(getColNames("users"), function(x) input[[x]], simplify = FALSE)
    data <- lapply(data, trimws, which = "both")
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data) ] <- "NULL"

    # Remove empty date field
    if( length(data[["usrstartdate"]]) == 0 ) {
      data[["usrstartdate"]] <- "NULL"
    } else {
      data[["usrstartdate"]] <- as.character(data[["usrstartdate"]])
    }
    if( length(data[["usrenddate"]]) == 0 ) {
      data[["usrenddate"]] <- "NULL"
    } else {
      data[["usrenddate"]] <- as.character(data[["usrenddate"]])
    }
    
    # Add '' around filled text fields
    dataChr <- c("usrinitials", "usrfirstname", "usrlastname", "usrposition", "usrstartdate", "usrenddate")
    for(i in dataChr) {
      if(data[i] != "NULL") {
        data[i] <- paste0("'", data[i], "'")
      }  
    }
    
    # Separate autoID
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Get all data to check update values
    dataSelected <- rvtbl$usr[input$tbl_usr_rows_selected, ]
    
    # INSERT values, if autoID == NA, else UPDATE
    
    if( dataAutoID == "NULL" ) {
      
      if( toupper(data[["usrinitials"]]) %in% toupper(rvtbl$usr$usrinitials) | toupper(data[["usrinitials"]]) %in% rvtbl$usr$usrinitials) {
        
        showNotification("Error: This value alread exists!", type = "error", duration = NULL)
        updateInputsUsr(dataSelected, session)
        
      } else {
        insertNames <- paste0(names(dataInsert), collapse = ", ")
        insertValues <- paste0(unname(dataInsert), collapse = ", ")
        insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", "users", insertNames, insertValues)
        print(insertQuery)
        dbGetQuery(pool, insertQuery)
        enable("usrinitials")
        
      }
    } else {
      if( !toupper(data[["usrinitials"]]) %in% toupper(rvtbl$usr$usrinitials) | nrow(rvtbl$usr[rvtbl$usr$usrid == input$usrid & toupper(rvtbl$usr$usrinitials) == toupper(input$usrinitials), ]) > 0) {
        
        updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
        updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID) )
        updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;", "users", updateData, updateAutoID)
        print(updateQuery)
      dbGetQuery(pool, updateQuery)
      enable("usrinitials")
      
      } else {
        showNotification("Error: This value alread exists!", type = "error", duration = NULL)
        updateInputsUsr(dataSelected, session)
      }
    }
    
    shinyjs::reset("setUsr")
    rvtbl$usr <- getTbl("users")

  })
  
  # Create output table
  output$tbl_usr <- DT::renderDataTable({
    tbl <- rvtbl$usr
    names(tbl) <- tbl_matchNames$Names[match(names(tbl),tbl_matchNames$Fields)]
    tbl
  }, selection = "single", rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0)))
  )
  
  # ----
  
  
  # Analyses lookup table
  # ----
  
  # Disable autoID
  disable("anuid")
  hide("anuid")
  
  # Check if all mandatory fields are filled
  observe({ mandFilled("anuname", "submitAnu", session) })
  observe({ mandFilled("anuid", "deleteAnu", session) })
  
  # Update input fields when row is selected
  updateInputsAnu <- function(data, session) {
    updateNumericInput(session, "anuid", value = as.integer(data[["anuid"]]))
    updateTextInput(session, "anuname", value = data[["anuname"]])
    updateTextInput(session, "anudescription", value = data[["anudescription"]])
  }
  
  observeEvent(input$tbl_anu_rows_selected, {
    if( length(input$tbl_anu_rows_selected) > 0) {
      dataSelected <- rvtbl$anu[input$tbl_anu_rows_selected, ]
      updateInputsAnu(dataSelected, session)
    }
  })
  
  # INSERT/UPDATE a record
  observeEvent(input$submitAnu, {
    
    # Get input data and trim whitespaces
    data <- sapply(getColNames("analysislookup"), function(x) input[[x]], simplify = FALSE)
    data <- lapply(data, trimws, which = "both")
    
    # Replace "" for NULL to define empty fields
    data[data == "" | is.na(data)] <- "NULL"
    
    # Separate autoID
    dataInsert <- data[-1]
    dataAutoID <- data[1]
    
    # Add '' around filled text fields
    dataChr <- c("anuname", "anudescription")
    for(i in dataChr) {
      if(dataInsert[i] != "NULL") {
        dataInsert[i] <- paste0("'", dataInsert[i], "'")
      }  
    }
    
    # Get all data to check update values
    dataSelected <- rvtbl$anu[input$tblAnu_rows_selected, ]
    
    # INSERT values, if autoID == NA, else UPDATE
    
    if( dataAutoID == "NULL" ) {
      
      if( toupper(data[["anuname"]]) %in% toupper(rvtbl$anu$anuname) ) {
        
        showNotification("Error: This value alread exists!", type  = "error", duration = NULL)
        updateInputsAnu(dataSelected, session)
        
      } else {
        insertNames <- paste0(names(dataInsert), collapse = ", ")
        insertValues <- paste0(unname(dataInsert), collapse = ", ")
        insertQuery <- sprintf("INSERT INTO %s (%s) VALUES (%s);", "analysislookup", insertNames, insertValues)
        print(insertQuery)
        dbGetQuery(pool, insertQuery)
        
      }
    } else {
      if( !toupper(data[["anuname"]]) %in% toupper(rvtbl$anu$anuname) | nrow(rvtbl$anu[rvtbl$anu$anuid == input$anuid & toupper(rvtbl$anu$anuname) == toupper(input$anuname), ]) > 0) {
        updateData <- paste0(names(dataInsert), " = ", dataInsert, collapse = ", ")
        updateAutoID <- paste0(names(dataAutoID), " = ", as.integer(dataAutoID) )
        updateQuery <- sprintf("UPDATE %s SET %s WHERE %s ;", "analysislookup", updateData, updateAutoID)
        print(updateQuery)
        dbGetQuery(pool, updateQuery)
      } else {
        showNotification("Error: This value alread exists!", type  = "error", duration = NULL)
        updateInputsAnu(dataSelected, session)
      }
    }
    shinyjs::reset("setAnu")
    rvtbl$anu <- getTbl("analysislookup")
    
  })
  
  
  # DELETE a record
  ## Show modal when button deletePat is clicked
  observeEvent(input$deleteAnu, {
    showModal(modalDialog(
      h4("Do you really want to delete this record?"),
      
      footer = tagList(modalButton("No"),
                       actionButton("deleteAnuYes", "Yes"))))
  })
  
  # DELETE a record after confirmation
  observeEvent(input$deleteAnuYes, {
    query <- sprintf("DELETE FROM analysislookup WHERE anuid = '%s'",
                     input$anuid)
    dbGetQuery(pool, query)
    removeModal()
    shinyjs::reset("setAnu")
    rvtbl$anu <- getTbl("analysislookup")
  })
  
  
  # RESET values
  observeEvent(input$resetAnu, {
    setInputFields <- c("anuid", "anuname", "anudescription")
    for(i in setInputFields) {
      reset(i)
    }
  })
  
  
  # Create output table
  output$tbl_anu <- DT::renderDataTable({
    tbl <- rvtbl$anu
    names(tbl) <- tbl_matchNames$Names[match(names(tbl),tbl_matchNames$Fields)]
    tbl
  }, selection = "single", rownames = FALSE, options = list(columnDefs = list(list(visible = FALSE, targets = 0)))
  )
  
  # ----
  
}