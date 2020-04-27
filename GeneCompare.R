library(shiny)
library(tidyverse)
library(shinythemes)
library(UpSetR)
library(shinyjs)
library(nVennR)
library(rsvg)
library(grImport2)
library(grid)
library(RVenn)

# Define the UI ----
ui <- fluidPage(
    
    # Set theme ----
    theme = shinytheme("flatly"),
    
    # Enable shinyjs for hide()/show()/toggle() ----
    useShinyjs(),
    
    # Navbar layout for the whole app ----
    navbarPage("GeneCompare",
               
               # File upload tab ----
               tabPanel("File Upload",
                        fluid = TRUE,
                        
                        # Sidebar layout ----
                        sidebarLayout(
                            
                            # Sidebar panel content ----
                            sidebarPanel(
                                
                                # File input module ----
                                fileInput(inputId = "files",
                                          label = "Upload CSV Files",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # File selector input ----
                                uiOutput("output.fileselector"),
                                
                                radioButtons(inputId = "preview",
                                             label = "Preview Mode",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head"),
                                
                                div(id = "fileinput"),
                                
                                div(id = "options",
                                   
                                    uiOutput("output.optionspanels")
                                    
                                    ),
                                
                                div(id = "geneIDs",
                                    
                                    uiOutput("output.geneIDselector"))
                            ),
                            
                            # Main panel content ----
                            mainPanel(
                                
                                h4("Preview"),
                                
                                hr(),
                                
                                tableOutput("preview")
                                
                            )
                        )
               ),
               
               # Comparison tab ----
               tabPanel("Comparison",
                        fluid = TRUE,
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                radioButtons(inputId = "comparisonchoice",
                                             label = "Comparison Type",
                                             choices = c(Summary = "summary",
                                                         Intersection = "intersection",
                                                         Combination = "combination")),
                                
                                downloadButton("overlaprdata",
                                               label = "Download .RData",
                                               style = "width:100%;"),
                                
                                div(id = "combinationside", tagList(
                                    
                                    hr(),
                                    
                                    radioButtons(inputId = "comparisonview",
                                                 label = "Preview Mode",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "head"),
                                    
                                    downloadButton("csvdl",
                                                   label = "Download CSV",
                                                   style = "width:100%;")
                                ))
                                
                            ),
                            
                            mainPanel(
                                
                                h4("Comparison"),
                                
                                hr(),
                                
                                div(id = "summarydiv", tagList(
                                    tableOutput("overlapsummary")
                                )),
                                
                                div(id = "overlapdiv", tagList(
                                    verbatimTextOutput("overlapprint")
                                )),
                                
                                div(id = "combinationdiv", tagList(
                                    tableOutput("combinationlist")
                                ))
                                
                            )
                            
                        )
                        
                        ),
               
               # Visualization tab ----
               tabPanel("Visualization",
                        fluid = TRUE,
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                radioButtons(inputId = "plotchoice",
                                             label = "Plot Type",
                                             choices = c(Upset = "upset",
                                                         Heatmap = "heatmap",
                                                         Venn = "venn")),
                                
                                hr(),
                                
                                strong("Download Options"),
                                
                                radioButtons(inputId = "plotfiletype",
                                             label = "Filetype",
                                             choices = c(PNG = "png",
                                                         PDF = "pdf")),
                                
                                div(id = "dimpx", tagList(
                                    strong("Height x Width (px)"),
                                    
                                    br(),
                                    
                                    div(style = "display:inline-block;vertical-align:top;",
                                        numericInput(inputId = "pxheight",
                                                     label = NULL,
                                                     value = 1000,
                                                     width = "100%")),
                                    
                                    div(style = "display:inline-block;vertical-align:top;",
                                        numericInput(inputId = "pxwidth",
                                                     label = NULL,
                                                     value = 1000,
                                                     width = "100%"))
                                    )),
                                
                                div(id = "dimin", tagList(
                                    strong("Height x Width (in)"),
                                    
                                    br(),
                                    
                                    div(style = "display:inline-block;vertical-align:top;",
                                        numericInput(inputId = "inheight",
                                                     label = NULL,
                                                     value = 7,
                                                     width = "100%")),
                                    
                                    div(style = "display:inline-block;vertical-align:top;",
                                        numericInput(inputId = "inwidth",
                                                     label = NULL,
                                                     value = 7,
                                                     width = "100%"))
                                )),
                                
                                div(id = "downloadUpset",
                                    tagList(downloadButton("upsetdl",
                                            label = "Download Upset Plot",
                                            style = "width:100%;"))),
                                
                                div(id = "downloadHeatmap",
                                    tagList(downloadButton("heatmapdl",
                                            label = "Download Heatmap",
                                            style = "width:100%;"))),
                                
                                div(id = "downloadVenn",
                                    tagList(downloadButton("venndl",
                                            label = "Download Venn Diagram",
                                            style = "width:100%;"))),
                                
                            ),
                            
                            mainPanel(
                                
                                div(id = "divUpset",
                                    tagList(h4("Upset Plot"),
                                
                                            hr(),
                                
                                            plotOutput("upset"))),
                                
                                div(id = "divHeatmap",
                                    tagList(h4("Heatmap"),
                                            
                                            hr(),
                                            
                                            plotOutput("heatmap"))),
                                
                                div(id = "divVenn",
                                    tagList(h4("Venn Diagram"),
                                            
                                            hr(),
                                            
                                            plotOutput("venn"))),
                                
                            )
                            
                        )
                        ),
               # About tab ----
               tabPanel("About",
                        fluid = TRUE,
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                p("GeneCompare is an app designed to allow comparisons of gene lists across experiments.  It was built using R Shiny version 1.4.0.2."),
                                
                                p("It was developed by Zachary Tatom at Virginia Commonwealth University under the guidance of Dr. Tim York, PhD."),
                                
                                p("Acknowledgments go to Dr. Roxanne Roberson-Nay, Dr. Dana Lapato, Eva Lancaster, and Hope Wolf, members of the VCU Data Science lab."),
                            
                                hr(),
                                
                                # Link to Twitter
                                actionButton(inputId = "github",
                                             label = tagList(img(src = "logo_twitter.png"), "Zachary Tatom on Twitter"),
                                             onclick = "window.open('https://twitter.com/zachary_tatom', '_blank')",
                                             style = "width:100%;"),
                                
                                br(),
                                
                                br(),
                                
                                # Link to GitHub
                                actionButton(inputId = "github",
                                             label = tagList(img(src = "logo_github.png"), "GeneCompare on GitHub"),
                                onclick = "window.open('https://github.com/zacharytatom/GeneCompare/', '_blank')",
                                style = "width:100%;")
                                
                                ),
                            
                            mainPanel(

                                h4("Citations"),
                                
                                hr(),
                                
                                uiOutput("citelist")
                            )
                            
                        )
                        
                        )
    )
)

server <- function(session, input, output) {
    
    # Create a file selector once files have been uploaded ----
    output$output.fileselector <- renderUI({
        
        # Require files be uploaded for the selector to appear ----
        req(input$files)
        
        # Create a selectize input (more flexible than selectInput()) ----
        selectizeInput(inputId = "input.fileselector",
                       label = "File",
                       choices = input$files$name,
                       multiple = FALSE)
    })
    
    # Check the files uploaded and generate a new sidebar panel for each ----
    output$output.optionspanels <- renderUI({
        
        # Require files be uploaded ----
        req(input$files)
        
        # Create an empty list to store tags in ----
        optionslist <- vector(mode = "list", length = nrow(input$files))
        
        names(optionslist) <- input$files$name
        
        # Use a for loop to run through all the uploaded files ----
        for(i in 1:nrow(input$files)) {
            
            # Create variables that will be used later ----
            divid <- paste0("optionid", i)
            header <- paste0("header", i)
            separator <- paste0("separator", i)
            quote <- paste0("quote", i)
            
            # Create a panel of inputs unique to each file ----
            optionslist[[i]] <- tagList(
                
                # Create a div and ID to store them in ----
                div(id = divid,
                    
                    strong(paste("File", i)),
                
                # Input: file header ----
                checkboxInput(inputId = header,
                              label = "Header",
                              value = TRUE),
                
                # Input: Select separator
                radioButtons(inputId = separator,
                             label = "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons(inputId = quote,
                             label = "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"')
                )
            )
        }
        
        return(optionslist)
        
    })
    
    # Only show the options panel for the relevant file ----
    observeEvent(input$input.fileselector, {
        
        # Get all the div IDs for all generated options panels ----
        optionids <- paste0("optionid", 1:nrow(input$files))
        
        # Grab the name of the file selected ----
        fileselect <- which(input$files$name == input$input.fileselector)
        
        # Identify which option panel corresponds to that file ----
        selection <- optionids[fileselect]
        
        # Identify all option panels that don't ----
        unselected <- optionids[-fileselect]
        
        # Show the relevant panel ----
        show(id = selection)
        
        # Hide all others ----
        lapply(unselected, hide)
    })
        
    # Create a list with all of the uploaded data ----
    csvlist <- reactive({
        
        req(input$files)
        
        csvs <- vector(mode = "list", length = nrow(input$files))
        names(csvs) <- input$files$name
        
        # For loop to run through all the uploaded files ----
        for(i in 1:nrow(input$files)) {
            
            # Set up variables to control read_delim ----
            delim <- paste0('input$separator', i)
            quote <- paste0('input$quote', i)
            cols <- paste0("input$header", i)
            
            # Convert variables created in last step to corresponding input values ----
            delim2 <- eval(str2lang(delim))
            quote2 <- eval(str2lang(quote))
            cols2 <- eval(str2lang(cols))
            
            # Read in the corresponding data using options and store in the list ----
            currentcsv <- read_delim(file = input$files$datapath[i],
                                     delim = delim2,
                                     quote = quote2,
                                     col_names = cols2)

            csvs[[i]] <- currentcsv
        }
        
        return(csvs)
        
    })
    
    # Render the preview table ----
    output$preview <- renderTable({
        
        # Require files be uploaded ----
        req(input$files)
        
        # Pulling out whichever file is selected for preview ----
        filename <- input$input.fileselector

        # Grabbing the data from that file ----
        filepreview <- as_tibble(csvlist()[[filename]])

        # if/else statement to display either the head or the whole preview ----
        if (input$preview == "head") {
            return(head(filepreview))
        } else {
            return(filepreview)
        }
        
    })
    
    # Create a selector for gene ID columns ----
    output$output.geneIDselector <- renderUI({
        
        # Require files be uploaded ----
        req(input$files)
        
        # Create an empty list to store tags in ----
        idlist <- vector(mode = "list", length = nrow(input$files))
        
        names(idlist) <- input$files$name
        
        # Use a for loop to run through all the uploaded files ----
        for(i in 1:nrow(input$files)) {
            
            # Create variables that will be used later ----
            divid <- paste0("genedivid", i)
            geneID <- paste0("geneID", i)
            
            # Create a panel of inputs unique to each file ----
            # If/else to hide all but the first div initially ----
            if (i == 1) {
                
                idlist[[i]] <- tagList(
                 div(
                     id = divid,
                      
                      # Input: select column for Gene ID ----
                      selectizeInput(inputId = geneID,
                                     label = "Gene ID",
                                     choices = names(csvlist()[[i]]),
                                     multiple = FALSE)
                  )
                )
            } else {
                
                idlist[[i]] <- tagList(
                hidden(
                    div(
                        id = divid,

                        # Input: select column for Gene ID ----
                        selectizeInput(inputId = geneID,
                                      label = "Gene ID",
                                      choices = names(csvlist()[[i]]),
                                      multiple = FALSE)
                        )
                    )
                )
            }
        }
        
        return(idlist)
        
    })
    
    # Only show the gene ID selector for the relevant file ----
    observeEvent(input$input.fileselector, {
        
        # Get all the div IDs for all generated options panels ----
        genedivids <- paste0("genedivid", 1:nrow(input$files))
        
        # Grab the name of the file selected ----
        fileselect <- which(input$files$name == input$input.fileselector)
        
        # Identify which selectize input corresponds to that file ----
        selection <- genedivids[fileselect]
        
        # Identify all selectize inputs that don't ----
        unselected <- genedivids[-fileselect]
        
        # Show the relevant panel ----
        show(id = selection)
        
        # Hide all others ----
        lapply(unselected, hide)
    })
    
    # Create a list of only the ID columns as vectors ----
    genelists <- reactive({
        
        # Create a list of id variable names ----
        genelistIDs <- paste0("input$geneID", 1:nrow(input$files))
        
        # Create the empty list ----
        genelist <- vector(mode = "list", length = nrow(input$files))
        names(genelist) <- input$files$name
        
        # For loop to run through each file ----
        for (i in 1:nrow(input$files)) {
        
            # Convert strings of variable names to their input counterparts ----
            genelistIDs[i] <- eval(str2lang(genelistIDs[[i]]))
            
            # Grab all of the data from the appropriate file ----
            data <- csvlist()[[i]]
            
            # Copy only the relevant column to the list ----
            genelist[i] <- data[genelistIDs[i]]
        
        
        }
        
        return(genelist)
        
    })
    
    # Create a reactive to store the list of all overlaps/intersections ----
    overlaps <- reactive({
        
        req(input$files)
        
        # Use plotVenn to find intersections in gene lists ----
        overlaplist <- plotVenn(genelists(), showPlot = FALSE)
        
        # List the overlapping regions ----
        listVennRegions(overlaplist, na.rm = FALSE)
        
    })
    
    # Print out a summary of overlaps/intersections ----
    output$overlapsummary <- renderTable({
        
        overlap.list <- lapply(overlaps(), function (x) x[!is.na(x)])
        
        listlength <- sapply(overlap.list, length)
        
        summarytibble <- tibble(Overlap = names(listlength),
                                n = listlength)
        
        summarytibble <- arrange(summarytibble, by = n)
        
        totaltibble <- tibble(Overlap = "Combined",
                              n = sum(summarytibble$n))
        
        summarytibble <- rbind(summarytibble, totaltibble)
        
        return(summarytibble)
    })
    
    # Print out the list of overlaps/intersections ----
    output$overlapprint <- renderPrint(
        
        overlaps()
        
        )
    
    # Generate the combined list ----
    combination <- reactive({
        
        # Require files be uploaded ----
        req(input$files)
        
        genecomp <- reduce(genelists(), union)
        
        # If/else statements for view mode ----
        if (input$comparisonview == "head") {
            return(head(genecomp))
        } else {
            return(genecomp)
        }
        
    })
    
    # Render the comparison ----
    output$combinationlist <- renderTable(
        
        combination()
        
    )
    
    # Observer for which to show: summary, overlaps, or combination ----
    observeEvent(input$comparisonchoice, {
        
        toggle(id = "summarydiv", condition = input$comparisonchoice == "summary")
        
        toggle(id = "overlapdiv", condition = input$comparisonchoice == "intersection")
        
        toggle(id = "combinationdiv", condition = input$comparisonchoice == "combination")
        
        toggle(id = "combinationside", condition = input$comparisonchoice == "combination")
        
    })
    
    # Create a download handler for the overlaps ----
    output$overlaprdata <- downloadHandler(
        
        filename = function(){
            paste0(Sys.Date(), "_comparison", ".RData")
        },
        
        content = function(file){
            overlaps <- overlaps()
            combination <- combination()
            save(overlaps, combination, file = file)
        }
        
    )
    
    # Create a download handler for the comparison ----
    output$csvdl <- downloadHandler(
        
        filename = function(){
            paste0(Sys.Date(), "_combination", ".csv")
        },
        
        content = function(file){
            write.csv(comparison(), 
                      file,
                      quote = FALSE,
                      row.names = FALSE)
        }
        
    )
    
    # Create the upset plot function ----
    fun.upset <- function(){
        
        req(input$files)
        
        upset(fromList(genelists()),
              order.by = "freq",
              point.size = 5,
              line.size = 1.3,
              text.scale = 2)
        
    }
    
    # Render the upset plot for comparisons ----
    output$upset <- renderPlot({
        
        fun.upset()
        
    },
    width = "auto",
    height = 1000)
    
    # Download handler for the upset plot ----
    output$upsetdl <- downloadHandler(
        
        # Generate filename and extension using if/else to check input$plotfiletype ----
        filename = function(){
            
            if (input$plotfiletype == "pdf")
                paste0(Sys.Date(), "_upset", ".pdf")
            else
                paste0(Sys.Date(), "_upset", ".png")
        },
        
        # Generate content using if/else to check input$plotfiletype and setting dimensions using inputs ----
        content = function(file){
            
            if (input$plotfiletype == "pdf")
                pdf(file,
                    width = (input$inwidth),
                    height = (input$inheight))
            else
                png(file,
                    width = input$pxwidth,
                    height = input$pxheight)
            
            print(fun.upset())
            dev.off()
        }
    )
    
    # Create the heatmap function ----
    fun.heatmap <- function(){
        
        req(input$files)
        
        heatlist <- lapply(genelists(), as_vector)
        
        heatVenn <- Venn(heatlist)
        
        setmap(heatVenn)
        
    }
    
    # Render the heatmap ----
    output$heatmap <- renderPlot({
        
        fun.heatmap()
        
    },
    width = "auto",
    height = 1000)
    
    # Download handler for the heatmap ----
    output$heatmapdl <- downloadHandler(
        
        # Generate filename and extension using if/else to check input$plotfiletype ----
        filename = function(){
            
            if (input$plotfiletype == "pdf")
                paste0(Sys.Date(), "_heatmap", ".pdf")
            else
                paste0(Sys.Date(), "_heatmap", ".png")
        },
        
        # Generate content using if/else to check input$plotfiletype and setting dimensions using inputs ----
        content = function(file){
            
            if (input$plotfiletype == "pdf")
                pdf(file,
                    width = (input$inwidth),
                    height = (input$inheight))
            else
                png(file,
                    width = input$pxwidth,
                    height = input$pxheight)
            
            print(fun.heatmap())
            dev.off()
        }
    )
    
    # Create the Venn diagram function ----
    fun.venn <- function(){
        
        req(input$files)
        
        v <- plotVenn(genelists(), showPlot = FALSE)
        
        v2 <- showSVG(v, opacity = 0.2, borderWidth = 1, fontScale = 2)
        
        grid.draw(v2)
        
    }
    
    # Render the Venn diagram ----
    output$venn <- renderPlot({
        
        fun.venn()
        
    },
    width = "auto",
    height = 1000)
    
    # Download handler for the Venn diagram ----
    output$venndl <- downloadHandler(
            
        # Generate filename and extension using if/else to check input$plotfiletype ----
        filename = function(){
            
            if (input$plotfiletype == "pdf")
                paste0(Sys.Date(), "_venn", ".pdf")
            else
                paste0(Sys.Date(), "_venn", ".png")
        },
        
        # Generate content using if/else to check input$plotfiletype and setting dimensions using inputs ----
        content = function(file){
            
            if (input$plotfiletype == "pdf")
                pdf(file,
                    width = (input$inwidth),
                    height = (input$inheight))
            else
                png(file,
                    width = input$pxwidth,
                    height = input$pxheight)
            
            print(fun.venn())
            dev.off()
        }
    )
    
    # Observer to see which plot to show ----
    observeEvent(input$plotchoice, {
        
        # Show download link for upset plot when chosen ----
        toggle(id = "downloadUpset",
               condition = input$plotchoice == "upset")
        
        # Show upset plot when chosen ----
        toggle(id = "divUpset",
               condition = input$plotchoice == "upset")
        
        # Show download link for heatmap when chosen ----
        toggle(id = "downloadHeatmap",
               condition = input$plotchoice == "heatmap")
        
        # Show heatmap when chosen ----
        toggle(id = "divHeatmap",
               condition = input$plotchoice == "heatmap")
        
        # Show download link for Venn diagram when chosen ----
        toggle(id = "downloadVenn",
               condition = input$plotchoice == "venn")
        
        # Show Venn diagram when chosen ----
        toggle(id = "divVenn",
               condition = input$plotchoice == "venn")
    })
    
    # Observer to see which set of dimensions to show ----
    observeEvent(input$plotfiletype, {
        
        # If downloading a pdf, show us inches ----
        toggle(id = "dimin",
               condition = input$plotfiletype == "pdf")
        
        # If downloading a png, show us pixels ----
        toggle(id = "dimpx",
               condition = input$plotfiletype == "png")
        
    })
    
    # Create our citations list ----
    citations <- reactive({
        # Note: the citations will differ if you download and run this in R Studio yourself.
        # (It will include packages you have on your local machine.)  Restart your session
        # before use or use the online version for accurate citations.
        
        # Get actively loaded packages ----
        packageids <- (.packages())
        
        # Create an empty vector to be our list ----
        packagecite <- vector(length = (length(packageids) - 6))
        
        # For loop runs through each package and grabs the citation ---
        for (i in 1:length(packagecite)) {
            
            packagecite[i] <- format(citation(packageids[i]), "html")
        }
        
        # Alphabetize citations ----
        packagecite <- sort(packagecite)
        
        # Apply HTML() to render HTML properly ----
        packagecite <- lapply(packagecite, HTML)
        
        return(packagecite)
        
    })
    
    # Render the citation list as a table ----
    output$citelist <- renderUI({
        
        citations()
        
    })
}

shinyApp(ui, server)