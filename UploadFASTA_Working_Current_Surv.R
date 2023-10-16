# Load R libs ####
setwd("C:\\Users\\ianho\\OneDrive\\Desktop\\BioinformaticStuff\\RCode\\MiSeVis\\")
library(shiny)
library(shinyjs)
library(shinythemes)
library(htmltools)
library(NGLVieweR)
library(survival)
library(survminer)
library(KMsurv)
library(ggplot2)
library(plyr)
path <- 'C:\\Users\\ianho\\OneDrive\\Desktop\\BioinformaticStuff\\RCode\\MiSeVis\\/'
df <- read.csv(file = paste0(path,'all.csv'))
gene_list <- sample(unique(na.omit(df$Gene.Name)),100)
pdb_list <- gsub(".pdb", "", list.files(path, pattern='pdb'))
pdb_list <- unique(gsub("_Mutated", "", pdb_list))
survive_list <- gsub(".tsv","", list.files(path, pattern='tsv'))

ui <- fluidPage(shinyjs::useShinyjs(), theme = shinytheme("cyborg"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                navbarPage("MiSeVis",
                           position = "static-top", 
                           tabPanel("Retrieve Sequence File",
                                    tags$hr(),
                                    sidebarPanel
                                    (
                                      tabsetPanel(id = "tabset",
                                                  tabPanel("Sequence File",
                                                           actionButton("showselect", "Click to upload file or show example sequence file"),
                                                           tags$hr(),
                                                           conditionalPanel(
                                                             condition = "input.showselect > 0",
                                                             selectInput('file1','Select An Example Sequence File...',
                                                                         choices = c("Select an example sequence file", list.files(path, pattern='fasta'))),
                                                             actionButton("showupload", "Click to Show Upload Option"),
                                                             conditionalPanel(
                                                               condition = "input.showupload > 0",
                                                               fileInput("fileupload", "Upload Your Own File", 
                                                                         multiple = TRUE, 
                                                                         accept = c("text/fasta",".fasta"))
                                                             )
                                                           )
                                                  ),
                                                  tabPanel("Structure File",
                                                           actionButton("showselect2", "Click to upload file or show example structure file"),
                                                           tags$hr(),
                                                           conditionalPanel(
                                                             condition = "input.showselect2 > 0",
                                                             selectInput('file2','Select An Example PDB File...',
                                                                         choices = c("Select an example structure file", list.files(path, pattern='pdb'))),
                                                             actionButton("showupload2", "Click to Show Upload Option"),
                                                             conditionalPanel(
                                                               condition = "input.showupload2 > 0",
                                                               fileInput("fileupload2", "Upload Your Own File", 
                                                                         multiple = TRUE, accept = c("text/pdb",".pdb"))
                                                             )
                                                           )
                                                  )
                                      ),
                                      tags$br(),
                                      actionButton("reset", "Clear Sequence"),
                                      actionButton("reset2", "Clear PDB")
                                    ),
                                    mainPanel(textOutput("file1"), textOutput("file2"),
                                              textOutput("fileupload"), tableOutput("fileupload2"))
                           ),
                           tabPanel("Visualize PDB File",
                                    selectizeInput("choosepdb", "Choose a Gene to Visualize", choice = c("", gsub(".pdb", "", list.files("C:", pattern='pdb'))), options = list(placeholder = "Select a Gene")),
                                    radioButtons("rbuttons", "", c("Normal" = "",
                                                                   "Mutated" = "_Mutated"), selected = character(0)),
                                    sidebarPanel(
                                      conditionalPanel(
                                        condition = "input.visparam > 0",
                                        textInput("selection", "Selection", "1-20"),
                                        selectInput("type", "Type", c("ball+stick", "cartoon", "backbone", "surface")),
                                        selectInput("color", "Color", c("orange", "grey", "white")),
                                        actionButton("add", "Add"),
                                        actionButton("remove", "Remove"),
                                        h4(actionButton("show_surface", "Click to show protein surface"))
                                      )
                                    ),
                                    mainPanel(
                                      actionButton("visparam", "Manipulate the 3D Visualization"),
                                      tags$br(),
                                      NGLVieweROutput("structure"),
                                    ),
                           ),
                           tabPanel("Check DrugBank",
                                    sidebarPanel(
                                      textInput("geneselected", label = h4("Select a Gene"), value = ""),
                                      actionButton("checkdrug", label = "Output Drug Targets")
                                    ),
                                    mainPanel(
                                      h5("Drug Targets Found"),
                                      textOutput("drugtargets"),
                                      h5("URL for DrugBank Drug Target Entry:"),
                                      h6(textOutput("drugtargeturl"))
                                    )
                           ),
                           tabPanel("Survival Analysis",
                                    sidebarPanel(
                                      selectInput("choosetsv", "Choose a Gene to Plot",
                                                  choice = survive_list,
                                                  selected = survive_list[1])
                                      
                                    ),
                                    mainPanel(
                                      h3(textOutput("Title")),
                                      plotOutput("sur_plot")
                                    )
                           ),
                           tabPanel("User Manual",
                                    mainPanel(
                                      h5("Overview"),
                                      h6("This pipeline not only outputs the contents of an inputted sequence and structure file but also allows the user to visualize 3D models of a PDB file. The user may also manipulate the model’s parameters and analyze individual amino acids through the generated model. The user may also look for drug matches in the DrugBank database based on the selection of the PDB visualization file. "),
                                      h5("Retrieve Sequence File"),
                                      h6("On the Retrieve Sequence Tab there are two options for file input: a Sequence file and a Structure file. The Sequence File tab has a button that is accessed with the “Click to Show Upload Option” button which allows the user to input a file from the computer’s local files. However, the file upload must be in the .FASTA format in order to properly show the syntax of the inputted FASTA file. You may click on the Clear Sequence button to clear the sequence shown or the Clear PDB button to get rid of the output of the Structure File tab. The Structure File tab works very similarly to the Sequence File tab with the exception of the inputted file type. The user-uploaded file must be a Protein Data Bank file instead of a FASTA file."), 
                                      h5("Visualize PDB File"),
                                      h6("The Visualize PDB tab allows the user to examine and modify the 3D model of the inputted gene. First, the user must select a gene in the “Choose a Gene to Visualize” dropdown menu. Then, they must specify whether or not they would like to see the normal or mutated structure of the gene. The 3D visualization will appear automatically and the user may use their cursor to examine the different sides of the model. Furthermore, since the model is based on the specified quaternary structure of the PDB file, the user may also hover over individual parts of the protein and see specific amino acids at the precise location on the amino acid sequence. The user may also highlight certain sections of the protein by pressing the “Manipulate the 3D Visualization” button. The first parameter is the amino acid selection, which allows the user to specify the range of amino acids they want highlighted. The second parameter is the representation type for the highlighted amino acids. The third parameter specifies the color of the highlighted regions in the protein. The “Add” button will add the specified parameters while the “Remove” button will remove them. Finally, the button “Click to Show Protein Surface” allows the user to visualize the surface of the entire 3D model. However, the user must have the type parameter set to “surface” in order for the button to work."),
                                      h5("Check DrugBank"),
                                      h6("The Check DrugBank tab is used to identify drug targets for specific genes based on the DrugBank database. The user simply needs to type in the gene name and click the “Output Drug Targets” button. The main panel will show the drug IDs that are linked to the queried gene."),
                                      h5("Survival analysis"),
                                      h6("The Survival analysis tab is used to assess the effect of specific gene expression on survival rates. The user simply needs to select a gene name. The main panel will show the plot.")
                                    )
                           )
                           
                )
)
server <- function(input, output, session){
  
  selected_file_content <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    # Read and store the content of the selected example file
    if (input$file1 != "Select an example sequence file") {
      filePath1 <- file.path(path, input$file1)
      content <- paste(readLines(filePath1), collapse = "\n")
      selected_file_content(content)
    } else {
      selected_file_content(NULL) # Clear content if the default option is selected
    }
  })
  
  observeEvent(input$file2, {
    # Read and store the content of the selected example file
    if (input$file2 != "Select an example structure file") {
      filePath2 <- file.path(path, input$file2)
      content2 <- paste(readLines(filePath2), collapse = "\n")
      selected_file_content(content2)
    } else {
      selected_file_content(NULL) # Clear content if the default option is selected
    }
  })
  
  output$file1 <- renderText({
    # Display the content of the selected file
    selected_file_content()
  })
  
  output$file2 <- renderText({
    # Display the content of the selected file
    selected_file_content()
  })
  
  output$fileupload <- renderText({
    filePath <- input$fileupload$datapath
    paste(readLines(filePath))
  })
  
  output$fileupload2 <- renderText({
    filePath2 <- input$fileupload2$datapath
    paste(readLines(filePath2))
  })
  
  observeEvent(input$reset, {
    output$fileupload <- renderText({
    })
  })
  
  observeEvent(input$reset2, {
    output$fileupload2 <- renderText({
    })
  })
  
  rv <<- reactiveValues(
    value_store = character()
  )
  
  
  observeEvent(c(input$choosepdb, input$rbuttons), {
    rv$value_store <<- paste(paste(input$choosepdb, input$rbuttons, sep = ""), ".pdb", sep = "")
    filePath <<- rv$value_store
    output$structure <- renderNGLVieweR({
      NGLVieweR(paste("C:\\Users\\ianho\\OneDrive\\Desktop\\BioinformaticStuff\\RCode\\MiSeVis\\", filePath, sep = "")) %>% 
        stageParameters(backgroundColor = "black") %>%
        setQuality("high") %>%
        setSpin(FALSE) %>%
        setFocus(0) %>% 
        addRepresentation("cartoon",
                          param = list(name = "cartoon", colorScheme = "residueindex")
        ) %>%
        addRepresentation("ball+stick",
                          param = list(
                            name = "ball+stick",
                            colorValue = "red",
                            colorScheme = "element",
                            sele = "200"
                          )
        ) %>%
        addRepresentation("label",
                          param = list(
                            name = "label", sele = "200:A.O",
                            showBackground = TRUE,
                            backgroundColor = "black",
                            backgroundMargin = 2,
                            backgroundOpacity = 0.5,
                            showBorder = TRUE,
                            colorValue = "white"
                          )
        )
    })
  })
  
  observeEvent(input$add, {
    NGLVieweR_proxy("structure") %>%
      addSelection(isolate(input$type),
                   param =
                     list(
                       name = "sel1",
                       sele = isolate(input$selection), 
                       colorValue = isolate(input$color)
                     )
      )
  })
  
  observeEvent(input$remove, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("sel1")
  })
  
  observeEvent(input$show_surface, {
    NGLVieweR_proxy("structure") %>%
      addSelection(isolate(input$type),
                   param = 
                     list(name = "surface",
                          colorValue = "white",
                          opacity = "0.1"
                     )
      )
  })
  
  # # Drug targets ####
  # drug_targets <- reactiveVal(NULL)
  # drug_target_url <- reactiveVal(NULL)
  # 
  # observeEvent(input$checkdrug, {
  #   if (input$checkdrug > 0) {
  #     gene_selected <- input$geneselected
  #     csv_data_selection <- df[df$Gene.Name == gene_selected, ]
  #     
  #     if (nrow(csv_data_selection) == 0) {
  #       drug_targets("Gene not found in the dataset.")
  #       drug_target_url(NULL)
  #     } else {
  #       drug_ids <- na.omit(unlist(strsplit(csv_data_selection$Drug.IDs, split = "; ")))
  #       drug_targets(paste(drug_ids, collapse = ", "))
  #       drug_target_url(paste("https://go.drugbank.com/drugs/", drug_ids, sep = ""))
  #     }
  #   }
  # })
  # 
  # # Render the drug targets text and URL in the UI
  # output$drugtargets <- renderText({
  #   drug_targets()
  # })
  # 
  # output$drugtargeturl <- renderText({
  #   drug_target_url()
  # })
  # 
  # Survival plot ####
  
  
  observeEvent(input$checkdrug, {
    if(input$checkdrug > 0)
    {
      output$drugtargets <- renderText({
        csv_data <- read.csv(file = 'all.csv')
        if(input$geneselected != "")
        {
          csv_data_selection <- csv_data[csv_data$Gene.Name == input$geneselected,] %>% drop_na(Drug.IDs)
          drug_ids = unlist(strsplit(csv_data_selection$Drug.IDs, split = "; "))
        }
      })
    }
  })
  
  observeEvent(input$checkdrug, {
    if(input$checkdrug > 0)
    {
      output$drugtargeturl <- renderText({
        csv_data <- read.csv(file = 'all.csv')
        if(input$geneselected != "")
        {
          csv_data_selection <- csv_data[csv_data$Gene.Name == input$geneselected,] %>% drop_na(Drug.IDs)
          drug_ids = unlist(strsplit(csv_data_selection$Drug.IDs, split = "; "))
        }
        paste("https://go.drugbank.com/drugs/", drug_ids, sep = "")
      })
    }
  })
  output$Title <- renderText({
    paste0("Survival Graph of ",input$choosetsv)
  })
  
  tsv_data <- reactive({
    x <- read.delim(paste0(path,input$choosetsv,".tsv"))
    colnames(x)[5] <- "Gene"
    return(x)
  })
  
  # Create the Kaplan-Meier survival plot
  output$sur_plot <- renderPlot({
    tsv <- tsv_data()
    km_fit <- survfit(Surv(OS.time, OS) ~ Gene, data = tsv)
    ggsurvplot(km_fit, data = tsv,
               #title = paste0("Effect of ",Gene_name()," expression level on OV patient survival"),
               xlab = "Time in Days",
               pval = TRUE,
               ggtheme = theme_bw(),
               font.main = c(16, "bold", "black"))
  })
  
}

shinyApp(ui=ui, server=server)






