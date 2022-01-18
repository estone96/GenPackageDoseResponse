## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    skin = 'yellow',
    dashboardHeader(title = "Dose Study Generator"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summmary", tabName = "summary"),
            menuItem("Cohorts", tabName = "cohort"),
            menuItem("Outcomes", tabName = "outcome"),
            menuItem("Models", tabName = "model")
        ),
        downloadButton("out_package","Get Package"),
        tags$style(type='text/css', "#out_package { width:90%; margin-left: 10px; color: black;}")
    ),
    dashboardBody(
        tabItems(
            tabItem("summary",
                    fluidRow(
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Study Title",
                            textInput("studyname","Input name for the study")
                            
                        ),
                        box(
                            width = 8, status = "info",
                            title = "About",
                            p("Package generator for generic dose study")
                        )
                    )
            ),
            tabItem("cohort",
                    fluidRow(
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Cohort Entry Events: Drug",
                            textInput("target_drugs","Concept IDs for Drug Exposure"),
                            textInput("target_drugs_route","Concept IDs for Drug Exposure Route")
                            
                        ),
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Cohort Entry Events: Condition",
                            textInput("target_outcome","Concept IDs for Inclusion Condition")
                        ),
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Cohort Entry Events: Inclusion Criteria",
                            numericInput("target_age_min","Minimum Index Date Age for Inclusion", 25),
                            numericInput("target_age_max","Maximum Index Date Age for Inclusion", 64)
                        )
                    )
            ),
            tabItem("outcome",
                    fluidRow(
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Target Outcome Condition 1",
                            textInput("target_outcome_1","Concept IDs for Outcome of Interest"),
                            textInput("target_outcome_1_name","Outcome Name")
                            
                        ),
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Target Outcome Condition 2",
                            textInput("target_outcome_2","Concept IDs for Outcome of Interest"),
                            textInput("target_outcome_2_name","Outcome Name")
                            
                        ),box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Target Outcome Condition 3",
                            textInput("target_outcome_3","Concept IDs for Outcome of Interest"),
                            textInput("target_outcome_3_name","Outcome Name")
                            
                        ),box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Target Outcome Condition 4",
                            textInput("target_outcome_4","Concept IDs for Outcome of Interest"),
                            textInput("target_outcome_4_name","Outcome Name")
                        )
                    )
            ),
            tabItem("model",
                    fluidRow(
                        box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Drug Analysis Settings",
                            h3('Drug #1'),
                            textInput("target_trans_1","Concept IDs for Drug Potency Translation"),
                            numericInput("target_trans_1_out","Weight for Drug Potency",1),
                            h3('Drug #2'),
                            textInput("target_trans_1","Concept IDs for Drug Potency Translation"),
                            numericInput("target_trans_1_out","Weight for Drug Potency",1),
                            h3('Drug #3'),
                            textInput("target_trans_1","Concept IDs for Drug Potency Translation"),
                            numericInput("target_trans_1_out","Weight for Drug Potency",1),
                            h3('Drug #4'),
                            textInput("target_trans_1","Concept IDs for Drug Potency Translation"),
                            numericInput("target_trans_1_out","Weight for Drug Potency",1)
                        ),box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Data Transformation",
                            radioButtons('transform_data',"Data Transformation for Drug Potency",
                                         choices = c('Log Transform'='log_tr',
                                                     'Exponential Transform'='exp_tr',
                                                     'No Transformation'='no_tr'))
                        )
                    )
            )
        )
    )
)

server <- function(input, output) {
    get.files <- reactive({
        list.files("generaldosestudy/package/")
    })  
    studyname <- reactive({
        input$studyname
    })  
    data_params = reactive({
        params_input = list()
        params_input[['studyname']] = input$studyname
        params_input[['target_drugs']] = input$target_drugs
        params_input[['target_drugs_route']] = input$target_drugs_route
        params_input[['target_outcome']] = input$target_outcome
        params_input[['target_age_min']] = input$target_age_min
        params_input[['target_age_max']] = input$target_age_max
        params_input[['target_outcome_1']] = input$target_outcome_1
        params_input[['target_outcome_1_name']] = input$target_outcome_1_name
        params_input[['target_outcome_2']] = input$target_outcome_2
        params_input[['target_outcome_2_name']] = input$target_outcome_2_name
        params_input[['target_outcome_3']] = input$target_outcome_3
        params_input[['target_outcome_3_name']] = input$target_outcome_3_name
        params_input[['target_outcome_4']] = input$target_outcome_4
        params_input[['target_outcome_4_name']] = input$target_outcome_4_name
        params_input[['target_trans_1']] = input$target_trans_1
        params_input[['target_trans_1_out']] = input$target_trans_1_out
        params_input[['target_trans_2']] = input$target_trans_2
        params_input[['target_trans_2_out']] = input$target_trans_2_out
        params_input[['target_trans_3']] = input$target_trans_3
        params_input[['target_trans_3_out']] = input$target_trans_3_out
        params_input[['target_trans_4']] = input$target_trans_4
        params_input[['target_trans_4_out']] = input$target_trans_4_out
        params_input[['log_tr']] = input$transform_data
        return(params_input)
    })
    
    
    output$out_package = downloadHandler(
        filename = function(){
            paste(studyname(),'.zip',sep='')
        },
        content = function(file){
            #go to a temp dir to avoid permission issues
            file.copy('analysis_main_param.R',paste(tempdir(),"/",'analysis_main_param.R',sep=''))
            file.copy('CodeToRun.R',paste(tempdir(),"/",'CodeToRun.R',sep=''))
            file.copy('GeneralDoseStudy.Rproj',paste(tempdir(),"/",'GeneralDoseStudy.Rproj',sep=''))
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            saveRDS(data_params(), file = "studysettings.RDS") 
            files = c('studysettings.RDS',"GeneralDoseStudy.Rproj","CodeToRun.R","analysis_main_param.R")
            #create the zip file
            system2("zip", args=(paste(file,files,sep=" ")))
        },
        contentType = "application/zip"
    )
}

shinyApp(ui, server)