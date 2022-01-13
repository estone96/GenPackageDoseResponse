require('data.table')
require('doSNOW')
require('ROCR')
require('dplyr')
require("remotes")
require("DatabaseConnector")
require('CohortMethod')
require('survey')
require('caret')

source('analysis_main_param.r')
# remotes::install_github("OHDSI/DatabaseConnector")

oracleTempSchema = NULL

cdmDatabaseSchema <- "" ## server_database.server_scheme 
resultsDatabaseSchema <- ""

# Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "C:/JDBC") 

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql", #"postgresql"
                                                                server = "",
                                                                user = "",
                                                                password = "",
                                                                port = NULL)


connection <- DatabaseConnector::connect(connectionDetails)

run_analysis(connection, cdmDatabaseSchema, oracleTempSchema,resultsDatabaseSchema)

return_output()
