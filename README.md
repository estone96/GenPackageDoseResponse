# Package Generator for Dose Study

This is the repository for package generator for dose study.

![Screenshot](https://raw.githubusercontent.com/estone96/GenPackageDoseResponse/main/assets/image1.png)

This package generates dose study package for user-defined index drugs by utilizing R shiny GUI and determines the cutoff level of maximum AUC using logistic regression, support vector machine and one-class support vector machine by utilizing generalized propensity score analysis. 

## Features
This package automatically generates dose study package by using user-defined..
1. 4 target drug groups for study
2. Target prerequsite condition group 
3. Target outcome condition group
4. Drug potency generalization by applying weights
5. Transformation of cumulative drug usage (log, exponential)

## Usage
1. Access R shiny webpage by cloning this repository or via supplied URL below.
https://ekiben.shinyapps.io/GenPackageDoseResponse/

2. Supply user-defined drug groups and condition groups for the study.
![Screenshot](https://raw.githubusercontent.com/estone96/GenPackageDoseResponse/main/assets/image2.png)
![Screenshot](https://raw.githubusercontent.com/estone96/GenPackageDoseResponse/main/assets/image3.png)

3. If drug potency varies among drug groups, apply weights for generalization of potency among drug groups.
![Screenshot](https://raw.githubusercontent.com/estone96/GenPackageDoseResponse/main/assets/image4.png)

4. If it is believed that transformation of variable (cumulative drug usage) is required, please select drug transformation measures.
![Screenshot](https://raw.githubusercontent.com/estone96/GenPackageDoseResponse/main/assets/image5.png)

5. Finally, click get package button in sidebar to access your package.

6. To operate your study, follow instructions below:
Add below details of your CDM database in "CodeToRun.R"
```
cdmDatabaseSchema <- "" ## server_database.server_scheme 
resultsDatabaseSchema <- ""

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "", #"postgresql"
                                                                server = "",
                                                                user = "",
                                                                password = "",
                                                                port = NULL)
```

When error caused by missing JDBC drivers, download required JDBC drivers using:
```
Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "C:/JDBC") 
downloadJdbcDrivers() #postgresql, redshift .. etc
``` 
