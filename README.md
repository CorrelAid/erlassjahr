# Erlassjahr

This repository contains all materials that were used to setup a (semi-)automated workflow and to deploy a shinyapp that interactively visualizes the annual report data for countreis debt situation by erlassjahr.de.

The goal of CorrelAid was to help the team of erlassjahr to automate their data generation process and to develop and implement an interactive web application where users can explore the debt situation of countries, complementary to their annual report.



## Repo structure

Below is a (reduced) structure of the repo. 

```
.Rprofile
.gitignore
Anleitung_Update_Karte.docx
Anleitung_zur_Excel_Vorlage.docx
data
   |-- final_data_2019.RData
   |-- final_data_2020.RData
erlassjahr.Rproj
erlassjahr_map
   |-- app.R
   |-- app_en.R
   |-- data
   |   |-- final_data_2020.RData
   |   |-- year_data.Rdata
   |-- map_files
   |   |-- ...
   |-- modules
   |   |-- graphics.R
   |   |-- legends.R
   |-- rsconnect
   |   |-- shinyapps.io
   |   |   |-- erlassjahr
   |   |   |   |-- erlassjahr_map.dcf
   |-- symbols
   |   |-- ...
   |-- www
   |   |-- ...
main.Rmd
renv.lock
renv
   |-- ...
scripts
   |-- data_preparation.R
   |-- functions.R
   |-- template_setup.R
```

The repo is structure in 4 main components.

#### root

Besides files to setup the Rproject and to make sure that all dependencies are loaded (using [renv]()), the root has three important files that detail how to use this repo to setup the automated data generation and how to publish the shinyapp (for more details, see the project description here).

- `main.Rmd` is the main file where all scripts can and should be exectued from
- `Anleitung_zur_Excel_Vorlage.docx` is a description of how to execute the first part of the `main.Rmd` to generate a new Excel file containing the static information from the previous year as well as scraping information from the World Bank API (using the [WDI]() package in R). To allow further comments and fully customization, this file is a .docx
- `Anleitung_Update_Karte.docx` details the second part in the `main.Rmd` and specifies the steps required to automatically categorize states and how to eventually update and deploy the shinyapp. Again, for customization, this file is a .docx.

#### data

The data folder is where the final data for all debt years are stored. 

#### erlassjahr_map

This is the folder that is used to generate the shinyapp. There are two app files: `app.R` is for the German version and `app_en.R` for the English version of the map. Both source the same files in the subfolders, i.e., the data, the helper functions in `modules` or any other source files.

#### scripts

- `data_preparation.R` generates the new Excel file for a given year
- `functions.R` contains helper functions
- `template_setup.R` is the original script that was used to generate the first automated version based on the raw dataset for the debt report in 2020.
