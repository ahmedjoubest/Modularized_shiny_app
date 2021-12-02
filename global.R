### load needed packages--
packages <- c('shinydashboard', 'shinyWidgets', 'tidyverse', 'data.table', 'DT', 'plotly',
              'dplyr','stringi','shinycssloaders','shinyalert')
for(package in packages){
  library(package, character.only = T)
}

source("helpers.R")

### Reading data description ----
xx <- library(help = "datasets")
xx <- xx[[3]][2] %>% unlist()
Data_description <- 
  data.frame(
    dataset = c(),
    description = c()
  )
for(i in 1:length(xx)){
  row <- str_split(xx[i],"   ") %>% unlist()
  name <- row[1]
  if(stri_length(name)>0){
    df <- 
      data.frame(
        dataset = name,
        description = stri_paste(row[2:length(row)], collapse='') %>% 
          gsub("^ *|(?<= ) | *$", "", . , perl = TRUE)
      )
    Data_description <- rbind(Data_description, df)
  } else {
    description = stri_paste(row[2:length(row)], collapse='') %>% 
      gsub("^ *|(?<= ) | *$", "", . , perl = TRUE)
    Data_description$description[nrow(Data_description)] <- 
      paste(Data_description$description[nrow(Data_description)],description)
  }
}

### Drop items that are not data.frames (time series, etc...) ----
index <- c()
for(i in 1:nrow(Data_description)){
  print(i)
  name <- Data_description$dataset[i]
  bool = tryCatch(
    {bool = class(get(name)) != 'data.frame'}, 
    error=function(error_message){
      return(FALSE)
    }
  )
  if(bool){
    index <- c(index,i)
  }
}
Data_description <- Data_description[-c(index),]
### drop datasets-package ----
Data_description <- Data_description[-c(which(Data_description$dataset=='datasets-package')),]
### drop state table (does not exist) ----
Data_description <- Data_description[-c(which(Data_description$dataset=='state')),]

dtset <- Data_description$dataset


