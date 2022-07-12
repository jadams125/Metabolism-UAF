# Plot Epscor Data

library(googledrive)
library(purrr)
library(tidyverse)




#### CRAWFORD UPSTREAM ####


## store the URL you have
Craw_us_url <- "https://drive.google.com/drive/u/1/folders/1k0Jncre-QGQff14gl4h3Ny7D9L_PX8go"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
craw_US_Folder <- drive_get(as_id(Craw_us_url))

## identify the csv files in that folder
craw_US_txt_files <- drive_ls(craw_US_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Upstream")
walk(craw_US_txt_files$id, ~ drive_download(as_id(.x)))

filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Upstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Upstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW")
### Stitch together manually downloaded MINI DOT data ###
craw_US_file_list <- list.files(path = "./upstream/", 
                            recursive=F, 
                            pattern=".csv", 
                            full.names=TRUE)

craw_us_data <-do.call("rbind", lapply(craw_US_file_list, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                    header=TRUE))



plot(craw_us_data$Time..sec., craw_us_data$DO..mg.l.)




#### CRAW DOWNSTREAM #### 


## store the URL you have
Craw_ds_url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
craw_DS_Folder <- drive_get(as_id(Craw_ds_url))

## identify the csv files in that folder
craw_DS_txt_files <- drive_ls(craw_DS_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Downstream")
walk(craw_DS_txt_files$id, ~ drive_download(as_id(.x)))

filelistCrawDS = list.files(pattern = ".txt")
for (i in 1:length(filelistCrawDS)){
  input<-filelistCrawDS[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Downstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW/Downstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/CRAW")
### Stitch together manually downloaded MINI DOT data ###
craw_DS_file_list <- list.files(path = "./Downstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

craw_DS_data <-do.call("rbind", lapply(craw_DS_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



plot(craw_DS_data$Time..sec., craw_DS_data$DO..mg.l.)



#### MAST UPSTREAM ####

## store the URL you have
Mast_us_url <- "https://drive.google.com/drive/u/1/folders/1lolgjQUg1w1gpHP5ZXPXhzr20raDXnyW"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
mast_US_Folder <- drive_get(as_id(Mast_us_url))

## identify the csv files in that folder
mast_US_txt_files <- drive_ls(mast_US_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Upstream")
walk(mast_US_txt_files$id, ~ drive_download(as_id(.x)))

filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Upstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Upstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST")
### Stitch together manually downloaded MINI DOT data ###
mast_US_file_list <- list.files(path = "./upstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

mast_us_data <-do.call("rbind", lapply(mast_US_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



plot(mast_us_data$Time..sec., mast_us_data$DO..mg.l.)





#### MAST DOWNSTREAM ####


## store the URL you have
Mast_ds_url <- "https://drive.google.com/drive/u/1/folders/1lpcvDF6eYxIIbpL63soPYTzjjlgmp3gA"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
mast_DS_Folder <- drive_get(as_id(Mast_ds_url))

## identify the csv files in that folder
mast_DS_txt_files <- drive_ls(mast_DS_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Downstream")
walk(mast_DS_txt_files$id, ~ drive_download(as_id(.x)))

filelistMastDS = list.files(pattern = ".txt")
for (i in 1:length(filelistMastDS)){
  input<-filelistMastDS[i]
  output<-paste0(input, "Z.csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Downstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST/Downstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/MAST")
### Stitch together manually downloaded MINI DOT data ###
mast_DS_file_list <- list.files(path = "./Downstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

mast_DS_data <-do.call("rbind", lapply(mast_DS_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



plot(mast_DS_data$Time..sec., mast_DS_data$DO..mg.l.)



