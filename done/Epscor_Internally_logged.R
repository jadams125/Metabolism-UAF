# Plot Epscor Data

library(googledrive)
library(purrr)
library(tidyverse)
library(ggplot2)




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

craw_us_data$datetimeAK <- force_tz(as_datetime(craw_us_data$Time..sec.), "America/Anchorage")



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
craw.US.DO.plot <- ggplot(data=craw_us_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Crawford Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                       r = 20,  # Right margin
                       b = 20,  # Bottom margin
                       l = 20)) # Left margin

craw.US.DO.plot

pdf("plots/craw.US.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(craw.US.DO.plot)   ## prints gram_ing_plot into the open device
dev.off() 




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



craw_DS_data$datetimeAK <- force_tz(as_datetime(craw_DS_data$Time..sec.), "America/Anchorage")



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
craw.DS.DO.plot <- ggplot(data=craw_DS_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Crawford Downstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

craw.DS.DO.plot

pdf("plots/craw.DS.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(craw.DS.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()



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



mast_us_data$datetimeAK <- force_tz(as_datetime(mast_us_data$Time..sec.), "America/Anchorage")



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
mast.US.DO.plot <- ggplot(data=mast_us_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Mastodon Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

mast.US.DO.plot

pdf("plots/mast.US.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(mast.US.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()





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



mast_DS_data$datetimeAK <- force_tz(as_datetime(mast_DS_data$Time..sec.), "America/Anchorage")



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
mast.DS.DO.plot <- ggplot(data=mast_DS_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Mastodon Downstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

mast.DS.DO.plot

pdf("plots/mast.DS.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(mast.DS.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()





#### SHOVEL UPSTREAM ####


## store the URL you have
Shov_us_url <- "https://drive.google.com/drive/u/1/folders/14aBdxBqjZJ99FEuYTfrt5kPBuP9wLcY-"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
shov_US_Folder <- drive_get(as_id(Shov_us_url))

## identify the csv files in that folder
shov_US_txt_files <- drive_ls(shov_US_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Upstream")
walk(shov_US_txt_files$id, ~ drive_download(as_id(.x)))

filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Upstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Upstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV")
### Stitch together manually downloaded MINI DOT data ###
shov_US_file_list <- list.files(path = "./upstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

shov_us_data <-do.call("rbind", lapply(shov_US_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))



shov_us_data$datetimeAK <- force_tz(as_datetime(shov_us_data$Time..sec.), "America/Anchorage")



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
shov.US.DO.plot <- ggplot(data=shov_us_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Shovel Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

shov.US.DO.plot

pdf("plots/shov.US.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(shov.US.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()




#### SHOV DOWNSTREAM #### 


## store the URL you have
Shov_ds_url <- "https://drive.google.com/drive/u/1/folders/14WmUFu4jJCwZyKlI9QgqUQphk_zO2jlp"

## identify this folder on Drive
## let googledrive know this is a file ID or URL, as opposed to file name
shov_DS_Folder <- drive_get(as_id(Shov_ds_url))

## identify the csv files in that folder
shov_DS_txt_files <- drive_ls(shov_DS_Folder, type = "txt")

## download them
setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Downstream")
walk(shov_DS_txt_files$id, ~ drive_download(as_id(.x)))

filelistShovDS = list.files(pattern = ".txt")
for (i in 1:length(filelistShovDS)){
  input<-filelistShovDS[i]
  output<-paste0(input, ".csv")
  print(paste("Processing the file:", input))
  data = read.delim(input, header = TRUE, skip = 2, sep = ",")  
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Downstream")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV/Downstream")
}

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/SHOV")
### Stitch together manually downloaded MINI DOT data ###
shov_DS_file_list <- list.files(path = "./Downstream/", 
                                recursive=F, 
                                pattern=".csv", 
                                full.names=TRUE)

shov_DS_data <-do.call("rbind", lapply(shov_DS_file_list, 
                                       read.csv, 
                                       stringsAsFactors=FALSE, 
                                       header=TRUE))




shov_DS_data$datetimeAK <- force_tz(as_datetime(shov_DS_data$Time..sec.), "America/Anchorage")



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF")
shov.DS.DO.plot <- ggplot(data=shov_DS_data, aes(y=DO..mg.l., x=datetimeAK)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Dissolved Oxygen (mg/L)")+
  ggtitle("Shovel Upstream Mini Dot")+
  theme(plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20)) # Left margin

shov.DS.DO.plot

pdf("plots/shov.DS.DO.plot.pdf")     ## opens a .pdf device with specified path and file name
print(shov.DS.DO.plot)   ## prints gram_ing_plot into the open device
dev.off()


