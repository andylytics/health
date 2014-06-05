# create list of URLs
fileUrl <- c("http://www.medicare.gov/download/HospitalCompare/2014/January/HOSArchive_Revised_Flatfiles_20140101.zip",
             "http://medicare.gov/download/HospitalCompare/2013/October/HOSArchive_Revised_Flatfiles_20131001.zip",
             "http://medicare.gov/download/HospitalCompare/2013/July/HOSArchive_Revised_Flatfiles_20130701.zip",
             "http://medicare.gov/download/HospitalCompare/2013/April/HOSArchive_Revised_Flatfiles_20130401.zip",
             "http://medicare.gov/download/HospitalCompare/2012/October/HOSArchive_Revised_Flatfiles_20121001.zip",
             "http://medicare.gov/download/HospitalCompare/2012/July/HOSArchive_Revised_Flatfiles_20120701.zip")

i <- 1
for (i in i:length(fileUrl)){
  # set zip file name
  file_name <- substr(fileUrl[i], nchar(fileUrl[i]) - 11, nchar(fileUrl[i]))
  
  #dir_name <- substr(fileUrl, nchar(fileUrl) - 11, nchar(fileUrl) - 4)
  
  # download file to working directory
  if (file.exists(file_name) == FALSE){
    download.file(fileUrl[i], destfile = file_name)
  }
  
  #dir.create(dir_name)
  #unzip(file_name, exdir = dir_name)
  
  #list.files(dir_name)
  
  #df <- read.table(paste(dir_name, "HCAHPS Measures - National.csv", sep = "/"), header = TRUE, sep = ",")
  
  # read tables
  library(dplyr)
  
  #NOTE: need to grep for HCAHPS and national
  df <- read.csv(unz(file_name, "HCAHPS Measures - National.csv"), sep = ",", stringsAsFactors = FALSE)
  nat_avg <- as.numeric(df[df$HCAHPS.Answer.Description == "Patients who gave a rating of 9 or 10 (high)", 3])
  
  
  md <- read.csv(unz(file_name, "Measure Dates.csv"), sep = ",", stringsAsFactors = FALSE)
  md <- md[grep("satisfaction", md[,1]), ]
  
  state <- read.csv(unz(file_name, "HCAHPS Measures - State.csv"), sep = ",", stringsAsFactors = FALSE)
  state <- as.numeric(state[grep("RI", state[,1]), grep("9.or.10", names(state))])
  
  hosp <- read.csv(unz(file_name, "HCAHPS Measures.csv"), sep = ",", stringsAsFactors = FALSE)
  hosp <- hosp[hosp$Provider.Number %in% c(410001, 410009, 410010), c(1:2, grep("9.or.10", names(hosp)))]
  
  # arrange hospitals in order
  hosp <- arrange(hosp, Provider.Number)
  
  labels <- c("Nat_Avg", "State_Avg", "MHRI", "KH", "WIH")
  values <- as.vector(c(nat_avg, state, hosp[,3]))
  data <- data.frame(variable = labels, values = values)
  data$Measure <- "Patients who gave a rating of 9 or 10 (high)"
  data$StartDate <- md[1, "Measure.Start.Date"]
  data$EndDate <- md[1, "Measure.End.Date"]
  data$fileset <- substr(file_name, 1, nchar(file_name) - 4)
  
  if (i == 1){
    data_f <- data
  }
  else {
    data_f <- rbind_list(data_f, data)
  }
}




# take the average




