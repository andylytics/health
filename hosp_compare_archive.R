# create list of URLs
fileUrl <- c("http://medicare.gov/download/HospitalCompare/2014/April/HOSArchive_Revised_FlatFiles_20140417.zip",
             "http://www.medicare.gov/download/HospitalCompare/2014/January/HOSArchive_Revised_Flatfiles_20140101.zip",
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
  
  # create list of HCAHPS files
  hcf <- unzip(file_name, list = TRUE)[1]
  hcf <- grep("HCAHPS", hcf[,1], value = TRUE)
  
  # create Measure Dates value
  mdname <- unzip(file_name, list = TRUE)[1]
  mdname <- grep("Dates", mdname[,1], value = TRUE)
  
  #df <- read.table(paste(dir_name, "HCAHPS Measures - National.csv", sep = "/"), header = TRUE, sep = ",")
  
  # read tables
  library(dplyr)
  
  nat_avg <- read.csv(unz(file_name, grep("National", hcf, value = TRUE)), sep = ",", stringsAsFactors = FALSE, colClasses = "character")
  #nat_avg <- as.numeric(df[df$HCAHPS.Answer.Description == "Patients who gave a rating of 9 or 10 (high)", 3])
  nat_avg <- nat_avg %>%
    filter(HCAHPS.Answer.Description == grep("9 or 10", HCAHPS.Answer.Description, value = TRUE)) %>%
    select(HCAHPS.Answer.Percent)
  
  nat_avg <- data.frame(variable = "Nat_Avg", value = nat_avg$HCAHPS.Answer.Percent, stringsAsFactors = FALSE)
  
  state <- read.csv(unz(file_name, grep("State", hcf, value = TRUE)), sep = ",", stringsAsFactors = FALSE)
  #state <- as.numeric(state[grep("RI", state[,1]), grep("9.or.10", names(state))])
  
  state <- state %>%
    filter(State %in% c("RI", "MA")) %>%
    select(State, grep("9.or.10", names(state)))
    
  names(state) <- c("variable", "value")
  
  hosp <- read.csv(unz(file_name, grep("National|State", hcf, invert = TRUE, value = TRUE)), sep = ",", stringsAsFactors = FALSE, colClasses = "character")
  #hosp <- hosp[hosp$Provider.Number %in% c(410001, 410009, 410010), c(1:2, grep("9.or.10", names(hosp)))]
  #hosp <- hosp[hosp$Provider.Number %in% c(410001, 410009, 410010, 220110, 220071, 220101, 220086, 220176, 220171, 220116, 220163, 220020, 220074, 220011, 220073, 220088, 220008), c(1:2, grep("9.or.10", names(hosp)))]
  
  # vector of selected MA hospitals
  select.ma <- c(220110, 220071, 220101, 220086, 220176, 220171, 220116, 220163, 220020, 220074, 220011, 220073, 220088, 220008)
  
  # get list of RI hospitals
  select.ri <- hosp %>%
    filter(State == "RI") %>%
    select(Provider.Number)
  
  # convert to vector and append to MA hospitals
  select.ri <- as.vector(select.ri[,1])
  
  # append RI and MA hospitals
  select.hospitals <- append(select.ri, select.ma)
  rm(select.ri, select.ma)
  
  
  # limit to just hospitals in chosen State
  hosp <- hosp %>%
    filter(Provider.Number %in% select.hospitals) %>%
    select(Hospital.Name, grep("9.or.10", names(hosp)))
  
  names(hosp) <- c("variable", "value")
  
  
  
  # read Measure Dates
  md <- read.csv(unz(file_name, mdname), sep = ",", stringsAsFactors = FALSE)
  md1 <- md[grep("satisfaction", md[,1]), ]
  
  
  if (nrow(md1) == 0){
    md1 <- md[grep("Hospital CAHPS", md[,1]), ]
  }
  
  rm(md)
  
  # arrange hospitals in order
  hosp <- arrange(hosp, variable)
  
  data <- rbind_list(nat_avg, state, hosp)
  
  data$type[data$variable %in% c("Nat_Avg", "RI", "MA")] <- "Benchmark"
  data$type[!data$variable %in% c("Nat_Avg", "RI", "MA")] <- "Hospital"
  #labels <- c("Nat_Avg", "State_Avg", "MHRI", "KH", "WIH")
  #values <- as.vector(c(nat_avg, state, hosp[,3]))
  #data <- data.frame(variable = labels, values = values)
  data$Measure <- "Patients who gave a rating of 9 or 10 (high)"
  data$StartDate <- md1[1, "Measure.Start.Date"]
  data$EndDate <- md1[1, "Measure.End.Date"]
  #data$fileset <- substr(file_name, 1, nchar(file_name) - 4)
  
  if (i == 1){
    data_f <- data
  }
  else {
    data_f <- rbind_list(data_f, data)
  }
}

data_f <- unique(data_f)