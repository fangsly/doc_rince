library(xlsx)
data_all <- NULL
                   
setwd("data\\merge")
fileName <- list.files()
for (i in 1:length(fileName))
{
  data <- read.xlsx2(fileName[i],sheetIndex=1, header=TRUE)
  data_all <- rbind(data_all,data)
}

data_all[data_all == "ERROR"] <- NA

splitintwo <- function(x,str) {
  
  dataframe <- NULL
  datanew <- NULL
  dataframe <- data.frame(str1=character(),str2=character())
  str1 <- paste(str,"_val",sep = "")
  str2 <- paste(str,"_per",sep = "")
  dataframe <- setNames(dataframe, c(str1,str2))
  
  for (i in 1:length(x))
  {
    if (is.na(x[i]))
    {
      datanew <- data.frame(NA, NA)
      datanew <- setNames(datanew, c(str1,str2))
      dataframe <- rbind(dataframe,datanew)
    }
    else if (grepl("\\%",x[i])==FALSE)
    {
      datanew <- data.frame(x[i], "0")
      datanew <- setNames(datanew, c(str1,str2))
      dataframe <- rbind(dataframe,datanew)
    }
    else{
      r <- regexpr("(.*?) \\(", x[i])
      m <- regmatches(x[i], r)
      number <- gsub(" \\(","",m)
      
      r <- regexpr("\\((.*?)%\\)", x[i])
      m <- regmatches(x[i], r)
      t <- gsub("\\(","",m)
      percent <- gsub("%\\)","",t)
      #final <- list(number, percent)
      datanew <- data.frame(number, percent)
      datanew <- setNames(datanew, c(str1,str2))
      dataframe <- rbind(dataframe,datanew)
    }
    
  }
  return(dataframe)
}

drops <- c("N_pub_c",
           "N_sc",
           "N_cco",
           "N_for",
           "N_rus",
           "N_VAK",
           "N_tr",
           "N_imp",
           "C_for",
           "C_rus",
           "C_VAK",
           "C_tr",
           "C_imp",
           "N_pub_5",
           "N_c_5",
           "N_cpub_5")
for (i in 1:length(drops))
{
  keep<-drops[i]
  dd<-data_all[ , keep]
  newcols<- splitintwo(dd,drops[i])
  data_all<-cbind(data_all,newcols)
}
#newcols<- splitintwo(data_all$N_pub_c,"N_pub_c")
#data_all<-cbind(data_all,newcols)

data_all<-data_all[ , !(names(data_all) %in% drops)]
namenew <-NULL

  for (i in 1:length(data_all$Name))
  {

    if (grepl("\\*",data_all$Name[i])[1]==TRUE)
    {
      r <- regexpr("(.*?) \\*", data_all$Name[i])
      m <- regmatches(data_all$Name[i], r)
      namenew[i] <- gsub("  \\*","",m)[1]
      data_all$Reg[i] <- 1
    }
    else
    {
      data_all$Reg[i] <- 0
      namenew[i] <- as.character(data_all$Name[i])
    }
    
  }


#newcols<- splitintwo(data_all$N_pub_c,"N_pub_c")
drops <- c("Name")
data_all<-data_all[ , !(names(data_all) %in% drops)]
namenew <- setNames(namenew, c("Name"))
data_all<-cbind(data_all,namenew)

splitplace <- function(x,str) {
  
  dataframe <- NULL
  datanew <- NULL
  dataframe <- data.frame(str1=character(),str2=character())
  str1 <- paste(str,"_name",sep = "")
  str2 <- paste(str,"_city",sep = "")
  dataframe <- setNames(dataframe, c(str1,str2))
  
  for (i in 1:length(x))
  {
    if (is.na(x[i]))
    {
      datanew <- data.frame(NA, NA)
      datanew <- setNames(datanew, c(str1,str2))
      dataframe <- rbind(dataframe,datanew)
    }
    else{
      r <- regexpr("*(.*?) \\([^\\(]*", x[i])
      m <- regmatches(x[i], r)
      name <- gsub(" \\(","",m)[1]
      
      r <- regexpr("\\((.*?)\\)$", x[i])
      m <- regmatches(x[i], r)
      city <- sub("\\).*", "", sub(".*\\(", "", m))[1] 

      datanew <- data.frame(name, city)
      datanew <- setNames(datanew, c(str1,str2))
      dataframe <- rbind(dataframe,datanew)
    }
    
  }
  return(dataframe)
}
newcols<- splitplace(data_all$Place,"Place")
# x<-data_all$Place
# str <-"Place"
data_all<-cbind(data_all,newcols)
drops <- c("Place")
data_all<-data_all[ , !(names(data_all) %in% drops)]

deduped.data <- unique( data_all[ , 2:59 ] )

library(xlsx)

write.xlsx(deduped.data, "mydata_all.xlsx")

sum(is.na(deduped.data$N_elibrary))/nrow(deduped.data)*100

