library(RSelenium)
library(docker)
library(rvest)
library(XML)
library(readxl)
library(httr)
library(htmltools)
library(dplyr)
library(xlsx)
library(stringr)
library(reshape2)
proxy <- read_excel("data/proxies.xlsx")
id_all <- scan("data\\id_doc.txt", what="", sep="\n")
id <- as.list(id_all)

error_code=TRUE


j=3
#N=length(id)
N=10

dpf <-NULL
dpe<-NULL
dpf <- data.frame(ID=numeric(),
                  Name_place=character(),
                  Place_period=character(),
                  Place_pub=character())
dpf <- setNames(dpe, c("ID","Name_place","Place_period","Place_pub"))


df <-NULL
de<-NULL

df <- data.frame(ID=numeric(),
                 Name=character(),
                 Place=character(),
                 Spin=character(),
                 N_elibrary=character(),
                 N_RINC=character(),
                 N_RINC_core=character(),
                 C_elibrary=character(),
                 C_RINC=character(),
                 C_RINC_core=character(),
                 H_elibrary=character(),
                 H_RINC=character(),
                 H_RINC_core=character(),
                 N_c=character(),
                 N_c_pub=character(),
                 N_pub_c=character(),
                 Mean_pub=character(),
                 H_wo_sc=character(),
                 H_pub_j=character(),
                 Year_pub=character(),
                 N_sc=character(),
                 N_cco=character(),
                 N_co=character(),
                 N_for=character(),
                 N_rus=character(),
                 N_VAK=character(),
                 N_tr=character(),
                 N_imp=character(),
                 C_for=character(),
                 C_rus=character(),
                 C_VAK=character(),
                 C_tr=character(),
                 C_imp=character(),
                 Imp_j_pub=character(),
                 Imp_j_c=character(),
                 N_pub_5=character(),
                 N_c_5=character(),
                 N_cpub_5=character(),
                 Category=character(),
                 Value=character())
df <- setNames(df, c("ID",
                     "Name",
                     "Place",
                     "Spin",
                     "N_elibrary",
                     "N_RINC",
                     "N_RINC_core",
                     "C_elibrary",
                     "C_RINC",
                     "C_RINC_core",
                     "H_elibrary",
                     "H_RINC",
                     "H_RINC_core",
                     "N_c",
                     "N_c_pub",
                     "N_pub_c",
                     "Mean_pub",
                     "H_wo_sc",
                     "H_pub_j",
                     "Year_pub",
                     "N_sc",
                     "N_cco",
                     "N_co",
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
                     "Imp_j_pub",
                     "Imp_j_c",
                     "N_pub_5",
                     "N_c_5",
                     "N_cpub_5",
                     "Category",
                     "Value"))

for (i in 1:N)
{
  
  
  str_author_profile <- paste("https://elibrary.ru/author_profile.asp?id=",toString(id[i]),sep = "")
  str_author_rubrics <- paste("https://elibrary.ru/author_profile_new_rubrics.asp?id=",toString(id[i]),sep = "")

  
  while (error_code==TRUE && j!=(length(proxy$ips)))
  {
    error_message1<-tryCatch(html_session(str_author_profile, use_proxy(proxy$ips[j], proxy$ports[j])),
                            error = function(e) {"error"},
                            warning = function(w) {"error"})
    error_message2<-tryCatch(html_session(str_author_rubrics, use_proxy(proxy$ips[j], proxy$ports[j])),
                             error = function(e) {"error"},
                             warning = function(w) {"error"})
    
    print(error_message1)
    print(error_message2)
    error1<-str_detect(error_message1,"error")
    error2<-str_detect(error_message1,"error")
    error<-rbind(error1,error2)
    
    if (sum(error)!=0) 
    {
      error_code=TRUE
      j=j+1
    }
    
    if (sum(error)==0 && (str_detect(error_message1$url,"ip_restricted")||str_detect(error_message1$url,"page_404")||str_detect(error_message2$url,"ip_restricted")||str_detect(error_message2$url,"page_404")))
    {
      error_code=TRUE
      j=j+1
    }
    else if(sum(error)==0 )
    {
      error_code=FALSE
    }
    if (i!=length(id) && j==(length(proxy$ips))) j=1
    print(paste("error_code=",error_code))
    print(paste("j=",j))
  }  
  
  session1<-error_message1
  session2<-error_message2
  
  author_profile <- read_html(session1)
  author_rubrics <- read_html(session2)
  
  main_inf <-author_profile %>%
    html_nodes("table") %>%
    .[[22]] %>%
    html_table(fill=TRUE)
  
  r <- regexpr("(.*?)\\r\\n", main_inf)
  m <- regmatches(main_inf, r)
  name <- gsub("\\r\\n","",m)[1]
  
  r <- regexpr("\\r\\n(.*?)\\r\\n", main_inf)
  m <- regmatches(main_inf, r)
  place <- gsub("\\r\\n","",m)[1]
  
  r <- regexpr("SPIN-???: (.*?),", main_inf)
  m <- regmatches(main_inf, r)
  t <- gsub("SPIN-???: ","",m)[1]
  spin <- gsub(",","",t)[1]
  
  main_places <-author_profile %>%
    html_nodes("table") %>%
    .[[23]] %>%
    html_table(fill=TRUE)
  
  count_place <- nrow(main_places)-3
  if (count_place > 0 && ncol(main_places)>3)
  {
    for (jj in 1:count_place)
    {
      dpe<-data.frame(c(id[i],main_places[jj+3,2],main_places[jj+3,3],main_places[jj+3,4]))
      dpe <- setNames(dpe, c("ID","Name_place","Place_period","Place_pub"))
      dpf <- rbind(dpf,dpe)
    }
    
    
    
    main_stat <-author_profile %>%
      html_nodes("table") %>%
      .[[24]] %>%
      html_table(fill=TRUE)
    
    n_elibrary <-main_stat[4,3]
    n_RINC <-main_stat[5,3]
    n_RINC_core <-main_stat[6,3]
    
    c_elibrary <-main_stat[8,3]
    c_RINC <-main_stat[9,3]
    c_RINC_core <-main_stat[10,3]
    
    h_elibrary <-main_stat[12,3]
    h_RINC <-main_stat[13,3]
    h_RINC_core <-main_stat[14,3]
    
    n_c <-main_stat[16,3]
    n_c_pub <-main_stat[17,3]
    n_pub_c <-main_stat[18,3]
    mean_pub <-main_stat[19,3]
    
    h_wo_sc <-main_stat[21,3]
    h_pub_j <-main_stat[22,3]
    year_pub <-main_stat[23,3]
    
    n_sc<-main_stat[25,3]
    n_cco <-main_stat[26,3]
    n_co <-main_stat[27,3]
    
    n_for <-main_stat[29,3]
    n_rus <-main_stat[30,3]
    n_VAK <-main_stat[31,3]
    n_tr <-main_stat[32,3]
    n_imp <-main_stat[33,3]
    
    
    c_for <-main_stat[35,3]
    c_rus <-main_stat[36,3]
    c_VAK <-main_stat[37,3]
    c_tr <-main_stat[38,3]
    c_imp <-main_stat[39,3]
    
    imp_j_pub <-main_stat[41,3]
    imp_j_c <-main_stat[42,3]
    
    n_pub_5 <-main_stat[44,3]
    n_c_5 <-main_stat[45,3]
    n_cpub_5 <-main_stat[46,3]
    
    
    discipline <-author_rubrics %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table(fill=TRUE)
    category <- discipline[3,2]
    value <- discipline[3,3]
    
    
    de<-data.frame(c(id[i],
                     name,
                     place,
                     spin,
                     n_elibrary,
                     n_RINC,
                     n_RINC_core,
                     c_elibrary,
                     c_RINC,
                     c_RINC_core,
                     h_elibrary,
                     h_RINC,
                     h_RINC_core,
                     n_c,
                     n_c_pub,
                     n_pub_c,
                     mean_pub,
                     h_wo_sc,
                     h_pub_j,
                     year_pub,
                     n_sc,
                     n_cco,
                     n_co,
                     n_for,
                     n_rus,
                     n_VAK,
                     n_tr,
                     n_imp,
                     c_for,
                     c_rus,
                     c_VAK,
                     c_tr,
                     c_imp,
                     imp_j_pub,
                     imp_j_c,
                     n_pub_5,
                     n_c_5,
                     n_cpub_5,
                     category,
                     value))
    de <- setNames(de, c("ID",
                         "Name",
                         "Place",
                         "Spin",
                         "N_elibrary",
                         "N_RINC",
                         "N_RINC_core",
                         "C_elibrary",
                         "C_RINC",
                         "C_RINC_core",
                         "H_elibrary",
                         "H_RINC",
                         "H_RINC_core",
                         "N_c",
                         "N_c_pub",
                         "N_pub_c",
                         "Mean_pub",
                         "H_wo_sc",
                         "H_pub_j",
                         "Year_pub",
                         "N_sc",
                         "N_cco",
                         "N_co",
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
                         "Imp_j_pub",
                         "Imp_j_c",
                         "N_pub_5",
                         "N_c_5",
                         "N_cpub_5",
                         "Category",
                         "Value"))
    
    
    
    #df <- rbind(df, de)
    #("ID","category","Value")
    df <- rbind(df,de)
    #df<-rbind(df, setNames(de, names(df)))
    #df<-rbind(df, data.frame(Author=id[i],Value = value))
    #Sys.sleep(sample(10, 1) * 0.1)
    
  }
  else
  {
    dpe<-data.frame(c(id[i],NA,NA,NA))
    dpe <- setNames(dpe, c("ID","Name_place","Place_period","Place_pub"))
    dpf <- rbind(dpf,dpe)
    
    main_stat <-author_profile %>%
      html_nodes("table") %>%
      .[[23]] %>%
      html_table(fill=TRUE)
    
    n_elibrary <-main_stat[4,3]
    n_RINC <-main_stat[5,3]
    n_RINC_core <-main_stat[6,3]
    
    c_elibrary <-main_stat[8,3]
    c_RINC <-main_stat[9,3]
    c_RINC_core <-main_stat[10,3]
    
    h_elibrary <-main_stat[12,3]
    h_RINC <-main_stat[13,3]
    h_RINC_core <-main_stat[14,3]
    
    n_c <-main_stat[16,3]
    n_c_pub <-main_stat[17,3]
    n_pub_c <-main_stat[18,3]
    mean_pub <-main_stat[19,3]
    
    h_wo_sc <-main_stat[21,3]
    h_pub_j <-main_stat[22,3]
    year_pub <-main_stat[23,3]
    
    n_sc<-main_stat[25,3]
    n_cco <-main_stat[26,3]
    n_co <-main_stat[27,3]
    
    n_for <-main_stat[29,3]
    n_rus <-main_stat[30,3]
    n_VAK <-main_stat[31,3]
    n_tr <-main_stat[32,3]
    n_imp <-main_stat[33,3]
    
    
    c_for <-main_stat[35,3]
    c_rus <-main_stat[36,3]
    c_VAK <-main_stat[37,3]
    c_tr <-main_stat[38,3]
    c_imp <-main_stat[39,3]
    
    imp_j_pub <-main_stat[41,3]
    imp_j_c <-main_stat[42,3]
    
    n_pub_5 <-main_stat[44,3]
    n_c_5 <-main_stat[45,3]
    n_cpub_5 <-main_stat[46,3]
    
    
    discipline <-author_rubrics %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table(fill=TRUE)
    category <- discipline[3,2]
    value <- discipline[3,3]
    
    
    de<-data.frame(c(id[i],
                     name,
                     place,
                     spin,
                     n_elibrary,
                     n_RINC,
                     n_RINC_core,
                     c_elibrary,
                     c_RINC,
                     c_RINC_core,
                     h_elibrary,
                     h_RINC,
                     h_RINC_core,
                     n_c,
                     n_c_pub,
                     n_pub_c,
                     mean_pub,
                     h_wo_sc,
                     h_pub_j,
                     year_pub,
                     n_sc,
                     n_cco,
                     n_co,
                     n_for,
                     n_rus,
                     n_VAK,
                     n_tr,
                     n_imp,
                     c_for,
                     c_rus,
                     c_VAK,
                     c_tr,
                     c_imp,
                     imp_j_pub,
                     imp_j_c,
                     n_pub_5,
                     n_c_5,
                     n_cpub_5,
                     category,
                     value))
    de <- setNames(de, c("ID",
                         "Name",
                         "Place",
                         "Spin",
                         "N_elibrary",
                         "N_RINC",
                         "N_RINC_core",
                         "C_elibrary",
                         "C_RINC",
                         "C_RINC_core",
                         "H_elibrary",
                         "H_RINC",
                         "H_RINC_core",
                         "N_c",
                         "N_c_pub",
                         "N_pub_c",
                         "Mean_pub",
                         "H_wo_sc",
                         "H_pub_j",
                         "Year_pub",
                         "N_sc",
                         "N_cco",
                         "N_co",
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
                         "Imp_j_pub",
                         "Imp_j_c",
                         "N_pub_5",
                         "N_c_5",
                         "N_cpub_5",
                         "Category",
                         "Value"))
    
    
    
    #df <- rbind(df, de)
    #("ID","category","Value")
    df <- rbind(df,de)
    #df<-rbind(df, setNames(de, names(df)))
    #df<-rbind(df, data.frame(Author=id[i],Value = value))
    #Sys.sleep(sample(10, 1) * 0.1)
    
    
  }
  
  
  if ((i%%3)==0) Sys.sleep(5)
  if ((i%%20)==0) Sys.sleep(100)
  print(paste("i=",i))
  error_code=TRUE
  if (((i%%250)==0))
  {
    
    date1<-str_replace_all(Sys.time(), " ", "-")
    date<-str_replace_all(date1, ":", "-")
    file_name1<-paste("doc_rince_",date,"_",i,".xlsx",sep="")
    write.xlsx(df[(i-249):i,],file_name1)
    file_name2<-paste("doc_rince_places_",date,"_",i,".xlsx",sep="")
    write.xlsx(dpf[(i-249):i,],file_name2)
    Sys.sleep(500)
  }
  
}

date1<-str_replace_all(Sys.time(), " ", "-")
date<-str_replace_all(date1, ":", "-")
file_name1<-paste("uni_rince_",date,".xlsx",sep="")
write.xlsx(df,file_name1)
file_name2<-paste("uni_rince_places_",date,".xlsx",sep="")
write.xlsx(dpf,file_name2)

