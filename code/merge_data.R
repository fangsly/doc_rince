library(readxl)
library(xlsx)
library(reshape2)
dis <- read_excel("data/final_1000_merge.xlsx")
rince <- read_excel("data/doc_rince_1000.xlsx")


dis <- read_excel("data/doctors_only.xlsx")
rince <- read_excel("data/one match id.xlsx")
all<-merge(dis, rince, by.x = "ID", by.y = "ID",all.x=T)

write.xlsx(all,"merged_rince_all.xlsx")