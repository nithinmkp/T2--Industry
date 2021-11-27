#packages
packages<-c("tidyverse","janitor","glue","waldo","dataCompareR","stringr","stringi",
            "readxl","rio","openxlsx")
#sapply(packages,install.packages,character.only=T)
sapply(packages,library,character.only=T)

#Read master data
master_data<-read_excel("coir (2).xlsx")
master_data<-master_data %>% remove_empty("rows")


#Compare data
check_data<-import_list("Coir OE 13 FYP.xlsx")
check_data<-check_data %>% map(~.x %>% select(-c(5,7)))

#some data cleaning on comapre sheets
years<-names(check_data)

#function to paste column names
rename_fn<-function(df,yr,cols=4:6){
  names(df)[cols]<-paste(yr,names(df)[cols],sep = "_")
  return(df)
}
check_data<-map2(check_data,years,~rename_fn(df=.x,
                                             yr=.y))

#some wrangling on master data
which(master_data$...3=="Total")
master_data<-master_data %>% slice(1:(which(master_data$...3=="Total")-1))
names(master_data)[1:3]<-master_data[2,1:3]
master_data[1,-c(1:3)]<-as.list(rep(rev(years),
                                      each=(ncol(master_data)-3)/length(years)))
names(master_data)[-c(1:3)]<-paste(master_data[1,-c(1:3)],
                                   master_data[2,-c(1:3)],sep = "_")
master_data<-master_data %>% slice(-c(1:2))
master_data$`Scheme Code`[9]<-"VSI 080" #Missing scheme code

master_data<-master_data %>% fill(`Scheme Code`)

delete_na<-function(df,col_na=1){
  df1<-df[,-c(1:3)]
  ind<-which(rowSums(!is.na(df1)) >= 1)
  df<-df[ind,]
}
master_copy<-delete_na(master_data)
master_copy[,-c(2:3)]<-map(master_copy[,-c(2:3)],as.numeric)


#master_copy<-master_copy %>% adorn_totals("row")
#for (i in seq(6,18,by=3)){
#  t2[20,i]<-(t2[20,(i-1)]/t2[20,(i-2)])*100
#}

#adjusting compare dataset

compare1<-check_data %>% reduce(full_join)
compare1<-compare1 %>% select(1:3,16:18,13:15,10:12,7:9,4:6)
compare1<-compare1 %>% filter(`Scheme Code` %in% master_copy$`Scheme Code`)

compare2<-inner_join(master_copy[,c(1:3)],compare1[,c(1:3)],
                     by = c( "Scheme Code", "Scheme Name"))
compare3<-inner_join(compare1[,c(1:3)],master_copy[,c(1:3)],
                     by = c( "Scheme Code", "Scheme Name"))
compare3<-compare1 %>% filter(`Scheme Name` %in% compare2$`Scheme Name`)
compare4<-semi_join(master_copy,compare1,by=c( "Scheme Code"))

write.xlsx(compare1,"check.xlsx")
write.xlsx(master_copy,"master.xlsx")
