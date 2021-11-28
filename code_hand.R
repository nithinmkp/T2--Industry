#packages
packages<-c("tidyverse","janitor","glue","waldo","dataCompareR","stringr","stringi",
            "readxl","rio","openxlsx")
#sapply(packages,install.packages,character.only=T)
sapply(packages,library,character.only=T)

#Read master data
master_data<-import_list("handloom_new.xlsx")
master_data<-master_data %>% map(~remove_empty(.x,"rows"))

#some wrangling on master data
ind<-map_dbl(master_data,~which(.x[2]=="Total")) #last row of data
master_data<-map2(master_data,ind,~slice(.x,-.y))
years<-rev(names(master_data$Handloom)[seq(4,16,by=3)])


names_fn<-function(x){
  names(x)[1:3]<-x[1,1:3]
  names(x)[-c(1:3)]<-rep(rev(years),
                         each=(ncol(x)-3)/length(years))
  names(x)[-c(1:3)]<-paste(names(x)[-c(1:3)],
                           x[1,-c(1:3)],sep = "_")
  x<-x[-1,]
  return(x)
  
}

master_data<-master_data %>% map(names_fn)

#master_data$`Scheme Code`[9]<-"VSI 080" #Missing scheme code

master_data<-master_data %>% map(~.x %>% fill(`Scheme Code`))

#delete_na<-function(df,col_na=1){
#  df1<-df[,-c(1:3)]
#  ind<-which(rowSums(!is.na(df1)) >= 1)
#  df<-df[ind,]
#}
#master_copy<-delete_na(master_data)

function_num<-function(df){
  df[,-c(2:3)]<-map(df[,-c(2:3)],as.numeric)
  df<-arrange(df,df$`Scheme Code`)
  return(df)
}

master_data<-master_data %>% map(function_num)
#map(master_data,~lapply(.x,class))

master_copy<-master_data %>% map(~adorn_totals(.x,"row"))
#for (i in seq(6,18,by=3)){
#  t2[20,i]<-(t2[20,(i-1)]/t2[20,(i-2)])*100
#}

tot_fn<-function(df,ind){
  for(i in seq(6,18,by=3)){
    df[(ind-1),i]<-(df[(ind-1),(i-1)]/df[(ind-1),(i-2)])*100
  }
  return(df)
}
master_copy<-map2(master_copy,ind,~tot_fn(df=.x,ind = .y))

#Compare data
check_data<-import_list("Handloom OE 13 FYP (1).xlsx")
check_data<-check_data %>% map(~.x %>% select(-c(5,7)) %>% 
                                 slice(-nrow(.x)))

#some data cleaning on comapre sheets

#function to paste column names
rename_fn<-function(df,yr,cols=4:6){
  names(df)[cols]<-paste(yr,names(df)[cols],sep = "_")
  return(df)
}
check_data<-map2(check_data,years,~rename_fn(df=.x,
                                             yr=.y))



#adjusting compare dataset

compare1<-check_data %>% reduce(full_join)

write.xlsx(compare1,"check_hand.xlsx")
write.xlsx(master_copy,"master_hand.xlsx")
