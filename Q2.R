library(tidyverse)
library(magrittr)
library(easyr)
library(writexl)

df <- read_csv("globalcount_data.csv")
glimpse(df)

tobool
# changin to True/False
df <- df %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(c(vars(gender_not_listed:white), 
              vars(matches("issue"))), as.factor)

#general df
TOTAL<-df %>% group_by(country_of_residence) %>% summarise(n())
dt_agg<-data.frame(matrix(0,dim(TOTAL)[1],13)) #11 issues + name of country + total

dt_agg[,1]=TOTAL[,1]
dt_agg[,2]=TOTAL[,2]

A=df[df[,33]==1,] %>% group_by(country_of_residence) %>% summarise(n())

for (l in 1:11) {
  A=df[df[,34+l]==1,] %>% group_by(country_of_residence) %>% summarise(n())
  i=1
  j=1
  while (j<dim(A)[1]) {
    if (dt_agg[i,1]==A[j,1]){
      dt_agg[i,l+2]=A[j,2]
      j=j+1
      i=i+1
    }
    else { 
      dt_agg[i,l+2]=0
      i=i+1
    }
  }
}
dt_agg=dt_agg[1:155,]
df_country_issues=dt_agg[,-1]

rownames(df_country_issues)=dt_agg[,1]
colnames(df_country_issues)=c("total", colnames(df)[35:45])

df_country_issues/colSums(df_country_issues)*100

write_xlsx(cbind(" "=rownames(df_country_issues), df_country_issues), "RESULTS.xlsx")
