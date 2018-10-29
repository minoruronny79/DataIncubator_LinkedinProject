remove(list = ls())
library(dplyr)
library(data.table)
library(ggplot2)
library(broom)
library(zoo)
library(stringi)
library(quanteda)


#########################################################
###############Linkedin Analysis#########################
#########################################################
setwd("F:/PythonTraining/Python_CaseStudy/PCS_DataIncubator_linkedin/")

linked<-read.csv("F:/PythonTraining/Python_CaseStudy/PCS_DataIncubator_linkedin/linkedin.csv",
                 sep = ",")

linkedin<-data.table(linked)

###################################################
############EXPLORATORY ANALYSIS###################
###################################################
head(linkedin)
colnames(linkedin) #Colnames

str(linkedin) #Structure dataset



#Rows without empty description
sum(linkedin$description !="") #[1] 600918

#Generating additional variables
linkedin$as_of_date2<-as.Date(linkedin$as_of_date)
linkedin$date_updated2<-as.Date(linkedin$date_updated)
linkedin$date_added2<-as.Date(linkedin$date_added)
linkedin$quarter<-as.yearqtr(linkedin$as_of_date2) #Dates by quarter
linkedin$year<-year(linkedin$as_of_date2)

companies<-tidy(table(linkedin$company_name)) #Companies
industries<-tidy(table(linkedin$industry)) #Industries


###########################################################
##1. Followers Analysis by industry

#1.1 Followers as an average in all periods
follow_ind<-linkedin%>%
                group_by(industry)%>%
                   summarise(promedio=mean(followers_count))
#1.2 Top followers
top_followers<-follow_ind%>%arrange(desc(promedio))
top_followers<-top_followers[1:10,]
bottom_followers<-follow_ind%>%arrange(promedio)
botton_followers<-bottom_followers[1:10,]

#1.3 Followers as an average quarterly
follow_ind_year<-linkedin%>%
        group_by(industry, year)%>%
        summarise(promedio=mean(followers_count))

vectortop<-as.vector(top_followers$industry)
vectortop
follow_redu_year<-follow_ind_year%>%
                        filter(industry=="Newspapers"|
                               industry=="Consumer Electronics"|
                               industry=="Sporting Goods" |
                               industry=="Music" | industry=="Computer Networking" |
                               industry=="Information Technology and Services" |
                               industry=="Investment Banking" | industry=="Internet" |
                               industry=="Package/Freight Delivery" |
                               industry=="Wine and Spirits")



#1.4 Graphs by industry
#Top ten followers by industry barplot
ggplot(top_followers, aes(x = industry, y = promedio)) + 
        geom_bar(stat = "identity", fill="blue") +
        theme(axis.text.x = element_text(angle = 90))


#Top ten followers by industry boxplot (Followers)
ggplot(follow_redu_year, aes(x = industry, y = promedio))+
        geom_boxplot()+
        geom_point(aes(colour = year))+
        theme(axis.text.x = element_text(angle = 90))


#Variation of followers by industry (Followers)
colnames(follow_redu_year)

ggplot(follow_redu_year, aes(x=year, y=promedio, color=industry))+ 
        geom_line()
colnames(follow_redu_year)

str(follow_redu_year)

###########################################################
##2. Employees Analysis by industry

#Employees as an average in all periods
emp_ind<-linkedin%>%
        group_by(industry)%>%
        summarise(promedio=mean(employees_on_platform))

top_employee<-emp_ind%>%arrange(desc(promedio))
top_employee<-top_employee[1:10,]

#Employees as an average in all periods
follow_emp_year<-linkedin%>%
        group_by(industry, year
                 )%>%
        summarise(promedio=mean(employees_on_platform))


###########################################################
##3. Text Analysis
#Using subsample with description
sublinked3<-linkedin[linkedin$description!="",]
sublinked3$description[5]

#Creating a sample of 500 rows
set.seed(1)
sublinked3$contador<-1:nrow(sublinked3)
muestra<-sample(seq_len(nrow(sublinked3)), size=500) 
prueba<-sublinked3[muestra,] 

#to create a long string
listadescrip<-list()
for(i in 1:nrow(prueba)){
        listadescrip[[i]]=as.character(prueba$description[[i]])
}

#Creating a corpus
alltext=do.call("paste", listadescrip)

#Exploring 500 rows sample
cuerpo1<-corpus(alltext)
mytokens<-tokens(cuerpo1, remove_punct=TRUE, 
                 remove_numbers=TRUE, remove_symbols=TRUE)
mytokens
tokfreq<-dfm(mytokens, remove = stopwords("english"))
plot(tokfreq, min.freq=15)








