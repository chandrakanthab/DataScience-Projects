#===============================================================================================================================
##Application of Machine Learning Techniques to HEC Data 
##  Author: Dr. Nabil R. Adam 
# Disease: Diabetes
# Data Analyzed: Claims 
###
# This Module focuses on Data Exploration and Visualization
# Last Modified On: Dec. 2018
#===============================================================================================================================
#
#Hospital readmission for diabetic patients is a major concern in the United States. 
#Over $250 million dollars was spent on treatment of readmitted diabetic inpatients in 2011. 
#This disease is chronic and does not have any specific cure. Hospital readmissions are expensive as hospitals face penalties 
#if their readmission rate is higher than expected and reflects the inadequacies in health care system. 
#Most hospitals can agree that their main goals are centered around improving outcomes, creating more satisfied 
#patients, and better value. 
#For these reasons, it is important for the hospitals to improve focus on reducing readmission rates. 
#This study attempts to identify the key factors that influence readmission for diabetes and to 
#predict the probability of patient readmission.
##
############### Initialize ######################
## Relaunch R without loading user Rprofile files
## Remove all objects from the workspace.
rm(list=ls(all=TRUE))
##
## output directed to myfile.txt in cwd. output is appended
# to existing file. output also send to terminal. 
#checking the reuired libraries installed or not,if not install missing libraries
list.of.packages <- c("dplyr","e1071","grid","jpeg","ggplot2","rstudioapi","gridExtra","qwraps2"  )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

rstudioapi::restartSession()

#library("e1071","grid","jpeg","ggplot2","rstudioapi")

library(rstudioapi)
library(dplyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(qwraps2)
library(reshape)

options(qwraps2_markup = "markdown")




#
sink(file ="C:/Users/Nabil.Adam/Documents/Adam/ML/Output/AHG-Diab-RA-DE1.txt", append = FALSE, type = c("output", "message"),split=TRUE)
#

## output directed to myfile.txt in cwd. output is appended
# to existing file. output also send to terminal. 
################  Reading The Data That Has Already Been Cleansed ##################
######
##### PLEASE DO NO OVERRIDE MY READ, WRITE, OR SETWD STATMENTS. JUST COMMENT THEM AND UNCOMMENT YOURS... THIS WAY I DO NOT HAVE TO REWRITE EVEYRTIME
#######
## 
HEC_Diab_RA=read.csv("E:\\HEC\\Diabetes DataSet\\Data sets\\HEC_Diab_RA_Clean.csv",stringsAsFactors=FALSE,
# HEC_Diab_RA=read.csv("C:\\Users\\Nabil.Adam\\Documents\\Adam\\ML\\Datasets\\HEC_Diab_RA_Clean.csv",stringsAsFactors=FALSE,
                     colClasses=c("FEMALE"="factor","AGE_GRP"="factor","RACE"="factor","ATYPE"="factor",                                  "NCPT_GRP"="factor","NDX_GRP"="factor","DIED"="factor",
                                  "READMIT"="factor","PROCTYPE"="factor","LOS_GRP"="factor","AWEEKEND"="factor","PAY1"="factor",
                                  "ERVISIT"="factor","DXGROUP"="factor","DISCHARGEDISPOSITION"="factor","HOSPSTATE"="factor",
                                  "DISCHARGEDISPCODE"="factor", "Diab.Type"="factor","C_COMINDEX"="factor"))


######## It seems that R does not maintain the oder of the factor variables. 
#Ordering isn't included in the CSV. The levels are being ordered alphabetically on import.
###### Make sure to order the factor features
##############
HEC_Diab_RA[,'DIED']<-factor(HEC_Diab_RA[,'DIED'],levels=c("Alive","Died_in_Hosp"),labels = c("Alive","Died_in_Hosp"),ordered=TRUE)
HEC_Diab_RA[,'FEMALE']<-factor(HEC_Diab_RA[,'FEMALE'],levels=c("Male","Female"),labels = c("Male","Female"),ordered=TRUE)
HEC_Diab_RA[,'RACE']<-factor(HEC_Diab_RA[,'RACE'],
                             levels=c("White","African_A","Hispanic","Other","Unavailable"),
                             ordered=TRUE)
HEC_Diab_RA[,'READMIT']<-factor(HEC_Diab_RA[,'READMIT'],levels=c("Non_Readmit","Readmitted"),labels = c("Non_Readmit","Readmitted"),ordered=TRUE)
#### Have a problem with the ATYPE as a factor... Will just leave it as a character for now.
# HEC_Diab_RA[,'ATYPE']<-factor(HEC_Diab_RA[,'ATYPE'],levels=c("ELECTIVE","EM_URGENT","TRAUMA_C", "NEWBORN","UNKNOWN"), 
                          # labels=c("ELECTIVE","EM_URGENT","TRAUMA_C", "NEWBORN","UNKNOWN"),ordered=TRUE)
HEC_Diab_RA[,'AMONTH']<-factor(HEC_Diab_RA[,'AMONTH'],levels=c("January","February","March","April","May","June","July","August","September",
                                                               "October","November","December"),ordered=TRUE)
HEC_Diab_RA[,'YEAR']<-factor(HEC_Diab_RA[,'YEAR'],levels=c("Yr13","Yr14","Yr15","Yr16","Yr17","Yr18"),
                             labels = c("Yr13","Yr14", "Yr15","Yr16","Yr17","Yr18"),ordered=TRUE)
HEC_Diab_RA[,'AWEEKEND']<-factor(HEC_Diab_RA[,'AWEEKEND'],levels=c("Week_Day","Weekend"),labels = c("Week_Day","Weekend"),ordered=TRUE)
HEC_Diab_RA[,'PAY1']<-factor(HEC_Diab_RA[,'PAY1'],levels=c("MSSP","MEDICARE","MEDICARE_ADVANT","PRIVATE","MEDICAID","OTHER","Unknown"),ordered=TRUE)
HEC_Diab_RA[,'ERVISIT']<-factor(HEC_Diab_RA[,'ERVISIT'],levels=c("Non_ER","ER_Visit"),labels = c("Non_ER","ER_Visit"),ordered=TRUE)
HEC_Diab_RA[,'HOSPSTATE']<-factor(HEC_Diab_RA[,'HOSPSTATE'], levels = c("NJ","PA","FL","TN","Other"),ordered = TRUE)
HEC_Diab_RA[,'DISCHARGEDISPOSITION']<-factor(HEC_Diab_RA[,'DISCHARGEDISPOSITION'], 
                                             levels = c("Routine", "Still_P", "ST_Hosp", "SNF_ICF_IRF","HHC", "DIED.Home_MF","Other"),ordered = TRUE)
HEC_Diab_RA$Diab.Type<-factor(HEC_Diab_RA$Diab.Type, labels = c("Type_1","Type_2","Non_D"), ordered = TRUE)
HEC_Diab_RA$C_COMINDEX<-factor(HEC_Diab_RA[,'C_COMINDEX'], labels=c("Zero","Within_1_2","Within_3_4","Over_5"),
                               ordered=TRUE)
HEC_Diab_RA[,'AGE_GRP']<- factor(HEC_Diab_RA[,'AGE_GRP'], levels=c("Upto_65","Within_65_75","Within_75_85","Above_85"),ordered=TRUE)
HEC_Diab_RA[,'LOS_GRP']<- factor(HEC_Diab_RA[,'LOS_GRP'],levels=c("Zero_d","Within_1_10","Above_10"),ordered=TRUE)
HEC_Diab_RA[,'NCPT_GRP']<- factor(HEC_Diab_RA[,'NCPT_GRP'], levels=c("Upto_1","Within_1_5","Above_5"),ordered=TRUE)
HEC_Diab_RA[,'NDX_GRP']<- factor(HEC_Diab_RA[,'NDX_GRP'], levels=c("Upto_2","Within_3_5","Above_5"),ordered=TRUE)
HEC_Diab_RA[,'ATYPE']<-factor(HEC_Diab_RA[,'ATYPE'],levels=c("ELECTIVE","EM_URGENT","TRAUMA_C","NEWBORN","Unknown"),ordered=TRUE)
###########################################################################################


dim(HEC_Diab_RA)
colSums(is.na(HEC_Diab_RA))
#str after reading the file 
str(HEC_Diab_RA)
###
##################  Data Exploration ##################
##
#1.	Visualizing Relationships between Features
#	For each categorical feature, bar plot should be included
#	A histogram for each continuous feature. 
#	For continuous features with cardinality <10, use bar plots instead of histograms as this usually 
#    	produces more informative data visualization
#2. Measuring Correlation
##
#
#################### Summary Statistics for Diabetic Patients ############################
#
#
#
HEC_Diab_RA_Table<-subset(HEC_Diab_RA,select = c('YEAR','AGE_GRP','FEMALE','PAY1','TOTCHG','LOS_GRP','NCPT_GRP',
                                                 'DIED','ERVISIT','AWEEKEND','ATYPE','DISCHARGEDISPOSITION','READMIT'))

HEC_Diab_RA_Table$Yr=factor(HEC_Diab_RA$YEAR, levels=c("Yr13","Yr14","Yr15","Yr16","Yr17","Yr18"), 
                            labels = c("Yr13","Yr14","Yr15","Yr16","Yr17","Yr18"))
HEC_Diab_RA_Table$Yr=HEC_Diab_RA$YEAR

dim(HEC_Diab_RA_Table)
#table(HEC_Diab_RA_Table$Yr)
#str(HEC_Diab_RA_Table)
#
table(HEC_Diab_RA_Table$AGE_GRP)
table(HEC_Diab_RA_Table$PAY1)
table(HEC_Diab_RA_Table$LOS_GRP)
table(HEC_Diab_RA_Table$NCPT_GRP)
table(HEC_Diab_RA_Table$ATYPE)
table(HEC_Diab_RA_Table$DISCHARGEDISPOSITION)
table(HEC_Diab_RA_Table$YEAR,HEC_Diab_RA_Table$DISCHARGEDISPOSITION)
class(HEC_Diab_RA_Table$DIED)
table(HEC_Diab_RA_Table$DIED)
table(HEC_Diab_RA_Table$YEAR,HEC_Diab_RA_Table$DIED)


#change working directory to save the output files for the follwoing code

setwd("E:\\HEC\\Diabetes DataSet\\Reports")
# setwd("C:\\Users\\Nabil.Adam\\Documents\\Adam\\ML\\Output\\AHG-Output\\Tables")


########################## 1. By Year ###################################


table.summary<-data.frame(unclass(table(HEC_Diab_RA$DIED,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$DIED,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(setNames(data.frame(Died="","","","","","",f.score,row.names = c("In Hosp Mort"),stringsAsFactors = F),names(table.summary)),table.summary)

f.score<-round(fisher.test(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Readmission")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

f.score<-round(fisher.test(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("ER Visit")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

f.score<-round(fisher.test(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Length of Stay")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

f.score<-round(fisher.test(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","","","","","",f.score,row.names = c("Comorbidity Index")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

f.score<-round(fisher.test(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("No. of Procd")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

f.score<-round(fisher.test(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Age Groups")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

# f.score<-round(fisher.test(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Gender")),names(table.summary)))
# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
# table.summary<-rbind(table.summary,table.summary1)

# f.score<-round(fisher.test(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Payer--")),names(table.summary)))
# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
# table.summary<-rbind(table.summary,table.summary1)

f.score<-round(fisher.test(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Admit Day")),names(table.summary)))
table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)

#filtering a data set to include only EM_URGENT and TRAUMA_c
#after ther we are droping unused factor level in the subset data using droplevel() function
temp.table<-subset(HEC_Diab_RA,ATYPE=="EM_URGENT"|ATYPE=="TRAUMA_C")
temp.table<-droplevels(temp.table)
f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Admit Type")),names(table.summary)))
table.summary1<-data.frame(unclass(table(temp.table$ATYPE,temp.table$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)


HEC_Diab_RA$TEMP<-1

Avg.Totchg<-round(cast(HEC_Diab_RA,TEMP~YEAR , mean, value = 'TOTCHG')[,-1],digits = 0)
HEC_Diab_RA$TEMP<-NULL

# f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
f.score<-""
table.summary<-rbind(table.summary,setNames(data.frame("","","","","","",f.score,row.names = c("Avg Total Chg")),names(table.summary)))
table.summary1<-data.frame(Avg.Totchg,p.value="",stringsAsFactors = F,row.names = c("Charges($)"))
table.summary<-rbind(table.summary,table.summary1)

temp.table<-HEC_Diab_RA[HEC_Diab_RA$Diab.Type=="Type_1" | HEC_Diab_RA$Diab.Type =="Type_2",]
temp.table<-droplevels(temp.table)
f.score<-round(fisher.test(table(temp.table$Diab.Type,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","","","","","",f.score,row.names = c("Diabetic Types")),names(table.summary)))
table.summary1<-data.frame(unclass(table(temp.table$Diab.Type,temp.table$YEAR)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)


# f.score<-round(fisher.test(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame(READMIT="","","","","","",f.score,row.names = c("Discharge Info")),names(table.summary)))
# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$YEAR)),p.value="",stringsAsFactors = F)
# table.summary<-rbind(table.summary,table.summary1)

table.summary$Category<-row.names(table.summary)
table.summary<-table.summary[,c("Category","Yr13","Yr14","Yr15","Yr16","Yr17","Yr18","p.value")]


# #setting up theme
# tt <- ttheme_default(base_size = 12,
#                      core=list(bg_params = list(fill = blues9[1:2], col=NA),
#                                fg_params=list(fontface=3)),
#                      colhead=list(fg_params=list(col="navyblue", fontface=4L)),
#                      rowhead=list(fg_params=list(col="blue", fontface=3L,hjust=0, x=0)
#  #                                 ,title(main="Diabetic Patients Summary Stastics: 2013-2018")
#  )
# )
# 
# pdf(file="AHG-Table-YR2.pdf",height=15, width=10, title = "Diabetic Patients' Summary Statistics: 2013-2018 (AHG)") 
# 
# #,title ="Diabetic Patients' Summary Statistics: 2013-2018 (AHG)")
# 
# gridExtra::grid.table(table.summary2,theme=tt,rows=NULL)
# 
# dev.off()


# Setting up the theme 

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

 
g <- tableGrob(table.summary,rows = NULL,theme = ttheme_minimal())

separators <- replicate(ncol(g)-1 , segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)), simplify=FALSE)

## add vertical lines on the left side of columns (after 2nd)
g <- gtable::gtable_add_grob(g,grobs = separators,t=2,b = nrow(g), l = seq_len(ncol(g)-1)+1)
title <- textGrob("\nDiabetic Patients' Summary Statistics: 2013-2018 (AHG)",
                  gp=gpar(fontsize=15,col="blue",face="bold"),
                  x=0.5, hjust=0.5)
# footnote <- textGrob("footnote", x=0, hjust=0,gp=gpar( fontface="italic"))

# ind <- find_cell(g, 3, 3, "core-fg")
# ind2 <- find_cell(g, 2, 1, "core-bg")
ind2 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                  which(table.summary=="In Hosp Mort",arr.ind = T)[1,2], "core-fg")
ind21 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                  which(table.summary=="In Hosp Mort",arr.ind = T)[1,2]+7, "core-fg")
ind3 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                   which(table.summary=="Readmission",arr.ind = T)[1,2], "core-fg")
ind31 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                  which(table.summary=="Readmission",arr.ind = T)[1,2]+7, "core-fg")
ind4 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                  which(table.summary=="ER Visit",arr.ind = T)[1,2], "core-fg")
ind41 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                   which(table.summary=="ER Visit",arr.ind = T)[1,2]+7, "core-fg")
ind5 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                  which(table.summary=="Length of Stay",arr.ind = T)[1,2], "core-fg")
ind51 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                   which(table.summary=="Length of Stay",arr.ind = T)[1,2]+7, "core-fg")
ind6 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                  which(table.summary=="Comorbidity Index",arr.ind = T)[1,2], "core-fg")
ind61 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                   which(table.summary=="Comorbidity Index",arr.ind = T)[1,2]+7, "core-fg")
ind7 <- find_cell(g, which(table.summary=="No. of Procd",arr.ind = T)[1,1]+1,
                  which(table.summary=="No. of Procd",arr.ind = T)[1,2], "core-fg")
ind71 <- find_cell(g, which(table.summary=="No. of Procd",arr.ind = T)[1,1]+1,
                   which(table.summary=="No. of Procd",arr.ind = T)[1,2]+7, "core-fg")
ind8 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                  which(table.summary=="Age Groups",arr.ind = T)[1,2], "core-fg")
ind81 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                   which(table.summary=="Age Groups",arr.ind = T)[1,2]+7, "core-fg")
ind9 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                  which(table.summary=="Avg Total Chg",arr.ind = T)[1,2], "core-fg")
ind91 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                   which(table.summary=="Avg Total Chg",arr.ind = T)[1,2]+7, "core-fg")
ind10 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                  which(table.summary=="Admit Day",arr.ind = T)[1,2], "core-fg")
ind101 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Day",arr.ind = T)[1,2]+7, "core-fg")
ind11 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                  which(table.summary=="Admit Type",arr.ind = T)[1,2], "core-fg")
ind111 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Type",arr.ind = T)[1,2]+7, "core-fg")
ind12 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                  which(table.summary=="Diabetic Types",arr.ind = T)[1,2], "core-fg")
ind121 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                   which(table.summary=="Diabetic Types",arr.ind = T)[1,2]+7, "core-fg")


g$grobs[ind2][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind21][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind3][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind31][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind4][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind41][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind5][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind51][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind6][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind61][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind7][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind71][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind8][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind81][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind9][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind91][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind10][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind101][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind11][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind111][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind12][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind121][[1]][["gp"]] <- gpar(col="blue")

pdf(file="AHG-Table-YR.pdf",height=12, width=8) 

grid.arrange(top = title,g)
grid.rect(width = .9, height = .97, gp = gpar(lwd = 2, col = "blue", fill = NA))

dev.off()





########################### 2. BY Patient Discharge Disposition: Alive/Died in Hospital #################################
##
#Died during hospitalization (DIED) is coded from the discharge disposition of patient. The coding varies across years of data.
#Beginning with 2001 HCUP data, the coding of DIED is assigned as follows:
#  If DISPUniform indicates that a patient was discharged alive (values 1-7, 21 (starting in 2010), 99), then DIED is coded as 0.
#If a patient had discharge status of died outside the hospital, then DISPuniform is set to 99 (discharged from the hospital alive, destination unknown) and DIED was set to alive (0). 
#If DISPUniform indicates that a patient died in the hospital (value 20), then DIED is coded as 1. 
#If DISPUniform is missing (.) or invalid (.A), then DIED is also coded as missing (.) or invalid (.A).
#

table.summary<-data.frame(unclass(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(setNames(data.frame("","",f.score,row.names = c("Year"),stringsAsFactors = F),names(table.summary)),table.summary)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Readmission"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("ER Visit"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Length of Stay"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Comorbidity Index"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("No. of Procd"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Age Groups"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 8)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Gender--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 8)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Payer--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Day"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


temp.table<-subset(HEC_Diab_RA,ATYPE=="EM_URGENT"|ATYPE=="ELECTIVE")
temp.table<-droplevels(temp.table)
table.summary1<-data.frame(unclass(table(temp.table$ATYPE,temp.table$DIED)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Type"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


HEC_Diab_RA$TEMP<-1

Avg.Totchg<-round(cast(HEC_Diab_RA,TEMP~DIED , mean, value = 'TOTCHG')[,-1],digits = 0)
HEC_Diab_RA$TEMP<-NULL
# f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
f.score<-""
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Avg Total Chg")),names(table.summary)))
table.summary1<-data.frame(Avg.Totchg,p.value="",stringsAsFactors = F,row.names = c("Charges($)"))
table.summary<-rbind(table.summary,table.summary1)


# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$DIED)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 8)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Discharge info.--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)


temp.table<-HEC_Diab_RA[HEC_Diab_RA$Diab.Type=="Type_1" | HEC_Diab_RA$Diab.Type =="Type_2",]
temp.table<-droplevels(temp.table)
f.score<-round(fisher.test(table(temp.table$Diab.Type,temp.table$DIED),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Diabetic Types")),names(table.summary)))
table.summary1<-data.frame(unclass(table(temp.table$Diab.Type,temp.table$DIED)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)


table.summary$Category<-row.names(table.summary)
table.summary<-table.summary[,c("Category","Alive","Died_in_Hosp","p.value")]


# Setting up the theme 

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}


g <- tableGrob(table.summary,rows = NULL,theme = ttheme_minimal())
g1 <- tableGrob(head(table.summary),rows = NULL,theme = ttheme_minimal())

separators <- replicate(ncol(g)-1 , segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)), simplify=FALSE)

## add vertical lines on the left side of columns (after 2nd)
g <- gtable::gtable_add_grob(g,grobs = separators,t=2,b = nrow(g), l = seq_len(ncol(g)-1)+1)
title <- textGrob("\nDiabetic Patients' Summary Statistics: 2013-2018 (AHG)",
                  gp=gpar(fontsize=12,col="blue",face="bold"),
                  x=0.5, hjust=0.5)


ind2 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                  which(table.summary=="Year",arr.ind = T)[1,2], "core-fg")
ind21 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                   which(table.summary=="Year",arr.ind = T)[1,2]+3, "core-fg")
ind3 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                  which(table.summary=="Readmission",arr.ind = T)[1,2], "core-fg")
ind31 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                   which(table.summary=="Readmission",arr.ind = T)[1,2]+3, "core-fg")
ind4 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                  which(table.summary=="ER Visit",arr.ind = T)[1,2], "core-fg")
ind41 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                   which(table.summary=="ER Visit",arr.ind = T)[1,2]+3, "core-fg")
ind5 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                  which(table.summary=="Length of Stay",arr.ind = T)[1,2], "core-fg")
ind51 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                   which(table.summary=="Length of Stay",arr.ind = T)[1,2]+3, "core-fg")
ind6 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                  which(table.summary=="Comorbidity Index",arr.ind = T)[1,2], "core-fg")
ind61 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                   which(table.summary=="Comorbidity Index",arr.ind = T)[1,2]+3, "core-fg")
ind7 <- find_cell(g, which(table.summary=="No. of Procd",arr.ind = T)[1,1]+1,
                  which(table.summary=="No. of Procd",arr.ind = T)[1,2], "core-fg")
ind71 <- find_cell(g, which(table.summary=="No. of Procd",arr.ind = T)[1,1]+1,
                   which(table.summary=="No. of Procd",arr.ind = T)[1,2]+3, "core-fg")
ind8 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                  which(table.summary=="Age Groups",arr.ind = T)[1,2], "core-fg")
ind81 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                   which(table.summary=="Age Groups",arr.ind = T)[1,2]+3, "core-fg")
ind9 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                  which(table.summary=="Avg Total Chg",arr.ind = T)[1,2], "core-fg")
ind91 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                   which(table.summary=="Avg Total Chg",arr.ind = T)[1,2]+3, "core-fg")
ind10 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Day",arr.ind = T)[1,2], "core-fg")
ind101 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Day",arr.ind = T)[1,2]+3, "core-fg")
ind11 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Type",arr.ind = T)[1,2], "core-fg")
ind111 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Type",arr.ind = T)[1,2]+3, "core-fg")
ind12 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                   which(table.summary=="Diabetic Types",arr.ind = T)[1,2], "core-fg")
ind121 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                    which(table.summary=="Diabetic Types",arr.ind = T)[1,2]+3, "core-fg")


g$grobs[ind2][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind21][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind3][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind31][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind4][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind41][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind5][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind51][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind6][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind61][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind7][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind71][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind8][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind81][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind9][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind91][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind10][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind101][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind11][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind111][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind12][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind121][[1]][["gp"]] <- gpar(col="blue")


pdf(file="AHG-Table-Died.pdf",height=13, width=8.5,title ="Diabetic Patients' Summary Statistics: 2013-2018 (AHG)")

grid.arrange(top=title,g)
grid.rect(width = .9, height = .97, gp = gpar(lwd = 2, col = "blue", fill = NA))
dev.off()
# #



###################### 3. Non-ReAdmits/Readmitted ######################## 

table.summary<-data.frame(unclass(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary[nrow(table.summary),ncol(table.summary)]<-f.score
table.summary<-rbind(setNames(data.frame("","",f.score,row.names = c("Year"),stringsAsFactors = F),names(table.summary)),table.summary)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DIED,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$DIED,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("In Hosp Mort"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("ER Visit"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Length of Stay"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Comorbidity Index"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("No.of Procd"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Age Groups"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Gender--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Day"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

temp.table<-subset(HEC_Diab_RA,ATYPE=="EM_URGENT"|ATYPE=="ELECTIVE")
temp.table<-droplevels(temp.table)
table.summary1<-data.frame(unclass(table(temp.table$ATYPE,temp.table$READMIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Type"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$READMIT)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Discharge info.--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

HEC_Diab_RA$TEMP<-1

Avg.Totchg<-round(cast(HEC_Diab_RA,TEMP~READMIT , mean, value = 'TOTCHG')[,-1],digits = 0)
HEC_Diab_RA$TEMP<-NULL
# f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
f.score<-""
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Avg Total Chg")),names(table.summary)))
table.summary1<-data.frame(Avg.Totchg,p.value="",stringsAsFactors = F,row.names = c("Charges($)"))
table.summary<-rbind(table.summary,table.summary1)


temp.table<-HEC_Diab_RA[HEC_Diab_RA$Diab.Type=="Type_1" | HEC_Diab_RA$Diab.Type =="Type_2",]
temp.table<-droplevels(temp.table)
f.score<-round(fisher.test(table(temp.table$Diab.Type,temp.table$READMIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Diabetic Types")),names(table.summary)))
table.summary1<-data.frame(unclass(table(temp.table$Diab.Type,temp.table$READMIT)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)


table.summary$Category<-row.names(table.summary)
table.summary<-table.summary[,c("Category","Non_Readmit","Readmitted","p.value")]

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}


g <- tableGrob(table.summary,rows = NULL,theme = ttheme_minimal())


separators <- replicate(ncol(g)-1 , segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)), simplify=FALSE)

## add vertical lines on the left side of columns (after 2nd)
g <- gtable::gtable_add_grob(g,grobs = separators,t=2,b = nrow(g), l = seq_len(ncol(g)-1)+1)
title <- textGrob("\nDiabetic Patients' Summary Statistics: 2013-2018 (AHG)",
                  gp=gpar(fontsize=15,col="blue",face="bold"),
                  x=0.5, hjust=0.5)


ind2 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                  which(table.summary=="Year",arr.ind = T)[1,2], "core-fg")
ind21 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                   which(table.summary=="Year",arr.ind = T)[1,2]+3, "core-fg")
ind3 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                  which(table.summary=="In Hosp Mort",arr.ind = T)[1,2], "core-fg")
ind31 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                   which(table.summary=="In Hosp Mort",arr.ind = T)[1,2]+3, "core-fg")
ind4 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                  which(table.summary=="ER Visit",arr.ind = T)[1,2], "core-fg")
ind41 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                   which(table.summary=="ER Visit",arr.ind = T)[1,2]+3, "core-fg")
ind5 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                  which(table.summary=="Length of Stay",arr.ind = T)[1,2], "core-fg")
ind51 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                   which(table.summary=="Length of Stay",arr.ind = T)[1,2]+3, "core-fg")
ind6 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                  which(table.summary=="Comorbidity Index",arr.ind = T)[1,2], "core-fg")
ind61 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                   which(table.summary=="Comorbidity Index",arr.ind = T)[1,2]+3, "core-fg")
ind7 <- find_cell(g, which(table.summary=="No.of Procd",arr.ind = T)[1,1]+1,
                  which(table.summary=="No.of Procd",arr.ind = T)[1,2], "core-fg")
ind71 <- find_cell(g, which(table.summary=="No.of Procd",arr.ind = T)[1,1]+1,
                   which(table.summary=="No.of Procd",arr.ind = T)[1,2]+3, "core-fg")
ind8 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                  which(table.summary=="Age Groups",arr.ind = T)[1,2], "core-fg")
ind81 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                   which(table.summary=="Age Groups",arr.ind = T)[1,2]+3, "core-fg")
ind9 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                  which(table.summary=="Avg Total Chg",arr.ind = T)[1,2], "core-fg")
ind91 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                   which(table.summary=="Avg Total Chg",arr.ind = T)[1,2]+3, "core-fg")
ind10 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Day",arr.ind = T)[1,2], "core-fg")
ind101 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Day",arr.ind = T)[1,2]+3, "core-fg")
ind11 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Type",arr.ind = T)[1,2], "core-fg")
ind111 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Type",arr.ind = T)[1,2]+3, "core-fg")
ind12 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                   which(table.summary=="Diabetic Types",arr.ind = T)[1,2], "core-fg")
ind121 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                    which(table.summary=="Diabetic Types",arr.ind = T)[1,2]+3, "core-fg")


g$grobs[ind2][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind21][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind3][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind31][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind4][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind41][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind5][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind51][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind6][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind61][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind7][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind71][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind8][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind81][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind9][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind91][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind10][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind101][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind11][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind111][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind12][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind121][[1]][["gp"]] <- gpar(col="blue")

pdf(file="AHG-Table-REA.pdf", 
    height=13, width=8.5,title ="Diabetic Patients' Summary Statistics: 2013-2018 (AHG)")

grid.arrange(top = title,g)
grid.rect(width = .9, height = .97, gp = gpar(lwd = 2, col = "blue", fill = NA))
dev.off()



######################### 4. Non-Emergency/Emergency Visits ######## 
###

table.summary<-data.frame(unclass(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary[nrow(table.summary),ncol(table.summary)]<-f.score
table.summary<-rbind(setNames(data.frame("","",f.score,row.names = c("Year"),stringsAsFactors = F),names(table.summary)),table.summary)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DIED,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$DIED,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("In Hosp Mort"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Readmission"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Length of Stay"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Comorbidity Index"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("No.of Procd"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Age Groups"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Gender--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AWEEKEND,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Day"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$ERVISIT)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Discharge Info.--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

temp.table<-subset(HEC_Diab_RA,ATYPE=="EM_URGENT"|ATYPE=="ELECTIVE")
temp.table<-droplevels(temp.table)
table.summary1<-data.frame(unclass(table(temp.table$ATYPE,temp.table$ERVISIT)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Type"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


HEC_Diab_RA$TEMP<-1
Avg.Totchg<-round(cast(HEC_Diab_RA,TEMP~ERVISIT , mean, value = 'TOTCHG')[,-1],digits = 0)
HEC_Diab_RA$TEMP<-NULL
# f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
f.score<-""
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Avg Total Chg")),names(table.summary)))
table.summary1<-data.frame(Avg.Totchg,p.value="",stringsAsFactors = F,row.names = c("Charges($)"))
table.summary<-rbind(table.summary,table.summary1)


temp.table<-HEC_Diab_RA[HEC_Diab_RA$Diab.Type=="Type_1" | HEC_Diab_RA$Diab.Type =="Type_2",]
temp.table<-droplevels(temp.table)
f.score<-round(fisher.test(table(temp.table$Diab.Type,temp.table$ERVISIT),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Diabetic Types")),names(table.summary)))
table.summary1<-data.frame(unclass(table(temp.table$Diab.Type,temp.table$ERVISIT)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)


table.summary$Category<-row.names(table.summary)
table.summary<-table.summary[,c("Category","Non_ER","ER_Visit","p.value")]

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}


g <- tableGrob(table.summary,rows = NULL,theme = ttheme_minimal())


separators <- replicate(ncol(g)-1 , segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)), simplify=FALSE)

## add vertical lines on the left side of columns (after 2nd)
g <- gtable::gtable_add_grob(g,grobs = separators,t=2,b = nrow(g), l = seq_len(ncol(g)-1)+1)
title <- textGrob("\nDiabetic Patients' Summary Statistics: 2013-2018 (AHG)",
                  gp=gpar(fontsize=15,col="blue",face="bold"),
                  x=0.5, hjust=0.5)


ind2 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                  which(table.summary=="Year",arr.ind = T)[1,2], "core-fg")
ind21 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                   which(table.summary=="Year",arr.ind = T)[1,2]+3, "core-fg")
ind3 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                  which(table.summary=="In Hosp Mort",arr.ind = T)[1,2], "core-fg")
ind31 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                   which(table.summary=="In Hosp Mort",arr.ind = T)[1,2]+3, "core-fg")
ind4 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                  which(table.summary=="Readmission",arr.ind = T)[1,2], "core-fg")
ind41 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                   which(table.summary=="Readmission",arr.ind = T)[1,2]+3, "core-fg")
ind5 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                  which(table.summary=="Length of Stay",arr.ind = T)[1,2], "core-fg")
ind51 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                   which(table.summary=="Length of Stay",arr.ind = T)[1,2]+3, "core-fg")
ind6 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                  which(table.summary=="Comorbidity Index",arr.ind = T)[1,2], "core-fg")
ind61 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                   which(table.summary=="Comorbidity Index",arr.ind = T)[1,2]+3, "core-fg")
ind7 <- find_cell(g, which(table.summary=="No.of Procd",arr.ind = T)[1,1]+1,
                  which(table.summary=="No.of Procd",arr.ind = T)[1,2], "core-fg")
ind71 <- find_cell(g, which(table.summary=="No.of Procd",arr.ind = T)[1,1]+1,
                   which(table.summary=="No.of Procd",arr.ind = T)[1,2]+3, "core-fg")
ind8 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                  which(table.summary=="Age Groups",arr.ind = T)[1,2], "core-fg")
ind81 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                   which(table.summary=="Age Groups",arr.ind = T)[1,2]+3, "core-fg")
ind9 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                  which(table.summary=="Avg Total Chg",arr.ind = T)[1,2], "core-fg")
ind91 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                   which(table.summary=="Avg Total Chg",arr.ind = T)[1,2]+3, "core-fg")
ind10 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Day",arr.ind = T)[1,2], "core-fg")
ind101 <- find_cell(g, which(table.summary=="Admit Day",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Day",arr.ind = T)[1,2]+3, "core-fg")
ind11 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Type",arr.ind = T)[1,2], "core-fg")
ind111 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Type",arr.ind = T)[1,2]+3, "core-fg")
ind12 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                   which(table.summary=="Diabetic Types",arr.ind = T)[1,2], "core-fg")
ind121 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                    which(table.summary=="Diabetic Types",arr.ind = T)[1,2]+3, "core-fg")


g$grobs[ind2][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind21][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind3][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind31][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind4][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind41][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind5][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind51][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind6][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind61][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind7][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind71][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind8][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind81][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind9][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind91][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind10][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind101][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind11][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind111][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind12][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind121][[1]][["gp"]] <- gpar(col="blue")

pdf(file="AHG-Table-ER.pdf", 
   height=13, width=8.5,title ="Diabetic Patients' Summary Statistics: 2013-2018 (AHG)")

grid.arrange(top = title,g)
grid.rect(width = .9, height = .97, gp = gpar(lwd = 2, col = "blue", fill = NA))
dev.off()



######################## 5. Week Day/Weekend ################## 
###

table.summary<-data.frame(unclass(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$YEAR,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary[nrow(table.summary),ncol(table.summary)]<-f.score
table.summary<-rbind(setNames(data.frame("","",f.score,row.names = c("Year"),stringsAsFactors = F),names(table.summary)),table.summary)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DIED,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$DIED,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("In Hosp Mort"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)


table.summary1<-data.frame(unclass(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$READMIT,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Readmission"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$ERVISIT,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("ER Visit"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$LOS_GRP,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Length of Stay"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$C_COMINDEX,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Comorbidity Index"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$NCPT_GRP,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("No.of Procd"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

table.summary1<-data.frame(unclass(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(HEC_Diab_RA$AGE_GRP,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Age Groups"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$FEMALE,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 8)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Gender--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$PAY1,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Gender--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)


temp.table<-subset(HEC_Diab_RA,ATYPE=="EM_URGENT"|ATYPE=="ELECTIVE")
temp.table<-droplevels(temp.table)
table.summary1<-data.frame(unclass(table(temp.table$ATYPE,temp.table$AWEEKEND)),p.value="",stringsAsFactors = F)
f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Admit Type"),stringsAsFactors = F),names(table.summary)))
# table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
table.summary<-rbind(table.summary,table.summary1)

# table.summary1<-data.frame(unclass(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$AWEEKEND)),p.value="",stringsAsFactors = F)
# f.score<-round(fisher.test(table(HEC_Diab_RA$DISCHARGEDISPOSITION,HEC_Diab_RA$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
# table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Discharge info.--"),stringsAsFactors = F),names(table.summary)))
# # table.summary1[nrow(table.summary1),ncol(table.summary1)]<-f.score
# table.summary<-rbind(table.summary,table.summary1)

HEC_Diab_RA$TEMP<-1
Avg.Totchg<-round(cast(HEC_Diab_RA,TEMP~AWEEKEND , mean, value = 'TOTCHG')[,-1],digits = 0)
HEC_Diab_RA$TEMP<-NULL
# f.score<-round(fisher.test(table(temp.table$ATYPE,temp.table$YEAR),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
f.score<-""
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Avg Total Chg")),names(table.summary)))
table.summary1<-data.frame(Avg.Totchg,p.value="",stringsAsFactors = F,row.names = c("Charges($)"))
table.summary<-rbind(table.summary,table.summary1)


temp.table<-HEC_Diab_RA[HEC_Diab_RA$Diab.Type=="Type_1" | HEC_Diab_RA$Diab.Type =="Type_2",]
temp.table<-droplevels(temp.table)
f.score<-round(fisher.test(table(temp.table$Diab.Type,temp.table$AWEEKEND),workspace = 2e8,simulate.p.value = T)$p.value,digits = 7)
table.summary<-rbind(table.summary,setNames(data.frame("","",f.score,row.names = c("Diabetic Types")),names(table.summary)))
table.summary1<-data.frame(unclass(table(temp.table$Diab.Type,temp.table$AWEEKEND)),p.value="",stringsAsFactors = F)
table.summary<-rbind(table.summary,table.summary1)


table.summary$Category<-row.names(table.summary)
table.summary<-table.summary[,c("Category","Week_Day","Weekend","p.value")]

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}


g <- tableGrob(table.summary,rows = NULL,theme = ttheme_minimal())


separators <- replicate(ncol(g)-1 , segmentsGrob(x1 = unit(0, "npc"), gp=gpar(lty=2)), simplify=FALSE)

## add vertical lines on the left side of columns (after 2nd)
g <- gtable::gtable_add_grob(g,grobs = separators,t=2,b = nrow(g), l = seq_len(ncol(g)-1)+1)
title <- textGrob("\nDiabetic Patients' Summary Statistics: 2013-2018 (AHG)",
                  gp=gpar(fontsize=15,col="blue",face="bold"),
                  x=0.5, hjust=0.5)


ind2 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                  which(table.summary=="Year",arr.ind = T)[1,2], "core-fg")
ind21 <- find_cell(g, which(table.summary=="Year",arr.ind = T)[1,1]+1,
                   which(table.summary=="Year",arr.ind = T)[1,2]+3, "core-fg")
ind3 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                  which(table.summary=="In Hosp Mort",arr.ind = T)[1,2], "core-fg")
ind31 <- find_cell(g, which(table.summary=="In Hosp Mort",arr.ind = T)[1,1]+1,
                   which(table.summary=="In Hosp Mort",arr.ind = T)[1,2]+3, "core-fg")
ind4 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                  which(table.summary=="Readmission",arr.ind = T)[1,2], "core-fg")
ind41 <- find_cell(g, which(table.summary=="Readmission",arr.ind = T)[1,1]+1,
                   which(table.summary=="Readmission",arr.ind = T)[1,2]+3, "core-fg")
ind5 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                  which(table.summary=="Length of Stay",arr.ind = T)[1,2], "core-fg")
ind51 <- find_cell(g, which(table.summary=="Length of Stay",arr.ind = T)[1,1]+1,
                   which(table.summary=="Length of Stay",arr.ind = T)[1,2]+3, "core-fg")
ind6 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                  which(table.summary=="Comorbidity Index",arr.ind = T)[1,2], "core-fg")
ind61 <- find_cell(g, which(table.summary=="Comorbidity Index",arr.ind = T)[1,1]+1,
                   which(table.summary=="Comorbidity Index",arr.ind = T)[1,2]+3, "core-fg")
ind7 <- find_cell(g, which(table.summary=="No.of Procd",arr.ind = T)[1,1]+1,
                  which(table.summary=="No.of Procd",arr.ind = T)[1,2], "core-fg")
ind71 <- find_cell(g, which(table.summary=="No.of Procd",arr.ind = T)[1,1]+1,
                   which(table.summary=="No.of Procd",arr.ind = T)[1,2]+3, "core-fg")
ind8 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                  which(table.summary=="Age Groups",arr.ind = T)[1,2], "core-fg")
ind81 <- find_cell(g, which(table.summary=="Age Groups",arr.ind = T)[1,1]+1,
                   which(table.summary=="Age Groups",arr.ind = T)[1,2]+3, "core-fg")
ind9 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                  which(table.summary=="Avg Total Chg",arr.ind = T)[1,2], "core-fg")
ind91 <- find_cell(g, which(table.summary=="Avg Total Chg",arr.ind = T)[1,1]+1,
                   which(table.summary=="Avg Total Chg",arr.ind = T)[1,2]+3, "core-fg")
ind10 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                   which(table.summary=="ER Visit",arr.ind = T)[1,2], "core-fg")
ind101 <- find_cell(g, which(table.summary=="ER Visit",arr.ind = T)[1,1]+1,
                    which(table.summary=="ER Visit",arr.ind = T)[1,2]+3, "core-fg")
ind11 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                   which(table.summary=="Admit Type",arr.ind = T)[1,2], "core-fg")
ind111 <- find_cell(g, which(table.summary=="Admit Type",arr.ind = T)[1,1]+1,
                    which(table.summary=="Admit Type",arr.ind = T)[1,2]+3, "core-fg")
ind12 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                   which(table.summary=="Diabetic Types",arr.ind = T)[1,2], "core-fg")
ind121 <- find_cell(g, which(table.summary=="Diabetic Types",arr.ind = T)[1,1]+1,
                    which(table.summary=="Diabetic Types",arr.ind = T)[1,2]+3, "core-fg")


g$grobs[ind2][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind21][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind3][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind31][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind4][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind41][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind5][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind51][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind6][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind61][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind7][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind71][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind8][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind81][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind9][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind91][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind10][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind101][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind11][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind111][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind12][[1]][["gp"]] <- gpar(col="blue")
g$grobs[ind121][[1]][["gp"]] <- gpar(col="blue")

pdf(file="AHG-Table-WE.pdf", 
   height=13, width=8.5,title ="Diabetic Patients' Summary Statistics: 2013-2018 (AHG)")

grid.arrange(top = title,g)
grid.rect(width = .9, height = .97, gp = gpar(lwd = 2, col = "blue", fill = NA))
dev.off()


####
########################## Top  Diagnoses Analysis ###########################
##

#change working directory to save the output files for the follwoing code

setwd("C:\\Users\\Nabil.Adam\\Documents\\Adam\\ML\\Output\\AHG-Output\\Plots")


class(HEC_Diab_RA$DX1)
#table(HEC_Diab_RA$DX1)

Top_CC1 = as.data.frame(table(HEC_Diab_RA$DX1))
Top_CC1 = Top_CC1[(Top_CC1$Var1!="Unknown"), ]
Top_CC1 = Top_CC1[order(-Top_CC1[,2]), ]
#Top_CC1 = head(Top_CC1,25)
#Top_CC1


Top_CC2 = as.data.frame(table(HEC_Diab_RA$DX2))
Top_CC2 = Top_CC2[(Top_CC2$Var1!="Unknown"), ]
Top_CC2 = Top_CC2[order(-Top_CC2[,2]), ]
#Top_CC2 = head(Top_CC2,25)
#Top_CC2

Top_CC3 = as.data.frame(table(HEC_Diab_RA$DX3))
Top_CC3 = Top_CC3[(Top_CC3$Var1!="Unknown"), ]
Top_CC3 = Top_CC3[order(-Top_CC3[,2]), ]
#Top_CC3 = head(Top_CC3,25)

Top_CC4 = as.data.frame(table(HEC_Diab_RA$DX4))
Top_CC4 = Top_CC4[(Top_CC4$Var1!="UnKnown"), ]
Top_CC4 = Top_CC4[order(-Top_CC4[,2]), ]
#Top_CC4 = head(Top_CC4,25)

Top_CC5 = as.data.frame(table(HEC_Diab_RA$DX5))
Top_CC5 = Top_CC5[(Top_CC5$Var1!="Unknown"), ]
Top_CC5 = Top_CC5[order(-Top_CC5[,2]), ]
#Top_CC5 = head(Top_CC5,25)

Top_CC6 = as.data.frame(table(HEC_Diab_RA$DX6))
Top_CC6 = Top_CC6[(Top_CC6$Var1!="Unknown"), ]
Top_CC6 = Top_CC6[order(-Top_CC6[,2]), ]
#Top_CC6 = head(Top_CC6,25)

Top_CC7 = as.data.frame(table(HEC_Diab_RA$DX7))
Top_CC7 = Top_CC7[(Top_CC7$Var1!="Unknown"), ]
Top_CC7 = Top_CC7[order(-Top_CC7[,2]), ]
#Top_CC7 = head(Top_CC7,25)

Top_CC8 = as.data.frame(table(HEC_Diab_RA$DX8))
Top_CC8 = Top_CC8[(Top_CC8$Var1!="Unknown"), ]
Top_CC8 = Top_CC8[order(-Top_CC8[,2]), ]
#Top_CC8 = head(Top_CC8,25)
#Top_CC8

#Top_CC_1_thr_8=aggregate(cbind(Top_CC1,Top_CC2,Top_CC3, 
#Top_CC4, Top_CC5, Top_CC6,Top_CC7,Top_CC8), FUN=sum)

Top_CC_1_thr_8 = rbind(Top_CC1,Top_CC2,Top_CC3, Top_CC4, Top_CC5, Top_CC6,Top_CC7,Top_CC8)
dim(Top_CC_1_thr_8)
#[1] 30082     2
Top_CC_1_thr_8 = Top_CC_1_thr_8[(Top_CC_1_thr_8$Var1!="Unknown"), ]
Top_CC_1_thr_8=aggregate(cbind(Freq)~Var1,FUN=sum,data=Top_CC_1_thr_8)
Top_CC_1_thr_8 = Top_CC_1_thr_8[order(-Top_CC_1_thr_8[,2]), ]
head(Top_CC_1_thr_8,10)
Top_CC_1_thr_8 = head(Top_CC_1_thr_8,10)
#
Top_CC_1_thr_8
dim(Top_CC_1_thr_8)

##
#Top_CC_1_thr_8

#Top_CC_1_thr_8$Var1=factor(as.character(Top_CC_1_thr_8$Var1))
class(Top_CC_1_thr_8$Var1)
overall_sum<-sum(Top_CC_1_thr_8$Freq)
overall_sum
#74987
#Top_CC_1_thr_8=aggregate(cbind(Freq)~Var1,FUN=sum,data=Top_CC_1_thr_8)
Top_CC_1_thr_8$Freq=round(100*(Top_CC_1_thr_8$Freq/overall_sum))
Top_CC_1_thr_8<-Top_CC_1_thr_8[order(-Top_CC_1_thr_8[,2]),]
#Top_CC_1_thr_8 = head(Top_CC_1_thr_8,10)
dim(Top_CC_1_thr_8)
#table(Top_CC_1_thr_8)
Top_CC_1_thr_8

#
overall_sum1<-sum(Top_CC_1_thr_8$Freq)
overall_sum1

###############  Top 10 Diagnoses across all patients ###############
####
#


ggplot(Top_CC_1_thr_8,aes(x=Top_CC_1_thr_8$Var1,y=Top_CC_1_thr_8$Freq))+
  geom_bar(aes(fill=Var1), stat="identity",  position = "dodge")+
  labs(title ="2013-2018(AHG)",x="ICD (v9/v10) Diagnosis Codes",y="%Frequency",
       fill="Top 10 \n Diagnosis Codes",
       caption = "Top 10 Diagnosis Codes for Diabetic Patients")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
ggsave(filename = "AHG_Diab_CC.jpeg",width = 15,units = "cm")
dev.off()


HEC_Diab_RA$Which_Top_CC=as.character("No_top_Chief_Comp")
table(HEC_Diab_RA$Which_Top_CC)
Top_CC_1_thr_8$Var1=as.character(Top_CC_1_thr_8$Var1)
#class(Top_CC_1_thr_8$Var1)
nrow(Top_CC_1_thr_8)

for (i in 1:nrow(Top_CC_1_thr_8)) {
  HEC_Diab_RA[which(HEC_Diab_RA$DX1 == Top_CC_1_thr_8$Var1[i]|HEC_Diab_RA$DX2 == Top_CC_1_thr_8$Var1[i]|
                      HEC_Diab_RA$DX3 == Top_CC_1_thr_8$Var1[i]|HEC_Diab_RA$DX4 == Top_CC_1_thr_8$Var1[i]|
                      HEC_Diab_RA$DX5 == Top_CC_1_thr_8$Var1[i]|HEC_Diab_RA$DX6 == Top_CC_1_thr_8$Var1[i]|
                      HEC_Diab_RA$DX7 == Top_CC_1_thr_8$Var1[i]|HEC_Diab_RA$DX8 == Top_CC_1_thr_8$Var1[i]),]$Which_Top_CC=Top_CC_1_thr_8$Var1[i]
}
table(HEC_Diab_RA$Which_Top_CC)
table(Top_CC_1_thr_8)

tmp_HEC_Diab_RA<-HEC_Diab_RA


tmp_HEC_Diab_RA$Has_Top_CC=0
tmp_HEC_Diab_RA$Has_Top_CC=as.integer(tmp_HEC_Diab_RA$Has_Top_CC)
class(tmp_HEC_Diab_RA$Has_Top_CC)
tmp_HEC_Diab_RA[which(tmp_HEC_Diab_RA$Which_Top_CC!="No_top_Chief_Comp"),]$Has_Top_CC=1
table(tmp_HEC_Diab_RA$Has_Top_CC)


overall_sum<-sum(tmp_HEC_Diab_RA$Has_Top_CC)
overall_sum

HEC_Diab_RA$Has_Top_CC<-tmp_HEC_Diab_RA$Has_Top_CC

#
#
tmp_HEC_Diab_RA=tmp_HEC_Diab_RA[tmp_HEC_Diab_RA$Which_Top_CC!="No_top_Chief_Comp",]
table(tmp_HEC_Diab_RA$Which_Top_CC)

#
table(HEC_Diab_RA$Has_Top_CC)
table(tmp_HEC_Diab_RA$Which_Top_CC,tmp_HEC_Diab_RA$Has_Top_CC)
##shoukd be the same count


##
###################################### Top CPT Analysis ###########################
##
#
class(HEC_Diab_RA$CPT1)
#table(HEC_Diab_RA$CPT1)

top_CPT1 = as.data.frame(table(HEC_Diab_RA$CPT1))
top_CPT1 = top_CPT1[(top_CPT1$Var1!="Unknown"), ]
top_CPT1 = top_CPT1[order(-top_CPT1[,2]), ]
#top_CPT1 = head(top_CPT1,25)
top_CPT1

top_CPT2 = as.data.frame(table(HEC_Diab_RA$CPT2))
top_CPT2 = top_CPT2[(top_CPT2$Var1!="Unknown"), ]
top_CPT2 = top_CPT2[order(-top_CPT2[,2]), ]
#top_CPT2 = head(top_CPT2,25)
top_CPT2

top_CPT3 = as.data.frame(table(HEC_Diab_RA$CPT3))
top_CPT3 = top_CPT3[(top_CPT3$Var1!="Unknown"), ]
top_CPT3 = top_CPT3[order(-top_CPT3[,2]), ]
#top_CPT3 = head(top_CPT3,25)

top_CPT4 = as.data.frame(table(HEC_Diab_RA$CPT4))
top_CPT4 = top_CPT4[(top_CPT4$Var1!="UnKnown"), ]
top_CPT4 = top_CPT4[order(-top_CPT4[,2]), ]
#top_CPT4 = head(top_CPT4,25)

top_CPT5 = as.data.frame(table(HEC_Diab_RA$CPT5))
top_CPT5 = top_CPT5[(top_CPT5$Var1!="Unknown"), ]
top_CPT5 = top_CPT5[order(-top_CPT5[,2]), ]
#top_CPT5 = head(top_CPT5,25)

top_CPT6 = as.data.frame(table(HEC_Diab_RA$CPT6))
top_CPT6 = top_CPT6[(top_CPT6$Var1!="Unknown"), ]
top_CPT6 = top_CPT6[order(-top_CPT6[,2]), ]
#top_CPT6 = head(top_CPT6,25)

top_CPT7 = as.data.frame(table(HEC_Diab_RA$CPT7))
top_CPT7 = top_CPT7[(top_CPT7$Var1!="Unknown"), ]
top_CPT7 = top_CPT7[order(-top_CPT7[,2]), ]
#top_CPT7 = head(top_CPT7,25)

top_CPT8 = as.data.frame(table(HEC_Diab_RA$CPT8))
top_CPT8 = top_CPT8[(top_CPT8$Var1!="Unknown"), ]
top_CPT8 = top_CPT8[order(-top_CPT8[,2]), ]
#top_CPT8 = head(top_CPT8,25)
top_CPT8

top_CPT9 = as.data.frame(table(HEC_Diab_RA$CPT9))
top_CPT9 = top_CPT9[(top_CPT9$Var1!="Unknown"), ]
top_CPT9 = top_CPT9[order(-top_CPT9[,2]), ]
#top_CPT9 = head(top_CPT9,25)
top_CPT9

top_CPT10 = as.data.frame(table(HEC_Diab_RA$CPT10))
top_CPT10 = top_CPT10[(top_CPT10$Var1!="Unknown"), ]
top_CPT10 = top_CPT10[order(-top_CPT10[,2]), ]
#top_CPT10 = head(top_CPT10,25)


#top_CPT_1_thr_10=aggregate(cbind(top_CPT1,top_CPT2,top_CPT3,top_CPT4, top_CPT5, top_CPT6,top_CPT7,top_CPT8, top_CPT9,top_CPT10), FUN=sum)

top_CPT_1_thr_10 = rbind(top_CPT1,top_CPT2,top_CPT3, top_CPT4, top_CPT5, top_CPT6,top_CPT7,top_CPT8,top_CPT9,top_CPT10)
dim(top_CPT_1_thr_10)
top_CPT_1_thr_10 = top_CPT_1_thr_10[(top_CPT_1_thr_10$Var1!="Unknown"), ]
top_CPT_1_thr_10=aggregate(cbind(Freq)~Var1,FUN=sum,data=top_CPT_1_thr_10)
top_CPT_1_thr_10 = top_CPT_1_thr_10[order(-top_CPT_1_thr_10[,2]), ]
top_CPT_1_thr_10 = head(top_CPT_1_thr_10,10)
top_CPT_1_thr_10
dim(top_CPT_1_thr_10)

##

#top_CPT_1_thr_10$Var1=factor(as.character(top_CPT_1_thr_10$Var1))
class(top_CPT_1_thr_10$Var1)
overall_sum<-sum(top_CPT_1_thr_10$Freq)
overall_sum
#1] 
top_CPT_1_thr_10$Freq=round(100*(top_CPT_1_thr_10$Freq/overall_sum))
top_CPT_1_thr_10 = top_CPT_1_thr_10[order(-top_CPT_1_thr_10[,2]), ]
overall_sum1<-sum(top_CPT_1_thr_10$Freq)
overall_sum1
overall_sum
dim(top_CPT_1_thr_10)
top_CPT_1_thr_10
#     

##
###############  Top 10 CPTs across all patients ###############
####
#

ggplot(top_CPT_1_thr_10,aes(x=top_CPT_1_thr_10$Var1,y=top_CPT_1_thr_10$Freq))+
  geom_bar(aes(fill=Var1), stat="identity",  position = "dodge")+
  labs(title="2013-2018(AHG)",
       caption =  "Top 10 CPTs for Diabetic Patients",x="Procedure Codes",y="%Frequency",
       fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
ggsave("AHG_Diab-CPT.jpeg",width = 15,units = "cm")

dev.off()
#
####
#
#
HEC_Diab_RA$Which_Top_CPT=as.character("No_top_CPT")
table(HEC_Diab_RA$Which_Top_CPT)
top_CPT_1_thr_10$Var1=as.character(top_CPT_1_thr_10$Var1)
#class(top_CPT_1_thr_10$Var1)
nrow(top_CPT_1_thr_10)

for (i in 1:nrow(top_CPT_1_thr_10)) {
  HEC_Diab_RA[which(HEC_Diab_RA$CPT1 == top_CPT_1_thr_10$Var1[i]|HEC_Diab_RA$CPT2 == top_CPT_1_thr_10$Var1[i]|
                      HEC_Diab_RA$CPT3 == top_CPT_1_thr_10$Var1[i]|HEC_Diab_RA$CPT4 == top_CPT_1_thr_10$Var1[i]|
                      HEC_Diab_RA$CPT5 == top_CPT_1_thr_10$Var1[i]|HEC_Diab_RA$CPT6 == top_CPT_1_thr_10$Var1[i]|
                      HEC_Diab_RA$CPT7 == top_CPT_1_thr_10$Var1[i]|HEC_Diab_RA$CPT8 == top_CPT_1_thr_10$Var1[i]|
                      HEC_Diab_RA$CPT9 == top_CPT_1_thr_10$Var1[i]|HEC_Diab_RA$CPT10 == top_CPT_1_thr_10$Var1[i]),]$Which_Top_CPT=top_CPT_1_thr_10$Var1[i]
}
table(HEC_Diab_RA$Which_Top_CPT)
##


tmp_CPT_HEC_Diab_RA<-HEC_Diab_RA
tmp_CPT_HEC_Diab_RA$Has_Top_CPT=0
tmp_CPT_HEC_Diab_RA$Has_Top_CPT=as.integer((tmp_CPT_HEC_Diab_RA$Has_Top_CPT))
class(tmp_CPT_HEC_Diab_RA$Has_Top_CPT)
tmp_CPT_HEC_Diab_RA[which(tmp_CPT_HEC_Diab_RA$Which_Top_CPT!="No_top_CPT"),]$Has_Top_CPT=1
class(tmp_CPT_HEC_Diab_RA$Has_Top_CPT)
table(tmp_CPT_HEC_Diab_RA$Has_Top_CPT)
overall_sum<-sum(tmp_CPT_HEC_Diab_RA$Has_Top_CPT)
overall_sum
#
### Has_Top_CPT is a flag for each patient. It is 1 if the patient has at least on of the top CPTs, 0 otherwise
## Include this the dataset for to have it as one of the perdictor in the Models to be developed in the next Module
#
HEC_Diab_RA$Has_Top_CPT<-tmp_CPT_HEC_Diab_RA$Has_Top_CPT
#
#
tmp_CPT_HEC_Diab_RA=tmp_CPT_HEC_Diab_RA[tmp_CPT_HEC_Diab_RA$Which_Top_CPT!="No_top_CPT",]
table(tmp_CPT_HEC_Diab_RA$Which_Top_CPT)
table(HEC_Diab_RA$Has_Top_CPT)
table(tmp_CPT_HEC_Diab_RA$Has_Top_CPT)


table(Top_CC_1_thr_8,top_CPT_1_thr_10)
table(top_CPT_1_thr_10)
##shoukd be the same count
###########
############################### ## Top 10 Dignosis Codes vs. Top 10 CPTs ########################################
# jpeg("C:/Users/Nabil.Adam/Documents/Adam/ML/Output/AHG-Output/Plots/AHG_Diab-Plots-CC-CPT.jpeg",pointsize = 10, width=880, height=680,bg = "white")
ggplot(tmp_CPT_HEC_Diab_RA[tmp_CPT_HEC_Diab_RA$Which_Top_CC!="No_top_Chief_Comp",],aes(x=Which_Top_CC))+ 
  geom_bar(aes(fill=Which_Top_CPT), position='dodge')+
 labs(caption="Top 10 Diagnosis Codes vs Top 10 CPTs for Diabetic Patients",title ="2013-2018 (AHG)",
      x="ICD (v9/v10) Diagnosis Codes",
      y="Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
ggsave("AHG_Diab-Plots-CC-CPT.jpeg",width = 20,units = "cm")
dev.off()

tmp_HEC_Diab_RA_No_CC_CPT<-HEC_Diab_RA
dim(tmp_HEC_Diab_RA_No_CC_CPT)
tmp_HEC_Diab_RA_No_CC_CPT<- tmp_HEC_Diab_RA_No_CC_CPT[tmp_HEC_Diab_RA_No_CC_CPT$Which_Top_CPT!="No_top_CPT",]
dim(tmp_HEC_Diab_RA_No_CC_CPT)
tmp_HEC_Diab_RA_No_CC_CPT<- tmp_HEC_Diab_RA_No_CC_CPT[tmp_HEC_Diab_RA_No_CC_CPT$Which_Top_CC!="No_top_Chief_Comp",]
dim(tmp_HEC_Diab_RA_No_CC_CPT)
table(tmp_HEC_Diab_RA_No_CC_CPT$Which_Top_CPT,tmp_HEC_Diab_RA_No_CC_CPT$Which_Top_CC)




################################# Top 10 Diagnosis Codes by Year #############################
#
class(tmp_HEC_Diab_RA$YEAR)
table(tmp_HEC_Diab_RA$YEAR)
table(tmp_HEC_Diab_RA$YEAR,tmp_HEC_Diab_RA$Which_Top_CC)

ggplot(data =tmp_HEC_Diab_RA,aes(x=YEAR,group=Which_Top_CC) )+
  geom_bar(aes(fill=Which_Top_CC),position = "dodge")+
  labs(caption="Top 10 Diagnosis Codes for Diabetic Patients by Year",
       x="ICD (v9/v10) Diagnosis Codes",
       y="Frequency",fill="ICD Codes",title = "2013-2018(AHG)")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
              axis.title.x = element_text(color="black", size=10, face="bold"),
              axis.title.y = element_text(color="black", size=10, face="bold"),
              plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
              legend.title =element_text(size=10,face="bold",hjust=0.5),
              axis.text.x = element_text(size = 8,face="bold",angle = 45),
              legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
              panel.background = element_rect(fill = "white"),
              legend.key.size = unit(0.5, "cm"),
              panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
              panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
        
ggsave("AHG_Diab-Plots-CC-Yr.jpeg",width = 20,units = "cm")
dev.off()
#

############################ Top 10 Diagnosis Codes by Alive/Died in Hospital ############################


ggplot(data =tmp_HEC_Diab_RA,aes(x=Which_Top_CC,group=DIED) )+
  geom_bar(aes(fill=DIED),position = "dodge")+
  labs(caption="Top 10 Diagnosis Codes for Diabetic Patients by Alive/Died in Hospital",
       x="ICD (v9/v10) Diagnosis Codes",
       y="Frequency",fill="",title = "2013-2018(AHG)")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))

  
ggsave("AHG_Diab-Plots-CC-Died.jpeg",width = 20,units="cm")

dev.off()


#
########################## Top 10 Diagnosis Codes  by Non-Readmission/Readmission #######################
#
ggplot(data =tmp_HEC_Diab_RA,aes(x=Which_Top_CC,group=Which_Top_CC))+
  geom_bar(aes(fill=Which_Top_CC),position = "dodge")+
  labs(caption="Top 10 Diagnosis Codes for Diabetic Patients by Non-Readmit Admission/Readmission",x="ICD Codes (v9/v10) of Diganosis Codes",
       y="Frequency",fill="Top 10 Diagnosis Codes",title = "2013-2018(AHG)")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
facet_wrap(.~READMIT,scales = "free")
ggsave("AHG_Diab-Plots-CC-REA.jpeg",width = 20,units="cm")
dev.off()


################# Top 10 Diagnosis Codes  by Non ER/ER #################
ggplot(tmp_HEC_Diab_RA,aes(x=Which_Top_CC))+
  labs(caption="Top 10 Diagnosis Codes for Diabetic Patients by Non-ER/ER",
       title ="2013-2018 (AHG)",
       x="ICD (v9/v10) Diagnosis Codes",y="Frequency",
       fill="ICD Codes")+
  geom_bar(aes(fill=Which_Top_CC),position = "dodge" )+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~ERVISIT,scales = "free")
  
ggsave("AHG_Diab-Plots-CC-ER.jpeg",width = 20,units = "cm")
dev.off()
#
############### Top 10 Diagnosis Codes by Gender ###################
#
class(tmp_HEC_Diab_RA$FEMALE)

table(tmp_HEC_Diab_RA$FEMALE)
table(HEC_Diab_RA$FEMALE)

ggplot(tmp_HEC_Diab_RA,aes(x=Which_Top_CC))+
  labs(caption="Top 10 Diagnosis Codes for Diabetic Patients by Gender",
       title ="2013-2018 (AHG)",
       x="ICD (v9/v10) Diagnosis Codes",y="Frequency",
       fill="ICD Codes")+
  geom_bar(aes(fill=Which_Top_CC),position = "dodge" )+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~FEMALE,scales = "free")

ggsave("AHG_Diab-Plots-CC-G.jpeg",width = 20,units = "cm")


dev.off()
#
######################## Top 10 Diagnosis Codes  by Payer #####################################
#
class(tmp_HEC_Diab_RA$PAY1)
table(tmp_HEC_Diab_RA$PAY1)
sum(is.na(tmp_CPT_HEC_Diab_RA$PAY1))

tmp_HEC_Diab_RA_payer<-tmp_HEC_Diab_RA[-which(tmp_HEC_Diab_RA$PAY1=="OTHER"|tmp_HEC_Diab_RA$PAY1=="Unknown"),]


#
##########################  Top  Complaintsby MSSP Patients Only, since it is over 90% of Patients ########################
class(tmp_HEC_Diab_RA$PAY1)
table(tmp_HEC_Diab_RA$PAY1)
table(tmp_HEC_Diab_RA_payer$PAY1)
sum(is.na(tmp_HEC_Diab_RA_payer$PAY1))
tmp_HEC_Diab_RA_payer$PAYER_FLAG<-"Unknown"
tmp_HEC_Diab_RA_payer[tmp_HEC_Diab_RA_payer$PAY1=="MSSP",]$PAYER_FLAG<-"MSSP Only"
tmp_HEC_Diab_RA_payer[tmp_HEC_Diab_RA_payer$PAY1!="MSSP",]$PAYER_FLAG<-"Medicaid/ Medicare/ Medicare Adv/ Private"
tmp_HEC_Diab_RA_payer$PAYER_FLAG<-factor(tmp_HEC_Diab_RA_payer[,"PAYER_FLAG"],levels = c("MSSP Only","Medicaid/ Medicare/ Medicare Adv/ Private"))

ggplot(tmp_HEC_Diab_RA_payer,aes(x=Which_Top_CC))+
  geom_bar(aes(fill=Which_Top_CC), position ="dodge")+
  labs(caption="Top 10 Diagnosis Codes for Diabetic Patients by Payer",
       title="2013-2018 (AHG)",x="ICD (v9/v10) Diagnosis Codes",y="Frequency",
       fill="ICD Codes")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~PAYER_FLAG,scales="free")
ggsave("AHG_Diab-Plots-CC-payer.jpeg",width = 22,units="cm")
dev.off()



#
################# Top 10 Diagnosis Codes  Weeked/Weekday #################
#
ggplot(tmp_HEC_Diab_RA,aes(x=Which_Top_CC))+ 
    geom_bar(aes(fill=Which_Top_CC), position = "dodge")+
  labs(title="2013-2018(AHG)",
       caption ="Top 10 Diagnosis Codes for Diabetic Patients by Day of The Week",
       x="ICD (v9/v10) Diagnosis Codes",y="Frequency",fill="Top Complaints")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~AWEEKEND,scales ="free")
ggsave("AHG_Diab-Plots-CC-WE.jpeg",width = 20,units="cm")
dev.off()
#

###
################## Top 10 Diagnosis Codes  by Hospital state... FL, NJ, PA, and TN #########

#
class(tmp_HEC_Diab_RA$HOSPSTATE)
tmp_HEC_Diab_RA_NJ_PA_FL_TN=tmp_HEC_Diab_RA[tmp_HEC_Diab_RA$HOSPSTATE=="FL"|tmp_HEC_Diab_RA$HOSPSTATE=="NJ"|tmp_HEC_Diab_RA$HOSPSTATE=="PA"|tmp_HEC_Diab_RA$HOSPSTATE=="TN",]
table(tmp_HEC_Diab_RA_NJ_PA_FL_TN$HOSPSTATE)
ggplot(tmp_HEC_Diab_RA_NJ_PA_FL_TN,aes(x=Which_Top_CC))+ 
  geom_bar(aes(fill=Which_Top_CC), position = "dodge")+
  labs(title="2013-2018(AHG)",
       caption ="Top 10 Diagnosis Codes for Diabetic Patients by Hospital State: FL, NJ, PA, & TN",
       x="ICD (v9/v10) Diagnosis Codes",y="Frequency",fill="Top Complaints")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~HOSPSTATE,scales ="free")

ggsave("AHG_Diab-Plots-CC-St.jpeg",width = 22,units="cm")
dev.off()


##
###################################### Top CPT Analysis ###########################
##
#
########################### Top 10 CPTs by Year ####################################
#
dim(tmp_CPT_HEC_Diab_RA)
class(tmp_CPT_HEC_Diab_RA$YEAR)
table(tmp_CPT_HEC_Diab_RA$YEAR)
table(tmp_CPT_HEC_Diab_RA$YEAR,tmp_CPT_HEC_Diab_RA$Which_Top_CPT)

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=YEAR))+
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption = "Top CPTs for Diabetic Patients by Year",
       title="2013-2018 (AHG)", x = "Year",
       y = "Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
  
 
ggsave("AHG_Diab-Plots-CPT-Yr.jpeg",width = 15,units = "cm")
dev.off()
#
#

################# Top 10 CPTs by Alive/Died #################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Which_Top_CPT))+
  geom_bar(aes(fill=DIED), position = "dodge")+
  labs(caption = "Top CPTs for Diabetic Patients by Alive/Died in Hospital",
       title="2013-2018 (AHG)", x = "Procedure Codes",
       y = "Frequency",fill="")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
  

ggsave("AHG_Diab-Plots-CPT-Died.jpeg",width = 15,units = "cm")
dev.off()
#

#
########### Top 10 CPTS by Non-Readmission/Readmission #######################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Which_Top_CPT))+
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption = "Top CPTs for Diabetic Patients by Non-Readmit Admission/Readmission",
       title="2013-2018 (AHG)", x = "Procedure Codes",
       y = "Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~READMIT,scales="free")
        
ggsave("AHG_Diab-Plots-CPT-REA.jpeg",width = 20,units = "cm")


dev.off()
#
############################ Top 10 CPTs by Non ER/ER #################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Which_Top_CPT))+ 
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption="Top CPTs for Diabetic Patients by Non-ER/ER",
       title="2013-2018 (AHG)",x="Procedure Codes",y="Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
    facet_wrap(.~ERVISIT,scales="free")
ggsave("AHG_Diab-Plots-CPT-ER.jpeg",width = 22,units = "cm")

dev.off()
#
################################# Top 10 CPTs by Gender ############################
#
class(tmp_CPT_HEC_Diab_RA$FEMALE)

table(tmp_CPT_HEC_Diab_RA$FEMALE)
table(HEC_Diab_RA$FEMALE)

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Which_Top_CPT))+ 
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption="Top CPTs for Diabetic Patients by Gender",
       title="2013-2018 (AHG)",x="Procedure Codes",y="Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
    facet_wrap(.~FEMALE,scales="free")
ggsave("AHG_Diab-Plots-CPT-G.jpeg",width = 20,units = "cm")

dev.off()
#
#################################### Top 10 CPTs by Payer ###################################
#
class(tmp_CPT_HEC_Diab_RA$PAY1)
table(tmp_CPT_HEC_Diab_RA$PAY1)
sum(is.na(tmp_CPT_HEC_Diab_RA$PAY1))
sum(is.na(tmp_HEC_Diab_RA$PAY1))
sum(is.na(HEC_Diab_RA$PAY1))
tmp_CPT_HEC_Diab_RA_payer<-tmp_CPT_HEC_Diab_RA[(tmp_CPT_HEC_Diab_RA$PAY1!="OTHER"&tmp_CPT_HEC_Diab_RA$PAY1!="Unknown"),]
table(tmp_CPT_HEC_Diab_RA_payer$PAY1)
#
##############################  Top 10 CPTs by MSSP Patients and Medicaid, Medicare, Medicare Advantage, or Private insurance Patients ########################
class(tmp_CPT_HEC_Diab_RA$PAY1)
table(tmp_CPT_HEC_Diab_RA$PAY1)

tmp_CPT_HEC_Diab_RA_payer$PAYER_FLAG<-"Unknown"
tmp_CPT_HEC_Diab_RA_payer[tmp_CPT_HEC_Diab_RA_payer$PAY1=="MSSP",]$PAYER_FLAG<-"MSSP Only"
tmp_CPT_HEC_Diab_RA_payer[tmp_CPT_HEC_Diab_RA_payer$PAY1!="MSSP",]$PAYER_FLAG<-"Medicaid/ Medicare/ Medicare Adv/ Private"
tmp_CPT_HEC_Diab_RA_payer$PAYER_FLAG<-factor(tmp_CPT_HEC_Diab_RA_payer[,"PAYER_FLAG"],levels = c("MSSP Only","Medicaid/ Medicare/ Medicare Adv/ Private"))


table(tmp_CPT_HEC_Diab_RA_payer$PAY1)
sum(is.na(tmp_CPT_HEC_Diab_RA_payer$PAY1))
sum(is.na(tmp_CPT_HEC_Diab_RA_payer$PAY1))

ggplot(tmp_CPT_HEC_Diab_RA_payer,aes(x=Which_Top_CPT))+
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption = "Top CPTs for Diabetic Patients by Payer",title="2013-2018 (AHG)",x="Procedure Codes",y="Frequency",fill="ICD Codes")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~PAYER_FLAG,scales="free")
ggsave("AHG_Diab-Plots-CPT-by-payer.jpeg",width = 20,units="cm")
dev.off()
#
################# 28. Top CPTS by Weeked/Weekday #################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Which_Top_CPT))+
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption = "Top 10 CPTs for Diabetic Patients by Day of The Week",
       title="2013-2018 (AHG)",
       x="Procedure Codes",y="Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~AWEEKEND,scale="free")

ggsave("AHG_Diab-Plots-CPT-WE.jpeg",width = 20,units = "cm")

dev.off()
#


################## Top 10 CPTs by Hospital state... FL, NJ, PA, and TN #########

class(tmp_CPT_HEC_Diab_RA$HOSPSTATE)
tmp_CPT_HEC_Diab_RA_NJ_PA_FL_TN=tmp_CPT_HEC_Diab_RA[tmp_CPT_HEC_Diab_RA$HOSPSTATE=="FL"|tmp_CPT_HEC_Diab_RA$HOSPSTATE=="NJ"|tmp_CPT_HEC_Diab_RA$HOSPSTATE=="PA"|tmp_CPT_HEC_Diab_RA$HOSPSTATE=="TN",]
table(tmp_CPT_HEC_Diab_RA_NJ_PA_FL_TN$HOSPSTATE)

ggplot(tmp_CPT_HEC_Diab_RA_NJ_PA_FL_TN,aes(x=Which_Top_CPT))+
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption = "Top CPTS for Diabetic Patients by Hospital Stat: FL, NJ, PA, and TN",
       title="2013-2018 (AHG)",
       x="Procedure Codes",y="Frequency",fill="Top CPTs")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
    facet_wrap(.~HOSPSTATE,scale="free")

ggsave("AHG_Diab-Plots-CPT-St.jpeg",width = 22,height=15,units="cm")

dev.off()
##

###### Top 10 CC- Daibetic codes by Year #######


ggplot(data =tmp_HEC_Diab_RA,aes(x=YEAR,group=Which_Top_CC) )+
  geom_bar(aes(fill=Which_Top_CC),position = "dodge")+
  labs(caption="Diabetic types of Top 10 Diagnosis Codes by Year",
       x="Diabetic Types",
       y="Frequency",fill="Diabetic Type",title = "2013-2018(AHG)")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~Diab.Type,scales = "free")

ggsave("AHG_Diab-Plots-CC-DT-YR.jpeg",width = 20,units = "cm")
dev.off()

#################### Top 10 CC- Diabetic Codes by Alive/Died in Hospital ############################

ggplot(data =tmp_HEC_Diab_RA,aes(x=Diab.Type,group=DIED) )+
  geom_bar(aes(fill=DIED),position = "dodge2")+
  labs(caption="Diabetic types of Top 10 Diagnosis Codes by Alive/Died in Hospital",
       x="Diabetic Types",
       y="Frequency",fill="",title = "2013-2018(AHG)")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
# +  facet_wrap(.~Diab.Type,scales="free")

ggsave("AHG_Diab-Plots-CC-DT-Died.jpeg",width = 20,units="cm")

dev.off()

########################## Top 10 CC- Diabetic Codes by Non-Readmission/Readmission #######################
#
ggplot(data =tmp_HEC_Diab_RA,aes(x=Diab.Type))+
  geom_bar(aes(fill=Diab.Type),position = "dodge")+
  labs(caption="Diabetic types of Top 10 Diagnosis Codes by Non-Readmit Admission/Readmission",
       x="Diabetic Types",
       y="Frequency",fill="Diabetic Types",title = "2013-2018(AHG)")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~READMIT,scales = "free")

ggsave("AHG_Diab-Plots-CC-DT-REA.jpeg",width = 20,units="cm")
dev.off()

################# Top 10 CC- Diabetic Codes by Non ER/ER #################
ggplot(tmp_HEC_Diab_RA,aes(x=Diab.Type))+
  labs(caption="Diabetic types of Top 10 Diagnosis Codes by Non-ER/ER",
       title ="2013-2018 (AHG)",
       x="Diabetic Types",y="Frequency",
       fill="Diabetic Types")+
  geom_bar(aes(fill=Diab.Type),position = "dodge" )+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~ERVISIT,scales = "free")

ggsave("AHG_Diab-Plots-CC-ER.jpeg",width = 20,units = "cm")
dev.off()


############### Top 10 CC- Diabetic Codes by  Gender ###################
#
ggplot(tmp_HEC_Diab_RA,aes(x=Diab.Type))+
  labs(caption="Diabetic types of Top 10 Diagnosis Codes by Gender",
       title ="2013-2018 (AHG)",
       x="Diabetic Types",y="Frequency",
       fill="Diabetic Types")+
  geom_bar(aes(fill=Diab.Type),position = "dodge" )+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~FEMALE,scales = "free")

ggsave("AHG_Diab-Plots-CC-DT-GENDER.jpeg",width = 20,units = "cm")


dev.off()

######################## Top 10 Diagnosis Codes by Payer #####################################

ggplot(tmp_HEC_Diab_RA_payer,aes(x=Diab.Type))+
  geom_bar(aes(fill=Diab.Type), position ="dodge")+
  labs(caption="Diabetic types of Top 10 Diagnosis Codes by Payer",
       title="2013-2018 (AHG)",x="Diabetic Types",y="Frequency",
       fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~PAYER_FLAG,scales="free")
ggsave("AHG_Diab-Plots-CC-DT-payer.jpeg",width = 22,units="cm")
dev.off()

#
################# Top 10 Diagnosis Codes  by  Weeked/Weekday #################
#
ggplot(tmp_HEC_Diab_RA,aes(x=Diab.Type))+ 
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(title="2013-2018(AHG)",
       caption ="Diabetic types of Top 10 Diagnosis Codes by Day of The Week",
       x="Diabetic Types",y="Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~AWEEKEND,scales ="free")
ggsave("AHG_Diab-Plots-CC-WE.jpeg",width = 20,units="cm")
dev.off()

################## Top 10 Diagnosis Codes  by Hospital state... FL, NJ, PA, and TN #########
### First the 4 states side by side then each separately ################
#

ggplot(tmp_HEC_Diab_RA_NJ_PA_FL_TN,aes(x=Diab.Type))+ 
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(title="2013-2018(AHG)",
       caption ="Diabetic types of Top 10 Diagnosis Codes by Hospital State: FL, NJ, PA, & TN",
       x="Diabetic Type",y="Frequency",fill="Diabetic Type")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~HOSPSTATE,scales ="free")

ggsave("AHG_Diab-Plots-CC-St.jpeg",width = 22,units="cm")
dev.off()


########################### Top 10 Diagnosis Codes  by Year ####################################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=YEAR,group=Which_Top_CPT))+
  geom_bar(aes(fill=Which_Top_CPT), position = "dodge")+
  labs(caption = "Diabetic types of Top 10 CPTs by Year",
       title="2013-2018 (AHG)", x = "Year",
       y = "Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~Diab.Type,scales="free")


ggsave("AHG_Diab-Plots-CPT-DT-Yr.jpeg",width = 15,units = "cm")
dev.off()

################# Top 10 Diagnosis Codes by Alive/Died #################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Diab.Type))+
  geom_bar(aes(fill=DIED), position = "dodge2")+
  labs(caption = "Diabetic types of Top 10 CPTs by Alive/Died in Hospital",
       title="2013-2018 (AHG)", x = "Diabetic Types",
       y = "Frequency",fill="")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))
  # facet_wrap(.~Diab.Type,scales="free")


ggsave("AHG_Diab-Plots-CPT-DT-Died.jpeg",width = 15,units = "cm")
dev.off()
#


########### Top 10 Diagnosis Codes by Non-Readmission/Readmission #######################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Diab.Type))+
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(caption = "Diabetic types of Top 10 CPTs by Non-Readmit /Readmission",
       title="2013-2018 (AHG)", x = "Diabetic Types",
       y = "Frequency",fill="Diabetic Type")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~READMIT,scales="free")

ggsave("AHG_Diab-Plots-CPT-DT-REA.jpeg",width = 20,units = "cm")
dev.off()

############################Top 10 CPT- Diabetic Codes by Non ER/ER #################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Diab.Type))+ 
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(caption="Diabetic types of Top 10 CPTs by Non-ER/ER",
       title="2013-2018 (AHG)",x="Diabetic Types",y="Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~ERVISIT,scales="free")

ggsave("AHG_Diab-Plots-CPT-ER.jpeg",width = 22,units = "cm")

dev.off()

################################# Top 10 CPT- Diabetic Codes by Gender ############################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Diab.Type))+ 
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(caption="Diabetic types of Top 10 CPTs by Gender",
       title="2013-2018 (AHG)",x="Diabetic Types",y="Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~FEMALE,scales="free")
ggsave("AHG_Diab-Plots-CPT-G.jpeg",width = 20,units = "cm")

dev.off()
#

#################################### Top 10 CPT- Diabetic Codes by Payer ###################################


ggplot(tmp_CPT_HEC_Diab_RA_payer,aes(x=Diab.Type))+
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(caption = "Diabetic types of Top 10 CPTs Payer",title="2013-2018 (AHG)",
       x="Diabetic Types",y="Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~PAYER_FLAG,scales="free")

ggsave("AHG_Diab-Plots-CPT-DT-by-payer.jpeg",width = 20,units="cm")
dev.off()
#
################# 28. Top CPTS by Weeked/Weekday #################
#

ggplot(tmp_CPT_HEC_Diab_RA,aes(x=Diab.Type))+
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(caption = "Diabetic types of Top 10 CPTs by Day of The Week",
       title="2013-2018 (AHG)",
       x="Diabetic Types",y="Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~AWEEKEND,scale="free")

ggsave("AHG_Diab-Plots-CPT-WE.jpeg",width = 20,units = "cm")

dev.off()
#


################## Top 10 CPTs by Hospital state... FL, NJ, PA, and TN #########

ggplot(tmp_CPT_HEC_Diab_RA_NJ_PA_FL_TN,aes(x=Diab.Type))+
  geom_bar(aes(fill=Diab.Type), position = "dodge")+
  labs(caption = "Diabetic types of Top 10 CPTs by Hospital Stat: FL, NJ, PA, and TN",
       title="2013-2018 (AHG)",
       x="Diabetic Types",y="Frequency",fill="Diabetic Types")+
  theme(plot.title = element_text(color="black", size=10, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=10, face="bold"),
        axis.title.y = element_text(color="black", size=10, face="bold"),
        plot.caption = element_text(color = "blue",size=12,face="bold.italic",hjust=0.5),
        legend.title =element_text(size=10,face="bold",hjust=0.5),
        axis.text.x = element_text(size = 8,face="bold",angle = 45),
        legend.background = element_rect(linetype = "solid",color ="black",size = 0.1 ),
        panel.background = element_rect(fill = "white"),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(size = 0.3, linetype = 'dotted',colour = "black"),
        panel.grid.minor = element_line(size = 0.3, linetype = 'dotted',colour = "black"))+
  facet_wrap(.~HOSPSTATE,scale="free")

ggsave("AHG_Diab-Plots-CPT-DT-St.jpeg",width = 22,height=15,units="cm")

dev.off()
##

#Next step is Model Development ####################
#### write out a subset of the output of this Module to be as input to te next Module: Model Development


#str just after writing the file 
str(HEC_Diab_RA)
setwd("C:\\Users\\Nabil.Adam\\Documents\\Adam\\ML\\Datasets")
write.csv(HEC_Diab_RA,"HEC_Diab_RA_subset_DE.csv")
dim(HEC_Diab_RA)
sum(is.na(HEC_Diab_RA))
colSums(is.na(HEC_Diab_RA))
#Check slected fields before starting the Analyses
#
#### End
#######################################################################################################

