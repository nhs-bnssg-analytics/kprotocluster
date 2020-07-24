#----------------------------------------------------------------------------------------------
######################################
###K-prototype model for mixed data###
######################################
#01/07/2020
#----------------------------------------------------------------------------------------------
#Load required packages
install.packages("clustMixType")
library(clustMixType)
install.packages("dplyr")
library(dplyr)
install.packages("RcolorBrewer")
library(RColorBrewer)


#----------------------------------------------------------------------------------------------
#Two clean datasets
#Dataset with the clustering variables
shielded_dataset <- readRDS(file = "C:/COVID-19/shielded_dataset.RDS")


#Dataset with all clustering variables plus non clustering variables to describe
#the different clusters by
allvariable_dataset <-readRDS(file = "C:/COVID-19/allvariable_dataset.RDS")
#----------------------------------------------------------------------------------------------
#Scale Numeric Variables
#Consider other transformations as appropriate e.g. log normalising attendance appts

shielded_analysis <- shielded_analysis %>%
  mutate(GP_community_appt = scale(GP_community_appt))%>%
  mutate(SC_elective_appt = scale(SC_elective_appt))%>%
  mutate(mentalhealth_appt = scale(mentalhealth_appt))%>%
  mutate(SC_nonelective_appt = scale(SC_nonelective_appt))%>%
  mutate(age = scale(age))

#----------------------------------------------------------------------------------------------
##Determining value of the lambda parameter (uses default method)
lambdaest(shielded_analysis %>% select(-nhs_number), num.method = 1, fac.method = 1, outtype = "numeric")

#----------------------------------------------------------------------------------------------
#Investigating number of clusters for the model
#Scree plot

#Set seed as there is a random initiation component to the algorithm and this ensures reproducible results
set.seed(123)


Es <- numeric(10)
for(i in 1:10){
  kpres <- kproto(shielded_analysis %>% select(-nhs_number), k=i, lambda = NULL, iter.max = 100000,
                  nstart = 50, na.rm = TRUE, keep.data = TRUE, verbose = FALSE)
  Es[i] <- kpres$tot.withinss
}

#Plot total within sum of squares against number of clusters
plot(1:10, Es, type = "b", ylab = "Total Within Sum of Squares", xlab = "Number of Clusters")


#Plotting cluster 2 to 10 only
EEE<- c(309757.1, 286488.8, 264354.7, 243689.9, 227313.3, 217342.0, 206578.7, 196894.5, 191006.9)

plot(2:10, EEE, type = "b", ylab = "Total Within Sum of Squares", xlab = "Number of Clusters")


#----------------------------------------------------------------------------------------------
#Investigate different indexes
#For this project we used the silouette index, but this can be replaced in the "" for 
#a number of other indexes (cindex, dunn etc) see clustMixType documentation

#If you have a large complex dataset you may want to keep track of how long a function takes to run
start_time <-Sys.time() 

gc()
set.seed(123)
silhouette <- validation_kproto(method = "silhouette", data = shielded_analysis %>% select(-nhs_number), k = 1:10, nstart = 20)

finish_time<-Sys.time() 

saveRDS(silhouette, "C:/COVID-19/silhouette.rds")


#----------------------------------------------------------------------------------------------
#Another method:

#create a list for silhouette index
silhouetteindex <- list()

#Loop through clusters 2 to 10 and give corresponding silhouette index values and save as a list
for (i in 2:10) {
gc()
set.seed(123)
silhouette <- kproto((data = shielded_analysis %>% select(-nhs_number)), i, keep.data = TRUE, nstart=1, verbose = FALSE, iter.max = 10)

silhouetteindex[i] <- validation_kproto(method = "silhouette", object = silhouette)}

#----------------------------------------------------------------------------------------------

#Apply 6 clusters to the k-prototype model:
#The k argument can be changed to the number of clusters you are interested in modelling

gc()
k<-6
set.seed(123)

kprotomod_6 <- kproto((shielded_analysis %>% select(-nhs_number)), k, lambda = NULL, iter.max = 1000000,
                             nstart = 1000, na.rm = TRUE, keep.data = TRUE, verbose = FALSE)

#Save the model
saveRDS(kprotomod_6, "C:/COVID-19/kprotomod_6.rds")

#To read the model back into R
#kprotomod_6 <- readRDS("C:/COVID-19/kprotomod_6.rds")

#----------------------------------------------------------------------------------------------
#Cluster profiles for the model with 6 clusters:

#To display plots in a 2 by 2 grid
par(mfrow=c(2,2))

clprofiles(kprotomod_6, shielded_analysis %>% select(-nhs_number), col = brewer.pal(6, "Set3"))

#----------------------------------------------------------------------------------------------
#Code to make further manual adjustments to these plots if required:

#To display plots in a 2 by 2 grid
par(mfrow=c(2,2))

#Select colour palette
col <- brewer.pal(6, "Set3")

#Boxplots for numerical variables (change variable name as appropriate)
boxplot(shielded_analysis$GP_community_appt ~ kprotomod_6$cluster, main = "GP/Community Appointments", xlab = "Cluster", ylab ="GP Community Appts Scaled", col = col)

#Barplots for categorical variables (change variable name as appropriate)
tab <- table(shielded_analysis$cardio_group,kprotomod_6$cluster)
for (j in 1:length(kprotomod_6$size)) tab[, j] <- tab[,j]/kprotomod_6$size[j]

barplot(t(tab), beside = TRUE, main = "Cardiovascular Condition", ylab ="Proportion", col = col, ylim = range(pretty(c(0, t(tab)))))

legend("topright", 
       legend = c(1:6), 
       fill = col,
       cex = 0.75)

#----------------------------------------------------------------------------------------------
#Investigating proportions of categorical variables 
#and mean/median, IQR for numerical variables by Cluster


#Attach the assigned cluster number to each individual in the dataset which includes both clustering
#and non clustering variables

cluster_investigations <-  allvariable_dataset%>%
  mutate(cluster=kprotomod_6$cluster)


#For numerical Variables:

cluster_investigations %>% 
  group_by(cluster) %>%
  summarise(`25%`=quantile(charlson_score, probs=0.25),
            `50%`=quantile(charlson_score, probs=0.5),
            `75%`=quantile(charlson_score, probs=0.75),
            avg=median(charlson_score),
            n=n())


median<- cluster_investigations %>% 
  group_by(cluster) %>% 
  summarise_if(is.numeric, median, na.rm = TRUE)

Q1<- cluster_investigations %>% 
  group_by(cluster) %>% 
  summarise_if(is.numeric, list(Q1 = quantile), probs = 0.25, na.rm = TRUE)

Q3<- cluster_investigations %>% 
  group_by(cluster) %>% 
  summarise_if(is.numeric, list(Q3 = quantile), probs = 0.75, na.rm = TRUE)

#For categorical variables

cluster_investigations %>%
  group_by(cluster) %>% 
  count(sex)

#----------------------------------------------------------------------------------------------










