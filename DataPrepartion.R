###################################################################################
# This corresponds to data assembly for the analsis of color data. The data comes #
# from an experiment where 33 soil fungi were grown on a temperature gradient     #
# ranging from 12 to 33C. The data on biomass, radii and pictures for color were  #
# stored in separate files. The following code assembles all these data into a    #
# a single data frame for analysis                                                #
###################################################################################

#load all packages
library(tidyverse)
library(vegan)
library(lattice) #plotting and splitting by an argument
library(lme4) #statistics
library(car) #statistics
library(lattice)
library(plyr)

###Step 1: prepare and merge the data for color analysis

temp = list.files(path="color_data/",pattern="*.csv")
temp<-paste("color_data/",temp,sep = "")
myfiles = lapply(temp,function(x){read.csv(x,header = TRUE,stringsAsFactors = FALSE)} )

#Transform the list "mylist" into a matrix that includes all data
allfungi<-rbind.fill.matrix(myfiles)
          allfungi[is.na(allfungi[,10]),]
          which(is.na(allfungi[,10]))

allfungi2<-data.frame(allfungi,stringsAsFactors = FALSE)
            allfungi2$fungus_temp[grep("c35",allfungi2$fungus_temp)]<-"C35_33"
            allfungi2$fungus_temp[grep("c43",allfungi2$fungus_temp)]<-"C33_33"
            allfungi2[grep("C_15",allfungi2$fungus_temp),]
            allfungi2$fungus_temp[25:28]<-"C11_15"
            allfungi2[133:136,]$fungus_temp<-"C23_27"
            allfungi2[425:428,]$fungus_temp<-"DF09_33"
            allfungi2[505:508,]$fungus_temp<-"DF16_30"
            allfungi2[509:512,]$fungus_temp<-"DF16_33"
            unique(allfungi2$Pilze)

            allfungi2$R.mean<-as.numeric(allfungi2$R.mean) #modify the data for allfungi2
            allfungi2$G.mean<-as.numeric(allfungi2$G.mean)
            allfungi2$B.mean<-as.numeric(allfungi2$B.mean)
            allfungi2$K.mean<-as.numeric(allfungi2$K.mean)
            
            allfungi2$Pilze<-sapply(strsplit(allfungi2$fungus_temp,"_"), 
                                    function(x)x[1])
            
            allfungi2$temp<-sapply(strsplit(allfungi2$fungus_temp,"_"), 
                                   function(x)x[2]);
            allfungi2$temp<-as.numeric(allfungi2$temp)




#Since each fungus had two replicates at each temperature,
#we calculated the mean of black channel color (K.mean) for each fungus per temperature
allDataColor<-
              sapply(
                split(allfungi2,allfungi2$fungus_temp),
                function(x){mean(x$K.mean)})
            allDataColor<-data.frame(allDataColor)
            names(allDataColor)<-"K.mean"
            
            allDataColor$fungus<-
              sapply(strsplit(rownames(allDataColor),"_"), 
                     function(x)x[1]);
            
            allDataColor$temp<-
              sapply(strsplit(rownames(allDataColor),"_"), 
                     function(x)x[2]);
            allDataColor$temp<-as.numeric(allDataColor$temp);
            allDataColor$fung_temp<-rownames(allDataColor);
            
            row.names(allDataColor)<-NULL



#Then we load the biomass data and tranform it so it can be combined with the color
AllDataBiomass<-read.csv("FungalBiomass.csv",header = TRUE,stringsAsFactors = FALSE)

Biomass<-sapply(split(AllDataBiomass,list(AllDataBiomass$Species,AllDataBiomass$Temperature)),
                  function(x){mean(x$FungalDryWeight)
                  });
            Biomass<-data.frame(Biomass)
            Biomass$fung_temp<-row.names(Biomass)
            Biomass<-Biomass[Biomass$Biomass!="NaN",]
            Biomass$fung_temp<-sub("\\.","_",Biomass$fung_temp)

#allDataColor$fung_temp<-paste(allDataColor$fungus,allDataColor$temp,sep="_")

#unique(allDataColor$fung_temp)==unique(Biomass$fung_temp)

#Now we can combine both datasets
allData<-left_join(Biomass,allDataColor)

View(allData) #check if everything looks fitting



###Step 3: join extension data
#Comibining extension rate and area of the colony at the time of harvest with data on color
#and biomass. Extension rate was measured as the the slope of the regression of radial growth
#in time (using info from the first week)

#loading the file
Extension<-read.csv("FungalExtension.csv",header = TRUE, stringsAsFactors = FALSE)

# #To visualize extension rate of each fungus at each temperature
#  lapply(split(Extension,Extension$FungusID),function(x){
#      #summary.lm(lm(x$Radius~x$Time))
#         plot(x$Radius~x$Time,main=x$FungusID[1])
#       #(x$FungusID[1])
#      })

#Calculating extension rate as the slope of the regression of radial growth in time (using info from the first week)
ExtensionRate<-
              sapply(split(Extension[Extension$Time<200,],Extension[Extension$Time<200,]$FungusID),
                     function(x){
                       regre<-summary.lm(lm(x$Radius~x$Time))
                       regre$coefficients[2]
                       #plot(x$Radius~x$Time,main=x$FungusID[1])
                       #(x$FungusID[1])
                     });
              ExtensionRate<-data.frame(ExtensionRate);
              ExtensionRate$fungID<-row.names(ExtensionRate)
              row.names(ExtensionRate)<-NULL
              ExtensionRate$fungus<-sapply(strsplit(ExtensionRate$fungID,"_"),function(x){x[1]})
              ExtensionRate$temp<-sapply(strsplit(ExtensionRate$fungID,"_"),function(x){x[2]})
              ExtensionRate$fung_temp<-paste(ExtensionRate$fungus,"_",ExtensionRate$temp,sep = "")

#calculating the mean from the two replicates we have
meanExtension<-
                sapply(split(ExtensionRate,ExtensionRate$fung_temp),
                       function(x){
                         mean(x$ExtensionRate)
                       })
              
              meanExtension<-data.frame(meanExtension)
              meanExtension$fung_temp<-row.names(meanExtension)
              row.names(meanExtension)<-NULL

#calculating the area of the colony at each time point
Extension$Area_mm2<-pi*(Extension$Radius)^2
                    Extension$fung_temp<-paste(Extension$Species,"_",Extension$Temperature,sep = "")
#calculating the mean area of the two samples at the time of harvest (312 hours)
meanArea<-
            sapply(split(Extension[Extension$Time=="312",],Extension[Extension$Time=="312",]$fung_temp),
                   function(x){
                     mean(x$Area_mm2)
                   })
          meanArea<-data.frame(meanArea)
          meanArea$fung_temp<-row.names(meanArea)
          row.names(meanArea)<-NULL


#merging extension rate and area a the time of harvest to all the other data
allData<-left_join(allData,meanExtension)
allData<-left_join(allData,meanArea)
          allData$fungus<-sapply(strsplit(allData$fung_temp,"_"),function(x){x[1]})
          allData$temp<-sapply(strsplit(allData$fung_temp,"_"),function(x){x[2]})


#joining the taxonomy information
taxonomy<-read.csv("TaxonomicData.csv",header = TRUE, stringsAsFactors = FALSE)
          taxonomy$fungus[10]<-"DF04"
          taxonomy$fungus[11]<-"DF09"
          
          
          
          allData<-left_join(allData,taxonomy)           
          
          allData$Phylum<-sub("^\\s","",allData$Phylum)
          allData$Phylum
          allData$temp<-as.numeric(allData$temp)


rm(list=ls()[2:16])

# create linear and quadratic terms for temperature, or polynomial regression
allData$temp.l <- poly(allData$temp, 2)[, 1]
allData$temp.q <- poly(allData$temp, 2)[, 2]

head(allData)
