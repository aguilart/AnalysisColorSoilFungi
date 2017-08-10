######################################################################################
# The following is the data visualization and data analysis used for Sarah bachelor###
# thesis. It correspond to regression of three variables (K.mean, biomass in mg, and #
# extension rate in mm/hour) on a temperature gradient from 12C to 33C. The data     #
# correspond to measurements on 33 soil fungi. Additionally, taxonomic affiliation   #
# is added. Details on the experiment can be found in the bachelor thesis of Marcel  #
# Krüger. Color was extracted out of pictures using the scipt of Jeff. Sarah did the #
# work of extacting the color out of the pictures. One needs to run first the script #
# DataPrepartion.R to get the data in a single dataframe called allData              #
######################################################################################

#load all packages
library(tidyverse)
library(vegan)
library(lattice) #plotting and splitting by an argument
library(lme4) #statistics
library(car) #statistics
library(lattice)
library(plyr)

###Plot color, biomass and extension rate containing all fungi to see general patterns

#Plotting color data (all fungi in one plot)
ggplot(data=allData, #This is a new way with the function ggplot
       aes(x=temp,y=K.mean,
           group=fungus,colour=fungus))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  labs(x="Temperature [C°]",y="Black channel colorization")+
  ggtitle("Colorization", subtitle ="of 33 different fungal species") + 
  theme(plot.title = element_text(family="serif",size=18, face="bold.italic"),
        axis.title = element_text(size=16))          




#Plotting biomass data (all fungi in one plot)
ggplot(data=allData, #This is a new way with the function ggplot
       aes(x=temp,y=Biomass,
           group=fungus,colour=fungus))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  labs(x="Temperature [C°]",y="Biomass production [mg]")+
  ggtitle("Biomass production", subtitle ="of 33 different fungal species") + 
  theme(plot.title = element_text(family="serif", size=18, face="bold.italic"),
        axis.title = element_text(size=16))          


#Plotting extension rate (all fungi in one plot)
ggplot(data=allData, #This is a new way with the function ggplot
       aes(x=temp,y=meanExtension,
           group=fungus,colour=fungus))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  labs(x="Temperature [C°]",y="Extension [mm/h]")+
  ggtitle("Extension rate", subtitle ="of 33 different fungal species") + 
  theme(plot.title = element_text(family="serif",size=18, face="bold.italic"),
        axis.title = element_text(size=16))          



##Show the data in a more organized way, plotting divided by phylum

#Color (summarized in phyla)
ggplot(data=allData,
       aes(x=temp,y=K.mean,
           group=Phylum,colour=Phylum))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  labs(x="Temperature [C°]",y="Black channel colorization")+
  ggtitle("Colorization", subtitle= "Ascomycota, Basidiomycota and Mucoromycotica") + 
  theme(plot.title = element_text(family="serif",size=18, face="bold.italic"),
        axis.title = element_text(size=16))   


#Biomass (summarized in phyla)
ggplot(data=allData,
       aes(x=temp,y=Biomass,
           group=Phylum,colour=Phylum))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  labs(x="Temperature [C°]°",y="Biomass [mg]")+
  ggtitle("Biomass production", subtitle= "Ascomycota, Basidiomycota and Mucoromycotica") + 
  theme(plot.title = element_text(family="serif",size=18, face="bold.italic"),
        axis.title = element_text(size=16))  


#Extension rate (summarized in phyla)
ggplot(data=allData,
       aes(x=temp,y=meanExtension,
           group=Phylum,colour=Phylum))+
  geom_point()+
  geom_smooth(method=lm,se=FALSE)+
  labs(x="Temperature [C°]°",y="Extension rate [mm/h]")+
  ggtitle("Extension rate", subtitle= "Ascomycota, Basidiomycota and Mucoromycotica") + 
  theme(plot.title = element_text(family="serif",size=18, face="bold.italic"),
        axis.title = element_text(size=16))  



#Plotting color, biomass and extension in response to temperature
#for one exemplary fungus: Truncatella angustata

#Color  
        allData[allData$Genus.species=="Truncatella angustata ",]%>%
          ggplot()+
          aes(x=temp,y=K.mean,group=fungus)+
          geom_point()+
          geom_smooth(method=lm,se=FALSE)+
          labs(x="Temperature [in °C]",y="Black channel coloration")+
          ggtitle(label="Truncatella angustata",subtitle="Workingtitle: C23, belonging to Ascomycota")+ #Add the actual name of the fungus, each time
                  theme(plot.title = element_text(family="serif", size=20, face="bold.italic"),
                  axis.title = element_text(size=18))

#biomass data
          allData[allData$Genus.species=="Truncatella angustata ",]%>%
            ggplot()+
            aes(x=temp,y=Biomass,group=fungus)+
            geom_point()+
            geom_smooth(method=loess,se=FALSE)+
            labs(x="Temperature [in °C]",y="Biomass [in mg]")+
            ggtitle(label="Truncatella angustata",subtitle="Workingtitle: C23, belonging to Ascomycota")+ #Add the actual name of the fungus, each time
                    theme(plot.title = element_text(family="serif", size=18, face="bold.italic"),
                    axis.title = element_text(size=16))

#Extension rate data
        allData[allData$Genus.species=="Truncatella angustata ",]%>%
          ggplot()+
          aes(x=temp,y=meanExtension,group=fungus)+
          geom_point()+
          geom_smooth(method=loess,se=FALSE)+
          labs(x="Temperature [in °C]",y="Extension rate [in mm/h]")+
          ggtitle(label="Truncatella angustata",subtitle="Workingtitle: C23, belonging to Ascomycota")+ #Add the actual name of the fungus, each time
          theme(plot.title = element_text(family="serif", size=18, face="bold.italic"),
                axis.title = element_text(size=16))
      




###Step 7: statistics

#8.1: Statistics color
ColorTemp<-
  lmer(log10(K.mean)~temp+ #we decided to do statistics in a log10 function because data fits better
         (1|fungus),data = allData) #but only for statistics, not to show the data in figures

Anova(ColorTemp)
summary(ColorTemp)
shapiro.test(residuals(ColorTemp))

#8.2: statistics biomass
BiomTemp<-
  lmer(Biomass~temp+ 
         (1|fungus),data = allData)

Anova(BiomTemp)
summary(BiomTemp)
shapiro.test(residuals(BiomTemp))
qplot(residuals(BiomTemp))

#8.3: statistics: Extension
ExtensionTemp<-
  lmer(meanExtension~temp+ 
         (1|fungus),data = allData) 

Anova(ExtensionTemp)
summary(ExtensionTemp)
shapiro.test(residuals(ExtensionTemp))


#9.0 Simpler analysis for each fungus
#The result of this analysis tells us that there is no general response of fungi to temperature, each
#species respond differently in a very strong way. Given this result, we decided to do a simpler analysis
#where we wanted to determine how each species individually responded to temperature. This would be 
#a normal regression using lm.

#9.1 Individual analysis for fungus Truncatella angustata

simplerAnalysisT<-lm(Biomass~temp,data=allData2[allData2$Genus.species=="Truncatella angustata ",])
summary.lm(simplerAnalysisT)


#9.3 extension analysis for each fungus
simplerAnalysisT<-lm(meanExtension~temp,data=allData2[allData2$Genus.species=="Truncatella angustata ",])
summary.lm(simplerAnalysisT)


      