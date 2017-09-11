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
library(plotrix)
library(MuMIn)


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
      


#Alternatively, plot separate relationship for each fungus using polynomial regression
quartz(height=9, width=4)

layout(matrix(1:3, nrow=3))
allData$Genus.species <- factor(allData$Genus.species)

par(mar=c(1, 4.5, 3, 1))
# plot relationship between colour and temp by isolate
lmlis1 <- lmList(K.mean ~ poly(temp, 2) | Genus.species, data=allData)
liscoef_K.mean <- coef(lmlis1)
allData1 <- split(allData, allData$Genus.species)
with(allData, plot(K.mean ~ temp, col=Genus.species, type='n', ylab='Culture blackness (\'K\')', 
                   ylim=c(0, 0.65), xlab='', cex.lab=1.5))
# legend('topright', 'P < 0.001', cex=0.8, bty='n')
for(i in 1:length(allData1)){
  xmin <- min(allData1[[i]]$temp)
  xmax <- max(allData1[[i]]$temp)
  lines(xmin:xmax, predict(lmlis1[[i]], data.frame(temp=xmin:xmax)), lwd=0.5)
}

par(mar=c(2, 4.5, 2, 1))
# plot relationship between biomass and temp by isolate
lmlis1 <- lmList(Biomass ~ poly(temp, 2) | Genus.species, data=allData)
liscoef_Biomass <- coef(lmlis1)
allData1 <- split(allData, allData$Genus.species)
with(allData, plot(Biomass ~ temp, col=Genus.species, type='n', ylab='Biomass (mg)', 
                   ylim=c(0, 250), xlab='', cex.lab=1.5))
# legend('topright', 'P < 0.001', cex=0.8, bty='n')
for(i in 1:length(allData1)){
  xmin <- min(allData1[[i]]$temp)
  xmax <- max(allData1[[i]]$temp)
  lines(xmin:xmax, predict(lmlis1[[i]], data.frame(temp=xmin:xmax)), lwd=0.5)
}

par(mar=c(4, 4.5, 0, 1))
# plot relationship between radius and temp by isolate
lmlis1 <- lmList(meanExtension ~ poly(temp, 2) | Genus.species, data=allData)
liscoef_meanExtension <- coef(lmlis1)
allData1 <- split(allData, allData$Genus.species)
with(allData, plot(meanExtension ~ temp, col=Genus.species, type='n', ylab='Colony extension rate (mm/d)', 
 ylim=c(0, 0.25), xlab='Temperature (degrees C)', cex.lab=1.5))
# legend('topright', 'P < 0.001', cex=0.8, bty='n')
for(i in 1:length(allData1)){
  xmin <- min(allData1[[i]]$temp)
  xmax <- max(allData1[[i]]$temp)
  lines(xmin:xmax, predict(lmlis1[[i]], data.frame(temp=xmin:xmax)), lwd=0.5)
}

dev.copy2pdf(file='output/predictedResponsesBySpecies.pdf')


# write output
# write.csv(allData, 'output/carlosSoil_colour.csv')



###Step 7: statistics

#8.1: Statistics color
ColorTemp<-
  lmer(log10(K.mean)~temp+ #we decided to do statistics in a log10 function because data fits better
         (1|fungus),data = allData) #but only for statistics, not to show the data in figures
ColorTemp2<-
  lmer(log10(K.mean)~temp+ 
         (temp|fungus),data = allData) 
ColorTemp.poly<-
  lmer(log10(K.mean)~temp.l+temp.q+ 
         (1|fungus),data = allData) 
ColorTemp.poly2<-
  lmer(log10(K.mean)~temp.l+temp.q+ 
         (temp.l+temp.q|fungus),data = allData) 

# compare two models
qqnorm(resid(ColorTemp)); qqline(resid(ColorTemp))  # residuals look close to normally distributed
qqnorm(resid(ColorTemp2)); qqline(resid(ColorTemp2))  # residuals look close to normally distributed
qqnorm(resid(ColorTemp.poly)); qqline(resid(ColorTemp.poly))  # residuals look poor, but...
qqnorm(resid(ColorTemp.poly2)); qqline(resid(ColorTemp.poly2))  # residuals look poor, but...
AICc(ColorTemp, ColorTemp2, ColorTemp.poly, ColorTemp.poly2, REML=F)  # quadratic provides better fit, esp. with random slopes

Anova(ColorTemp.poly2)
summary(ColorTemp.poly2)
shapiro.test(residuals(ColorTemp.poly2))
plot(ColorTemp.poly2)


#8.2: statistics biomass
BiomTemp<-
  lmer(Biomass~temp+ 
         (1|fungus),data = allData)
BiomTemp.poly<-
  lmer(Biomass~temp.l+temp.q+ 
         (1|fungus),data = allData)
BiomTemp.poly2<-
  lmer(Biomass~temp.l+temp.q+ 
         (temp.l|fungus),data = allData)
qqnorm(resid(BiomTemp)); qqline(resid(BiomTemp))  # residuals look poor
qqnorm(resid(BiomTemp.poly)); qqline(resid(BiomTemp.poly))  # residuals look poor
qqnorm(resid(BiomTemp.poly2)); qqline(resid(BiomTemp.poly2))  # residuals look better
AICc(BiomTemp, BiomTemp.poly, BiomTemp.poly2, REML=F)  # quadratic provides better fit, esp. with random slopes

Anova(BiomTemp.poly2)
summary(BiomTemp.poly2)
shapiro.test(residuals(BiomTemp.poly2))
plot(BiomTemp.poly2)

#8.3: statistics: Extension
ExtensionTemp<-
  lmer(meanExtension~temp+ 
         (1|fungus),data = allData) 
ExtensionTemp.poly<-
  lmer(meanExtension~temp.l+temp.q+ 
         (1|fungus),data = allData) 
ExtensionTemp.poly2<-
  lmer(meanExtension~temp.l+temp.q+ 
         (temp.l+temp.q|fungus),data = allData) 
qqnorm(resid(ExtensionTemp)); qqline(resid(ExtensionTemp))  # residuals look poor
qqnorm(resid(ExtensionTemp.poly)); qqline(resid(ExtensionTemp.poly))  # residuals look poor
qqnorm(resid(ExtensionTemp.poly2)); qqline(resid(ExtensionTemp.poly2))  # residuals look better...
AICc(ExtensionTemp, ExtensionTemp.poly, ExtensionTemp.poly2, REML=F)  # quadratic provides better fit, esp. with random slopes

Anova(ExtensionTemp.poly)
summary(ExtensionTemp.poly)
shapiro.test(residuals(ExtensionTemp.poly))


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


      