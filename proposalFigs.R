#########FINAL PROPOSAL FIGURE SCRIPTS

# FIGURE1
# Industry size and employment information.
rm(list=ls())
library(ggplot2)

#TRI Data
data <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/allSecSummary.csv", stringsAsFactors=FALSE)
data$Industry <- factor(data$Industry, levels=c("Paper & Pulp", "Printed CBs", "PVC"), 
                               labels=c("Pulp & Paper", "Printed CBs", "Plastic Pipes"))

a <- qplot(years, NumFac, shape=Industry, scale_shape(solid = FALSE), color=Industry, size=I(8), data=data,
           main="Count of Facilities Reporting TRI Releases \
By Industry Over Time",
           ylab="Number of Facilities",
           xlab="Years")
aa <-  a + geom_point(aes(colour = factor(Industry)), size = 4) +  
  geom_point(colour="grey90", size = 2) +
  theme(legend.text=element_text(size=27), 
        legend.title=element_text(size=27,face="bold"), 
        axis.text=element_text(size=27, face="bold"),
        axis.title=element_text(size=27,face="bold"),
        plot.title=element_text(size=28,face="bold")) #+ theme_bw()
#Census Data
data1 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/paperPulp/ppEmploy.csv", stringsAsFactors=FALSE)
data2 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/printedCB/printedCBEmploy.csv", stringsAsFactors=FALSE)
data3 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/pvcPipe/pvcEmploy.csv", stringsAsFactors=FALSE)

data1$Employees <- factor(data1$Employees, levels=c("10 to 49", "50 to 99", "100 to 499", "500+", "Total"), 
                               labels=c("10 to 49", "50 to 99", "100 to 499", "500+", "Total"))
data2$Employees <- factor(data2$Employees, levels=c("10 to 49", "50 to 249", "250+", "Total"), 
                               labels=c("10 to 49", "50 to 249", "250+", "Total"))
data3$Employees <- factor(data3$Employees, levels=c("10 to 49", "50 to 99", "100+", "Total"), 
                               labels=c("10 to 49", "50 to 99", "100+", "Total"))

e1 <- qplot(Years, NumFac, shape=Employees, scale_shape(solid = FALSE), color=Employees, size=I(8), data=data1,
            main="Count of Facilities By Employment Category\
            Industry: Pulp and Paper Milling",
            ylab="Number of Facilities",
            xlab="Years")
e2 <- qplot(Years, NumFac, shape=Employees, scale_shape(solid = FALSE), color=Employees, size=I(8), data=data2,
            main="Count of Facilities By Employment Category\
            Industry: Printed Circuit Boards",
            ylab="Number of Facilities",
            xlab="Years")
e3 <- qplot(Years, NumFac, shape=Employees, scale_shape(solid = FALSE), color=Employees, size=I(8), data=data3,
            main="Count of Facilities By Employment Category\
            Industry: Plastic Pipe",
            ylab="Number of Facilities",
            xlab="Years")

ee1 <- e1 + geom_point(aes(colour = factor(Employees)), size = 4) +  
  geom_point(colour="grey90", size = 2) +
  theme(legend.text=element_text(size=27), 
        legend.title=element_text(size=27,face="bold"), 
        axis.text=element_text(size=27, face="bold"),
        axis.title=element_text(size=27,face="bold"),
        plot.title=element_text(size=28,face="bold")) #+ theme_bw()
ee2 <- e2 + geom_point(aes(colour = factor(Employees)), size = 4) +  
  geom_point(colour="grey90", size = 2) +
  theme(legend.text=element_text(size=27), 
      legend.title=element_text(size=27,face="bold"), 
      axis.text=element_text(size=27, face="bold"),
      axis.title=element_text(size=27,face="bold"),
      plot.title=element_text(size=28,face="bold")) #+ theme_bw()
ee3 <- e3 + geom_point(aes(colour = factor(Employees)), size = 4) +  
  geom_point(colour="grey90", size = 2) +
  theme(legend.text=element_text(size=27), 
        legend.title=element_text(size=27,face="bold"), 
        axis.text=element_text(size=27, face="bold"),
        axis.title=element_text(size=27,face="bold"),
        plot.title=element_text(size=28,face="bold")) #+ theme_bw()

require(gridExtra)
grid.arrange(aa, ee1, ee2, ee3, ncol=2, nrow=2)
# End Figure 1

# FIGURE2
# Industry size and employment information.
rm(list=ls())
library(ggplot2)
#TRI Data
data <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/allSecSummary.csv", stringsAsFactors=FALSE)
data$Industry <- factor(data$Industry, levels=c("Paper & Pulp", "Printed CBs", "PVC"), 
                        labels=c("Pulp & Paper", "Printed CBs", "Plastic Pipes"))

b <- qplot(years, gini, shape=Industry, scale_shape(solid = FALSE), color=Industry, size=I(8), data=data,
           main="Disproportionality By Industry Over Time",
           ylab="Disproportionality Metric: Gini Coefficient",
           xlab="Years",
           ylim=c(.5,1))
b + geom_point(aes(colour = factor(Industry)), size = 4) +  
  geom_point(colour="grey90", size = 2) +
  theme(legend.text=element_text(size=28), 
        legend.title=element_text(size=28,face="bold"), 
        axis.text=element_text(size=28, face="bold"),
        axis.title=element_text(size=28,face="bold"),
        plot.title=element_text(size=28,face="bold"))   #+ theme_bw()

c <- qplot(years, Prop90, shape=Industry, scale_shape(solid = FALSE), color=Industry, size=I(8), data=data,
           main="Proportion of Facilities Producing 90 Percent of All Yearly Industrial Release Pounds Over Time",
           ylab="Proportion of Facilities",
           xlab="Years",
           ylim=c(0,.5))
c + geom_point(aes(colour = factor(Industry)), size = 4) +  
  geom_point(colour="grey90", size = 2) +
  theme(legend.text=element_text(size=28), 
        legend.title=element_text(size=28,face="bold"), 
        axis.text=element_text(size=28, face="bold"),
        axis.title=element_text(size=28,face="bold"),
        plot.title=element_text(size=28,face="bold"))  #+ theme_bw()



####################################
#########TEST SCRIPTS
####################################
rm(list=ls())
library(ggplot2)
data <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/allSecSummary.csv", stringsAsFactors=FALSE)

a <- qplot(years, NumFac, shape=sector, scale_shape(solid = FALSE), color=sector, size=I(6), data=data,
           main="(A) Count of Facilities Reporting Releases By Industry Over Time",
           ylab="Number of Facilities",
           xlab="Years")

b <- qplot(years, gini, shape=sector, scale_shape(solid = FALSE), color=sector, size=I(6), data=data,
           main="(B) Disproportionality By Industry Over Time",
           ylab="Disproportionality Metric: Gini Coefficient",
           xlab="Years",
           ylim=c(.5,1))

c <- qplot(years, Num90, shape=sector, scale_shape(solid = FALSE), color=sector, size=I(6), data=data,
           main="(C) Number of Facilities Producing 90 Percent of All Yearly Industrial Release Pounds Over Time",
           ylab="Number of Facilities",
           xlab="Years")

d <- qplot(years, Prop90, shape=sector, scale_shape(solid = FALSE), color=sector, size=I(6), data=data,
           main="(D) Proportion of Facilities Producing 90 Percent of All Yearly Industrial Release Pounds Over Time",
           ylab="Proportion of Facilities",
           xlab="Years",
           ylim=c(0,.5))

aa <- a + geom_point(aes(colour = factor(sector)), size = 4) +  geom_point(colour="grey90", size = 1.5)
bb <- b + geom_point(aes(colour = factor(sector)), size = 4) +  geom_point(colour="grey90", size = 1.5)
cc <- c + geom_point(aes(colour = factor(sector)), size = 4) +  geom_point(colour="grey90", size = 1.5)
dd <- d + geom_point(aes(colour = factor(sector)), size = 4) +  geom_point(colour="grey90", size = 1.5)

require(gridExtra)
grid.arrange(aa, bb, cc, dd, ncol=2, nrow=2)

rm(list=ls())
data1 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/paperPulp/ppEmployNT.csv", stringsAsFactors=FALSE)
data2 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/printedCB/printedCBEmployNT.csv", stringsAsFactors=FALSE)
data3 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/pvcPipe/pvcEmployNT.csv", stringsAsFactors=FALSE)

data1$NumEmployeeCat <- factor(data1$NumEmployeeCat, levels=c("10 to 49", "50 to 99", "100 to 499", "500+"), 
                               labels=c("10 to 49", "50 to 99", "100 to 499", "500+"))
data2$NumEmployeeCat <- factor(data2$NumEmployeeCat, levels=c("10 to 49", "50 to 249", "250+"), 
                               labels=c("10 to 49", "50 to 249", "250+"))
data3$NumEmployeeCat <- factor(data3$NumEmployeeCat, levels=c("10 to 49", "50 to 99", "100+"), 
                               labels=c("10 to 49", "50 to 99", "100+"))

e1 <- qplot(Years, NumFac, shape=NumEmployeeCat, scale_shape(solid = FALSE), color=NumEmployeeCat, size=I(3), data=data1,
            main="(E) Count of Facilities By Employment Category Over Time \
           Pulp and Paper Milling \
           (Source: US Census County Business Data)",
            ylab="Number of Facilities",
            xlab="Years")

e2 <- qplot(Years, NumFac, shape=NumEmployeeCat, scale_shape(solid = FALSE), color=NumEmployeeCat, size=I(3), data=data2,
            main="(E) Count of Facilities By Employment Category Over Time \
           Printed Circuit Boards \
            (Source: US Census County Business Data)",
            ylab="Number of Facilities",
            xlab="Years")

e3 <- qplot(Years, NumFac, shape=NumEmployeeCat, scale_shape(solid = FALSE), color=NumEmployeeCat, size=I(3), data=data3,
            main="(E) Count of Facilities By Employment Category Over Time \
           PVC Pipe \
            (Source: US Census County Business Data)",
            ylab="Number of Facilities",
            xlab="Years")
ee1 <- e1 + geom_point(aes(colour = factor(NumEmployeeCat)), size = 4) +  geom_point(colour="grey90", size = 1.5)
ee2 <- e2 + geom_point(aes(colour = factor(NumEmployeeCat)), size = 4) +  geom_point(colour="grey90", size = 1.5)
ee3 <- e3 + geom_point(aes(colour = factor(NumEmployeeCat)), size = 4) +  geom_point(colour="grey90", size = 1.5)

grid.arrange(ee1, ee2, ee3, ncol=3)


rm(list=ls())
data1 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/paperPulp/ppEmploy.csv", stringsAsFactors=FALSE)
data2 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/printedCB/printedCBEmploy.csv", stringsAsFactors=FALSE)
data3 <- read.csv("~/Dropbox/Egregious Polluters/Collins caseStudies/pvcPipe/pvcEmploy.csv", stringsAsFactors=FALSE)

e1 <- qplot(Years, NumFac, shape=NumEmployeeCat, scale_shape(solid = FALSE), color=NumEmployeeCat, size=I(3), data=data1,
           main="(E) Count of Facilities By Employment Category Over Time \
           Pulp and Paper Milling \
           (Source: US Census County Business Data)",
           ylab="Number of Facilities",
           xlab="Years")

e2 <- qplot(Years, NumFac, shape=NumEmployeeCat, scale_shape(solid = FALSE), color=NumEmployeeCat, size=I(3), data=data2,
            main="(E) Count of Facilities By Employment Category Over Time \
           Printed Circuit Boards \
            (Source: US Census County Business Data)",
            ylab="Number of Facilities",
            xlab="Years")

e3 <- qplot(Years, NumFac, shape=NumEmployeeCat, scale_shape(solid = FALSE), color=NumEmployeeCat, size=I(3), data=data3,
            main="(E) Count of Facilities By Employment Category Over Time \
           PVC Pipe \
            (Source: US Census County Business Data)",
            ylab="Number of Facilities",
            xlab="Years")
ee1 <- e1 + geom_point(aes(colour = factor(NumEmployeeCat)), size = 4) +  geom_point(colour="grey90", size = 1.5)
ee2 <- e2 + geom_point(aes(colour = factor(NumEmployeeCat)), size = 4) +  geom_point(colour="grey90", size = 1.5)
ee3 <- e3 + geom_point(aes(colour = factor(NumEmployeeCat)), size = 4) +  geom_point(colour="grey90", size = 1.5)

grid.arrange(ee1, ee2, ee3, ncol=3)
