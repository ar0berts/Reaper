library(numDeriv) # Needed for interpolation
library(ggplot2) # Needed for plotting
library(stringr)
library(gplots) # Needed for Heatmap.2
source("http://bioconductor.org/biocLite.R")
biocLite("Heatplus")
# Create some dummy data
# Initial walking speeds
sp <-c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1.1,1.2,1.3,1.4,1.5) 
# Hazard ratios
hr <-c(2.5,2.2,2,1.87,1.75,1.57,1.4,1.3,1.2,1.15,1.138,1.12,1.1,1.06,1.02,1)
# m/s change after treatment
incr <-c(1,.9,.8,.7,.6,.5,.4,.3,.2,.1,0,-.1,-.2,-.3,-.4,-.5) 
sphr<-as.data.frame(cbind(sp,hr))
sphr$rod <-sphr$hr * 3 # convert this to risk of 
                       # death in next 12 months assume HR=1 is 3%
plot(data=sphr,rod~sp,main="Dummy Risk Data",
     xlab="Walking Speed (M/s)",ylab="Risk of Death (%/Yr)")

# If the speeds in the data set are not expressed 
# tidily there will need to be some interpolation
# to give the ability to identify hazard ratio 
# extrapolation at desired speeds to enable matrix 
# calculation. Here the dummy data has speeds 
# in .1 m/s intervals so no need for extrapolation.

# At each speed let's work out the reductions in 
# risk of death supposedly delivered by each in(de)crease in speed

empty <-matrix(numeric(0), 16,16) # Make a blank 16*16 matrix
attach(sphr)
for(s in 1:nrow(sphr)){# A loop that handles each speed
  sp2 <-sp[s]+incr     # Calculate the post op speed by column
                       # The next loop removes negative speeds 
                       #(we don't want stroke victims
                       # walking backwards for health & safety reasons)
  for(y in 1:length(sp2)){if(sp2[y]<0) sp2[y]<-0}
            # The next loop removes post op speeds above 
            # the upper speed in the dataset
  for(y in 1:length(sp2)){if(sp2[y]>sphr$sp[nrow(sphr)]) 
               sp2[y]<-sphr$sp[nrow(sphr)]}
           # The next loop finds the equivalent risk for each level of sp2
   for(x in length(sp2):1){
        for(z in 1:nrow(sphr)){ sphr$diff[z]<-abs(sphr$sp[z]-sp2[x])}
        speedpoint <-which.min(sphr$diff)
        empty[x,s] <-sphr$rod[s] - sphr$rod[speedpoint]
                                                    
                                       }
                        }
detach(sphr)

emptyF <-format(empty,width=2)
labR<-incr
labC<-sphr$sp
heatmap.2(empty, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
          cellnote=emptyF, notecol="black", trace='none',
          notecex=.5, scale="none", density.info="none",
          main="Risk of Death Reduction (%)",
          xlab="Initial Speed (M/s)",ylab="Speed Change Post Op (M/s)",
          margins = c(3.5,3.8),
          labRow=labR, labCol=labC)