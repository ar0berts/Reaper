####### Load libraries and functions                          ##############################
library(randomForestSRC)
library(stringr)
library(gplots)
library(XML)
library(vcd,vcdExtra)
library(reporttools)
library(Hmisc)
library(pROC)
setwd(dir="~/Desktop/Liemannen")
####### Clean up the main dataset                             #####################  
Swedish_stroke <-read.csv(file="Swedish_stroke_2014.csv",header=TRUE,sep=",")
Swedish_stroke$DOB <-as.Date(Swedish_stroke$DOB,format="%d/%m/%Y")
Swedish_stroke$strokedate <-as.Date(Swedish_stroke$strokedate,format="%d/%m/%Y")
Swedish_stroke$examdate <-as.Date(Swedish_stroke$examdate,format="%d/%m/%Y")
Swedish_stroke$DOD <-as.Date(Swedish_stroke$DOD,format="%d/%m/%Y")
Swedish_stroke$Age_at_Stroke <- Swedish_stroke$strokedate - Swedish_stroke$DOB
Swedish_stroke$Years_at_Stroke <- (Swedish_stroke$Age_at_Stroke)/365
Swedish_stroke$Age_at_exam <- Swedish_stroke$examdate - Swedish_stroke$DOB
#Swedish_stroke$Sex <-factor(Swedish_stroke$Sex,levels=c(2,1), labels=c("Male","Female")) # Gender 1=female 2=male
Swedish_stroke$Survival <-factor(Swedish_stroke$Status,levels=c(0,1),labels=c("Alive","Dead"))
Swedish_stroke$Time_after_stroke <-as.numeric((Swedish_stroke$Age_at_exam - Swedish_stroke$Age_at_Stroke)/28)
Swedish_stroke$remaining_life <-as.numeric((Swedish_stroke$DOD - Swedish_stroke$examdate))
Swedish_stroke$Age <-as.numeric(Swedish_stroke$Age_at_exam/365)
####### ROC evaluation of the  speed threshold as a predictor #########
pdf(file="./ROC.pdf",width=8,height=8)
Ss.roc <-roc(Swedish_stroke$Status, Swedish_stroke$Age,levels=c(0,1))
plot(Ss.roc, print.thres="best", print.thres.best.method="youden", main="Age Threshold for Survival")

Ss.roc <-roc(Swedish_stroke$Status, Swedish_stroke$speed_shoes,levels=c(0,1))
plot(Ss.roc, print.thres="best", print.thres.best.method="youden", main="Speed in shoes: Threshold for Survival")

Ss.roc <-roc(Swedish_stroke$Status, Swedish_stroke$speed_barefeet,levels=c(0,1))
plot(Ss.roc, print.thres="best", print.thres.best.method="youden", main="Speed barefoot: Threshold for Survival")
dev.off()
####### Describe the data                                     ########################
sink(file="Swedish_stroke_Variables.txt",append=FALSE)
describe.data.frame(Swedish_stroke)
table(Swedish_stroke$status)
sink()
library(etable)
Tab <- tabular.ade(x_vars='Years_at_Stroke', rows='Sex', rnames='Sex',  cols='Survival', cnames='Status at 10 Years',data=Swedish_stroke, FUN=iqr_cell)
print(xtable(Tab, caption='Median (Q1/Q3) of Age'), include.rownames=F, include.colnames=F, caption.placement="top")
rm(Tab)
  Tab<-tabular.ade(x_vars='speed_shoes', rows='Sex', rnames='Sex', cols='Survival', cnames='Status at 10 Years', data=Swedish_stroke, FUN=iqr_cell)
print(xtable(Tab, caption='Median (Q1/Q3) of Speed in Shoes'), include.rownames=F, include.colnames=F, caption.placement="top")
rm(Tab)
####### Look at the variables of interest                     ########################
Ss.obj <- rfsrc(Surv(remaining_life, Status) ~ ., Swedish_stroke, nsplit = 30, ntree = 1000)
library(XML) # required to export Tree as a .zip file. 
rf2rfz(Ss.obj, forestName = "Swedish_stroke")
pdf(file="./Variables.pdf",height=8,width=11,onefile=TRUE)
plot.variable(Ss.obj, partial = TRUE, smooth.lines = TRUE,
              xvar.names="Sex",main="Dr Riad's Stroke Data",)
plot.variable(Ss.obj, partial = TRUE, smooth.lines = TRUE,
              xvar.names="Age_at_exam",main="Dr Riad's Stroke Data",)
dev.off()
Ss.max <- max.subtree(Ss.obj)
print(round(Ss.max$order, 3))
####### Here is a cut down version of the data                #####################
Swedish_stroke2 <-Swedish_stroke[,c(-1,-2,-4,-5,-7,-8,-9,-10,-11,-12,-13,-16,-18)] 
names(Swedish_stroke2)[5] <-"time"
names(Swedish_stroke2)[4] <-"status"
attach(Swedish_stroke2)
Swedish_stroke2 <-Swedish_stroke2[order(speed_shoes),]
detach(Swedish_stroke2)
Ss3 <- subset(Swedish_stroke2[c(1:35,102:136),])
Ss3$speedgroup[1:35] <-"Slowest"
Ss3$speedgroup[36:70] <-"Fastest"
Ss2.obj <- rfsrc(Surv(time, status) ~ ., Swedish_stroke2, nsplit = 30, ntree = 1000,na.action="na.impute")
Ss2.obj <- rfsrc(status ~ ., data = Swedish_stroke2, nsplit = 30, ntree=1000,na.action="na.impute")
plot.variable(Ss2.obj, partial = TRUE, smooth.lines = TRUE,main="Stroke Data")
#plot.survival(rfsrc(Surv(time, status)~ ., Swedish_stroke2), cens.model = "km",collapse=TRUE)
####### Speed effect                                          ########################################
library(survival)
stroke.coxph <-coxph(formula=Surv(as.numeric(time),status)~speedgroup, data=Ss3)
summary(stroke.coxph)
pdf(file="cox-plot-speed-quartiles.pdf", height=10,width=10)
plot(survfit(Surv(as.numeric(time/365),status)~speedgroup,data=Ss3),
     col=c("blue","red"),conf.int=TRUE,main="Survival Slowest vs Fastest",
     ylab="Proportion Surviving",xlab="Years Post Examination")
legend("bottomleft", legend = c("Fastest quartile", "Slowest quartile"), lty = c(1,2), col = c("blue","red"), lwd = 1, cex = .8)
text(x=3, y=.2, labels = "Wald test = 18.8  on 1 df, p=1.45e-05",cex = 1, col = "black")
dev.off()
####### Sex Effect                                            ########################################
pdf(file="./Survival_by_Sex.pdf",width=11,height=8)
plot(survfit(Surv(as.numeric(time/365),status)~Sex,data=Swedish_stroke2),
     col=c("pink","blue"),conf.int=FALSE,main="Survival by Sex",
     ylab="Proportion Surviving",xlab="Years Post Examination")
legend("bottomleft", legend = c("Male", "Female"), lty = c(1,2), col = c("blue","pink"), lwd = 1, cex = .8)
dev.off()
####### Swedish_stroke2 Variables                             ##################
plot.survival(rfsrc(Surv(time, status)~ ., Swedish_stroke2), cens.model = "km",collapse=TRUE)
pdf(file="Variable_plots.pdf",width=11,height=8)
plot.variable(Ss2.obj, partial = TRUE, smooth.lines = TRUE,main="Stroke Data")
dev.off()
####### Make a blank data frame to accept values for training ######
newData <-Swedish_stroke2[1,]
newData[1,1]<-as.integer(1)
newData[1,2]<-.4
newData[1,4]<-NA
newData[1,5]<-NA
newData[1,3]<-as.difftime(22000,units="days") # Age
# Test the prediction system
Ss2.pred <-predict(Ss2.obj, newData, na.action="na.impute")
Ss2.pred$predicted[[2]]
# Now let's systematically drop blanks down the tree for each sex, age and speed .
####### Build a 16 x 16 matrix of risks                       #######################################################
sex <-c(1,2)
names(sex) <-c("Female","Male")
age <-c(20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000,32000,33000,34000,35000)
speed <-c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1.1,1.2,1.3,1.4,1.5)
emptyGirls <-matrix(numeric(0), 16,16) # Make a blank 16*16 matrix
emptyBoys <-matrix(numeric(0), 16,16) # Make a blank 16*16 matrix

for(i in 1:length(age)){ # Female
  newData[1,3]<-as.difftime(age[i],units="days")
  newData[1,1]<-as.integer(1)
  for(j in 1:length(speed)){
   newData[1,2]<-speed[j] 
   Ss2.pred <-predict(Ss2.obj, newData, na.action="na.impute")
   emptyGirls[i,j]<-Ss2.pred$predicted[[2]] #rows=age columns=speed
                           }
}
for(i in 1:length(age)){ # Male
  newData[1,3]<-as.difftime(age[i],units="days")
  newData[1,1]<-as.integer(2)
  for(j in 1:length(speed)){
    newData[1,2]<-speed[j] 
    Ss2.pred <-predict(Ss2.obj, newData, na.action="na.impute")
    emptyBoys[i,j]<-Ss2.pred$predicted[[2]] #rows=age columns=speed
  }
}
save(emptyBoys,emptyGirls,file="Small_risk_matrix_small.rData")
####### Produce heatmaps of mortality by age~speed            ###############
load(file="./Small_risk_matrix_small.rData")
pdf(file="./PDF_Output/Small_Heatmaps.pdf",width=11,height=8)
emptyB <-format(emptyBoys,digits=2,justify="left",trim=TRUE)
labR<-age
labC<-speed
heatmap.2(emptyBoys, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
          cellnote=emptyB, notecol="black", trace='none',
          notecex=.9, scale="none", density.info="none",
          main="Mortality Post Stroke by Age and Speed (Males)",
          xlab="Initial Speed (M/s)",ylab="Age at Examination",
          margins = c(5,5),
          labRow=format(labR/365,digits=3), labCol=labC)
emptyF <-format(emptyGirls,digits=2)
labR<-age
labC<-speed
heatmap.2(emptyGirls, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
          cellnote=emptyF, notecol="black", trace='none',
          notecex=.9, scale="none", density.info="none",
          main="Mortality Post Stroke by Age and Speed (Females)",
          xlab="Initial Speed (M/s)",ylab="Age at Examination",
          margins = c(5,5),
          labRow=format(labR/365,digits=3), labCol=labC)

dev.off()
####### Build a 51 x 31 matrix                                #############
age2 <-seq(20000,35000,300)
speed2 <-seq(0,1.5,.05)
bigGirls <-matrix(numeric(0), 51,31) # Make a blank 51*31 matrix
bigBoys <-matrix(numeric(0), 51,31) # Make a blank 51*31 matrix
for(i in 1:length(age2)){ # Female
  newData[1,3]<-as.difftime(age2[i],units="days")
  newData[1,1]<-as.integer(1)
  for(j in 1:length(speed2)){
    newData[1,2]<-speed2[j] 
    Ss2.pred <-predict(Ss2.obj, newData, na.action="na.impute")
    bigGirls[i,j]<-Ss2.pred$predicted[[2]] #rows=age columns=speed
  }
}
for(i in 1:length(age2)){ # Male
  newData[1,3]<-as.difftime(age2[i],units="days")
  newData[1,1]<-as.integer(2)
  for(j in 1:length(speed2)){
    newData[1,2]<-speed2[j] 
    Ss2.pred <-predict(Ss2.obj, newData, na.action="na.impute")
   bigBoys[i,j]<-Ss2.pred$predicted[[2]] #rows=age columns=speed
  }
}
save(bigBoys,bigGirls,file="Big_risk_matrix_small.rData")
####### Produce fine grain heatmaps                           ##############################
load(file="Big_risk_matrix_small.rData")
blue.spec <-c("#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
#blue.spec <-c(#f7fbff,#deebf7,#c6dbef","#9ecae1","#6baed6","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
pdf(file="Big_Heatmaps.pdf",onefile=TRUE,width=14,height=8)
emptyBB <-format(bigBoys,digits=2)
labR<-age2
labC<-speed2
heatmap.2(bigBoys, Rowv=FALSE, Colv=FALSE, col=blue.spec, dendrogram='none', 
          cellnote=emptyBB, notecol="black", trace='none',
          notecex=.6, scale="none", density.info="none",
          main="Mortality Post Stroke by Age and Speed (Males)",
          xlab="Initial Speed (M/s)",ylab="Age at Examination",
          margins = c(5,5), labRow=format(labR/365,digits=3), labCol=labC)
empty <-matrix(numeric(0), 16,16) # Make a blank 51*31 matrix
emptyBG <-format(bigGirls,digits=1)
labR<-age2
labC<-speed2
heatmap.2(bigGirls, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
          cellnote=emptyBG, notecol="black", trace='none',
          notecex=.6, scale="none", density.info="none",
          main="Mortality Post Stroke by Age and Speed (Females)",
          xlab="Initial Speed (M/s)",ylab="Age at Examination",
          margins = c(5,5),  labRow=format(labR/365,digits=3), labCol=labC)
dev.off() 
####### Mortality curves ~ sex + age with lowess smoothing    #########
####### Each lowess curve is stored as an rData file with the title mort/Sex/Age.rData ###
pdf(file="Male_curves.pdf",width=10,height=8)
for(i in 1:length(age2)){
  CurveM <-as.data.frame(cbind(as.numeric(speed2),as.numeric(bigBoys[i,])))
  ti <-str_c("Speed ~ Mortality curve for Men Aged ",format(age2[[i]]/365,digits=2))
  ti2 <-str_c("mortM",format(age2[[i]]/365,digits=2),".rData")
  low<- as.data.frame(lowess(formula=CurveM[[2]]~CurveM[[1]],data=CurveM,iter=4,f=.62))
  plot(CurveM[[2]]~CurveM[[1]],main=ti, xlab="Walking Speed in Shoes",ylab="10 Year Mortality %")
  lines(lowess(formula=CurveM[[2]]~CurveM[[1]],data=CurveM,iter=4,f=.62),col=2)
  sexlow<-"M"
  agelow<-round(age2[[i]]/365,digits=0)
  fname <-str_c("mort",sexlow,agelow,".rData")
  load(file=fname)
  # Initial walking speeds
  sp <-low[,1]
  # risk
  rod <-low[,2]
  # m/s change after treatment
  incr <-c(1,.9,.8,.7,.6,.5,.4,.3,.2,.1,0,-.1,-.2,-.3,-.4,-.5) 
  names(low) <-c("sp","rod")
  plot(data=low,rod~sp,main="Data", xlab="Walking Speed (M/s)",ylab="10 Year Risk of Death")
  
  # If the speeds in the data set are not expressed 
  # tidily there will need to be some interpolation
  # to give the ability to identify hazard ratio 
  # extrapolation at desired speeds to enable matrix 
  # calculation. Here the dummy data has speeds 
  # in .1 m/s intervals so no need for extrapolation.
  
  # At each speed let's work out the reductions in 
  # risk of death supposedly delivered by each in(de)crease in speed
  
  empty <-matrix(numeric(0), length(incr),nrow(low)) # Make a blank matrix
  attach(low)
  for(s in 1:nrow(low)){# A loop that handles each speed
    sp2 <-sp[s]+incr     # Calculate the post op speed by column
    # The next loop removes negative speeds 
    #(we don't want stroke victims
    # walking backwards for health & safety reasons)
    for(y in 1:length(sp2)){if(sp2[y]<0) sp2[y]<-0}
    # The next loop removes post op speeds above 
    # the upper speed in the dataset
    for(y in 1:length(sp2)){if(sp2[y]>low$sp[nrow(low)]) 
      sp2[y]<-low$sp[nrow(low)]}
    # The next loop finds the equivalent risk for each level of sp2
    for(x in length(sp2):1){
      for(z in 1:nrow(low)){ low$diff[z]<-abs(low$sp[z]-sp2[x])}
      speedpoint <-which.min(low$diff)
      empty[x,s] <-low$rod[s] - low$rod[speedpoint]
    }
  }
  detach(low)
  
  emptyF <-round(empty,digits=1)
  labR<-incr
  labC<-low$sp
  heatmap.2(empty, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
            cellnote=emptyF, notecol="black", trace='none',
            notecex=.5, scale="none", density.info="none",
            main=str_c("Risk of Death Reduction (%) Sex = ",sexlow," Age = ",agelow,"Yrs"),
            xlab="Initial Speed (M/s)",ylab="Speed Change Post Op (M/s)",
            margins = c(3.5,3.8),
            labRow=labR, labCol=labC)
  save(low,file=ti2)
  par(new=FALSE)}
dev.off()
pdf(file="Female_curves.pdf",width=10,height=8)
for(i in 1:length(age2)){
  CurveF <-as.data.frame(cbind(as.numeric(speed2),as.numeric(bigGirls[i,])))
  ti <-str_c("Speed ~ Mortality curve for Women Aged ",format(age2[[i]]/365,digits=2))
  ti2 <-str_c("mortF",format(age2[[i]]/365,digits=2),".rData")
  low<- as.data.frame(lowess(formula=CurveF[[2]]~CurveF[[1]],data=CurveF,iter=4,f=.62))
  plot(CurveF[[2]]~CurveF[[1]],main=ti, xlab="Walking Speed in Shoes",ylab="10 Year Mortality %")
  lines(lowess(formula=CurveF[[2]]~CurveF[[1]],data=CurveF,iter=4,f=.62),col=2)
  sexlow<-"F"
  agelow<-round(age2[[i]]/365,digits=0)
  fname <-str_c("mort",sexlow,agelow,".rData")
  load(file=fname)
  # Initial walking speeds
  sp <-low[,1]
  # risk
  rod <-low[,2]
  # m/s change after treatment
  incr <-c(1,.9,.8,.7,.6,.5,.4,.3,.2,.1,0,-.1,-.2,-.3,-.4,-.5) 
  names(low) <-c("sp","rod")
  plot(data=low,rod~sp,main="Data", xlab="Walking Speed (M/s)",ylab="10 Year Risk of Death")
  
  # If the speeds in the data set are not expressed 
  # tidily there will need to be some interpolation
  # to give the ability to identify hazard ratio 
  # extrapolation at desired speeds to enable matrix 
  # calculation. Here the dummy data has speeds 
  # in .1 m/s intervals so no need for extrapolation.
  
  # At each speed let's work out the reductions in 
  # risk of death supposedly delivered by each in(de)crease in speed
  
  empty <-matrix(numeric(0), length(incr),nrow(low)) # Make a blank matrix
  attach(low)
  for(s in 1:nrow(low)){# A loop that handles each speed
    sp2 <-sp[s]+incr     # Calculate the post op speed by column
    # The next loop removes negative speeds 
    #(we don't want stroke victims
    # walking backwards for health & safety reasons)
    for(y in 1:length(sp2)){if(sp2[y]<0) sp2[y]<-0}
    # The next loop removes post op speeds above 
    # the upper speed in the dataset
    for(y in 1:length(sp2)){if(sp2[y]>low$sp[nrow(low)]) 
      sp2[y]<-low$sp[nrow(low)]}
    # The next loop finds the equivalent risk for each level of sp2
    for(x in length(sp2):1){
      for(z in 1:nrow(low)){ low$diff[z]<-abs(low$sp[z]-sp2[x])}
      speedpoint <-which.min(low$diff)
      empty[x,s] <-low$rod[s] - low$rod[speedpoint]
    }
  }
  detach(low)
  
  emptyF <-round(empty,digits=1)
  labR<-incr
  labC<-low$sp
  heatmap.2(empty, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
            cellnote=emptyF, notecol="black", trace='none',
            notecex=.5, scale="none", density.info="none",
            main=str_c("Risk of Death Reduction (%) Sex = ",sexlow," Age = ",agelow,"Yrs"),
            xlab="Initial Speed (M/s)",ylab="Speed Change Post Op (M/s)",
            margins = c(3.5,3.8),
            labRow=labR, labCol=labC)
 save(low,file=ti2)
  par(new=FALSE)}
dev.off()
####### Sandbox                                               #######################################################
sexlow<-"F"
agelow<-70
fname <-str_c("mort",sexlow,agelow,".rData")
load(file=fname)
# Initial walking speeds
sp <-low[,1]
# risk
rod <-low[,2]
# m/s change after treatment
incr <-c(1,.9,.8,.7,.6,.5,.4,.3,.2,.1,0,-.1,-.2,-.3,-.4,-.5) 
names(low) <-c("sp","rod")
plot(data=low,rod~sp,main="Data", xlab="Walking Speed (M/s)",ylab="10 Year Risk of Death")

# If the speeds in the data set are not expressed 
# tidily there will need to be some interpolation
# to give the ability to identify hazard ratio 
# extrapolation at desired speeds to enable matrix 
# calculation. Here the dummy data has speeds 
# in .1 m/s intervals so no need for extrapolation.

# At each speed let's work out the reductions in 
# risk of death supposedly delivered by each in(de)crease in speed

empty <-matrix(numeric(0), length(incr),nrow(low)) # Make a blank matrix
attach(low)
for(s in 1:nrow(low)){# A loop that handles each speed
  sp2 <-sp[s]+incr     # Calculate the post op speed by column
  # The next loop removes negative speeds 
  #(we don't want stroke victims
  # walking backwards for health & safety reasons)
  for(y in 1:length(sp2)){if(sp2[y]<0) sp2[y]<-0}
  # The next loop removes post op speeds above 
  # the upper speed in the dataset
  for(y in 1:length(sp2)){if(sp2[y]>low$sp[nrow(low)]) 
    sp2[y]<-low$sp[nrow(low)]}
  # The next loop finds the equivalent risk for each level of sp2
  for(x in length(sp2):1){
    for(z in 1:nrow(low)){ low$diff[z]<-abs(low$sp[z]-sp2[x])}
    speedpoint <-which.min(low$diff)
    empty[x,s] <-low$rod[s] - low$rod[speedpoint]
                        }
}
detach(low)

emptyF <-round(empty,digits=1)
labR<-incr
labC<-low$sp
heatmap.2(empty, Rowv=FALSE, Colv=FALSE, dendrogram='none', 
          cellnote=emptyF, notecol="black", trace='none',
          notecex=.5, scale="none", density.info="none",
          main=str_c("Risk of Death Reduction (%) Sex = ",sexlow," Age = ",agelow,"Yrs"),
          xlab="Initial Speed (M/s)",ylab="Speed Change Post Op (M/s)",
          margins = c(3.5,3.8),
          labRow=labR, labCol=labC)
####### Look at this in 3D                                    ################
library(scatterplot3d)
library(sculpt3d)
library(plot3D)
load(file="Big_risk_matrix_small.rData")
s3d.dat <- data.frame(cols=as.vector(col(bigBoys)),
                      rows=as.vector(row(bigBoys)),
                      value=as.vector(bigBoys))

sculpt3d(s3d.dat, type="s", radius=.5 )
scatterplot3d(s3d.dat, type="p", lwd=5, pch=" ",
              x.ticklabs=colnames(bigBoys), y.ticklabs=rownames(bigBoys), main="scatterplot3d - 4")
scatter3D(s3d.dat[,1], s3d.dat[,2], s3d.dat[,3], colvar=s3d.dat[,3], lighting =  TRUE, surf=TRUE, lphi = 60, ltheta = 10)
bBa <-as.array(as.vector(col(bigBoys)),as.vector(row(bigBoys)),as.vector(bigBoys),dim=3)
filename <- tempfile(fileext=".obj")
open3d()
plot3d(z=as.vector(bigBoys),y=age2/365,x=speed2,zlab="Mortality %",xlab="Walking Speed M/sec",ylab="Age Yrs")
writeOBJ("test.obj",
         pointRadius = 0.005, pointShape = icosahedron3d(),
         lineRadius = pointRadius, lineSides = 20,
         pointsAsPoints = FALSE, linesAsLines = FALSE,
         withNormals = TRUE, withTextures = TRUE,
         separateObjects = TRUE,
         ids = NULL)
writePLY("test.ply",
         pointRadius = 0.005, pointShape = icosahedron3d(),
        lineSides = 20, pointsAsEdges = FALSE,  withColors = TRUE,  ids = NULL)
writeWebGL(dir = "./webGL", filename = file.path(dir, "index.html"), 
           template = system.file(file.path("WebGL", "template.html"), package = "rgl"),
           prefix = "",
           snapshot = TRUE, font = "Arial", width, height)
browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
                                      width=500), sep=""))
