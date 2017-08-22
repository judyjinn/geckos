

##################################
#           Set shit up          #
##################################

#open data
soapwater = read.csv("soapwater_all.csv") #all trials
#soapwater_indv = read.csv("soapwater_indv.csv") #indivudal gecko averages for their trials


#change things to factors
soapwater$geckoID = as.factor(soapwater$geckoID)
soapwater$trial = as.factor(soapwater$trial)

#soapwater_indv$geckoID = as.factor(soapwater_indv$geckoID)


#Wilcox.test only looks at values to the 4th decimal. This is unfortunate because it reveals many ties on accident in our stride kinematics and impulse data. Multiple stride times by 1000 to make it into milliseconds. Multiply the impulses by 1000 to make them into mN-s and multiple percent impulse to make into whole percent
#---------------------------- for all trials ---------------------------------
soapwater$stance = soapwater$stance*1000
soapwater$swing = soapwater$swing*1000
soapwater$period = soapwater$period*1000
soapwater$gait.timing = soapwater$gait.timing*1000 #likely wrong right now.

soapwater$fl.slap.imp = soapwater$fl.slap.imp*1000
soapwater$fl.stroke.imp = soapwater$fl.stroke.imp*1000
soapwater$bl.slap.imp = soapwater$bl.slap.imp*1000
soapwater$bl.stroke.imp = soapwater$bl.stroke.imp*1000
soapwater$min.imp = soapwater$min.imp*1000
soapwater$Est.impulse = soapwater$Est.impulse*1000

#make the percents whole
soapwater$imp.percent = soapwater$imp.percent*100
# 

# #---------------------------- for individuals ---------------------------------
# soapwater_indv$stance = soapwater_indv$stance*1000
# soapwater_indv$swing = soapwater_indv$swing*1000
# soapwater_indv$period = soapwater_indv$period*1000
# soapwater_indv$gait.timing = soapwater_indv$gait.timing*1000 #likely wrong right now.
# 
# 
# soapwater_indv$fl.slap.imp = soapwater_indv$fl.slap.imp*1000
# soapwater_indv$fl.stroke.imp = soapwater_indv$fl.stroke.imp*1000
# soapwater_indv$bl.slap.imp = soapwater_indv$bl.slap.imp*1000
# soapwater_indv$bl.stroke.imp = soapwater_indv$bl.stroke.imp*1000
# soapwater_indv$min.imp = soapwater_indv$min.imp*1000
# soapwater_indv$Est.impulse = soapwater_indv$Est.impulse*1000
# 
# #make the percents whole
# soapwater_indv$imp.percent = soapwater_indv$imp.percent*100


#turn off scientific format for numbers
options(scipen=999)

##################################
#         Correlations           #
##################################

#correlation scatter plots
pairs(~vel + head.ht + fl.height + bl.height + fl.slap.vel + bl.slap.vel + fl.stroke.vel + bl.stroke.vel, data= soapwater[soapwater$expt=="soap",], main="Scatterplot Matrix for Soap Expts")

pairs(~vel + head.ht + fl.height + bl.height + fl.slap.vel + bl.slap.vel + fl.stroke.vel + bl.stroke.vel, data= soapwater[soapwater$expt=="water",], main="Scatterplot Matrix for Water Expts")

#correlation matrix of data. Gives r2 values
soapX=as.matrix(soapwater[(soapwater$expt=="soap"),5:10])
cor.soapX=cor(soapX)
write.csv(cor.soapX, file="cor.soapX.csv") #save file


waterX=as.matrix(soapwater[(soapwater$expt=="water"),5:10])
cor.waterX=cor(waterX)
write.csv(cor.waterX, file="cor.waterX.csv") #save file



##################################
#             LMEs               #
##################################
library(nlme)
#linear mixed effects model for geckoID
#stance, swing, period not used because they are directly related to SSvelocities

#-------------------- Water trials --------------------------

#lme for head height
water.headht.lme = lme(head.ht ~ fl.height + bl.height + fl.slap.vel + bl.slap.vel + fl.stroke.vel + bl.stroke.vel, random=~1|geckoID, soapwater[soapwater$expt=="water",])
summary(water.headht.lme)

qqnorm(resid(water.headht.lme), main = "Normal Q-Q Plot for Head Height in Water Expts", xlab = "Predictors", ylab = "Head Height")

#lme for vel
water.vel.lme = lme(vel ~ fl.height + bl.height + fl.slap.vel + bl.slap.vel + fl.stroke.vel + bl.stroke.vel, random=~1|geckoID, soapwater[soapwater$expt=="water",])
summary(water.vel.lme)

qqnorm(resid(water.vel.lme), main = "Normal Q-Q Plot for Velocity in Water Epts", xlab = "Predictors", ylab = "Velocity")

#checking for direct corelation of headht and vel
asdf = lme(head.ht~vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
summary(asdf)


#The following section is unusable because the soap trials contain too little data to use a lme. Can only look at simple correlational data. Will throw a NaNs produced error.

"-------------------- Soap trials --------------------------"
#lme for head height
# soap.headht.lme = lme(head.ht ~ fl.height + bl.height + fl.slap.vel + bl.slap.vel + fl.stroke.vel + bl.stroke.vel, random=~1|geckoID, soapwater[soapwater$expt=="soap",])
# summary(soap.headht.lme)
# # 
# qqnorm(resid(soap.headht.lme), main = "Normal Q-Q Plot for Head Height in Water Expts", xlab = "Predictors", ylab = "Head Height")
# 
# #lme for vel
# soap.vel.lme = lme(vel ~ fl.height + bl.height + fl.slap.vel + bl.slap.vel + fl.stroke.vel + bl.stroke.vel, random=~1|geckoID, soapwater[soapwater$expt=="soap",])
# summary(soap.vel.lme)
# 
# qqnorm(resid(soap.vel.lme), main = "Normal Q-Q Plot for Velocity in Soap Epts", xlab = "Predictors", ylab = "Velocity")
# 
# #checking for direct corelation of headht and vel
# asdf2 = lme(head.ht~vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
# summary(asdf2)





#----------------------------- descriptive stats -----------------------------------

#checking for normality, and gets the basic stats for the data 
install.packages("boot")
install.packages("pastecs")
library(boot)
library(pastecs)

#for independent trials
soapwater_desc_raw = do.call("rbind",by(data = soapwater[, c("fl.height", "bl.height", "fl.slap.vel", "bl.slap.vel", "fl.stroke.vel", "bl.stroke.vel", "stance", "swing", "period", "vel", "head.ht", "fl.slap.imp", "fl.stroke.imp", "bl.slap.imp", "bl.stroke.imp", "min.imp", "Est.impulse", "imp.percent", "gait.timing")], INDICES = soapwater$expt, FUN=stat.desc, basic= TRUE, norm = TRUE))
    #do.call("rbind", ....) converts the by (which is a list) into a dataframe.
    #all soap are normally distributed, except gait.timing
    #all water except bl.height and bl.stroke.vel are NOT normally distributed
write.csv(soapwater_desc_raw, file="soapwater_desc_raw.csv") #save file

# #for individuals.
# soapwater_indv_desc = do.call("rbind",by(data = soapwater_indv[, c("fl.height", "bl.height", "fl.slap.vel", "bl.slap.vel", "fl.stroke.vel", "bl.stroke.vel", "stance", "swing", "period", "vel", "head.ht", "fl.slap.imp", "fl.stroke.imp", "bl.slap.imp", "bl.stroke.imp", "min.imp", "Est.impulse", "imp.percent", "gait.timing")], INDICES = soapwater_indv$expt, FUN=stat.desc, basic= TRUE, norm = TRUE))
# #do.call("rbind", ....) converts the by (which is a list) into a dataframe.
# #all soap are normally distributed
# #all water except bl.height and bl.stroke.vel are NOT normally distributed
# write.csv(soapwater_indv_desc, file="soapwater_indv_desc.csv") #save file
# 


################################
# 1 way repeated ANOVA         #
################################

aov_fl.height = ezANOVA(data=soapwater, dv=fl.height, wid=geckoID, between=expt)
aov_bl.height = ezANOVA(data=soapwater, dv=bl.height, wid=geckoID, between=expt)
aov_fl.slap = ezANOVA(data=soapwater, dv=fl.slap.vel, wid=geckoID, between=expt)
aov_bl.slap = ezANOVA(data=soapwater, dv=bl.slap.vel, wid=geckoID, between=expt)
aov_fl.stroke = ezANOVA(data=soapwater, dv=fl.stroke.vel, wid=geckoID, between=expt)
aov_bl.stroke = ezANOVA(data=soapwater, dv=bl.stroke.vel, wid=geckoID, between=expt)
aov_stance = ezANOVA(data=soapwater, dv=stance, wid=geckoID, between=expt)
aov_swing = ezANOVA(data=soapwater, dv=swing, wid=geckoID, between=expt)
aov_period = ezANOVA(data=soapwater, dv=period, wid=geckoID, between=expt)
aov_vel = ezANOVA(data=soapwater, dv=vel, wid=geckoID, between=expt)
aov_head.ht = ezANOVA(data=soapwater, dv=head.ht, wid=geckoID, between=expt)
aov_fl.slap.imp = ezANOVA(data=soapwater, dv=fl.slap.imp, wid=geckoID, between=expt)
aov_fl.stroke.imp = ezANOVA(data=soapwater, dv=fl.stroke.imp, wid=geckoID, between=expt)
aov_bl.slap.imp = ezANOVA(data=soapwater, dv=bl.slap.imp, wid=geckoID, between=expt)
aov_bl.stroke.imp = ezANOVA(data=soapwater, dv=bl.stroke.imp, wid=geckoID, between=expt)
aov_min.imp = ezANOVA(data=soapwater, dv=min.imp, wid=geckoID, between=expt)
aov_est.impulse = ezANOVA(data=soapwater, dv=Est.impulse, wid=geckoID, between=expt)
aov_per.imp = ezANOVA(data=soapwater, dv=imp.percent, wid=geckoID, between=expt)


















#####################################
#     Mann-Whitney-Wilcoxon Test    #
#####################################
#Ignore all of this MWW stuff. used a 2 way repeated ANOVA

#Using the MWW ranks sum test because almost all soap vs water comparisons have water as nonnormal data. We also have unequal sample sizes, and some unequal variances, thus a t-test is not a good choice.
#We cannot use the MWW rank sign test for paired data because we have nonnormal data
#The MWW test is a nonparameteric test which compares MEDIANS.

#IGNORE MWW
# #-------------------------------- MWW-tests set up --------------------------------------------
# 
# install.packages("plyr") #so we can save our output into a data frame
# library(plyr)
# 
# #------------------------- MWW-tests with each trial as independent------------------------------
# 
# #name the columns desired to test
# cols_to_test <- c("fl.height", "bl.height", "fl.slap.vel", "bl.slap.vel", "fl.stroke.vel", "bl.stroke.vel", "stance", "swing", "period", "vel", "head.ht", "fl.slap.imp", "fl.stroke.imp", "bl.slap.imp", "bl.stroke.imp", "min.imp", "Est.impulse", "imp.percent", "gait.timing")
# MWW_independent<- ldply(
#   cols_to_test,
#   function(colname) {
#     w_val = wilcox.test(soapwater[[colname]] ~ soapwater$expt)$statistic
#     p_val = wilcox.test(soapwater[[colname]] ~ soapwater$expt)$p.value
#     return(data.frame(colname=colname, w_value=w_val, p_val=p_val))
#   })
# write.csv(MWW_independent, file="MWW_independent.csv") #save file
# 
# #for some crazy reason, the wilcox test keeps saying the stance, swing, period, and min.imp is all tied, but the only real tie is a value of 25.0 in stance. I jittered all the values in the data, redid the test, the warnings go away, and the p-value barely changes, so I would say we can safely ignore the warning.
# 
# 
# #------------------------- MWW-tests with individual gecko avgs------------------------------
# 
# 
# #name the columns desired to test
# cols_to_test <- c("fl.height", "bl.height", "fl.slap.vel", "bl.slap.vel", "fl.stroke.vel", "bl.stroke.vel", "stance", "swing", "period", "vel", "head.ht", "fl.slap.imp", "fl.stroke.imp", "bl.slap.imp", "bl.stroke.imp", "min.imp", "Est.impulse", "imp.percent", "gait.timing")
# MWW_indv <- ldply(
#   cols_to_test,
#   function(colname) {
#     w_val = wilcox.test(soapwater_indv[[colname]] ~ soapwater_indv$expt)$statistic
#     p_val = wilcox.test(soapwater_indv[[colname]] ~ soapwater_indv$expt)$p.value
#     return(data.frame(colname=colname, w_value=w_val, p_val=p_val))
#   })
# write.csv(MWW_indv, file="MWW_indv.csv") #save file
# 
# #all significances stay the same except bl.stroke and fl.stroke.imp changes to non sig for indv.
# 
# 







##############################################
# Simple Correlations, QQ, Residuals         #
##############################################


#----------------------------Water Trials -------------------------

################front legs#######################
water.fl.slapVfl.height = lme(fl.slap.vel~fl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(fl.slap.vel~fl.height, data=soapwater[soapwater$expt=="water",])
summary(water.fl.slapVfl.height)
     #CORRELATED. Higher front limb height = faster slap vel
qqnorm(resid(water.fl.slapVfl.height))
plot(soapwater[soapwater$expt=="water", c("fl.slap.vel")], residuals(water.fl.slapVfl.height)) + abline(0,0)
    #residual shows not normal

#Analyze this correlation for the split in data
water.fl.strokeVslap = lme(fl.stroke.vel~fl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(fl.stroke.vel~fl.slap.vel, data=soapwater[soapwater$expt=="water",])
summary(water.fl.strokeVslap)
     #CORRELATED. 
qqnorm(resid(water.fl.strokeVslap))
plot(soapwater[soapwater$expt=="water", c("fl.stroke.vel")], residuals(water.fl.strokeVslap)) + abline(0,0)

#head height
water.fl.headhtvsheight = lme(head.ht~fl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
summary(water.fl.headhtvsheight)
qqnorm(resid(water.fl.headhtvsheight))
plot(soapwater[soapwater$expt=="water", c("fl.height")], residuals(water.fl.headhtvsheight)) + abline(0,0)


water.fl.headhtVslap=lme(head.ht~fl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(head.ht~fl.slap.vel, data=soapwater[soapwater$expt=="water",])
summary(water.fl.headhtVslap)
     #CORRELATE: fl slap used for head height 
qqnorm(resid(water.fl.headhtVslap))
plot(soapwater[soapwater$expt=="water", c("fl.slap.vel")], residuals(water.fl.headhtVslap)) + abline(0,0)


water.fl.headhtVstroke=lme(head.ht~fl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(head.ht~fl.stroke.vel, data=soapwater[soapwater$expt=="water",])
summary(water.fl.headhtVstroke)
qqnorm(resid(water.fl.headhtVstroke))
plot(soapwater[soapwater$expt=="water", c("fl.stroke.vel")], residuals(water.fl.headhtVstroke)) + abline(0,0)


#velocity
water.fl.velvsheight=lme(vel~fl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
summary(water.fl.velvsheight)
qqnorm(resid(water.fl.velvsheight))
plot(soapwater[soapwater$expt=="water", c("fl.height")], residuals(water.fl.velvsheight)) + abline(0,0)

water.fl.velVslap = lme(vel~fl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(vel~fl.slap.vel, data=soapwater[soapwater$expt=="water",])
summary(water.fl.velVslap)
qqnorm(resid(water.fl.velVslap))
plot(soapwater[soapwater$expt=="water", c("fl.slap.vel")], residuals(water.fl.velVslap)) + abline(0,0)

water.fl.velVstroke = lme(vel~fl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(vel~fl.stroke.vel, data=soapwater[soapwater$expt=="water",])
summary(water.fl.velVstroke)
     #CORRELATED: fl stroke used for speed4
qqnorm(resid(water.fl.velVstroke))
plot(soapwater[soapwater$expt=="water", c("fl.stroke.vel")], residuals(water.fl.velVstroke)) + abline(0,0)

    

############################back legs#####################3
water.bl.slapVbl.height = lme(bl.slap.vel~bl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(bl.slap.vel~bl.height, data=soapwater[soapwater$expt=="water",])
summary(water.bl.slapVbl.height)
     #CORRELATED
qqnorm(resid(water.bl.slapVbl.height))
plot(soapwater[soapwater$expt=="water", c("bl.slap.vel")], residuals(water.bl.slapVbl.height)) + abline(0,0)

#Analyze for the grouping situation
water.bl.strokeVslap= lme(bl.stroke.vel~bl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(bl.stroke.vel~bl.slap.vel,data=soapwater[soapwater$expt=="water",])
summary(water.bl.strokeVslap)
     #CORRELATED
qqnorm(resid(water.bl.strokeVslap))
plot(soapwater[soapwater$expt=="water", c("bl.slap.vel")], residuals(water.bl.strokeVslap)) + abline(0,0)

#head height
water.bl.headhtvsheight=lme(head.ht~bl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
summary(water.bl.headhtvsheight)
plot(head.ht~bl.height, data=soapwater[soapwater$expt=="water",])
qqnorm(resid(water.bl.headhtvsheight))
plot(soapwater[soapwater$expt=="water", c("bl.height")], residuals(water.bl.headhtvsheight)) + abline(0,0)

water.bl.headhtVslap = lme(head.ht~bl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(head.ht~bl.slap.vel, data=soapwater[soapwater$expt=="water",])
summary(water.bl.headhtVslap)
     #not correlated, angle of foot entry doesn't really create much for forces
qqnorm(resid(water.bl.headhtVslap))
plot(soapwater[soapwater$expt=="water", c("bl.slap.vel")], residuals(water.bl.headhtVslap)) + abline(0,0)


water.bl.headhtVstroke = lme(head.ht~bl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(head.ht~bl.stroke.vel, data=soapwater[soapwater$expt=="water",])
summary(water.bl.headhtVstroke)
qqnorm(resid(water.bl.headhtVstroke))
plot(soapwater[soapwater$expt=="water", c("bl.stroke.vel")], residuals(water.bl.headhtVstroke)) + abline(0,0)


#velocity
water.bl.velvsheight=lme(vel~bl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
summary(water.bl.velvsheight)
plot(vel~bl.height, data=soapwater[soapwater$expt=="water",])
qqnorm(resid(water.bl.velvsheight))
plot(soapwater[soapwater$expt=="water", c("bl.height")], residuals(water.bl.velvsheight)) + abline(0,0)

water.bl.velVslap = lme(vel~bl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(vel~bl.slap.vel, data=soapwater[soapwater$expt=="water",])
summary(water.bl.velVslap)
qqnorm(resid(water.bl.velVslap))
plot(soapwater[soapwater$expt=="water", c("bl.slap.vel")], residuals(water.bl.velVslap)) + abline(0,0)

water.bl.velVstroke = lme(vel~bl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="water",])
plot(vel~bl.stroke.vel, data=soapwater[soapwater$expt=="water",])
summary(water.bl.velVstroke)
     #CORRELATED.
qqnorm(resid(water.bl.velVstroke))
plot(soapwater[soapwater$expt=="water", c("bl.stroke.vel")], residuals(water.bl.velVstroke)) + abline(0,0)






#Might have problems with low number of soap trials.

#----------------------------Soap Trials -------------------------

################front legs#######################
soap.fl.slapVfl.height = lme(fl.slap.vel~fl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(fl.slap.vel~fl.height, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.slapVfl.height)
qqnorm(resid(soap.fl.slapVfl.height))
plot(soapwater[soapwater$expt=="soap", c("fl.slap.vel")], residuals(soap.fl.slapVfl.height)) + abline(0,0)

soap.fl.strokeVslap = lme(fl.stroke.vel~fl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(fl.stroke.vel~fl.slap.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.strokeVslap)
qqnorm(resid(soap.fl.strokeVslap))
plot(soapwater[soapwater$expt=="soap", c("fl.stroke.vel")], residuals(soap.fl.strokeVslap)) + abline(0,0)

#head height
soap.fl.headhtvsheight = lme(head.ht~fl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.headhtvsheight)
plot(head.ht~fl.height, data=soapwater[soapwater$expt=="soap",])
qqnorm(resid(soap.fl.headhtvsheight))
plot(soapwater[soapwater$expt=="soap", c("fl.height")], residuals(soap.fl.headhtvsheight)) + abline(0,0)


soap.fl.headhtVslap=lme(head.ht~fl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(head.ht~fl.slap.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.headhtVslap)
qqnorm(resid(soap.fl.headhtVslap))
plot(soapwater[soapwater$expt=="soap", c("fl.slap.vel")], residuals(soap.fl.headhtVslap)) + abline(0,0)


soap.fl.headhtVstroke=lme(head.ht~fl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(head.ht~fl.stroke.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.headhtVstroke)
qqnorm(resid(soap.fl.headhtVstroke))
plot(soapwater[soapwater$expt=="soap", c("fl.stroke.vel")], residuals(soap.fl.headhtVstroke)) + abline(0,0)


#velocity
soap.fl.velvsheight=lme(vel~fl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.velvsheight)
qqnorm(resid(soap.fl.velvsheight))
plot(soapwater[soapwater$expt=="soap", c("fl.height")], residuals(soap.fl.velvsheight)) + abline(0,0)

soap.fl.velVslap = lme(vel~fl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(vel~fl.slap.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.velVslap)
qqnorm(resid(soap.fl.velVslap))
plot(soapwater[soapwater$expt=="soap", c("fl.slap.vel")], residuals(soap.fl.velVslap)) + abline(0,0)

soap.fl.velVstroke = lme(vel~fl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(vel~fl.stroke.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.fl.velVstroke)
qqnorm(resid(soap.fl.velVstroke))
plot(soapwater[soapwater$expt=="soap", c("fl.stroke.vel")], residuals(soap.fl.velVstroke)) + abline(0,0)



############################back legs#####################3
soap.bl.slapVbl.height = lme(bl.slap.vel~bl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(bl.slap.vel~bl.height, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.slapVbl.height)
qqnorm(resid(soap.bl.slapVbl.height))
plot(soapwater[soapwater$expt=="soap", c("bl.slap.vel")], residuals(soap.bl.slapVbl.height)) + abline(0,0)

#Analyze for the grouping situation
soap.bl.strokeVslap= lme(bl.stroke.vel~bl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(bl.stroke.vel~bl.slap.vel,data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.strokeVslap)
qqnorm(resid(soap.bl.strokeVslap))
plot(soapwater[soapwater$expt=="soap", c("bl.slap.vel")], residuals(soap.bl.strokeVslap)) + abline(0,0)

#head height
soap.bl.headhtvsheight=lme(head.ht~bl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.headhtvsheight)
plot(head.ht~bl.height, data=soapwater[soapwater$expt=="soap",])
qqnorm(resid(soap.bl.headhtvsheight))
plot(soapwater[soapwater$expt=="soap", c("bl.height")], residuals(soap.bl.headhtvsheight)) + abline(0,0)

soap.bl.headhtVslap = lme(head.ht~bl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(head.ht~bl.slap.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.headhtVslap)
qqnorm(resid(soap.bl.headhtVslap))
plot(soapwater[soapwater$expt=="soap", c("bl.slap.vel")], residuals(soap.bl.headhtVslap)) + abline(0,0)


soap.bl.headhtVstroke = lme(head.ht~bl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(head.ht~bl.stroke.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.headhtVstroke)
qqnorm(resid(soap.bl.headhtVstroke))
plot(soapwater[soapwater$expt=="soap", c("bl.stroke.vel")], residuals(soap.bl.headhtVstroke)) + abline(0,0)


#velocity
soap.bl.velvsheight=lme(vel~bl.height, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.velvsheight)
plot(vel~bl.height, data=soapwater[soapwater$expt=="soap",])
qqnorm(resid(soap.bl.velvsheight))
plot(soapwater[soapwater$expt=="soap", c("bl.height")], residuals(soap.bl.velvsheight)) + abline(0,0)

soap.bl.velVslap = lme(vel~bl.slap.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(vel~bl.slap.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.velVslap)
qqnorm(resid(soap.bl.velVslap))
plot(soapwater[soapwater$expt=="soap", c("bl.slap.vel")], residuals(soap.bl.velVslap)) + abline(0,0)

soap.bl.velVstroke = lme(vel~bl.stroke.vel, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(vel~bl.stroke.vel, data=soapwater[soapwater$expt=="soap",])
summary(soap.bl.velVstroke)
qqnorm(resid(soap.bl.velVstroke))
plot(soapwater[soapwater$expt=="soap", c("bl.stroke.vel")], residuals(soap.bl.velVstroke)) + abline(0,0)


#### Further testing in soap conditions for velocity#####
soap.velVstance = lme(vel~stance, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(vel~stance, data=soapwater[soapwater$expt=="soap",])
summary(soap.velVstance)
qqnorm(resid(soap.velVstance))
plot(soapwater[soapwater$expt=="soap", c("bl.stroke.vel")], residuals(soap.velVstance)) + abline(0,0)

soap.velVperiod = lme(vel~period, random=~1|geckoID, data=soapwater[soapwater$expt=="soap",])
plot(vel~period, data=soapwater[soapwater$expt=="soap",])
summary(soap.velVperiod)
qqnorm(resid(soap.velVperiod))
plot(soapwater[soapwater$expt=="soap", c("bl.stroke.vel")], residuals(soap.velVperiod)) + abline(0,0)











###################----------GGPLOTs----------###################
library(ggplot2)

###Head Height in WATER
water.hdhtVflslap = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.slap.vel, y=head.ht)) + 
     xlab(label = "Front Leg Slap Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
     geom_point(size=8, shape=21, bg="light blue", col="blue") +
     theme_bw() +
     theme(
          legend.position="none",
          axis.title.x = element_text(face="bold", colour=, size=16),
          axis.text.x  = element_text(angle=, vjust=, size=14),
          axis.title.y = element_text(face="bold", colour=, size=16),
          axis.text.y  = element_text(angle=, vjust=, size=14),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          aspect.ratio=(6/10)
          )
water.hdhtVflstroke = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.stroke.vel, y=head.ht)) + 
  xlab(label = "Front Leg Stroke Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=8, shape=21, bg="royalblue", col="royalblue4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )


water.hdhtVblslap = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.slap.vel, y=head.ht)) + 
     xlab(label = "Hind Leg Slap Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
     geom_point(size=8, shape=21, bg="darkolivegreen1", col="darkolivegreen4") +
     theme_bw() +
     theme(
          legend.position="none",
          axis.title.x = element_text(face="bold", colour=, size=16),
          axis.text.x  = element_text(angle=, vjust=, size=14),
          axis.title.y = element_text(face="bold", colour=, size=16),
          axis.text.y  = element_text(angle=, vjust=, size=14),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          aspect.ratio=(6/10)
          )

water.hdhtVblstroke = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.stroke.vel, y=head.ht)) + 
  xlab(label = "Hind Leg Stroke Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=8, shape=21, bg="palegreen", col="palegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

library(grid) #run at bottom of this script
multiplot(water.hdhtVflslap, water.hdhtVflstroke, water.hdhtVblslap, water.hdhtVblstroke, cols=2)

###Velocity in WATER

water.velVflslap = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.slap.vel, y=vel)) + 
  xlab(label = "Front Leg Slap Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=24, bg="light blue", col="blue") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
water.velVflstroke = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.stroke.vel, y=vel)) + 
  xlab(label = "Front Leg Stroke Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=24, bg="royalblue", col="royalblue4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )


water.velVblslap = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.slap.vel, y=vel)) + 
  xlab(label = "Hind Leg Slap Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=24, bg="darkolivegreen1", col="darkolivegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

water.velVblstroke = ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.stroke.vel, y=vel)) + 
  xlab(label = "Hind Leg Stroke Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=24, bg="palegreen", col="palegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

library(grid) #run at bottom of this script
multiplot(water.velVflslap, water.velVflstroke, water.velVblslap, water.velVblstroke, cols=2)








######### SOAP PLOTS FOR INDIVIDUALS ####################
###Head Height in SOAP
soap.hdhtVflslap = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = fl.slap.vel, y=head.ht)) + 
  xlab(label = "Front Leg Slap Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=8, shape=16, bg="light blue", col="blue") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
soap.hdhtVflstroke = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = fl.stroke.vel, y=head.ht)) + 
  xlab(label = "Front Leg Stroke Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=8, shape=16, bg="royalblue", col="royalblue4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )


soap.hdhtVblslap = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = bl.slap.vel, y=head.ht)) + 
  xlab(label = "Hind Leg Slap Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=8, shape=16, bg="darkolivegreen1", col="darkolivegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

soap.hdhtVblstroke = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = bl.stroke.vel, y=head.ht)) + 
  xlab(label = "Hind Leg Stroke Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=8, shape=16, bg="palegreen", col="palegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

library(grid) #run at bottom of this script
multiplot(soap.hdhtVflslap, soap.hdhtVflstroke, soap.hdhtVblslap, soap.hdhtVblstroke, cols=2)

###Velocity in Soap

soap.velVflslap = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = fl.slap.vel, y=vel)) + 
  xlab(label = "Front Leg Slap Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=17, bg="light blue", col="blue") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
soap.velVflstroke = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = fl.stroke.vel, y=vel)) + 
  xlab(label = "Front Leg Stroke Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=17, bg="royalblue", col="royalblue4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )


soap.velVblslap = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = bl.slap.vel, y=vel)) + 
  xlab(label = "Hind Leg Slap Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=17, bg="darkolivegreen1", col="darkolivegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

soap.velVblstroke = ggplot(data=soapwater[soapwater$expt=="soap",], aes(x = bl.stroke.vel, y=vel)) + 
  xlab(label = "Hind Leg Stroke Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=8, shape=17, bg="palegreen", col="palegreen4") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

library(grid) #run at bottom of this script
multiplot(soap.velVflslap, soap.velVflstroke, soap.velVblslap, soap.velVblstroke, cols=2)

















#################
##GGPLOTs by gecko ID
library(ggplot2)

###Head Height
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.slap.vel, y = head.ht, color=geckoID)) + 
  xlab(label = "Front Leg Slap Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=10) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.stroke.vel, y = head.ht, color=geckoID)) + 
  xlab(label = "Front Leg Stroke Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=10, shape=17) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.slap.vel, y = head.ht, color=geckoID)) + 
  xlab(label = "Hind Leg Slap Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=10, shape=1) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.stroke.vel, y = head.ht, color=geckoID)) + 
  xlab(label = "Hind Leg Stroke Velocity (mm/s)") + ylab(label = "Head Height (mm)") + 
  geom_point(size=10, shape=2) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )

###Velocity
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.slap.vel, y = vel, color=geckoID)) + 
  xlab(label = "Front Leg Slap Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=10, shape=15) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = fl.stroke.vel, y = vel, color=geckoID)) + 
  xlab(label = "Front Leg Stroke Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=10, shape=18) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.slap.vel, y = vel, color=geckoID)) + 
  xlab(label = "Hind Leg Slap Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=10, shape=0) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )
ggplot(data=soapwater[soapwater$expt=="water",], aes(x = bl.stroke.vel, color=geckoID)) + 
  xlab(label = "Hind Leg Stroke Velocity (mm/s)") + ylab(label = "Velocity (mm/s)") + 
  geom_point(size=10, shape=5) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.x = element_text(face="bold", colour=, size=16),
    axis.text.x  = element_text(angle=, vjust=, size=14),
    axis.title.y = element_text(face="bold", colour=, size=16),
    axis.text.y  = element_text(angle=, vjust=, size=14),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio=(6/10)
  )





# 
# #running individiual differences
# 
# bartlett.test(fl.height ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(fl.height ~ geckoID, data=soapwater[soapwater$expt=="water",]))
#      
# bartlett.test(bl.height ~ geckoID, data=soapwater[soapwater$expt=="water",])     
# summary(aov(bl.height ~ geckoID, data=soapwater[soapwater$expt=="water",]))
# 
# bartlett.test(fl.slap.vel ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(fl.slap.vel ~ geckoID, data=soapwater[soapwater$expt=="water",]))
# 
# bartlett.test(bl.slap.vel ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(bl.slap.vel ~ geckoID, data=soapwater[soapwater$expt=="water",])) #*
# 
# bartlett.test(fl.stroke.vel ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(fl.stroke.vel ~ geckoID, data=soapwater[soapwater$expt=="water",]))  #*
# 
# bartlett.test(bl.stroke.vel ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(bl.stroke.vel ~ geckoID, data=soapwater[soapwater$expt=="water",])) #*
# 
# bartlett.test(stance ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(stance ~ geckoID, data=soapwater[soapwater$expt=="water",])) #*
# 
# bartlett.test(swing ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(swing ~ geckoID, data=soapwater[soapwater$expt=="water",]))
# 
# bartlett.test(period ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(period ~ geckoID, data=soapwater[soapwater$expt=="water",]))
# 
# bartlett.test(head.ht ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(head.ht ~ geckoID, data=soapwater[soapwater$expt=="water",])) #*
# 
# bartlett.test(vel ~ geckoID, data=soapwater[soapwater$expt=="water",])
# summary(aov(vel ~ geckoID, data=soapwater[soapwater$expt=="water",]))


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
