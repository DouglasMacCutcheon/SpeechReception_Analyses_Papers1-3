 setwd("C:\\Users\\dosman\\Google Drive\\Experiment_Doug_Florian\\DataEvaluation")
# setwd("D:/GoogleDrive/iCARE/Experiment_Doug_Florian/DataEvaluation")
# setwd("C:/Users/fpa/Google Drive/iCARE/Experiment_Doug_Florian/DataEvaluation")
# setwd("C:/Users/maccutcheon/Google Drive/Experiment_Doug_Florian/DataEvaluation")
# setwd("C:\\Users\\currys\\Google Drive\\Experiment_Doug_Florian\\DataEvaluation")


install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("weights")
library(weights)
#install.packages("car")
library(car)
 #install.packages("biotools")
library(biotools)
 #install.packages("psych")
library(psych)
 #install.packages("ez")
library(ez)
 #install.packages("afex")
library(afex)
#install.packages("lattice")
library(lattice)
library(car)
############## PREPARING DATAFILE for Swedish L1 subset  ########################################

# Read .csv
SiN_SwedL1 <- read.csv(file="main_datafile_SPSS_swedL1_only_1.3.2017.csv", header=TRUE,sep=";")
sapply(SiN_SwedL1, class)

# Changes commas to full-stops
for (i in 1:18) {SiN_SwedL1[,i] <- as.numeric(gsub(",", ".", as.character(SiN_SwedL1[,i])))}
sapply(SiN_SwedL1, class)

# SRT_language: 0 = Eng, 1 = Swed; SRT_azimuth: 0 = colocated 1 = spatially separated; SRT_noisetype

# SiN_SwedL1$SRT_language[SiN_SwedL1$SRT_language==0] = "English" 
# SiN_SwedL1$SRT_language[SiN_SwedL1$SRT_language==1] = "Swedish"
language <- SiN_SwedL1$SRT_language

# SiN_SwedL1$SRT_azimuth[SiN_SwedL1$SRT_azimuth==0] = "colocated"
# SiN_SwedL1$SRT_azimuth[SiN_SwedL1$SRT_azimuth==1] = "separated" 
azimuth <- SiN_SwedL1$SRT_azimuth

# SiN_SwedL1$SRT_noisetype[SiN_SwedL1$SRT_noisetype==0] = "SSN"
# SiN_SwedL1$SRT_noisetype[SiN_SwedL1$SRT_noisetype==1] = "babble"
noisetype <- SiN_SwedL1$SRT_noisetype

SRTs <- SiN_SwedL1$SRTs

colnames(SiN_SwedL1)[1] <- "ID"

SiN_SwedL1$First_language = "Swedish" 

SiN_SwedL1$SRT_language <- as.factor(SiN_SwedL1$SRT_language)
levels(SiN_SwedL1$SRT_language) <- c("English","Swedish")

SiN_SwedL1$SRT_noisetype <- as.factor(SiN_SwedL1$SRT_noisetype)
levels(SiN_SwedL1$SRT_noisetype) <- c("SSN","babble")

SiN_SwedL1$SRT_azimuth <- as.factor(SiN_SwedL1$SRT_azimuth)
levels(SiN_SwedL1$SRT_azimuth) <- c("collocated","spatially separated")



################### SRT PLOTS  ########################################################################



# Preparing datafile for plotting main effects, data taken from SPSS output for RM factorial ANOVA,
#  S1=Co-located Target/Distractor, S2=Spatially separated Target/Distractor, 
# N1=Speech-shaped noise, N2=Babble noise, L1=English, L2=Swedish
azimuth <- c("colocated", "spatially_separated")
azimuth_mean <- c(-6.047, -9.862)
#azimuth_SE <- c(.215,.189)
azimuth_CI_low <- c(-6.480, -10.243)
azimuth_CI_upper <- c(-5.614, -9.481)

noisetype <- c("SSN", "babble noise")
noisetype_mean <- c(-9.393, -6.516)
#noisetype_SE <- c(.197, .239)
noisetype_CI_low <- c(-9.790, -6.997)
noisetype_CI_upper <- c(-8.997,-6.034)

language <- c("Swedish", "English")
language_mean <- c(-7.510, -8.399)
#language_SE <- c(.157, .234)
language_CI_low <- c(-7.827, -8.872)
language_CI_upper <- c(-7.193,-7.927)
##################################################aggregate function 

x <- SiN_SwedL1


mainEffect_plotData <- data.frame(azimuth,
                                  azimuth_mean,
                                  azimuth_CI_low,
                                  azimuth_CI_upper,
                                 # azimuth_SE,
                                  noisetype,
                                  noisetype_mean,
                                 noisetype_CI_low,
                                 noisetype_CI_upper,
                                #  noisetype_SE,
                                  language,
                                  language_mean,
                                # language_SE,
                                language_CI_low,
                                language_CI_upper)

colnames(mainEffect_plotData)[1] <- "mean SRT for azimuth conditions"
colnames(mainEffect_plotData)[4] <- "mean SRT for noise type conditions"
colnames(mainEffect_plotData)[7] <- "mean SRT for language conditions"


factors(mainEffect_plotData)
## Barplot main effect 1, language
plot_Lang_mainEffect <- ggplot(data = mainEffect_plotData, 
       aes(x = language, y = language_mean, fill = language)) + 
  stat_summary(fun.y=mean, geom="bar") +
  geom_errorbar(ymin = language_mean - language_SE, ymax = language_mean + language_SE, width = 0.35) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Language") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  ylim(-11, 0) +
  scale_x_discrete(labels=c("English", "Swedish")) +
  ylab("Speech reception threshold (dB)") +
  xlab("") +
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))

print(x)

SiN_SwedL1_maineffectplot <- SiN_SwedL1[c(1,7:18)]
SiN_SwedL1_maineffectplot$main_effect_lang_eng <- sum(SiN_SwedL1_maineffectplot$S1N1 [SiN_SwedL1_maineffectplot$SRT_language=="English"])

ggplot(data = SiN_SwedL1, 
       aes(x = language, y = language_mean, fill = language))  +
  geom_boxplot()





## Barplot main effect 2, Location
plot_Azimuth_mainEffect <- ggplot(data = mainEffect_plotData, 
       aes(x = azimuth, y = azimuth_mean, fill = azimuth)) + 
  stat_summary(fun.y=mean, geom="bar") +
  geom_errorbar(ymin = azimuth_mean - azimuth_SE, ymax = azimuth_mean + azimuth_SE, width = 0.35) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Location") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  ylim(-11, 0) +
  scale_x_discrete(labels=c("collocated","separated")) +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))

## Barplot main effect 3, noise type
plot_Noisetype_mainEffect <- ggplot(data = mainEffect_plotData, 
       aes(x = noisetype, y = noisetype_mean, fill = noisetype)) + 
  stat_summary(fun.y=mean, geom="bar") +
  geom_errorbar(ymin = noisetype_mean - noisetype_SE, ymax = noisetype_mean + noisetype_SE, width = 0.35) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Noise type") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  ylim(-11, 0) +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))

# Put all plots onto a single plot
grid.arrange(plot_Lang_mainEffect,plot_Azimuth_mainEffect,plot_Noisetype_mainEffect, ncol = 3)


# save as eps
setEPS()
grid.arrange(plot_Lang_mainEffect,plot_Azimuth_mainEffect,plot_Noisetype_mainEffect, ncol = 3)
postscript("./plots/main_effects_SRTs.eps")
dev.off()

# save as png
png(filename="./plots/main_effects_SRTs_highres.png", width = 8, height = 8, units = 'in', res = 600)
grid.arrange(plot_Lang_mainEffect,plot_Azimuth_mainEffect,plot_Noisetype_mainEffect, ncol = 3)
dev.off()




# Long format of main effect data for alternative plotting
condition <- c("English", "Swedish", "colocated", "spatially_separated", "SSN", "babble noise")
threshold_mean <- c(language_mean,azimuth_mean,noisetype_mean)
SE_all <- c(language_SE, azimuth_SE, noisetype_SE)

mainEffect_plotData_long <- data.frame(condition, threshold_mean, SE_all)




###################   SIN ANALYSIS   ################################################################

## Make sum of squares the same as SPSS, paste line at top of code
# "to adhere to the sum-to-zero convention for effect weights as in SPSS. If you don't do it, 
# your sum of squares calculations may not match what you get, for example, in SPSS"
# https://gribblelab.wordpress.com/2009/03/09/repeated-measures-anova-using-r/
options(contrasts=c("contr.sum","contr.poly"))

## Summary stats, use by(variable, group, output)
# Install package for stats.desc function output recommended by Field
# 
descriptives <- describe(SiN_SwedL1[seq(1,44),c(6,11,12,13,14,15,16,17,18)])
print(descriptives)
var.ratio <- max(descriptives$sd)^2 / min(descriptives$sd)^2


# FLORIAN: variability in all English conditions seems to be quite a bit higher than Swedish conditions,
# is this relevant and can it help us to explain the main effects in some way? Like, perhaps the 
# English scores are misleading because of this variability?

## Sphericity
#If k = 2 (a repeated measures factor with only two levels) then the sphericity assumption is always 
# met. Using the lower-bound formula one can see that when k = 2 epsilon can't be lower than 
# 1/(k-1) = 1/(2-1) = 1. This is also true for the paired t test (in effect a one-way repeated measures 
# ANOVA where k = 2).

# check data for homogeneity of variances
# leveneTest(SRTs ~ SRT_language*SRT_azimuth*SRT_noisetype, data=SiN_SwedL1)
# bartlett.test(SRTs ~ interaction(SRT_language, SRT_azimuth, SRT_noisetype), data=SiN_SwedL1)
# fligner.test(SRTs ~ interaction(SRT_language, SRT_azimuth, SRT_noisetype), data=SiN_SwedL1)
# boxM(SiN_SwedL1[seq(1,44),seq(11,18)],names(SiN_SwedL1[seq(1,44),seq(11,18)]) ) # needs biotools installed

# # Levene's test
# leveneTest(SRTs ~ SRT_language * SRT_azimuth * SRT_noisetype, data = SiN_SwedL1)
# leveneTest(SRTs_ALL_sqareTransf ~ SRT_language * SRT_azimuth * SRT_noisetype, data = SiN_SwedL1)
# leveneTest(SRTs_ALL_logTransf ~ SRT_language * SRT_azimuth * SRT_noisetype, data = SiN_SwedL1)
# leveneTest(SRTs_ALL_sqrtTransf ~ SRT_language * SRT_azimuth * SRT_noisetype, data = SiN_SwedL1)
# 
# pairwise.t.test(SiN_SwedL1$SRTs, SiN_SwedL1$SRT_condition, p.adj="none", pool.sd=T, var.eq=F)
# t.test(SRTs~SRT_azimuth, data = SiN_SwedL1, var.equal=F)
# t.test(SRTs~SRT_noisetype, data = SiN_SwedL1, var.equal=F)
# t.test(SRTs~SRT_language, data = SiN_SwedL1, var.equal=F)


df <- SiN_SwedL1[seq(1,44),c(seq(11,18))]
desc <- describe(df)
print(desc)
var.ratio <- max(desc$sd)^2 / min(desc$sd)^2

# srt.ratio <- max(SiN_SwedL1$SRTs)  min(SiN_SwedL1$SRTs)

# df.long <- melt(df,df$id)
df.long <- SiN_SwedL1[,c(1,6,7)]
df.long$SRT_condition <- as.factor(c("S1N1L1",
                                     "S2N1L1",
                                     "S2N2L2",
                                     "S1N2L1",
                                     "S1N2L2",
                                     "S2N2L1",
                                     "S2N2L2",
                                     "S1N1L2"))

# ggplot(data = df.long,aes(sample=SRT_condition)) +
#   stat_qq()

#######################################
plots.qq <- list()
plots.hist <- list()
plot.names <- c("C1: S1N1L1", "C2: S2N1L1", "C3: S2N2L2", "C3: S1N1L1",
                "C4: S1N2L1", "C5: S1N2L2", "C6: S2N2L1", "C7: S2N1L2",
                "C8: ")
for (idx in seq(1,8)){
  df.sub <- df[,idx]
  swt <- shapiro.test(df.sub)
  W <- format(rd(round(swt$statistic,digits = 3)))
  p <- format(rd(swt$p.value,digits = 3))
  
  M <- format(rd(round(mean(df.sub),digits=2)))
  SD <- format(rd(round(sd(df.sub),digits=2)))
  V <- format(rd(round(sd(df.sub)^2,digits=2)))
  
  # plots.hist[[idx]] <- ggplot(data = data.frame(SRTs=df.sub)) +
  #  geom_histogram(binwidth = 0.5,aes(x=SRTs,y=..density..),alpha=.5) +
  #  geom_density(aes(x=SRTs,y = ..scaled..),size=1) +
  #  ggtitle(plot.names[idx]) +
  #  theme(plot.title = element_text(hjust = 0.5)) +
  #  annotate("text",label=paste("M = ",M,", SD = ", SD, ", Var = ", V), x=min(df.sub), y=1.5, hjust=0) +
  #  xlab("speech reception thresholds") 
  
  for (idx in c(1,2,3,5,6,7,8))
  {plots.qq[[idx]] <- ggplot(data=as.data.frame(qqnorm( df.sub, plot=F)), mapping=aes(x=x, y=y)) + 
    geom_point() + geom_smooth(method="lm", se=FALSE) +
    labs(x="Standard normal quantiles",y="Quantiles of sample") +
    ggtitle(plot.names[idx]) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text",label=paste("list(italic(W) == '",W,"', italic(p) == '", p,"')"),parse=T, x=0, y=min(df.sub)) +
    ylab("") +
    xlab("")}
  
  for (idx in 4)
  {plots.qq[[idx]] <- ggplot(data=as.data.frame(qqnorm( df.sub, plot=F)), mapping=aes(x=x, y=y)) + 
    geom_point() + geom_smooth(method="lm", se=FALSE) +
    labs(x="Standard normal quantiles",y="Quantiles of sample") +
    ggtitle(plot.names[idx]) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text",label=paste("list(italic(W) == '",W,"', italic(p) == '", p,"')"),parse=T, x=0, y=min(df.sub)) +
    xlab("") }
  
  for (idx in 8)
  {plots.qq[[idx]] <- ggplot(data=as.data.frame(qqnorm( df.sub, plot=F)), mapping=aes(x=x, y=y)) + 
    geom_point() + geom_smooth(method="lm", se=FALSE) +
    labs(x="Standard normal quantiles",y="Quantiles of sample") +
    ggtitle(plot.names[idx]) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text",label=paste("list(italic(W) == '",W,"', italic(p) == '", p,"')"),parse=T, x=0, y=min(df.sub)) +
    ylab("") }
  
 
}

do.call(grid.arrange,plots.qq)
# do.call(grid.arrange,plots.hist)

# Wide format of dataframe
df.wide <- SiN_SwedL1[seq(1,44),seq(1,18)]

# Attempt to bootstrap
# SRT_ANOVA_resampled <- ezBoot(data=df.wide,
#                                  wid=ID,
#                                  within = .(SRT_language, SRT_azimuth, SRT_noisetype),
#
#                                  between = NULL,
#                                  resample_within = T,
#                                  resample_between = F,
#                                  check_args = TRUE
# )
# print(SRT_ANOVA_resampled)


## Normality tests Shap-Wilk
# First, visualise data
# View distribution with histogram: replace XXX with column you want to view
ggplot(data = SiN_SwedL1, 
       aes(x = S1N1)) + 
  geom_histogram() + 
  ggtitle("Normality plot") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5))

# Double-check normality with Qq plot: replace XXX with column you want to view
ggplot(data = SiN_SwedL1, aes(sample = XXX)) +
  stat_qq()




# FLORIAN: Conclusion: Before transform 3 variables non-normal, after square transform only 2 are non-normal
# but after log transform 3 are still non-normal (but a different 3) and after square root transform,
# 4 are non-normal. Only problem is that the square transform means that all values become positive,
# so higher scores represent lower thresholds (therefore, higher = better performance and visa-versa).
# However, the problem as I see it is that the raw scores are already transformed to a log scale 
# (I assume you did this as part of your data extraction script, so we only get the dB values in the 
# datafile but they have been converted from raw scores already somewhere along the line). I still 
# don't think it is proper to double-transform data even if it is still not normally distributed
# after the first log transform.

## Outliers, checking for these with boxplots from car package that label outliers (please note:
#                labels are according to line number, NOT ID number).
library(car)
Boxplot(SiN_SwedL1[,11])
Boxplot(SiN_SwedL1[,12])
Boxplot(SiN_SwedL1[,13])
Boxplot(SiN_SwedL1[,14])
Boxplot(SiN_SwedL1[,15])
Boxplot(SiN_SwedL1[,16])
Boxplot(SiN_SwedL1[,17])
Boxplot(SiN_SwedL1[,18])
# [FLORIAN: Outliers are never the same people, so I assume we can just continue without 
#                         excluding anyone?]


## ANOVA. Design: Factorial repeated measures three-way ANOVA (three within-subjects factors 
#                                   (language, noisetype, azimuth) on one responses value (SRTs))
# ezANOVA according to Field is: 
# newModel<-ezANOVA(data = dataFrame, dv = .(outcome variable), 
#         wid = .(variable that identifies participants), within = .(repeated measures 
#         predictors), between = .(between-group predictors), detailed = FALSE, type = 2)
# Heteroskedastic dat: white.adjust used as our data is not homogenously distributed 
# (Levshina, N. (2015).How to do linguistics with R: Data exploration and statistical analysis. 
# John Benjamins Publishing Company.)

#
SRT_ANOVA <- ezANOVA(data=SiN_SwedL1,
                     dv = SRTs,
                     wid = ID,
                     within = .(SRT_language, SRT_azimuth, SRT_noisetype),
                     detailed = T,
                     return_aov = F
                     )
print(SRT_ANOVA)


# Some alternative ways of running RM factorial ANOVA
# Using anova and lm functions
anova(lm(SiN_SwedL1$SRTs~SiN_SwedL1$SRT_azimuth*SiN_SwedL1$SRT_noisetype*
           SiN_SwedL1$SRT_language)+ Error(SiN_SwedL1$ID / (SiN_SwedL1$SRT_azimuth*SiN_SwedL1$SRT_noisetype*
                                                              SiN_SwedL1$SRT_language)))
# Using Anova from car package
library(car)
my.aov <- aov(SRTs~SRT_azimuth*SRT_noisetype*SRT_language + 
                Error(ID/(SRT_azimuth*SRT_noisetype*SRT_language)),
              data=SiN_SwedL1)
summary(my.aov)

# Using Anova from lm 
model.lm <- lm(SRTs~SRT_azimuth*SRT_noisetype*SRT_language + SRT_condition, 
               data=SiN_SwedL1,na.action=na.omit)
Anova(model.lm,type='II',white.adjust=F,error.df=F)
summary(model.lm)


# Using afex (identical values to SPSS)

SRT_ANOVA_afex <- aov_car(SRTs~SRT_azimuth*SRT_noisetype*SRT_language + 
                            Error(ID / (SRT_azimuth*SRT_noisetype*SRT_language)),
                          data=SiN_SwedL1, fun.aggregate =NULL)

summary(x)

# [FLORIAN: Also significant main effects but we get a significant interaction as well. However,
# not sure about the double transform.]
# Concerning the "ges" (generalized eta squared) effect size output of the ANOVA: 
# Only recently has a generally useful effect size statistic been proposed for such [repeated measures]
# designs: generalized eta squared (??G2; Olejnik & Algina, 2003). Here, we present this method, explain 
# that ??G2 is preferred to eta squared and partial eta squared because it provides comparability 
# across between-subjects and within-subjects designs, show that it can easily be computed from 
# information provided by standard statistical packages, and recommend that investigators provide 
# it routinely in their research reports when appropriate.
# [Source: Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. 
# Behavior research methods, 37(3), 379-384.]



# Swed subgroup main_datafile
write.table(SiN_SwedL1,"main_data_Swed_subgroup_v3.csv", sep=";",row.names = F) 


# Plot adjusted main effects
# Create separate datafile with means for Language, Noisetype and Spatial conditions. 
SiN_SwedL1_wide <- read.csv("mainData_SiN_SwedL1_wide.csv", sep=";", header=T)
SiN_df_mainEffect <- data.frame(rowMeans(SiN_SwedL1_wide[1:44,c(3,4,6,8)],na.rm=F),
                                          rowMeans(SiN_SwedL1_wide[1:44,c(5,7,9,10)],na.rm=F),
                                          rowMeans(SiN_SwedL1_wide[1:44,c(3,4,9,10)],na.rm=F),
                                          rowMeans(SiN_SwedL1_wide[1:44,c(5,6,7,8)],na.rm=F),
                                          rowMeans(SiN_SwedL1_wide[1:44,c(3,6,7,10)],na.rm=F),
                                          rowMeans(SiN_SwedL1_wide[1:44,c(4,5,8,9)],na.rm=F)
                                          )
colnames(SiN_df_mainEffect) <- c("mainEffect_Eng","mainEffect_Swed","mainEffect_ssn","mainEffect_babble",
                                           "mainEffect_coloc", "mainEffect_spatiallySep")
# Apply Field's adjustment
source("rmMeanAdjust_mainEffects_Lang.R")
source("rmMeanAdjust_mainEffects_noise.R")
source("rmMeanAdjust_mainEffects_spatial.R")
mainEffect_lang_adj <- rmMeanAdjust_mainEffects_Lang(SiN_df_mainEffect)
mainEffect_noise_adj <- rmMeanAdjust_mainEffects_noise(SiN_df_mainEffect)
mainEffect_spatial_adj <- rmMeanAdjust_mainEffects_spatial(SiN_df_mainEffect)

SiN_df_mainEffect_adj_wide <- data.frame(mainEffect_lang_adj,mainEffect_noise_adj,mainEffect_spatial_adj)

# Save wide main effects adjusted for plotting data frame
write.table(SiN_df_mainEffect_adj_wide,"mainEffectsAdjForPlotting_wide.csv",sep=";",row.names=F)

# Create long version of data
group <- NA
SiN_df_mainEffect_adj_long <- data.frame(group,c(rowMeans(SiN_SwedL1_wide[1:44,c(3,4,6,8)],na.rm=F),
                                  rowMeans(SiN_SwedL1_wide[1:44,c(5,7,9,10)],na.rm=F)),
                                c(rowMeans(SiN_SwedL1_wide[1:44,c(3,4,9,10)],na.rm=F),
                                  rowMeans(SiN_SwedL1_wide[1:44,c(5,6,7,8)],na.rm=F)),
                                c(rowMeans(SiN_SwedL1_wide[1:44,c(3,6,7,10)],na.rm=F),
                                  rowMeans(SiN_SwedL1_wide[1:44,c(4,5,8,9)],na.rm=F))
                                )
# name columns
colnames(SiN_df_mainEffect_adj_long) <- c("group","SRTs_Language_adj","SRTs_Noise_adj","SRTs_Spatial_adj")
# Label groups
SiN_df_mainEffect_adj_long$group[1:44] <- 1
SiN_df_mainEffect_adj_long$group[45:88] <- 2

# Save main effects adjusted for plotting data frame
write.table(SiN_df_mainEffect_adj_long,"mainEffectsAdjForPlotting_long.csv",sep=";",row.names=F)

#

# Conclusion regarding adjustment: boxplots are misleading because they change the mean values
# which were the output of the ANOVA. Stick with original data for plotting and if reviewers 
# complain then we have the adjusted data.


## Plot 3-Way ANOVA main effects
LangMainEffect_plot <- ggplot(data=SiN_SwedL1, aes(factor(SRT_language),SRTs, fill=SRT_language)) +
  geom_boxplot(notch=T) +
  ylim(-17,5) +
  ylab("Speech in noise threshold") +
  xlab("Language factor levels") +
  ggtitle("Main effect: language") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank()) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE)

ggsave(filename="SRT_LanguageMainEffect_plot.png", plot=LangMainEffect_plot, path = ".\\plots")
ggsave(filename="SRT_LanguageMainEffect_plot.eps", plot=LangMainEffect_plot, path = ".\\plots")

  
NoisetypeMainEffect_plot <- ggplot(data=SiN_SwedL1, aes(factor(SRT_noisetype),SRTs, fill=SRT_noisetype)) +
  geom_boxplot(notch=T) +
  ylim(-17,5) +
  ylab("Speech in noise threshold") +
  xlab("Noisetype factor levels") +
  ggtitle("Main effect: Noise type") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank()) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE)

ggsave(filename="SRT_NoisetypeMainEffect_plot.png", plot=NoisetypeMainEffect_plot, path = ".\\plots")
ggsave(filename="SRT_NoisetypeMainEffect_plot.eps", plot=NoisetypeMainEffect_plot, path = ".\\plots")


AzimuthMainEffect_plot <- ggplot(data=SiN_SwedL1, aes(factor(SRT_azimuth),SRTs, fill=SRT_azimuth)) +
  geom_boxplot(notch=T) +
  ylim(-17,5) +
  ylab("Speech in noise threshold") +
  xlab("Azimuth factor levels") +
  ggtitle("Main effect: Azimuth") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank()) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE)

ggsave(filename="SRT_AzimuthMainEffect_plot.png", plot=AzimuthMainEffect_plot, path = ".\\plots")
ggsave(filename="SRT_AzimuthMainEffect_plot.eps", plot=AzimuthMainEffect_plot, path = ".\\plots")



## Checking outliers on boxplots are not always the same 2 people:
library(car)
Boxplot(SiN_SwedL1$SRTs[SiN_SwedL1$SRT_language=="English"])
Boxplot(SiN_SwedL1$SRTs[SiN_SwedL1$SRT_language=="Swedish"])
Boxplot(SiN_SwedL1$SRTs[SiN_SwedL1$SRT_azimuth=="colocated"])
Boxplot(SiN_SwedL1$SRTs[SiN_SwedL1$SRT_azimuth=="spatially separated"])
Boxplot(SiN_SwedL1$SRTs[SiN_SwedL1$SRT_noisetype=="SSN"])
Boxplot(SiN_SwedL1$SRTs[SiN_SwedL1$SRT_noisetype=="babble"])
# Conclusion: They are always different people except #56 in colocated and babble conditions, 
# very high threshold in both



##########################################################################################################3
# Plot 2-Way ANOVA results (analysis done in SPSS)

SiN_df_mainEffect_SwedOnly <- data.frame(SiN_SwedL1$ID[SiN_SwedL1$SRT_language=="Swedish"], 
                                       SiN_SwedL1$SRTs[SiN_SwedL1$SRT_language=="Swedish"],
                                       SiN_SwedL1$SRT_azimuth[SiN_SwedL1$SRT_language=="Swedish"],
                                       SiN_SwedL1$SRT_noisetype[SiN_SwedL1$SRT_language=="Swedish"])
colnames(SiN_df_mainEffect_SwedOnly)[c(1,2,3,4)] <- c("ID", "SRTs", "azimuth", "noisetype")


NoiseMainEffect_plot <- ggplot(data = SiN_df_mainEffect_SwedOnly, 
                        aes(y = SRTs, x = noisetype, fill = noisetype)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width = 0.35) + 
  stat_summary(fun.y=mean, geom="bar") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Noise-type") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  ylab("Speech reception thresholds (dB)") +
  xlab("")  +
  coord_cartesian(ylim = c(0, -10))

SpatialMainEffect_plot <- ggplot(data = SiN_df_mainEffect_SwedOnly, 
                               aes(y = SRTs, x = azimuth, fill = azimuth)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width = 0.35) + 
  stat_summary(fun.y=mean, geom="bar") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Location") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  ylab("") +
  xlab("") +
  coord_cartesian(ylim = c(0, -10))

# Put all plots onto a single plot
RM_plot_combined <- grid.arrange(NoiseMainEffect_plot,SpatialMainEffect_plot, ncol = 2)
RM_plot_combined

# save as eps
setEPS()
grid.arrange(NoiseMainEffect_plot,SpatialMainEffect_plot, ncol = 2)
postscript("./plots/MainEffects_L1only.eps")
dev.off()

# save as png
png(filename="./plots/MainEffects_L1only.png")
grid.arrange(NoiseMainEffect_plot,SpatialMainEffect_plot, ncol = 2)
dev.off()

############################# RELEASE FROM MASKING DATAFRAME #########################################################

RM_noise_L1 <- SiN_SwedL1[1:44,14] - SiN_SwedL1[1:44,11]
RM_noise_L2 <- SiN_SwedL1[1:44,15] - SiN_SwedL1[1:44,18]
RM_spatial_L1 <- SiN_SwedL1[1:44,14] - SiN_SwedL1[1:44,16]
RM_spatial_L2 <- SiN_SwedL1[1:44,15] - SiN_SwedL1[1:44,13]
RM_total_L1 <- SiN_SwedL1[1:44,14] - SiN_SwedL1[1:44,12]
RM_total_L2 <- SiN_SwedL1[1:44,15] - SiN_SwedL1[1:44,17]

SiN_RM_SwedL1_wide <- data.frame(SiN_SwedL1[1:44,c(1,11:18)],RM_noise_L1, RM_noise_L2, 
                                 RM_spatial_L1, RM_spatial_L2, RM_total_L1, RM_total_L2)

write.table(SiN_RM_SwedL1_wide,"ReleaseFM_SwedL1_wide.csv", sep=";", row.names = F)

## Table: Mean SRTs (dB SNR) for speech reception and RM/SRM in noise for the noisetype and spatial location conditions 
# in bilingual children's first VS. second language
# Table structure (based on Ching, T. Y. C., van Wanrooy, E., Dillon, H., & Carter, L. (2011). Spatial release from masking 
# in normal-hearing children and children who use hearing aids. The Journal of the Acoustical Society of America, 129(1), 
# 368-375. http://doi.org/10.1121/1.3523295): 
# Noise in L1 vs L2 means, SD, range and RM
# L1 (n=44)   Mean: S1N2L1/SiN_SwedL1[,14]  S1N1L1/SiN_SwedL1[,11]    NRM/RM_noise_L1
#             SD:
#             Range:
# L2 (n=44)   Mean: S1N2L2/SiN_SwedL1[,15]  S1N1L2/SiN_SwedL1[,18]    NRM/RM_noise_L2
#             SD:
#             Range:
# Spatial in L1 vs L2 means, SD, range and RM
# L1 (n=44)   S1N2L1/SiN_SwedL1[,14]  S2N2L1/SiN_SwedL1[,16]    SRM/RM_spatial_L1
#             SD:
#             Range:
# L2 (n=44)   S1N2L2/SiN_SwedL1[,15]  S2N2L2/SiN_SwedL1[,13]    SRM/RM_spatial_L2
#             SD:
#             Range:
# Language means, SD, range and RM
# S1N2L1/SiN_SwedL1[,14]  S1N2L2/SiN_SwedL1[,15]    LRM/RM_language
#       SD:
#       Range:

# Descriptive data for the table 
describe(SiN_SwedL1[1:44,11])
describe(SiN_SwedL1[1:44,12])
describe(SiN_SwedL1[1:44,13])
describe(SiN_SwedL1[1:44,14])
describe(SiN_SwedL1[1:44,15])
describe(SiN_SwedL1[1:44,16])
describe(SiN_SwedL1[1:44,17])
describe(SiN_SwedL1[1:44,18])

desc_noise_L1 <- describe(SiN_RM_SwedL1_wide$RM_noise_L1)
desc_noise_L2 <- describe(SiN_RM_SwedL1_wide$RM_noise_L2)
desc_spatial_L1 <- describe(SiN_RM_SwedL1_wide$RM_spatial_L1)
desc_spatial_L2 <- describe(SiN_RM_SwedL1_wide$RM_spatial_L2)
desc_total_L1 <- describe(SiN_RM_SwedL1_wide$RM_total_L1)
desc_total_L2 <- describe(SiN_RM_SwedL1_wide$RM_total_L2)

## Barplot noise RM advantage

RM_language <- NA
SiN_RM_long <- data.frame(c(SiN_SwedL1[1:44,1],SiN_SwedL1[1:44,1]),c(RM_noise_L1, RM_noise_L2), 
                          c(RM_spatial_L1, RM_spatial_L2), 
                          c(RM_total_L1, RM_total_L2), RM_language)
colnames(SiN_RM_long)[1] <- "ID"
colnames(SiN_RM_long)[2] <- "RM_noise"
colnames(SiN_RM_long)[3] <- "RM_spatial"
colnames(SiN_RM_long)[4] <- "RM_total"

SiN_RM_long$RM_language[1:44] <- "English"
SiN_RM_long$RM_language[45:88] <- "Swedish"

SiN_RM_long$RM_language <- factor(SiN_RM_long$RM_language, levels = c("Swedish", "English"))
x.text <- element_text(size = 16)

plot_RM_noise <- ggplot(data = SiN_RM_long, 
                        aes(y = RM_noise, x = RM_language, fill = RM_language)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width = 0.35) + 
  stat_summary(fun.y=mean, geom="bar") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 15)) + 
  ggtitle("Noise type \n advantage") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=15)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  scale_x_discrete(labels=c("Swedish","English")) +
  ylab("Release from masking (dB)") +
  theme(axis.text.x = element_text(size = 15))+
  xlab("") +
  coord_cartesian(ylim = c(0, 5))

## Barplot spatial RM advantage
plot_RM_spatial <- ggplot(data = SiN_RM_long, 
                          aes(y = RM_spatial, x = RM_language, fill = RM_language)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width = 0.35) + 
  stat_summary(fun.y=mean, geom="bar") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Spatial \n advantage") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=15)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  scale_x_discrete(labels=c("Swedish","English")) +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(size = 15))+
  coord_cartesian(ylim = c(0, 5))


## Barplot total RM advantage
plot_RM_total <- ggplot(data = SiN_RM_long, 
                        aes(y = RM_total, x = RM_language, fill = RM_language)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width = 0.35) + 
  stat_summary(fun.y=mean, geom="bar") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle("Total \n advantage") + 
  theme(plot.title = element_text(lineheight=.8)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=15)) +
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  scale_x_discrete(labels=c("Swedish","English")) +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(size = 15))+
  coord_cartesian(ylim = c(0, 8))

# Put all plots onto a single plot
RM_plot_combined_L1only <- grid.arrange(plot_RM_noise,plot_RM_spatial, ncol = 2)
RM_plot_combined_L1only

# save as eps
setEPS()
grid.arrange(plot_RM_noise,plot_RM_spatial, ncol = 2)
postscript("./plots/RM_plot_combined.eps")
dev.off()

# save as png
png(filename="./plots/RM_plot_combined.png")
grid.arrange(plot_RM_noise,plot_RM_spatial, ncol = 2)
dev.off()


################# RM ANALYSIS

leveneTest(RM_noise ~ RM_language, data=SiN_RM_long)
leveneTest(RM_spatial ~ RM_language, data=SiN_RM_long)
leveneTest(RM_total ~ RM_language, data=SiN_RM_long)

shapiro.test(SiN_RM_SwedL1_wide$RM_noise_L1)
shapiro.test(SiN_RM_SwedL1_wide$RM_noise_L2)
shapiro.test(SiN_RM_SwedL1_wide$RM_spatial_L1)
shapiro.test(SiN_RM_SwedL1_wide$RM_spatial_L2)
shapiro.test(SiN_RM_SwedL1_wide$RM_total_L1)
shapiro.test(SiN_RM_SwedL1_wide$RM_total_L2)

hist(SiN_RM_SwedL1_wide$RM_noise_L1)
hist(SiN_RM_SwedL1_wide$RM_noise_L2)
hist(SiN_RM_SwedL1_wide$RM_spatial_L1)
hist(SiN_RM_SwedL1_wide$RM_spatial_L2)
hist(SiN_RM_SwedL1_wide$RM_total_L1)
hist(SiN_RM_SwedL1_wide$RM_total_L2)

# Check RM is significantly different from zero
t.test(data=SiN_RM_SwedL1_wide, RM_noise_L1, mu=0)
t.test(data=SiN_RM_SwedL1_wide, RM_noise_L2, mu=0)
t.test(data=SiN_RM_SwedL1_wide, RM_spatial_L1, mu=0)
t.test(data=SiN_RM_SwedL1_wide, RM_spatial_L2, mu=0)
t.test(data=SiN_RM_SwedL1_wide, RM_total_L1, mu=0)
t.test(data=SiN_RM_SwedL1_wide, RM_total_L2, mu=0)

t.test(data=SiN_RM_long, SiN_RM_long$RM_total, mu=0)
t.test(data=SiN_RM_long, SiN_RM_long$RM_noise, mu=0)
t.test(data=SiN_RM_long, SiN_RM_long$RM_spatial, mu=0) 

# Check L1/L2 differences between RM conditions
t.test(data=SiN_RM_SwedL1_wide, RM_noise_L1, RM_noise_L2, alternative=c("two.sided"),paired=TRUE)
t.test(data=SiN_RM_SwedL1_wide, RM_spatial_L1, RM_spatial_L2, alternative=c("two.sided"),paired=TRUE)
t.test(data=SiN_RM_SwedL1_wide, RM_total_L1, RM_total_L2, alternative=c("two.sided"),paired=TRUE)

describe(SiN_RM_SwedL1_wide$RM_noise_L1)
describe(SiN_RM_SwedL1_wide$RM_noise_L2)


t.test(data=SiN_RM_SwedL1_wide, RM_noise_L1, RM_noise_L2, alternative=c("two.sided"),paired=TRUE)







###################################################################################################
### Plot linear regression between age of L2 acquisition and L2 SRM

RM_andSRTs_wide_WithDVs <- read.csv("RM_andSRTs_wide_WithDVs.csv",sep=",", header = T)
# Remove rows with NAs
RM_andSRTs_wide_WithDVs <- data.frame(RM_andSRTs_wide_WithDVs[c(1:11,13:33,35:43),])

# Checking assumptions
RM_regression_L2.lm = lm(RM_spatial_L2 ~ age_2nd_lang, data=RM_andSRTs_wide_WithDVs) 
RM_regression_L2.stdres = rstandard(RM_regression_L2.lm)
# Normality
qqnorm(RM_regression_L2.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(RM_regression_L2.stdres)
shapiro.test(RM_regression_L2.stdres) # Conclusion: residuals are normally distributed
# Heteroscedasticity
ncvTest(RM_regression_L2.lm) # Conclusion: homogenous
# Nonlinearity
crPlots(RM_regression_L2.lm) # Conclusion: linear


# Plot
RM_regression_L2_plot <- ggplot(data=RM_andSRTs_wide_WithDVs, 
                     aes(x=RM_spatial_L2, y=age_2nd_lang)) + 
  geom_point() +    
  geom_smooth(method=lm, colour = "black")  +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_fill_grey(start = .8, end = .9) +
  guides(fill=FALSE) +
  xlab("L2 spatial release from masking (dB)") +
  ylab("Age of L2 acquisition") 

# save as eps
setEPS()
RM_regression_L2_plot
postscript("./plots/RM_regression_L2_plot.eps")
dev.off()

# save as png
png(filename="./plots/RM_regression_L2_plot.png")
RM_regression_L2_plot
dev.off()






###################################################################################################
### Plot linear regression between age of L2 acquisition and L2 SRM

# Aggregate spatially separated L2 conditions
RM_andSRTs_wide_WithDVs$aggreg_L2_spatSepCond <- RM_andSRTs_wide_WithDVs[,7] + RM_andSRTs_wide_WithDVs[,11] / 2

# Check assumptions
BNT_SRT_regression_L2.lm = lm(aggreg_L2_spatSepCond ~ BNT_Eng, data=RM_andSRTs_wide_WithDVs) 
BNT_SRT_regression_L2.stdres = rstandard(BNT_SRT_regression_L2.lm)

qqnorm(BNT_SRT_regression_L2.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(BNT_SRT_regression_L2.stdres)
shapiro.test(BNT_SRT_regression_L2.stdres) # Conclusion: residuals are normally distributed
# Heteroscedasticity
ncvTest(BNT_SRT_regression_L2.lm) # Conclusion: homogenous
# Nonlinearity
crPlots(BNT_SRT_regression_L2.lm) # Conclusion: linear

#Plot
SRM_BNT_regression_L2_plot <- ggplot(data=RM_andSRTs_wide_WithDVs, 
                                aes(x=aggreg_L2_spatSepCond, y=BNT_Eng)) + 
  geom_point() +    
  geom_smooth(method=lm, colour = "black")  +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_fill_grey(start = .3, end = .9) +
  guides(fill=FALSE) +
  xlab("L2 spatially separated SRTs (dB)") +
  ylab("English Boston Naming Test scores") 

# save as eps
setEPS()
SRM_BNT_regression_L2_plot
postscript("./plots/SRM_BNT_regression_L2_plot.eps")
dev.off()

# save as png
png(filename="./plots/SRM_BNT_regression_L2_plot.png")
SRM_BNT_regression_L2_plot
dev.off()


#### PLOT MULTIPLE REGRESSION SRTS, WM AND AGE OF L2 ACQUISITION

install.packages("car")
library(car)
install.packages("corrplot")
library(corrplot) # We'll use corrplot later on in this example too.
install.packages("visreg")
library(visreg) # This library will allow us to show multivariate graphs.
install.packages("rgl")
library(rgl)
install.packages("knitr")
library(knitr)
install.packages("scatterplot3d")
library(scatterplot3d)

Analysis2_datafile <- read.csv("Analysis2_wide_14.8.2017.csv",sep=",", header = T)
# Remove rows with NAs
Analysis2_datafile <- data.frame(Analysis2_datafile[c(1:11,13:33, 35:43),c(8,72)])

abline(plot(Analysis2_datafile, pch=16, col="blue", main="Matrix Scatterplot of SRTs, WM and age of L2 acquisition"))

mod1 = lm(SNRs_avg_ALL ~ WMC, data=Analysis2_datafile)
summary(mod1)

abline(mod1)

library(ggplot2)

regression_WM_SRTs_plot <- ggplot(Analysis2_datafile, aes(x = Analysis2_datafile[,2], y = Analysis2_datafile[,1])) + 
  xlab("Speech reception thresholds in dB") +
  ylab("Working memory standard scores") +
  geom_point() +    
  geom_smooth(method=lm, col = "black", se=FALSE)  +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)) + 
  scale_fill_grey(start = .3, end = .9) + 
  annotate("text", x = -6.5, y = 9, label = "italic(r) == .48", parse = TRUE, size = 7)
  
  ggsave(file=file.path("C:\\Users\\dosman\\Google Drive\\Experiment_Doug_Florian\\DataEvaluation\\plots",
                        'regression_WM_SRTs_plot.png'),pointsize = 26,bg = "transparent",dpi=800,
                 width = 10, height = 10)


regression_WM_SRTs_plot <- regression_WM_SRTs_plot + 
  annotate("text", x = -6.5, y = 9, label = "italic(r) == .48", parse = TRUE, size = 7)
  
  axis.title.y = # save as eps
  setEPS()
  regression_WM_SRTs_plot
  postscript("./plots/regression_WM_SRTs_plot.eps")
  dev.off()
  
  # save as png
  png(filename="./plots/regression_WM_SRTs_plot.png")
  regression_WM_SRTs_plot
  dev.off()

  
  ####### PLOT BOTH l1 srts and L2 srts on the same plot separately
  
  Analysis2_datafile <- read.csv("Analysis2_wide_14.8.2017.csv",sep=",", header = T)
  # Remove rows with NAs
  Analysis2_datafile <- data.frame(Analysis2_datafile[c(1:11,13:33, 35:43),c(8,60:61)])
  
  
  library(ggplot2)
  
  # Create new dataframe for the Plot
  Plot1plus2 <- data.frame()
  group <- NA
  Plot1 <- data.frame(Analysis2_datafile[,1],Analysis2_datafile[,2],group)
  Plot2 <- data.frame(Analysis2_datafile[,1],Analysis2_datafile[,3],group)
  colnames(Plot1)[1] <- "WMC"
  colnames(Plot1)[2] <- "SRTs"
  colnames(Plot2)[1] <- "WMC"
  colnames(Plot2)[2] <- "SRTs"
  
  Plot1$group <- 1
  Plot2$group <- 2
  
  Plot1plus2 <- rbind(Plot1, Plot2)
  
  sapply(Plot1plus2, class)
  Plot1plus2$group <- as.factor(Plot1plus2$group)
  levels(Plot1plus2$group) <- c("English","Swedish")
  

  
  regression_WM_SRTs_plot_both <- ggplot(Plot1plus2, aes(x = WMC, y = SRTs, group=group, col=group, 
                                                         fill=group)) + 
    xlab("WM standard scores") +
    ylab("SRTs (dB SNR)") +
    geom_point(aes(shape=group),size=4, colour="black") +    
    scale_shape_manual(values=c(4, 1))+
    geom_smooth(method=lm, se=FALSE, aes(linetype=group))  +
    theme_bw() +
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size=18),
          axis.title.x = element_text(size=18),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16)) + 
    scale_fill_grey(start = .3, end = .9) + 
    annotate("text", x = 10, y = -7.6, label = "italic(r) == .30", parse = TRUE, size = 7) + 
    annotate("text", x = 9.9, y = -9, label = "italic(r) == .46", parse = TRUE, size = 7) + 
    scale_color_manual(values=c("black", "black")) + 
    theme(legend.text=element_text(size=18)) +
    theme(legend.title=element_blank())
  
 # png(filename="C:/Users/currys/Google Drive/Experiment_Doug_Florian/Paper/Scandinavian_Journal_of_Psychology/Revision1/Plots highres 2.1.2018/regression_WM_SRTs_plot_both_highres_13.3.2018.png", 
      width = 8, height = 8, units = 'in', res = 600)
#  regression_WM_SRTs_plot_both
 # dev.off()
  
  png(filename="./plots/regression_WM_SRTs_plot_both_highres_13.3.2018.png", width = 8, height = 8, units = 'in', res = 600, 
      width = 8, height = 8, units = 'in', res = 600)
  regression_WM_SRTs_plot_both
  dev.off()
  
  
  ################### SRT PLOTS REMADE WITH CIs INSTEAD OF SE for whiskers  ########################################################################
  
  
  
  # Preparing datafile for plotting main effects, data taken from SPSS output for RM factorial ANOVA,
  #  S1=Co-located Target/Distractor, S2=Spatially separated Target/Distractor, 
  # N1=Speech-shaped noise, N2=Babble noise, L1=English, L2=Swedish
  azimuth <- c("colocated", "spatially_separated")
  azimuth_mean <- c(-6.047, -9.862)
  #azimuth_SE <- c(.215,.189)
  azimuth_CI_low <- c(-6.480, -10.243)
  azimuth_CI_upper <- c(-5.614, -9.481)
  
  noisetype <- c("SSN", "babble noise")
  noisetype_mean <- c(-9.393, -6.516)
  #noisetype_SE <- c(.197, .239)
  noisetype_CI_low <- c(-9.790, -6.997)
  noisetype_CI_upper <- c(-8.997,-6.034)
  
  language <- c("Swedish", "English")
  language_mean <- c(-7.510, -8.399)
  #language_SE <- c(.157, .234)
  language_CI_low <- c(-7.827, -8.872)
  language_CI_upper <- c(-7.193,-7.927)
  ##################################################aggregate function 
  
  x <- SiN_SwedL1

  mainEffect_plotData <- data.frame(azimuth,azimuth_mean,azimuth_CI_low, azimuth_CI_upper, 
                                    noisetype,noisetype_mean,noisetype_CI_low, noisetype_CI_upper,
                                    language,language_mean,language_CI_low,language_CI_upper )
  
  colnames(mainEffect_plotData)[1] <- "mean SRT for azimuth conditions"
  colnames(mainEffect_plotData)[4] <- "mean SRT for noise type conditions"
  colnames(mainEffect_plotData)[7] <- "mean SRT for language conditions"
  
  factors(mainEffect_plotData)
  ## Barplot main effect 1, language
  plot_Lang_mainEffect <- ggplot(data = mainEffect_plotData, 
                                 aes(x = language, y = language_mean, fill = language)) + 
    stat_summary(fun.y=mean, geom="bar") +
    geom_errorbar(ymin = language_CI_low, ymax = language_CI_upper, width = 0.35) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"),
          axis.title.x = element_text(colour = "black"),
          axis.text.x = element_text(colour = "black")) + 
    ggtitle("Language") + 
    theme(plot.title = element_text(lineheight=.8, face="italic", hjust = 0.5)) +
    scale_fill_grey(start = .3, end = .9) +
    guides(fill=FALSE) +
    ylim(-11, 0) +
    scale_x_discrete(labels=c("English", "Swedish")) +
    ylab("SRTs (dB SNR)") +
    xlab("") +
    theme(axis.text.x = element_text(size = 15))+
    theme(axis.title.x = element_text(size = 17))+
    theme(axis.title.y = element_text(size = 20))
  
  
  ## Barplot main effect 2, Location
  plot_Azimuth_mainEffect <- ggplot(data = mainEffect_plotData, 
                                    aes(x = azimuth, y = azimuth_mean, fill = azimuth)) + 
    stat_summary(fun.y=mean, geom="bar") +
    geom_errorbar(ymin = azimuth_CI_low, ymax = azimuth_CI_upper, width = 0.35) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"),
          axis.title.x = element_text(colour = "black"),
          axis.text.x = element_text(colour = "black")) + 
    ggtitle("Location") + 
    theme(plot.title = element_text(lineheight=.8, face="italic", hjust = 0.5)) +
    scale_fill_grey(start = .3, end = .9) +
    guides(fill=FALSE) +
    ylim(-11, 0) +
    scale_x_discrete(labels=c("Collocated  ","  Separated")) +
    ylab("") +
    xlab("") +
    theme(axis.text.x = element_text(size = 15))+
    theme(axis.title.x = element_text(size = 17))+
    theme(axis.title.y = element_text(size = 15))
  
  ## Barplot main effect 3, noise type
  plot_Noisetype_mainEffect <- ggplot(data = mainEffect_plotData, 
                                      aes(x = noisetype, y = noisetype_mean, fill = noisetype)) + 
    stat_summary(fun.y=mean, geom="bar") +
    geom_errorbar(ymin = noisetype_CI_low, ymax = noisetype_CI_upper, width = 0.35) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black"),
          axis.title.x = element_text(colour = "black"),
          axis.text.x = element_text(colour = "black")) + 
    ggtitle("Noise type") + 
    theme(plot.title = element_text(lineheight=.8, face="italic", hjust = 0.5)) + 
    scale_fill_grey(start = .3, end = .9) +
    scale_x_discrete(labels=c("MTBN","SSN")) +
    guides(fill=FALSE) +
    ylim(-11, 0) +
    ylab("") +
    xlab("") +
    theme(axis.text.x = element_text(size = 15))+
    theme(axis.title.x = element_text(size = 17))+
    theme(axis.title.y = element_text(size = 15)) 
  
  # Put all plots onto a single plot
  grid.arrange(plot_Lang_mainEffect,plot_Azimuth_mainEffect,plot_Noisetype_mainEffect, ncol = 3)
  
  
  # save as eps
  setEPS()
  grid.arrange(plot_Lang_mainEffect,plot_Azimuth_mainEffect,plot_Noisetype_mainEffect, ncol = 3)
  postscript("./plots/main_effects_SRTs.eps")
  dev.off()
  
  # save as png
  png(filename="./plots/main_effects_SRTs_highres_SEs_13.3.2018.png", width = 8, height = 8, units = 'in', res = 600)
  grid.arrange(plot_Lang_mainEffect,plot_Azimuth_mainEffect,plot_Noisetype_mainEffect, ncol = 3)
  dev.off()
  
  
  
  
  # Long format of main effect data for alternative plotting
  condition <- c("English", "Swedish", "colocated", "spatially_separated", "SSN", "babble noise")
  threshold_mean <- c(language_mean,azimuth_mean,noisetype_mean)
  SE_all <- c(language_SE, azimuth_SE, noisetype_SE)
  
  mainEffect_plotData_long <- data.frame(condition, threshold_mean, SE_all)
  
  
  
  
