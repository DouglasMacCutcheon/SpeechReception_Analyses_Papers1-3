setwd("C:\\Users\\dosman\\Dropbox\\1. GAVLE\\1. ARTICLES\\1. My PHD papers\\3. Paper 3 Monolinguals SRTs\\Data")
install.packages("ggplot2")
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)
install.packages("tidyverse")
library(tidyverse)
install.packages("gridExtra")
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)
install.packages("kableExtra")
library(kableExtra)
install.packages('ReporteRs') 
library('ReporteRs')
install.packages('car') 
library('car')
install.packages('cowplot')
library('cowplot')
install.packages('gridExtra')
library('gridExtra')
install.packages('tidyverse')
library('tidyverse')
install.packages('egg')
library('egg')

# Read .csv
wide_data <- read.csv(file="MainData_MusicStudy_CrossSect_9_6_2018.csv", header=TRUE,sep=";")
# Remove unnecessary variables
wide_data <- wide_data[1:39,c(1,23,24,25,26,30,32,8,9)]
# Change commas into fullstops
for (i in 1:39) {wide_data[,i] <- as.numeric(gsub(",", ".", as.character(wide_data[,i])))}
# Change variable classes
sapply(wide_data, class)
wide_data[,'Memory_medSpl_2'] <- as.factor(wide_data[,'Memory_medSpl_2'])
wide_data[,'Lang_medSpl_2'] <- as.factor(wide_data[,'Lang_medSpl_2'])
wide_data[,'FDS_medianSplit'] <- as.factor(wide_data[,'FDS_medianSplit'])
wide_data[,'BDS_medianSplit'] <- as.factor(wide_data[,'BDS_medianSplit'])
sapply(wide_data, class)

# prepare long format version
long_data <- data.frame(matrix(ncol = 12, nrow = 156))
# Name columns
colnames(long_data)[1] <- "sin"
colnames(long_data)[2] <- "condition" # 1=SiN_S1N1, 2=SiN_S3N1, 3=SiN_S1N2, 4=SiN_S3N2
colnames(long_data)[3] <- "cog_split"
colnames(long_data)[4] <- "lang_split"
colnames(long_data)[5] <- "spatial_cond"
colnames(long_data)[6] <- "noise_cond"
colnames(long_data)[7] <- "WM"
colnames(long_data)[8] <- "Language"
colnames(long_data)[9] <- "fds_split"
colnames(long_data)[10] <- "bds_split"
colnames(long_data)[11] <- "fds_group"
colnames(long_data)[12] <- "bds_group"
colnames(long_data)[13] <- "ID"


# Sin variable
long_data[,1] <- c(wide_data$SiN_S1N1,
                   wide_data$SiN_S3N1,
                   wide_data$SiN_S1N2,
                   wide_data$SiN_S3N2)
# Condition variable
long_data[1:39,2] <- 'S1N1'
long_data[40:78,2] <- 'S2N1'
long_data[79:117,2] <- 'S1N2'
long_data[118:156,2] <- 'S2N2'
# cog split variable
long_data[,3] <- wide_data$Memory_medSpl_2[]
factor(long_data[,3],labels = c("Low MS", "High MS"))
long_data[,7][long_data[,3] == 0] <- "Low MS"
long_data[,7][long_data[,3] == 1] <- "High MS"
#long_data[,3] <- long_data[,7]
#long_data <- long_data[,-7]
# lang split variable
long_data[,4] <- wide_data$Lang_medSpl_2[]
factor(long_data[,4],labels = c("Low EL", "High EL"))
long_data[,8][long_data[,4] == 0] <- "Low EL"
long_data[,8][long_data[,4] == 1] <- "High EL"
# spatial condition
long_data[1:39,5] <- '0'
long_data[40:78,5] <- '90'
long_data[79:117,5] <- '0'
long_data[118:156,5] <- '90'
# noise condition
long_data[1:39,6] <- 'SSN'
long_data[40:78,6] <- 'SSN'
long_data[79:117,6] <- 'Speech'
long_data[118:156,6] <- 'Speech'
# change classes for new variables
sapply(long_data, class)
long_data[,'spatial_cond'] <- as.factor(long_data[,'spatial_cond'])
long_data[,'noise_cond'] <- as.factor(long_data[,'noise_cond'])

# FDS
long_data[,9] <- wide_data$FDS_medianSplit
#Name groups for easy plotting
long_data[,11][long_data[,9] == 0] <- "Low FDS"
long_data[,11][long_data[,9] == 1] <- "High FDS"
# BDS
long_data[,10] <- wide_data$BDS_medianSplit
#Name groups for easy plotting
long_data[,12][long_data[,10] == 0] <- "Low BDS"
long_data[,12][long_data[,10] == 1] <- "High BDS"
sapply(long_data, class)
long_data[,13] <- wide_data[,1]



######################## INTERACTION PLOTS #############################################################


############ Spatial*Noise###############
pd <- position_dodge(width = 0.2)
spatial_noise_interaction <- ggplot(long_data, aes(x=spatial_cond, 
                      y=sin, 
                      colour=noise_cond,
                      group=noise_cond, 
                      linetype=noise_cond,
                      shape = noise_cond)) + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.2, size=2, colour = "black", position = pd) + 
  stat_summary(fun.y="mean", geom="point", size=7, colour = "black", aes(shape = noise_cond), position = pd) + 
  stat_summary(fun.y="mean", geom="line", size=2, colour = "black", position = pd) + 
  theme_bw() + 
  theme(legend.position = c(0.76, 0.90),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=35, colour = "black"),
        legend.key.size = unit(2.5, 'lines')) +
  labs( x = "Spatial location (°)",
        y = expression(SRT(SNR["50%"], dB))) + 
  theme(axis.title.y = element_text(size=35, colour = "black"),
        axis.text.y = element_text(size=35, colour = "black"),
        axis.title.x = element_text(size=35, colour = "black"),
        axis.text.x = element_text(size=35, colour = "black")) +
  coord_cartesian(ylim = c(-6, 6))  +
  scale_y_continuous(breaks=seq(-6, 6, 1))  

interaction.plot(long_data$spatial_cond, long_data$noise_cond, long_data$sin)


install.packages("ez")
library(ez)

ezANOVA(
  long_data
  , sin
  , ID
  , within = spatial_cond*noise_cond
  , within_full = NULL
  , within_covariates = NULL
  , between = bds_group
  , between_covariates = NULL
  , observed = NULL
  , diff = NULL
  , reverse_diff = FALSE
  , type = 3
  , white.adjust = FALSE
  , detailed = F
  , return_aov = F
)



# save as png
png(filename="spatial_noise_interaction.png", 
    width = 8, height = 7, units = 'in', res = 300)
plot(spatial_noise_interaction)
dev.off()

############## Spatial*BDS #######################
pd <- position_dodge(width = 0.2)
spatial_BDS_interaction <- ggplot(long_data, aes(x=spatial_cond, 
                                                y=sin, 
                                                colour=bds_group,
                                                group=bds_group, 
                                                linetype=bds_group,
                                                shape = bds_group)) + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.2, size=2, colour = "black", position = pd) + 
  stat_summary(fun.y="mean", geom="point", size=7, colour = "black", aes(shape = bds_group), position = pd) + 
  stat_summary(fun.y="mean", geom="line", size=2, colour = "black", position = pd) + 
  theme_bw() + 
  theme(legend.position = c(0.76, 0.90),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=35, colour = "black"),
        legend.key.size = unit(2.5, 'lines')) +
  labs( x = "Spatial location (°)",
        y = expression(SRT(SNR["50%"], dB))) + 
  theme(axis.title.y = element_text(size=35, colour = "black"),
        axis.text.y = element_text(size=35, colour = "black"),
        axis.title.x = element_text(size=35, colour = "black"),
        axis.text.x = element_text(size=35, colour = "black")) +
  coord_cartesian(ylim = c(-5, 3))  +
  scale_y_continuous(breaks=seq(-4, 3, 1))   

# save as png
png(filename="spatial_BDS_interaction.png", 
    width = 8, height = 7, units = 'in', res = 300)
plot(spatial_BDS_interaction)
dev.off()


################ Spatial*Language #####################
pd <- position_dodge(width = 0.2)
spatial_language_interaction <- ggplot(long_data, aes(x=spatial_cond, 
                                                      y=sin, 
                                                      colour=Language,
                                                      group=Language, 
                                                      linetype=Language,
                                                      shape = Language)) + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.2, size=2, colour = "black", position = pd) + 
  stat_summary(fun.y="mean", geom="point", size=7, colour = "black", aes(shape = Language), position = pd) + 
  stat_summary(fun.y="mean", geom="line", size=2, colour = "black", position = pd) + 
  theme_bw() + 
  theme(legend.position = c(0.76, 0.90),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=35, colour = "black"),
        legend.key.size = unit(2.5, 'lines')) +
  labs( x = "Spatial location (°)",
        y = expression(SRT(SNR["50%"], dB))) + 
  theme(axis.title.y = element_text(size=35, colour = "black"),
        axis.text.y = element_text(size=35, colour = "black"),
        axis.title.x = element_text(size=35, colour = "black"),
        axis.text.x = element_text(size=35, colour = "black")) +
  coord_cartesian(ylim = c(-5, 4))  +
  scale_y_continuous(breaks=seq(-4, 4, 1))


# save as png
png(filename="spatial_language_interaction.png", 
    width = 8, height = 7, units = 'in', res = 300)
plot(spatial_language_interaction)
dev.off()

combined_2way_interactions <- grid.arrange(spatial_noise_interaction,
                                                      spatial_WM_interaction,
                                                      spatial_language_interaction, ncol = 3)
# save as png
png(filename="combined_2way_interactions.png", 
    width = 10, height = 5, units = 'in', res = 600)
plot(combined_2way_interactions)
dev.off()


################## BDS*Language interaction



pd <- position_dodge(width = 0.2)
FDS_language_interaction <- ggplot(long_data, aes(x= Language, 
                                                      y=sin, 
                                                      colour=fds_group,
                                                      group=fds_group, 
                                                      linetype=fds_group,
                                                      shape = fds_group)) + 
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=.2, size=2, colour = "black", position = pd) + 
  stat_summary(fun.y="mean", geom="point", size=7, colour = "black", aes(shape = fds_group), position = pd) + 
  stat_summary(fun.y="mean", geom="line", size=2, colour = "black", position = pd) + 
  theme_bw() + 
  theme(legend.position = c(0.76, 0.90),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=35, colour = "black"),
        legend.key.size = unit(2.5, 'lines')) +
  labs( x = "FDS group",
        y = expression(SRT(SNR["50%"], dB))) + 
  theme(axis.title.y = element_text(size=35, colour = "black"),
        axis.text.y = element_text(size=35, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=35, colour = "black")) +
  coord_cartesian(ylim = c(-5, 5))  +
  scale_y_continuous(breaks=seq(-4, 5, 1))

# save as png
png(filename="FDS_language_interaction.png", 
    width = 8, height = 7, units = 'in', res = 300)
plot(FDS_language_interaction)
dev.off()


############## BDS/SiN REGRESSION
# Data file for correlation between BDS and SiN
BDS_correlation_data<- read.csv(file="MainData_MusicStudy_CrossSect_9_6_2018.csv", header=TRUE,sep=";")
# Change commas into fullstops
for (i in 1:39) {BDS_correlation_data[,i] <- as.numeric(gsub(",", ".", as.character(BDS_correlation_data[,i])))}
sapply(BDS_correlation_data, class)
BDS_correlation_data[,11] <- as.integer(BDS_correlation_data[,11])
BDS_correlation_data[,27] <- as.integer(BDS_correlation_data[,27])

BDS_regression <- 
  ggplot(BDS_correlation_data, aes(x=BDS_correlation_data[,11], y=BDS_correlation_data[,27])) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 1, position = "jitter")  +
  geom_smooth(method="lm", se=FALSE, color = "black") +
  labs( x = "BDS",
        y = expression(SRT(SNR["50%"], dB))) + 
  theme(axis.title.y = element_text(size=30, colour = "black"),
        axis.text.y = element_text(size=30, colour = "black"),
        axis.title.x = element_text(size=30, colour = "black"),
        axis.text.x = element_text(size=30, colour = "black")) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  annotate("text", x = 7.5, y = 7, label = "italic(r) == -0.452", parse = TRUE, size = 8) +
  coord_cartesian(ylim = c(-5, 10))  +
  scale_y_continuous(breaks=seq(-5, 10, 1)) +
    coord_cartesian(xlim = c(-2, 10)) + 
  scale_x_continuous(breaks=seq(0, 8, 1)) 


png(filename="BDS_regression.png", 
    width = 7, height = 7, units = 'in', res = 300)
plot(BDS_regression)
dev.off()











## INTERACTION PLOTS 2ND ATTEMPT

s_n_interaction <- data.frame(matrix(ncol = 10, nrow = 4))
colnames(s_n_interaction)[1] <- "spatial_cond"
s_n_interaction[,1] <- c("0","0","90","90")
colnames(s_n_interaction)[2] <- "noise_cond"
s_n_interaction[,2] <- c("SSN","Speech","SSN","Speech")
colnames(s_n_interaction)[3] <- "s_n_means"
s_n_interaction[,3] <- c(-3.318, 4.841, -4.242, -.529)
colnames(s_n_interaction)[4] <- "s_n_standard_error"
s_n_interaction[,4] <- c(.611, .597, .796, .849)


pd = position_dodge(.6)
s_n_interaction_2 <- ggplot(s_n_interaction, aes(x= spatial_cond, 
                          y=s_n_means, 
                          colour=noise_cond,
                          group=noise_cond, 
                          linetype=noise_cond,
                          shape = noise_cond)) + 
      geom_errorbar(aes(ymin = s_n_means - s_n_standard_error,
                        ymax = s_n_means + s_n_standard_error),
                    width=.4, size=1, position=pd, colour = "black") +
      geom_point(shape=19, size=3, position=pd,colour = "black", aes(shape = noise_cond)) +
      geom_line(size=1, colour = "black", position = pd) + 
      theme_bw() + 
      theme(legend.position = c(0.63, 0.87),
            legend.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=18, colour = "black"),
            legend.key.size = unit(2.5, 'lines')) +
      labs( x = "",
            y = expression(SRT(SNR["50%"], dB))) + 
      theme(axis.title.y = element_text(size=25, colour = "black"),
            axis.text.y = element_text(size=20, colour = "black"),
            axis.title.x = element_text(size=20, colour = "black"),
            axis.text.x = element_text(size=20, colour = "black"),
            plot.title = element_text(hjust = 0.5,  size =17)) +
      coord_cartesian(ylim = c(-5, 6))  +
      scale_y_continuous(breaks=seq(-5, 6, 1)) + 
  ggtitle("Spatial x \n Background noise")

    # save as png
    png(filename="s_n_interaction_2.png", 
        width = 8, height = 7, units = 'in', res = 300)
    plot(s_n_interaction_2)
    dev.off()

    
#### S*BDS interaction    
    
    s_bds_interaction <- data.frame(matrix(ncol = 10, nrow = 4))
    colnames(s_bds_interaction)[1] <- "spatial_cond"
    s_bds_interaction[,1] <- c("0","90","0","90")
    colnames(s_bds_interaction)[2] <- "bds_group"
    s_bds_interaction[,2] <- c("Low BDS","Low BDS","High BDS","High BDS")
    colnames(s_bds_interaction)[3] <- "s_bds_means"
    s_bds_interaction[,3] <- c(1.093, -0.793, .430, -3.978)
    colnames(s_bds_interaction)[4] <- "s_bds_standard_error"
    s_bds_interaction[,4] <- c(.573, .839, .606, .888)    


    pd = position_dodge(.4)
    s_bds_interaction_2 <- ggplot(s_bds_interaction, aes(x= spatial_cond, 
                                                     y=s_bds_means, 
                                                     colour=bds_group,
                                                     group=bds_group, 
                                                     linetype=bds_group,
                                                     shape = bds_group)) + 
      geom_errorbar(aes(ymin = s_bds_means - s_bds_standard_error,
                        ymax = s_bds_means + s_bds_standard_error),
                    width=.4, size=1, position=pd, colour = "black") +
      geom_point(shape=19, size=3, position=pd,colour = "black", aes(shape = bds_group)) +
      geom_line(size=1, colour = "black", position = pd) + 
      theme_bw() + 
      theme(legend.position = c(0.60, 0.87),
            legend.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=18, colour = "black"),
            legend.key.size = unit(2.5, 'lines'),
            plot.title = element_text(hjust = 0.5, size =17)) +
      labs( x = "Spatial location (°)",
            y = "") + 
      theme(axis.title.y = element_text(size=20, colour = "black"),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size=25, colour = "black"),
            axis.text.x = element_text(size=20, colour = "black")) +
      coord_cartesian(ylim = c(-5, 6))  +
      scale_y_continuous(breaks=seq(-5, 6, 1))+ 
      ggtitle("Spatial x BDS")
    
    # save as png
    png(filename="s_bds_interaction_2.png", 
        width = 8, height = 7, units = 'in', res = 300)
    plot(s_bds_interaction_2)
    dev.off()

    

#### s*language interaction
    
    s_lang_interaction <- data.frame(matrix(ncol = 4, nrow = 4))
    colnames(s_lang_interaction)[1] <- "spatial_cond"
    s_lang_interaction[,1] <- c("0","90","0","90")
    colnames(s_lang_interaction)[2] <- "lang_group"
    s_lang_interaction[,2] <- c("Low EL","Low EL","High EL","High EL")
    colnames(s_lang_interaction)[3] <- "s_lang_means"
    s_lang_interaction[,3] <- c(1.812, -3.003, -.288, -1.767)
    colnames(s_lang_interaction)[4] <- "s_lang_standard_error"
    s_lang_interaction[,4] <- c(.616, .903, .562, .823)    
    
    
    pd = position_dodge(.4)
    s_lang_interaction_2 <- ggplot(s_lang_interaction, aes(x= spatial_cond, 
                                                         y=s_lang_means, 
                                                         colour=lang_group,
                                                         group=lang_group, 
                                                         linetype=lang_group,
                                                         shape = lang_group)) + 
      geom_errorbar(aes(ymin = s_lang_means - s_lang_standard_error,
                        ymax = s_lang_means + s_lang_standard_error),
                    width=.4, size=1, position=pd, colour = "black") +
      geom_point(shape=19, size=3, position=pd,colour = "black", aes(shape = lang_group)) +
      geom_line(size=1, colour = "black", position = pd) + 
      theme_bw() + 
      theme(legend.position = c(0.60, 0.87),
            legend.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=18, colour = "black"),
            legend.key.size = unit(2.5, 'lines'),
            plot.title = element_text(hjust = 0.5, size =17)) +
      labs( x = "",
            y = "") + 
      theme(axis.title.y = element_text(size=20, colour = "black"),
            axis.text.y = element_blank(),
            axis.title.x = element_text(size=20, colour = "black"),
            axis.text.x = element_text(size=20, colour = "black")) +
      coord_cartesian(ylim = c(-5, 6))  +
      scale_y_continuous(breaks=seq(-5, 6, 1)) + 
      ggtitle("Spatial x EL")
    
    # save as png
    png(filename="s_lang_interaction_2.png", 
        width = 8, height = 7, units = 'in', res = 300)
    plot(s_lang_interaction_2)
    dev.off()    
    
    
    
    
    
    
    
    
    
    # Join 3 plots into one
   joined_interaction_plot <- plot_grid(s_n_interaction_2,  
                                        s_bds_interaction_2, 
                                        s_lang_interaction_2, 
                                        align = "v",
                                        align = "h",
                                        ncol = 3)
    
    # save as png
    png(filename="joined_interaction_plot.png", 
        width = 8, height = 7, units = 'in', res = 300)
    plot(joined_interaction_plot)
    dev.off()
    
    
    joined_interaction_plot2 <- grid.arrange(s_n_interaction_2,  
              s_bds_interaction_2, 
              s_lang_interaction_2, 
             ncol = 2, nrow = 2)
    
    # save as png
    png(filename="joined_interaction_plot2.png", 
        width = 8, height = 7, units = 'in', res = 300)
    plot(joined_interaction_plot2)
    dev.off()
    
    pdf("joined_interaction_plot2.png", width=5,height=8)
    plot(joined_interaction_plot3)
    dev.off()
    
    
    
    int_plot <- ggarrange(s_n_interaction_2,  
              s_bds_interaction_2, 
              s_lang_interaction_2, ncol=3)
    # save as png
    png(filename="int_plot.png", 
        width = 8, height = 7, units = 'in', res = 400)
    plot(int_plot)
    dev.off()
    