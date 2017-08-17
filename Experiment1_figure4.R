## Analysis for Experiment 1 Visual Identification Tasks - Creates Figure 4 and ANOVA 
## output for Kinateder et al 'Using an Augmented Reality Device to Improve Functional 
## Vision in Blindness - Promises and Limitations'
## Written 2017 by Max Kinateder


# Instructions: 
# This code assumes that the data are contained in a subfolder of the source file called "Data" and should run as 
# is when you source it. 

# packages needed to run this code
necessary.packages = c('ez', 'schoRsch', 'ggplot2', 'scales', 'gridExtra')
# find out which packages are missing
new.packages        = necessary.packages[!(necessary.packages %in% installed.packages()[,'Package'])]

# install missing packages 
if(length(new.packages)) install.packages(new.packages)

# clear workspace
rm(list=ls(all=TRUE)) 


# load libraries
library(dplyr)    # for data wrangling
library(ggplot2)  # for plotting
library(scales)   # to adjust scales in ggplot
library(gridExtra)# for multipane plotting
library(ez)       # for ANOVAs
library(schoRsch) # return ez output in APA style

# set working directory to source file location (this works only if you source the script
# not when you run it line by line; alternatively you can set your wd manually)
source.dir = dirname(parent.frame(2)$ofile)
setwd(source.dir)

# load data
dat = read.csv2("Data/Experiment1_visual_identification_tasks.csv", sep = ',', dec = '.')
dat = dplyr::select(dat, -objects_guess) # remove columns we don't need (specific object response)

# make sure that all data are the correct type
dat$subject            = as.character(dat$subject)
dat$exp_group          = factor(dat$exp_group, 
                                levels = c("control", "color", "opacity")) # sort exp_group levels for plotting so that control is first
dat$pose_score         = as.numeric(dat$pose_score)
dat$objects_score      = as.numeric(dat$objects_score)
dat$gesture_score      = as.numeric(dat$gesture_score)
dat$pose_confidence    = as.numeric(dat$pose_confidence)
dat$objects_confidence = as.numeric(dat$objects_confidence)
dat$gesture_confidence = as.numeric(dat$gesture_confidence)


# show & count number of rows with missing values (see manuscript for details)
print(paste('Number of incomplete cases: ', nrow(dat[!complete.cases(dat),])))


#################################
#                               #
#  Plot visual id tasks         #
#   A) performance (p1:p3)      #
#   B) confidence  (p4:p6)      #
#                               #
#################################

p1 = ggplot(dat, aes(x = exp_group, y = pose_score, fill = trial_type))+
  stat_summary(fun.y="mean", 
               geom="bar", 
               position = 'dodge', 
               color = 'black')+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.4)+
  annotate(geom = 'text', 
           x = 0.5, y = 0.9, 
           label = "bold(A)", 
           parse = T)+
  scale_y_continuous(labels = scales::percent,limits = c(0, 1), 
                     expand = c(0, 0))+
  labs(x = '', y = "Percent correct", fill = "AR", title = "Pose recognition")+
  theme(legend.direction = 'horizontal', 
        legend.position = c(.9,0.8), 
        legend.justification = c(1, 0))


p2 = ggplot(dat, aes(x = exp_group, y = objects_score, fill = trial_type))+
  stat_summary(fun.y="mean", 
               geom="bar", 
               position = 'dodge', 
               color = 'black')+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.4)+
  annotate(geom = 'text', 
           x = 0.5, y = 0.9, 
           label = "bold(B)", 
           parse = T)+
  scale_y_continuous(labels = scales::percent,limits = c(0, 1), 
                     expand = c(0, 0))+      
  labs(x = '', y = "Percent correct", fill = "AR", title = "Object recognition")+
  guides(fill=FALSE)


p3 = ggplot(dat, aes(x = exp_group, y = gesture_score, fill = trial_type))+
  stat_summary(fun.y="mean", 
               geom="bar", 
               position = 'dodge', 
               color = 'black')+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.4)+
  annotate(geom = 'text', 
           x = 0.5, y = 0.9, 
           label = "bold(C)", 
           parse = T)+
  scale_y_continuous(labels = scales::percent,limits = c(0, 1), 
                     expand = c(0, 0))+
  labs(x = '', y = "Percent correct", fill = "AR", title = "Gesture recognition")+
  guides(fill=FALSE)


p4 = ggplot(dat, aes(x = exp_group, y = pose_confidence, fill = trial_type))+
  stat_summary(fun.y="mean", 
               geom="bar", 
               position = 'dodge', 
               color = 'black')+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.4)+
  annotate(geom = 'text', 
           x = 0.5, y = 2.9, 
           label = "bold(D)", 
           parse = T)+
  scale_y_continuous(limits=c(1,3),
                     oob = rescale_none, 
                     expand = c(0, 0))+      labs(x = '', y = "Confidence [1:3]", fill = "Overlay", title = '')+
  guides(fill=FALSE)


p5 = ggplot(dat, aes(x = exp_group, y = objects_confidence, fill = trial_type))+
  stat_summary(fun.y="mean", 
               geom="bar", 
               position = 'dodge', 
               color = 'black')+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.4)+
  annotate(geom = 'text', 
           x = 0.5, y = 2.9, 
           label = "bold(E)", 
           parse = T)+
  scale_y_continuous(limits=c(1,3),
                     oob = rescale_none, 
                     expand = c(0, 0))+
  labs(x = '', y = "Confidence [1:3]", fill = "Overlay", title = '')+
  guides(fill=FALSE)


p6 = ggplot(dat, aes(x = exp_group, y = gesture_confidence, fill = trial_type))+
  stat_summary(fun.y="mean", 
               geom="bar", 
               position = 'dodge', 
               color = 'black')+
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.4)+
  annotate(geom = 'text', 
           x = 0.5, y = 2.9, 
           label = "bold(F)", 
           parse = T)+
  scale_y_continuous(limits=c(1,3),
                     oob = rescale_none, 
                     expand = c(0, 0))+      
  labs(x = '', y = "Confidence [1:3]", fill = "Overlay", title = ' ')+
  guides(fill=FALSE)

# make a Figure from all the subplots (needs gridExtra package)
Figure4 = grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3, nrow =2)

# save figure 4
ggsave(filename = 'Plots/Figure4.png', plot = Figure4, device = NULL, path = NULL,
        scale = 1, width = 7, height = NA, units = "in",
        dpi = 300, limitsize = TRUE)


#########################
#                       #
# ANOVAS - performance  #
#                       #
#########################

# remove 1 missing value for analysis (see manuscript for details)
dat_na_removed = as.data.frame(na.omit(dat))


# pose recognition
pose_anova = ezANOVA(data = dat_na_removed, 
                     dv = pose_score, 
                     wid = subject, 
                     between = exp_group,
                     within = trial_type,
                     detailed = T,
                     return_aov = T)

# return the result in APA style (requires schoRsch)
print('POSE RECOGNITION ACCURACY')
anova_out(pose_anova)

# objects
object_anova = ezANOVA(data = dat_na_removed, 
                       dv = objects_score, 
                       wid = subject, 
                       between = exp_group,
                       within = trial_type,
                       detailed = T,
                       return_aov = T)

print('OBJECT RECOGNITION ACCURACY')
anova_out(object_anova)

# gestures
gesture_anova = ezANOVA(data = dat_na_removed, 
                        dv = gesture_score, 
                        wid = subject, 
                        between = exp_group,
                        within = trial_type,
                        detailed = T,
                        return_aov = T)

print('GESTURE RECOGNITION ACCURACY')
anova_out(gesture_anova)

# post hoc comparisons (paired t tests)

# create new data sets for each experimental group
dat.col = filter(dat, exp_group == 'color')
dat.lum = filter(dat, exp_group == 'opacity')
dat.con = filter(dat, exp_group == 'control')

# objects
t_con_o = t.test(dat.con$objects_score[dat.con$trial_type == 'on'],dat.con$objects_score[dat.con$trial_type == 'off'], paired = T)
t_col_o = t.test(dat.col$objects_score[dat.col$trial_type == 'on'],dat.col$objects_score[dat.col$trial_type == 'off'], paired = T)
t_lum_o = t.test(dat.lum$objects_score[dat.lum$trial_type == 'on'],dat.lum$objects_score[dat.lum$trial_type == 'off'], paired = T)
t_out(t_con_o)
t_out(t_col_o)
t_out(t_lum_o)

# gestures
t_col_g = t.test(dat.col$gesture_score[dat.col$trial_type == 'on'],dat.col$gesture_score[dat.col$trial_type == 'off'], paired = T)
t_lum_g = t.test(dat.lum$gesture_score[dat.lum$trial_type == 'on'],dat.lum$gesture_score[dat.lum$trial_type == 'off'], paired = T)
t_con_g = t.test(dat.con$gesture_score[dat.con$trial_type == 'on'],dat.con$gesture_score[dat.con$trial_type == 'off'], paired = T)
t_out(t_con_g)
t_out(t_col_g)
t_out(t_lum_g)

#######################
#
# CONFIDENCE 
#
######################


# ANOVAS confidence
# poses
pose_anova = ezANOVA(data = dat_na_removed, 
                     dv = pose_confidence, 
                     wid = subject, 
                     between = exp_group,
                     within = trial_type,
                     detailed = T,
                     return_aov = T)

print('POSE RECOGNITION CONFIDENCE')
anova_out(pose_anova)

# objects
object_anova = ezANOVA(data = dat_na_removed, 
                       dv = objects_confidence, 
                       wid = subject, 
                       between = exp_group,
                       within = trial_type,
                       detailed = T,
                       return_aov = T)

print('OBJECT RECOGNITION CONFIDENCE')
anova_out(object_anova)

# gestures
gesture_anova = ezANOVA(data = dat_na_removed, 
                        dv = gesture_confidence, 
                        wid = subject, 
                        between = exp_group,
                        within = trial_type,
                        detailed = T,
                        return_aov = T)

print('GESTURE RECOGNITION CONFIDENCE')
anova_out(gesture_anova)

# post hoc comparisons: objects
t_col_o = t.test(dat.col$objects_confidence[dat.col$trial_type == 'on'],dat.col$objects_confidence[dat.col$trial_type == 'off'], paired = T)
t_lum_o = t.test(dat.lum$objects_confidence[dat.lum$trial_type == 'on'],dat.lum$objects_confidence[dat.lum$trial_type == 'off'], paired = T)
t_con_o = t.test(dat.con$objects_confidence[dat.con$trial_type == 'on'],dat.con$objects_confidence[dat.con$trial_type == 'off'], paired = T)
t_out(t_con_o)
t_out(t_col_o)
t_out(t_lum_o)

# post hoc comparisons: gestures
t_col_g = t.test(dat.col$gesture_confidence[dat.col$trial_type == 'on'],dat.col$gesture_confidence[dat.col$trial_type == 'off'], paired = T)
t_lum_g =t.test(dat.lum$gesture_confidence[dat.lum$trial_type == 'on'],dat.lum$gesture_confidence[dat.lum$trial_type == 'off'], paired = T)
t_con_g =t.test(dat.con$gesture_confidence[dat.con$trial_type == 'on'],dat.con$gesture_confidence[dat.con$trial_type == 'off'], paired = T)
t_out(t_con_g)
t_out(t_col_g)
t_out(t_lum_g)

