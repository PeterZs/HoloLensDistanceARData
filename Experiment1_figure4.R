## Analysis for Experiment 1 Visual Identification Tasks - Creates Figure 4 and
#  stats output for Kinateder et al 'Using an Augmented Reality Device to
#  Improve Functional Vision in Blindness - Promise and Limitations'
#  Written 2017 by Max Kinateder


## Instructions: 
# This code assumes that the data are contained in a subfolder of 
# the source file called 'Data' and should run as 
# is when you source it. 
# A note on print(); this is only needed if you source the script; if you 
# run the code line by line it is not necessary

# Note that this code requires a few packages. The lines below will install them
# if they are missing. 

# packages needed to run this code
necessary.packages = c('dplyr', 'reshape2', 'tidyr', 
                       'BaylorEdPsych', 'effsize', 'schoRsch', 
                       'ggplot2', 'scales', 'gridExtra')
# find out which packages are missing
new.packages        = necessary.packages[!(necessary.packages %in% installed.packages()[,'Package'])]

# install missing packages 
if(length(new.packages)) install.packages(new.packages)

# clear workspace
rm(list=ls(all=TRUE)) 

# load libraries
library(dplyr)    # for filtering
library(reshape2) # for data wrangling and clean up
library(tidyr)    # for data wrangling and clean up
library(BaylorEdPsych) # to return partial eta squared
library(effsize)  # to return effect sizes (like cohens d)
library(ggplot2)  # for plotting
library(scales)   # to adjust scales in ggplot
library(gridExtra)# for multipanel plots
library(schoRsch) # to return stats in APA format

# set working directory to source file location (this works only if you source the script
# not when you run it line by line; alternatively you can set your wd manually)
source.dir = dirname(parent.frame(2)$ofile)
setwd(source.dir)

# load data
dat = read.csv2('Data/Experiment1.csv', sep = ',', dec = '.')

# make sure that all data are the correct type
dat$subject                 = as.character(dat$subject)
dat$person_score            = as.numeric(dat$person_score) 
dat$pose_score              = as.numeric(dat$pose_score) 
dat$objects_score           = as.numeric(dat$objects_score) 
dat$person_confidence       = as.numeric(dat$person_confidence) 
dat$pose_confidence         = as.numeric(dat$pose_confidence) 
dat$objects_confidence      = as.numeric(dat$objects_confidence) 
dat$distance_from_obstacle  = as.numeric(dat$distance_from_obstacle)

# add column with trial numbers to data frame
dat$trial = rep(1:5, 8)

# show & count number of rows with missing values (see manuscript for details)
print(paste('Number of incomplete cases: ', nrow(dat[!complete.cases(dat),])))

# select data needed for plotting and ANOVAs
dat = dplyr::select(dat, subject, trial, AR_on_off, distance_from_obstacle, person_score, person_confidence, pose_score, pose_confidence, objects_score, objects_confidence)

########################
#
#    Plot figure 4
#    A) accuracy (p1:p3)
#    B) confidence (p4:p6)
#    C) Distanc from obstacle (p7)
#
#########################

# plot accuracy
p1 = ggplot(dat,  aes(x = subject, y = person_score, fill = AR_on_off))+ 
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black') + 
  guides(fill=FALSE)+
  labs(y = 'Percent correct', title = 'Person localization: accuracy')

p2 = ggplot(dat,    aes(x = subject, y = pose_score, fill = AR_on_off))+ 
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+ 
  guides(fill=FALSE)+
  labs(y = 'Percent correct', title = 'Pose recognition: accuracy')

p3 = ggplot(dat, aes(x = subject, y = objects_score, fill = AR_on_off))+ 
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  guides(fill=FALSE)+
  labs(y = 'Percent correct', title = 'Objects recognition: accuracy')

# plot confidence (note that this has errorbars)
p4 = ggplot(dat, aes(x = subject, y = person_confidence, fill = AR_on_off))+  
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4) + 
  scale_y_continuous(limits=c(0.9,3.1), # set axis so that y starts at 1 (lowest possibl confidence score)
                   oob = rescale_none, 
                   expand = c(0, 0))+
  guides(fill=FALSE)+
  labs(y = 'Confidence [1:3]', title = 'Person localization: confidence')

p5 = ggplot(dat, aes(x = subject, y = pose_confidence, fill = AR_on_off))+ 
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4)+
  scale_y_continuous(limits=c(0.9,3.1),
                   oob = rescale_none, 
                   expand = c(0, 0))+
  guides(fill=FALSE)+
  labs(y = 'Confidence [1:3]', title = 'Pose recognition: confidence')

p6 = ggplot(dat, aes(x = subject, y = objects_confidence, fill = AR_on_off))+  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4)+ 
  scale_y_continuous(limits=c(0.9,3.1),
                   oob = rescale_none, 
                   expand = c(0, 0))+
  guides(fill=FALSE)+
  labs(y = 'Confidence [1:3]', title = 'Objects recognition: confidence')

# plot distance from obstacle
p7 = ggplot(dat, aes(x = subject, y = distance_from_obstacle, fill = AR_on_off))+ 
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+ 
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
             position = position_dodge(width = 0.90), width = 0.4)+ 
  labs(y = 'Distance from obstacle [m]', title = 'Distance from obstacle')


# make a Figure from all the subplots (needs gridExtra package)
Figure4 = grid.arrange(p1,p4,p2,p5,p3,p6,p7, ncol=2, nrow =4)


# save figure 7 (note that this will a fairly big image)
ggsave(filename = 'Plots/Figure4.png', plot = Figure4, device = NULL, path = NULL,
        scale = 1, width = 7, height = 21, units = 'in',
        dpi = 300, limitsize = TRUE)

########################
#
#    ANOVAs and posthoc tests
#
#########################

# clean up workspace a little
rm(list=setdiff(ls(), 'dat')) # removes all objects but 'dat' from workspace


# person_score.aov = aov(person_score ~ subject * AR_on_off, data = dat) # run anova
# print(summary(person_score.aov), quote = F)  # return anova results
# print(EtaSq(person_score.aov), quote = F)    # return effect size measures (needs BaylorEdPsych package)

# pairwise comparison between AR on and baseline 
 
# aggregate data over trials
dat_agg = aggregate(dat, by = list(dat$subject, dat$AR_on_off), FUN = mean, na.rm = T)
dat_agg = select(dat_agg, -subject, -AR_on_off)

colnames(dat_agg)[1] = "subject"
colnames(dat_agg)[2] = "AR_on_off"

# FIGURE POINTING: Accuracy
print('################### PERSON LOCALIZATION TASK: Accuracy #################', quote = F)
acc.test = t.test(dat_agg$person_score[dat_agg$AR_on_off == 'on'], 
                  dat_agg$person_score[dat_agg$AR_on_off == 'off'],
                  paired = T, alternative = 'greater')
t_out(acc.test)

# FIGURE POINTING: confidence
print('################### PERSON LOCALIZATION TASK: Confidence #################', quote = F)
conf.test = t.test(dat_agg$person_confidence[dat_agg$AR_on_off == 'on'], 
                   dat_agg$person_confidence[dat_agg$AR_on_off == 'off'], 
                   paired = T, alternative = 'greater')
t_out(conf.test, d.corr = F)


# POSE RECOGNITION: Accuracy
print('################### POSE RECOGNITION TASK: Accuracy #################', quote = F)
acc.test = t.test(dat_agg$pose_score[dat_agg$AR_on_off == 'on'], 
                  dat_agg$pose_score[dat_agg$AR_on_off == 'off'], 
                  paired = T, alternative = 'greater')
t_out(acc.test)

# POSE RECOGNITION: Confidence
print('################### POSE RECOGNITION TASK: Confidence #################')
conf.test = t.test(dat_agg$pose_confidence[dat_agg$AR_on_off == 'on'], 
                   dat_agg$pose_confidence[dat_agg$AR_on_off == 'off'], 
                   paired = T, alternative = 'greater')

t_out(conf.test, d.corr = F)


# Object recognition task: accuracy
print('################### Object RECOGNITION TASK: accuracy #################', quote = F)
acc.test = t.test(dat_agg$objects_score[dat_agg$AR_on_off == 'on'], 
                  dat_agg$objects_score[dat_agg$AR_on_off == 'off'], 
                  paired = T, alternative = 'greater')
t_out(acc.test)


# Object recognition task: confidence
print('################### Object RECOGNITION TASK: confidence #################', quote = F)
conf.test = t.test(dat_agg$objects_confidence[dat_agg$AR_on_off == 'on'], 
                   dat_agg$objects_confidence[dat_agg$AR_on_off == 'off'], 
                   paired = T, alternative = 'greater')
t_out(conf.test, d.corr = F)


# Obstacle distance
print('################### Obstacle distance #################', quote = F)
dist.test = t.test(dat_agg$distance_from_obstacle[dat_agg$AR_on_off == 'on'], 
                   dat_agg$distance_from_obstacle[dat_agg$AR_on_off == 'off'], 
                   paired = T, alternative = 'greater')
t_out(dist.test, d.corr = F)


# clean up workspace a little
#rm(list=setdiff(ls(), 'dat')) # removes all objects but 'dat' from workspace