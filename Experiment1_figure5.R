## Analysis for Experiment 1 Visual Identification Tasks - Creates Figure 5 for Kinateder et al 'Using an Augmented Reality Device to Improve Functional 
## Vision in Blindness - Promise and Limitations'
## Written 2017 by Max Kinateder


# Instructions: 
# This code assumes that the data are contained in a subfolder of the source file called 'Data' and should run as 
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
dat = read.csv2('Data/Experiment1_visual_identification_tasks.csv', sep = ',', dec = '.')
dat = dplyr::select(dat, -objects_guess) # remove columns we don't need (specific object response)

# make sure that all data are the correct type
dat$subject            = as.character(dat$subject)
dat$exp_group          = factor(dat$exp_group, 
                                levels = c('control', 'color', 'opacity')) # sort exp_group levels for plotting so that control is first
dat$pose_score         = as.numeric(dat$pose_score)
dat$objects_score      = as.numeric(dat$objects_score)
dat$gesture_score      = as.numeric(dat$gesture_score)
dat$pose_confidence    = as.numeric(dat$pose_confidence)
dat$objects_confidence = as.numeric(dat$objects_confidence)
dat$gesture_confidence = as.numeric(dat$gesture_confidence)
dat$trial_type = ifelse(dat$trial_type == 'on', 'AR', 'baseline')
dat$trial_type = factor(dat$trial_type, levels = c('baseline', 'AR'))


# show & count number of rows with missing values (see manuscript for details)
print(paste('Number of incomplete cases: ', nrow(dat[!complete.cases(dat),])))

# combine color and opacity overlays into 'test' category
dat$exp_group_new = ifelse((dat$exp_group == 'color' | dat$exp_group =='opacity'),'test','control')

# bin gesture and pose responses into 3 categories
dat$pose_score_binned    = ifelse(dat$pose_score < .25, 'incorrect', 
                           ifelse(dat$pose_score > .75, 'correct', 'unclear'))

dat$objects_score_binned = ifelse(dat$objects_score == 0, 'incorrect', 'correct')

dat$gesture_score_binned = ifelse(dat$gesture_score < .25, 'incorrect', 
                           ifelse(dat$gesture_score > .75, 'correct', 'unclear'))

dat$pose_score_binned    = factor(dat$pose_score_binned,    levels = c('correct', 'incorrect', 'unclear'))
dat$objects_score_binned = factor(dat$objects_score_binned, levels = c('correct', 'incorrect'))
dat$gesture_score_binned = factor(dat$gesture_score_binned, levels = c('correct', 'incorrect', 'unclear'))

########################
#
#    Plot figure 5: Confidence as a function of accuracy
#    p1: pose recognition task
#    p2: object recognition task
#    p3: gesture recognition task
#
#########################

p1 = ggplot(data = filter(dat[complete.cases(dat), ],pose_score_binned != 'unclear'), aes(x = exp_group_new, y = pose_confidence, fill = trial_type ))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', position = position_dodge(width = 0.90), width = 0.4)+
  facet_grid(.~pose_score_binned)+
  scale_y_continuous(limits=c(0.9,3.1), oob = rescale_none, expand = c(0, 0))+
  labs(y = 'Confidence [1:3]', x = '', title = 'Pose recognition: \nconfidence & accuracy')+
  theme(legend.position = c(0.8, 0.85), legend.title = element_blank(), legend.background = element_blank())

p2 = ggplot(data = filter(dat[complete.cases(dat), ],objects_score_binned != 'unclear'), aes(x = exp_group_new, y = objects_confidence, fill = trial_type ))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', position = position_dodge(width = 0.90), width = 0.4)+
  facet_grid(.~objects_score_binned)+
  scale_y_continuous(limits=c(0.9,3.1), oob = rescale_none, expand = c(0, 0))+ guides(fill=FALSE)+
  labs(y = 'Confidence [1:3]', x = '', title = 'Object recognition: \nconfidence & accuracy')

p3 = ggplot(data = filter(dat[complete.cases(dat), ],gesture_score_binned != 'unclear'), aes(x = exp_group_new, y = gesture_confidence, fill = trial_type ))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', position = position_dodge(width = 0.90), width = 0.4)+
  facet_grid(.~gesture_score_binned)+
  scale_y_continuous(limits=c(0.9,3.1), oob = rescale_none, expand = c(0, 0))+ guides(fill=FALSE)+
  labs(y = 'Confidence [1:3]', x = '', title = 'Gesture recognition: \nconfidence & accuracy')

# make a Figure from all the subplots (needs gridExtra package)
Figure5 = grid.arrange(p1,p2,p3, ncol=3)


# save figure 5
ggsave(filename = 'Plots/Figure5.png', plot = Figure5, device = NULL, path = NULL,
       scale = 1, width = 8, height = NA, units = 'in',
       dpi = 300, limitsize = TRUE)


