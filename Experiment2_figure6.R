## Analysis for Experiment 2 Visual Identification Tasks - Creates Figure 6 for Kinateder et al 'Using an Augmented Reality Device to Improve Functional 
## Vision in Blindness - Promise and Limitations'
## Written 2017 by Max Kinateder


# Instructions: 
# This code assumes that the data are contained in a subfolder of the source file called 'Data' and should run as 
# is when you source it. 

# packages needed to run this code
necessary.packages = c('ez', 'schoRsch', 'ggplot2', 'scales', 'gridExtra', 'Partiallyoverlapping')
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
dat = read.csv2('Data/Experiment2_visual_identification_tasks.csv', sep = ',', dec = '.')
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
Figure6 = grid.arrange(p1,p2,p3, ncol=3)


# save figure 6
ggsave(filename = 'Plots/Figure6.png', plot = Figure6, device = NULL, path = NULL,
       scale = 1, width = 8, height = NA, units = 'in',
       dpi = 300, limitsize = TRUE)

# t-tests
## Pose recognition
t.test(dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "correct" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "correct" & dat$trial_type == "AR"])

t.test(dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "correct" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "correct" & dat$trial_type == "AR"])

t.test(dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "incorrect" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "incorrect" & dat$trial_type == "AR"])

t.test(dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "incorrect" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "incorrect" & dat$trial_type == "AR"])

## Object recognition
t.test(dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "correct" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "correct" & dat$trial_type == "AR"])

t.test(dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "correct" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "correct" & dat$trial_type == "AR"])

t.test(dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "incorrect" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "control" & dat$pose_score_binned == "incorrect" & dat$trial_type == "AR"])

t.test(dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "incorrect" & dat$trial_type == "baseline"],
       dat$pose_confidence[dat$exp_group_new == "test" & dat$pose_score_binned == "incorrect" & dat$trial_type == "AR"])

## Gesture recognition
t.test(dat$gesture_confidence[dat$exp_group_new == "control" & dat$gesture_score_binned == "correct" & dat$trial_type == "baseline"],
       dat$gesture_confidence[dat$exp_group_new == "control" & dat$gesture_score_binned == "correct" & dat$trial_type == "AR"])

t.test(dat$gesture_confidence[dat$exp_group_new == "test" & dat$gesture_score_binned == "correct" & dat$trial_type == "baseline"],
       dat$gesture_confidence[dat$exp_group_new == "test" & dat$gesture_score_binned == "correct" & dat$trial_type == "AR"])

t.test(dat$gesture_confidence[dat$exp_group_new == "control" & dat$gesture_score_binned == "incorrect" & dat$trial_type == "baseline"],
       dat$gesture_confidence[dat$exp_group_new == "control" & dat$gesture_score_binned == "incorrect" & dat$trial_type == "AR"])

t.test(dat$gesture_confidence[dat$exp_group_new == "test" & dat$gesture_score_binned == "incorrect" & dat$trial_type == "baseline"],
       dat$gesture_confidence[dat$exp_group_new == "test" & dat$gesture_score_binned == "incorrect" & dat$trial_type == "AR"])



# run partially overlapping t-test
dat$pose_confCor = ifelse(dat$pose_score_binned == "correct", dat$pose_confidence, NA)
dat$pose_confInC = ifelse(dat$pose_score_binned == "incorrect", dat$pose_confidence, NA)

dat$objects_confCor = ifelse(dat$objects_score_binned == "correct", dat$objects_confidence, NA)
dat$objects_confInC = ifelse(dat$objects_score_binned == "incorrect", dat$objects_confidence, NA)

dat$gesture_confCor = ifelse(dat$gesture_score_binned == "correct", dat$gesture_confidence, NA)
dat$gesture_confInC = ifelse(dat$gesture_score_binned == "incorrect", dat$gesture_confidence, NA)


library(Partiallyoverlapping)
# create vectors for each test
# Pose
pose.correct.control.baseline = dat$pose_confCor[dat$exp_group_new == "control" & dat$trial_type == "baseline"]
pose.correct.control.AR       = dat$pose_confCor[dat$exp_group_new == "control" & dat$trial_type == "AR"]
a = Partover.test(pose.correct.control.baseline,pose.correct.control.AR,var.equal=TRUE,stacked=TRUE)  

pose.correct.test.baseline = dat$pose_confCor[dat$exp_group_new == "test" & dat$trial_type == "baseline"]
pose.correct.test.AR       = dat$pose_confCor[dat$exp_group_new == "test" & dat$trial_type == "AR"]
b = Partover.test(pose.correct.test.baseline,pose.correct.test.AR,var.equal=TRUE,stacked=TRUE)   


pose.incorrect.control.baseline = dat$pose_confInC[dat$exp_group_new == "control" & dat$trial_type == "baseline"]
pose.incorrect.control.AR       = dat$pose_confInC[dat$exp_group_new == "control" & dat$trial_type == "AR"]
c = Partover.test(pose.incorrect.control.baseline,pose.incorrect.control.AR,var.equal=TRUE,stacked=TRUE)  

pose.incorrect.test.baseline = dat$pose_confInC[dat$exp_group_new == "test" & dat$trial_type == "baseline"]
pose.incorrect.test.AR       = dat$pose_confInC[dat$exp_group_new == "test" & dat$trial_type == "AR"]
d = Partover.test(pose.incorrect.test.baseline,pose.incorrect.test.AR,var.equal=TRUE,stacked=TRUE)  

# Objects
objects.correct.control.baseline = dat$objects_confCor[dat$exp_group_new == "control" & dat$trial_type == "baseline"]
objects.correct.control.AR       = dat$objects_confCor[dat$exp_group_new == "control" & dat$trial_type == "AR"]
e = Partover.test(objects.correct.control.baseline,objects.correct.control.AR,var.equal=TRUE,stacked=TRUE)

objects.correct.test.baseline = dat$objects_confCor[dat$exp_group_new == "test" & dat$trial_type == "baseline"]
objects.correct.test.AR       = dat$objects_confCor[dat$exp_group_new == "test" & dat$trial_type == "AR"]
f = Partover.test(objects.correct.test.baseline,objects.correct.test.AR,var.equal=TRUE,stacked=TRUE)   

objects.incorrect.control.baseline = dat$objects_confInC[dat$exp_group_new == "control" & dat$trial_type == "baseline"]
objects.incorrect.control.AR       = dat$objects_confInC[dat$exp_group_new == "control" & dat$trial_type == "AR"]
g = Partover.test(objects.incorrect.control.baseline,objects.incorrect.control.AR,var.equal=TRUE,stacked=TRUE)   

objects.incorrect.test.baseline = dat$objects_confInC[dat$exp_group_new == "test" & dat$trial_type == "baseline"]
objects.incorrect.test.AR       = dat$objects_confInC[dat$exp_group_new == "test" & dat$trial_type == "AR"]
h = Partover.test(objects.incorrect.test.baseline,objects.incorrect.test.AR,var.equal=TRUE,stacked=TRUE)   

# Gestures
gesture.correct.control.baseline = dat$gesture_confCor[dat$exp_group_new == "control" & dat$trial_type == "baseline"]
gesture.correct.control.AR       = dat$gesture_confCor[dat$exp_group_new == "control" & dat$trial_type == "AR"]
i = Partover.test(gesture.correct.control.baseline,gesture.correct.control.AR,var.equal=TRUE,stacked=TRUE)   

gesture.correct.test.baseline = dat$gesture_confCor[dat$exp_group_new == "test" & dat$trial_type == "baseline"]
gesture.correct.test.AR       = dat$gesture_confCor[dat$exp_group_new == "test" & dat$trial_type == "AR"]
j = Partover.test(gesture.correct.test.baseline,gesture.correct.test.AR,var.equal=TRUE,stacked=TRUE)   

# check if there are any paired observations in this test
df = as.data.frame(cbind(gesture.correct.test.baseline, gesture.correct.test.AR))
df = filter(df, (!is.na(gesture.correct.test.baseline) & !is.na(gesture.correct.test.AR)))

gesture.incorrect.control.baseline = dat$gesture_confInC[dat$exp_group_new == "control" & dat$trial_type == "baseline"]
gesture.incorrect.control.AR       = dat$gesture_confInC[dat$exp_group_new == "control" & dat$trial_type == "AR"]
k = Partover.test(gesture.incorrect.control.baseline,gesture.incorrect.control.AR,var.equal=TRUE,stacked=TRUE)   

gesture.incorrect.test.baseline = dat$gesture_confInC[dat$exp_group_new == "test" & dat$trial_type == "baseline"]
gesture.incorrect.test.AR       = dat$gesture_confInC[dat$exp_group_new == "test" & dat$trial_type == "AR"]
l = Partover.test(gesture.incorrect.test.baseline,gesture.incorrect.test.AR,var.equal=TRUE,stacked=TRUE)   

# Print out results for
# Poses
paste("T(",round(a$parameter,2),")=", round(a$statistic,2), " , p=", round(a$p.value, 3), sep ="")
paste("T(",round(b$parameter,2),")=", round(b$statistic,2), " , p=", round(b$p.value, 3), sep ="")
paste("T(",round(c$parameter,2),")=", round(c$statistic,2), " , p=", round(c$p.value, 3), sep ="")
paste("T(",round(d$parameter,2),")=", round(d$statistic,2), " , p=", round(d$p.value, 3), sep ="")

# Objects
paste("T(",round(e$parameter,2),")=", round(e$statistic,2), " , p=", round(e$p.value, 3), sep ="")
paste("T(",round(f$parameter,2),")=", round(f$statistic,2), " , p=", round(f$p.value, 3), sep ="")
paste("T(",round(g$parameter,2),")=", round(g$statistic,2), " , p=", round(g$p.value, 3), sep ="")
paste("T(",round(h$parameter,2),")=", round(h$statistic,2), " , p=", round(h$p.value, 3), sep ="")
# Gestures
paste("T(",round(i$parameter,2),")=", round(i$statistic,2), " , p=", round(i$p.value, 3), sep ="")
paste("T(",round(j$parameter,2),")=", round(j$statistic,2), " , p=", round(j$p.value, 3), sep ="")
paste("T(",round(k$parameter,2),")=", round(k$statistic,2), " , p=", round(k$p.value, 3), sep ="")
paste("T(",round(l$parameter,2),")=", round(l$statistic,2), " , p=", round(l$p.value, 3), sep ="")


## Multiple t-tests for time, age, frail between genders.
lapply(dat[,c("exp_group_new", "pose_score_binned", "trial_type")], function(x) t.test(dat$pose_confidence ~ x, var.equal = TRUE))
