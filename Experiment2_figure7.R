## Analysis for Experiment 2 mobility - Creates Figure 7 and
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
necessary.packages = c('reshape2', 'ggplot2', 'dplyr', 
                       'tidyr', 'Rmisc', 'gridExtra',
                       'ez', 'schoRsch', 'scales')
# find out which packages are missing
new.packages        = necessary.packages[!(necessary.packages %in% installed.packages()[,'Package'])]

# install missing packages 
if(length(new.packages)) install.packages(new.packages)

# clear workspace
rm(list=ls(all=TRUE)) 

# load libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Rmisc)
library(ez)
library(schoRsch)
library(scales)
library(gridExtra)

# set working directory to source file location (this works only if you source the script
# not when you run it line by line; alternatively you can set your wd manually)
source.dir = dirname(parent.frame(2)$ofile)
setwd(source.dir)

# load data
dat = read.csv2('Data/Experiment2_mobility_task_likert_ratings.csv', sep = ',', dec = '.')

# show & count number of rows with missing values (see manuscript for details)
print(paste('Number of incomplete cases: ', nrow(dat[!complete.cases(dat),])))

# combine color and opacity group into 'test' category
dat$exp_group_new = ifelse(dat$exp_group == 'color', 'test',
                    ifelse(dat$exp_group == 'luminance', 'test', 'control'))

dat$exp_group_new = factor(dat$exp_group_new, levels = c('control', 'test'))


# Prepare plotting: create a new data frame for each of the 4 questions 
# select relevant variables and create data frames
dat.run_into = select(dat, -exp_group, -comfort, -navigate, -vision)
dat.comfort  = select(dat, -exp_group, -run_into, -navigate, -vision)
dat.navigate = select(dat, -exp_group, -comfort, -run_into, -vision)
dat.vision   = select(dat, -exp_group, -comfort, -navigate, -run_into)

# transform to wide format
wide.run_into = spread(data = dat.run_into, key = trial_type, value = run_into)
wide.comfort  = spread(data = dat.comfort,  key = trial_type, value = comfort)
wide.navigate = spread(data = dat.navigate, key = trial_type, value = navigate)
wide.vision   = spread(data = dat.vision,   key = trial_type, value = vision)

# calculate difference between baseline and cane/AR scores
wide.run_into$diff.cane = wide.run_into$cane - wide.run_into$baseline
wide.run_into$diff.AR   = wide.run_into$AR   - wide.run_into$baseline
wide.comfort$diff.cane  = wide.comfort$cane - wide.comfort$baseline
wide.comfort$diff.AR    = wide.comfort$AR   - wide.comfort$baseline
wide.navigate$diff.cane = wide.navigate$cane - wide.navigate$baseline
wide.navigate$diff.AR   = wide.navigate$AR   - wide.navigate$baseline
wide.vision$diff.cane   = wide.vision$cane - wide.vision$baseline
wide.vision$diff.AR     = wide.vision$AR   - wide.vision$baseline

# remove unnecessary variables from data frame
wide.run_into = select(wide.run_into, -baseline, -cane, -AR)
wide.comfort  = select(wide.comfort, -baseline, -cane, -AR)
wide.navigate = select(wide.navigate, -baseline, -cane, -AR)
wide.vision   = select(wide.vision, -baseline, -cane, -AR)

# go back to long format for plotting
dat.run_into = wide.run_into %>% gather(diff_type, diff.score, diff.cane:diff.AR)
dat.comfort  = wide.comfort %>% gather(diff_type, diff.score, diff.cane:diff.AR)
dat.navigate = wide.navigate %>% gather(diff_type, diff.score, diff.cane:diff.AR)
dat.vision   = wide.vision %>% gather(diff_type, diff.score, diff.cane:diff.AR)

# clean up workspace a little
rm(list=setdiff(ls(), c('dat','dat.run_into','dat.comfort', 'dat.navigate', 'dat.vision'))) # removes all objects but 'dat' from workspace


#################################
#                               
#  Plot mobility tasks          
#   p1: reduced collision risk
#   p2: usefulness of vision
#   p3: comfort while exploring
#   p4: Ease of navigation
#                               
#################################

p1 = ggplot(dat.run_into, aes(exp_group_new, diff.score, fill = diff_type))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4)+
  labs(x = '', y = 'Difference to \nbaseline [-6:6]', fill = '', title = 'Reduced collision risk')+
  scale_y_continuous(limits=c(-1,3), oob = rescale_none, expand = c(0, 0))+
  theme(legend.position = c(0.3, 0.85), legend.direction = 'horizontal', legend.title = element_blank(), legend.background = element_blank())


p2 = ggplot(dat.vision, aes(exp_group_new, diff.score, fill = diff_type))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4)+ guides(fill=FALSE)+
  labs(x = '', y = 'Difference to \nbaseline [-6:6]', fill = '', title = 'Usefulness of vision')+
  scale_y_continuous(limits=c(-1,3), oob = rescale_none, expand = c(0, 0))

p3 = ggplot(dat.comfort, aes(exp_group_new, diff.score, fill = diff_type))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4)+ guides(fill=FALSE)+
  labs(x = '', y = 'Difference to \nbaseline [-6:6]', fill = '', title = 'Comfort while exploring')+
  scale_y_continuous(limits=c(-1,3), oob = rescale_none, expand = c(0, 0))

p4 = ggplot(dat.navigate, aes(exp_group_new, diff.score, fill = diff_type))+
  stat_summary(fun.y='mean', geom='bar', position = 'dodge', color = 'black')+
  stat_summary(fun.data = mean_se, geom = 'errorbar', 
               position = position_dodge(width = 0.90), width = 0.4)+ guides(fill=FALSE)+
  labs(x = '', y = 'Difference to \nbaseline [-6:6]', fill = '', title = 'Ease of navigation')+
  scale_y_continuous(limits=c(-1,3), oob = rescale_none, expand = c(0, 0))


# make a big plot from all the subplots
Figure7 = grid.arrange(p1, p2, p3, p4, ncol=2, nrow =2)


# save figure 7
ggsave(filename = 'Plots/Figure7.png', plot = Figure7, device = NULL, path = NULL,
       scale = 1, width = 7, height = NA, units = 'in',
       dpi = 300, limitsize = TRUE)


# ANOVAs

# Reduced collision risk
print('############ REDUCED COLLISION RISK: anova output')
run_into_anova = ezANOVA(data = dat, 
                         dv = run_into, 
                         wid = subject, 
                         between = exp_group_new,
                         within = trial_type,
                         type = 2,
                         detailed = T,
                         return_aov = T)
print(anova_out(run_into_anova))


# t t-test against 0 for difference scores
t.diff.ri.cane.con    = t.test(dat.run_into$diff.score[dat.run_into$diff_type == 'diff.cane' & dat.run_into$exp_group_new == 'control'])
t.diff.ri.overlay.con = t.test(dat.run_into$diff.score[dat.run_into$diff_type == 'diff.AR' & dat.run_into$exp_group_new == 'control'])
t.diff.ri.cane.aug    = t.test(dat.run_into$diff.score[dat.run_into$diff_type == 'diff.cane' & dat.run_into$exp_group_new == 'test'])
t.diff.ri.overlay.aug = t.test(dat.run_into$diff.score[dat.run_into$diff_type == 'diff.AR' & dat.run_into$exp_group_new == 'test'])
print(t.diff.ri.cane.con)
print(t.diff.ri.overlay.con)
print(t.diff.ri.cane.aug)
print(t.diff.ri.overlay.aug)

# Usefulness of vision
print('############ USEFULNESS OF VISION: anova output')
vision_anova = ezANOVA(data = dat, 
                       dv = vision, 
                       wid = subject, 
                       between = exp_group_new,
                       within = trial_type,
                       type = 2,
                       detailed = T,
                       return_aov = T)

print(anova_out(vision_anova))

# t t-test against 0 for difference scores
t.diff.vis.cane.con    = t.test(dat.vision$diff.score[dat.vision$diff_type == 'diff.cane' & dat.vision$exp_group_new == 'control'])
t.diff.vis.overlay.con = t.test(dat.vision$diff.score[dat.vision$diff_type == 'diff.AR' & dat.vision$exp_group_new == 'control'])
t.diff.vis.cane.aug    = t.test(dat.vision$diff.score[dat.vision$diff_type == 'diff.cane' & dat.vision$exp_group_new == 'test'])
t.diff.vis.overlay.aug = t.test(dat.vision$diff.score[dat.vision$diff_type == 'diff.AR' & dat.vision$exp_group_new == 'test'])

print(t.diff.vis.cane.con)
print(t.diff.vis.overlay.con)
print(t.diff.vis.cane.aug)
print(t.diff.vis.overlay.aug)

