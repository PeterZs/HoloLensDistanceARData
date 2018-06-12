# HoloLensDistanceARData

This repository contains the data and analysis scripts for the two perceptual experiments reported in

Using an Augmented Reality Device as a Distance-Based Vision Aid – Promise and Limitations.
M. Kinateder, J. Gualtieri, M.J. Dunn, W. Jarosz, X. Yang and E.A. Cooper
Optometry & Vision Science 2018
doi: 10.1097/OPX.0000000000001232

## 1. Data files

Data are saved in the Data directory and organized in tables with headers that can be read into R or similar tools

Visual identification task in Experiment 2

Experiment2_visual_identification_tasks: NAs (2) reflect that the subject's response was not recorded

Mobility task in Experiment 2

Experiment2_mobility_task_comparisons: Responses are preference statements, i.e., participants responded which of the 3 possible condition (AR, cane, or no asssistive device) they preferred

Experiment2_mobility_task_likert_ratings: Responses are likert ratings


## 2. Analysis Scripts

The R scripts require the following packages:

readxl, dplyr, ggplot2, scales, gridExtra, ez, schoRsch

The necessary packages will be automatically downloaded when sourcing the scripts.