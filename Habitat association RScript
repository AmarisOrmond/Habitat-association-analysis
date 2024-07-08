library(plotly)
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(mgcv)
library(mgcViz)
install.packages("readxl")
install.packages("tidyverse", dependencies = TRUE)
library(forcats)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(grid) 
install.packages("magrittr")
library(magrittr)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggpmisc")
library(ggpmisc)
install.packages("ggplot2")
library(ggplot2)



plot.new()



#### Statistics summary ##############

model.lm <- lm(mm ~ specialism,  data = water_np_dr_species_ranks)
summary(model.lm)


############# summarise - mm optimal range #####################

av <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(mm, na.rm = TRUE),
    average_range = mean(mm))
av

  
  ggplot(water_np_dr_species_ranks, aes(x = mm, y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE)+
  geom_point(size = 4) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  xlab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  ylab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(0,1.3), ylim = c(1,7))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(1,7, by=1), limits = c(1,7), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(0,1.3, by=0.1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin = average_range - sd, ymax = average_range + sd), width = 0.1, position = position_dodge(0.9))

  ### insert this to show p value  ##############
  
  stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 
  
  #### this inserts (y=mx+c) equation ####
  
  stat_regline_equation(aes(label = ..eq.label..)) +
    
    #### r2 values ########
  
  stat_regline_equation(aes(label = ..rr.label..)) +
    




###############################################################
##### mm min WC ####################################

mm_min <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(Min_mm, na.rm = TRUE),
    Min_mm = mean(Min_mm))
mm_min

  
  
############# Summary of statistics ###########

model.minmm <- lm(Min_mm ~ specialism,  data = water_np_dr_species_ranks)
summary(model.minmm)


ggplot(water_np_dr_species_ranks, aes(x = Min_mm, y =  specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Minimum~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,1.3))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,0.8, by=0.1), limits = c(0,0.8), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  theme(legend.position = "none")  +
  geom_errorbar(aes(ymin = mm_min - sd, ymax = mm_min + sd), width = 0.1, position = position_dodge(0.9)) +


### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  
  
  

#############################################################
################ ##### mm max WC ####################################

############# Summary of statistics ###########
model.maxmm <- lm(Max_mm ~ specialism,  data = water_np_dr_species_ranks_cephalo)
summary(model.maxmm)


mm_max <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(Max_mm, na.rm = TRUE),
    Max_mm = mean(Max_mm))
mm_max


ggplot(water_np_dr_species_ranks_chloro, aes(x = Max_mm, y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Maximum~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,1.3))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  theme(legend.position = "none")  +
  geom_errorbar(aes(ymin = mm_max - sd, ymax = mm_max + sd), width = 0.1, position = position_dodge(0.9)) +
  
  ### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  


###################################################################################################################






## ##### % optimum WC ####################################

### summarise data ######## 
optimumpercent <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(percent, na.rm = TRUE),
    opt_percent = mean(percent))
optimumpercent


############# Summary of statistics ###########
model.percentoptimum <- lm(percent ~ specialism,  data = water_np_dr_species_ranks_cephalo)
summary(model.percentoptimu)


ggplot(water_np_dr_species_ranks, aes(x = specialism, y = percent, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,500))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = optimumpercent - sd, ymax =  optimumpercent + sd), width = 0.1, position = position_dodge(0.9))  +
  
  ### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  

###################################################################################################################



######## % min WC ####################################

### summarise dataset #### 
min_percent <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(Min_percent, na.rm = TRUE),
    min_percent = mean(Min_percent))
min_percent


############# Summary of statistics ###########
min.percent <- lm(Min_percent ~ specialism,  data = water_np_dr_species_ranks)
summary(min.percent)


ggplot(water_np_dr_species_ranks, aes(x = specialism, y = Min_percent, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,500))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = min_percent - sd, ymax = min_percent + sd), width = 0.1, position = position_dodge(0.9)) 

### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  

###################################################################################################################

######## % max WC ####################################

max_percent <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(Max_percent, na.rm = TRUE),
    max_percent = mean(Max_percent))
max_percent


############# Summary of statistics ###########
max.percent <- lm(Max_percent ~ specialism,  data = water_np_dr_species_ranks)
summary(max.percent)


ggplot(water_np_dr_species_ranks, aes(x = Max_percent, y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
 ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,800))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = max_percent - sd, ymax = max_percent + sd), width = 0.1, position = position_dodge(0.9)) 

### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  


  
######## NP ####################################

np <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(NP, na.rm = TRUE),
    np = mean(NP))
np
  
############# Summary of statistics ###########

model.np <- lm(NP ~ specialism,  data = water_np_dr_species_ranks)
summary(model.np)

  
  
  ggplot(water_np_dr_species_ranks, aes(x = NP , y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,60))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = average_range - sd, ymax = average_range + sd), width = 0.1, position = position_dodge(0.9)) 

  ### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  



######## DR ####################################

dr <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(DR, na.rm = TRUE),
    average_range = mean(DR))
dr

############# Summary of statistics ###########
model.dr <- lm(DR ~ specialism,  data = water_np_dr_species_ranks)
summary(model.dr)


  
  ggplot(water_np_dr_species_ranks, aes(x = DR, y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(-10,0))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = average_range - sd, ymax = average_range + sd), width = 0.1, position = position_dodge(0.9)) 

### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  




######## CGE ####################################

cge <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(CGE, na.rm = TRUE),
    average_range = mean(CGE))
cge

############# Summary of statistics ###########

model.cue <- lm(CUE ~ specialism,  data = water_np_dr_species_ranks)
summary(model.cue)


ggplot(water_np_dr_species_ranks_cephalo, aes(x = CGE , y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) +
ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,10))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = average_range - sd, ymax = average_range + sd), width = 0.1, position = position_dodge(0.9)) 

### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  




################ LCP    ###########################################################################


LCP <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(LCP, na.rm = TRUE),
    average_range = mean(LCP))
LCP

############# Summary of statistics ###########
model.LCP <- lm(specialism ~ LCP ,  data = water_np_dr_species_ranks)
summary(model.LCP)



ggplot(water_np_dr_species_ranks, aes(x = LCP , y = specialism, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,200))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
   theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = average_range - sd, ymax = average_range + sd), width = 0.1, position = position_dodge(0.9)) 

### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  





############ LSP ########################################################## 


LSP <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd(LSP, na.rm = TRUE),
    average_range = mean(LSP))
LSP


############# Summary of statistics ###########
model.LSP <- lm(LSP ~ specialism,  data = water_np_dr_species_ranks)
summary(model.LSP)


ggplot(water_np_dr_species_ranks, aes(x = specialism, y = LSP, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,2500))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
   theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = average_range - sd, ymax = average_range + sd), width = 0.1, position = position_dodge(0.9)) 


### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) +
  




############## chlorophyll ##########################################

chlorophyll <- water_np_dr_species_ranks %>% 
  group_by(specialism, Photobiont) %>% 
  summarise (
    sd = sd( chl, na.rm = TRUE),
    chlorophyll = mean(chl))
chlorophyll

############# Summary of statistics ###########
model.opt.chlorophyll  <- lm(chlorophyll  ~ specialism,  data = water_np_dr_species_ranks)
summary(model.opt.chlorophyll )


ggplot(water_np_dr_species_ranks, aes(x = specialism, y =  chlorophyll, colour = Photobiont, fill = Photobiont)) +
  geom_point(size = 4) +
  text(water_np_dr_species_ranks$Species, labels = water_np_dr_species_ranks$Species) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, show.legend = FALSE) +
  scale_color_manual(values = c("grey40","forestgreen","dodgerblue3")) +
  ylab(expression(Optimum~range~of~thallus~water~content~(mm~H[2]*O ~precipitation~equivalent))) +
  xlab("Species with increasing degree of association to the temperate rainforest") +
  coord_cartesian(xlim =c(1, 7), ylim = c(0,20))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18))  +
  theme(axis.text.y = element_text(size =16)) +
  theme(axis.text.x = element_text(size =16)) +
  scale_y_continuous(breaks=seq(0,1.3, by=0.1), limits = c(0,1.3), expand= c(0,0)) +
  scale_x_continuous(breaks=seq(1,7, by=1)) +
  theme(legend.key.size = unit(0.8, "cm")) +
  theme(legend.title = element_text(size =18))+
  theme(legend.position = c(0.14, 0.85)) +
  theme(legend.text=element_text(size=16)) +
  theme(plot.margin=unit(c(1,-0.1,0.25,0.15), "cm")) +
  geom_errorbar(aes(ymin = optimumpercent - sd, ymax =  optimumpercent + sd), width = 0.1, position = position_dodge(0.9)) 

### insert this to show p value  ##############

stat_fit_glance(method = 'lm', geom = 'text', aes(label = paste0('p = ', format(..p.value.., 0.3)))) 

#### this inserts (y=mx+c) equation ####

stat_regline_equation(aes(label = ..eq.label..)) +
  
  #### r2 values ########

stat_regline_equation(aes(label = ..rr.label..)) 
  

  






