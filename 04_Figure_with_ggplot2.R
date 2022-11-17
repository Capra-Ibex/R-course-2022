####~~~~~~~~~~~~~Header~~~~~~~~~~~~~~####
####
#### TITLE : Learning graphics in R with ggplot2
####
#### Auteur(s) : Raguet P.
####
#### Date creation : 2022-11-16
####
#### Date modification : 2022-11-16
####
#### Comments : Original author: Maxwell T.
####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### WORKING DIRECTORY ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# dir <- "directory"
# setwd(dir)
getwd()

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### PACKAGES ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
library(tidyverse)
# library(ggplot2) # ggplot2 is already in the tidyverse metapackage
# library(dplyr) # dplyr is already in the tidyverse metapackage

library(datasets)
library(ggpubr)

library(svglite)
library(ggThemeAssist)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### DATA IMPORT AND BUILD ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# clean the environment
rm(list = ls())

#download the two datasets (available from the package "datasets")
tab <- as.data.frame(Orange)
tab2 <- as.data.frame(nhtemp)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Color palette ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

newtheme_print<- 
  theme_classic()+
  theme(panel.border = element_rect(linetype = "solid", fill = "NA"))+
  theme(axis.title.x=element_text(angle=0, size=14, family="serif",color = "black"),
        axis.text.x = element_text(angle =0, size=12,family="serif",color = "black"),
        axis.title.y=element_text(angle=90, size=14, hjust=.5, vjust=.5,family="serif",color = "black"),
        axis.text.y = element_text(size=12,family="serif",color = "black"),
        legend.title=element_text(size=12, face="bold", hjust=.5, family="serif"),
        legend.text=element_text(size=14, family="serif"),
        legend.background = element_rect(linetype="solid", colour ="black"),
        legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.position = c(0.9, 0.2))

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Orange tree ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
str(tab)

# Research question: how does circumference change with age?

### Point graph
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree))
g

### Tree levels order ? ####
levels(tab$Tree)

## reorder factors
tab$Tree <- factor(tab$Tree, levels = c("1", "2", "3", "4", "5"))

# tab %>% 
#   mutate(Tree = fct_relevel(Tree, c("1", "2", "3", "4", "5")))

### Point graph with correct order ####
g2 <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree)) +
  scale_colour_manual(name = "Tree ID", values = cbPalette)
g2

### Manage points ####
g3 <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette)
g3

### Manage axes ####
g4 <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)") +
  theme_bw()
g4

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Temperature data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
str(tab2)

tab_temp <- data.frame(Year = 1912:1971, Temp = tab2$x)


ylab_name <- expression("Temperature "~( degree~F))

G <- ggplot(tab_temp, aes(x = Year, y = Temp)) +
  geom_line()+
  scale_y_continuous(name = ylab_name) +
  scale_x_continuous(name = "Year", limits = c(1912, 1971),
                     breaks = c(1920, 1930, 1940, 1950, 1960),
                     labels = c("1920", "1930", "1940", "1950", "1960")) +
  geom_text(x=1953, y=54.8, label="This was a warm year",
            size=5, color="black", family="serif")+
  theme_bw()
G

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### multiple graph ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
G_comp <- ggarrange(g4, G, labels = c("A", "B"),
                    # font.label = list (size=14,family="serif"),
                    ncol = 2, nrow = 1)
G_comp


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Average in ggplot ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
tab %>%
  group_by(age) %>%
  summarise_at(vars("circumference"),
               list(Mean = ~mean(.x),
                    se = ~sd(.x)/sqrt(n()))) %>%
  
  ggplot(aes(x=age, y=Mean))+
  geom_line() +
  geom_point(size=2.5,shape=16)+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),
                size= 0.3, width=15)+
  theme_bw()


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Save the ggplot ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
ggsave("Export/Fig_comp.pdf", G_comp, width = 8, height = 4)
ggsave("Export/Fig_comp.png", G_comp, width = 8, height = 4)
ggsave("Export/Fig_comp.svg", G_comp, width = 8, height = 4)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####  ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### TEST ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### TIPS & CLUES ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####





