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

#download the two datasets (available from the package "datasets", already in R)
tab <- as.data.frame(Orange)
tab2 <- as.data.frame(nhtemp)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Orange tree ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# look at the structure of your data
str(tab)

### Research question: how does circumference change with age?

### Basic ggplot - setting up a canvas on which to add your work ####
# aes = aesthetics are a group of parameters that specify what and how data is displayed
g <- ggplot(tab, aes(x=age, y=circumference)) 
g

### once your canvas is ready, you can add different layers:

### Point graph ####
# Use a 'geom_' function to represent data points
# use the geom's aesthetic properties to represent variables.
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree))
# scatterplot 
# aes --> without "aes", ggplot will not group them by the Tree factor
g

## Tree levels order ?
levels(tab$Tree)
# use the 'factor' function to reorder factors
tab$Tree <- factor(tab$Tree, levels = c("1", "2", "3", "4", "5"))

# OR uncomment the following lines (fct_relevel function):

# tab %>% 
#   mutate(Tree = fct_relevel(Tree, c("1", "2", "3", "4", "5")))

## Point graph with correct order
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree))
g

### Manage points color, shape, fill ####
## colour
# build your own palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree)) +
  scale_colour_manual(name = "Tree ID", values = cbPalette)
g

# some usful websites:
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
# https://mycolor.space/
# http://www.proftnj.com/RGB3.htm

## shape and fill
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette)
g

# however, only use color when necessary
# here, we can easily give the information in black and white
# always check if the information is understandable in black and white

### Name axis ####
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)")
g

### Theme in ggplot ####
## Preset themes
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)") +
  theme_bw()
g

## Start to personalize
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)") +
  theme_bw() +
  theme(panel.border = element_rect(linetype = "solid", fill = "NA")) + # box drawn around the figure
  # now to manually edit the axes
  # angle = 0 horizontoal; angle = 30 sideways, =90 vertical
  # size= font size - increase this for presentations 
  # color= "black" - R uses a dark grey by deffault
  # family = "serif" for Times New Roman, "sans" for Arial, "mono" for Courrier monospaced
  # hjust = 0.5 in the middle
  theme(axis.title.x=element_text(angle=0, size=14, family="serif",color = "black"), # x-axis title
        axis.text.x = element_text(angle =0, size=12,family="serif",color = "black"), # x-axis text 
        axis.title.y=element_text(angle=90, size=14, hjust=.5, vjust=.5,family="serif",color = "black"), # y-axis title
        axis.text.y = element_text(size=12,family="serif",color = "black")) # y-axis text 
g

## Personalizing the legend
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)") +
  # if you want to change the legend title, you do it in all places where it is called:
  # in the scale_shape_manual(name=" "), scale_shape_fill(name=" ") AND labs( fill = " ")
  theme_bw()+
  theme(panel.border = element_rect(linetype = "solid", fill = "NA"))+
  theme(axis.title.x=element_text(angle=0, size=14, family="serif",color = "black"), # x-axis title
        axis.text.x = element_text(angle =0, size=12,family="serif",color = "black"), # x-axis text 
        axis.title.y=element_text(angle=90, size=14, hjust=.5, vjust=.5,family="serif",color = "black"),
        axis.text.y = element_text(size=12,family="serif",color = "black"), 
        # continuing with the theme, can specify the theme for the legend 
        legend.title=element_text(size=12, face="bold", hjust=.5, family="serif"), # legend title
        legend.text=element_text(size=14, family="serif"), # legend text (the tree numbers)
        legend.background = element_rect(linetype="solid", colour ="black"), # box around the legend
        legend.key.size = unit(0.5,"cm"), # legend height 
        legend.key.width = unit(0.8, "cm"), # legend width
        legend.position = c(0.9, 0.2))# position of legend within the figure 
g

## Saving your theme:
# NOTE: you can only save items which all fit within the "theme()" function
# for example, can't save + labs() 
newtheme_print<- theme_classic()+
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

# lets also make a theme for presentations with larger text
newtheme_pres <- theme_classic()+
  theme(panel.border = element_rect(linetype = "solid", fill = "NA"))+
  #increase all font sizes 
  theme(axis.title.x=element_text(angle=0, size=20, family="serif",color = "black"), 
        axis.text.x = element_text(angle =0, size=18,family="serif",color = "black"), 
        axis.title.y=element_text(angle=90, size=20, hjust=.5, vjust=.5,family="serif",color = "black"),
        axis.text.y = element_text(size=18,family="serif",color = "black"), 
        legend.title=element_text(size=18, face="bold", hjust=.5, family="serif"), 
        legend.text=element_text(size=20, family="serif"), 
        legend.background = element_rect(linetype="solid", colour ="black"), 
        legend.key.size = unit(0.5,"cm"), 
        legend.key.width = unit(0.8, "cm"), 
        legend.position = c(0.9, 0.2))

# looking at our new themes with our previous script
# theme for print
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)")+
  newtheme_print
g

# theme for presentations (larger text)
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)")+
  newtheme_pres
g

### Axis scale ###
g <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  labs(y = "Circumference (mm)", x = "Age (days)")+
  newtheme_pres +
  # the name will overright what was written in your "labs"line
  # limits = c(,) sets the minimum and maximum of your axes
  # expand = c(0,0) places the lower limit of x and y at the bottom left corner
  # because we set our lower limits to 0, this is the 0,0 --> important for reading your graph
  # breaks - you can set where the tick marks will be and what the text below it should say
  # the break numbers refer to the data which ggplot for x (here, the age in days) and for y (the circumference in mm)
  # you can change the tick labels using " "
  scale_x_continuous(name="Age (years)", limits=c(0,1700), expand=c(0,0), breaks=c(0,365,730,1095,1450), labels=c("0","1", "2", "3","4"))+
  scale_y_continuous(name="Tree trunk circumference (cm)",limits=c(0,220),  expand=c(0,0),
                     breaks=c(0,50,100,150,200), labels=c("0","5", "10", "15","20"))
g

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Temperature data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# look at the structure of your data
str(tab2)
# include date and temperature in a data frame
tab_temp <- data.frame(Year = 1912:1971, Temp = tab2$x)

### Customize axis ###
G <- ggplot(tab_temp, aes(x = Year, y = Temp)) +
  geom_line()+
  scale_y_continuous(name = "Temperature (F)") +
  scale_x_continuous(name = "Year", limits = c(1912, 1971),
                     breaks = c(1920, 1930, 1940, 1950, 1960),
                     labels = c("1920", "1930", "1940", "1950", "1960")) +
  newtheme_pres
G

## Adding special symbols to graph axes
ylab_name <- expression("Temperature "~( degree~F))

G <- ggplot(tab_temp, aes(x = Year, y = Temp)) +
  geom_line()+
  scale_y_continuous(name = ylab_name) + # using the name preset
  scale_x_continuous(name = "Year", limits = c(1912, 1971),
                     breaks = c(1920, 1930, 1940, 1950, 1960),
                     labels = c("1920", "1930", "1940", "1950", "1960")) +
  newtheme_pres
G

### Adding text to graph ####
G <- ggplot(tab_temp, aes(x = Year, y = Temp)) +
  geom_line()+
  scale_y_continuous(name = ylab_name) +
  scale_x_continuous(name = "Year", limits = c(1912, 1971),
                     breaks = c(1920, 1930, 1940, 1950, 1960),
                     labels = c("1920", "1930", "1940", "1950", "1960")) +
  geom_text(x=1953, y=54.8, label="This was a warm year",
            size=5, color="black", family="serif")+
  newtheme_pres
G

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### multiple graph ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
G_comp <- ggarrange(g, G, labels = c("A", "B"),
                    # font.label = list (size=14,family="serif"),
                    ncol = 2, nrow = 1)
G_comp

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Average in ggplot ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# in dplyr/tidyverse coding mod
g <- tab %>%
  group_by(age) %>%
  summarise_at(vars("circumference"),
               list(Mean = ~mean(.x),
                    se = ~sd(.x)/sqrt(n()))) %>%
  
  ggplot(aes(x=age, y=Mean))+
  geom_line() +
  geom_point(size=2.5,shape=16)+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),
                size= 0.3, width=15)+
  newtheme_print+
  scale_x_continuous(name="Age (years)", limits=c(0,1700), expand=c(0,0), breaks=c(0,365,730,1095,1450), labels=c("0","1", "2", "3","4"))+
  scale_y_continuous(name="Tree trunk circumference (cm)",limits=c(0,220),  expand=c(0,0),
                     breaks=c(0,50,100,150,200), labels=c("0","5", "10", "15","20"))+
  labs(title="Average circumference of five trees")+
  theme(plot.title = element_text(angle=0, size=18, family="serif",color = "black",hjust=0.5))
g

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### fitting a logistical regression ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# logistical model fitted to the mean of the trees;
# could also do this per individual tree
cdata <- tab %>%
  group_by(age) %>%
  summarise_at("circumference",
               list(mean = ~mean(.x),
                    se = ~sd(.x)/sqrt(n())))
               

logistical_mod <- nls(mean ~ SSlogis(age, Asym, xmid, scal),
                      data = cdata)
logistical_mod

mod.predict <- cbind(data=cdata, 
                     predict(logistical_mod, interval = 'confidence'))
colnames(mod.predict) <- c("Age","mean","se","Predicted_values")
mod.predict

l <- ggplot(tab, aes(x = age, y = circumference)) +
  geom_point(aes(fill = Tree, shape = Tree), size = 3) +
  scale_shape_manual(name = "Tree ID", values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(name = "Tree ID", values = cbPalette) +
  newtheme_pres +
  scale_x_continuous(name="Age (years)", limits=c(0,1700), expand=c(0,0), breaks=c(0,365,730,1095,1450), labels=c("0","1", "2", "3","4"))+
  scale_y_continuous(name="Tree trunk circumference (cm)",limits=c(0,220),  expand=c(0,0),
                     breaks=c(0,50,100,150,200), labels=c("0","5", "10", "15","20"))+
  #here is where we add the model data - need to specify that we're using data not from the original data frame called in the script
  geom_line(data=mod.predict, aes(x=Age, y=Predicted_values))
l

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### Save the ggplot ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
## Several options
# device = "tiff", "png", "jpeg","bmp": bit-map, pixel-based
# device = "pdf", "svg", "eps" can be editer later (in Inkscape, for example)

#this will save in your working directory

# ggsave("Figure_juxtaposition.pdf",plot = figure,device="pdf") 
# caution with legend placement / bordering parts of your graph may be displaced

# ggsave("Figure_logmod.emf", plot = p,  width=6, height=6, dpi=300, device="emf") 
# can adjust width, height, dpi = resolution

ggsave("Export/Fig_comp.pdf", G_comp, width = 8, height = 4)
ggsave("Export/Fig_comp.png", G_comp, width = 8, height = 4)
ggsave("Export/Fig_comp.svg", G_comp, width = 8, height = 4)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### FOR WINDOWS USERS ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Installation for windows only, not essential
library(rvg) 
library(svglite)
library(officer)
library(extrafont)

# options for those without image editors: saving within Microsoft Powerpoint

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
myplot <- figure
doc <- ph_with_vg(doc, code = print(myplot) , type = "body")
print(doc, target= "Final_figure.pptx")
# high quality image output 

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### TIPS & CLUES ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####





