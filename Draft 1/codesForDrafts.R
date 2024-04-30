library(tidyr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(DescTools)

# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

forest_area = "forest_area_percent.csv"
mydata=read.csv(forest_area)

mydata2 <- mydata %>%
  select(Country.Name:Country.Code, X1992:X2021)

mydata3 <- mydata2[mydata2$Country.Name %in% c('High income', 'Upper middle income', 'Middle income', 
                                               'Lower middle income', 'Low income'),]

mydata_world <- mydata2[mydata2$Country.Name %in% c('World'),]


# see data ----------------------------------------------------------


head(mydata3)


# see data types ----------------------------------------------------------

str(mydata3)

# deliverable 1 ----------------------------------------------------------

### Bar plot
LABELS = paste0(round(mydata3$X2021,2), '%')
base = ggplot(data = mydata3,
             aes(x = reorder(Country.Name, X2021),
                 y = X2021))
base = base + theme_classic()

plot1 = base + geom_bar (fill = 'gray',
                         stat = 'identity')

title_text = 'World Forest Area % according to Income'
source_text = 'Source: Kaggle, World Bank Data'
sub_title_text = 'Global Data - 2021'
x_axis_text = 'Income Bracket'
y_axis_text = 'Forest %'

plot2 = plot1 + labs(title=title_text,
                     subtitle = sub_title_text,
                     x = NULL, #x_axis_text
                     y = y_axis_text,
                     caption = source_text)

plot3 = plot2 + geom_hline(yintercept = 31.17705, #where
                           linetype="dashed", 
                           linewidth=1, #thickness
                           alpha=0.5) #transparency

plot4 = plot3 + scale_y_continuous(breaks=c(0,25,50),
                                   limits = c(0, 50), 
                                   labels=unit_format(suffix = '%')) 

plot5 = plot4 + geom_text(vjust=0,
                          size = 5,
                          aes(y = X2021 ,
                              label = LABELS))
plot5

### Lollipop plot
mydata3$gap=mydata3$X2021-mydata3$X1992
mydata3

base = ggplot(mydata3, aes(x = reorder(Country.Name,X2021),
                           y = gap)) 
base = base + theme_classic()

#new:geom_segment
lolliplot1 = base + geom_segment(aes(y = 0, 
                                   yend = gap,
                                   x = reorder(Country.Name,X2021),
                                   xend = reorder(Country.Name,X2021)), 
                               color = "gray") 

lolliplot2 = lolliplot1 + geom_point(size=5)
lolliplot2


mydata3$PositiveGap = ifelse(mydata3$gap > 0, "Yes", "No")
mydata3


base = ggplot(mydata3, aes(x = reorder(Country.Name,X2021),
                             y = gap,
                             color = PositiveGap,
                             label = round(gap, 2)))#here

base = base + theme_classic()
#base= base + scale_x_discrete(limits=localesOrd)

lolliplot1=base + geom_segment(aes(y = 0, 
                                   yend = gap, 
                                   x = reorder(Country.Name,X2021), 
                                   xend = reorder(Country.Name,X2021)), 
                               color = "gray") 

lolliplot2 = lolliplot1 + geom_point() + geom_text(nudge_x = 0.3, show.legend = F)

lolliplot3 = lolliplot2 + geom_hline(yintercept = 0)

lolliplot4 = lolliplot3 + theme(axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.line.y = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.title.x = element_blank(),
                                axis.line.x = element_blank(),
                                legend.position = c(0.92,0.1),
                                legend.background = element_rect(fill = 'grey95'))

lolliplot5 = lolliplot4 +  geom_label(aes(label = Country.Name),
                                      color = 'black ',
                                      size = 3,
                                      y = 0,
                                      show.legend = FALSE ) +
  theme(axis.text.x = element_blank())


title_text = '% Change in World Forest Area according to Income'
source_text = 'Source: Kaggle, World Bank Data'
sub_title_text = 'Global Data - 1992 to 2021'
x_axis_text = 'Income Bracket'
y_axis_text = 'Forest %'


lolliplot6 = lolliplot5 + labs(title=title_text,
                     subtitle = sub_title_text,
                     x = NULL, #x_axis_text,
                     y = NULL, #y_axis_text,
                     caption = source_text)

del1Draft = lolliplot6
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 -----------------------------------------------------------
forest_area_km = "forest_area_km.csv"
mydata_km = read.csv(forest_area_km)

mydata_d2 <- mydata_km[1:211,]
mydata_d2 <- mydata_d2[-c(76),]

base= ggplot(mydata_d2,aes(y = X2021))  #var to plot in 'y' 
base + geom_boxplot()

# -------------------------------------------------------------------------
(statVals = summary(mydata_d2$X2021, digits = 2)[1:6])
#(statVals = round(summary(mydata_d2$X2021, digits = 3), digits = 1)[1:6])
statVals = statVals %>% as.vector() #notice '%>%'

base = ggplot(mydata_d2,aes(y = X2021))  
b1 = base + geom_boxplot() 
b1 = b1 + scale_y_continuous(breaks = statVals) #custom breaks

b1 = b1 + coord_flip()
b1


(upperT = ggplot_build(b1)$data[[1]]$ymax)

(numOutliers = sum(mydata_d2$X2021 > upperT, na.rm = T))


# text for annotations
txtOutliers=paste0('#Outlying countries: ',numOutliers)
txtUpper=paste0('Threshold:',round(upperT))

b1_vertical = b1 + geom_hline(yintercept = upperT,
                            color = 'red',
                            linetype = "dotted",
                            size = 2) 
b1_annot=b1_vertical + annotate(geom = 'text',
                                label = txtUpper,
                                y = upperT+150000,
                                x = 0.2,
                                angle = 90) # text angle

b1_annot=b1_annot + annotate(geom = 'text',
                                label = txtOutliers,
                                y = upperT+2000000,
                                x = 0.1,
                                angle = 0)
b1_annot


b1_annot_noX = b1_annot + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
b1_annot_noX


b1_newGrid = b1_annot_noX + theme_classic()
b1_newGrid


b1_better_axisText = b1_newGrid + theme(axis.text.x = element_text(angle = 60,
                                                                  size = 7,
                                                                  vjust = 0.5))
b1_better_axisText

# -------------------------------------------------------------------------
# standard deviation:
sd(mydata_d2$X2021, na.rm = T)

# median absolute deviation:
mad(mydata_d2$X2021, na.rm = T)

# coefficient of variation
CoefVar(mydata_d2$X2021,
        na.rm = T)

# asymmetry
Skew(mydata_d2$X2021,
     na.rm = T)

# kurtosis
Kurt(mydata_d2$X2021,
     na.rm = T)

# confidence interval for the mean
MeanCI(mydata_d2$X2021,
       na.rm = T)

# saving values
cv = CoefVar(mydata_d2$X2021, na.rm = T)
sd = SD(mydata_d2$X2021, na.rm = T)
md = Median(mydata_d2$X2021, na.rm = T)
mn = Mean(mydata_d2$X2021, na.rm = T)
mn.low = MeanCI(mydata_d2$X2021,
              na.rm = T)[['lwr.ci']]
mn.up = MeanCI(mydata_d2$X2021,
             na.rm = T)[['upr.ci']]
sk = Skew(mydata_d2$X2021,
        na.rm = T)

# -------------------------------------------------------------------------
mydata_d2$X2021mi = ((mydata_d2$X2021)/1.609)
mydata_d2$X2021mi2 = ((mydata_d2$X2021mi)/1000000)

base = ggplot(mydata_d2, aes(x = 0, y = X2021mi2)) +
  theme_classic()

vio = base + geom_violin(trim = FALSE, fill = "orange")

viobox = vio + geom_boxplot(width = 0.2)
viobox = viobox + coord_flip()
viobox 

theVar = mydata_d2$X2021mi2
theIQR = IQR(theVar, na.rm = T)
upperT = summary(theVar)[[5]] + theIQR*1.5
lowerT = summary(theVar)[[2]] - theIQR*1.5

# top
(numOutUp = sum(mydata_d2$X2021mi2 > upperT, na.rm = T))

# bottom
(numOutLw = sum(mydata_d2$X2021mi2 < lowerT, na.rm = T))

# prepare texts:
annOutUN = paste0(numOutUp,' countries\nabove ',round(upperT, digits = 2))
annOutLN = paste0(numOutLw,' countries\nbelow ',round(lowerT, digits = 2))

title_text2 = 'Forest Mileage per Country'
source_text2 = 'Source: Kaggle, World Bank Data'
sub_title_text2 = 'Global Data - 2021'
y_axis_text2 = 'Forest Mileage (millions)'

# plotting
viobox = viobox  + labs(title=title_text2,
                        subtitle = sub_title_text2,
                        x = '',
                        y = y_axis_text2, #y_axis_text,)
                        caption = source_text) + 
  annotate(geom = 'text',
                  label = annOutUN,
                  y = upperT + .35,
                  x = 0.1,
                  angle = 0) + 
  theme(axis.text.y = element_blank(), # no text on y
        axis.ticks.y = element_blank(),# no ticks on y
        axis.line.y = element_blank()) # no line on y

del2Draft = viobox
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

del3Draft = base + geom_point(aes(x = Student.Teacher.Ratio,
                                 y = Free.Lunch))
del3Draft 

# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("WA_County_Boundaries.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

base=ggplot(myMapLunch)
del4Draft=base + geom_sf(aes(fill=Free.Lunch))
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")