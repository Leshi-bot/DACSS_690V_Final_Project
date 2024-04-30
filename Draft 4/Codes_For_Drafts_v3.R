library(tidyr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(DescTools)
library(sf)
library(stringr)
library(magrittr)
library(scales)

# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

forest_area = "forest_area_percent.csv"
mydata = read.csv(forest_area)

mydata_d1 <- mydata %>%
  select(Country.Name:Country.Code, X1992:X2021)

mydata_d1 <- mydata_d1[mydata_d1$Country.Name %in% c('High income', 'Upper middle income', 'Middle income', 
                                               'Lower middle income', 'Low income'),]

mydata_d1$Country.Name <- factor(mydata_d1$Country.Name, order = TRUE, 
                                    levels = c("Low income", "Lower middle income", "Middle income", 
                                               "Upper middle income", "High income"))


# see data ----------------------------------------------------------


head(mydata_d1)


# see data types ----------------------------------------------------------

str(mydata_d1)


# deliverable 1 ----------------------------------------------------------

title_text = 'Who owns our forests?'
source_text = 'Source: Kaggle, World Bank Data'
sub_title_text = 'Global Data - 1992 to 2021'
x_axis_text = 'Income Bracket'
y_axis_text = 'Forest %'

## Lollipop plot
mydata_d1$gap = mydata_d1$X2021 - mydata_d1$X1992

mydata_d1$PositiveGap = ifelse(mydata_d1$gap > 0, "Yes", "No")
mydata_d1


base = ggplot(mydata_d1, aes(x = Country.Name,
                           y = gap,
                           color = PositiveGap,
                           label = round(gap, 2)))#here

base = base + theme_classic()


lolliplot1 = base + geom_segment(aes(y = 0, 
                                   yend = gap, 
                                   x = Country.Name, 
                                   xend = Country.Name), 
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

lolliplot5 = lolliplot4 +  geom_label(aes(label=Country.Name),
                                      color = 'black ',
                                      size = 3,
                                      y = 0,
                                      show.legend = FALSE ) +
  theme(axis.text.x = element_blank())

lolliplot6 = lolliplot5 + labs(#title = title_text,
                     subtitle = sub_title_text,
                     x = NULL, #x_axis_text,
                     y = NULL) #y_axis_text,
                     #caption = source_text)

del1Draft = lolliplot6
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 -----------------------------------------------------------
mydata_d2 <- mydata[1:210,]

base = ggplot(mydata_d2, aes(x = 0, y = X2021)) +
  theme_classic()


## saving values
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


# prepare texts
title_text2 = 'Where are our forests?'
source_text2 = 'Source: Kaggle, World Bank Data'
sub_title_text2 = 'Global Data - 2021'
y_axis_text2 = 'Forest Mileage (millions)'

theVar = mydata_d2$X2021

upperT = summary(theVar)[[5]]
(numOutUp = sum(mydata_d2$X2021 > upperT, na.rm = T))

lowerT = summary(theVar)[[2]]
(numOutLw = sum(mydata_d2$X2021 < lowerT, na.rm = T))

annOutUN = paste0(numOutUp,' countries\nabove ',round(upperT, digits = 2), '%')
annOutLN = paste0(numOutLw,' countries\nbelow ',round(lowerT, digits = 2), '%')


# Violin plot
vio = base + geom_violin(trim = FALSE, fill = "orange")

viobox = vio + geom_boxplot(width = 0.2)

viobox = viobox  + coord_flip() +
  labs(#title=title_text2,
                        subtitle = sub_title_text2,
                        x = '',
                        y = y_axis_text2) +
                        #caption = source_text) +
  annotate(geom = 'text',
           label = annOutUN,
           y = upperT,
           x = 0.175,
           angle = 0) +
  annotate(geom = 'text',
           label = annOutLN,
           y = lowerT,
           x = -0.175,
           angle = 0) + 
  theme(axis.text.y = element_blank(), # no text on y
        axis.ticks.y = element_blank(),# no ticks on y
        axis.line.y = element_blank()) # no line on y

del2Draft = viobox
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

# Loading in US  population data
## Monthly Civilian non-institutional population is defined as persons 16 years 
## of age and older residing in the 50 states and the District of Columbia, who 
## are not inmates of institutions (e.g., penal and mental facilities, homes for 
## the aged), and who are not on active duty in the Armed Forces.
## source: https://fred.stlouisfed.org/series/CNP16OV

yr_pop_levels = 'CNP16OV.csv'
pop_data = read.csv(yr_pop_levels)

pop_data = pop_data[grep('-12-01', pop_data$DATE), ]
colnames(pop_data) <- c('year', 'population')
pop_data <- pop_data[45:74,]
row.names(pop_data) <- NULL

yearly_pop = pop_data %>%
  mutate(year = 
           str_remove(year, '-12-01')) %>%
  mutate(population = 
           population * 1000) 

# Decimal percentages pulled from 
# https://www.weforum.org/agenda/2022/07/household-income-distribution-wealth-inequality-united-states/
yearly_pop$high_income_pop = yearly_pop$population * .181
yearly_pop$upper_mid_income_pop = yearly_pop$population * .197
yearly_pop$middle_income_pop = yearly_pop$population * .287
yearly_pop$lower_mid_income_pop = yearly_pop$population * .153
yearly_pop$low_income_pop = yearly_pop$population * .183

summary(yearly_pop)

# divide forest coverage area by world economic class distribution, then do the  
# same to yearly population. Finally, divide each classes forest coverage area 
# by each of their approximate yearly populations
forest_area_km = "forest_area_km.csv"
mydata_km = read.csv(forest_area_km)

mydata_mi = mydata_km %>%
  select(Country.Name:Country.Code, X1992:X2021) %>%
  pivot_longer(
    cols = 'X1992':'X2021',
    names_to = 'year',
    values_to = 'forest_mileage') %>%
  mutate(forest_mileage = forest_mileage * 0.621371) %>%
  pivot_wider(
    names_from = 'year',
    values_from = 'forest_mileage')

US_data_mi <- mydata_mi[mydata_km$Country.Name %in% c('United States'),]

colnames(US_data_mi) = sub('X', '', colnames(US_data_mi))


econ_class_data = mydata

colnames(econ_class_data) = sub("X", "", colnames(econ_class_data))



econ_class_data = econ_class_data[econ_class_data$Country.Name %in% 
                                    c('High income', 'Upper middle income', 'Middle income', 
                                      'Lower middle income', 'Low income'),]

econ_class_data$Country.Name = factor(econ_class_data$Country.Name, order = TRUE, 
                                      levels = c("Low income", "Lower middle income", "Middle income", 
                                                 "Upper middle income", "High income"))

econ_class_data$Country.Code = factor(econ_class_data$Country.Code, order = TRUE, 
                                      levels = c("LIC", "LMC", "MIC", 
                                                 "UMC", "HIC"))

class_data = econ_class_data %>%
  select(Country.Name:Country.Code, '1992':'2021') %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'decimal') %>%
  mutate(decimal = decimal / 100) %>%
  pivot_wider(
    names_from = name,
    values_from = decimal)


upper = class_data[class_data$Country.Name %in% 
                                    c('High income'),]
upper = upper %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'decimal')

up_mid = class_data[class_data$Country.Name %in% 
                     c('Upper middle income'),]
up_mid = up_mid %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'decimal')

mid = class_data[class_data$Country.Name %in% 
                     c('Middle income'),]
mid = mid %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'decimal')

low_mid = class_data[class_data$Country.Name %in% 
                     c('Lower middle income'),]
low_mid = low_mid %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'decimal')

low = class_data[class_data$Country.Name %in% 
                     c('Low income'),]
low = low %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'decimal')

class_mileage = US_data_mi %>%
  pivot_longer(
    cols = '1992':'2021',
    values_to = 'forest_mileage')

class_mileage$high_income_mi = upper$decimal
class_mileage$upper_mid_income_mi = up_mid$decimal
class_mileage$middle_income_mi = mid$decimal
class_mileage$lower_mid_income_mi = low_mid$decimal
class_mileage$low_income_mi = low$decimal
 
class_mileage = class_mileage %>%
  mutate(high_income_mi = high_income_mi * forest_mileage) %>%
  mutate(upper_mid_income_mi = upper_mid_income_mi * forest_mileage) %>%
  mutate(middle_income_mi = middle_income_mi * forest_mileage) %>%
  mutate(lower_mid_income_mi = lower_mid_income_mi * forest_mileage) %>%
  mutate(low_income_mi = low_income_mi * forest_mileage) 

class_mileage_pop = class_mileage

class_mileage_pop$total_population = yearly_pop$population
class_mileage_pop$high_income_pop = yearly_pop$high_income_pop
class_mileage_pop$upper_mid_income_pop = yearly_pop$upper_mid_income_pop
class_mileage_pop$middle_income_pop = yearly_pop$middle_income_pop
class_mileage_pop$lower_mid_income_pop = yearly_pop$lower_mid_income_pop
class_mileage_pop$low_income_pop = yearly_pop$low_income_pop

names(class_mileage_pop)

str(class_mileage_pop, width = 50, strict.width='cut')



HIC = class_mileage_pop %>%
  select(name, high_income_mi) %>%
  mutate(high_income_mi = high_income_mi / (class_mileage_pop$high_income_pop/1000))

HIC$class = 'High Income'

# ratio will refer to square mileage per 1000 people
colnames(HIC) = sub('high_income_mi', 'ratio', colnames(HIC))
HIC = pivot_wider(HIC,
  names_from = name,
  values_from = ratio)



UMC = class_mileage_pop %>%
  select(name, upper_mid_income_mi) %>%
  mutate(upper_mid_income_mi = upper_mid_income_mi / (class_mileage_pop$upper_mid_income_pop/1000))

UMC$class = 'Upper Middle Income'
colnames(UMC) = sub('upper_mid_income_mi', 'ratio', colnames(UMC))
UMC = pivot_wider(UMC,
                  names_from = name,
                  values_from = ratio)


MIC = class_mileage_pop %>%
  select(name, middle_income_mi) %>%
  mutate(middle_income_mi = middle_income_mi / (class_mileage_pop$middle_income_pop/1000))

MIC$class = 'Middle Income'
colnames(MIC) = sub('middle_income_mi', 'ratio', colnames(MIC))
MIC = pivot_wider(MIC,
                  names_from = name,
                  values_from = ratio)


LMC = class_mileage_pop %>%
  select(name, lower_mid_income_mi) %>%
  mutate(lower_mid_income_mi = lower_mid_income_mi / (class_mileage_pop$lower_mid_income_pop/1000))

LMC$class = 'Lower Middle Income'
colnames(LMC) = sub('lower_mid_income_mi', 'ratio', colnames(LMC))
LMC = pivot_wider(LMC,
                  names_from = name,
                  values_from = ratio)


LIC = class_mileage_pop %>%
  select(name, low_income_mi) %>%
  mutate(low_income_mi = low_income_mi / (class_mileage_pop$low_income_pop/1000))

LIC$class = 'Low Income'
colnames(LIC) = sub('low_income_mi', 'ratio', colnames(LIC))
LIC = pivot_wider(LIC,
                  names_from = name,
                  values_from = ratio)

class_ratios = rbind(HIC, UMC, MIC, LMC, LIC)
class_ratios$class = factor(class_ratios$class, order = TRUE, 
                            levels = c("Low Income", "Lower Middle Income", "Middle Income", 
                                       "Upper Middle Income", "High Income"))

names(class_ratios)

str(class_ratios, width = 50, strict.width='cut')



pivot_ratios = class_ratios %>%
  select(class, '1993', '2000', '2007', '2014', '2021') %>%
  pivot_longer(
    cols = '1993':'2021',
    names_to = 'year',
    values_to = 'ratio')


bivariate_base = ggplot(data = pivot_ratios,
             aes(x = year, y = ratio,
                 fill = class)) + theme_minimal()

Dodge_graph = bivariate_base +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_text(aes(label = round(ratio, digits = 0)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +  # Adjust vertical position of labels
  labs(#title = "How much room do our forests have?",
       subtitle = "Forest coverage by year (every 7 years)",
       x = "",
       y = "sq miles / 1000 people",
       fill = "class") +
       #caption = "Source: Kaggle, World Bank Data, Federal Reserve Economic Data, World Economic Forum") +
  theme_minimal()

del3Draft = Dodge_graph
del3Draft 



# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

# file sourced from https://hub.arcgis.com/datasets/esri::world-countries/explore
global_map = st_read('World_Countries_(Generalized).gpkg')
global_map = global_map[-c(7, 8, 16, 28, 31, 33, 43, 49, 50, 55, 75, 80, 82,
                           89, 93, 96, 101, 115, 117, 133, 140, 143, 147, 150,
                           155, 163, 164, 178, 182, 183, 187, 188, 189, 190, 
                           194, 211, 218, 227, 240, 245, 248), ]
row.names(global_map) <- NULL

head(global_map)

names(global_map)

base = ggplot(data = global_map)
base + geom_sf()



map_data_diff = mydata_mi %>%
  select(Country.Name, X1992, X2012, X2021)
map_data_diff <- map_data_diff[1:211,]
colnames(map_data_diff) = sub("X", "mi", colnames(map_data_diff))

str(global_map$COUNTRY)
str(map_data_diff$Country.Name)

map_data_diff$Country.Name[14] <- 'Bahamas'
map_data_diff$Country.Name[45] <- 'Congo DRC'
map_data_diff$Country.Name[46] <- 'Congo'
map_data_diff$Country.Name[48] <- "Côte d'Ivoire"
map_data_diff$Country.Name[53] <- "Czech Republic"
map_data_diff$Country.Name[59] <- "Egypt"
map_data_diff$Country.Name[72] <- "Gambia"
map_data_diff$Country.Name[91] <- "Iran"
map_data_diff$Country.Name[103] <- "North Korea"
map_data_diff$Country.Name[104] <- "South Korea"
map_data_diff$Country.Name[106] <- "Kyrgyzstan"
map_data_diff$Country.Name[107] <- "Laos"
map_data_diff$Country.Name[126] <- "Micronesia"
map_data_diff$Country.Name[167] <- "Sint Maarten"
map_data_diff$Country.Name[168] <- "Slovakia"
map_data_diff$Country.Name[176] <- "Saint Kitts and Nevis"
map_data_diff$Country.Name[177] <- "Saint Lucia"
map_data_diff$Country.Name[178] <- "Saint Martin"
map_data_diff$Country.Name[179] <- "Saint Vincent and the Grenadines"
map_data_diff$Country.Name[184] <- "Syria"
map_data_diff$Country.Name[205] <- "Venezuela"
map_data_diff$Country.Name[206] <- "Vietnam"
map_data_diff$Country.Name[207] <- "US Virgin Islands"
map_data_diff$Country.Name[208] <- "Palestinian Territory"
map_data_diff$Country.Name[209] <- "Yemen"



map_diff1 = map_data_diff %>%
  select(Country.Name, mi2012)
map_diff1$year = 2012
map_diff1$percent_diff = (map_diff1$mi2012 / map_data_diff$mi1992) * 100
map_diff1 = pivot_wider(map_diff1,
                  names_from = year,
                  values_from = mi2012)
map_diff1 = pivot_longer(map_diff1,
                         cols = '2012',
                         names_to = 'year',
                         values_to = 'mileage')

map_diff2 = map_data_diff %>%
  select(Country.Name, mi2021)
map_diff2$year = 2021
map_diff2$percent_diff = (map_diff2$mi2021 / map_data_diff$mi1992) * 100
map_diff2 = pivot_wider(map_diff2,
                        names_from = year,
                        values_from = mi2021)
map_diff2 = pivot_longer(map_diff2,
                         cols = '2021',
                         names_to = 'year',
                         values_to = 'mileage')

map_data_yr <- rbind(map_diff1, map_diff2)

# Maybe get average increase/decrease instead of percent difference? 
mi_map_yr = merge(global_map, map_data_yr,
                    by.x = 'COUNTRY', by.y = 'Country.Name')





head(mi_map_yr)

base = ggplot(data = global_map) + theme_light() + geom_sf(fill = 'red') 


del4Draft = base + geom_sf(data = mi_map_yr,
                         aes(fill = percent_diff)) + 
  scale_fill_viridis_c(direction = -1) +
  facet_grid(~year)
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")
