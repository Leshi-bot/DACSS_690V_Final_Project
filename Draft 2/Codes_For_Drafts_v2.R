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

#mydata_world <- mydata2[mydata2$Country.Name %in% c('World'),]


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

lolliplot6 = lolliplot5 + labs(title = title_text,
                     subtitle = sub_title_text,
                     x = NULL, #x_axis_text,
                     y = NULL, #y_axis_text,
                     caption = source_text)

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
  labs(title=title_text2,
                        subtitle = sub_title_text2,
                        x = '',
                        y = y_axis_text2, #y_axis_text,)
                        caption = source_text) +
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

### Drafts with reduced year coverage
mydata_d3 <- mydata %>%
  select(Country.Name:Country.Code, X2016:X2021)

mydata_d3 <- mydata_d3[mydata_d3$Country.Name %in% c('High income', 'Upper middle income', 'Middle income', 
                                                     'Lower middle income', 'Low income'),]

mydata_d3$Country.Name <- factor(mydata_d3$Country.Name, order = TRUE, 
                                 levels = c("Low income", "Lower middle income", "Middle income", 
                                            "Upper middle income", "High income"))

mydata_d3$Country.Code <- factor(mydata_d3$Country.Code, order = TRUE, 
                                 levels = c("LIC", "LMC", "MIC", 
                                            "UMC", "HIC"))
mydata_d3 = mydata_d3 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "coverage",
    values_drop_na = TRUE
  )

mydata_d3$year = as.numeric(mydata_d3$year)


str(mydata_d3,width = 70,strict.width='cut')

summary(mydata_d3$year)

mydata_d3 = mydata_d3[complete.cases(mydata_d3),]

tapply(mydata_d3$coverage,mydata_d3$Country.Code, summary)

# Box plot drafts
base_d3 = ggplot(data = mydata_d3,
                aes(x = Country.Code, y = coverage))

box_d3 = base_d3 + geom_boxplot() + labs(title = 'yearly coverage')
box_d3


base_d3 = ggplot(data = mydata_d3,
                   aes(x = coverage, y = Country.Code)) 

box_d3 = base_d3 + geom_boxplot() + labs(title = 'yearly coverage')
box_d3

base_box = ggplot(data = mydata_d3,
                  aes(y = coverage))

base_box + geom_boxplot(aes(x = reorder(Country.Code, coverage , median))) +
  coord_flip()


# Probability testing
kruskal.test(coverage ~ Country.Code, data = mydata_d3)

pairwise.wilcox.test(mydata_d3$coverage, mydata_d3$Country.Code,
                     p.adjust.method = "BH")


# Density plot draft
ggplot(mydata_d3) + geom_density(aes(x = coverage), show.legend = F) + 
  facet_grid(reorder(Country.Code, coverage, median)~.) 


# Histogram plot draft
baseHist = ggplot(data = mydata_d3,
                aes(x = coverage))

baseHist + geom_histogram() +facet_grid(reorder(Country.Code, coverage, median)~.)


# Mean erro plot drafts
base_means = ggplot(mydata_d3, aes(x = Country.Code,
                                    y = coverage)) 
point_means = base_means + geom_point(stat = "summary") 
point_means

error_plot = point_means + geom_errorbar(stat = "summary") 
error_plot

jitter_means = base_means + geom_jitter(colour = "blue",
                                     alpha = 0.2 #transparency
)
jitter_means = jitter_means + geom_point(stat = "summary") +
  geom_errorbar(stat = "summary", width = .2)
jitter_means 


# Violin plot drafts
vio = base_box + geom_violin(aes(x = Country.Code), fill = "magenta", trim = F)
vio + coord_flip() 

vio2 = vio + coord_flip() + 
  scale_y_continuous(breaks = c(20,25,40)) + 
  theme(panel.grid.minor = element_blank())
vio2


# Bar plot drafts
summary_by = aggregate(data = mydata_d3,
                    coverage ~ Country.Code,
                    FUN = function(x) c(max = max(x),
                                        min = min(x)) )
# when several functions at play
summary_by = do.call(data.frame, summary_by)
summary_by

names(summary_by) = c('Country.Code','max', 'min')

base = ggplot(data = summary_by, aes(x = Country.Code))
base + geom_point(aes(y = min),color='blue') +
  geom_point(aes(y = max),color='red')

summary_by_long = reshape2::melt(summary_by, variable.name = 'stats',
                              value.name = 'coverage',
                              id.vars = 'Country.Code')
summary_by_long

levels(summary_by_long$stats)

summary_by_long$stats = factor(summary_by_long$stats,
                            levels = c('min','max'))

base = ggplot(data = summary_by_long, aes(x = Country.Code)) + theme_light()
base + geom_point(aes(y=coverage, color=stats))

base1 = ggplot(data = summary_by_long,
             aes(x = Country.Code, y = coverage,
                 fill = stats)) # fill brings a legend
bar_dodge = base1 +  geom_bar(stat = "identity",
                            position ='dodge') 
bar_dodge + geom_text(size = 4,
                     position = position_dodge(1),hjust = 0,
                     aes(label = round(coverage, 1)))+
  coord_flip()


bar_dodge = base1 +  geom_bar(stat="identity",
                            position = position_dodge(.7), width = 0.2) 
bar_dodge + geom_text(size = 4,
                     position = position_dodge(0.7), hjust = 0,
                     aes(label = round(coverage, 1))) +
  coord_flip()

bars = base1 + geom_bar(aes(y = coverage, fill = stats),
                     stat = 'identity', width = 0.3,
                     show.legend = F) +
  geom_text(size = 6,
            position = position_dodge(0.7),hjust=1,
            aes(y = coverage,
                label = round(coverage, 1))) 

bars + facet_grid(~stats) + coord_flip() 


bars = base1 + geom_point(aes(y = coverage, fill = stats),
                       stat = 'identity',
                       show.legend = F) + 
  geom_text(size = 4,hjust = -0.5,
            aes(y = coverage,
                label = round(coverage, 1))) +
  geom_segment(aes(y = 0, 
                   x = Country.Code, 
                   yend = coverage, 
                   xend = Country.Code), 
               color = "grey50") 


bars = bars + facet_grid(~stats) + coord_flip() 
bars

bars + scale_y_continuous(limits = c(0,50))

bars = base1 + 
  geom_text(size = 5,hjust=1,vjust=-0.1, 
            aes(y = coverage,
                label = round(coverage, 1),
                color = stats), show.legend = F) +
  geom_segment(aes(y = 0, 
                   x = Country.Code, 
                   yend = coverage, 
                   xend = Country.Code), 
               color = "grey50") 


bars + facet_grid(~stats) + coord_flip() 

bars = base1 + 
  geom_text(size = 5, hjust = 1, vjust = -0.1, 
            aes(y = coverage,
                label = round(coverage, 1),
                color = stats),
            show.legend = F, fontface='bold') +
  geom_segment(aes(y = 0, 
                   x = Country.Code, 
                   yend = coverage, 
                   xend = Country.Code), 
               color = "grey50") +
  scale_colour_manual(values = c("black","red"))


bars + facet_grid(~stats) + coord_flip()





### Drafts with full year coverage
mydata_d3_2 <- mydata[mydata$Country.Name %in% c('High income', 'Upper middle income', 'Middle income', 
                                                     'Lower middle income', 'Low income'),]

mydata_d3_2$Country.Name <- factor(mydata_d3_2$Country.Name, order = TRUE, 
                                 levels = c("Low income", "Lower middle income", "Middle income", 
                                            "Upper middle income", "High income"))

mydata_d3_2$Country.Code <- factor(mydata_d3_2$Country.Code, order = TRUE, 
                                 levels = c("LIC", "LMC", "MIC", 
                                            "UMC", "HIC"))
mydata_d3_2 = mydata_d3_2 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "coverage",
    values_drop_na = TRUE
  )

mydata_d3_2$year = as.numeric(mydata_d3_2$year)


str(mydata_d3_2,width = 70,strict.width='cut')

summary(mydata_d3_2$year)

mydata_d3_2 = mydata_d3_2[complete.cases(mydata_d3_2),]

tapply(mydata_d3_2$coverage,mydata_d3_2$Country.Code, summary)

# Box plot drafts
base_d3_2 = ggplot(data = mydata_d3_2,
                 aes(x = Country.Code, y = coverage))

base_d3_2 = base_d3_2 + geom_boxplot() + labs(title = 'yearly coverage')
base_d3_2


base_d3_2 = ggplot(data = mydata_d3_2,
                   aes(x = coverage, y = Country.Code)) 

box_d3_2 = base_d3_2 + geom_boxplot() + labs(title = 'yearly coverage')
box_d3_2

base_box_2 = ggplot(data = mydata_d3_2,
                  aes(y = coverage))

base_box_2 + geom_boxplot(aes(x = reorder(Country.Code, coverage , median))) +
  coord_flip()


# Probability testing
kruskal.test(coverage ~ Country.Code, data = mydata_d3_2)

pairwise.wilcox.test(mydata_d3_2$coverage, mydata_d3_2$Country.Code,
                     p.adjust.method = "BH")


# Density plot draft
ggplot(mydata_d3_2) + geom_density(aes(x = coverage), show.legend = F) + 
  facet_grid(reorder(Country.Code, coverage, median)~.) 


# Histogram plot draft
baseHist_2 = ggplot(data = mydata_d3_2,
                  aes(x = coverage))

baseHist_2 + geom_histogram() +facet_grid(reorder(Country.Code, coverage, median)~.)


# Mean erro plot drafts
base_means_2 = ggplot(mydata_d3_2, aes(x = Country.Code,
                                   y = coverage)) 
point_means_2 = base_means_2 + geom_point(stat = "summary") 
point_means_2

error_plot_2 = point_means_2 + geom_errorbar(stat = "summary") 
error_plot_2

jitter_means_2 = base_means_2 + geom_jitter(colour = "blue",
                                        alpha = 0.2 #transparency
)
jitter_means_2 = jitter_means_2 + geom_point(stat = "summary") +
  geom_errorbar(stat = "summary", width = .2)
jitter_means_2 


# Violin plot drafts
vio_1 = base_box_2 + geom_violin(aes(x = Country.Code), fill = "magenta", trim = F)
vio_1 + coord_flip() 

vio_2 = vio_1 + coord_flip() + 
  scale_y_continuous(breaks = c(20,25,40)) + 
  theme(panel.grid.minor = element_blank())
vio_2


# Bar plot drafts
summary_by_2 = aggregate(data = mydata_d3_2,
                       coverage ~ Country.Code,
                       FUN = function(x) c(max = max(x),
                                           median = median(x)) )
# when several functions at play
summary_by_2 = do.call(data.frame, summary_by)
summary_by_2

names(summary_by_2) = c('Country.Code','max', 'median')

base_2 = ggplot(data = summary_by_2, aes(x = Country.Code))
base_2 + geom_point(aes(y = median),color='blue') +
  geom_point(aes(y = max),color='red')

summary_by_long_2 = reshape2::melt(summary_by_2, variable.name = 'stats',
                                 value.name = 'coverage',
                                 id.vars = 'Country.Code')
summary_by_long_2

levels(summary_by_long_2$stats)

summary_by_long_2$stats = factor(summary_by_long_2$stats,
                               levels = c('median','max'))

base_2 = ggplot(data = summary_by_long_2, aes(x = Country.Code)) + theme_light()
base_2 + geom_point(aes(y = coverage, color = stats))

base2 = ggplot(data = summary_by_long_2,
               aes(x = Country.Code, y = coverage,
                   fill = stats)) # fill brings a legend
bar_dodge_2 = base2 +  geom_bar(stat = "identity",
                              position ='dodge') 
bar_dodge_2 + geom_text(size = 4,
                      position = position_dodge(1),hjust = 0,
                      aes(label = round(coverage, 1)))+
  coord_flip()


bar_dodge_2 = base2 +  geom_bar(stat="identity",
                              position = position_dodge(.7), width = 0.2) 
bar_dodge_2 + geom_text(size = 4,
                      position = position_dodge(0.7), hjust = 0,
                      aes(label = round(coverage, 1))) +
  coord_flip()

bars_2 = base2 + geom_bar(aes(y = coverage, fill = stats),
                        stat = 'identity', width = 0.3,
                        show.legend = F) +
  geom_text(size = 6,
            position = position_dodge(0.7),hjust=1,
            aes(y = coverage,
                label = round(coverage, 1))) 

bars_2 + facet_grid(~stats) + coord_flip() 


bars_2 = base2 + geom_point(aes(y = coverage, fill = stats),
                          stat = 'identity',
                          show.legend = F) + 
  geom_text(size = 4,hjust = -0.5,
            aes(y = coverage,
                label = round(coverage, 1))) +
  geom_segment(aes(y = 0, 
                   x = Country.Code, 
                   yend = coverage, 
                   xend = Country.Code), 
               color = "grey50") 


bars_2 = bars_2 + facet_grid(~stats) + coord_flip() 
bars_2

bars_2 + scale_y_continuous(limits = c(0,50))

bars_2 = base2 + 
  geom_text(size = 5,hjust = 1,vjust = -0.1, 
            aes(y = coverage,
                label = round(coverage, 1),
                color = stats), show.legend = F) +
  geom_segment(aes(y = 0, 
                   x = Country.Code, 
                   yend = coverage, 
                   xend = Country.Code), 
               color = "grey50") 


bars_2 + facet_grid(~stats) + coord_flip() 

bars_2 = base2 + 
  geom_text(size = 5, hjust = 1, vjust = -0.1, 
            aes(y = coverage,
                label = round(coverage, 1),
                color = stats),
            show.legend = F, fontface='bold') +
  geom_segment(aes(y = 0, 
                   x = Country.Code, 
                   yend = coverage, 
                   xend = Country.Code), 
               color = "grey50") +
  scale_colour_manual(values = c("black","red"))


bars_2 + facet_grid(~stats) + coord_flip()



#Alternate attempt at bivariate




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

#%>%
#  pivot_wider(
#    names_from = year,
#    values_from = population) %>%
#  select('1992':'2021')

#select(Country.Name:Country.Code, X2016:X2021)
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
# ratio is equal to square mileage per 1000 people
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



#(yr_class_ratio_mgCol = prop.table(class_ratios,
#                            margin = 2)%>%round(.,3))

pivot_ratios = class_ratios %>%
  select(class, '1993', '2000', '2007', '2014', '2021') %>%
  pivot_longer(
    cols = '1993':'2021',
    names_to = 'year',
    values_to = 'ratio')



ggplot(data = pivot_ratios, 
       aes(x = class, y = ratio)) + geom_bar(stat = "identity")



bivariate_base = ggplot(data = pivot_ratios,
             aes(x = year, y = ratio,
                 fill = class)) # fill brings a legend

Dodge_graph = bivariate_base +  geom_bar(stat="identity",
                             position ='dodge') 
Dodge_graph 



bivariate_base2 = ggplot(data = pivot_ratios,
             aes(x = year, y = ratio, # % not counts
                 fill = class)) + theme_minimal()

Dodge_graph2 = bivariate_base2 +  geom_bar(stat = "identity",
                             position = 'dodge') 
Dodge_graph2 = Dodge_graph2 + labs(title = "How much room do our forests have?",
                 subtitle = "Forest coverage by year (every 7 years)",
                 x="",
                 y="sq miles / 1000 people",
                 fill="class",
                 caption = "Source: Kaggle, World Bank Data, Federal Reserve Economic Data, World Economic Forum") +
  annotate(geom = 'text',
           label = round(pivot_ratios$ratio, digits = 2),
           y = round(pivot_ratios$ratio, digits = 1),
           x = pivot_ratios$year,
           angle = 0)
  
  
Dodge_graph2


Dodge_graph2 = bivariate_base2 +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_text(aes(label = round(ratio, digits = 0)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +  # Adjust vertical position of labels
  labs(title = "How much room do our forests have?",
       subtitle = "Forest coverage by year (every 7 years)",
       x = "",
       y = "sq miles / 1000 people",
       fill = "class",
       caption = "Source: Kaggle, World Bank Data, Federal Reserve Economic Data, World Economic Forum") +
  theme_minimal()

Dodge_graph2



barStacked_graph = bivariate_base2 + geom_bar(stat = "identity",
                               position = 'stack')#default

barStacked_graph = barStacked_graph + labs(title = "How much room do our forests have?",
                       subtitle = "Forest coverage by year (every 7 years)",
                       x = "",
                       y = "sq miles / 1000 people",
                       fill = "class",
                       caption = "Source: Kaggle, World Bank Data, Federal Reserve Economic Data, World Economic Forum")

barStacked_graph

barStacked_graph2 = barStacked_graph + geom_text(size = 5,# check below:
                                     position = position_stack(vjust = 0.5),# center
                                     aes(label = round(ratio, digits = 1)),
                                     colour = 'black',)# percent format

barStacked_graph2















total_mi = class_mileage_pop %>%
  select(name, forest_mileage)

total_mi$class = 'total_mileage'
test1 = test1 %>%
  pivot_wider(
    names_from = name,
    values_from = forest_mileage)





del3Draft = base + geom_point(aes(x = Student.Teacher.Ratio,
                                  y = Free.Lunch))
del3Draft 



# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------


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