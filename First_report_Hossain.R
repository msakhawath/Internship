library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)

# read the file
demographic <- read.csv(file = 'census2001_2021.csv')

str(demographic)
summary(demographic)


colSums(is.na(demographic))

demographic <- na.omit(demographic)   # Omitting null values.

table(is.na(demographic))



### Part1

head(demographic)





# Frequency distribution of Mortality rate in 2021


gr11 <- ggplot(filter(demographic, Year == 2021)) +
  geom_histogram(aes(x = Infant.Mortality.Rate..Both.Sexes, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") +  xlab("Mortality rate") +
  ylab("Density") + 
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=20)) +
  geom_vline(aes(xintercept = mean(Infant.Mortality.Rate..Both.Sexes)),col='red',size=1)
gr11
mortality_rate_21 <- filter(demographic, Year == 2021)$Infant.Mortality.Rate..Both.Sexes

mean(mortality_rate_21) # 20.24568
median(mortality_rate_21) #12.58
max(mortality_rate_21) # 106.75
min(mortality_rate_21) # 1.53
sd(mortality_rate_21) # 19.19284


# Frequency distribution of life expectancy of both sexes in 2021

gr21 <- ggplot(filter(demographic, Year == 2021)) +
  geom_histogram(aes(x = Life.Expectancy..Both.Sexes, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") +  xlab("Life expectancy both sexes") + 
  ylab("Density") +
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=20)) +
  geom_vline(aes(xintercept = mean(Life.Expectancy..Both.Sexes)),col='red') 
gr21

life_expec_both_21 <- filter(demographic, Year == 2021)$Life.Expectancy..Both.Sexes
mean(life_expec_both_21)  # 74.28
median(life_expec_both_21) #75.80
max(life_expec_both_21)# 89.4
min(life_expec_both_21) # 53.25
sd(life_expec_both_21) # 6.91


combined1 <- gr11 + gr21 & theme(legend.position = "bottom")
combined1 + plot_layout(guides = "collect")

# Frequency distribution of female life expectancy in 2021
gr21_f <- ggplot(filter(demographic, Year == 2021)) +
  geom_histogram(bins = 30, aes(x = Life.Expectancy..Females, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") +  xlab("Female life expectancy") + 
  ylab("Density") +
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=20)) +
  geom_vline(aes(xintercept = mean(Life.Expectancy..Females)),col='red') 

gr21_f

life_expect_f_21 <- filter(demographic, Year == 2021)$Life.Expectancy..Females
mean(life_expect_f_21) # 76.89
median(life_expect_f_21) # 78.36
max(life_expect_f_21) # 93.4
min(life_expect_f_21) # 54.85
sd(life_expect_f_21) #7.21

# Frequency distribution of male life expectancy in 2021

gr21_m <- ggplot(filter(demographic, Year == 2021)) +
  geom_histogram(bins = 30, aes(x = Life.Expectancy..Males, y = ..density..),fill = "grey", color = "black") +  xlab("Male life expectancy") + 
  ylab("Density") + 
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=20)) +
  geom_vline(aes(xintercept = mean(Life.Expectancy..Males)),col='red') 
gr21_m

life_expec_m_21 <- filter(demographic, Year == 2021)$Life.Expectancy..Males

mean(life_expec_m_21) # 71.78
median(life_expec_m_21) # 72.99
max(life_expec_m_21) # 85.55
min(life_expec_m_21) #51.73
sd(life_expec_m_21) # 6.74


combined2 <- gr21_m + gr21_f & theme(legend.position = "bottom")
comb_1 <- combined2 + plot_layout(guides = "collect")






### Part 2
# Are there bi variate correlations between the variables?






# Correlations between life expectancy female and mortality rate.

gr12 <- filter(demographic, Year == 2021) %>% 
  ggplot( aes(Infant.Mortality.Rate..Both.Sexes, Life.Expectancy..Females)) +
  ylab("Life expectancy females") + xlab("mortality rate") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr12

cor(mortality_rate_21,life_expec_both_21,method = 'pearson') # -0.90
cor(mortality_rate_21,life_expec_m_21,method = 'pearson' ) # -0.88
cor(mortality_rate_21,life_expect_f_21,method = 'pearson' ) # -0.91
cor(life_expec_both_21,life_expec_m_21,method = 'pearson' ) # .99
cor(life_expec_both_21,life_expect_f_21,method = 'pearson') # .99
cor(life_expect_f_21,life_expec_m_21 ,method = 'pearson') #.97


# Correlations between life expectancy male and mortality rate.

gr22 <- filter(demographic, Year == 2021) %>% 
  ggplot( aes(Infant.Mortality.Rate..Both.Sexes, Life.Expectancy..Males)) +
  ylab("Life expectancy males") + xlab("mortality rate") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr22

# Correlations between life expectancy both sexes and mortality rate.


gr32 <- filter(demographic, Year == 2021) %>% 
  ggplot( aes(Infant.Mortality.Rate..Both.Sexes, Life.Expectancy..Both.Sexes)) +
  ylab("Life expectancy both sexes") + xlab("mortality rate") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr32




combined <- gr12 + gr22 + gr32 & theme(legend.position = "bottom")
comb_2 <- combined + plot_layout(guides = "collect")
comb_2


# Correlations between life expectancy female and both sexes.

gr13 <- filter(demographic, Year == 2021) %>% 
  ggplot( aes(Life.Expectancy..Both.Sexes, Life.Expectancy..Females)) +
  ylab("Life expectancy females") + xlab("Life expect. both") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr13

# Correlations between life expectancy male and both sexes.

gr23 <- filter(demographic, Year == 2021) %>% 
  ggplot( aes(Life.Expectancy..Both.Sexes, Life.Expectancy..Males)) +
  ylab("Life expectancy males") + xlab("Life expect. both") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr23

# Correlations between life expectancy male and life expectancy female.

gr33 <- filter(demographic, Year == 2021) %>% 
  ggplot( aes(Life.Expectancy..Males, Life.Expectancy..Females)) +
  ylab("Life expectancy female") + xlab("Life expect. male") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)
gr33

combined <- gr13 + gr23 + gr33 & theme(legend.position = "bottom")
comb_3 <- combined + plot_layout(guides = "collect")
comb_3






### Part 3






# Box plot for Infant.Mortality.Rate..Both.Sexes

data_mor <- data.frame(Region = demographic[ demographic$Year == 2021, 'Region'],
                       Subregion = demographic[ demographic$Year == 2021, 'Subregion'],
                       mortality = demographic[ demographic$Year == 2021, 'Infant.Mortality.Rate..Both.Sexes'])


data_mor <- data_mor[order(data_mor$Region, data_mor$Subregion), ]
data_mor$Subregion <- factor(data_mor$Subregion, levels = rev(unique(data_mor$Subregion)), ordered = TRUE)
ggplot(data_mor, aes(Subregion, mortality, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Mortality rate")

data_mor

# Box plot for life expectancy of both sexes

data_exp <- data.frame(Region = demographic[ demographic$Year == 2021, 'Region'],
                       Subregion = demographic[ demographic$Year == 2021, 'Subregion'],
                       life_expectency = demographic[ demographic$Year == 2021, 'Life.Expectancy..Both.Sexes'])

data_exp <- data_exp[order(data_exp$Region, data_exp$Subregion), ]
data_exp$Subregion <- factor(data_exp$Subregion, levels = rev(unique(data_exp$Subregion)), ordered = TRUE)
ggplot(data_exp, aes(Subregion, life_expectency, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "life expectancy for both sexes")

data_exp

# Box plot for life expectancy of male

data_male <- data.frame(Region = demographic[ demographic$Year == 2021, 'Region'],
                        Subregion = demographic[ demographic$Year == 2021, 'Subregion'],
                        life_expectency = demographic[ demographic$Year == 2021, 'Life.Expectancy..Males'])
data_male <- data_male[order(data_male$Region, data_male$Subregion), ]
data_male$Subregion <- factor(data_male$Subregion, levels = rev(unique(data_male$Subregion)), ordered = TRUE)
ggplot(data_male, aes(Subregion, life_expectency, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "life expectancy for male")

data_male

# Box plot for life expectancy of female

data_female <- data.frame(Region = demographic[ demographic$Year == 2021, 'Region'],
                          Subregion = demographic[ demographic$Year == 2021, 'Subregion'],
                          life_expectency = demographic[ demographic$Year == 2021, 'Life.Expectancy..Females'])
data_female <- data_female[order(data_female$Region, data_female$Subregion), ]
data_female$Subregion <- factor(data_female$Subregion, levels = rev(unique(data_female$Subregion)), ordered = TRUE)
ggplot(data_female, aes(Subregion, life_expectency, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "life expectancy for female")

data_female













###  Part 4


# Scatter plot to show the comparison of mortality rate in 2001 and 2021

demographic <- read.csv(file = 'census2001_2021.csv')
data_2021 <- subset(demographic,demographic$Year == "2021")
data_2001 <- subset(demographic,demographic$Year == "2001")

plot17<-ggplot(data_2021, aes(x= data_2021$Infant.Mortality.Rate..Both.Sexes, y= data_2001$Infant.Mortality.Rate..Both.Sexes)) +
  geom_point(size=2,aes(color = Region, shape = Region))+
labs(x="Total mortality Rate in 2021", y="Total mortality Rate in 2001")
k<-plot17+ geom_abline()
k

# Scatter plot to show the comparison of life expectancy both sexes in 2001 and 2021

plot18<-ggplot(data_2021, aes(x=data_2021$Life.Expectancy..Both.Sexes, y=data_2001$Life.Expectancy..Both.Sexes)) +
  geom_point(size=2,aes(color = Region, shape = Region))+ labs(x="Life expectancy both sexes in 2021", y="Life expectancy both sexes in 2001")
p<-plot18+ geom_abline()
p

grid.arrange(k,p)






#To find mean, median, variance and IQR of subgroups
#We change the name of subgroup each time

data_2021 <- subset(demographic,demographic$Year == "2021")


all_the_subrigions <- c("Western Africa","Southern Africa", "Northern Africa", "Middle Africa","Eastern Africa","South America",
                        "Northern America","Central America","Caribbean","Western Asia","South-Eastern Asia",
                        "South-Central Asia","Eastern Asia","Western Europe","Southern Europe","Northern Europe",
                        "Eastern Europe","Polynesia","Micronesia","Melanesia","Australia/New Zealand")
noya<-filter(data_2021, data_2021$Subregion == "Middle Africa" )


#For Life exp.both sexes
mean(noya$Life.Expectancy..Both.Sexes)

median(noya$Life.Expectancy..Both.Sexes)

var(noya$Life.Expectancy..Both.Sexes)

IQR(noya$Life.Expectancy..Both.Sexes)






# Infant mortality rate.
mean(noya$Infant.Mortality.Rate..Both.Sexes)

median(noya$Infant.Mortality.Rate..Both.Sexes)

var(noya$Infant.Mortality.Rate..Both.Sexes)

IQR(noya$Infant.Mortality.Rate..Both.Sexes)





















