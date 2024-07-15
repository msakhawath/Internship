library(graphics)
library(stats) 
library(ggplot2)
library(ggpubr)
library(cowplot)

# read the file
df <- read.csv(file = 'SwimmingTimes.csv', sep = ",")

head(df)

# Removing the duplicate row w.r.t. name which pair has medley category
# and the pair of duplicate row w.r.t name that don't have a medley category

df['duplicate'] <- 'FALSE'

for(i in 1:nrow(df)) {
  for(j in 1:nrow(df)) {
    if(i<j) {
      if(df[i,2]==df[j,2]) {
        if(df[i, 1]=='Medley'){
          df[j, 4] <- 'TRUE'
        }
        else if(df[j, 1]=='Medley') {
          df[i, 4] <- 'TRUE'
        }
        else {
          df[i, 4] <- 'TRUE'
          df[j, 4] <- 'TRUE'
        }
      }
    }
  }
}

df <- subset(df, duplicate == "FALSE")
df <- subset(df, select = -c(duplicate))

# descriptive statistics

t1 <- df %>% group_by(Category) %>% get_summary_stats(Time, show = c("n", "min", "max", "mean", "median", "iqr", "sd")) 
t1





# ---Boxplot--- to show difference between categories

gr2 <- ggplot(df, aes(x=Category, y=Time, fill=Category))+
  geom_boxplot() + xlab("Category") + ylab("Time (seconds)") + labs(fill = "Category")+
  theme(legend.position = "none", plot.title = element_text(
    size=20,hjust = 0.5,face = "bold"),text = element_text(size=20)) + coord_flip()

gr2


# ---Assumptions check---

# check normality assumption

Backstroke <- subset(df, Category == 'Backstroke')
gr3 <-  ggqqplot(Backstroke, "Time",xlab="Theoritical Quantile" ,ylab="Sample Quantile(Backstroke)",color = 'blue', conf.int = FALSE, conf.int.level = 0)
gr3

Breaststroke <- subset(df, Category == 'Breaststroke')
gr4 <- ggqqplot(Breaststroke, "Time",color = 'blue',xlab="Theoritical Quantile" , ylab="Sample Quantile for Breaststroke",conf.int = FALSE, conf.int.level = 0)
gr4
Butterfly <- subset(df, Category == 'Butterfly')
gr5 <-ggqqplot(Butterfly, "Time",color = 'blue',xlab="Theoritical Quantile" , ylab="Sample Quantile for Butterfly",conf.int = FALSE, conf.int.level = 0)
gr5
Freestyle <- subset(df, Category == 'Freestyle')
gr6 <-ggqqplot(Freestyle, "Time",color = 'blue',xlab="Theoritical Quantile" , ylab="Sample Quantile for Freestyle",conf.int = FALSE, conf.int.level = 0)
gr6
Medley <- subset(df, Category == 'Medley')
gr7 <-ggqqplot(Medley, "Time",color = 'blue',xlab="Theoritical Quantile" ,ylab="Sample Quantile for Medley",conf.int = FALSE, conf.int.level = 0)
gr7
qqplot <- plot_grid(gr3, gr4, gr5, gr6, gr7, 
          
          ncol=3, nrow=2)

qqplot



# ---Global test---

t2 <-aov(Time ~ Category, data = df)
t2
summary(t2)



# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences
# between the groups 

# ---Two-sample t-tests---

t3 <- pairwise.t.test(df$Time, df$Category, pool.sd = TRUE, p.adjust.method = "none")
t3

# ---Adjustment for Multiple testing ---

# t-tests with Bonferroni method
t4 <- pairwise.t.test(df$Time, df$Category, pool.sd = TRUE, p.adjust.method = "bonferroni")$p.value
t4

# t-tests with Holm-Bonferroni method
t5 <- pairwise.t.test(df$Time, df$Category, pool.sd = TRUE, p.adjust.method = "holm")$p.value
t5


















