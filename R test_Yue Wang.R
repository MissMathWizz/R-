install.packages('lubridate')
install.packages('fst')
install.packages('plotly')
install.packages('scales')
install.packages('caTools')
install.packages('randomForest')
require(tidyverse)

library(dplyr)
library(tidyr)
library(fst)
library(base)
library(lubridate)
library(ggplot2) 
library(plotly)
library(scales)
library(grid)
library(gridExtra)


setwd("C:/Users/stant/Documents/R_test")
#1.a

ft <- read_fst("IIGF_ETH_ERA5_19952021_ALL.fst") 
summary(ft$GID_2)
u_list<-unique(ft$GID_2)
length(unique(ft$GID_2))
u_list[1]
#1.b 586920 values total and 57 unique values in GID_2 column and
#1.c

G1c<-ft%>%filter(GID_2 == "ETH.8.14_1")

#1.d	
G1c<-G1c %>% mutate(month = month(date))
#1.e.	
G1c<-G1c%>%group_by(month)%>%mutate(mean_temp_monthly=mean(tas))
#f.	
dev.off() 
p <-G1c%>% ggplot( aes(x=date, y=mean_temp_monthly)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("monthly temperature mean") + scale_x_date(date_labels="%Y", date_break ="1 year", date_minor_break = "1 month")
ETH814_1_plot <- ggplotly(p)
ETH814_1_plot


dev.off() 

G <-list()
plots <- list()  # new empty list

for (i in 1:10) {
  G12<-ft%>%filter(GID_2 == u_list[i])%>% mutate(month = month(date))%>%group_by(month)%>%mutate(mean_temp_monthly=mean(tas))
  G[[i]]<-G12
  p1 <- G[[i]]%>% ggplot( aes(x=date, y=mean_temp_monthly)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    facet_wrap(u_list[i]) +
    ylab("monthly temperature mean") + scale_x_date(date_labels="%Y", date_break ="2 year", date_minor_break = "1 month")
  plots[[i]] <- p1  # add each plot into plot list
}

print(plots[[1]])
grid.arrange(plots, ncol=2)
grid.arrange(grobs = plots, ncol= 5)



facet<- c( u_list[1:10])

G111<-ft%>% mutate(month = month(date))%>%group_by(month)%>%mutate(mean_temp_monthly=mean(tas))
ppp<-G111[G111$GID_2 %in% facet,]%>%ggplot( aes(x=date, y=mean_temp_monthly)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  facet_wrap(~GID_2, ncol=5) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ylab("monthly temperature mean") + scale_x_date(date_labels="%Y", date_break ="2 year", date_minor_break = "1 month")

  ggsave(file="bench_query_sort.pdf", width=4, height=4, dpi=300)
  
ppp

for (i in 1:10) {
  G12<-ft%>%filter(GID_2 == u_list[i])%>% mutate(month = month(date))%>%group_by(month)%>%mutate(mean_temp_monthly=mean(tas))
  G[[i]]<-G12
  p1 <- G[[i]]%>% ggplot( aes(x=date, y=mean_temp_monthly)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    facet_wrap(~GID_2) +
    ylab("monthly temperature mean") + scale_x_date(date_labels="%Y", date_break ="2 year", date_minor_break = "1 month")
  plots[[i]] <- p1  # add each plot into plot list
}





for (i in 1:10) {
  G12[[i]]<-ft%>%filter(GID_2 == u_list[i])
  p1[[i]] <- G12[[i]]%>% ggplot( aes(x=date, y=mean_temp_monthly)) +
    ggtitle(paste0("region:", u_list[i]))
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("monthly temperature mean") + scale_x_date(date_labels="%Y", date_break ="1 year", date_minor_break = "1 month")
  plots[[i]] <- p1  # add each plot into plot list
}


#2

##a.

data <- load(file='df1.rda')

summary(df1)
##b.
panelb<-lm(log_gdppc ~ SPI01_l00s_new + SPI01_l02n_new + SPI01_l03n_new + SPI01_l04n_new + SPI01_l05n_new+SPI01_l06n_new+SPI01_l07n_new + SPI01_l09s_new + lin_temp + sq_temp + pctODA  , data= df1)
summary(panelb)


#c.	Control var: ODA in comparison to government budget or tradeopen as a measure of country's trade openness.
#d. SPI01_l04n_new,SPI01_l09s_new, pctODA, lin_temp , SPI01_l05n_new, SPI01_l07n_new are statistically significant:Neutral weather and wet weather have a positive effect on GDP per capital. Trade openess is negatively correlated with GDP per capital. The deviation from historical rain patter is negatively associated with GDP per capita. 

#Potential analytically gaps: 
  #1. More control variable need to be added, since we have have something statistically significant in the intercept terms.
  #2. Country level/spatial level investigation is recommended. 
  #3. model(b) doesn't concern time series. Will be good to add the lag terms in df1 to regression, maybe try other panel data models: random effect, fixed effect, between. 
  
#p.s. since the depedent variable is logged, I did some calculation to coefficient to better understand its numerical meanings.

exp(7.488e-04)-1
exp(1.038e-04)-1
exp(-1.279e-01 )-1


##e.for contry-level precision, just add "as.factor(country) as a regressor. Further step can investigate the heterougenous effect using an interaction term consists of key regressor and as.factor(country)"


#3 Machine learning
#Disclaimer: I used to do machine learning in Python, this is my first time doing machine learning in R. I like the experience. 


#0 cleaning, encode factors
df1<-drop_na(df1)
summary(is.na(df1))


#df1$gdppc = factor(df1$gdppc, levels = c(0, 1)) not working....which means I will have a weired confusion matrix
#summary(df1$gdppc)

#1 split dataset 25%~75%

library(caTools)
set.seed(123)
split = sample.split(df1$gdppc, SplitRatio = 0.75)
training_set = subset(df1, split == TRUE)
test_set = subset(df1, split == FALSE)

#train 
#I chose Random Forest machine learning model.
library(randomForest)

train_set1<-training_set%>%select(-"gdppc")
summary(is.na(train_set1))


classifier = randomForest(x = train_set1,
                          y = training_set$gdppc,
                          ntree = 500, random_state = 0)

test_set1 <-test_set%>%select(-"gdppc")
y_pred = predict(classifier, newdata = test_set1)
y_pred

#confusion matrix: this is not a real confusion matrix since factorizing target variable didn't work
cm = table(test_set[,"gdppc"], y_pred)
cm
heatmap(cm) #there must be another way to evaluate the accuracy, but I don't have enough time. Now I regret that I didn't choose linear regression model.

 #P.S.,I personally like to use k-mean cluster model to plot the factor importance.  I will do it if time allows.

#4
#b.	
 
library(data.table)
ft[,.N] 
unique(ft, by = "GID_2")

 #c.
setDT(ft)
setkey(ft,GID_2) 
gic<-ft[c("ETH.8.14_1")] 

#d.
month_gic<-month(gic$date)

gic<-cbind(ft, month_gic)

#e.	
mean_tas<-gic[ ,list(mean_tas=mean(tas)), by=month_gic]
gic<-cbind(gic,mean_tas)


#f.	

gic[ , plot(date, mean_tas, type="l", ylab="mean", xlab="")]

#Comment: data.table is generally faster than dplyr
