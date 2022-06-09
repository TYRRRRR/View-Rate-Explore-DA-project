library(tidyverse)
library(visdat)
library(reshape2)

df <- read_csv('boat_data.csv')
str(df)
dim(df)

moneydf <-  as.data.frame(str_split_fixed(df$Price,' ',2))
colnames(moneydf) <- c("currency","amount")
#########Data Cleaning#############################################################################################
df <- cbind.data.frame(df,moneydf)

#Â£  Pound sterling
#EUR euro
#DKK Danish krone
#CHF Swiss Franc

unique(df$currency)
df <- df %>% mutate(size = Length * Width) %>% arrange(desc(size))

df <- df %>% mutate(amount=as.numeric(amount))%>%
  mutate(priceUSD = case_when(currency == "CHF"~ amount * 1.03,
                              currency == "EUR"~ amount * 1.07,
                              currency == "DKK"~ amount * 0.14,
                              currency == "Â£" ~ amount * 1.26))%>%
  select(-Price,-currency,amount)%>%
  filter(!is.na(Type) & !is.na(Length) & !is.na(Width) & !is.na(Location) & `Year Built` !=0)


country <- as.data.frame(str_split_fixed(df$Location,'Â»',2))
colnames(country) <-c("Country","Region")
country$Country <- str_trim(country$Country, side = c("both", "left", "right"))
unique(country$Country)



df <- cbind.data.frame(df,country)
df

countryList <- country %>% group_by(Country) %>% summarise(count = n()) %>% arrange(desc(count))%>% filter(count >5) %>% select(Country)

df <- df %>% filter(size > 1 & size <= 400)

df <- df %>% mutate(Size_Type = case_when(
  size <= 20 ~ "Small",
  size <= 35 ~ "Median",
  size <= 60 ~ "Large",
  size <= 100 ~ "Huge",
  size <= 400 ~ "Luxury"))


df <- df %>% filter(Country %in% c(countryList)[[1]]) %>% select(-Location, -Region)

dim(df)


df %>% arrange(desc(priceUSD))%>%
  vis_miss()
summary(df$`Year Built`)


df %>% group_by(`Year Built`) %>%
  summarise(count = n())%>%ggplot(aes(x=`Year Built`,y=count))+
  geom_bar(stat = "identity")


df <- df %>% filter(`Year Built` > 1940) %>% mutate(Year_built_Type = case_when(
  `Year Built` <= 1960 ~ "Ancient",
  `Year Built` <= 1975 ~ "Old",
  `Year Built` <= 1990 ~ "Classics",
  `Year Built` <= 2005 ~ "Current",
  `Year Built` <= 2022 ~ "New"))


str(df)

unique(df$Material)
####Finish Data Cleaning#################################################################################################

med <- median(df$`Number of views last 7 days`)
med

summary(df$`Number of views last 7 days`)

out <- quantile(df$`Number of views last 7 days`,0.75) + 1.5*IQR(df$`Number of views last 7 days`)
out

ggplot(df,aes(x=`Number of views last 7 days`))+
  geom_histogram(bins=70)+
  geom_vline(xintercept = out,linetype=3,size=1,color="red")+
  annotate("rect", xmin = out, xmax = Inf, ymin = 0,ymax=Inf, fill = "#EAA9A9",alpha=0.3)+
  annotate("text", x = out + 300, y = 400, label = "Outlier Area\n weekly view > 322", color = "red",size = 5)+
  scale_x_log10()+
  labs(title ="Histogram of Weekly Views")+
  xlab("Weekly Views (Log Scale)")+
  ylab("Frequency")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))




#####Extract Most Views
most_views <- df %>% rename(WeekViews=`Number of views last 7 days`) %>%
  filter(WeekViews > 322)
dim(most_views)

all <- nrow(most_views)
most_views %>% filter(!is.na(Manufacturer)) %>% group_by(Manufacturer)%>% summarize(count=n()/all*100)%>%arrange(desc(count))%>%
  head(10) %>% ggplot(aes(x = count, y = factor(Manufacturer,levels = rev(Manufacturer)))) +
  geom_point(size = 6,color = "#3264BB")+
  geom_segment(aes(xend = 0, yend = Manufacturer), size = 4,color = "#3264BB")+
  geom_text(aes(label = paste(round(count,2),"%")), color = "black", size = 3,hjust = -0.35)+
  xlim(0,5)+
  xlab("Percentage (%)")+
  ylab("")+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        axis.ticks.y =element_blank(),
        legend.position = "none")

unique(most_views$Manufacturer)

######### work on material ##################################

a <- most_views %>% filter(!is.na(Material)) %>% group_by(Material)%>% summarize(count=n()/all*100)%>%arrange(desc(count))%>%
  head(10)
a


b <- df %>% filter(!is.na(Material) & Material %in% unique(most_views$Material)) %>% group_by(Material)%>% summarize(count=n()/nrow(df)*100)%>%arrange(desc(count))%>%
  head(10) 
b

c<- merge(a,b,by.x = "Material", by.y = "Material")


c <- melt(c, id.vars='Material')
c

c %>% arrange(variable,value) %>% ggplot(aes(x = factor(Material,levels = (unique(Material))), y = value, fill = variable)) +
  geom_bar(stat = "identity",position = "dodge",width = 0.8)+
  geom_text(aes(label=paste(round(value,2),"%")),position = position_dodge(0.8),hjust = -0.2)+
  coord_flip()+
  xlab(" ")+
  ylab("Percentage %")+
  scale_fill_manual(values=c("#E7522A","#2A7AE7"),name = " ", labels = c("Most View List", "Original Data"))+
  scale_y_continuous( limits = c(0, 80), position = "right")+
  theme_classic()

#######################
a <- most_views %>% filter(!is.na(Year_built_Type)) %>% group_by(Year_built_Type)%>% summarize(count=n()/all*100)%>%arrange(desc(count))%>%
  head(10)
a


b <- df %>% filter(!is.na(Year_built_Type) & Year_built_Type %in% unique(most_views$Year_built_Type)) %>% group_by(Year_built_Type)%>% summarize(count=n()/nrow(df)*100)%>%arrange(desc(count))%>%
  head(10) 
b

c<- merge(a,b,by.x = "Year_built_Type", by.y = "Year_built_Type")


c <- melt(c, id.vars='Year_built_Type')
c

c %>% arrange(variable,value) %>% ggplot(aes(x = factor(Year_built_Type,levels = (unique(Year_built_Type))), y = value, fill = variable)) +
  geom_bar(stat = "identity",position = "dodge",width = 0.8)+
  geom_text(aes(label=paste(round(value,2),"%")),position = position_dodge(0.8),hjust = -0.2)+
  coord_flip()+
  xlab(" ")+
  ylab("Percentage %")+
  scale_fill_manual(values=c("#E7522A","#2A7AE7"),name = " ", labels = c("Most View List", "Original Data"))+
  scale_y_continuous( limits = c(0, 80), position = "right")+
  theme_classic()




######################################################################################################################
str(most_views)
unique(most_views$`Type`)


most_views <- most_views %>% mutate(type_trans=case_when(
  grepl("Diesel", Type) ~ "Diesel",
  grepl("Unleaded", Type) ~ "Unleaded",
  grepl("Electric", Type) ~ "Electric",
  grepl("Gas", Type) ~ "Gas",
  TRUE ~ "Unknown"
))


prop.table(table(most_views$type_trans))

df <- df %>% mutate(type_trans=case_when(
  grepl("Diesel", Type) ~ "Diesel",
  grepl("Unleaded", Type) ~ "Unleaded",
  grepl("Electric", Type) ~ "Electric",
  grepl("Gas", Type) ~ "Gas",
  TRUE ~ "Unknown"
))

prop.table(table(df$type_trans))

a <- most_views %>% filter(!is.na(type_trans)) %>% group_by(type_trans)%>% summarize(count=n()/all*100)%>%arrange(desc(count))%>%
  head(10)
a


b <- df %>% filter(!is.na(type_trans) & type_trans %in% unique(most_views$type_trans)) %>% group_by(type_trans)%>% summarize(count=n()/nrow(df)*100)%>%arrange(desc(count))%>%
  head(10) 
b

c<- merge(a,b,by.x = "type_trans", by.y = "type_trans")


c <- melt(c, id.vars='type_trans')
c

c %>% arrange(variable,value) %>% ggplot(aes(x = factor(type_trans,levels = (unique(type_trans))), y = value, fill = variable)) +
  geom_bar(stat = "identity",position = "dodge",width = 0.8)+
  geom_text(aes(label=paste(round(value,2),"%")),position = position_dodge(0.8),hjust = -0.2)+
  coord_flip()+
  xlab(" ")+
  ylab("Percentage %")+
  scale_fill_manual(values=c("#E7522A","#2A7AE7"),name = " ", labels = c("Most View List", "Original Data"))+
  scale_y_continuous( limits = c(0, 80), position = "right")+
  theme_classic()


most_views %>% ggplot(aes(x = factor(type_trans,levels = rev(c("Unleaded","Diesel","Unknown","Electric","Gas"))), fill = factor(Year_built_Type,levels = c("Ancient","Old","Classics","Current","New"))))+
                          geom_bar(width = 0.7)+
  scale_fill_manual(values=c("#d1d3ff","#a0a2d3", "#7174a8", "#44497e","#132257"))+
  coord_flip()+
  labs(fill = "Built Year \n Type")+
  xlab("")+
  ylab("Frequency")+
  scale_y_continuous( position = "right")+
  theme_classic()



#######################################################################################################################
str(df)


df %>% group_by(`Year Built`) %>%
  summarise(count = n())%>%mutate(Year_built_Type = case_when(
    `Year Built` <= 1960 ~ "Ancient",
    `Year Built` <= 1975 ~ "Old",
    `Year Built` <= 1990 ~ "Classics",
    `Year Built` <= 2005 ~ "Current",
    `Year Built` <= 2022 ~ "New"))%>%
  ggplot(aes(x=`Year Built`,y=count,fill=factor(Year_built_Type,levels = c("Ancient","Old","Classics","Current","New"))))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#F4EDCA","#C3D7A4", "#FFDB6D", "#C4961A","#D16103"))+
  labs(fill = "Built Year \n Type")+
  ylab("Frequency")+
  scale_y_continuous( position = "right")+
  theme_bw()+
  theme_classic()


df %>% group_by(`Year Built`)%>% 
  summarize(total_views = sum(`Number of views last 7 days`))%>% 
  ggplot(aes(x=`Year Built`,y=total_views,size = total_views))+
  geom_point(aes(color=total_views))+
  scale_colour_gradient(low = "#6699ff", high = "black")+
  scale_y_log10()+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(breaks= round(seq(min(df$`Year Built`), max(df$`Year Built`), by = 15)))+
  ylab("Total Weekly Views")+
  ggtitle("Total Weekly Views by Year Built \n VS. \n Year Built")+
  theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))

str(most_views)
most_views %>% group_by(`Year Built`)%>% 
  summarize(avg_views = mean(WeekViews))%>% 
  ggplot(aes(x=`Year Built`,y=avg_views,size = avg_views))+
  geom_point(aes(color=avg_views))+
  scale_colour_gradient(low = "#6699ff", high = "black")+
  scale_y_log10()+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(breaks= round(seq(min(df$`Year Built`), max(df$`Year Built`), by = 15)))+
  ylab("Average Weekly Views")+
  ggtitle("Average Weekly Views by Year Built")+
  theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))


df %>% group_by(Year_built_Type)%>% 
  summarize(avg_views = mean(`Number of views last 7 days`))%>% 
  ggplot(aes(x=factor(Year_built_Type,levels = c("Ancient","Old","Classics","Current","New")),y=avg_views))+
  geom_bar(stat="identity",width = 0.7,fill= "#6E8DAB")+
  geom_text(aes(label = round(avg_views),vjust = -0.3))+
  coord_cartesian(ylim=c(50,250))+
  xlab("")+
  ylab("Average Views")+
  theme_classic()

df %>% group_by(Year_built_Type)%>% 
  ggplot(aes(x=factor(Year_built_Type,levels = c("Ancient","Old","Classics","Current","New")),y=`Number of views last 7 days`))+
  geom_boxplot()+
  xlab("")+
  ylab("")+
  theme_classic()

total_view <- sum(df$`Number of views last 7 days`)

summary(df$priceUSD)

unique(df$`Boat Type`)
unique(df$`Year Built`)
str(df)

summary(df$size)

ggplot(df,aes(x=size))+
  geom_boxplot()
  
ggplot(df,aes(x=size))+
  geom_histogram(bins=50)+
  theme_classic()


summary(df$size)


most_views %>% mutate(size=round(size)) %>% group_by(size)%>% 
  summarize(avg = mean(WeekViews))%>% 
  ggplot(aes(x=size,y=avg,size=avg))+
  geom_point(stat = 'identity')+
  xlab("Boat Size")+
  ylab("Average Weekly Views")+
  theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))

most_views %>% mutate(size=round(size)) %>% 
  ggplot(aes(x=size,y=WeekViews,color = Year_built_Type))+
  geom_point(position = 'jitter',alpha = 0.4)+
  scale_x_log10()+
  xlab("Boat Size")+
  ylab("Average Weekly Views")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

most_views %>% mutate(size=round(size)) %>% ggplot(aes(x=priceUSD,y=WeekViews))+geom_point(alpha = 0.3)+
  xlab("Price in USD")+
  ylab("Weekly Views")+
  xlim(0,250000)+
  scale_x_log10()+
  theme_classic()

table(most_views$Country)

high_review_size <- df %>% mutate(size=round(size)) %>% group_by(size)%>% 
  mutate(avg = mean(`Number of views last 7 days`))%>% ungroup()%>%filter(avg > 200)

####################################################################################




most_views %>% group_by(Country)%>% 
  summarize(count = n())%>% arrange(count)%>% 
  ggplot(aes(x= factor(Country,levels = Country),y=count))+
  geom_bar(stat = 'identity',width=0.7,fill= "#6E8DAB")+
  geom_text(aes(label = count),hjust = -0.1)+
  coord_flip()+
  xlab(" ")+
  theme_classic()


most_views %>% mutate(new)


df %>% group_by(Size_Type) %>% summarize(mean = mean(priceUSD),
                                         median = median(priceUSD),
                                         count = n())%>%
  arrange(desc(mean)) %>% ggplot(aes(factor(Size_Type,levels = c("Luxury","Huge","Large","Median","Small")),y=mean))+
  geom_point(size = 5,color = "#666699")+
  geom_text(aes(label = paste(round((mean/1000),2),"k"), color = "white", size = 2),vjust=-1)+
  geom_segment(aes(xend = Size_Type, yend = 0), size = 2,color="#666699")+
  labs(title ="Averge price for different boat size")+
  xlab("Boat Size Type")+
  ylab("Average Price ($)")+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))
  

hist <- df %>% ggplot(aes(x=`Number of views last 7 days`,fill=factor(Size_Type,levels = c("Luxury","Huge","Large","Median","Small"))))+
  geom_histogram(bins=50,alpha = 0.5)+
  scale_fill_manual(values=c("#69b3a2", "#404080","#E69F00", "#56B4E9","#666699"))+
  scale_x_log10()+
  labs(fill = "Scize Type")+
  theme_classic()+
  theme_bw() 

hist

hist + geom_vline(xintercept =300, color = "red", linetype = 3,size = 1) +
  annotate("text", x=500, y =400 ,label = "High Weekly \n Views Zone \n views > 300", color = "red",size = 4)
 


df %>% group_by(Size_Type) %>% summarize(mean = mean(`Number of views last 7 days`),
                                         median = median(`Number of views last 7 days`),
                                         min = min(`Number of views last 7 days`),
                                         max = max(`Number of views last 7 days`),
                                         count = n()) 

df  %>% summarize(mean = mean(`Number of views last 7 days`),
                                         median = median(`Number of views last 7 days`),
                                         min = min(`Number of views last 7 days`),
                                         max = max(`Number of views last 7 days`),
                                         quant = quantile(`Number of views last 7 days`,0.75),
                                         count = n()) 

highview <- df %>% filter(`Number of views last 7 days` > 300)

dim(highview)  
unique(most_views$Type)
table(highview$Manufacturer)

top5 <- highview %>% filter(!is.na(Manufacturer)) %>% group_by(Manufacturer) %>% summarise(count = n()) %>% arrange((count))%>%
  tail(10)

bot5 <- highview %>% filter(!is.na(Manufacturer)) %>% group_by(Manufacturer) %>% summarise(count = n()) %>% arrange((count))

top5$Manufacturer <- factor(top5$Manufacturer , levels = top5$Manufacturer)

avg <- (mean(bot5$count))

ggplot(top5, aes(x = count, y = Manufacturer)) +
  geom_point(size = 5)+
  geom_segment(aes(xend = 0, yend = Manufacturer), size = 2)+
  geom_vline(xintercept =avg  , color = "grey40", linetype = 3,size = 1) +
  geom_text(aes(label = count), color = "white", size = 3)+
  theme_classic()+
  annotate(
    "text",
    x = 5, y = 5.7,
    label = "The\naverage",
    vjust = 1, size = 3, color = "grey40"
  )+
  theme(axis.line.y = element_blank(),
        axis.ticks.y =element_blank(),
        legend.position = "none")

