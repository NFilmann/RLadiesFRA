# Now that we have tidied the data, we will look at some graphs.


# 1. Boxplot between baseline viral load and Virologic response, b) stratify for gender

# first take a look at the baseline viral load

ggplot( data = HIVdata ) +
  aes(x = TErgNum.0) +
  theme_minimal() +
  geom_histogram( fill = "blue", bins=sqrt(length(HIVdata$TErgNum.0)))

# -> Let's logarithmize the data

ggplot( data = HIVdata ) +
  aes(x = log10(TErgNum.0)) +
  theme_minimal() +
  geom_histogram( fill = "blue",bins=sqrt(length(HIVdata$TErgNum.0)))


# boxplot
ggplot( data = HIVdata ) +
  aes(x = VirResponse, y = log10(TErgNum.0+1))+
  geom_boxplot()


ggplot( data = HIVdata ) +
  aes(x = VirResponse, y = log10(TErgNum.0+1))+
  geom_boxplot(fill = "grey")+
  theme_minimal()+
  ggtitle("Correlation between viral load at baseline and virologic response", subtitle = " ") + 
  xlab(attributes(HIVdata$VirResponse)$label) +
  ylab(paste(attributes(HIVdata$TErgNum.0)$label, "(log-transformed)"))

#1. b)
ggplot( data = HIVdata ) +
  aes(x = VirResponse, y = log10(TErgNum.0+1))+
  facet_grid(rows = vars(Gender))+
  geom_boxplot(fill = "grey")+
  theme_minimal()+
  ggtitle("Correlation between viral load at baseline and virologic response", subtitle = " ") + 
  xlab(attributes(HIVdata$VirResponse)$label) +
  ylab(paste(attributes(HIVdata$TErgNum.0)$label, "(log-transformed)"))
  

# 2. Scatterplot beetween baseline CD4 and baseline viral load

ggplot( data = HIVdata ) +
  aes(x = Value.0, y = log10(TErgNum.0+1))+
  geom_point()

ggplot( data = HIVdata ) +
  aes(x = Value.0, y = log10(TErgNum.0+1), color=VirResponse)+
  geom_point()+
  theme_minimal()+
  ggtitle("Correlation between CD4 and viral load at baseline", subtitle = " ") + 
  xlab(attributes(HIVdata$Value.0)$label) +
  ylab(paste(attributes(HIVdata$TErgNum.0)$label, "(log-transformed)"))+
  scale_color_manual(values=c("red", "black"))+
  labs(color=  xlab(attributes(HIVdata$VirResponse)$label))
  #geom_smooth(span = 1.5)


# 3. Barplot beetween Gender and therapeutic response, b) Barplot beetween HCV- HBV-status and therapeutic response


ggplot(data = HIVdata) + 
  aes(  x = VirResponse, fill = Gender)+
  geom_bar()

ggplot(data = HIVdata) + 
  aes(  x = VirResponse, fill = Gender)+
  geom_bar(show.legend = TRUE)+
  theme_minimal()+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  xlab(attributes(HIVdata$VirResponse)$label) +
  ylab("Count")


# 3.b) Barplot beetween HCV- HBV-status and therapeutic response

ggplot(data = HIVdata) + 
  aes(  x = VirResponse, fill = interaction(HBVpos, HCVpos))+
  geom_bar(show.legend = TRUE)


ggplot(data = HIVdata) + 
  aes(  x = VirResponse, fill = interaction(HBVpos, HCVpos))+
  geom_bar(show.legend = TRUE, position ="dodge")+
  labs(fill="HBV and HCV status")+
  scale_fill_manual(labels = c("neg", "HBV pos", "HCV pos", "HBV and HCV pos"), values = c("darkgrey", "gold","darkblue", "red"))+
  theme_minimal()+
  geom_text(stat = 'count', aes(label=..count..),
            position = position_dodge(.9), 
            vjust = -0.5) +
  xlab(attributes(HIVdata$VirResponse)$label) +
  ylab("Count")



# 4. Spaghetti plot of viral load

# here the original data formate is more appropiate
HIVdata_long <- full_join(labdata, basedata) %>%
  mutate(Time_weeks = structure(Time_weeks, label = "Time (weeks)"),
         TErgNum = structure(TErgNum, label = "Viral load"))

str(HIVdata_long )


ggplot(data = HIVdata_long)+
  aes(x = Time_weeks, y = TErgNum, group = PatientID)+
  scale_y_log10()+ 
  geom_line() 


ggplot(data = HIVdata_long, aes(x = Time_weeks, y = TErgNum+1, group = PatientID))+
  geom_line(col="grey")+
  theme_minimal()+
  #facet_grid(. ~ Gender)+ 
  scale_y_log10()+ 
  stat_summary(aes(group = 1), geom = "line", fun.y = median,col="red",size = 2) +
  xlab(attributes(HIVdata_long$Time_weeks)$label) +
  ylab(attributes(HIVdata_long$TErgNum)$label)+
  geom_hline(yintercept = 20) + 
  annotate("text", 5, 40, label = "Limit of detection", size=3)

