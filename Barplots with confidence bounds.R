

library(ggplot2)
library(ggthemes)
dir <- 'C:/Plots/'

# ---------------------------------   Plot 1    ----------------------------------------------#

# Creating data frame

Age <- as.factor(c("0-23 months", "24-59 months", 
                   "0-23 months", "24-59 months", "0-23 months", "24-59 months"))

Indicator <- as.factor(c(rep("Cough with Rapid Breathing", 2), rep("Diarrhea", 2), 
               rep("Fever", 2)))
Prevalence <- c(10.5, 7.5, 22.1, 15.1, 29.6, 25.3)
#Prevalence <- c(7.5, 10.5, 15.1, 22.1, 25.3, 29.6)
ymin <- c(4.6, 3.5, 14.1, 7.6, 20.7, 19.8)
#ymin <- c(3.5, 4.6, 7.6, 14.1, 19.8, 20.7)
ymax <- c(16.5, 11.6, 30.1, 22.6, 38.6, 30.8)
#ymax <- c(11.6, 16.5, 22.6, 30.1, 30.8, 38.6)
df1 <- data.frame(Age, Indicator, Prevalence, ymin, ymax)
#levels(df1$Age) <- c("24-59 months", "0-23 months")

# Creating plot

png(paste0(dir, 'Figure1.png'), 12, 8, units='in', res=300)

ggplot(df1, aes(x=Indicator, y=Prevalence, fill=Age)) + 
  
  geom_hline(yintercept = c(10,20,30,40), color="grey93") + 
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.2,position=position_dodge(.9)) + 
  scale_fill_manual("Age of Child", values = c("0-23 months" = "steelblue", "24-59 months" = "slategray2"))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), 
                     labels = c("0%", "10%", "20%", "30%", "40%"),
                     limits = c(0, 42)) +
  labs(title = "2-week prevalence of cough with rapid breathing, diarrhea, and fever in children under 5") +
# ggtitle("Figure 1. Prevalence of cough with rapid breathing, diarrhea, and fever in children under 5") +
  xlab("Indicator") + 
  ylab("2-week prevalence") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15, face = "bold", margin = unit(c(5, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 15, face = "bold", margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = element_text(size = 12, face = "bold", vjust=3),
        axis.text.y = element_text(size = 12, face = "bold"),
#        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.ticks=element_blank()) +
        #axis.line = element_line(color="black")) + 
  geom_hline(yintercept = 0, color="black", lwd=1) +
  geom_segment(aes(x = 0.4, y = 0, xend = 0.4, yend = 40), color="black", lwd=1.15) +
  annotate("text", x=1, y=41, label="OR=0.69", size=6) +
  annotate("text", x=2, y=41, label="OR=0.63", size=6) +
  annotate("text", x=3, y=41, label="OR=0.81", size=6) 

dev.off()

# ---------------------------------   Plot 2    ----------------------------------------------#

# Creating data frame

Age2 <- as.factor(c("9-23 months", "24-59 months", "9-23 months", "24-59 months"))
Indicator2 <- as.factor(c(rep("Any report of vaccination history", 2),
                         rep("Verified by vaccination card", 2)))
#Prevalence2 <- c(54.8, 65.4, 26.6, 18.8)
Prevalence2 <- c(65.4, 54.8, 18.8, 26.6)
#ymin2 <- c(42.3, 54.2, 14.8, 10.4)
ymin2 <- c(54.2, 42.3, 10.4, 14.8)
#ymax2 <- c(67.4, 76.7, 38.4, 27.2)
ymax2 <- c(76.7, 67.4, 27.2, 38.4)
df2 <- data.frame(Age2, Indicator2, Prevalence2, ymin2, ymax2)
levels(df2$Age2) <- c("9-23 months", "24-59 months")

# Creating plot

png(paste0(dir, 'Figure2.png'), 12, 8, units='in', res=300)

ggplot(df2, aes(x=Indicator2, y=Prevalence2, fill=Age2)) + 
  geom_hline(yintercept = c(20,40,60,80,100), color="grey93") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=ymin2, ymax=ymax2), width=.2, position=position_dodge(.9)) + 
  scale_fill_manual("Age of Child", values = c("24-59 months" = "slategray2", "9-23 months" = "steelblue")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 95, 100), 
                     labels = c("0%", "20%", "40%", "60%", "80%", "95%", "100%"),
                     limits = c(0, 100)) + 
  ggtitle("Measles vaccine coverage rates in children 9-59 months") +
  xlab("Report of vaccination") + 
  ylab("Vaccination coverage rate") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15, face = "bold", margin = unit(c(5, 0, 0, 0), "mm")), 
        axis.title.y = element_text(size = 15, face = "bold", margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.ticks=element_blank()) + 
        geom_hline(yintercept = 95, color="firebrick") +
        geom_hline(yintercept = 0, color="black", lwd=1) +
        geom_segment(aes(x = 0, y = 0, xend = 0, yend = 100), color="black", lwd=1.15) +
        #geom_vline(xintercept = 0.4, color="black", lwd=1) +
        #geom_line(aes(a = "cutoff")) + 
        annotate("text", x=1, y=82, label="OR=1.56", size=6) +
        annotate("text", x=2, y=82, label="OR=0.64", size=6)  
        
  
dev.off()







