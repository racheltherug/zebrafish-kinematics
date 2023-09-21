library(rhdf5)
library(lme4)
library(emmeans)
library(tidyverse)
# setwd("C:/Working Directory/Kinematics Experiments")

# read in file for individual fish, choose Tail Angle data frame
h5f <- h5read("TRUN8.5_2023_09_11-09_44_44.h5","/dataForWell0/dataForAnimal0/dataPerFrame")

# turn .h5 into .csv
write.csv(h5f,"TRUN.8.5.csv",row.names=FALSE)

# read in files
tail_angle_MED1 <- read.csv("Combined Data_MED1.csv")
tail_angle_MED3 <- read.csv("Combined Data_MED3.csv")
tail_angle_MED10 <- read.csv("Combined Data_MED10.csv")
tail_angle_MED11  <- read.csv("Combined Data_MED11.csv")
tail_angle_MED12 <- read.csv("Combined Data_MED12.csv")
tail_angle_MED13 <- read.csv("Combined Data_MED13.csv")

tail_angle_TRUN1 <- read.csv("Combined Data_TRUN1.csv")
tail_angle_TRUN2 <- read.csv("Combined Data_TRUN2.csv")
tail_angle_TRUN3 <- read.csv("Combined Data_TRUN3.csv")
tail_angle_TRUN4 <- read.csv("Combined Data_TRUN4.csv")
tail_angle_TRUN6 <- read.csv("Combined Data_TRUN6.csv")
tail_angle_TRUN7 <- read.csv("Combined Data_TRUN7.csv")
tail_angle_TRUN8 <- read.csv("Combined Data_TRUN8.csv")

tail_angle_WT11 <- read.csv("Combined Data_WT11.csv")
tail_angle_WT10 <- read.csv("Combined Data_WT10.csv")
tail_angle_WT6 <- read.csv("Combined Data_WT6.csv")
tail_angle_WT5 <- read.csv("Combined Data_WT5.csv")
tail_angle_WT3 <- read.csv("Combined Data_WT3.csv")
tail_angle_WT2 <- read.csv("Combined Data_WT2.csv")
tail_angle_WT1 <- read.csv("Combined Data_WT1.csv")


# absolute value of Tail Angle

## medaka
tail_angle_MED1$TailAngle <- abs(tail_angle_MED1$TailAngle)
tail_angle_MED3$TailAngle <- abs(tail_angle_MED3$TailAngle)
tail_angle_MED10$TailAngle <- abs(tail_angle_MED10$TailAngle)
tail_angle_MED11$TailAngle <- abs(tail_angle_MED11$TailAngle)
tail_angle_MED12$TailAngle <- abs(tail_angle_MED12$TailAngle)
tail_angle_MED13$TailAngle <- abs(tail_angle_MED13$TailAngle)

## truncate
tail_angle_TRUN1$TailAngle <- abs(tail_angle_TRUN1$TailAngle)
tail_angle_TRUN2$TailAngle <- abs(tail_angle_TRUN2$TailAngle)
tail_angle_TRUN3$TailAngle <- abs(tail_angle_TRUN3$TailAngle)
tail_angle_TRUN4$TailAngle <- abs(tail_angle_TRUN4$TailAngle)
tail_angle_TRUN6$TailAngle <- abs(tail_angle_TRUN6$TailAngle)
tail_angle_TRUN7$TailAngle <- abs(tail_angle_TRUN7$TailAngle)
tail_angle_TRUN8$TailAngle <- abs(tail_angle_TRUN8$TailAngle)

## wild type
tail_angle_WT11$TailAngle <- abs(tail_angle_WT11$TailAngle)
tail_angle_WT10$TailAngle <- abs(tail_angle_WT10$TailAngle)
tail_angle_WT6$TailAngle <- abs(tail_angle_WT6$TailAngle)
tail_angle_WT5$TailAngle <- abs(tail_angle_WT5$TailAngle)
tail_angle_WT3$TailAngle <- abs(tail_angle_WT3$TailAngle)
tail_angle_WT2$TailAngle <- abs(tail_angle_WT2$TailAngle)
tail_angle_WT1$TailAngle <- abs(tail_angle_WT1$TailAngle)


# graph frame vs tail angle in .csv file
tail_angle.g <- ggplot(tail_angle_WT11,aes(Percentage,TailAngle),color=Trial)+geom_point(aes(color=Trial))+geom_smooth()
tail_angle.g

tail_angle_2.g <- ggplot(tail_angle_MED13,aes(Percentage,TailAngle),color=Trial)+geom_point(aes(color=Trial))+geom_smooth()
tail_angle_2.g

tail_angle_3.g <- ggplot(tail_angle_WT11,aes(Frame,TailAngle),color=Trial)+geom_point(aes(color=Trial))
tail_angle_3.g

tail_angle_4.g <- ggplot(tail_angle_TRUN8,aes(Percentage,TailAngle))+geom_point(aes(color=Trial))+geom_smooth()
tail_angle_4.g


#finding max + speed of each recording
max<-tail_angle_TRUN8%>%
  group_by(Trial)%>%
  summarize(max_angle=max(abs(TailAngle)),Speed=which.max(TailAngle)/max(Frame),Recovery=1-Speed)%>%
  print()


## AVERAGE OF EACH TRIAL'S MAX
mean(max$max_angle)

max_WT6<-tail_angle_TRUN6%>%
  group_by(Trial)%>%
  summarize(max_angle=max(abs(TailAngle)))%>%
  print()

mean(max_WT6$max_angle)

# duration of c-start
maxFrame <- tail_angle_WT11 %>%
  group_by(Trial) %>%
  summarize(Duration=max(Frame)/1057) %>%
  print()

# graphing the means
max_means <- read.csv("Mean Angles.csv")

means <- ggplot(max_means,aes(Condition,Max))+geom_boxplot()+ylab("Maximum Tail Angle (radians)")+annotate(geom="text",x=1,y=1.6,label='a')+annotate(geom="text",x=2,y=2.3,label='b')+annotate(geom="text",x=3,y=2.37,label='b')+theme_classic()
means


means2 <- ggplot(max_means,aes(Condition,Duration))+geom_boxplot()+ylab("Duration of C-Start (seconds)")+theme_classic()
means2

# speed
speed_acc <- ggplot(max_means,aes(Fish.ID,Max,fill=Condition))+geom_boxplot()+geom_jitter()+theme_classic()
speed_acc

speed_dec <-ggplot(max_means,aes(Fish.ID,Duration,fill=Condition))+geom_boxplot()+geom_jitter()
speed_dec

# stats
maxANOVA <- lmer(Max~Condition+(1|Fish.ID),data=max_means)
maxaov<-anova(maxANOVA)
tukmax<-emmeans(maxANOVA,~Condition)
pairs(tukmax)

speedANOVA <- lmer(Speed~Condition+(1|Fish.ID),data=max_means)
speedaov<-anova(speedANOVA)
tukspeed<-emmeans(speedANOVA,~Condition)
pairs(tukspeed)

durANOVA <- lmer(Duration~Condition+(1|Fish.ID),data=max_means)
duraov <-anova(durANOVA)
tukdur<-emmeans(durANOVA,~Condition)
pairs(tukdur)