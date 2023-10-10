library(tidyverse)

f <- list.files(pattern=".csv")
f <- f[grep("Combined",f)]

d_l <- lapply(f,read_csv)

#This is because not all files have the same col names
d <- lapply(d_l,function(x) x %>% select(Frame:Trial)) %>% do.call(rbind,.)

d2 <- d %>% 
  group_by(Condition,`Fish ID`,Trial) %>% 
  as.numeric(d$TailAngle) %>%
  mutate(frame_per=Frame/max(Frame),TailAngle=abs(TailAngle)) 

d2%>% 
  ggplot(aes(frame_per,TailAngle,col=as.factor(Trial)))+geom_point()+facet_wrap(.~Condition)

d3 <- d2 %>% 
  summarise(max_ang=max(TailAngle))

d3 %>%
  ggplot(aes(Condition,max_ang))+geom_boxplot()

ang_aov <- anova(lm(max_ang~Condition,data = d3))

ang_aov2 <- aov(max_ang~Condition,data = d3)
TukeyHSD(ang_aov2)  
