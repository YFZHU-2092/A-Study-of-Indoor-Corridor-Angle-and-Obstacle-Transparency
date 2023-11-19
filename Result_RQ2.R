setwd("D:/研究生工作/2022下半本科毕设/数据处理后汇总")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(patchwork)
library(hrbrthemes)
library(car)
library(rstatix)
library(HH)
library(BruceR)
library(patchwork)
mbltxt = read.csv("RQ2.csv")
mbltxt$Viewable = factor(mbltxt$Viewable) # Rv4
mbltxt$Vertical_or_not = factor(mbltxt$Vertical_or_not) # Rv4
mbltxt$Gender = factor(mbltxt$Gender)
summary(mbltxt)

summary(mbltxt[mbltxt$Viewable == "UnViewed",])
summary(mbltxt[mbltxt$Viewable == "Viewed",])
summary(mbltxt[mbltxt$Vertical_or_not == "UnVertical",])
summary(mbltxt[mbltxt$Vertical_or_not == "Vertical",])

#mbltxt[mbltxt == 0]
mbltxt$group = ifelse(mbltxt$Viewable == "UnViewed" & mbltxt$Vertical_or_not == "UnVertical", "Group 1",
                      ifelse(mbltxt$Viewable == "Viewed" & mbltxt$Vertical_or_not == "UnVertical", "Group 2",
                             ifelse(mbltxt$Viewable == "UnViewed" & mbltxt$Vertical_or_not == "Vertical", "Group 3", "Group 4")))
mbltxt$group = factor(mbltxt$group)
summary(mbltxt)

##############第一步验证位置记忆存在误差############################
##############第一步验证位置记忆存在误差############################
##############第一步验证位置记忆存在误差############################
##############第一步验证位置记忆存在误差############################
shapiro.test(mbltxt$Reload_error)
shapiro.test(mbltxt$Reload_T_error)


library(dplyr)
group_shap <- mbltxt%>%
  group_by(group) %>%
  summarize(
    Reload_error_index = shapiro.test(Reload_error)$p.value,
    Reload_T_error_index = shapiro.test(Reload_T_error)$p.value,
  )
group_shap

library(car)
hist(mbltxt$Reload_error)
powerTransform(mbltxt$Reload_error)
powerTransform(mbltxt$Reload_error[mbltxt$group == "Group 1"])
hist(mbltxt$Reload_error[mbltxt$group == "Group 1"])
mbltxt$Reload_error <- (mbltxt$Reload_error)^(1/2)


hist(mbltxt$Reload_T_error)
powerTransform(mbltxt$Reload_T_error)
powerTransform(mbltxt$Reload_T_error[mbltxt$group == "Group 3"])
hist(mbltxt$Reload_T_error[mbltxt$group == "Group 3"])
mbltxt$Reload_T_error <- (mbltxt$Reload_T_error)^(-1/2)

#参数检验
library(car)
leveneTest(Reload_error ~ group, data=mbltxt, center=median)
leveneTest(Reload_T_error ~ group, data=mbltxt, center=median)

g <- ggplot(mbltxt, aes(x = group,y = Reload_error, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(Reload_error ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)

library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(car)
library(rstatix)
library(HH)
library(BruceR)
library(patchwork)
g <- ggplot(mbltxt, aes(x = group,y = Reload_T_error, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(Reload_T_error ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)

fit <- aov(Reload_error ~ Reload_T_error + Viewable * Vertical_or_not, data=mbltxt)
summary(fit)
partial_eta_squared(fit)


##############第二步验证存在距离记忆的差异############################
##############第二步验证存在距离记忆的差异############################
##############第二步验证存在距离记忆的差异############################
##############第二步验证存在距离记忆的差异############################
#方差分析基本假设检验 # convert to nominal factor
shapiro.test(mbltxt$Pre_table)
shapiro.test(mbltxt$Pre_write)
shapiro.test(mbltxt$Pre_write_zoom)
shapiro.test(mbltxt$Estimate_err)


library(dplyr)
group_shap <- mbltxt%>%
  group_by(group) %>%
  summarize(
    Pre_table_index = shapiro.test(Pre_table)$p.value,
    Pre_write_index = shapiro.test(Pre_write)$p.value,
    Pre_write_zoom_index = shapiro.test(Pre_write_zoom)$p.value,
    Estimate_err_index = shapiro.test(Estimate_err)$p.value,
  )
group_shap

library(car)
hist(mbltxt$Pre_table)
powerTransform(mbltxt$Pre_table)
powerTransform(mbltxt$Pre_table[mbltxt$group == "Group 1"])
powerTransform(mbltxt$Pre_table[mbltxt$group == "Group 2"])
powerTransform(mbltxt$Pre_table[mbltxt$group == "Group 3"])
powerTransform(mbltxt$Pre_table[mbltxt$group == "Group 4"])
hist(mbltxt$Pre_table[mbltxt$group == "Group 1"])
hist(mbltxt$Pre_table[mbltxt$group == "Group 2"])
hist(mbltxt$Pre_table[mbltxt$group == "Group 3"])
hist(mbltxt$Pre_table[mbltxt$group == "Group 4"])
#mbltxt$Pre_table <- (mbltxt$Pre_table)^(-1/16)


hist(mbltxt$Pre_write)
powerTransform(mbltxt$Pre_write)
mbltxt$Pre_write <- (mbltxt$Pre_write)^(-1/8)


hist(mbltxt$Estimate_err)
powerTransform(mbltxt$Estimate_err)
mbltxt$Estimate_err <- (mbltxt$Estimate_err)^(-1/3)


## 不满足正态性使用非参数检验
library(ggplot2)
library(ggsignif)
kruskal.test(Pre_table~group,data = mbltxt)
g <- ggplot(mbltxt, aes(x = group,y = Pre_table, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

library(ARTool) # for art, artlm
m = art(Pre_table ~ Viewable * Vertical_or_not, data=mbltxt) # uses LMM
anova(m) # report anova
#2*2不需要事后检验


## 满足正态性使用参数检验
library(car)
leveneTest(Pre_write ~ group, data=mbltxt, center=median)
leveneTest(Pre_write_zoom ~ group, data=mbltxt, center=median)

g <- ggplot(mbltxt, aes(x = group,y = Pre_write, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(Pre_write ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)

partial_eta_squared(m)

library(ggsignif)
g <- ggplot(mbltxt, aes(x = group,y = Pre_write_zoom, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
g <- g +geom_signif(
  comparisons = list(c("Group 1", "Group 2"), c("Group 1", "Group 3"), c("Group 1", "Group 4"),
                     c("Group 2", "Group 3"), c("Group 2", "Group 4"),
                     c("Group 3", "Group 4")),
  map_signif_level = T,
  step_increase = 0.1,
  textsize = 4,
  vjust = 1.5,
  test=t.test
)
print(g)

m = aov(Pre_write_zoom ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)



####### 先去验证认知和实际位置之间的关联#######
####### 先去验证认知和实际位置之间的关联#######
####### 先去验证认知和实际位置之间的关联#######
####### 先去验证认知和实际位置之间的关联#######
setwd("D:/研究生工作/2022下半本科毕设/数据处理后汇总")
Sheading = read.csv("RQ2-1.csv")
head(Sheading)
Sheading$Subject = factor(Sheading$Subject) # Rv4
Sheading$Trail = factor(Sheading$Trail) # Rv4
Sheading$Viewable  = factor(Sheading$Viewable ) # Rv4
Sheading$Vertical_or_not  = factor(Sheading$Vertical_or_not ) # Rv4
Sheading$Gender = factor(Sheading$Gender) # Rv4
summary(Sheading)

shapiro.test(Sheading$Estimate_err)
shapiro.test(Sheading$ERR_R)
shapiro.test(Sheading$Speed)
shapiro.test(Sheading$Var)


hist(Sheading$Estimate_err)
powerTransform(Sheading$Estimate_err)
Sheading$Estimate_err <- (Sheading$Estimate_err)^(1/6)
shapiro.test(Sheading$Estimate_err)

powerTransform(Sheading$Var)
Sheading$Var <- (Sheading$Var)^(-1/8)
shapiro.test(Sheading$Var)

hist(Sheading$ERR_R)
shapiro.test(Sheading$ERR_R)
powerTransform(Sheading$ERR_R)
Sheading$ERR_R <- (Sheading$ERR_R)^(1/5)
shapiro.test(Sheading$ERR_R)

hist(Sheading$Speed)
shapiro.test(Sheading$Speed)
powerTransform(Sheading$Speed)
Sheading$Speed <- (Sheading$Speed)^(-1/2)
shapiro.test(Sheading$Speed)


hist(Sheading$Var)
shapiro.test(Sheading$Var)
powerTransform(Sheading$Var)
Sheading$Var <- (Sheading$Var)^(1/2)

p1 <- ggplot(Sheading, aes(x=Estimate_err, y=Speed)) +
  geom_point()  + ylab("(Speed)^-1/2")+ xlab("(Estimate_err)^1/6") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=FALSE) +
  theme_ipsum()+ 
  stat_cor(data=Sheading, method = "pearson")
print(p1)

fit <- aov(ERR_R ~ O_D + Estimate_err + Viewable * Vertical_or_not, data=Sheading)
summary(fit)
partial_eta_squared(fit)

fit <- aov(Speed ~ O_D + ERR_R + Estimate_err + Viewable * Vertical_or_not, data=Sheading)
summary(fit)
partial_eta_squared(fit)
ancova(Speed ~ Estimate_err + Vertical_or_not, data=Sheading)
cor.test(Sheading$Estimate_err, Sheading$Speed, method=c("pearson", "kendall", "spearman"))

fit <- aov(Var ~ O_D + ERR_R + Estimate_err + Viewable * Vertical_or_not, data=Sheading)
summary(fit)
partial_eta_squared(fit)
ancova(Var ~ Estimate_err + Vertical_or_not, data=Sheading)

fit <- aov(Estimate_err ~ O_D + ERR_R + Viewable * Vertical_or_not, data=Sheading)
summary(fit)
partial_eta_squared(fit)

cor.test(Sheading$Estimate_err, Sheading$ERR_R, method=c("pearson", "kendall", "spearman"))
fit <- aov(Estimate_err ~ O_D + ERR_R + Viewable * Vertical_or_not, data=Sheading[Sheading$Trail=="Trail_1",])
summary(fit)
partial_eta_squared(fit)


##############寻找原因是否有方向性############################
##############寻找原因是否有方向性############################
##############寻找原因是否有方向性############################
##############寻找原因是否有方向性############################
##############寻找原因是否有方向性############################
shapiro.test(mbltxt$Trail_1)
shapiro.test(mbltxt$Trail_2)
shapiro.test(mbltxt$Trail_3)
shapiro.test(mbltxt$Trail_4)

mbltxt$Trail_Shape <- (mbltxt$Trail_1+mbltxt$Trail_3)/(mbltxt$Trail_2+mbltxt$Trail_4)

shapiro.test(mbltxt$Trail_Shape)
library(dplyr)
group_shap <- mbltxt%>%
  group_by(group) %>%
  summarize(
    Trail_Shape_index = shapiro.test(Trail_Shape)$p.value,
  )
group_shap

## 满足正态性使用参数检验
library(car)
leveneTest(Trail_Shape ~ group, data=mbltxt, center=median)

g <- ggplot(mbltxt, aes(x = group,y = Trail_Shape, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)
m = aov(Trail_Shape ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)


shapiro.test(mbltxt$ERR_1)
shapiro.test(mbltxt$ERR_2)
shapiro.test(mbltxt$ERR_3)
shapiro.test(mbltxt$ERR_4)
library(dplyr)
group_shap <- mbltxt%>%
  group_by(group) %>%
  summarize(
    ERR_1_index = shapiro.test(ERR_1)$p.value,
    ERR_2_index = shapiro.test(ERR_2)$p.value,
    ERR_3_index = shapiro.test(ERR_3)$p.value,
    ERR_4_index = shapiro.test(ERR_4)$p.value,
  )
group_shap

#mbltxt$ERR_1 <- (mbltxt$ERR_1)
powerTransform(mbltxt$ERR_2)
hist(mbltxt$ERR_2)
mbltxt$ERR_2 <- (mbltxt$ERR_2)^(1/2)
powerTransform(mbltxt$ERR_3)
hist(mbltxt$ERR_3)
mbltxt$ERR_3 <- (mbltxt$ERR_3)^(1/2)
powerTransform(mbltxt$ERR_4)
hist(mbltxt$ERR_4)
mbltxt$ERR_4 <- (mbltxt$ERR_4)^(1/2)

m = aov(ERR_1 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)
m = aov(ERR_2 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
m = aov(ERR_3 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
m = aov(ERR_4 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)

###########寻找一下指向任务上的差异############################
###########寻找一下指向任务上的差异############################
###########寻找一下指向任务上的差异############################
###########寻找一下指向任务上的差异############################
###########寻找一下指向任务上的差异############################
##思路上依然是先检验一下差异说明指向任务也有衰减，再看变形
##汇报上先说明有无变形相应距离估计结果，再汇报整体差异不明显
###误差
#mbltxt$angle_1 <- (mbltxt$D_10+mbltxt$D_12)/2
#mbltxt$angle_2 <- (mbltxt$D_16+mbltxt$D_18)/2
#mbltxt$angle_3 <- (mbltxt$D_22+mbltxt$D_24)/2


mbltxt$angle_1 <- (mbltxt$Angle_423)
mbltxt$angle_2 <- (mbltxt$Angle_432)
mbltxt$angle_3 <- (mbltxt$Angle_243)

shapiro.test(mbltxt$angle_1)
shapiro.test(mbltxt$angle_2)
shapiro.test(mbltxt$angle_3)
shapiro.test(mbltxt$laser_timing)
shapiro.test(mbltxt$laser_task_error)

library(dplyr)
group_shap <- mbltxt%>%
  group_by(group) %>%
  summarize(
    angle_1_index = shapiro.test(angle_1)$p.value,
    angle_2_index = shapiro.test(angle_2)$p.value,
    angle_3_index = shapiro.test(angle_3)$p.value,
    laser_timing_index = shapiro.test(laser_timing)$p.value,
    laser_task_error_index = shapiro.test(laser_task_error)$p.value,
  )
group_shap

hist(mbltxt$angle_1)
##powerTransform(mbltxt$angle_1)
##mbltxt$angle_1 <- (mbltxt$angle_1)^(1/2)

hist(mbltxt$angle_2)
##powerTransform(mbltxt$angle_2)
##mbltxt$angle_2 <- (mbltxt$angle_2)^(1/2)

hist(mbltxt$angle_3)
##powerTransform(mbltxt$angle_3)
##mbltxt$angle_3 <- (mbltxt$angle_3)^(1/2)

hist(mbltxt$laser_timing)
##powerTransform(mbltxt$laser_timing)
#mbltxt$laser_timing <- (mbltxt$laser_timing)^(-1)

hist(mbltxt$laser_task_error)
powerTransform(mbltxt$laser_task_error)
mbltxt$laser_task_error <- (mbltxt$laser_task_error)^(1/2)

library(car)
leveneTest(angle_1 ~ group, data=mbltxt, center=median)
leveneTest(angle_2 ~ group, data=mbltxt, center=median)
leveneTest(angle_3 ~ group, data=mbltxt, center=median)

g <- ggplot(mbltxt, aes(x = group,y = angle_1, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(angle_1 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)
boxplot(angle_1~Vertical_or_not,data = mbltxt)

g <- ggplot(mbltxt, aes(x = group,y = angle_2, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(angle_2 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)
boxplot(angle_2~Vertical_or_not,data = mbltxt)

g <- ggplot(mbltxt, aes(x = group,y = angle_3, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(angle_3 ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
boxplot(angle_3~Vertical_or_not,data = mbltxt)

fit<-aov(laser_timing~Vertical_or_not*Viewable,data = mbltxt)
summary(fit)
partial_eta_squared(fit)
boxplot(laser_timing~Vertical_or_not,data = mbltxt)

fit<-aov(laser_timing~Viewable,data = mbltxt[mbltxt$Vertical_or_not == "Vertical",])
summary(fit)
partial_eta_squared(fit)
boxplot(laser_timing~Vertical_or_not,data = mbltxt)


library(ARTool) # for art, artlm
fit<-art(laser_task_error~Vertical_or_not*Viewable,data = mbltxt)
anova(fit)
boxplot(laser_task_error~Vertical_or_not,data = mbltxt)



#######相关性建模######
cor.test(mbltxt$ERR_1, mbltxt$angle_1, method=c("pearson", "kendall", "spearman"))
cor.test(mbltxt$Trail_Shape, mbltxt$angle_1, method=c("pearson", "kendall", "spearman"))
cor.test(mbltxt$ERR_1, mbltxt$Trail_Shape, method=c("pearson", "kendall", "spearman"))

p1 <- ggplot(mbltxt, aes(x=Trail_Shape, y=angle_1)) +
  geom_point()  + ylab("Trail_Shape")+ xlab("angle_1") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=FALSE) +
  theme_ipsum()+ 
  stat_cor(data=mbltxt, method = "pearson")
print(p1)


####### 验证行为#######
setwd("D:/研究生工作/2022下半本科毕设/数据处理后汇总")
Behaivor = read.csv("RQ3.csv")
head(Behaivor)
Behaivor$Viewable  = factor(Behaivor$Viewable ) # Rv4
Behaivor$Vertical_or_not  = factor(Behaivor$Vertical_or_not ) # Rv4
Behaivor$group = ifelse(Behaivor$Viewable == "UnViewed" & Behaivor$Vertical_or_not == "UnVertical", "Group 1",
                      ifelse(Behaivor$Viewable == "Viewed" & Behaivor$Vertical_or_not == "UnVertical", "Group 2",
                             ifelse(Behaivor$Viewable == "UnViewed" & Behaivor$Vertical_or_not == "Vertical", "Group 3", "Group 4")))
Behaivor$group = factor(Behaivor$group)
library(Hmisc) 
impute(Behaivor$Excess_length[Behaivor$group == "Group 4"], median) # 插补中位数值 
Behaivor$Excess_length[Behaivor$group == "Group 4"] <- impute(Behaivor$Excess_length[Behaivor$group == "Group 4"], median)
impute(Behaivor$Average_speed[Behaivor$group == "Group 4"], median) # 插补中位数值 
Behaivor$Average_speed[Behaivor$group == "Group 4"] <- impute(Behaivor$Average_speed[Behaivor$group == "Group 4"], median)
impute(Behaivor$N_VAR[Behaivor$group == "Group 4"], median) # 插补中位数值 
Behaivor$N_VAR[Behaivor$group == "Group 4"] <- impute(Behaivor$N_VAR[Behaivor$group == "Group 4"], median)
summary(Behaivor)

shapiro.test(Behaivor$Length_room)
shapiro.test(Behaivor$Room_frequency)
shapiro.test(Behaivor$corridor_time)

library(dplyr)
group_shap <- Behaivor%>%
  group_by(group) %>%
  summarize(
    Length_room_index = shapiro.test(Length_room)$p.value,
    Room_frequency_index = shapiro.test(Room_frequency)$p.value,
    corridor_time_index = shapiro.test(corridor_time)$p.value,
  )
group_shap

library(ARTool) # for art, artlm
m = art(Length_room ~ Viewable * Vertical_or_not, data=Behaivor) # uses LMM
anova(m) # report anova
partial_eta_squared(m)
m = art(Room_frequency ~ Viewable * Vertical_or_not, data=Behaivor) # uses LMM
anova(m) # report anova
m = art(corridor_time ~ Viewable * Vertical_or_not, data=Behaivor) # uses LMM
anova(m) # report anova




######相关图#########
Behaivor$Viewable = ifelse(Behaivor$Viewable == "UnViewed","0","1")
Behaivor$Vertical_or_not = ifelse(Behaivor$Vertical_or_not == "UnVertical","0","1")
Behaivor <- select(Behaivor,-c(group))
Behaivor$Viewable = as.numeric(Behaivor$Viewable)
Behaivor$Vertical_or_not = as.numeric(Behaivor$Vertical_or_not)
Behaivor$laser_task_error <- (Behaivor$laser_task_error)^(1/2)
Behaivor$Pre_write <- (Behaivor$Pre_write)^(-1/8)
Behaivor$N_VAR <- (Behaivor$N_VAR)^(1/2)
Behaivor$Average_speed <- (Behaivor$Average_speed)^(-1)
library(ggplot2)
library(ggcorrplot)
my_f_corralation <- function(dataset){
  corralation <- round(cor(dataset),3)
  ggcorrplot(corralation,method = "circle",lab=T)
  pmtdatas <- cor_pmat(dataset)
  ggcorrplot(corralation,# hc.order = T,  #分等级聚类重排矩阵
             ggtheme = ggplot2::theme_void(base_size = 15), #主题修改
             colors = c("CornflowerBlue","white","Salmon"), #自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
             lab = T,lab_size = 5,    #相关系数文本字体大小
             tl.cex = 15,             #坐标轴字体大小
             p.mat = pmtdatas,         #添加显著性信息
             sig.level = 0.05,        #显著性水平
             pch = 4,                 #不够显著的色块进行标记，pch表示选择不同的标记方法，可以尝试其他数字表示什么标记方法
             pch.cex = 10)            #不显著标记的大小，使用insig = "blank"将不显著的空白处理
}
my_f_corralation(Behaivor)

cor.test(Behaivor$Education, Behaivor$laser_task_error, method=c("pearson", "kendall", "spearman"))
cor.test(Behaivor$VR_exp, Behaivor$laser_task_error, method=c("pearson", "kendall", "spearman"))
cor.test(Behaivor$Math_Grade, Behaivor$laser_task_error, method=c("pearson", "kendall", "spearman"))
cor.test(Behaivor$Age, Behaivor$Average_speed, method=c("pearson", "kendall", "spearman"))

p1 <- ggplot(Behaivor, aes(x=Age, y=Average_speed)) +
  geom_point()  + ylab("Age")+ xlab("Average_speed") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=FALSE) +
  theme_ipsum()+ 
  stat_cor(data=Behaivor, method = "pearson")
print(p1)

fit <- aov(Age ~ Viewable*Vertical_or_not,data = Behaivor)
summary(fit)

