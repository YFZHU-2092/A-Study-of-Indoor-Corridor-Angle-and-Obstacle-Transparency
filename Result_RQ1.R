setwd("D:/研究生工作/2022下半本科毕设/数据处理后汇总")
mbltxt = read.csv("RQ1.csv")
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(car)
library(rstatix)
library(HH)
library(BruceR)
library(patchwork)
mbltxt$Viewable = factor(mbltxt$Viewable) # Rv4
mbltxt$Vertical_or_not = factor(mbltxt$Vertical_or_not) # Rv4
mbltxt$Gender = factor(mbltxt$Gender)
summary(mbltxt)

summary(mbltxt[mbltxt$Viewable == "UnViewed",])
summary(mbltxt[mbltxt$Viewable == "Viewed",])
summary(mbltxt[mbltxt$Vertical_or_not == "UnVertical",])
summary(mbltxt[mbltxt$Vertical_or_not == "Vertical",])

#计算描述统计分析(Excess length and speed)
summary(mbltxt)
mbltxt[mbltxt == 0]

mbltxt$group = ifelse(mbltxt$Viewable == "UnViewed" & mbltxt$Vertical_or_not == "UnVertical", "Group 1",
                     ifelse(mbltxt$Viewable == "Viewed" & mbltxt$Vertical_or_not == "UnVertical", "Group 2",
                            ifelse(mbltxt$Viewable == "UnViewed" & mbltxt$Vertical_or_not == "Vertical", "Group 3", "Group 4")))
mbltxt$group = factor(mbltxt$group)

library(Hmisc) 
impute(mbltxt$Excess_length[mbltxt$group == "Group 4"], median) # 插补中位数值 
mbltxt$Excess_length[mbltxt$group == "Group 4"] <- impute(mbltxt$Excess_length[mbltxt$group == "Group 4"], median)
impute(mbltxt$Average_speed[mbltxt$group == "Group 4"], median) # 插补中位数值 
mbltxt$Average_speed[mbltxt$group == "Group 4"] <- impute(mbltxt$Average_speed[mbltxt$group == "Group 4"], median)
impute(mbltxt$Tlength[mbltxt$group == "Group 4"], median) # 插补中位数值 
mbltxt$Tlength[mbltxt$group == "Group 4"] <- impute(mbltxt$Tlength[mbltxt$group == "Group 4"], median)
impute(mbltxt$Tspeed[mbltxt$group == "Group 4"], median) # 插补中位数值 
mbltxt$Tspeed[mbltxt$group == "Group 4"] <- impute(mbltxt$Tspeed[mbltxt$group == "Group 4"], median)
impute(mbltxt$T_VAR[mbltxt$group == "Group 4"], median) # 插补中位数值 
mbltxt$T_VAR[mbltxt$group == "Group 4"] <- impute(mbltxt$T_VAR[mbltxt$group == "Group 4"], median)
impute(mbltxt$N_VAR[mbltxt$group == "Group 4"], median) # 插补中位数值 
mbltxt$N_VAR[mbltxt$group == "Group 4"] <- impute(mbltxt$N_VAR[mbltxt$group == "Group 4"], median)


#方差分析基本假设检验 # convert to nominal factor
shapiro.test(mbltxt$Excess_length)
shapiro.test(mbltxt$Average_speed)

library(dplyr)
group_shap <- mbltxt%>%
  group_by(group) %>%
  summarize(
    Excess_length_index = shapiro.test(Excess_length)$p.value,
    Average_speed_index = shapiro.test(Average_speed)$p.value,
    Tlength_index = shapiro.test(Tlength)$p.value,
    Tspeed_index = shapiro.test(Tspeed)$p.value,
    TVar_index = shapiro.test(T_VAR)$p.value,
    NVar_index = shapiro.test(N_VAR)$p.value
  )
group_shap

hist(mbltxt$Excess_length[mbltxt$group == "Group 4"])
hist(mbltxt$Excess_length[mbltxt$group == "Group 4"])
hist(mbltxt$Average_speed[mbltxt$group == "Group 3"])
hist(mbltxt$Average_speed[mbltxt$group == "Group 4"])
library(car)
hist(mbltxt$N_VAR)
powerTransform(mbltxt$N_VAR[mbltxt$group == "Group 4"])
mbltxt$N_VAR <- (mbltxt$N_VAR)^(1/2)
mbltxt$T_VAR <- (mbltxt$T_VAR)^(1/2)

hist(mbltxt$Tspeed)
powerTransform(mbltxt$Tspeed[mbltxt$group == "Group 4"])
powerTransform(mbltxt$Average_speed[mbltxt$group == "Group 4"])
mbltxt$Tspeed <- (mbltxt$Tspeed)^(-1)
mbltxt$Average_speed <- (mbltxt$Average_speed)^(-1)

## 不满足正态性使用非参数检验
library(ggplot2)
library(ggsignif)
kruskal.test(Excess_length~group,data = mbltxt)
g <- ggplot(mbltxt, aes(x = group,y = Average_speed, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

g <- ggplot(mbltxt, aes(x = group,y = N_VAR, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

library(ARTool) # for art, artlm
m = art(Excess_length ~ Viewable * Vertical_or_not, data=mbltxt) # uses LMM
fit <- anova(m) # report anova
fit
partial_eta_squared(fit)

m = art(Tlength ~ Viewable * Vertical_or_not, data=mbltxt) # uses LMM
fit <- anova(m) # report anova
fit
partial_eta_squared(fit)
#2*2不需要事后检验
library(phia)
testInteractions(artlm(m, "Viewable:Vertical_or_not"), 
                 pairwise=c("Viewable", "Vertical_or_not"), 
                 adjustment="holm")

## 满足正态性检查方差齐性
library(car)
leveneTest(Average_speed ~ group, data=mbltxt, center=median)
leveneTest(Tspeed ~ group, data=mbltxt, center=median)
leveneTest(T_VAR ~ group, data=mbltxt, center=median)
leveneTest(N_VAR ~ group, data=mbltxt, center=median)

# 但真正重要的是残差正态性【仅展示】
g <- ggplot(mbltxt, aes(x = group,y = Tspeed, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

g <- ggplot(mbltxt, aes(x = group,y = Average_speed, fill = Vertical_or_not))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue","lightpink"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)
m = aov(Tspeed ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)

m = aov(Average_speed ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)

#绘图原比例，Before box-cox
ggplot(mbltxt) +
  geom_bar( aes(x=group, y = Average_speed/13, fill = Vertical_or_not), stat="identity", alpha=1)+
  scale_fill_manual(values = c("lightblue","lightpink"))+
  geom_pointrange( aes(x=group, y=Average_speed, ymin=Average_speed, ymax=Average_speed), colour="black", alpha=1, size=0.3)

ggplot(mbltxt) +
  geom_bar( aes(x=group, y = N_VAR/13, fill = Vertical_or_not), stat="identity", alpha=1)+
  scale_fill_manual(values = c("lightblue","lightpink"))+
  geom_pointrange( aes(x=group, y=N_VAR, ymin=Average_speed, ymax=Average_speed), colour="black", alpha=1, size=0.3)


g <- ggplot(mbltxt, aes(x = group,y = T_VAR, fill = group))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

g <- ggplot(mbltxt, aes(x = group,y = N_VAR, fill = Vertical_or_not))
g <- g  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue","lightpink"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .1) +
  geom_point() 
print(g)

m = aov(T_VAR ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)

m = aov(N_VAR ~ Viewable * Vertical_or_not, data=mbltxt) # fit model
shapiro.test(residuals(m)) # test residuals
anova(m)
partial_eta_squared(m)
################行为校验# ###############
################行为校验# ###############
################行为校验# ###############
################行为校验# ###############
################行为校验# ###############
################行为校验# ###############
setwd("D:/研究生工作/2022下半本科毕设/数据处理后汇总")
Sheading = read.csv("RQ1-1.csv")
head(Sheading)
Sheading$Subject = factor(Sheading$Subject) # Rv4
Sheading$Trail = factor(Sheading$Trail) # Rv4
Sheading$TT = factor(Sheading$TT) # Rv4
Sheading$Stage = factor(Sheading$Stage, levels = c("S2", "S1")) # Rv4
Sheading$Viewable  = factor(Sheading$Viewable ) # Rv4
Sheading$Vertical_or_not  = factor(Sheading$Vertical_or_not ) # Rv4
Sheading$Gender = factor(Sheading$Gender) # Rv4
summary(Sheading)
#Sheading <- Sheading [Sheading$Ecess_length != max(Sheading$Ecess_length),]
#Sheading <- Sheading [Sheading$Ecess_length != min(Sheading$Ecess_length),]
#存在一个复数的最小值用同组数据次最小值替代，最大值被同组次最大值替代
#方差分析基本假设检验 # convert to nominal factor
library(dplyr)
group_shap <- Sheading%>%
  group_by(TT) %>%
  summarize(
    Var_index = shapiro.test(Var)$p.value,
    Excess_length_index = shapiro.test(Ecess_length)$p.value,
    Speed_index = shapiro.test(Speed)$p.value,
  )
group_shap

library(car)
hist(Sheading$Var)
powerTransform(Sheading$Var)
#Sheading$Var <- (Sheading$Var)^(1/2)
hist(Sheading$Speed)
powerTransform(Sheading$Speed)
#Sheading$Speed <- (Sheading$Speed)^(-1)
#先比较通道组差异,再用配对比较差异

###这里应该用配对检验试一试有没有差异，平均值不一定到位
###这里应该用配对检验
###这里应该用配对检验
summary(Sheading)
library(coin)
wilcox.test(Sheading[Sheading$Stage == "S1",]$Ecess_length,Sheading[Sheading$Stage == "S2",]$Ecess_length,paired=TRUE, exact=FALSE)
wilcox.test(Sheading[Sheading$Stage == "S1",]$Speed,Sheading[Sheading$Stage == "S2",]$Speed,paired=TRUE, exact=FALSE)
wilcox.test(Sheading[Sheading$Stage == "S1",]$Var,Sheading[Sheading$Stage == "S2",]$Var,paired=TRUE, exact=FALSE)

## distribution="asymptotic" 指定了在进行检验时使用的渐近分布。
## 也就是说，在进行检验时，函数会根据数据的分布情况来确定使用何种渐近分布。
summary(Sheading[Sheading$Stage=="S1",])
summary(Sheading[Sheading$Stage=="S2",])
library(ggplot2)
library(ggsignif)
result <- wilcox.test(Sheading[Sheading$Stage == "S1",]$Ecess_length,Sheading[Sheading$Stage == "S2",]$Ecess_length,paired=TRUE, exact=FALSE)
g <- ggplot(Sheading, aes(x =Ecess_length ,y = Stage, fill = Stage))
g <- g  +
  scale_fill_manual(values = c("lightgreen", "lightpink"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .3)
g <- g +geom_signif(
  comparisons = list(c("S1", "S2")),
  step_increase = 0.1,
  textsize = 4,
  vjust = 1.5,
  map_signif_level = T,
  test=wilcox.test,
  test.args = list(paired=TRUE, exact=FALSE)
)
print(g)

g <- ggplot(Sheading, aes(x =Speed ,y = Stage, fill = Stage))
g <- g  +
  scale_fill_manual(values = c("lightgreen", "lightpink"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .3)
g <- g +geom_signif(
  comparisons = list(c("S1", "S2")),
  map_signif_level = T,
  paired=TRUE,
  step_increase = 0.1,
  textsize = 4,
  vjust = 1.5,
  test=wilcox.test,
  test.args = list(paired=TRUE, exact=FALSE)
)
print(g)

g <- ggplot(Sheading, aes(x =Var ,y = Stage, fill = Stage))
g <- g  +
  scale_fill_manual(values = c("lightgreen", "lightpink"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .3)
g <- g +geom_signif(
  comparisons = list(c("S1", "S2")),
  map_signif_level = T,
  step_increase = 0.1,
  textsize = 4,
  vjust = 1.5,
  test=wilcox.test,
  test.args = list(paired=TRUE, exact=FALSE)
)
print(g)

# Friedman test on Var
friedman_test(Var ~ Trail | Subject, data=Sheading[Sheading$Stage == "S1",], distribution="asymptotic")
friedman_test(Var ~ Trail | Subject, data=Sheading[Sheading$Stage == "S2",], distribution="asymptotic")

# Friedman test on Speed
friedman_test(Speed ~ Trail | Subject, data=Sheading[Sheading$Stage == "S1",], distribution="asymptotic")
friedman_test(Speed ~ Trail | Subject, data=Sheading[Sheading$Stage == "S2",], distribution="asymptotic")


library(ggplot2)
library(ggsignif)
p <- ggplot(Sheading, aes(x = Stage,y = Var, fill = Trail))
p <- p  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .3)
print(p)


p <- ggplot(Sheading, aes(x = Stage,y = Speed, fill = Trail))
p <- p  + geom_smooth(method = "lm") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink","lightyellow"))+ 
  geom_boxplot(outlier.alpha = 1, coef = 0,color = "black", width = .3)
print(p)



t2.4=wilcox.test(Sheading[Sheading$TT=="Trail_2",]$Var, 
                 Sheading[Sheading$TT=="Trail_4",]$Var, 
                 paired=TRUE, exact=FALSE)
t4.6=wilcox.test(Sheading[Sheading$TT=="Trail_4",]$Var, 
                 Sheading[Sheading$TT=="Trail_6",]$Var, 
                 paired=TRUE, exact=FALSE)
t6.8=wilcox.test(Sheading[Sheading$TT=="Trail_6",]$Var, 
                 Sheading[Sheading$TT=="Trail_8",]$Var, 
                 paired=TRUE, exact=FALSE)
t2.8=wilcox.test(Sheading[Sheading$TT=="Trail_2",]$Var, 
                 Sheading[Sheading$TT=="Trail_8",]$Var, 
                 paired=TRUE, exact=FALSE)
t4.8=wilcox.test(Sheading[Sheading$TT=="Trail_4",]$Var, 
                 Sheading[Sheading$TT=="Trail_8",]$Var, 
                 paired=TRUE, exact=FALSE)
t2.6=wilcox.test(Sheading[Sheading$TT=="Trail_2",]$Var, 
                 Sheading[Sheading$TT=="Trail_6",]$Var, 
                 paired=TRUE, exact=FALSE)
p.adjust(c(t2.4$p.value, t4.6$p.value, t6.8$p.value,
           t2.8$p.value, t4.8$p.value, t2.6$p.value), method="holm")
t4.6


t2.4=wilcox.test(Sheading[Sheading$TT=="Trail_2",]$Speed, 
                 Sheading[Sheading$TT=="Trail_4",]$Speed, 
                 paired=TRUE, exact=FALSE)
t4.6=wilcox.test(Sheading[Sheading$TT=="Trail_4",]$Speed, 
                 Sheading[Sheading$TT=="Trail_6",]$Speed, 
                 paired=TRUE, exact=FALSE)
t6.8=wilcox.test(Sheading[Sheading$TT=="Trail_6",]$Speed, 
                 Sheading[Sheading$TT=="Trail_8",]$Speed, 
                 paired=TRUE, exact=FALSE)
t2.8=wilcox.test(Sheading[Sheading$TT=="Trail_2",]$Speed, 
                 Sheading[Sheading$TT=="Trail_8",]$Speed, 
                 paired=TRUE, exact=FALSE)
t4.8=wilcox.test(Sheading[Sheading$TT=="Trail_4",]$Speed, 
                 Sheading[Sheading$TT=="Trail_8",]$Speed, 
                 paired=TRUE, exact=FALSE)
t2.6=wilcox.test(Sheading[Sheading$TT=="Trail_2",]$Speed, 
                 Sheading[Sheading$TT=="Trail_6",]$Speed, 
                 paired=TRUE, exact=FALSE)
p.adjust(c(t2.4$p.value, t4.6$p.value, t6.8$p.value,
           t2.8$p.value, t4.8$p.value, t2.6$p.value), method="holm")


########建立协方差分析模型 ####
########建立协方差分析模型 ####
########建立协方差分析模型 ####
########建立协方差分析模型 ####
Sheading = read.csv("RQ1-1.csv")
head(Sheading)
Sheading$Subject = factor(Sheading$Subject) # Rv4
Sheading$Trail = factor(Sheading$Trail) # Rv4
Sheading$TT = factor(Sheading$TT) # Rv4
Sheading$Stage = factor(Sheading$Stage, levels = c("S2", "S1")) # Rv4
Sheading$Viewable  = factor(Sheading$Viewable ) # Rv4
Sheading$Vertical_or_not  = factor(Sheading$Vertical_or_not ) # Rv4
Sheading$Gender = factor(Sheading$Gender) # Rv4
summary(Sheading)

She_linear=Sheading[Sheading$Stage=="S2",]
She_linear$Verticality  = factor(She_linear$Verticality )

hist(She_linear$Var)
shapiro.test(She_linear$Var)
powerTransform(She_linear$Var)
She_linear$Var <- (She_linear$Var)^(-1/8)
shapiro.test(She_linear$Var)

cor.test(She_linear$Var,She_linear$O_D)
cor.test(She_linear$Var,She_linear$Verticality)

hist(She_linear$Speed)
shapiro.test(She_linear$Speed)
powerTransform(She_linear$Speed)
She_linear$Speed <- (She_linear$Speed)^(-1/2)
shapiro.test(She_linear$Speed)

cor.test(She_linear$Speed,She_linear$O_D)
cor.test(She_linear$Speed,She_linear$Verticality)

###依照讨论中所属，线性回归模型不适合做干扰变量排除，应采用协方差检验
#reg1 <- lm(Var ~ O_D + Verticality,data = She_linear)
#reg1
#summary(reg1)
#plot(reg1, which = 2)

#reg2 <- lm(Speed ~ O_D + Verticality,data = She_linear)
#summary(reg2)

library(multcomp)
library(car)
library(rstatix)
library(HH)
library(BruceR)
leveneTest(Speed ~ Vertical_or_not)
aov(O_D ~ Vertical_or_not ,data=She_linear)%>%
  summary()


fit <- aov(Var ~ O_D + Vertical_or_not*Viewable, data=She_linear)
summary(fit)
partial_eta_squared(fit)

ancova(Var ~ O_D + Vertical_or_not*Viewable, data=She_linear)


fit <- aov(Speed ~ O_D + Vertical_or_not*Viewable, data=She_linear)
summary(fit)
partial_eta_squared(fit)

ancova(Speed ~ O_D + Vertical_or_not*Viewable, data=She_linear)

