# 单因素方差分析学习

# 先做一个比较, 基线的血红蛋白数值和差值的关系

# 读入必要的包
# load the useful packages
library(dplyr)
library(plotly)
library(CBCgrps)
library(networkD3)

library(PerformanceAnalytics)

##randomForest 包的随机森林
library(randomForest)

#使用 MASS 包 polr() 训练序数 logistic 回归，详情 ?polr
library(MASS)

# 下载ggpubr
# install.packages("ggpubr")
library("ggpubr")

#读取 circAnks1a 的 qPCR 定量数据
circAnks1a <- read.delim('glm_anova/circAnks1a_qPCR.txt', stringsAsFactors = FALSE)

#预定义因子水平，也就是给分组按时间排个序
circAnks1a$Group <- factor(circAnks1a$Group, levels = c('Sham', 'SNL 3d', 'Day 7d', 'Day 10d', 'Day 14d'))

##首先需要对响应变量进行正态性或方差齐性评估，满足这些假设才能进行方差分析
#可使用 car 包 QQ-plot 检查数据是否符合正态分布（所有的点都离直线很近，落在置信区间内说明正态性良好）
library(car)
qqPlot(lm(Relative_expression~Group, data = circAnks1a), simulate = TRUE, main = 'QQ Plot', labels = FALSE)

#可使用 Bartlett 检验进行方差齐性检验（p 值大于 0.05 说明等方差）
bartlett.test(Relative_expression~Group, data = circAnks1a)

##上述两个假设通过，执行单因素方差分析（One Way ANOVA）
fit_aov <- aov(Relative_expression~Group, data = circAnks1a)
summary(fit_aov)  #查看方差分析概要

##方差分析后，使用 Tukey HSD 检验进行事后多重比较，继续探寻两两分组间的差异
#multcomp 包提供了直观的结果比较
library(multcomp)

tuk_aov <- glht(fit_aov, alternative = 'two.sided', linfct = mcp(Group = 'Tukey'))
summary(tuk_aov)  #查看 Tukey HSD 概要

tuk_aov.cld <- cld(tuk_aov, level = 0.05, decreasing = TRUE)
tuk_aov.cld  #按差异高低自动标识了 a、b、c 水平
plot(tuk_aov.cld, col = c('#7CBAD8', '#E1EBF4', '#D3E599', '#F9D2B8', '#E7CBE2'))



















