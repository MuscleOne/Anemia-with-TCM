####方差分析

#读取 circAnks1a 的 qPCR 定量数据
circAnks1a <- read.delim('circAnks1a_qPCR.txt', stringsAsFactors = FALSE)

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

####线性回归

#读取 circAnks1a 的 qPCR 定量数据
circAnks1a <- read.delim('circAnks1a_qPCR.txt', stringsAsFactors = FALSE)

#预定义因子水平，也就是给分组按时间排个序
circAnks1a$Group <- factor(circAnks1a$Group, levels = c('Sham', 'SNL 3d', 'Day 7d', 'Day 10d', 'Day 14d'))

##一般来说，一般线性模型也对响应变量的正态性和方差齐性有要求，但在很多情况下经常被忽略
#如果您期望评估这两种假设条件，方法和上文方差分析一样
#可使用 car 包 QQ-plot 检查数据是否符合正态分布（所有的点都离直线很近，落在置信区间内说明正态性良好）
library(car)
qqPlot(lm(Relative_expression~Group, data = circAnks1a), simulate = TRUE, main = 'QQ Plot', labels = FALSE)

#可使用 Bartlett 检验进行方差齐性检验（p 值大于 0.05 说明等方差）
bartlett.test(Relative_expression~Group, data = circAnks1a)

##类别（或因子）型自变量的一元线性模型
fit_lm <- lm(Relative_expression~Group, data = circAnks1a)
summary(fit_lm)  #查看线性模型概要

##对于类别（或因子）型自变量的一元线性模型，仍可使用 Tukey HSD 检验进行事后多重比较，继续探寻两两分组间的差异
#仍然可使用 multcomp 包中的方法执行 Tukey HSD 检验
library(multcomp)

tuk_lm <- glht(fit_lm, alternative = 'two.sided', linfct = mcp(Group = 'Tukey'))
summary(tuk_lm)  #查看 Tukey HSD 概要

tuk_lm.cld <- cld(tuk_lm, level = 0.05, decreasing = TRUE)
tuk_lm.cld  #按差异高低自动标识了 a、b、c 水平
plot(tuk_lm.cld, col = c('#7CBAD8', '#E1EBF4', '#D3E599', '#F9D2B8', '#E7CBE2'))


####当线性模型的自变量为类别或因子型时，R函数lm()如何处理？

#查看自变量预定义的因子水平和顺序
levels(circAnks1a$Group)

#通过 contrasts() 查看名义变量的编码过程
contrasts(circAnks1a$Group)

#查看上文线性模型的结果概要
summary(fit_lm)
