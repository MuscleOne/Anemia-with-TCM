# 随机森林教程

# 读取数据
data("Boston", package = "MASS")
head(Boston)

# Fitting the machine learning model

set.seed(42)
library("iml")
library("randomForest")
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, ntree = 50)


X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

imp <- FeatureImp$new(predictor, loss = "mae")
library("ggplot2")
plot(imp)

shapley <- Shapley$new(predictor, x.interest = X[1, ])
shapley$plot()

## 关于贫血的样例
rf_贫血 = randomForest(
  随访贫血是否改善~CRP差异+IL6差异,
  data = df_差异作图, ntree = 50
)

y_贫血 = df_差异作图$随访贫血是否改善
x_贫血 = df_差异作图[, c("CRP差异", "IL6差异")]






