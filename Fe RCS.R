# 加载必要的包
library(readr)
library(rms)
library(ggplot2)
library(dplyr)
library(car)
# 读取数据
df <- read.csv('金属.csv')
# 数据预处理
df$optimal_embryo <- factor(df$optimal_embryo, levels = c(0, 1)) # 转换为因子
#标准化金属浓度
#df$Fe <- scale(df$Fe)
# 创建RCS模型
dd <- datadist(df)
options(datadist = "dd")
model <- lrm(
  optimal_embryo ~ rcs(Fe, 4) + age + BMI,  # Fe使用4个节点
  data = df,
  x = TRUE,  # 存储设计矩阵
  y = TRUE   # 存储响应变量
)
summary(model)
anova(model)
#生成预测数据
pred_data <- expand.grid(
  Fe = seq(min(df$Fe), max(df$Fe), length.out = 150),
  age = mean(df$age),  # 固定年龄为均值
  BMI = mean(df$BMI)   # 固定BMI为均值
)
#预测几率（对数）
pred <- predict(model, 
                newdata = pred_data, 
                type = "lp",  # 线性预测值
                se.fit = TRUE)

pred_data <- cbind(pred_data, pred)
#设置95%置信区间
pred_data <- pred_data %>% 
  mutate(
    lower = linear.predictors - 1.96 * se.fit,
    upper = linear.predictors + 1.96 * se.fit
  )

ggplot(pred_data, aes(x = Fe, y = linear.predictors)) +
  geom_line(color = "#2B8CBE", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.2, fill = "#2B8CBE") +
  labs(
    title = "铁浓度与良胚率的非线性关系",
    x = "标准化铁浓度 (Fe)", 
    y = "Log Odds"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")
