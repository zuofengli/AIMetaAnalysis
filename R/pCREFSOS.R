# 安装（如果尚未安装）
install.packages(c("ggplot2", "meta", "metafor", "dplyr", "ggrepel"))

# 加载
library(ggplot2)
library(meta)
library(metafor)
library(dplyr)
library(ggrepel)

# 创建数据框（包含各试验的pCR率、EFS HR、OS HR）
# 注意：pCR率输入的是治疗组和对照组的百分比（0~100）
data <- data.frame(
  trial = c("CheckMate 816", "KEYNOTE-671", "RATIONALE-315", "NEOTORCH", "NADIM-II"),
  # pCR率（%）
  pcr_treat = c(24.0, 18.1, 40.7, 24.8, 36.8),
  pcr_ctrl  = c(2.2,  4.0,  5.7,  1.0,  6.9),
  # EFS HR 和 95% CI
  efs_hr    = c(0.63, 0.59, 0.57, 0.40, 0.47),
  efs_lci   = c(0.45, 0.48, 0.40, 0.28, 0.25),
  efs_uci   = c(0.87, 0.72, 0.79, 0.57, 0.88),
  # OS HR 和 95% CI
  os_hr     = c(0.71, 0.72, 0.62, 0.62, 0.43),
  os_lci    = c(0.51, 0.56, 0.39, 0.38, 0.19),
  os_uci    = c(0.98, 0.93, 0.98, 1.00, 0.98)
)

# 计算pCR的风险比（RR）及其对数、标准误
# 使用meta包的metabin可以自动计算，或者手动计算
# 这里手动计算logRR和se(logRR)（基于四格表，假设各组样本量已知）
# 如果不知道样本量，可用近似公式，但这里我们手动输入各组的样本量（根据您的数据）
# 假设样本量如下（来自您之前的数据）：
n_treat <- c(179, 397, 226, 202, 57)   # 治疗组人数
n_ctrl  <- c(179, 400, 227, 202, 29)    # 对照组人数

# 计算事件数
events_treat <- data$pcr_treat / 100 * n_treat
events_ctrl  <- data$pcr_ctrl / 100 * n_ctrl

# 计算logRR 和 se(logRR)
# 使用meta包中的metabin（更稳健）
library(meta)
m <- metabin(event.e = events_treat, n.e = n_treat,
             event.c = events_ctrl, n.c = n_ctrl,
             sm = "RR", method = "Inverse")
data$logRR <- log(m$TE)   # 对数RR
data$se_logRR <- m$seTE   # 标准误

# 为了后续加权回归，计算EFS和OS的logHR及标准误
data$logHR_efs <- log(data$efs_hr)
data$se_logHR_efs <- (log(data$efs_uci) - log(data$efs_lci)) / (2 * 1.96)  # 近似

data$logHR_os <- log(data$os_hr)
data$se_logHR_os <- (log(data$os_uci) - log(data$os_lci)) / (2 * 1.96)

# 查看数据
print(data)


# 使用metafor进行加权线性回归（权重为EFS的方差倒数）
res_efs <- rma(yi = logHR_efs, sei = se_logHR_efs,
               mods = ~ logRR, data = data, method = "REML")
summary(res_efs)

# 提取回归系数和R²
coef_efs <- coef(res_efs)
r2_efs <- res_efs$R2

# 绘制散点图 + 回归线
p1 <- ggplot(data, aes(x = logRR, y = logHR_efs)) +
  geom_point(aes(size = 1/se_logHR_efs^2), color = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = logHR_efs - 1.96*se_logHR_efs,
                    ymax = logHR_efs + 1.96*se_logHR_efs), width = 0.05) +
  geom_errorbarh(aes(xmin = logRR - 1.96*se_logRR,
                     xmax = logRR + 1.96*se_logRR), height = 0.05) +
  geom_abline(intercept = coef_efs[1], slope = coef_efs[2], color = "red", size = 1) +
  geom_text_repel(aes(label = trial), size = 4) +
  labs(title = "pCR获益 vs EFS获益",
       subtitle = paste0("加权回归 R² = ", round(r2_efs, 3),
                         ", p = ", round(res_efs$pval[2], 4)),
       x = "log(RR) for pCR", y = "log(HR) for EFS") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

res_os <- rma(yi = logHR_os, sei = se_logHR_os,
              mods = ~ logRR, data = data, method = "REML")
summary(res_os)

coef_os <- coef(res_os)
r2_os <- res_os$R2

p2 <- ggplot(data, aes(x = logRR, y = logHR_os)) +
  geom_point(aes(size = 1/se_logHR_os^2), color = "darkgreen", alpha = 0.7) +
  geom_errorbar(aes(ymin = logHR_os - 1.96*se_logHR_os,
                    ymax = logHR_os + 1.96*se_logHR_os), width = 0.05) +
  geom_errorbarh(aes(xmin = logRR - 1.96*se_logRR,
                     xmax = logRR + 1.96*se_logRR), height = 0.05) +
  geom_abline(intercept = coef_os[1], slope = coef_os[2], color = "red", size = 1) +
  geom_text_repel(aes(label = trial), size = 4) +
  labs(title = "pCR获益 vs OS获益",
       subtitle = paste0("加权回归 R² = ", round(r2_os, 3),
                         ", p = ", round(res_os$pval[2], 4)),
       x = "log(RR) for pCR", y = "log(HR) for OS") +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)


# 同样使用metafor，但这里以EFS的logHR为自变量，OS的logHR为因变量
# 注意：权重应为OS方差的倒数，但回归中也可使用双变量模型，这里简化为加权回归
res_efs_os <- rma(yi = logHR_os, sei = se_logHR_os,
                  mods = ~ logHR_efs, data = data, method = "REML")
summary(res_efs_os)

coef_efs_os <- coef(res_efs_os)
r2_efs_os <- res_efs_os$R2

p3 <- ggplot(data, aes(x = logHR_efs, y = logHR_os)) +
  geom_point(aes(size = 1/se_logHR_os^2), color = "purple", alpha = 0.7) +
  geom_errorbar(aes(ymin = logHR_os - 1.96*se_logHR_os,
                    ymax = logHR_os + 1.96*se_logHR_os), width = 0.02) +
  geom_errorbarh(aes(xmin = logHR_efs - 1.96*se_logHR_efs,
                     xmax = logHR_efs + 1.96*se_logHR_efs), height = 0.02) +
  geom_abline(intercept = coef_efs_os[1], slope = coef_efs_os[2], color = "red", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # 参考线 y=x
  geom_text_repel(aes(label = trial), size = 4) +
  labs(title = "EFS获益 vs OS获益",
       subtitle = paste0("加权回归 R² = ", round(r2_efs_os, 3),
                         ", p = ", round(res_efs_os$pval[2], 4)),
       x = "log(HR) for EFS", y = "log(HR) for OS") +
  theme_minimal() +
  theme(legend.position = "none")

print(p3)

# 安装并加载patchwork
install.packages("patchwork")
library(patchwork)

(p1 | p2) / p3   # 三图排列

# 打印详细回归结果
cat("=== pCR -> EFS 回归 ===\n")
print(summary(res_efs))
cat("\n=== pCR -> OS 回归 ===\n")
print(summary(res_os))
cat("\n=== EFS -> OS 回归 ===\n")
print(summary(res_efs_os))

