# =============================
# 生存数据的网络 Meta 分析 (基于 gemtc)
# =============================

# 1. 安装与加载包（首次需安装）
# install.packages("gemtc")
# install.packages("igraph")
library(gemtc)
library(igraph)

# =============================
# 2. 读入数据（此处为示例，你需要替换为实际文件路径）
# =============================
# 假设已将你给出的数据保存为 CSV 文件 "survival_data.csv"
raw <- read.csv("survival_data.csv", stringsAsFactors = FALSE)

# 查看数据前几行
head(raw)

# =============================
# 3. 数据清洗与筛选
# =============================
# 去除 'removed' 列标记为移除的行（如果存在）
dat <- raw[raw$removed %in% c("NO", ""), ]

# 删除分层数据（按需选择“Overall”且未分层的行，或自行决定）
dat <- dat[dat$subgroup == "Overall" & dat$stratified %in% c("No", ""), ]

# 去掉重复的研究（同一 trial 只保留一条记录，可按最新 pub_year 或最完整信息选择）
dat <- dat[!duplicated(dat$trial), ]
dat
# 确保 te 和 se_te 已提供（这是对数风险比及其标准误）
# 如果缺失 se_te，可从 hr, hr_lci, hr_uci 计算：
if(any(is.na(dat$se_te))) {
  # 近似法：se_te = (log(hr_uci) - log(hr_lci)) / (2 * 1.96)
  dat$se_te <- (log(dat$hr_uci) - log(dat$hr_lci)) / (2 * qnorm(0.975))
}
if(any(is.na(dat$te))) {
  dat$te <- log(dat$hr)
}

# =============================
# 4. 构建 mtc.network 所需的 data.re 格式
# =============================
# data.re 格式要求：每一行是一个研究的一个臂，包含：
#   study（字符型，研究ID）
#   treatment（字符型，治疗ID）
#   diff（数值，相对于基线臂的效应差值，基线臂为 NA）
#   std.err（数值，差值的标准误，基线臂为 NA，但多臂研究需设置基线臂的 std.err）

# 当前数据为两臂研究，每行代表一个比较 treat1 vs treat2
# 将每个比较重塑为两行：实验臂和基线臂
study_list <- list()
for (i in 1:nrow(dat)) {
  study_id <- dat$trial[i]
  t1 <- dat$treat1[i]
  t2 <- dat$treat2[i]
  te_val <- as.numeric(dat$te[i])       # 确保数值
  se_val <- as.numeric(dat$se_te[i])    # 确保数值

  baseline <- data.frame(
    study = study_id,
    treatment = t2,
    diff = NA_real_,
    std.err = NA_real_,
    stringsAsFactors = FALSE
  )
  experimental <- data.frame(
    study = study_id,
    treatment = t1,
    diff = te_val,
    std.err = se_val,
    stringsAsFactors = FALSE
  )
  study_list[[i]] <- rbind(baseline, experimental)
}

data_re <- do.call(rbind, study_list)
# 再次强制确保数值型（应对可能的意外）
data_re$diff <- as.numeric(data_re$diff)
data_re$std.err <- as.numeric(data_re$std.err)
rownames(data_re) <- NULL

# （可选）检查是否有研究包含三个或以上臂，若有，需为基线臂提供 std.err
# 这里假设所有研究为两臂，可直接继续

# =============================
# 5. 创建网络对象
# =============================
network <- mtc.network(data.re = data_re, description = "Perioperative immunotherapy EFS network")

# 查看网络概况
summary(network)

# 绘制证据网络图
plot(network, use.description = TRUE)

# =============================
# 6. 建立一致性模型（正态似然，identity 链接 – 适用于对数 HR）
# =============================
model <- mtc.model(
  network,
  type = "consistency",   # 一致性模型
  likelihood = "normal",  # 连续型数据（对数风险比）
  link = "identity",      # 恒等链接，表示效应已在 log 尺度
  linearModel = "random"  # 随机效应模型
)

# 查看模型结构
model

# =============================
# 7. 运行 MCMC
# =============================
# 可根据数据复杂度调整迭代次数
mtc.run <- mtc.run(
  model,
  n.adapt = 5000,    # 预热/自适应迭代
  n.iter = 50000,    # 总迭代次数
  thin = 10          # 稀释因子
)

# =============================
# 8. 收敛诊断与结果
# =============================
# 收敛检查
gelman.plot(mtc.run)

# 拟合摘要
summary(mtc.run)

# 森林图（相对于某一特定治疗）
# 例如以 "Chemotherapy_ctrl" 为参照
forest(relative.effect(mtc.run, t1 = "Chemotherapy_ctrl"))

# 排名概率
ranks <- rank.probability(mtc.run, preferredDirection = -1) # -1 代表 HR < 1 更好（生存获益）
print(ranks)

# 排名图
plot(ranks)

# =============================
# 9. (可选) 节点拆分法检测不一致性
# =============================
# nodesplit <- mtc.nodesplit(network, likelihood = "normal", link = "identity",
#                            linearModel = "random", n.adapt = 5000, n.iter = 30000, thin = 10)
# summary(nodesplit)
# plot(nodesplit)

