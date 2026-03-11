此文件夹包含按照 Goldsmith & Komarova (2025) 协议设计的元数据模板，并针对临床试验 Meta 分析进行了优化。

每个子文件夹以文章标识符（如 PMID、DOI 或自定义简称）命名，存放该文章独立的元数据。

每个文章子文件夹内包含以下文件：

【研究特征相关】
01_study_info.csv：研究级别基本信息（每个研究一行）
   - pmid                    : PubMed ID（唯一标识）
   - study_name              : 研究名称（如 'CheckMate816'）
   - year                    : 发表年份（由 PMID 自动填充）
   - first_author            : 第一作者（由 PMID 自动填充）
   - trial_design            : 试验设计（如 'Double blind, randomized phase III'）
   - total_patients          : 研究总人数
   - drug                    : 使用的免疫检查点抑制剂
   - notes                   : 备注（包含完整作者、标题、期刊，由 PMID 自动填充）

01_study_features.csv：研究特征键值对表（灵活存储特定领域的研究特征）
   - pmid                    : PubMed ID
   - feature_name            : 特征名称
   - feature_value           : 特征值
   - feature_unit            : 单位
   - notes                   : 备注

   当前项目定义的 feature（来自 config/study_features.csv）：
   - age_median                    : 中位年龄 (单位: years, 示例: 65, 64, 63)
   - stage_distribution            : 分期分布 (单位: 无, 示例: Stage 2 (30%); Stage 3A (45%); Stage 3B (25%))
   - pd_l1_positivity_rate         : PD-L1阳性率 (单位: %, 示例: 67)
   - squamous_histology_rate       : 鳞癌比例 (单位: %, 示例: 49)
   - median_followup               : 中位随访时间 (单位: months, 示例: 11.7, 25.2, 41.4)
   - neoadjuvant_cycles            : 新辅助治疗周期数 (单位: cycles, 示例: 3, 4)
   - adjuvant_cycles               : 辅助治疗周期数 (单位: cycles, 示例: 0, 6, 12, 13)
   - treatment_setting             : 治疗设置 (单位: 无, 示例: Perioperative, Neoadjuvant, Adjuvant)

01_arms.csv：各 arm 详细信息（每个 arm 一行）
   - pmid                    : PubMed ID（关联到 study_info）
   - study_name              : 研究名称
   - arm                     : 组别编号（1, 2, 3...）
   - arm_description         : 组别描述（如 'Intervention', 'Control'）
   - sample_size             : 该组样本量
   - treatment_regimen       : 治疗方案详情（如 'Nivolumab (360 mg) + platinum-doublet'）
   - cycles                  : 治疗周期数
   - notes                   : 备注

【重叠检查】
02_overlap_check.csv：用于标记可能重叠的研究，避免重复数据。
   - pmid1, pmid2            : 可能存在重叠的两篇文章的 PMID
   - overlap_reason          : 重叠原因
   - decision                : 处理决定

【结局数据】
03_survival_hr.csv：生存分析的风险比（HR）及其 95% 置信区间
   - pmid                    : PubMed ID
   - study_name              : 研究名称
   - parameter_name          : 参数名称（如 'EFS', 'OS'）
   - subgroup                : 亚组信息
   - measure                 : 效应量类型（'HR'）
   - effect_size             : 风险比点估计
   - ci_lower/ci_upper       : 95% 置信区间
   - p_value                 : P 值

04_binary_counts.csv：二分类结局数据（如 pCR、毒性事件）
   - pmid                    : PubMed ID
   - study_name              : 研究名称
   - parameter_name          : 参数名称（如 'pCR'）
   - subgroup                : 亚组信息
   - arm                     : 组别编号（对应 01_arms.csv 中的 arm）
   - events                  : 事件数
   - total                   : 该组总人数

05_continuous_means.csv：连续型结局数据（如骨密度变化）
   - pmid                    : PubMed ID
   - study_name              : 研究名称
   - parameter_name          : 参数名称
   - subgroup                : 亚组信息
   - arm                     : 组别编号
   - n                       : 样本量
   - mean                    : 均值
   - sd                      : 标准差

使用说明：
1. 首先填写 01_study_info.csv，记录研究的基本信息
2. 然后在 01_study_features.csv 中添加该研究特有的特征
3. 在 01_arms.csv 中为每个 arm 添加一行，填写各组的具体信息
4. 在 04_binary_counts.csv 和 05_continuous_means.csv 中记录各 arm 的结局数据
5. 在 03_survival_hr.csv 中记录生存分析的 HR 结果

【管理 feature 列表】
feature 列表存储在项目根目录的 config/study_features.csv 中。
您可以随时编辑此文件来：
   - 添加新的 feature
   - 删除不需要的 feature
   - 修改 feature 的描述、单位或示例值
   - 调整显示顺序
修改后，新创建的研究将使用更新后的 feature 列表。
已有研究的 feature 文件不会自动更新，如需更新请设置 overwrite = TRUE。

当同一研究有多篇文章时，请为每篇文章创建独立的子文件夹并分别填写数据。
