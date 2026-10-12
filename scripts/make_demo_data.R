# 完全合成的数据：不读取任何病例文件，机构/住址均为虚构演示文本。
# 从项目根目录执行 Rscript scripts/make_demo_data.R [输出路径]
args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args)) args[[1]] else "data/demo_surveillance.csv"
set.seed(20261008)

make_rows <- function(n, start, days, prefix) {
  onset <- as.Date(start) + sample.int(days, n, replace = TRUE) - 1L
  district <- sample(c("五华区", "盘龙区", "官渡区", "西山区"), n, replace = TRUE)
  pac <- c("五华区" = "530102", "盘龙区" = "530103", "官渡区" = "530111", "西山区" = "530112")
  age <- sample(1:80, n, replace = TRUE)
  d <- data.frame(
    卡片ID = sprintf("SYNTH-%s-%04d", prefix, seq_len(n)),
    卡片编号 = sprintf("DEMO-%s-%04d", prefix, seq_len(n)),
    卡片状态 = "有效卡", 审核状态 = "已终审",
    疾病名称 = sample(c("流行性感冒", "手足口病", "水痘", "其他感染性腹泻病", "肺结核"), n, TRUE, c(.4, .25, .15, .15, .05)),
    病例分类 = "临床诊断病例", 发病日期 = as.character(onset),
    诊断时间 = paste(onset + 1, "09:00:00"),
    订正终审时间 = paste(onset + 2, "11:00:00"),
    报告卡录入时间 = paste(onset + 1, "10:00:00"),
    出生日期 = as.character(onset - round(age * 365.25)),
    性别 = sample(c("男", "女"), n, TRUE), 年龄 = paste0(age, "岁"),
    人群分类 = ifelse(age < 7, "散居儿童", ifelse(age < 20, "学生", "其他")),
    现住详细地址 = paste0("云南省昆明市", district, "演示路虚构住宅", seq_len(n), "号"),
    现住地址国标 = paste0("156", unname(pac[district])),
    患者工作单位 = "无", check.names = FALSE, stringsAsFactors = FALSE
  )
  d[["（删除/标注）时间"]] <- ""
  d[["（删除/未纳入统计）原因"]] <- ""
  d[["（删除/标注）用户"]] <- ""
  d[["（删除/标注）用户所属单位"]] <- ""
  d
}

background <- rbind(
  make_rows(60, "2025-09-01", 30, "Y2025"),
  make_rows(80, "2026-08-01", 31, "M08"),
  make_rows(140, "2026-09-01", 30, "M09"),
  make_rows(35, "2026-10-01", 8, "M10")
)

cluster <- make_rows(16, "2026-10-01", 7, "CLUSTER")
cluster$发病日期 <- as.character(as.Date("2026-10-01") + c(0, 0, 1, 2, 3, 4, 1, 2, 3, 4, 4, 5, 0, 1, 2, 3))
cluster$疾病名称 <- c(rep("流行性感冒", 6), rep("手足口病", 6), rep("水痘", 4))
cluster$患者工作单位 <- c(
  "演示晨光小学三年级二班", "演示晨光小学3年级2班", "演示晨光小学（三年级二班）",
  "演示晨光小学三年级一班", "演示晨光小学3年级1班", "演示晨光小学三年级三班",
  "演示星河幼儿圆大班", "演示星河幼儿园大班", "演示星河幼儿苑大班",
  "演示星河幼儿园中班", "演示星河幼儿园中班", "演示星河幼儿园小班",
  "演示云杉中学七年级一班", "演示云杉中学七年级二班", "演示云杉中学七年级三班", "演示云杉中学七年级四班"
)
cluster$现住地址国标 <- "156530102"
cluster$现住详细地址 <- paste0("云南省昆明市五华区虚构演示住宅", seq_len(nrow(cluster)), "号")
cluster$年龄 <- c(rep("9岁", 6), rep("4岁", 6), rep("13岁", 4))
cluster$出生日期 <- as.character(as.Date(cluster$发病日期) - round(as.numeric(sub("岁", "", cluster$年龄)) * 365.25))
cluster$人群分类 <- c(rep("学生", 6), rep("幼托儿童", 6), rep("学生", 4))
cluster$诊断时间 <- paste(as.Date(cluster$发病日期) + 1, "09:00:00")
cluster$报告卡录入时间 <- paste(as.Date(cluster$发病日期) + 1, "10:00:00")
cluster$订正终审时间 <- paste(as.Date(cluster$发病日期) + 2, "11:00:00")

# 故意加入清洗反例：完全重复1条、旧版本1条、删除卡1条、无效日期1条。
latest_deleted <- background[1, ]
latest_deleted$卡片状态 <- "删除卡"
latest_deleted$订正终审时间 <- "2026-10-08 18:00:00"
latest_deleted[["（删除/标注）时间"]] <- "2026-10-08 18:00:00"
latest_deleted[["（删除/未纳入统计）原因"]] <- "合成数据：演示订正后删除"
invalid_date <- make_rows(1, "2026-10-01", 1, "BADDATE")
invalid_date$发病日期 <- "无效日期（演示）"

demo <- rbind(background, cluster, cluster[1, ], latest_deleted, invalid_date)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
con <- file(output_path, open = "wt", encoding = "UTF-8")
write.csv(demo, con, row.names = FALSE, na = "")
close(con)
cat(sprintf("写入 %d 条完全合成记录：%s\n", nrow(demo), output_path))
cat("预期清洗后330条；2026-10-01至10-08 / 阈值3 / 窗口14天：3条机构预警，2条班级预警，1条机构内散发提示。\n")
