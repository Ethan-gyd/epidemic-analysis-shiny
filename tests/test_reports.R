# 使用合成记录检验统计口径，不读取真实病例文件。
# 运行：Rscript tests/test_reports.R
source("report_generation.R", encoding = "UTF-8")

checks <- 0L
expect <- function(condition, label) {
  if (!isTRUE(condition)) stop("FAIL: ", label, call. = FALSE)
  checks <<- checks + 1L
  cat("PASS:", label, "\n")
}
expect_error <- function(expr, label) {
  error <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  expect(error, label)
}
date <- as.Date

expect(identical(report_period("monthly", date("2024-02-14")), date(c("2024-02-01", "2024-02-29"))), "月报覆盖闰年二月")
expect(identical(report_period("weekly", date("2026-01-01")), date(c("2025-12-29", "2026-01-04"))), "跨年周从周一开始")
expect_error(report_period("special", date_range = c("2026-09-30", "2026-09-01")), "拒绝倒置日期范围")
expect_error(report_period("monthly", "2026-02-30"), "拒绝不存在的日期")
expect_error(report_period("monthly", 20100), "拒绝无日期基准的数字")

sample <- data.frame(
  发病日期 = date(c("2025-09-01", "2026-08-01", "2026-08-31", "2026-09-01", "2026-09-30", "2026-09-30", "2026-10-01")),
  诊断时间 = as.POSIXct(c("2025-09-02", "2026-08-02", "2026-09-01", NA, "2026-10-01", "2026-09-30", "2026-10-02"), tz = "Asia/Shanghai"),
  标准病种 = c("病种甲", "病种甲", "病种甲", "病种甲", "病种乙", NA, "病种乙"),
  区县 = c("甲区", "甲区", "甲区", "甲区", "乙区", NA, "乙区"),
  性别标准 = c("男", "男", "女", "男", "女", NA, "女"),
  年龄组 = "20~岁", 人群分类 = "其他",
  姓名 = "严禁泄露姓名", 卡片编号 = "PRIVATE-CARD-123", 现住详细地址 = "私人详细地址",
  check.names = FALSE
)
period <- date(c("2026-09-01", "2026-09-30"))
payload <- build_report_payload(sample, period)
expect(payload$total_cases == 3L, "统计起止日期均包含")
expect(nrow(payload$daily) == 30L && sum(payload$daily$病例数) == 3L, "完整日期序列补零且总数守恒")
expect(sum(payload$disease_table$病例数) == 3L && "未填写" %in% payload$disease_table$病种, "缺失病种保留在构成比分母")
expect(payload$disease_count == 2L && payload$district_count == 2L, "病种和地区计数不把缺失当作已知类别")
expect(all(is.na(payload$comparison$比较期病例数)) && all(is.na(payload$comparison$变化百分比)), "观察到历史病例不能代替完整覆盖确认")
expect(is.na(payload$outbreak$count) && !payload$outbreak$evaluated, "未执行探测不显示零预警")
expect(!any(grepl("严禁泄露|PRIVATE-CARD|私人详细地址", unlist(payload))), "报告负载不保留个人卡片信息")

complete <- build_report_payload(sample, period, coverage_range = date(c("2025-01-01", "2026-10-08")), data_cutoff = date("2026-10-08"))
expect(identical(complete$comparison$比较期病例数, c(2L, 1L)), "完整覆盖时环比和同比计数正确")
expect(identical(complete$comparison$变化百分比, c(50, 200)), "百分比以比较期为分母")
partial <- build_report_payload(sample, period, coverage_range = date(c("2025-01-01", "2026-09-15")))
expect(all(is.na(partial$comparison$变化百分比)), "本期只覆盖半月时不生成可比百分比")
expect_error(build_report_payload(sample, period, coverage_range = date(c("2025-01-01", "2026-10-31")), data_cutoff = date("2026-10-08")), "覆盖期不能超过已知提取截止")

diagnosis <- build_report_payload(sample, period, time_col = "诊断时间")
expect(diagnosis$total_cases == 2L && diagnosis$invalid_date_count == 1L, "诊断日期缺失不回退为发病日期")
expect_error(build_report_payload(sample, period, time_col = "不存在字段"), "缺失统计字段直接报错")
expect_error(build_report_payload(sample, date(c("2026-09-02", "2026-09-30"))), "不完整自然月应选择专题")
expect_error(build_report_payload(sample, date(c("2026-09-01", "2026-09-07")), report_type = "weekly"), "拒绝错位的自然周")

empty <- build_report_payload(sample, period, diseases = character())
expect(empty$total_cases == 0L && nrow(empty$disease_table) == 0L && all(empty$daily$病例数 == 0), "明确空筛选可正常生成零记录汇总")
expect(grepl("不能据此认定实际零发病", empty$summary_text), "覆盖未知的零记录报告说明局限")
zero_base <- build_report_payload(sample, period, diseases = "病种乙", coverage_range = date(c("2025-01-01", "2026-10-08")))
expect(all(zero_base$comparison$比较期病例数 == 0L) && all(is.na(zero_base$comparison$变化百分比)), "确认零基数也不产生无穷增长率")

invalid <- sample[1:2, , drop = FALSE]
invalid$发病日期 <- c("bad", "2026-02-30")
invalid_payload <- build_report_payload(invalid, period)
expect(invalid_payload$total_cases == 0L && invalid_payload$invalid_date_count == 2L, "全部日期无效时仍可给出质量说明")
expect(all(is.na(invalid_payload$observed_date_range)), "无有效日期不输出 Inf 截止日期")

leap <- build_report_payload(sample, date(c("2024-02-01", "2024-02-29")))
expect(identical(leap$comparison$结束日期[2], date("2023-02-28")), "月度同比正确处理二月闰年")
week <- build_report_payload(sample, date(c("2025-12-29", "2026-01-04")), report_type = "weekly")
expect(identical(week$comparison$起始日期[2], date("2024-12-30")), "周报同比对齐前52周相同星期")
special <- build_report_payload(sample, date(c("2026-09-28", "2026-10-01")), report_type = "special", diseases = "病种乙")
expect(special$total_cases == 2L && special$comparison$起始日期[1] == date("2026-09-24"), "专题病种筛选和前等长区间正确")

alerts <- data.frame(疾病名称 = c("病种甲", "病种甲", "病种乙"), 场所类型 = c("工作单位", "工作单位", "现住地址"), 聚集病例数 = c(3L, 5L, 4L),
                     core_institution = "保密机构", 机构预警ID = "保密卡片关联ID", check.names = FALSE)
signal <- build_report_payload(sample, period, outbreak_alerts = alerts)
expect(signal$outbreak$count == 3L && nrow(signal$outbreak$table) == 2L, "预警按病种及场所类型聚合")
expect(signal$include_place_names && all(signal$outbreak_details$机构名称 == "保密机构"), "默认导出具体机构名称")
expect(!any(grepl("保密卡片|PRIVATE-|严禁泄露|私人详细地址", unlist(signal))), "机构实名仍不导出关联标识或个人信息")
anonymous_signal <- build_report_payload(sample, period, outbreak_alerts = alerts, include_place_names = FALSE)
expect(!anonymous_signal$include_place_names && !any(grepl("保密机构|保密卡片", unlist(anonymous_signal))), "显式匿名仍隐藏机构和关联标识")
none <- build_report_payload(sample, period, outbreak_alerts = alerts[0, ])
expect(none$outbreak$evaluated && none$outbreak$count == 0L, "运行后零预警和未运行明确区分")
attr(sample, "data_quality") <- data.frame(项目 = c("原始记录", "完全重复"), 记录数 = c(8L, 1L), 姓名 = "不得拷贝", check.names = FALSE)
quality <- build_report_payload(sample, period)
expect(identical(names(quality$import_quality), c("项目", "记录数")), "只接受质量汇总白名单列")

# 可选格式冒烟测试；生成到临时目录并清理，不与真实文件混用。
if (identical(Sys.getenv("REPORT_TEST_EXPORT"), "1")) {
  destination <- tempfile("report-tests-")
  dir.create(destination)
  word <- file.path(destination, "zero.docx")
  write_surveillance_report(empty, word, "docx")
  expect(file.exists(word) && file.info(word)$size > 1000, "零病例 Word 可导出")
  entries <- utils::unzip(word, list = TRUE)$Name
  expect("word/document.xml" %in% entries, "Word 为有效 OOXML 压缩包")
  # Validate actual exported OOXML, including the named micro-alert table, not only payload strings.
  export_alerts <- data.frame(
    机构预警ID = c("PRIVATE-CLASS-ID", "PRIVATE-SCATTER-ID"),
    core_institution = c("测试晨星小学", "测试青禾幼儿园"),
    疾病名称 = c("病种甲", "病种乙"), 场所类型 = "工作单位", 涉及区县 = c("甲区", "乙区"),
    聚集病例数 = c(5L, 3L), 预警窗口起 = date("2026-09-01"), 预警窗口止 = date("2026-09-14"),
    最早发病日期 = date("2026-09-01"), 最晚发病日期 = date("2026-09-03"),
    时间跨度天数 = 3L, 子部门数 = 2L,
    子部门分布 = c("三年级一班(3)；三年级二班(2)", "中一班(2)；大二班(1)"),
    姓名 = "严禁泄露姓名", 卡片编号 = "PRIVATE-CARD-123", 现住详细地址 = "私人详细地址", check.names = FALSE)
  export_micro <- export_alerts
  export_micro$sub_level <- c("三年级一班", "机构内散发")
  export_micro$预警类型 <- c("班级/科室聚集", "机构级聚集，子部门未达阈值")
  export_micro$聚集病例数 <- c(3L, 3L)
  export_micro$子部门分布 <- c("三年级一班(3)", "中一班(2)；大二班(1)")
  named_export <- build_report_payload(sample, period, outbreak_alerts = export_alerts, outbreak_micro_alerts = export_micro)
  named_word <- file.path(destination, "named.docx")
  write_surveillance_report(named_export, named_word, "docx")
  read_docx_document <- function(path, label) {
    target <- file.path(destination, label)
    utils::unzip(path, files = "word/document.xml", exdir = target)
    xml2::read_xml(file.path(target, "word/document.xml"))
  }
  named_xml <- read_docx_document(named_word, "named-xml")
  ns <- xml2::xml_ns(named_xml)
  named_text <- paste(xml2::xml_text(xml2::xml_find_all(named_xml, "//w:t", ns)), collapse = "")
  expect(all(vapply(c("测试晨星小学", "测试青禾幼儿园", "三年级一班", "三年级二班", "中一班", "大二班"),
    function(x) grepl(x, named_text, fixed = TRUE), logical(1))), "默认Word正文确实含机构和全部具体子部门名称")
  rows <- lapply(xml2::xml_find_all(named_xml, "//w:tr", ns), function(row) {
    vapply(xml2::xml_find_all(row, "./w:tc", ns), function(cell) {
      paste(xml2::xml_text(xml2::xml_find_all(cell, ".//w:t", ns)), collapse = "")
    }, character(1))
  })
  expect(any(vapply(rows, function(row) all(c("机构名称", "子部门名称", "病例数", "子部门分布") %in% row), logical(1))),
    "Word子部门明细表明确列出机构名称与具体子部门列")
  expect(any(vapply(rows, function(row) all(c("测试晨星小学", "三年级一班", "3", "三年级一班(3)") %in% row), logical(1))),
    "Word聚集明细同一行含所属机构、班级名称和病例数")
  expect(any(vapply(rows, function(row) all(c("测试青禾幼儿园", "中一班、大二班", "3", "中一班(2)；大二班(1)") %in% row), logical(1))),
    "Word散发明细同一行含具体机构、全部部门及2加1计数")
  expect(!grepl("PRIVATE-|严禁泄露姓名|私人详细地址", named_text), "实际实名Word不含个人姓名卡片地址或内部关联ID")
  anonymous_export <- build_report_payload(sample, period, outbreak_alerts = export_alerts,
    outbreak_micro_alerts = export_micro, include_place_names = FALSE)
  anonymous_word <- file.path(destination, "anonymous.docx")
  write_surveillance_report(anonymous_export, anonymous_word, "docx")
  anonymous_xml <- read_docx_document(anonymous_word, "anonymous-xml")
  anonymous_text <- paste(xml2::xml_text(xml2::xml_find_all(anonymous_xml, "//w:t", xml2::xml_ns(anonymous_xml))), collapse = "")
  expect(!grepl("测试晨星小学|测试青禾幼儿园|三年级一班|三年级二班|中一班|大二班|PRIVATE-|严禁泄露姓名|私人详细地址", anonymous_text),
    "显式匿名Word实际文档不含机构部门名称或个人信息")
  expect(grepl("部门01(2)；部门02(1)", anonymous_text, fixed = TRUE), "匿名Word仍保留散发部门真实计数")
  if (report_export_capabilities()$pdf) {
    pdf <- file.path(destination, "zero.pdf")
    write_surveillance_report(empty, pdf, "pdf")
    handle <- file(pdf, "rb")
    signature <- rawToChar(readBin(handle, "raw", n = 5L)); close(handle)
    expect(identical(signature, "%PDF-"), "零病例 PDF 可导出且文件头正确")
  }
  unlink(destination, recursive = TRUE)
}
cat("\nAll", checks, "report checks passed.\n")
