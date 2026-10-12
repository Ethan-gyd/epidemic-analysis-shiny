# Run from the project root: Rscript tests/test_detection.R
source("data_preprocessing.R", encoding = "UTF-8")
source("outbreak_detection.R", encoding = "UTF-8")
assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
assert_error <- function(expr) assert(inherits(tryCatch({force(expr); NULL}, error = identity), "error"), "Expected a validation error")
assert(identical(as.character(parse_cn_date(c("2026-03-01", "2026/03/01 00:00:00"))), c("2026-03-01", "2026-03-01")), "Shanghai midnight must keep its calendar date")
assert(identical(as.character(make_age_group(c(4.9, 5, 84.9, 85))), c("0-4岁", "5-9岁", "80-84岁", "85岁及以上")), "Age bands must include all ages below their upper bound")
raw <- tibble::tibble(
  卡片ID = c("A", "A", "B", "B", NA, NA, "D", "E"),
  疾病名称 = rep("流行性感冒", 8), 发病日期 = c(rep("2026-10-01", 7), "bad"),
  订正终审时间 = c("2026-10-02", "2026-10-03", "2026-10-02", "2026-10-02", "2026-10-02", "2026-10-03", "2026-10-04", "2026-10-04"),
  卡片状态 = c("有效", "删除", rep("有效", 6)),
  患者工作单位 = c("演示曙光小学一年级一班", "演示曙光小学一年级一班", "演示曙光小学一年级一班", "演示曙光小学一年级一班", "演示曙光小学二年级一班", "演示曙光小学二年级二班", "演示曙光小学学生宿舍", "演示曙光小学学生宿舍"),
  现住详细地址 = rep("云南省昆明市五华区演示虚构住址", 8), 现住地址国标 = rep("530102", 8)
)
path <- tempfile(fileext = ".csv")
readr::write_excel_csv(raw, path)
x <- load_surveillance_data(path)
q <- attr(x, "data_quality")
assert(nrow(x) == 4L, "Latest deleted version, exact duplicates and invalid dates must be removed")
assert(!"A" %in% x$卡片ID && sum(is.na(x$卡片ID)) == 2, "Missing IDs must not collapse unrelated cards")
assert(q$记录数[q$项目 == "原始记录"] == sum(q$记录数[q$项目 != "原始记录"]), "Cleaning stages must reconcile")
# Encoding parity (uses only generated fixture rows).
legacy <- tempfile(fileext = ".csv")
writeLines(iconv(readLines(path, encoding = "UTF-8"), "UTF-8", "GB18030"), legacy, useBytes = TRUE)
x2 <- load_surveillance_data(legacy)
assert(nrow(x2) == nrow(x) && identical(x2$疾病名称, x$疾病名称), "UTF-8 and GB18030 must produce the same records")
# Same institution core, subdepartments preserved, inclusive 14-day bound.
y <- x[rep(1L, 3L), ]; y$卡片ID <- paste0("T", 1:3)
y$发病日期 <- as.Date(c("2026-10-01", "2026-10-05", "2026-10-14"))
a <- detect_outbreak_clusters(y, as.Date("2026-10-31"), start_date = as.Date("2026-10-01"))
assert(nrow(a) == 1L && a$聚集病例数 == 3 && a$时间跨度天数 == 14, "Day 1 through day 14 is one valid window")
y$发病日期[3] <- as.Date("2026-10-15")
assert(nrow(detect_outbreak_clusters(y, as.Date("2026-10-31"), start_date = as.Date("2026-10-01"))) == 0L, "A 15-day span must not pass a 14-day rule")
y$发病日期[] <- as.Date("2026-10-01"); y$现住地址国标[3] <- "130102"
assert(nrow(detect_outbreak_clusters(y, as.Date("2026-10-14"), start_date = as.Date("2026-10-01"))) == 0L, "Different geographic groups must not merge into an alert")
places <- split_outbreak_place_text(c("演示曙光小学一年级一班", "演示曙光小学学生宿舍", "小学", "演示诊所内科"))
assert(places$core_institution[1] == places$core_institution[2] && places$sub_level[1] == "1年级1班", "Institution and department extraction must be stable")
assert(!places$核心机构有效[3] && places$核心机构有效[4], "Generic names are rejected; named clinics are supported")
assert_error(detect_outbreak_clusters(x, as.Date("2026-10-14"), threshold = 0))
assert_error(detect_outbreak_clusters(x, as.Date("2026-10-14"), keyword_pattern = "["))
assert_error(detect_outbreak_clusters(x, as.Date("2026-10-14"), date_col = "诊断时间"))
assert(nrow(detect_outbreak_clusters(x[0, ], as.Date("2026-10-14"))) == 0L, "Empty input must return a typed empty result")
# No department reaches three cases: preserve the original 2+1 distribution,
# rather than recycling the institution total (3) into every department label.
scatter_cards <- tibble::tibble(
  场所类型 = "工作单位", 场所地区 = "530102", 机构预警ID = "SYNTHETIC-SCATTER",
  core_institution = "合成守恒小学", sub_level = c("演示部门甲", "演示部门甲", "演示部门乙"),
  疾病名称 = "合成病种", 发病日期 = as.Date(c("2026-10-05", "2026-10-06", "2026-10-07")), 区县 = "合成甲区"
)
scatter_micro <- detect_outbreak_micro_alerts(scatter_cards, threshold = 3)
assert(nrow(scatter_micro) == 1L && scatter_micro$聚集病例数 == 3L &&
  scatter_micro$预警类型 == "机构级聚集，子部门未达阈值", "A 2+1 institution must produce one three-case scatter summary")
scatter_parts <- strsplit(scatter_micro$子部门分布, "；", fixed = TRUE)[[1L]]
assert(setequal(scatter_parts, c("演示部门甲(2)", "演示部门乙(1)")), "Scatter labels must retain the distinct department counts 2 and 1")
scatter_counts <- as.integer(sub("^.*\\(([0-9]+)\\)$", "\\1", scatter_parts))
assert(sum(scatter_counts) == scatter_micro$聚集病例数, "Scatter department counts must sum to the institution case total")
class_micro <- detect_outbreak_micro_alerts(scatter_cards, threshold = 2)
assert(nrow(class_micro) == 1L && class_micro$聚集病例数 == 2L &&
  class_micro$预警类型 == "班级/科室聚集" && class_micro$子部门分布 == "演示部门甲(2)",
  "When a department reaches threshold, emit that department without a duplicate residual scatter summary")
# Exercise macro/micro joins and tie-breaks on reproducible synthetic demo.
demo <- load_surveillance_data("data/demo_surveillance.csv")
alerts <- detect_outbreak_clusters(demo, as.Date("2026-10-08"), start_date = as.Date("2026-10-01"))
candidates <- build_outbreak_candidate_cards(demo, as.Date("2026-10-01"), as.Date("2026-10-08"))
cards <- filter_outbreak_cards_by_alerts(candidates, alerts)
micro <- detect_outbreak_micro_alerts(cards)
assert(nrow(alerts) == 3L && sum(micro$预警类型 == "班级/科室聚集") == 2L, "Synthetic demo should have three institution and two department alerts")
assert(nrow(cards) == sum(alerts$聚集病例数), "Alert-card joins must agree with the selected windows")
unlink(c(path, legacy))
cat("All cleaning and outbreak detection checks passed.\n")
