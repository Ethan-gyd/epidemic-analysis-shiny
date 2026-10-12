# 丰富报告统计边界测试；全部病例、人口和地图均为本文件合成。
# 运行：Rscript tests/test_report_analysis.R
source("report_generation.R", encoding = "UTF-8")
source("report_analysis.R", encoding = "UTF-8")

checks <- 0L
expect <- function(condition, label) {
  if (!isTRUE(condition)) stop("FAIL: ", label, call. = FALSE)
  checks <<- checks + 1L
  cat("PASS:", label, "\n")
}
expect_error <- function(expr, label) expect(tryCatch({ force(expr); FALSE }, error = function(e) TRUE), label)
dates <- as.Date
period <- dates(c("2026-09-01", "2026-09-30"))
coverage <- dates(c("2025-01-01", "2026-10-08"))
ages <- c(paste0(seq(0, 80, 5), "-", seq(4, 84, 5), "岁"), "85岁及以上")
cases <- data.frame(
  发病日期 = dates(c("2025-09-01", "2025-09-02", "2026-08-01", "2026-08-31", "2026-09-01", "2026-09-02", "2026-09-15", "2026-09-30")),
  标准病种 = c("甲病", "丙病", "甲病", "丙病", "甲病", "甲病", "乙病", NA),
  区县 = c("甲区", "乙区", "甲区", "乙区", "甲区", "甲区", "乙区", NA),
  管理级别 = c(rep("乙类", 6), "丙类", NA), 传播途径 = c(rep("呼吸道", 6), "接触", NA),
  性别标准 = c(rep("男", 5), "女", "男", NA),
  年龄组 = factor(c(rep("0-4岁", 5), "5-9岁", "10-14岁", NA), levels = ages, ordered = TRUE),
  人群分类 = c(rep("学生", 6), "其他", NA),
  姓名 = "禁止输出个人姓名", 卡片编号 = "PRIVATE-CARD", 现住详细地址 = "禁止输出私人地址", check.names = FALSE)
attr(cases, "private") <- "禁止输出属性"
population <- data.frame(地区名称 = c("甲区", "乙区", "丙区", "总计", "未填写"),
  人口数 = c(100000, 200000, 50000, 350000, 999999), 是否汇总行 = c(FALSE, FALSE, FALSE, TRUE, FALSE),
  数据源 = "合成人口2026", check.names = FALSE)
make <- function(data = cases, pop = NULL, complete = NULL, map = NULL, alerts = NULL, micro = NULL, names = TRUE) {
  p <- build_report_payload(data, period, coverage_range = complete, outbreak_alerts = alerts)
  enrich_report_payload(p, data, population_data = pop, admin_map_data = map,
    outbreak_alerts = alerts, outbreak_micro_alerts = micro, include_place_names = names)
}

p <- make(pop = population)
expect(sum(p$management_table$病例数) == 4L && sum(p$route_table$病例数) == 4L, "管理类别和传播途径缺失值不丢记录")
expect(sum(p$weekly$病例数) == 4L && nrow(p$weekly) == 5L, "自然周趋势覆盖本期全部日期并补零")
expect(p$weekly$统计起始[1] == period[1] && tail(p$weekly$统计结束, 1) == period[2], "跨月周明确标记实际纳入起止")
expect(sum(p$disease_daily$病例数) == 4L && nrow(p$disease_daily) == 90L, "病种日趋势每病种补齐全月并守恒")
expect(sum(p$disease_weekly$病例数) == 4L && sum(p$district_weekly$病例数) == 4L, "病种和地区周趋势总数守恒")
expect("丙区" %in% p$district_weekly$区县 && all(p$district_weekly$病例数[p$district_weekly$区县 == "丙区"] == 0), "人口地区的零病例周序列仍保留")
expect(identical(p$age_levels, c(ages, "未填写")), "年龄自然顺序完整保留十八组并单列未填写")
expect(nrow(p$age_sex) == 57L && sum(p$age_sex$病例数) == 4L, "年龄性别交叉补零并保留未知性别和年龄")
expect(sum(p$age_table$病例数) == 4L && nrow(p$age_table) == 19L, "年龄表不截尾或合并为其他")
expect(p$population_summary$total_population == 350000, "总计和未知人口不重复进入人口分母")
expect(p$population_summary$unmatched_cases == 1L && is.na(p$population_summary$total_rate), "部分人口匹配时不把全部病例除以部分人口")
expect(p$district_rates$每10万人口记录数[p$district_rates$区县 == "甲区"] == 2, "地区率使用本区人口分母")
expect(is.na(p$district_rates$每10万人口记录数[p$district_rates$区县 == "未填写"]), "未知地区病例保留但率为NA")
expect(sum(p$district_rates$病例数) == 4L && sum(p$disease_district$病例数) == 4L, "地区表和病种地区交叉不丢未匹配病例")
cross <- p$disease_district[p$disease_district$病种 == "甲病" & p$disease_district$区县 == "甲区", ]
expect(cross$构成比 == 50 && cross$每10万人口记录数 == 2, "交叉表构成比以全期病例为分母而人口率按该区分母")
expect(all(is.na(p$disease_rates$每10万人口记录数)), "部分人口匹配时不生成貌似全范围的病种总率")
expect(grepl("记录数人口比", p$rate_label) && grepl("完整覆盖未确认", p$rate_note), "未确认覆盖时只标记录数人口比")
expect(p$population_summary$source == "合成人口2026", "保留人口参数来源")
expect(all(is.na(p$disease_comparison$环比变化百分比)) && all(is.na(p$district_comparison$同比变化例数)), "覆盖未知时病种地区比较不造百分比和增减")
expect("丙病" %in% p$disease_comparison$病种, "比较表保留仅历史期间有病例的病种")
expect(identical(unique(p$comparison_curve$期间), "本期"), "未确认覆盖时不生成历史比较曲线")
expect(length(p$disease_profiles) == 2L && sum(p$disease_profiles[[1]]$daily$病例数) == p$disease_profiles[[1]]$total, "重点病种画像返回日趋势及完整三间统计")
expect(p$disease_profiles[[1]]$share == 50 && p$disease_profiles[[1]]$district_table$构成比[1] == 100, "重点病种总占比与病种内部地区构成比使用各自分母")
expect(all(vapply(p$disease_profiles, function(x) sum(x$age_table$病例数) == x$total && sum(x$district_table$病例数) == x$total, logical(1))), "各重点病种画像病例数与构成分母独立守恒")

full <- make(data = cases[seq_len(7), ], pop = population, complete = coverage)
expect(isTRUE(all.equal(full$population_summary$total_rate, 3 / 350000 * 100000)), "完整人口匹配使用全范围总人口")
expect(all(full$disease_rates$人口数 == 350000), "不同病种使用相同地理人口分母含零病例地区")
expect(isTRUE(all.equal(full$disease_rates$每10万人口记录数, full$disease_rates$病例数 / 350000 * 100000)), "病种总率不按出现地区重新缩小人口分母")
expect(full$rate_label == "报告发病率（/10万）", "确认完整覆盖且发病日期口径时标明报告发病率")
expect(nrow(full$comparison_curve) == 91L && sum(full$comparison_curve$病例数) == 7L, "完整覆盖时比较曲线按各自期内天序保留不同月长")
historical <- full$disease_comparison[full$disease_comparison$病种 == "丙病", ]
expect(historical$本期病例数 == 0 && historical$环比变化例数 == -1L && historical$环比变化百分比 == -100, "历史有病例本期为零可正确比较")
new_disease <- full$disease_comparison[full$disease_comparison$病种 == "乙病", ]
expect(new_disease$环比病例数 == 0L && new_disease$环比变化例数 == 1L && is.na(new_disease$环比变化百分比), "零基数可算例数增量但不算百分比")
partial <- make(complete = dates(c("2026-09-01", "2026-09-30")))
expect(all(is.na(partial$disease_comparison$环比病例数)) && all(is.na(partial$disease_comparison$环比变化例数)), "仅本期覆盖不可把历史观察数当完整比较期")
partial_current <- make(complete = dates(c("2025-01-01", "2026-09-15")))
expect(all(is.na(partial_current$disease_comparison$环比变化百分比)), "本期覆盖不足不可计算百分比")

none <- make()
expect(!none$population_summary$available && all(is.na(none$district_rates$每10万人口记录数)), "无人口仍输出地区计数且不生成率")
bad_pop <- population; bad_pop$人口数[1] <- 0
bad <- make(pop = bad_pop)
expect(is.na(bad$district_rates$每10万人口记录数[bad$district_rates$区县 == "甲区"]), "零人口不产生Inf或假零率")
same <- make(pop = rbind(population, population[1, ]))
expect(same$population_summary$total_population == 350000, "同地区重复相同人口值不翻倍")
conflict_pop <- rbind(population, population[1, ]); conflict_pop$人口数[nrow(conflict_pop)] <- 110000
conflict <- make(pop = conflict_pop)
expect(is.na(conflict$district_rates$人口数[conflict$district_rates$区县 == "甲区"]), "同地区人口冲突时禁用该分母")
empty <- make(data = cases[FALSE, ], pop = population)
expect(empty$total_cases == 0L && all(empty$weekly$病例数 == 0L) && length(empty$disease_profiles) == 0L, "空报告正常返回补零趋势和空画像")
expect(nrow(empty$district_rates) == 3L && all(empty$district_rates$病例数 == 0L), "空报告仍保留人口分析地区")
expect(nrow(empty$disease_comparison) == 0L && nrow(empty$disease_district) == 0L, "空数据比较和交叉表稳定为空")

scope_data <- cases[cases$区县 %in% "甲区", ]
scope <- build_report_payload(scope_data, period)
scope$district_filter_values <- "甲区"
scope <- enrich_report_payload(scope, scope_data, population_data = population)
expect(scope$population_summary$total_population == 100000 && nrow(scope$district_rates) == 1L, "地区筛选同时约束零病例地区人口分母")
expect_error(enrich_report_payload(build_report_payload(cases, period), cases[1:7, ]), "拒绝基础和扩展病例范围不一致")
diagnosis_cases <- cases; diagnosis_cases$诊断时间 <- cases$发病日期
diagnosis <- build_report_payload(diagnosis_cases, period, time_col = "诊断时间", coverage_range = coverage)
diagnosis <- enrich_report_payload(diagnosis, diagnosis_cases, population_data = population)
expect(diagnosis$rate_label == "报告病例率（/10万）" && grepl("诊断时间", diagnosis$rate_note), "非发病日期率正确标记时间口径")

alerts <- data.frame(机构预警ID = "PRIVATE-ALERT-ID", core_institution = "禁止输出机构名",
  疾病名称 = "甲病", 场所类型 = "工作单位", 涉及区县 = "甲区", 聚集病例数 = 5L,
  预警窗口起 = dates("2026-09-01"), 预警窗口止 = dates("2026-09-14"),
  最早发病日期 = dates("2026-09-01"), 最晚发病日期 = dates("2026-09-04"), 时间跨度天数 = 4L,
  子部门数 = 2L, 子部门分布 = "禁止输出部门甲(3)；禁止输出部门乙(2)", 卡片编号 = "PRIVATE-CARD", check.names = FALSE)
micro <- data.frame(机构预警ID = "PRIVATE-ALERT-ID", core_institution = "禁止输出机构名", sub_level = "禁止输出部门甲",
  疾病名称 = "甲病", 场所类型 = "工作单位", 预警类型 = "班级/科室聚集", 涉及区县 = "甲区", 聚集病例数 = 3L,
  最早发病日期 = dates("2026-09-01"), 最晚发病日期 = dates("2026-09-03"), 子部门分布 = "禁止输出部门甲(3)", 姓名 = "禁止输出个人姓名", check.names = FALSE)
safe <- make(alerts = alerts, micro = micro, names = FALSE)
expect(safe$outbreak_details$线索编号 == "S001" && safe$outbreak_micro_details$线索编号 == "S001", "场所与子部门明细通过匿名编号关联")
expect(safe$outbreak_details$子部门分布 == "部门01(3)；部门02(2)" && safe$outbreak_micro_details$部门编号 == "部门01", "部门分布保留计数并同一映射匿名化")
expect(safe$outbreak_micro_details$预警窗口止 == dates("2026-09-14"), "子部门继承机构探测窗口而非伪造窗口")
expect(!any(grepl("禁止输出|PRIVATE-", unlist(safe))), "显式匿名payload不保存个人字段、机构名、部门名或私有属性")
expect(!any(c("data", "current", "cases", "cards", "raw_data", "outbreak_alerts") %in% names(safe)), "报告payload没有任何病例级数据入口")
named <- build_report_payload(cases, period, outbreak_alerts = alerts, outbreak_micro_alerts = micro)
expect(named$include_place_names && named$outbreak_details$机构名称 == "禁止输出机构名" && named$outbreak_micro_details$子部门名称 == "禁止输出部门甲", "默认报告保留机构和具体子部门名称")
default_enriched <- enrich_report_payload(build_report_payload(cases, period), cases,
  outbreak_alerts = alerts, outbreak_micro_alerts = micro)
expect(default_enriched$include_place_names && default_enriched$outbreak_details$机构名称 == "禁止输出机构名" &&
  default_enriched$outbreak_micro_details$机构名称 == "禁止输出机构名", "扩展统计默认实名并保留子部门所属机构")
expect(!any(grepl("PRIVATE-|禁止输出个人姓名|禁止输出私人地址|禁止输出属性", unlist(named))), "实名机构选项仍不导出姓名卡片地址或内部标识")
expect(is.na(none$outbreak_micro_count) && !none$outbreak_micro_evaluated, "未运行子部门探测不冒充零线索")
zero_alerts <- make(alerts = alerts[FALSE, ], micro = micro[FALSE, ])
expect(zero_alerts$outbreak_micro_count == 0L && zero_alerts$outbreak_micro_evaluated, "已运行零子部门线索明确记录零")
micro_only <- make(micro = micro)
expect(nrow(micro_only$outbreak_micro_details) == 1L && is.na(micro_only$outbreak_micro_details$预警窗口起), "缺父场所汇总时不虚构子部门探测窗口")
two_micro <- rbind(micro, micro)
two_micro$sub_level[2] <- "禁止输出部门乙"; two_micro$子部门分布[2] <- "禁止输出部门乙(3)"
two_micro_payload <- make(micro = two_micro, names = FALSE)
expect(identical(two_micro_payload$outbreak_micro_details$部门编号, c("部门01", "部门02")) &&
  identical(two_micro_payload$outbreak_micro_details$子部门分布, c("部门01(3)", "部门02(3)")), "仅子部门表仍保持跨行部门编号一致")

scatter_alert <- alerts
scatter_alert$聚集病例数 <- 3L
scatter_alert$子部门分布 <- "禁止输出部门甲(2)；禁止输出部门乙(1)"
scatter_micro <- micro
scatter_micro$sub_level <- "机构内散发"
scatter_micro$预警类型 <- "机构级聚集，子部门未达阈值"
scatter_micro$子部门分布 <- scatter_alert$子部门分布
scatter_named <- build_report_payload(cases, period, outbreak_alerts = scatter_alert, outbreak_micro_alerts = scatter_micro)
expect(scatter_named$outbreak_micro_details$机构名称 == "禁止输出机构名" &&
  scatter_named$outbreak_micro_details$子部门名称 == "禁止输出部门甲、禁止输出部门乙" &&
  scatter_named$outbreak_micro_details$子部门分布 == "禁止输出部门甲(2)；禁止输出部门乙(1)" &&
  scatter_named$outbreak_micro_details$聚集病例数 == 3L,
  "机构内散发默认保留所属机构及全部具体部门名称和真实计数")
scatter_safe <- build_report_payload(cases, period, outbreak_alerts = scatter_alert,
  outbreak_micro_alerts = scatter_micro, include_place_names = FALSE)
expect(!scatter_safe$include_place_names && !any(grepl("禁止输出|PRIVATE-", unlist(scatter_safe))) &&
  scatter_safe$outbreak_micro_details$子部门分布 == "部门01(2)；部门02(1)",
  "显式匿名选项保留散发部门计数并清除具体名称")

if (requireNamespace("sf", quietly = TRUE)) {
  polygon <- function(x) sf::st_polygon(list(matrix(c(x, 0, x + 1, 0, x + 1, 1, x, 1, x, 0), ncol = 2L, byrow = TRUE)))
  map <- sf::st_sf(区县 = c("甲区", "乙区"), 姓名 = "禁止输出地图属性",
    geometry = sf::st_sfc(polygon(0), polygon(1), crs = 4326))
  mapped <- make(pop = population, map = map)
  expect(inherits(mapped$map_sf, "sf") && sum(mapped$map_sf$病例数) == 3L, "地图只匹配本期汇总病例数")
  expect(!"姓名" %in% names(mapped$map_sf) && !any(grepl("禁止输出地图属性", unlist(mapped))), "地图输出字段白名单不带来源私有属性")
}
cat("\nAll", checks, "rich report analysis checks passed.\n")
