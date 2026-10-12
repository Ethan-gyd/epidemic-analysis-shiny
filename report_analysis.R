# 报告扩展统计：仅返回用于图表和表格的汇总，不在 payload 中保存病例行。
# 本文件仅依赖 report_generation.R 中的日期/标签函数；地图功能可选依赖 sf。

report_analysis_column <- function(data, name) {
  if (name %in% names(data)) data[[name]] else rep(NA_character_, nrow(data))
}

report_analysis_week <- function(dates) dates - ((as.POSIXlt(dates)$wday + 6L) %% 7L)

report_analysis_grid <- function(x, y, x_levels, y_levels, x_name, y_name) {
  out <- expand.grid(x = x_levels, y = y_levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  counts <- table(factor(x, levels = x_levels), factor(y, levels = y_levels))
  names(out) <- c(x_name, y_name)
  out$病例数 <- as.integer(counts)
  out
}

report_analysis_comparison <- function(values, dates, payload, label) {
  periods <- report_comparison_periods(payload$date_range, payload$report_type)
  valid <- !is.na(dates)
  mask <- function(period) valid & dates >= period[1] & dates <= period[2]
  current <- mask(payload$date_range)
  previous <- mask(periods$previous)
  year_ago <- mask(periods$year_ago)
  covered <- function(period) !is.null(payload$coverage_range) &&
    payload$coverage_range[1] <= period[1] && payload$coverage_range[2] >= period[2]
  categories <- sort(unique(values[current | previous | year_ago]))
  counts <- function(rows) as.integer(table(factor(values[rows], levels = categories)))
  now <- counts(current)
  out <- data.frame(分类 = categories, 本期病例数 = now,
                    构成比 = if (sum(now)) round(100 * now / sum(now), 1) else rep(0, length(now)), check.names = FALSE)
  names(out)[1L] <- label
  for (kind in c("环比", "同比")) {
    p <- if (kind == "环比") periods$previous else periods$year_ago
    base <- if (covered(p)) counts(mask(p)) else rep(NA_integer_, length(categories))
    comparable <- isTRUE(payload$current_complete) && covered(p)
    delta <- if (comparable) now - base else rep(NA_integer_, length(categories))
    rate <- if (comparable) ifelse(base > 0, 100 * delta / base, NA_real_) else rep(NA_real_, length(categories))
    reason <- if (!isTRUE(payload$current_complete)) rep("本期完整覆盖未确认", length(categories)) else
      if (!covered(p)) rep("比较期完整覆盖未确认", length(categories)) else
        ifelse(base == 0, "比较期为零 不计算百分比", "可比较")
    out[[paste0(kind, "病例数")]] <- base
    out[[paste0(kind, "变化例数")]] <- delta
    out[[paste0(kind, "变化百分比")]] <- rate
    out[[paste0(kind, "说明")]] <- reason
  }
  out[order(-out$本期病例数, out[[label]]), , drop = FALSE]
}

report_analysis_population <- function(population_data, payload) {
  empty <- data.frame(区县 = character(), 人口数 = numeric(), 人口匹配状态 = character(), check.names = FALSE)
  if (is.null(population_data) || !is.data.frame(population_data) || !nrow(population_data)) {
    return(list(units = empty, source = "未提供人口参数", note = "未提供人口参数；不计算人口比率。"))
  }
  name_col <- intersect(c("地区名称", "区县"), names(population_data))
  if (!length(name_col) || !"人口数" %in% names(population_data)) {
    return(list(units = empty, source = "人口参数字段不完整", note = "人口参数缺少地区名称或人口数；不计算人口比率。"))
  }
  labels <- report_labels(population_data[[name_col[1L]]])
  unknown <- labels %in% c("未填写", "未知", "地区未知", "未匹配")
  totals <- labels %in% c("合计", "总计", "全市", "全省", "全国", "总人口")
  if ("是否汇总行" %in% names(population_data)) {
    flag <- population_data$是否汇总行
    totals <- totals | (!is.na(flag) & as.character(flag) %in% c("TRUE", "1", "是"))
  }
  # 参数名称是预处理器提供的研究范围；它与其下辖单元不可重复计人口。
  if ("参数名称" %in% names(population_data)) {
    scope <- unique(report_labels(population_data$参数名称))
    totals <- totals | (labels %in% scope & length(setdiff(labels, scope)) > 0L)
  }
  keep <- !unknown & !totals
  if (!is.null(payload$district_filter_values)) keep <- keep & labels %in% payload$district_filter_values
  labels <- labels[keep]
  numbers <- suppressWarnings(as.numeric(gsub(",", "", as.character(population_data$人口数[keep]), fixed = TRUE)))
  units <- if (!length(labels)) empty else do.call(rbind, lapply(sort(unique(labels)), function(label) {
    vals <- numbers[labels == label]
    valid <- all(is.finite(vals) & vals > 0)
    consistent <- length(unique(vals)) == 1L
    data.frame(区县 = label, 人口数 = if (valid && consistent) vals[1L] else NA_real_,
      人口匹配状态 = if (!valid) "缺少有效人口" else if (!consistent) "人口参数重复且不一致" else "已匹配", check.names = FALSE)
  }))
  source <- if ("数据源" %in% names(population_data)) paste(unique(report_labels(population_data$数据源)), collapse = "；") else "当前人口参数"
  list(units = units, source = source,
    note = "人口分母按地区名称与当前人口参数精确匹配，排除汇总行和未知地区；相同地区重复相同人口值只取一次，冲突值不参与计算。")
}

report_analysis_departments <- function(text) {
  if (length(text) != 1L || is.na(text) || !nzchar(text)) return(data.frame(name = character(), cases = integer()))
  parts <- strsplit(text, "[；;]", perl = TRUE)[[1L]]
  valid <- grepl("[（(][0-9]+[）)]$", trimws(parts))
  parts <- trimws(parts[valid])
  data.frame(name = sub("[（(][0-9]+[）)]$", "", parts),
    cases = as.integer(sub("^.*[（(]([0-9]+)[）)]$", "\\1", parts)), stringsAsFactors = FALSE)
}

report_analysis_alerts <- function(alerts, micro_alerts, include_place_names) {
  detail <- data.frame(线索编号 = character(), 疾病名称 = character(), 场所类型 = character(), 涉及区县 = character(),
    聚集病例数 = integer(), 预警窗口起 = as.Date(character()), 预警窗口止 = as.Date(character()),
    最早发病日期 = as.Date(character()), 最晚发病日期 = as.Date(character()), 时间跨度天数 = integer(),
    子部门数 = integer(), 子部门分布 = character(), check.names = FALSE)
  micro <- data.frame(线索编号 = character(), 部门编号 = character(), 疾病名称 = character(), 场所类型 = character(),
    预警类型 = character(), 涉及区县 = character(), 聚集病例数 = integer(),
    预警窗口起 = as.Date(character()), 预警窗口止 = as.Date(character()),
    最早发病日期 = as.Date(character()), 最晚发病日期 = as.Date(character()), 子部门分布 = character(), check.names = FALSE)
  if (!is.null(alerts) && !is.data.frame(alerts)) stop("outbreak_alerts 必须为汇总数据框。", call. = FALSE)
  if (!is.null(micro_alerts) && !is.data.frame(micro_alerts)) stop("outbreak_micro_alerts 必须为汇总数据框。", call. = FALSE)
  # 身份映射仅在本函数内部使用，不返回机构预警 ID、卡片 ID 或病例行。
  identity <- function(d) {
    if ("机构预警ID" %in% names(d)) return(as.character(d$机构预警ID))
    paste(report_analysis_column(d, "场所类型"), report_analysis_column(d, "场所地区"),
      report_analysis_column(d, "core_institution"), report_analysis_column(d, "疾病名称"), sep = "\r")
  }
  field <- function(d, name, kind = "text") {
    x <- report_analysis_column(d, name)
    if (kind == "date") return(if (all(is.na(x))) rep(as.Date(NA), length(x)) else report_date(x))
    if (kind == "integer") return(suppressWarnings(as.integer(x)))
    report_labels(x)
  }
  ids <- if (is.null(alerts)) character() else identity(alerts)
  dept_maps <- vector("list", length(ids))
  if (!is.null(alerts) && nrow(alerts)) {
    counts <- field(alerts, "聚集病例数", "integer")
    if (anyNA(counts) || any(counts < 0)) stop("预警汇总病例数无效。", call. = FALSE)
    detail <- data.frame(线索编号 = sprintf("S%03d", seq_len(nrow(alerts))),
      疾病名称 = field(alerts, "疾病名称"), 场所类型 = field(alerts, "场所类型"), 涉及区县 = field(alerts, "涉及区县"),
      聚集病例数 = counts, 预警窗口起 = field(alerts, "预警窗口起", "date"), 预警窗口止 = field(alerts, "预警窗口止", "date"),
      最早发病日期 = field(alerts, "最早发病日期", "date"), 最晚发病日期 = field(alerts, "最晚发病日期", "date"),
      时间跨度天数 = field(alerts, "时间跨度天数", "integer"), 子部门数 = field(alerts, "子部门数", "integer"),
      子部门分布 = rep("未提供", nrow(alerts)), check.names = FALSE)
    for (i in seq_len(nrow(alerts))) {
      parsed <- report_analysis_departments(field(alerts, "子部门分布")[i])
      dept_maps[[i]] <- setNames(sprintf("部门%02d", seq_len(nrow(parsed))), parsed$name)
      if (nrow(parsed)) detail$子部门分布[i] <- paste0(if (include_place_names) parsed$name else dept_maps[[i]],
        "(", parsed$cases, ")", collapse = "；")
    }
    if (include_place_names) detail$机构名称 <- field(alerts, "core_institution")
  }
  if (!is.null(micro_alerts) && nrow(micro_alerts)) {
    counts <- field(micro_alerts, "聚集病例数", "integer")
    if (anyNA(counts) || any(counts < 0)) stop("子部门预警病例数无效。", call. = FALSE)
    matches <- match(identity(micro_alerts), ids)
    extra_ids <- unique(identity(micro_alerts)[is.na(matches)])
    group_indices <- ifelse(is.na(matches), length(ids) + match(identity(micro_alerts), extra_ids), matches)
    labels <- sprintf("S%03d", group_indices)
    # 即使调用方只提供子部门表，同一场所的部门编号也在整份报告中保持一致。
    if (length(extra_ids)) dept_maps <- c(dept_maps, vector("list", length(extra_ids)))
    for (i in seq_len(nrow(micro_alerts))) {
      group <- group_indices[i]
      parsed <- report_analysis_departments(field(micro_alerts, "子部门分布")[i])
      dept <- field(micro_alerts, "sub_level")[i]
      candidates <- setdiff(unique(c(parsed$name, dept)), c("未填写", "机构内散发"))
      new_names <- setdiff(candidates, names(dept_maps[[group]]))
      if (length(new_names)) dept_maps[[group]] <- c(dept_maps[[group]],
        setNames(sprintf("部门%02d", length(dept_maps[[group]]) + seq_along(new_names)), new_names))
    }
    micro <- data.frame(线索编号 = labels, 部门编号 = rep("未提供", nrow(micro_alerts)),
      疾病名称 = field(micro_alerts, "疾病名称"), 场所类型 = field(micro_alerts, "场所类型"),
      预警类型 = field(micro_alerts, "预警类型"), 涉及区县 = field(micro_alerts, "涉及区县"), 聚集病例数 = counts,
      预警窗口起 = detail$预警窗口起[matches], 预警窗口止 = detail$预警窗口止[matches],
      最早发病日期 = field(micro_alerts, "最早发病日期", "date"), 最晚发病日期 = field(micro_alerts, "最晚发病日期", "date"),
      子部门分布 = rep("未提供", nrow(micro_alerts)), check.names = FALSE)
    if (!nrow(detail)) {
      micro$预警窗口起 <- rep(as.Date(NA), nrow(micro))
      micro$预警窗口止 <- rep(as.Date(NA), nrow(micro))
    }
    for (i in seq_len(nrow(micro))) {
      parsed <- report_analysis_departments(field(micro_alerts, "子部门分布")[i])
      map <- dept_maps[[group_indices[i]]]
      dept <- field(micro_alerts, "sub_level")[i]
      if (dept == "机构内散发") micro$部门编号[i] <- "机构内散发" else if (dept %in% names(map)) micro$部门编号[i] <- unname(map[dept]) else micro$部门编号[i] <- sprintf("部门%02d", i)
      if (nrow(parsed)) {
        shown <- if (include_place_names) parsed$name else ifelse(parsed$name %in% names(map), map[parsed$name], sprintf("部门%02d", seq_len(nrow(parsed))))
        micro$子部门分布[i] <- paste0(shown, "(", parsed$cases, ")", collapse = "；")
      }
    }
    if (include_place_names) {
      micro$机构名称 <- field(micro_alerts, "core_institution")
      micro$子部门名称 <- field(micro_alerts, "sub_level")
      for (i in seq_len(nrow(micro))) {
        if (micro$子部门名称[i] %in% c("机构内散发", "未填写")) {
          parsed <- report_analysis_departments(field(micro_alerts, "子部门分布")[i])
          if (nrow(parsed)) micro$子部门名称[i] <- paste(unique(parsed$name), collapse = "、")
        }
      }
    }
  }
  list(details = detail, micro_details = micro, micro_evaluated = !is.null(micro_alerts),
    micro_count = if (is.null(micro_alerts)) NA_integer_ else nrow(micro_alerts))
}

# data 必须与基础 payload 使用同一筛选范围，保留全日期供历史比较。
# district_filter_values（NULL 为全部）由基础构造器记录，用于保留筛选范围内零病例地区的分母。
enrich_report_payload <- function(payload, data, population_data = NULL, admin_map_data = NULL,
                                  outbreak_alerts = NULL, outbreak_micro_alerts = NULL,
                                  include_place_names = TRUE) {
  if (!inherits(payload, "surveillance_report") || !is.data.frame(data)) stop("请提供基础报告 payload 和同范围清洗数据。", call. = FALSE)
  if (!payload$time_col %in% names(data) || !payload$disease_col %in% names(data)) stop("数据缺少报告统计字段。", call. = FALSE)
  if (!is.null(payload$disease_filter_values)) data <- data[!is.na(data[[payload$disease_col]]) & as.character(data[[payload$disease_col]]) %in% payload$disease_filter_values, , drop = FALSE]
  if (!is.null(payload$district_filter_values)) data <- data[!is.na(report_analysis_column(data, "区县")) & as.character(report_analysis_column(data, "区县")) %in% payload$district_filter_values, , drop = FALSE]
  dates <- report_date(data[[payload$time_col]])
  in_period <- !is.na(dates) & dates >= payload$date_range[1] & dates <= payload$date_range[2]
  current <- data[in_period, , drop = FALSE]
  if (nrow(current) != payload$total_cases) stop("扩展统计与基础报告的病例范围不一致，请传入相同筛选范围。", call. = FALSE)
  current_dates <- dates[in_period]
  get <- function(name) report_labels(report_analysis_column(current, name))
  disease <- get(payload$disease_col)
  district <- get("区县")
  payload$management_table <- report_count_table(get("管理级别"), "管理级别")
  payload$route_table <- report_count_table(get("传播途径"), "传播途径")
  payload$disease_comparison <- report_analysis_comparison(report_labels(data[[payload$disease_col]]), dates, payload, "病种")
  payload$district_comparison <- report_analysis_comparison(report_labels(report_analysis_column(data, "区县")), dates, payload, "区县")
  comparison_periods <- report_comparison_periods(payload$date_range, payload$report_type)
  curve_periods <- c(list(本期 = payload$date_range), setNames(comparison_periods, c("环比期", "同比期")))
  payload$comparison_curve <- do.call(rbind, lapply(names(curve_periods), function(label) {
    bounds <- curve_periods[[label]]
    confirmed <- isTRUE(payload$current_complete) && !is.null(payload$coverage_range) &&
      payload$coverage_range[1] <= bounds[1] && payload$coverage_range[2] >= bounds[2]
    if (label != "本期" && !confirmed) return(NULL)
    days <- seq(bounds[1], bounds[2], by = "day")
    data.frame(期间 = label, 期内天序 = seq_along(days), 日期 = days,
      病例数 = as.integer(table(factor(dates, levels = days))), check.names = FALSE)
  }))
  rownames(payload$comparison_curve) <- NULL
  weeks <- unique(report_analysis_week(seq(payload$date_range[1], payload$date_range[2], by = "day")))
  current_weeks <- report_analysis_week(current_dates)
  payload$weekly <- data.frame(周起始 = weeks, 周结束 = weeks + 6L,
    统计起始 = pmax(weeks, payload$date_range[1]), 统计结束 = pmin(weeks + 6L, payload$date_range[2]),
    病例数 = as.integer(table(factor(current_weeks, levels = weeks))), check.names = FALSE)
  disease_levels <- payload$disease_table$病种
  district_levels <- payload$district_table$区县
  payload$disease_daily <- report_analysis_grid(current_dates, disease, payload$daily$日期, disease_levels, "日期", "病种")
  payload$disease_weekly <- report_analysis_grid(current_weeks, disease, weeks, disease_levels, "周起始", "病种")
  payload$district_weekly <- report_analysis_grid(current_weeks, district, weeks, district_levels, "周起始", "区县")
  canonical_age <- c(paste0(seq(0, 80, by = 5), "-", seq(4, 84, by = 5), "岁"), "85岁及以上")
  source_age <- report_analysis_column(data, "年龄组")
  age <- get("年龄组")
  age_levels <- if (is.factor(source_age)) levels(source_age) else canonical_age
  age_levels <- unique(c(age_levels, sort(setdiff(unique(age), c(age_levels, "未填写"))), "未填写"))
  sex <- get("性别标准")
  sex_levels <- unique(c("男", "女", sort(setdiff(unique(sex), c("男", "女", "未填写"))), "未填写"))
  payload$age_levels <- age_levels
  payload$age_sex <- report_analysis_grid(age, sex, age_levels, sex_levels, "年龄组", "性别")
  payload$age_sex$构成比 <- if (nrow(current)) round(100 * payload$age_sex$病例数 / nrow(current), 1) else rep(0, nrow(payload$age_sex))
  payload$age_table <- data.frame(年龄组 = age_levels,
    病例数 = as.integer(table(factor(age, levels = age_levels))), check.names = FALSE)
  payload$age_table$构成比 <- if (nrow(current)) round(100 * payload$age_table$病例数 / nrow(current), 1) else rep(0, nrow(payload$age_table))
  pop <- report_analysis_population(population_data, payload)
  district_levels <- unique(c(pop$units$区县, district_levels))
  index <- match(district_levels, pop$units$区县)
  rates <- data.frame(区县 = district_levels, 人口数 = pop$units$人口数[index],
    病例数 = as.integer(table(factor(district, levels = district_levels))), check.names = FALSE)
  rates$构成比 <- if (nrow(current)) round(100 * rates$病例数 / nrow(current), 1) else rep(0, nrow(rates))
  rates$每10万人口记录数 <- 100000 * rates$病例数 / rates$人口数
  rates$人口匹配状态 <- ifelse(is.na(index), "未匹配人口", pop$units$人口匹配状态[index])
  rates <- rates[order(is.na(rates$每10万人口记录数), -rates$每10万人口记录数, -rates$病例数, rates$区县), , drop = FALSE]
  rownames(rates) <- NULL
  payload$district_rates <- rates
  matched <- !is.na(rates$人口数)
  denominator <- if (any(matched)) sum(rates$人口数[matched]) else NA_real_
  unmatched_cases <- sum(rates$病例数[!matched])
  complete_population <- nrow(pop$units) > 0L && all(!is.na(pop$units$人口数)) && unmatched_cases == 0L
  payload$population_summary <- list(available = any(matched), matched_cases = sum(rates$病例数[matched]),
    unmatched_cases = unmatched_cases, total_population = denominator,
    total_rate = if (complete_population) 100000 * nrow(current) / denominator else NA_real_,
    matched_districts = sum(matched), total_districts = nrow(rates), complete = complete_population, source = pop$source)
  payload$rate_label <- if (isTRUE(payload$current_complete) && identical(payload$time_col, "发病日期")) "报告发病率（/10万）" else
    if (isTRUE(payload$current_complete)) "报告病例率（/10万）" else "记录数人口比（/10万）"
  payload$rate_note <- paste0(pop$note, if (isTRUE(payload$current_complete))
    paste0("使用者已确认本期数据完整覆盖；率为按", payload$time_col, "纳入的病例记录数除以匹配人口乘10万，未作年化或年龄标准化。") else
    "本期完整覆盖未确认，率仅表示已导入记录数/人口×10万，不能解释为真实发病率。",
    if (!complete_population) "人口匹配不完整或参数存在冲突，不计算整个筛选范围的总率。" else "各病种总率使用相同地理范围的总人口，不能将各病种出现地区的人口分别相加作分母。")
  payload$disease_rates <- data.frame(病种 = disease_levels, 病例数 = payload$disease_table$病例数,
    人口数 = rep(if (complete_population) denominator else NA_real_, length(disease_levels)), check.names = FALSE)
  payload$disease_rates$每10万人口记录数 <- 100000 * payload$disease_rates$病例数 / payload$disease_rates$人口数
  payload$disease_district <- report_analysis_grid(disease, district, disease_levels, district_levels, "病种", "区县")
  payload$disease_district$构成比 <- if (nrow(current)) round(100 * payload$disease_district$病例数 / nrow(current), 1) else rep(0, nrow(payload$disease_district))
  payload$disease_district$人口数 <- rates$人口数[match(payload$disease_district$区县, rates$区县)]
  payload$disease_district$每10万人口记录数 <- 100000 * payload$disease_district$病例数 / payload$disease_district$人口数
  payload$district_weekly <- report_analysis_grid(current_weeks, district, weeks, district_levels, "周起始", "区县")
  # 重点病种画像全部由汇总表组成；各画像的构成比分母为该病种本期记录。
  profile_diseases <- head(setdiff(disease_levels, "未填写"), 3L)
  payload$disease_profiles <- lapply(profile_diseases, function(name) {
    rows <- disease == name
    one <- current[rows, , drop = FALSE]
    ages <- data.frame(年龄组 = age_levels,
      病例数 = as.integer(table(factor(get("年龄组")[rows], levels = age_levels))), check.names = FALSE)
    ages$构成比 <- round(100 * ages$病例数 / sum(rows), 1)
    districts <- report_count_table(district[rows], "区县")
    districts$人口数 <- rates$人口数[match(districts$区县, rates$区县)]
    districts$每10万人口记录数 <- 100000 * districts$病例数 / districts$人口数
    list(name = name, total = sum(rows), share = round(100 * sum(rows) / nrow(current), 1),
      daily = data.frame(日期 = payload$daily$日期,
        病例数 = as.integer(table(factor(current_dates[rows], levels = payload$daily$日期))), check.names = FALSE),
      district_table = districts, age_table = ages,
      occupation_table = report_count_table(report_analysis_column(one, "人群分类"), "人群分类"),
      sex_table = report_count_table(report_analysis_column(one, "性别标准"), "性别"))
  })
  payload$map_sf <- NULL
  if (inherits(admin_map_data, "sf") && "区县" %in% names(admin_map_data) && requireNamespace("sf", quietly = TRUE)) {
    selected <- !is.na(admin_map_data$区县) & as.character(admin_map_data$区县) %in% rates$区县
    geometry <- sf::st_geometry(admin_map_data)[selected]
    map_labels <- as.character(admin_map_data$区县[selected])
    safe_map <- sf::st_sf(区县 = map_labels, geometry = geometry)
    for (name in setdiff(names(rates), "区县")) safe_map[[name]] <- rates[[name]][match(map_labels, rates$区县)]
    payload$map_sf <- safe_map
  }
  alerts <- report_analysis_alerts(outbreak_alerts, outbreak_micro_alerts, isTRUE(include_place_names))
  payload$outbreak_details <- alerts$details
  payload$outbreak_micro_details <- alerts$micro_details
  payload$outbreak_micro_evaluated <- alerts$micro_evaluated
  payload$outbreak_micro_count <- alerts$micro_count
  payload$include_place_names <- isTRUE(include_place_names)
  payload$limitations[1L] <- paste0("统计单位为清洗后病例记录。", payload$rate_note)
  payload$limitations[4L] <- if (isTRUE(include_place_names))
    "本报告导出汇总并按用户选择保留机构及部门名称；不包含姓名、卡片编号和详细地址。机构明细仅供内部核查，公开前应脱敏。" else
    "本报告仅包含汇总；场所及部门使用报告内编号，不含姓名、卡片编号、详细地址或具体机构名称。公开前仍应审查小样本汇总的可识别风险。"
  payload$analysis_version <- "2.0"
  payload
}
