# 自动报告模块。统计函数只使用 R 基础包，加载模块不会启动 Shiny 或读取病例文件。
# 外部入口：report_period(), build_report_payload(),
#           report_export_capabilities(), write_surveillance_report().
source("report_analysis.R", encoding = "UTF-8")

report_date <- function(x, argument = "日期") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "Asia/Shanghai"))
  if (!is.character(x)) stop(argument, "必须为 Date、POSIXt 或 YYYY-MM-DD 字符串。", call. = FALSE)
  text <- trimws(x)
  valid_shape <- !is.na(text) & grepl("^\\d{4}-\\d{2}-\\d{2}$", text)
  result <- rep(as.Date(NA), length(text))
  result[valid_shape] <- suppressWarnings(as.Date(text[valid_shape], format = "%Y-%m-%d"))
  roundtrip <- rep(FALSE, length(text))
  roundtrip[valid_shape] <- !is.na(result[valid_shape]) & format(result[valid_shape], "%Y-%m-%d") == text[valid_shape]
  result[!roundtrip] <- as.Date(NA)
  result
}

report_validate_range <- function(x, argument = "日期范围") {
  x <- report_date(x, argument)
  if (length(x) != 2L || anyNA(x) || x[1] > x[2]) {
    stop(argument, "必须是两个有效且按先后排列的日期。", call. = FALSE)
  }
  x
}

report_type_value <- function(x) {
  aliases <- c(月报 = "monthly", 周报 = "weekly", 专题 = "special", 专题分析 = "special")
  if (length(x) != 1L || is.na(x)) stop("请选择一种报告类型。", call. = FALSE)
  if (x %in% names(aliases)) x <- unname(aliases[x])
  match.arg(x, c("monthly", "weekly", "special"))
}

# 月报为完整自然月；周报为周一至周日；专题使用用户给定闭区间。
report_period <- function(type = "monthly", anchor_date = Sys.Date(), date_range = NULL) {
  type <- report_type_value(type)
  if (type == "special") return(report_validate_range(date_range))
  anchor_date <- report_date(anchor_date, "锚定日期")
  if (length(anchor_date) != 1L || is.na(anchor_date)) stop("锚定日期无效。", call. = FALSE)
  if (type == "monthly") {
    first <- as.Date(format(anchor_date, "%Y-%m-01"))
    return(c(first, seq(first, by = "month", length.out = 2L)[2] - 1))
  }
  first <- anchor_date - ((as.POSIXlt(anchor_date)$wday + 6L) %% 7L)
  c(first, first + 6L)
}

report_previous_year <- function(dates) {
  years <- as.integer(format(dates, "%Y")) - 1L
  text <- paste0(years, format(dates, "-%m-%d"))
  text[format(dates, "%m-%d") == "02-29"] <- paste0(years[format(dates, "%m-%d") == "02-29"], "-02-28")
  as.Date(text)
}

report_comparison_periods <- function(period, type) {
  if (type == "monthly") {
    previous <- report_period("monthly", period[1] - 1L)
    year_ago <- report_period("monthly", report_previous_year(period[1]))
  } else {
    previous <- period - (as.integer(diff(period)) + 1L)
    year_ago <- if (type == "weekly") period - 364L else report_previous_year(period)
  }
  list(previous = previous, year_ago = year_ago)
}

report_labels <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "未填写"
  x
}

report_count_table <- function(x, label = "分类") {
  values <- report_labels(x)
  counts <- sort(table(values), decreasing = TRUE)
  output <- data.frame(分类 = names(counts), 病例数 = as.integer(counts), check.names = FALSE)
  names(output)[1] <- label
  output$构成比 <- if (sum(output$病例数) > 0L) round(100 * output$病例数 / sum(output$病例数), 1) else numeric(nrow(output))
  output
}

report_aggregate_alerts <- function(alerts) {
  empty <- data.frame(疾病名称 = character(), 场所类型 = character(), 预警条数 = integer(),
                      最大聚集病例数 = integer(), check.names = FALSE)
  if (is.null(alerts)) return(list(evaluated = FALSE, count = NA_integer_, table = empty))
  if (!is.data.frame(alerts)) stop("outbreak_alerts 必须为聚集预警汇总表。", call. = FALSE)
  if (nrow(alerts) == 0L) return(list(evaluated = TRUE, count = 0L, table = empty))
  required <- c("疾病名称", "场所类型", "聚集病例数")
  if (!all(required %in% names(alerts))) stop("预警表缺少疾病名称、场所类型或聚集病例数。", call. = FALSE)
  safe <- data.frame(疾病名称 = report_labels(alerts$疾病名称), 场所类型 = report_labels(alerts$场所类型),
                     聚集病例数 = suppressWarnings(as.integer(alerts$聚集病例数)), check.names = FALSE)
  if (anyNA(safe$聚集病例数) || any(safe$聚集病例数 < 0L)) stop("预警病例数无效。", call. = FALSE)
  grouped <- split(seq_len(nrow(safe)), interaction(safe$疾病名称, safe$场所类型, drop = TRUE))
  combined <- do.call(rbind, lapply(grouped, function(rows) {
    data.frame(疾病名称 = safe$疾病名称[rows[1]], 场所类型 = safe$场所类型[rows[1]],
               预警条数 = length(rows), 最大聚集病例数 = max(safe$聚集病例数[rows]), check.names = FALSE)
  }))
  combined <- combined[order(-combined$预警条数, -combined$最大聚集病例数), , drop = FALSE]
  rownames(combined) <- NULL
  list(evaluated = TRUE, count = nrow(safe), table = combined)
}

# data 应为全日期、已清洗且按相同病种/地区范围筛选的数据；不要只传当前报告期数据。
# coverage_range 是使用者确认的完整数据覆盖区间，不能以最早/最晚病例日期代替。
# NULL diseases 表示全部；character(0) 表示明确选择空集。
build_report_payload <- function(data, date_range, report_type = "monthly", time_col = "发病日期",
                                 region_label = "监测地区", diseases = NULL, disease_col = "标准病种",
                                 districts = NULL, outbreak_alerts = NULL,
                                 outbreak_params = list(threshold = 3L, max_span_days = 14L),
                                 coverage_range = NULL, data_cutoff = NULL, topic_title = NULL,
                                 filter_note = NULL, data_quality = attr(data, "data_quality"),
                                 population_data = NULL, admin_map_data = NULL,
                                 outbreak_micro_alerts = NULL, include_place_names = TRUE) {
  if (!is.data.frame(data)) stop("data 必须为清洗后的数据框。", call. = FALSE)
  report_type <- report_type_value(report_type)
  period <- report_validate_range(date_range)
  if (report_type != "special" && !identical(unname(period), unname(report_period(report_type, period[1])))) {
    stop("月报须使用完整自然月，周报须使用周一至周日；自定义日期请使用专题分析。", call. = FALSE)
  }
  if (as.integer(diff(period)) > 3660L) stop("单份报告的统计范围不能超过十年。", call. = FALSE)
  if (!time_col %in% names(data)) stop("统计日期字段不存在：", time_col, "。", call. = FALSE)
  if (!disease_col %in% names(data)) stop("病种字段不存在：", disease_col, "。", call. = FALSE)
  if (!is.null(coverage_range)) coverage_range <- report_validate_range(coverage_range, "完整数据覆盖范围")
  if (!is.null(data_cutoff)) {
    data_cutoff <- report_date(data_cutoff, "数据提取截止日期")
    if (length(data_cutoff) != 1L || is.na(data_cutoff)) stop("数据提取截止日期无效。", call. = FALSE)
    if (!is.null(coverage_range) && data_cutoff < coverage_range[2]) {
      stop("确认的数据覆盖终点不能晚于数据提取截止日期。", call. = FALSE)
    }
  }
  filtered <- data
  if (!is.null(diseases)) filtered <- filtered[!is.na(filtered[[disease_col]]) & as.character(filtered[[disease_col]]) %in% diseases, , drop = FALSE]
  if (!is.null(districts)) {
    if (!"区县" %in% names(filtered)) stop("按区县筛选时需要区县字段。", call. = FALSE)
    filtered <- filtered[!is.na(filtered$区县) & as.character(filtered$区县) %in% districts, , drop = FALSE]
  }
  dates <- report_date(filtered[[time_col]], time_col)
  valid <- !is.na(dates)
  observed_range <- if (any(valid)) range(dates[valid]) else rep(as.Date(NA), 2L)
  in_period <- valid & dates >= period[1] & dates <= period[2]
  current <- filtered[in_period, , drop = FALSE]
  current_dates <- dates[in_period]
  column <- function(name) if (name %in% names(current)) current[[name]] else rep(NA_character_, nrow(current))
  confirmed <- function(range) !is.null(coverage_range) && coverage_range[1] <= range[1] && coverage_range[2] >= range[2]
  current_complete <- confirmed(period)
  comparators <- report_comparison_periods(period, report_type)
  current_count <- nrow(current)
  comparison <- do.call(rbind, lapply(seq_along(comparators), function(i) {
    compare_period <- comparators[[i]]
    covered <- confirmed(compare_period)
    base <- if (covered) sum(valid & dates >= compare_period[1] & dates <= compare_period[2]) else NA_integer_
    reason <- if (!current_complete) "本期完整覆盖未确认" else if (!covered) "比较期完整覆盖未确认" else if (base == 0L) "比较期为零 不计算百分比" else "可比较"
    rate <- if (current_complete && covered && base > 0L) 100 * (current_count - base) / base else NA_real_
    data.frame(比较方式 = if (i == 1L) "环比" else if (report_type == "weekly") "同比 前52周" else "同比",
               起始日期 = compare_period[1], 结束日期 = compare_period[2], 本期病例数 = current_count,
               比较期病例数 = base, 变化百分比 = rate, 说明 = reason, check.names = FALSE)
  }))
  day_sequence <- seq(period[1], period[2], by = "day")
  daily <- data.frame(日期 = day_sequence, 病例数 = as.integer(table(factor(current_dates, levels = day_sequence))), check.names = FALSE)
  table_disease <- report_count_table(column(disease_col), "病种")
  known_disease <- unique(report_labels(column(disease_col)))
  disease_count <- length(setdiff(known_disease, "未填写"))
  table_district <- report_count_table(column("区县"), "区县")
  known_district <- unique(report_labels(column("区县")))
  district_count <- length(setdiff(known_district, "未填写"))
  if (length(region_label) != 1L || is.na(region_label) || !nzchar(trimws(region_label))) region_label <- "监测地区"
  if (!is.null(topic_title)) topic_title <- substr(trimws(as.character(topic_title)[1L]), 1L, 80L)
  type_label <- c(monthly = "月报", weekly = "周报", special = "专题分析")[[report_type]]
  title <- if (report_type == "special" && !is.null(topic_title) && !is.na(topic_title) && nzchar(topic_title)) topic_title else paste0(region_label, "传染病疫情", type_label)
  alerts <- report_aggregate_alerts(outbreak_alerts)
  outbreak_params <- modifyList(list(threshold = 3L, max_span_days = 14L), outbreak_params)
  for (name in c("threshold", "max_span_days")) {
    value <- suppressWarnings(as.numeric(outbreak_params[[name]]))
    if (length(value) != 1L || !is.finite(value) || value < 1L || value != as.integer(value)) stop("预警参数应为正整数。", call. = FALSE)
    outbreak_params[[name]] <- as.integer(value)
  }
  alert_range <- if (!is.null(outbreak_params$date_range)) report_validate_range(outbreak_params$date_range, "预警日期范围") else period
  quality <- data.frame(项目 = c("当前筛选范围内输入记录", paste0(time_col, "缺失或无效排除"), "本期纳入记录"),
                        记录数 = c(nrow(filtered), sum(!valid), current_count), check.names = FALSE)
  import_quality <- NULL
  if (is.data.frame(data_quality) && all(c("项目", "记录数") %in% names(data_quality))) {
    import_quality <- data.frame(项目 = as.character(data_quality$项目), 记录数 = suppressWarnings(as.integer(data_quality$记录数)), check.names = FALSE)
    import_quality <- import_quality[!is.na(import_quality$记录数) & import_quality$记录数 >= 0, , drop = FALSE]
  }
  summary_text <- if (current_count > 0L) {
    paste0("本期已导入数据中纳入 ", format(current_count, big.mark = ","), " 条病例记录，涉及 ", disease_count,
           " 种已填写病种、", district_count, " 个已填写区县。",
           if (nrow(table_disease)) paste0("记录最多的病种为", table_disease$病种[1], "，共 ", table_disease$病例数[1], " 条，占本期的 ", sprintf("%.1f", table_disease$构成比[1]), "%。") else "")
  } else {
    paste0("本期在当前筛选条件和已导入数据中未检出病例记录。", if (!current_complete) "数据覆盖完整性尚未确认，不能据此认定实际零发病。" else "该结果仅反映当前数据与筛选口径。")
  }
  completeness_text <- if (current_complete) {
    paste0("使用者确认完整覆盖区间为 ", format(coverage_range[1]), " 至 ", format(coverage_range[2]), "，覆盖本期。")
  } else if (!is.null(coverage_range)) {
    paste0("使用者确认完整覆盖区间为 ", format(coverage_range[1]), " 至 ", format(coverage_range[2]), "，未完整覆盖本期。本期结果为已导入记录汇总。")
  } else "未确认导入数据的完整覆盖区间；空白日期仅表示导入数据中无记录，环比和同比暂不判断。"
  cutoff_text <- if (!is.null(data_cutoff)) paste0("数据提取截止日期为 ", format(data_cutoff), "。") else "数据提取截止日期未提供。"
  if (any(valid)) cutoff_text <- paste0(cutoff_text, "当前筛选范围内所选日期字段最晚为 ", format(observed_range[2]), "，它不等同于数据库提取截止日期。")
  result <- list(
    title = title, report_type = report_type, type_label = type_label,
    date_range = period, time_col = time_col, region_label = region_label, disease_col = disease_col,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    filter_note = if (is.null(filter_note) || !nzchar(filter_note[1])) "沿用生成时选择的管理级别、传播途径、病种及地区范围。" else as.character(filter_note)[1],
    disease_filter = if (is.null(diseases)) "当前范围内全部病种" else if (!length(diseases)) "空病种选择" else paste(diseases, collapse = "、"),
    coverage_range = coverage_range, current_complete = current_complete, data_cutoff = data_cutoff,
    observed_date_range = observed_range, completeness_text = completeness_text, cutoff_text = cutoff_text,
    total_cases = current_count, disease_count = disease_count, district_count = district_count,
    invalid_date_count = sum(!valid), summary_text = summary_text, comparison = comparison,
    daily = daily, disease_table = table_disease, district_table = table_district,
    sex_table = report_count_table(column("性别标准"), "性别"),
    age_table = report_count_table(column("年龄组"), "年龄组"),
    occupation_table = report_count_table(column("人群分类"), "人群分类"),
    outbreak = alerts, outbreak_params = outbreak_params, outbreak_date_range = alert_range,
    quality = quality, import_quality = import_quality,
    limitations = c("统计单位为清洗后病例记录；未提供人口分母，不计算发病率或患病率。病例数、构成比不代表人群发病风险。",
                    "数据更新、补报和订正会改变历史统计结果。缺失分类统一计为未填写，并保留在本期总数和构成比分母中。",
                    "预警是规则筛出的聚集线索，需要人工核实病例关联、场所名称和重复报告，不能直接认定暴发或用于自动处置。",
                    "报告只导出汇总，不包含姓名、卡片编号或详细地址；机构和子部门名称按报告选项呈现。")
  )
  class(result) <- c("surveillance_report", "list")
  result$district_filter_values <- districts
  enrich_report_payload(result, filtered, population_data = population_data,
    admin_map_data = admin_map_data, outbreak_alerts = outbreak_alerts,
    outbreak_micro_alerts = outbreak_micro_alerts, include_place_names = include_place_names)
}

find_report_soffice <- function() {
  configured <- Sys.getenv("SURVEILLANCE_SOFFICE", unset = "")
  candidates <- c(configured, unname(Sys.which(c("soffice", "libreoffice"))),
                  "/Applications/LibreOffice.app/Contents/MacOS/soffice",
                  "C:/Program Files/LibreOffice/program/soffice.exe",
                  "C:/Program Files (x86)/LibreOffice/program/soffice.exe")
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) normalizePath(candidates[1L], winslash = "/", mustWork = TRUE) else ""
}

report_export_capabilities <- function() {
  required <- c("officer", "flextable", "ggplot2", "ragg", "systemfonts")
  missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  soffice <- find_report_soffice()
  list(docx = length(missing) == 0L, pdf = length(missing) == 0L && nzchar(soffice),
       missing_packages = missing, soffice = soffice,
       pdf_message = if (nzchar(soffice)) "检测到 LibreOffice，可直接生成 PDF。" else "未检测到 LibreOffice。请安装 LibreOffice，或先下载 Word 并另存为 PDF。")
}

report_font_family <- function() {
  families <- unique(systemfonts::system_fonts()$family)
  # macOS 的 PingFang 系统字体可能使用不可由 FreeType/LibreOffice 读取的压缩格式。
  preferences <- c("Noto Sans CJK SC", "Microsoft YaHei", "Heiti SC", "SimHei", "WenQuanYi Zen Hei", "Songti SC", "Arial Unicode MS", "PingFang SC")
  matches <- preferences[preferences %in% families]
  if (length(matches)) matches[1L] else "sans"
}

report_top_rows <- function(table, max_rows = 12L) {
  if (nrow(table) <= max_rows) return(table)
  kept <- table[seq_len(max_rows - 1L), , drop = FALSE]
  rest <- table[seq.int(max_rows, nrow(table)), , drop = FALSE]
  other <- rest[1L, , drop = FALSE]
  other[1L, 1L] <- "其他合计"
  other$病例数 <- sum(rest$病例数)
  other$构成比 <- round(100 * other$病例数 / sum(table$病例数), 1L)
  rbind(kept, other)
}

# 图表展示前 N 位时显式标注，正文统计表保留完整分类。
report_chart_theme <- function(font) {
  ggplot2::theme_minimal(base_family = font, base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#333333"), legend.position = "bottom",
      plot.title = ggplot2::element_text(color = "black", size = 11, face = "bold"),
      plot.margin = ggplot2::margin(7, 18, 7, 5))
}
report_wrap <- function(x, width = 14L) vapply(as.character(x), function(s) {
  if (is.na(s)) return("未填写")
  starts <- seq.int(1L, max(1L, nchar(s)), by = width)
  paste(substring(s, starts, starts + width - 1L), collapse = "\n")
}, character(1))
report_bar_chart <- function(data, label, value = "病例数", font, top = 10L, xlab = "病例记录数", color = "#277B8C") {
  if (!nrow(data) || !all(c(label, value) %in% names(data))) return(NULL)
  d <- data[is.finite(data[[value]]), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- head(d[order(-d[[value]], as.character(d[[label]])), , drop = FALSE], top)
  if (!nrow(d)) return(NULL)
  d$label <- factor(as.character(d[[label]]), levels = rev(as.character(d[[label]])))
  d$value <- d[[value]]
  ggplot2::ggplot(d, ggplot2::aes(x = value, y = label)) +
    ggplot2::geom_col(fill = color, width = .65) +
    ggplot2::geom_text(ggplot2::aes(label = format(round(value, 2), trim = TRUE)), hjust = -.12, family = font, size = 3) +
    ggplot2::scale_y_discrete(labels = function(x) report_wrap(x, 17)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .18))) +
    ggplot2::labs(x = xlab, y = NULL) + report_chart_theme(font)
}
report_trend_chart <- function(data, font, date_col = "日期", group_col = NULL, top = 5L, date_label = "日期") {
  if (!nrow(data)) return(NULL)
  d <- data.frame(date = data[[date_col]], cases = data$病例数)
  if (!is.null(group_col)) {
    d$group <- as.character(data[[group_col]])
    totals <- sort(tapply(d$cases, d$group, sum), decreasing = TRUE)
    d <- d[d$group %in% head(names(totals), top), , drop = FALSE]
    d$group <- factor(d$group, levels = head(names(totals), top))
    if (length(unique(d$date)) == 1L) {
      bars <- aggregate(cases ~ group, d, sum)
      names(bars) <- c("分类", "病例数")
      return(report_bar_chart(bars, "分类", font = font, top = top))
    }
  }
  span <- as.integer(diff(range(d$date)))
  if (span > 366L && date_col == "日期") d$date <- as.Date(format(d$date, "%Y-%m-01"))
  else if (span > 62L && date_col == "日期") d$date <- report_analysis_week(d$date)
  if (is.null(group_col)) {
    d <- aggregate(cases ~ date, d, sum)
    p <- ggplot2::ggplot(d, ggplot2::aes(date, cases)) + ggplot2::geom_col(fill = "#277B8C", width = if (span > 366) 23 else if (span > 62) 5.6 else .8)
  } else {
    d <- aggregate(cases ~ date + group, d, sum)
    p <- ggplot2::ggplot(d, ggplot2::aes(date, cases, color = group, group = group)) +
      (if (length(unique(d$date)) > 1L) ggplot2::geom_line(linewidth = .6) else NULL) + ggplot2::geom_point(size = 1.6) +
      ggplot2::scale_color_manual(values = c("#277B8C", "#D28839", "#635DA0", "#A7494E", "#5B8050"), labels = function(x) report_wrap(x, 17)) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
  }
  p + ggplot2::scale_x_date(date_labels = if (span > 366) "%Y-%m" else "%m-%d", breaks = scales::breaks_pretty(n = 6)) +
    ggplot2::scale_y_continuous(limits = c(0, NA), breaks = function(x) unique(pmax(0, round(pretty(x, n = 4))))) +
    ggplot2::labs(x = if (date_col == "周起始") "周起始日期 周一" else if (span > 366) "按月汇总" else if (span > 62) "周起始日期 周一" else date_label, y = "病例记录数", color = NULL) + report_chart_theme(font)
}
report_make_chart <- function(payload, file, font) {
  chart <- report_trend_chart(payload$daily, font, date_label = payload$time_col)
  ggplot2::ggsave(file, chart, device = ragg::agg_png, width = 6.85, height = 2.5, dpi = 180, bg = "white")
}
report_add_paragraph <- function(doc, text, font, size = 10.5, bold = FALSE, style = "Normal", after = 6) {
  paragraph <- officer::fpar(officer::ftext(text, officer::fp_text(font.family = font, eastasia.family = font, font.size = size, bold = bold, color = "#000000")),
    fp_p = officer::fp_par(text.align = "left", padding.bottom = after, line_spacing = 1.18))
  officer::body_add_fpar(doc, paragraph, style = style)
}
report_add_heading <- function(doc, text, font) report_add_paragraph(doc, text, font, size = 13, bold = TRUE, style = "heading 1", after = 6)
report_add_table <- function(doc, data, font, widths = NULL, max_rows = NULL) {
  if (!nrow(data)) return(report_add_paragraph(doc, "当前范围内无可汇总记录。", font))
  # max_rows 仅为旧调用保留；完整报告不截断统计表。
  numeric_cols <- which(vapply(data, is.numeric, logical(1)))
  for (j in seq_along(data)) {
    if (inherits(data[[j]], "Date")) data[[j]] <- format(data[[j]], "%Y-%m-%d")
    else if (names(data)[j] == "构成比") data[[j]] <- sprintf("%.1f%%", data[[j]])
    else if (is.numeric(data[[j]])) data[[j]] <- format(round(data[[j]], 2), trim = TRUE, scientific = FALSE, big.mark = ",")
    data[[j]] <- as.character(data[[j]])
    data[[j]][is.na(data[[j]]) | data[[j]] %in% c("NA", "NaN")] <- "—"
  }
  table <- flextable::flextable(data)
  table <- flextable::font(table, fontname = font, part = "all")
  table <- flextable::fontsize(table, size = 9, part = "all")
  table <- flextable::bold(table, part = "header")
  table <- flextable::bg(table, bg = "#E6EEF3", part = "header")
  table <- flextable::color(table, color = "#000000", part = "all")
  table <- flextable::border_remove(table)
  for (border_fun in list(flextable::border_outer, flextable::border_inner_h, flextable::border_inner_v)) table <- border_fun(table, border = officer::fp_border(color = "#D9D9D9", width = .6), part = "all")
  table <- flextable::padding(table, padding.top = 4, padding.bottom = 4, padding.left = 6, padding.right = 6, part = "all")
  table <- flextable::valign(table, valign = "center", part = "all")
  table <- flextable::align(table, align = "left", part = "all")
  if (length(numeric_cols)) table <- flextable::align(table, j = numeric_cols, align = "right", part = "all")
  if (is.null(widths)) widths <- if (ncol(data) == 3L) c(4.15, 1.3, 1.4) else c(2.4, rep(4.45 / (ncol(data) - 1L), ncol(data) - 1L))
  table <- flextable::width(table, width = widths)
  table <- flextable::set_table_properties(table, layout = "fixed", opts_word = list(split = FALSE, repeat_headers = TRUE))
  table <- flextable::paginate(table, init = FALSE, hdr_ftr = TRUE)
  doc <- flextable::body_add_flextable(doc, value = table, align = "center")
  report_add_paragraph(doc, "", font, size = 2, after = 2)
}
report_compact_comparison <- function(d, label) {
  out <- d[, c(label, "本期病例数", "构成比", "环比病例数", "同比病例数"), drop = FALSE]
  for (kind in c("环比", "同比")) out[[paste0(kind, "变化")]] <- ifelse(is.na(d[[paste0(kind, "变化百分比")]]), "—", sprintf("%+.1f%%", d[[paste0(kind, "变化百分比")]]))
  names(out)[2] <- "本期例数"
  out[, c(1, 2, 3, 4, 6, 5, 7)]
}
report_top_sentence <- function(d, label, n = 3L) {
  d <- head(d[d$病例数 > 0, , drop = FALSE], n)
  if (!nrow(d)) return("无记录")
  paste(paste0(d[[label]], " ", d$病例数, " 例", if ("构成比" %in% names(d)) paste0("（", sprintf("%.1f", d$构成比), "%）") else ""), collapse = "；")
}
report_write_docx <- function(payload, file, work_dir) {
  font <- report_font_family()
  doc <- officer::read_docx()
  doc <- officer::docx_set_paragraph_style(doc, style_id = "Title", style_name = "Title", base_on = "Normal",
    fp_p = officer::fp_par(keep_with_next = TRUE), fp_t = officer::fp_text(font.family = font, eastasia.family = font, font.size = 21, bold = TRUE, color = "#000000"))
  heading_id <- doc$styles$style_id[doc$styles$style_name == "heading 1" & doc$styles$style_type == "paragraph"][1]
  doc <- officer::docx_set_paragraph_style(doc, style_id = heading_id, style_name = "heading 1", base_on = "Normal",
    fp_p = officer::fp_par(keep_with_next = TRUE), fp_t = officer::fp_text(font.family = font, eastasia.family = font, font.size = 13, bold = TRUE, color = "#000000"))
  doc <- officer::set_doc_properties(doc, title = payload$title, subject = "传染病疫情完整分析报告", creator = "传染病疫情数据分析与暴发探测", description = "自动生成的聚合统计报告，须经人工复核。")
  footer <- officer::fpar(officer::ftext("自动生成 供审核使用     ", officer::fp_text(font.family = font, eastasia.family = font, font.size = 8, color = "#666666")), officer::run_word_field("PAGE"), officer::ftext(" / ", officer::fp_text(font.size = 8)), officer::run_word_field("NUMPAGES"), fp_p = officer::fp_par(text.align = "right"))
  doc <- officer::body_set_default_section(doc, officer::prop_section(page_size = officer::page_size(width = 8.27, height = 11.69), page_margins = officer::page_mar(top = .7, bottom = .65, left = .7, right = .7, header = .25, footer = .3), footer_default = officer::block_list(footer)))
  figure <- 0L
  p <- function(text, ...) doc <<- report_add_paragraph(doc, text, font, ...)
  h <- function(text, new_page = FALSE) { if (new_page) doc <<- officer::body_add_break(doc); doc <<- report_add_heading(doc, text, font) }
  tab <- function(data, widths = NULL) doc <<- report_add_table(doc, data, font, widths)
  plot <- function(chart, caption, height = 2.5) {
    if (is.null(chart)) { p(paste0(caption, "：当前范围内无可绘制数据。"), size = 9); return(invisible(NULL)) }
    figure <<- figure + 1L
    path <- file.path(work_dir, paste0("figure-", figure, ".png"))
    ggplot2::ggsave(path, chart, device = ragg::agg_png, width = 6.85, height = height, units = "in", dpi = 180, bg = "white")
    p(paste0("图 ", figure, "  ", caption), size = 10, bold = TRUE, after = 2)
    doc <<- officer::body_add_img(doc, src = path, width = 6.85, height = height)
  }
  p(payload$title, size = 21, bold = TRUE, style = "Title", after = 10)
  p(paste0(format(payload$date_range[1]), " 至 ", format(payload$date_range[2]), "    按", payload$time_col, "统计"), size = 11)
  p(paste0("分析范围：", payload$region_label, "。", payload$filter_note, " 病种范围：", payload$disease_filter, "。"), size = 9.5)
  p(payload$summary_text)
  p(payload$completeness_text, size = 9.5)
  p(payload$cutoff_text, size = 9.5)
  h("一 疫情概况")
  peak <- payload$daily[payload$daily$病例数 == max(payload$daily$病例数), , drop = FALSE]
  metrics <- data.frame(指标 = c("本期病例记录", "已填写病种", "已填写区县", "日记录最高值", "机构聚集线索", "部门聚集与机构散发条目"),
    数值 = c(paste0(payload$total_cases, " 例"), paste0(payload$disease_count, " 种"), paste0(payload$district_count, " 个"), paste0(max(payload$daily$病例数), " 例"), if (payload$outbreak$evaluated) as.character(payload$outbreak$count) else "未评估", if (isTRUE(payload$outbreak_micro_evaluated)) as.character(payload$outbreak_micro_count) else "未评估"), check.names = FALSE)
  tab(metrics, c(4.7, 2.15))
  p(paste0("主要病种：", report_top_sentence(payload$disease_table, "病种"), "。"))
  p(paste0("记录较多的区县：", report_top_sentence(payload$district_table, "区县"), "。"))
  if (payload$total_cases > 0) p(paste0("日记录峰值为 ", max(payload$daily$病例数), " 例，最早出现在 ", format(peak$日期[1]), if (nrow(peak) > 1) paste0("，共 ", nrow(peak), " 天并列") else "", "。"))
  comparisons <- payload$comparison
  tab(data.frame(比较 = comparisons$比较方式, 比较期 = paste0(format(comparisons$起始日期), " 至 ", format(comparisons$结束日期)), 病例数 = ifelse(is.na(comparisons$比较期病例数), "未确认", comparisons$比较期病例数), 变化 = ifelse(is.na(comparisons$变化百分比), "不计算", sprintf("%+.1f%%", comparisons$变化百分比)), check.names = FALSE), c(.95, 3.3, 1.3, 1.3))
  p(paste0(paste(paste0(comparisons$比较方式, "：", comparisons$说明), collapse = "；"), "。比较按病例数计算，不作天数或人口标准化。"), size = 9)

  h("二 管理类别与传播途径", TRUE)
  p("以下构成比均以本期病例记录总数为分母，未填写类别单独保留。")
  plot(report_bar_chart(payload$management_table, "管理级别", font = font, top = 10), "管理类别分布", 1.7)
  tab(payload$management_table)
  plot(report_bar_chart(payload$route_table, "传播途径", font = font, top = 10), "传播途径分布", 1.9)
  tab(payload$route_table)

  h("三 病种构成与历史比较", TRUE)
  p(paste0("本期病种构成以 ", payload$total_cases, " 例为分母。下表包含本期或比较期出现的全部病种；图表展示本期前10位。"))
  plot(report_bar_chart(payload$disease_table, "病种", font = font), "本期病例数前10位病种", 2.6)
  tab(report_compact_comparison(payload$disease_comparison, "病种"), c(2.15, .72, .8, .8, .79, .8, .79))
  p("— 表示不满足计算条件。未确认完整覆盖时不推断历史零病例；确认比较期为零时不计算增长百分比。各比较期日期见疫情概况。", size = 9)
  if (nrow(payload$disease_rates)) {
    p(paste0("各病种", payload$rate_label), bold = TRUE)
    d <- payload$disease_rates[, c("病种", "病例数", "每10万人口记录数"), drop = FALSE]
    names(d)[3] <- "每10万人口"
    tab(d)
    p(payload$rate_note, size = 9)
  }

  h("四 时间分布", TRUE)
  plot(report_trend_chart(payload$daily, font, date_label = payload$time_col), "本期病例时间分布", 2.4)
  plot(report_trend_chart(payload$disease_weekly, font, "周起始", "病种"), if (length(unique(payload$disease_weekly$周起始)) == 1L) "前5位病种本周病例数" else "前5位病种周趋势", 2.4)
  p("周按周一至周日汇总，报告首末周可能仅含部分日期；曲线补零仅表示已导入数据中无记录。", size = 9)
  w <- payload$weekly[, c("统计起始", "统计结束", "病例数"), drop = FALSE]
  names(w) <- c("本报告实际纳入起日", "本报告实际纳入止日", "病例数")
  tab(w, c(2.7, 2.7, 1.45))
  if (length(unique(payload$comparison_curve$期间)) > 1L) {
    h("同期趋势对照", TRUE)
    d <- payload$comparison_curve
    chart <- ggplot2::ggplot(d, ggplot2::aes(期内天序, 病例数, color = 期间, group = 期间)) + ggplot2::geom_line(linewidth = .6) + ggplot2::geom_point(size = 1) + ggplot2::scale_color_manual(values = c("#277B8C", "#D28839", "#635DA0")) + ggplot2::labs(x = "各期内第几天", y = "病例记录数", color = NULL) + report_chart_theme(font)
    plot(chart, "确认完整覆盖的期间病例对照", 3.6)
    p("横轴按各期的相对天序对齐；自然月和闰年对应期间的天数可能不同，仅展示已确认完整覆盖的比较期。")
  }

  h("五 地区分布与人口比率", TRUE)
  plot(report_bar_chart(payload$district_table, "区县", font = font, top = 14), "各区县病例记录数 前14位", 3)
  plot(report_bar_chart(payload$district_rates, "区县", "每10万人口记录数", font, top = 14, xlab = payload$rate_label, color = "#627AA1"), paste0("各区县", payload$rate_label, " 前14位"), 3)
  p(payload$rate_note, size = 9)
  if (isTRUE(payload$population_summary$available)) p(paste0("当前有效地区人口参数合计 ", format(payload$population_summary$total_population, big.mark = ",", scientific = FALSE, trim = TRUE), " 人；本报告总体", payload$rate_label, "为 ", if (is.finite(payload$population_summary$total_rate)) sprintf("%.2f", payload$population_summary$total_rate) else "无法计算", "。"), size = 9)
  p(paste0("已匹配人口参数的病例 ", payload$population_summary$matched_cases, " 例；未匹配病例 ", payload$population_summary$unmatched_cases, " 例。病例数排序与人口比率排序的含义不同。"), size = 9)

  h("地区空间分布与完整明细", TRUE)
  if (!is.null(payload$map_sf) && nrow(payload$map_sf)) {
    chart <- ggplot2::ggplot(payload$map_sf) + ggplot2::geom_sf(ggplot2::aes(fill = 病例数), color = "white", linewidth = .2) +
      ggplot2::scale_fill_gradient(low = "#E9F3F4", high = "#1E6576", na.value = "#DDDDDD") + ggplot2::labs(fill = "病例数") + report_chart_theme(font) +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), legend.position = "right")
    plot(chart, "区县病例记录空间分布", 2.8)
    p("地图使用软件当前匹配的区县边界，仅作统计分布示意；灰色表示没有可匹配的汇总值。", size = 8.5)
  } else p("未载入与当前地区匹配的边界图层，本节提供完整地区表。")
  d <- payload$district_rates[, c("区县", "人口数", "病例数", "构成比", "每10万人口记录数", "人口匹配状态"), drop = FALSE]
  names(d)[5] <- "每10万人口"
  tab(d, c(1.5, 1.15, .7, .8, 1.15, 1.55))
  p(paste0("人口来源：", payload$population_summary$source, "。未匹配人口或人口参数存在冲突的地区不计算比率。"), size = 9)

  h("地区变化与病种分布", TRUE)
  plot(report_trend_chart(payload$district_weekly, font, "周起始", "区县"), if (length(unique(payload$district_weekly$周起始)) == 1L) "前5位区县本周病例数" else "病例数前5位区县周趋势", 2.5)
  tab(report_compact_comparison(payload$district_comparison, "区县"), c(1.65, .82, .86, .88, .88, .88, .88))
  p("地区同比环比使用与总览一致的历史期间和完整覆盖要求；不满足条件以 — 标记。", size = 9)
  if (nrow(payload$disease_district)) {
    h("病种与地区交叉分布", TRUE)
    d <- payload$disease_district
    ds <- head(payload$disease_table$病种, 10); rs <- head(payload$district_table$区县, 14)
    d <- d[d$病种 %in% ds & d$区县 %in% rs, , drop = FALSE]
    d$病种 <- factor(d$病种, levels = rev(ds)); d$区县 <- factor(d$区县, levels = rs)
    chart <- ggplot2::ggplot(d, ggplot2::aes(区县, 病种, fill = 病例数)) + ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = 病例数), family = font, size = 3, color = "#111111") +
      ggplot2::scale_fill_gradient(low = "#F1F7F8", high = "#7EB7C3") + ggplot2::scale_y_discrete(labels = function(x) report_wrap(x, 10)) +
      ggplot2::labs(x = NULL, y = NULL, fill = "病例数") + report_chart_theme(font) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    plot(chart, "前10位病种与前14位区县病例数交叉分布", 3.0)
    p("按本期病例数选择病种与区县。交叉分布用于定位病例构成，不能单凭颜色深浅判断传播关系。", size = 9)
    # 完整交叉表用长表呈现，仅列有病例组合；零组合在图中仍补零。
    cross <- payload$disease_district[payload$disease_district$病例数 > 0, c("病种", "区县", "病例数", "构成比", "每10万人口记录数"), drop = FALSE]
    names(cross)[4:5] <- c("本期占比", "每10万人口")
    cross$本期占比 <- sprintf("%.1f%%", cross$本期占比)
    tab(cross, c(2.3, 1.6, .8, 1, 1.15))
    p("完整交叉明细列出所有有病例的病种与区县组合；本期占比以本报告全部病例数为分母。", size = 9)
  }

  h("六 年龄与性别分布", TRUE)
  d <- payload$age_sex[payload$age_sex$性别 %in% c("男", "女"), , drop = FALSE]
  if (nrow(d)) {
    d$年龄组 <- factor(d$年龄组, levels = payload$age_levels)
    d$绘图例数 <- ifelse(d$性别 == "男", -d$病例数, d$病例数)
    chart <- ggplot2::ggplot(d, ggplot2::aes(绘图例数, 年龄组, fill = 性别)) + ggplot2::geom_col(width = .8) +
      ggplot2::scale_x_continuous(labels = abs) + ggplot2::scale_fill_manual(values = c("男" = "#277B8C", "女" = "#D28839")) +
      ggplot2::labs(x = "病例记录数 男左女右", y = "年龄组", fill = NULL) + report_chart_theme(font)
    plot(chart, "病例年龄与性别分布", 3.3)
  }
  p("此图是病例年龄性别分布图，非人口金字塔；未填写或其他性别不绘入左右两侧，但纳入下表及总数。", size = 9)
  ages <- payload$age_table[, c("年龄组", "病例数", "构成比"), drop = FALSE]
  # 按标签匹配，避免对 age_sex 的行排列作假设。
  for (sex in unique(payload$age_sex$性别)) { s <- payload$age_sex[payload$age_sex$性别 == sex, ]; ages[[sex]] <- s$病例数[match(ages$年龄组, s$年龄组)] }
  tab(ages, c(1.3, rep(5.55 / (ncol(ages) - 1L), ncol(ages) - 1L)))

  h("七 职业与人群构成", TRUE)
  p(paste0("性别分布：", report_top_sentence(payload$sex_table, "性别", nrow(payload$sex_table)), "。"))
  tab(payload$sex_table)
  plot(report_bar_chart(payload$occupation_table, "人群分类", font = font), "前10位职业与人群分类", 2.6)
  tab(payload$occupation_table)
  p("职业与人群分类以原始填报分类为基础；缺少对应分人群人口时，仅比较病例构成，不计算职业别发病率。", size = 9)

  if (length(payload$disease_profiles)) for (i in seq_along(payload$disease_profiles)) {
    profile <- payload$disease_profiles[[i]]
    h(paste0("八 重点病种分析 ", i, "  ", profile$name), TRUE)
    p(paste0("本期 ", profile$total, " 例，占本报告病例数 ", sprintf("%.1f", profile$share), "%。按本期病例数选取最多3个病种自动生成分析。"))
    plot(report_trend_chart(profile$daily, font, date_label = payload$time_col), paste0(profile$name, " 时间分布"), 2)
    plot(report_bar_chart(profile$district_table, "区县", font = font, top = 5), paste0(profile$name, " 前5位区县"), 1.8)
    p(paste0("地区分布：", report_top_sentence(profile$district_table, "区县"), "。"))
    sorted_age <- profile$age_table[order(-profile$age_table$病例数), ]
    p(paste0("主要年龄组：", report_top_sentence(sorted_age, "年龄组"), "。"))
    p(paste0("职业与人群：", report_top_sentence(profile$occupation_table, "人群分类"), "。"))
    p(paste0("性别分布：", report_top_sentence(profile$sex_table, "性别", nrow(profile$sex_table)), "。"))
    p("地区、年龄、职业和性别段落的构成比以该病种本期病例数为分母。可结合补报订正、就诊检测和现场信息解释变化，不能仅由描述性分布推断原因。", size = 9)
  }

  h("九 重点场所聚集探测", TRUE)
  p(paste0("探测期：", format(payload$outbreak_date_range[1]), " 至 ", format(payload$outbreak_date_range[2]), "，固定按发病日期。机构级线索 ", if (payload$outbreak$evaluated) payload$outbreak$count else "未评估", " 条；细分部门线索 ", if (isTRUE(payload$outbreak_micro_evaluated)) payload$outbreak_micro_count else "未评估", " 条。"))
  p(paste0("在同一场所来源、地区代理、规范化机构主体及同一原始疾病名称内，搜索连续 ", payload$outbreak_params$max_span_days, " 天闭区间；病例数达到 ", payload$outbreak_params$threshold, " 例触发线索。每组保留病例数最多的窗口，并列时取较早窗口。"))
  p("场所文本统一格式及常见错字，再按首个机构后缀拆分主体与班级科室；地区使用居住地代码或区县代理。细分探测在选定机构窗口内进行：有部门达阈值时仅列这些部门；所有部门均未达阈值时列为机构内散发。", size = 9.5)
  p("候选病例先限定在探测期内，窗口止日仍按起日加设定天数推算；超出探测期末日的部分未纳入观察，不代表已经完成整个窗口的随访。", size = 9)
  if (!payload$outbreak$evaluated) p("本次未运行或未提供机构探测结果。")
  else if (!payload$outbreak$count) p("本次未检出达到阈值的机构聚集线索。未触发规则不能排除实际暴发。")
  else {
    tab(payload$outbreak$table, c(2.5, 1.7, 1.2, 1.45))
    for (i in seq_len(nrow(payload$outbreak_details))) {
      d <- payload$outbreak_details[i, ]
      h(paste0("线索 ", d$线索编号, "  ", d$疾病名称))
      if (isTRUE(payload$include_place_names)) p(paste0("机构名称：", d$机构名称), bold = TRUE)
      p(paste0("场所类型：", d$场所类型, "；涉及区县：", d$涉及区县, "；聚集病例数：", d$聚集病例数, "。"))
      p(paste0("窗口：", d$预警窗口起, " 至 ", d$预警窗口止, "；实际最早至最晚发病：", d$最早发病日期, " 至 ", d$最晚发病日期, "；跨度 ", d$时间跨度天数, " 天。"), size = 9.5)
      p(paste0(if (isTRUE(payload$include_place_names)) "子部门名称及病例数：" else "子部门编号及病例数：", d$子部门分布, "。"), size = 9.5)
    }
  }
  if (isTRUE(payload$outbreak_micro_evaluated) && nrow(payload$outbreak_micro_details)) {
    h("班级科室及散发记录明细")
    d <- payload$outbreak_micro_details
    d$病例数 <- d$聚集病例数
    if (isTRUE(payload$include_place_names)) {
      cols <- c("线索编号", "机构名称", "子部门名称", "预警类型", "病例数", "子部门分布")
      tab(d[, cols, drop = FALSE], c(.65, 1.55, 1.2, 1.15, .6, 1.7))
      p("各行显示所属机构、具体子部门及病例分布；机构内散发条目在子部门分布中列出实际班级或科室及人数。线索编号关联上方机构窗口，机构与部门病例不能相加为受影响总人数。", size = 9)
    } else {
      cols <- c("线索编号", "部门编号", "预警类型", "病例数", "子部门分布")
      tab(d[, cols, drop = FALSE], c(.75, .8, 1.65, .65, 3))
      p("细分记录通过线索编号关联上方机构窗口，部门编号在同一机构内使用；机构与部门病例可能重叠，不可相加为受影响总人数。", size = 9)
    }
  }
  p(if (isTRUE(payload$include_place_names)) "本报告显示数据中识别出的机构和子部门名称；未填写或未能识别的名称不作推测。" else "本报告已选择匿名呈现，使用线索及部门编号，不导出机构名称、姓名、卡片号或详细地址。")
  p("预警属于筛查线索，须核查病例真实性、机构归并和流行病学关联后判断，不能直接认定暴发。")

  h("十 数据质量与分析口径", !isTRUE(payload$outbreak$evaluated) || payload$outbreak$count > 0L || nrow(payload$outbreak_micro_details) > 0L)
  tab(payload$quality, c(5.25, 1.6))
  if (!is.null(payload$import_quality) && nrow(payload$import_quality)) {
    p("导入阶段清洗汇总", bold = TRUE)
    tab(payload$import_quality, c(5.25, 1.6))
    p("导入质量对应整份文件；报告可能仅使用其中一部分。按顺序排除完全重复记录、旧版卡片、无效或删除卡片、无效发病日期及缺失疾病记录，各排除项互斥。", size = 9)
  }
  p(paste0("报告使用", payload$time_col, "筛选日期，缺失值不由其他日期替代。年龄按清洗后分组，区县按当前人口参数匹配。报告图表与表格采用同一筛选范围。"))
  p("月报为自然月，周报为周一至周日，专题为自定义闭区间。环比使用上一自然月或前一等长区间；周报同比为前52周，其他报告为上一年对应日期。")
  p("病种、地区和人群的全部分类均保留在统计表中，较多行时自动续页并重复表头。图中前N位筛选不改变报告分母。四舍五入可能使百分比之和不等于100%。", size = 9)

  h("十一 解读与核查建议", TRUE)
  for (limitation in payload$limitations) p(limitation)
  p(paste0("优先复核病例较多病种及地区的填报与订正情况：", report_top_sentence(payload$disease_table, "病种", 2), "；", report_top_sentence(payload$district_table, "区县", 2), "。"))
  if (payload$outbreak$evaluated && payload$outbreak$count > 0L) p("对达到阈值的机构及班级科室逐条核查发病时序、接触关系、重复记录与机构名称归并；补充现场调查和处置结果后再形成事件结论。")
  else p("继续结合常规监测与现场信息判断；本期未获得阳性聚集线索不等于不存在暴发。")
  p("当前数据未提供可核验的死亡、病原学检测、处置或报告时效资料时，报告不自动推算病死率、阳性率或处置达标率。可在人工审核后补充相应专题材料。")
  p(paste0("生成时间：", payload$generated_at, "。本文件为自动汇总初稿，请审核后使用。"), size = 9)
  print(doc, target = file)
  invisible(file)
}

# 部分 macOS 无界面 LibreOffice 的打包 fontconfig 路径失效。
# 转换时显式提供本机实际字体目录和私有缓存；不修改进程/系统的全局字体设置。
report_conversion_environment <- function(work_dir) {
  if (!identical(unname(Sys.info()["sysname"]), "Darwin")) return(character())
  font_dirs <- unique(dirname(systemfonts::system_fonts()$path))
  font_dirs <- font_dirs[dir.exists(font_dirs)]
  if (!length(font_dirs)) return(character())
  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  cache <- file.path(work_dir, "font-cache")
  dir.create(cache)
  config <- file.path(work_dir, "fonts.conf")
  contents <- c('<?xml version="1.0"?>', '<!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">',
                "<fontconfig>", paste0("<dir>", xml_escape(font_dirs), "</dir>"),
                paste0("<cachedir>", xml_escape(normalizePath(cache, winslash = "/")), "</cachedir>"), "</fontconfig>")
  writeLines(enc2utf8(contents), config, useBytes = TRUE)
  paste0("FONTCONFIG_FILE=", shQuote(normalizePath(config, winslash = "/")))
}

write_surveillance_report <- function(payload, file, format = c("docx", "pdf")) {
  format <- match.arg(tolower(format), c("docx", "pdf"))
  if (!inherits(payload, "surveillance_report")) stop("请先使用 build_report_payload() 生成报告内容。", call. = FALSE)
  if (length(file) != 1L || is.na(file) || !nzchar(file)) stop("请指定输出文件路径。", call. = FALSE)
  capabilities <- report_export_capabilities()
  if (!capabilities$docx) stop("报告缺少 R 包：", paste(capabilities$missing_packages, collapse = ", "), "。请先运行安装脚本。", call. = FALSE)
  if (format == "pdf" && !capabilities$pdf) stop(capabilities$pdf_message, call. = FALSE)
  work_dir <- tempfile("surveillance-report-")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  source_docx <- file.path(work_dir, "report.docx")
  report_write_docx(payload, source_docx, work_dir)
  output <- source_docx
  if (format == "pdf") {
    profile <- file.path(work_dir, "lo-profile")
    dir.create(profile)
    profile_uri <- paste0("file://", if (.Platform$OS.type == "windows") "/" else "", utils::URLencode(normalizePath(profile, winslash = "/"), reserved = TRUE))
    # LibreOffice requires URI separators, not percent-encoded path separators.
    profile_uri <- gsub("%2F", "/", profile_uri, fixed = TRUE)
    conversion <- suppressWarnings(system2(capabilities$soffice,
      args = c(shQuote(paste0("-env:UserInstallation=", profile_uri)), "--headless", "--convert-to", "pdf", "--outdir", shQuote(work_dir), shQuote(source_docx)),
      stdout = TRUE, stderr = TRUE, timeout = 120L, env = report_conversion_environment(work_dir)))
    output <- file.path(work_dir, "report.pdf")
    status <- attr(conversion, "status")
    if ((!is.null(status) && status != 0L) || !file.exists(output) || file.info(output)$size < 100L) {
      stop("LibreOffice PDF 转换失败。请检查安装与中文字体，或下载 Word 后另存为 PDF。", call. = FALSE)
    }
  }
  if (!dir.exists(dirname(file))) dir.create(dirname(file), recursive = TRUE)
  if (!file.copy(output, file, overwrite = TRUE)) stop("无法写入目标报告文件。", call. = FALSE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}
