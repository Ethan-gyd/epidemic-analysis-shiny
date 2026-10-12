# 重点场所规则聚集探测。使用发病日期；预警为待核实线索。
# 由 app.R 或测试加载 data_preprocessing.R 后再加载本文件。

empty_outbreak_alerts <- function() {
  tibble(
    场所类型 = character(),
    场所地区 = character(),
    机构预警ID = character(),
    core_institution = character(),
    疾病名称 = character(),
    聚集病例数 = integer(),
    时间跨度天数 = integer(),
    最早发病日期 = as.Date(character()),
    最晚发病日期 = as.Date(character()),
    预警窗口起 = as.Date(character()),
    预警窗口止 = as.Date(character()),
    涉及区县 = character(),
    子部门数 = integer(),
    子部门分布 = character(),
    暴发场所名称 = character()
  )
}

empty_outbreak_micro_alerts <- function() {
  tibble(
    场所类型 = character(),
    场所地区 = character(),
    机构预警ID = character(),
    core_institution = character(),
    sub_level = character(),
    疾病名称 = character(),
    预警类型 = character(),
    聚集病例数 = integer(),
    最早发病日期 = as.Date(character()),
    最晚发病日期 = as.Date(character()),
    涉及区县 = character(),
    子部门分布 = character()
  )
}

empty_outbreak_cards <- function() {
  tibble(
    场所类型 = character(),
    场所地区 = character(),
    机构预警ID = character(),
    core_institution = character(),
    sub_level = character(),
    场所原始名称 = character(),
    场所清洗文本 = character(),
    暴发场所名称 = character(),
    疾病名称 = character(),
    卡片ID = character(),
    卡片编号 = character(),
    病例分类 = character(),
    发病日期 = as.Date(character()),
    诊断时间 = as.POSIXct(character()),
    订正终审时间 = as.POSIXct(character()),
    报告卡录入时间 = as.POSIXct(character()),
    性别 = character(),
    年龄 = character(),
    年龄组 = character(),
    人群分类 = character(),
    区县 = character(),
    现住详细地址 = character(),
    现住地址国标 = character(),
    患者工作单位 = character()
  )
}

outbreak_institution_suffixes <- c(
  "托育机构", "培训机构", "幼儿园", "托儿所", "小学", "中学", "学校",
  "大学", "学院", "医院", "卫生院", "诊所", "养老院", "福利院", "敬老院",
  "监狱", "看守所", "拘留所", "工地", "工厂", "厂", "公司", "宿舍"
)

outbreak_institution_suffix_regex <- paste(
  outbreak_institution_suffixes[order(nchar(outbreak_institution_suffixes), decreasing = TRUE)],
  collapse = "|"
)

cn_number_map <- c(
  "二十" = "20", "十九" = "19", "十八" = "18", "十七" = "17", "十六" = "16",
  "十五" = "15", "十四" = "14", "十三" = "13", "十二" = "12", "十一" = "11",
  "十" = "10", "九" = "9", "八" = "8", "七" = "7", "六" = "6",
  "五" = "5", "四" = "4", "三" = "3", "二" = "2", "两" = "2",
  "一" = "1", "〇" = "0", "零" = "0"
)

standardize_cn_grade_numbers <- function(x) {
  number_units <- c("年级", "班", "组", "科", "室", "病区", "单元", "栋", "楼", "号")

  for (cn_num in names(cn_number_map)) {
    for (unit in number_units) {
      x <- str_replace_all(x, fixed(paste0(cn_num, unit)), paste0(cn_number_map[[cn_num]], unit))
    }
  }

  x
}

clean_outbreak_place_text <- function(x) {
  x %>%
    normalize_place_name() %>%
    replace_na("") %>%
    str_replace_all("幼儿殓园|幼儿圆|幼儿苑", "幼儿园") %>%
    str_replace_all("小学校", "小学") %>%
    standardize_cn_grade_numbers() %>%
    str_replace_all("[[:space:]]+", "") %>%
    str_replace_all("[,，;；:：。\\.、/\\\\\\-—_()（）\\[\\]【】{}<>《》\"'“”‘’]+", "") %>%
    str_squish()
}

split_outbreak_place_text <- function(x) {
  cleaned_text <- clean_outbreak_place_text(x)
  core <- str_extract(cleaned_text, paste0("^.*?(?:", outbreak_institution_suffix_regex, ")"))
  sub_level <- if_else(
    is.na(core),
    NA_character_,
    str_remove(cleaned_text, fixed(core))
  )
  sub_level <- if_else(is.na(sub_level) | sub_level == "", NA_character_, sub_level)

  admin_prefix_pattern <- paste0("云南省|昆明市|", paste(district_alias_mapping$区县, collapse = "|"))
  core_specific_part <- core %>%
    replace_na("") %>%
    str_remove(paste0("(?:", outbreak_institution_suffix_regex, ")$")) %>%
    str_replace_all(admin_prefix_pattern, "")

  tibble(
    场所清洗文本 = cleaned_text,
    core_institution = core,
    sub_level = sub_level,
    核心机构有效 = !is.na(core) & nchar(core_specific_part) >= 2
  )
}

make_outbreak_place_cards <- function(data, place_col, place_type, keyword_pattern) {
  if (!place_col %in% names(data) || nrow(data) == 0) {
    return(data %>% slice(0))
  }

  place_data <- data %>%
    filter(!is.na(.data[[place_col]]), .data[[place_col]] != "") %>%
    mutate(
      场所类型 = place_type,
      场所原始名称 = .data[[place_col]]
    )

  if (nrow(place_data) == 0) {
    return(place_data)
  }

  place_data %>%
    bind_cols(split_outbreak_place_text(place_data$场所原始名称)) %>%
    filter(
      .data$核心机构有效,
      str_detect(.data$场所清洗文本, regex(keyword_pattern, ignore_case = TRUE))
    )
}

build_outbreak_candidate_cards <- function(data, start_date, end_date, keyword_pattern = NULL, date_col = "发病日期") {
  if (!identical(date_col, "发病日期")) stop("聚集探测仅使用发病日期，不能混用诊断或订正日期。")
  if (length(start_date) != 1L || length(end_date) != 1L ||
      is.na(start_date) || is.na(end_date) || as.Date(start_date) > as.Date(end_date)) {
    stop("请设置有效的预警起止日期。")
  }
  if (nrow(data) == 0 || is.na(start_date) || is.na(end_date)) {
    return(empty_outbreak_cards())
  }

  if (is.null(keyword_pattern) || !nzchar(trimws(keyword_pattern))) {
    keyword_pattern <- paste(priority_place_keywords, collapse = "|")
  }
  tryCatch(str_detect("学校", regex(keyword_pattern)), error = function(e) stop("重点场所正则表达式无效，请检查括号等符号。"))

  recent_data <- data %>%
    mutate(
      窗口日期 = as.Date(.data$发病日期),
      场所地区 = coalesce(make_pac6(.data$现住地址国标), empty_to_na(.data$区县), "地区未知")
    ) %>%
    filter(
      !is.na(.data$窗口日期),
      .data$窗口日期 >= as.Date(start_date),
      .data$窗口日期 <= as.Date(end_date)
    )

  card_columns <- c(
    "场所类型", "场所地区", "机构预警ID", "core_institution", "sub_level", "场所原始名称",
    "场所清洗文本", "暴发场所名称", "疾病名称", "卡片ID", "卡片编号", "病例分类",
    "发病日期", "诊断时间", "订正终审时间", "报告卡录入时间", "性别", "年龄",
    "年龄组", "人群分类", "区县", "现住详细地址", "现住地址国标", "患者工作单位"
  )

  work_unit_cards <- make_outbreak_place_cards(
    recent_data,
    place_col = "工作单位标准",
    place_type = "患者工作单位",
    keyword_pattern = keyword_pattern
  )

  address_cards <- make_outbreak_place_cards(
    recent_data,
    place_col = "地址标准",
    place_type = "现住详细地址",
    keyword_pattern = keyword_pattern
  )

  bind_rows(work_unit_cards, address_cards) %>%
    mutate(暴发场所名称 = .data$core_institution) %>%
    select(any_of(card_columns))
}

summarise_best_outbreak_window <- function(group_data, max_span_days = 14) {
  group_data <- group_data %>%
    filter(!is.na(.data$发病日期)) %>%
    mutate(发病日期 = as.Date(.data$发病日期))

  if (nrow(group_data) == 0) {
    return(tibble())
  }

  span_days <- max(1, as.integer(max_span_days))
  start_candidates <- sort(unique(group_data$发病日期))
  window_counts <- vapply(
    start_candidates,
    function(window_start) {
      sum(group_data$发病日期 >= window_start & group_data$发病日期 <= window_start + days(span_days - 1))
    },
    integer(1)
  )
  best_start <- start_candidates[[which.max(window_counts)]]
  best_end <- best_start + days(span_days - 1)
  window_data <- group_data %>%
    filter(.data$发病日期 >= best_start, .data$发病日期 <= best_end)

  sub_distribution <- window_data %>%
    mutate(sub_level = replace_na(.data$sub_level, "未填写子部门")) %>%
    count(.data$sub_level, sort = TRUE, name = "病例数") %>%
    transmute(label = glue::glue("{sub_level}({病例数})")) %>%
    pull(.data$label) %>%
    paste(collapse = "；")

  tibble(
    聚集病例数 = nrow(window_data),
    时间跨度天数 = as.integer(max(window_data$发病日期) - min(window_data$发病日期)) + 1L,
    最早发病日期 = min(window_data$发病日期),
    最晚发病日期 = max(window_data$发病日期),
    预警窗口起 = best_start,
    预警窗口止 = best_end,
    涉及区县 = paste(sort(unique(na.omit(window_data$区县))), collapse = "、"),
    子部门数 = n_distinct(window_data$sub_level, na.rm = TRUE),
    子部门分布 = sub_distribution
  )
}

detect_outbreak_clusters <- function(data, end_date, lookback_days = 14, start_date = NULL, threshold = 3, keyword_pattern = NULL, date_col = "发病日期", max_span_days = 14) {
  if (length(threshold) != 1L || !is.finite(threshold) || threshold < 2 || threshold != as.integer(threshold) ||
      length(max_span_days) != 1L || !is.finite(max_span_days) || max_span_days < 1 || max_span_days != as.integer(max_span_days)) {
    stop("病例数阈值须为至少 2 的整数，时间跨度须为正整数。")
  }
  if (is.null(start_date)) {
    start_date <- as.Date(end_date) - days(as.integer(lookback_days) - 1)
  }

  candidate_cards <- build_outbreak_candidate_cards(
    data = data,
    start_date = start_date,
    end_date = end_date,
    keyword_pattern = keyword_pattern,
    date_col = date_col
  )

  if (nrow(candidate_cards) == 0) {
    return(empty_outbreak_alerts())
  }

  candidate_cards %>%
    group_by(.data$场所类型, .data$场所地区, .data$core_institution, .data$疾病名称) %>%
    group_modify(~ summarise_best_outbreak_window(.x, max_span_days = max_span_days)) %>%
    ungroup() %>%
    filter(.data$聚集病例数 >= threshold, .data$时间跨度天数 <= max_span_days) %>%
    mutate(
      机构预警ID = paste(.data$场所类型, .data$场所地区, .data$core_institution, .data$疾病名称, .data$预警窗口起, sep = "__"),
      暴发场所名称 = .data$core_institution
    ) %>%
    select(
      场所类型, 场所地区, 机构预警ID, core_institution, 疾病名称, 聚集病例数, 时间跨度天数,
      最早发病日期, 最晚发病日期, 预警窗口起, 预警窗口止, 涉及区县,
      子部门数, 子部门分布, 暴发场所名称
    ) %>%
    arrange(desc(.data$聚集病例数), desc(.data$最晚发病日期), .data$疾病名称)
}

filter_outbreak_cards_by_alerts <- function(candidate_cards, alerts) {
  if (nrow(candidate_cards) == 0 || nrow(alerts) == 0) {
    return(empty_outbreak_cards())
  }

  candidate_cards %>%
    inner_join(
      alerts %>% select(.data$场所类型, .data$场所地区, .data$机构预警ID, .data$core_institution, .data$疾病名称, .data$预警窗口起, .data$预警窗口止),
      by = c("场所类型", "场所地区", "core_institution", "疾病名称")
    ) %>%
    filter(
      .data$发病日期 >= .data$预警窗口起,
      .data$发病日期 <= .data$预警窗口止
    ) %>%
    mutate(暴发场所名称 = .data$core_institution) %>%
    select(-any_of(c("预警窗口起", "预警窗口止"))) %>%
    arrange(.data$场所类型, .data$core_institution, .data$疾病名称, .data$发病日期)
}

detect_outbreak_micro_alerts <- function(alert_cards, threshold = 3) {
  if (nrow(alert_cards) == 0) {
    return(empty_outbreak_micro_alerts())
  }

  sub_counts <- alert_cards %>%
    mutate(sub_level = replace_na(.data$sub_level, "未填写子部门")) %>%
    group_by(.data$场所类型, .data$场所地区, .data$机构预警ID, .data$core_institution, .data$sub_level, .data$疾病名称) %>%
    summarise(
      聚集病例数 = n(),
      最早发病日期 = min(.data$发病日期, na.rm = TRUE),
      最晚发病日期 = max(.data$发病日期, na.rm = TRUE),
      涉及区县 = paste(sort(unique(na.omit(.data$区县))), collapse = "、"),
      .groups = "drop"
    )

  class_alerts <- sub_counts %>%
    filter(.data$sub_level != "未填写子部门", .data$聚集病例数 >= threshold) %>%
    mutate(
      预警类型 = "班级/科室聚集",
      子部门分布 = glue::glue("{sub_level}({聚集病例数})")
    )

  scatter_alerts <- sub_counts %>%
    # 先保存各部门原始计数；summarise 内重新赋值聚集病例数后会遮蔽原列。
    mutate(部门病例标签 = glue::glue("{sub_level}({聚集病例数})")) %>%
    group_by(.data$场所类型, .data$场所地区, .data$机构预警ID, .data$core_institution, .data$疾病名称) %>%
    summarise(
      聚集病例数 = sum(.data$聚集病例数),
      最早发病日期 = min(.data$最早发病日期, na.rm = TRUE),
      最晚发病日期 = max(.data$最晚发病日期, na.rm = TRUE),
      涉及区县 = paste(sort(unique(na.omit(.data$涉及区县))), collapse = "、"),
      子部门分布 = paste(.data$部门病例标签, collapse = "；"),
      .groups = "drop"
    ) %>%
    anti_join(
      class_alerts %>% distinct(.data$场所类型, .data$场所地区, .data$机构预警ID, .data$core_institution, .data$疾病名称),
      by = c("场所类型", "场所地区", "机构预警ID", "core_institution", "疾病名称")
    ) %>%
    mutate(
      sub_level = "机构内散发",
      预警类型 = "机构级聚集，子部门未达阈值"
    )

  bind_rows(class_alerts, scatter_alerts) %>%
    select(
      场所类型, 场所地区, 机构预警ID, core_institution, sub_level, 疾病名称, 预警类型,
      聚集病例数, 最早发病日期, 最晚发病日期, 涉及区县, 子部门分布
    ) %>%
    arrange(.data$预警类型, desc(.data$聚集病例数), .data$core_institution)
}
