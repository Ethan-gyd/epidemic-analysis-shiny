# 昆明市传染病疫情自动化监测与分析系统
# 完整 Shiny App（shinydashboard）

suppressPackageStartupMessages({
  library(DT)
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(glue)
  library(ggspatial)
  library(htmltools)
  library(htmlwidgets)
  library(leaflet)
  library(lubridate)
  library(plotly)
  library(scales)
  library(shiny)
  library(shinydashboard)
  library(stringr)
  library(tidyr)
})

options(shiny.maxRequestSize = 100 * 1024^2)

if (isTRUE(capabilities("cairo"))) {
  options(shiny.usecairo = TRUE)
}

source("data_preprocessing.R", encoding = "UTF-8")
source("outbreak_detection.R", encoding = "UTF-8")
source("report_generation.R", encoding = "UTF-8")
source("report_module.R", encoding = "UTF-8")

surveillance_file <- Sys.getenv("SURVEILLANCE_DATA_FILE", unset = "data/demo_surveillance.csv")

if (!file.exists(surveillance_file)) {
  stop(glue("未找到监测数据文件: {surveillance_file}"))
}

default_surveillance_data <- load_surveillance_data(surveillance_file)
default_population_data <- normalize_population_data(km_population_2025, source_label = "内置2025昆明市人口")

send_loading_overlay <- function(session, action = "update", text = NULL, progress = NULL) {
  session$sendCustomMessage(
    "global-loading-overlay",
    list(
      action = action,
      text = text,
      progress = progress
    )
  )
}

set_analysis_progress <- function(session, value, detail) {
  setProgress(value = value, detail = detail)
  send_loading_overlay(session, action = "update", text = detail, progress = value)
}

time_filter_choices <- c(
  "发病时间" = "发病日期",
  "诊断时间" = "诊断时间",
  "订正终审时间" = "订正终审时间"
)

get_time_filter_dates <- function(data, time_col) {
  if (!time_col %in% names(data)) {
    return(rep(as.Date(NA), nrow(data)))
  }
  as.Date(data[[time_col]], tz = "Asia/Shanghai")
}

compute_default_date_range <- function(data, time_col = "发病日期") {
  date_values <- get_time_filter_dates(data, time_col)
  if (all(is.na(date_values))) date_values <- data$发病日期
  date_range <- range(date_values, na.rm = TRUE)

  if (as.numeric(diff(date_range)) > 120) {
    c(max(date_range[2] - days(90), date_range[1]), date_range[2])
  } else {
    date_range
  }
}

management_filter_levels <- disease_dictionary %>%
  pull(.data$管理级别) %>%
  as.character() %>%
  setdiff(c("未分类", "", NA_character_)) %>%
  unique()
management_filter_levels <- c("甲类", "乙类", "丙类", "重点监测", "补充监测")[
  c("甲类", "乙类", "丙类", "重点监测", "补充监测") %in% management_filter_levels
]
supplementary_disease_group <- "补充监测"
route_filter_levels <- disease_dictionary %>%
  pull("主要传播途径") %>%
  as.character() %>%
  setdiff(c("未分类", "", NA_character_)) %>%
  unique()
route_filter_levels <- c(
  "呼吸道传染病",
  "消化道传染病",
  "自然疫源地及虫媒传染病",
  "血源及性传播传染病",
  "分亚型对应"
)[c(
  "呼吸道传染病",
  "消化道传染病",
  "自然疫源地及虫媒传染病",
  "血源及性传播传染病",
  "分亚型对应"
) %in% route_filter_levels]

build_standard_disease_choices <- function(data, selected_management = NULL, selected_routes = NULL) {
  dictionary_choices <- disease_dictionary %>%
    mutate(字典顺序 = row_number()) %>%
    transmute(
      标准病种 = .data$标准病种,
      管理级别 = .data$管理级别,
      下拉分组 = if_else(.data$管理级别 %in% management_filter_levels, .data$管理级别, supplementary_disease_group),
      传播途径 = .data$主要传播途径,
      字典顺序 = .data$字典顺序
    )

  supplementary_choices <- data %>%
    filter(!is.na(.data$标准病种), .data$标准病种 != "") %>%
    distinct(.data$标准病种, .data$管理级别, .data$传播途径) %>%
    anti_join(disease_dictionary %>% distinct(.data$标准病种), by = "标准病种") %>%
    mutate(
      管理级别 = supplementary_disease_group,
      下拉分组 = supplementary_disease_group,
      字典顺序 = row_number() + nrow(dictionary_choices)
    )

  disease_choices <- bind_rows(dictionary_choices, supplementary_choices) %>%
    mutate(
      管理级别 = as.character(.data$管理级别),
      下拉分组 = as.character(.data$下拉分组),
      传播途径 = as.character(.data$传播途径)
    )

  if (!is.null(selected_management)) {
    disease_choices <- if (length(selected_management) == 0) {
      disease_choices %>% slice(0)
    } else {
      disease_choices %>% filter(.data$下拉分组 %in% selected_management)
    }
  }

  if (!is.null(selected_routes)) {
    disease_choices <- if (length(selected_routes) == 0) {
      disease_choices %>% slice(0)
    } else {
      disease_choices %>%
        filter(
          .data$传播途径 %in% selected_routes |
            (.data$标准病种 == "病毒性肝炎" & any(selected_routes %in% c("消化道传染病", "血源及性传播传染病")))
        )
    }
  }

  disease_group_levels <- unique(c(management_filter_levels, supplementary_disease_group))

  grouped_choices <- disease_choices %>%
    arrange(factor(.data$下拉分组, levels = disease_group_levels), .data$字典顺序, .data$标准病种) %>%
    distinct(.data$下拉分组, .data$标准病种, .keep_all = TRUE) %>%
    split(.$下拉分组) %>%
    purrr::map(~ .x$标准病种)

  grouped_choices <- grouped_choices[disease_group_levels[disease_group_levels %in% names(grouped_choices)]]

  list(
    grouped = grouped_choices,
    all_values = disease_choices$标准病种 %>% unique()
  )
}

build_management_choices <- function(data) {
  management_filter_levels
}

build_route_choices <- function(data) {
  route_filter_levels
}

available_date_range <- range(default_surveillance_data$发病日期, na.rm = TRUE)
default_date_range <- compute_default_date_range(default_surveillance_data)
standard_disease_choice_info <- build_standard_disease_choices(default_surveillance_data)
standard_disease_choices <- standard_disease_choice_info$grouped
all_standard_disease_values <- standard_disease_choice_info$all_values
management_filter_choices <- build_management_choices(default_surveillance_data)
route_filter_choices <- build_route_choices(default_surveillance_data)
default_population_units <- population_analysis_units(default_population_data)

plot_colors <- c(
  "主色" = "#0F7C82",
  "辅色" = "#2A9D8F",
  "强调" = "#E76F51",
  "暖色" = "#F4A261",
  "深蓝" = "#264653",
  "金色" = "#E9C46A"
)

detect_chinese_font <- function() {
  candidates <- c(
    "Heiti SC", "Noto Sans CJK SC", "Microsoft YaHei", "SimHei",
    "Songti SC", "PingFang SC", "Hiragino Sans GB", "WenQuanYi Micro Hei"
  )

  if (requireNamespace("systemfonts", quietly = TRUE)) {
    available_fonts <- unique(systemfonts::system_fonts()$family)
    matched_font <- candidates[candidates %in% available_fonts][1]

    if (!is.na(matched_font)) {
      return(matched_font)
    }
  }

  "sans"
}

km_font_family <- detect_chinese_font()

build_leaflet_palette <- function(values) {
  valid_values <- values[is.finite(values)]

  if (length(valid_values) == 0) {
    return(colorNumeric(
      palette = c("#EAF4F4", "#0F7C82"),
      domain = c(0, 1),
      na.color = "#F2F5F7"
    ))
  }

  domain_range <- range(valid_values, na.rm = TRUE)

  if (domain_range[1] == domain_range[2]) {
    domain_range[2] <- domain_range[2] + 1
  }

  colorNumeric(
    palette = c("#EAF4F4", "#A7D9D5", "#58B7AE", "#0F7C82", "#0B3C49"),
    domain = domain_range,
    na.color = "#F2F5F7"
  )
}

get_map_metric_column <- function(metric_key) {
  if (identical(metric_key, "count")) "病例数" else "发病率"
}

get_map_metric_label <- function(metric_key) {
  if (identical(metric_key, "count")) "发病数" else "发病率（1/10万）"
}

format_map_metric_value <- function(values, metric_col) {
  if (identical(metric_col, "病例数")) {
    comma(values, accuracy = 1)
  } else {
    number(values, accuracy = 0.01)
  }
}

choose_scale_km <- function(map_bbox) {
  center_lat <- mean(c(map_bbox[["ymin"]], map_bbox[["ymax"]]), na.rm = TRUE)
  map_width_km <- as.numeric(map_bbox[["xmax"]] - map_bbox[["xmin"]]) * 111.32 * cos(center_lat * pi / 180)
  candidates <- c(1, 2, 5, 10, 20, 40, 50, 100, 200, 500, 1000)
  target <- max(map_width_km * 0.18, 1, na.rm = TRUE)
  selected <- max(candidates[candidates <= target], na.rm = TRUE)

  if (!is.finite(selected)) {
    selected <- min(candidates)
  }

  selected
}

build_map_guides <- function(map_sf) {
  map_bbox <- sf::st_bbox(map_sf)
  map_width <- as.numeric(map_bbox[["xmax"]] - map_bbox[["xmin"]])
  map_height <- as.numeric(map_bbox[["ymax"]] - map_bbox[["ymin"]])
  center_lat <- mean(c(map_bbox[["ymin"]], map_bbox[["ymax"]]), na.rm = TRUE)
  scale_km <- choose_scale_km(map_bbox)
  scale_degree_width <- scale_km / (111.32 * cos(center_lat * pi / 180))
  plot_xmin <- as.numeric(map_bbox[["xmin"]]) - map_width * 0.04
  plot_xmax <- as.numeric(map_bbox[["xmax"]]) + map_width * 0.26
  plot_ymin <- as.numeric(map_bbox[["ymin"]]) - map_height * 0.16
  plot_ymax <- as.numeric(map_bbox[["ymax"]]) + map_height * 0.06

  list(
    scale_km = scale_km,
    scale_x0 = as.numeric(map_bbox[["xmin"]]) + map_width * 0.02,
    scale_x1 = as.numeric(map_bbox[["xmin"]]) + map_width * 0.02 + scale_degree_width,
    scale_y = as.numeric(map_bbox[["ymin"]]) - map_height * 0.08,
    scale_tick_height = map_height * 0.018,
    north_x = as.numeric(map_bbox[["xmax"]]) + map_width * 0.10,
    north_y = as.numeric(map_bbox[["ymax"]]) - map_height * 0.04,
    north_marker_y = as.numeric(map_bbox[["ymax"]]) - map_height * 0.10,
    x_range = c(plot_xmin, plot_xmax),
    y_range = c(plot_ymin, plot_ymax)
  )
}

sf_feature_plotly_coords <- function(sf_row) {
  coord_df <- sf::st_coordinates(sf::st_geometry(sf_row)) %>%
    as.data.frame()
  split_cols <- intersect(c("L1", "L2", "L3"), names(coord_df))

  if (length(split_cols) == 0) {
    return(list(x = coord_df$X, y = coord_df$Y))
  }

  parts <- split(coord_df, interaction(coord_df[split_cols], drop = TRUE))

  list(
    x = unlist(lapply(parts, function(part) c(part$X, NA_real_)), use.names = FALSE),
    y = unlist(lapply(parts, function(part) c(part$Y, NA_real_)), use.names = FALSE)
  )
}

build_map_label_annotations <- function(label_df) {
  if (nrow(label_df) == 0) {
    return(list())
  }

  purrr::pmap(
    label_df,
    function(区县, X, Y, ...) {
      list(
        x = X,
        y = Y,
        text = 区县,
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        font = list(family = km_font_family, size = 10.5, color = "#183642")
      )
    }
  )
}

theme_km <- function() {
  theme_minimal(base_family = km_font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 14, colour = "#183642"),
      plot.subtitle = element_text(size = 11, colour = "#61707D"),
      axis.title = element_text(face = "bold", colour = "#183642"),
      axis.text = element_text(colour = "#425466"),
      legend.title = element_blank(),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#E6EDF2"),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

format_top_items <- function(data, name_col, n = 3) {
  if (nrow(data) == 0) {
    return("暂无数据")
  }

  top_df <- data %>%
    count({{ name_col }}, sort = TRUE) %>%
    filter(!is.na({{ name_col }}), {{ name_col }} != "") %>%
    slice_head(n = n)

  if (nrow(top_df) == 0) {
    return("暂无数据")
  }

  top_df %>%
    transmute(label = glue("{.data[[names(top_df)[1]]]}（{comma(n)}例）")) %>%
    pull(label) %>%
    paste(collapse = "、")
}

overview_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "overview",
    fluidRow(
      valueBoxOutput(ns("total_cases"), width = 4),
      valueBoxOutput(ns("disease_count"), width = 4),
      valueBoxOutput(ns("district_count"), width = 4)
    ),
    fluidRow(
      box(
        width = 12,
        title = "常规疫情摘要",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("summary_text"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "三间分布 - 时间",
        status = "primary",
        solidHeader = TRUE,
        radioButtons(
          ns("curve_unit"),
          "统计粒度",
          choices = c("按日" = "day", "按周" = "week"),
          selected = "day",
          inline = TRUE
        ),
        plotlyOutput(ns("epi_curve"), height = 340)
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "疾病病种发病数排名",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("disease_rank_plot"), height = 420)
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "三间分布 - 人群分类",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("occupation_plot"), height = 360)
      ),
      box(
        width = 6,
        title = "性别/年龄组人口金字塔",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("pyramid_plot"), height = 360)
      )
    )
  )
}

regional_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "regional",
    fluidRow(
      box(
        width = 4,
        title = "重点区县设置",
        status = "primary",
        solidHeader = TRUE,
        selectizeInput(
          ns("focus_districts"),
          "选择重点地区（留空表示全部）",
          choices = default_population_units$地区名称,
          selected = default_population_units$地区名称,
          multiple = TRUE
        ),
        radioButtons(
          ns("trend_unit"),
          "时间粒度",
          choices = c("按周" = "week", "按日" = "day"),
          selected = "week",
          inline = TRUE
        ),
        radioButtons(
          ns("map_metric"),
          "统计口径",
          choices = c("发病率" = "rate", "发病数" = "count"),
          selected = "rate",
          inline = TRUE
        ),
        p(
          class = "module-note",
          "发病率按 10 万人口计算；重点地区留空时默认展示全部地区。"
        )
      ),
      box(
        width = 8,
        title = "地区疫情地图",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("map_note")),
        plotlyOutput(ns("region_map"), height = 520)
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "各区县统计指标排名",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("rank_plot"), height = 360)
      ),
      box(
        width = 6,
        title = "重点区县统计指标趋势",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("trend_plot"), height = 360)
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "各区县发病数与发病率明细",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("region_table"))
      )
    )
  )
}

outbreak_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "outbreak",
    fluidRow(
      box(
        width = 4,
        title = "预警参数",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("alert_date_range_ui")),
        numericInput(
          ns("threshold"),
          "聚集判定阈值（同病种病例数）",
          value = 3,
          min = 2,
          max = 20,
          step = 1
        ),
        numericInput(
          ns("max_span_days"),
          "最长发病时间跨度（天）",
          value = 14,
          min = 3,
          max = 30,
          step = 1
        ),
        textInput(
          ns("keywords"),
          "重点场所关键字（正则表达式）",
          value = paste(priority_place_keywords, collapse = "|")
        ),
        p(
          class = "module-note",
          "系统会先提取核心机构，再识别班级、科室、病区等子部门；机构级达到阈值后，再判断是否存在班级/科室级聚集。"
        )
      ),
      valueBoxOutput(ns("alert_events"), width = 4),
      valueBoxOutput(ns("alert_places"), width = 4)
    ),
    fluidRow(
      box(
        width = 12,
        title = "预警窗口说明",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("window_note"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "机构级疑似聚集性疫情预警表",
        status = "danger",
        solidHeader = TRUE,
        downloadButton(ns("download_alert_cards"), "导出预警场所卡片明细 CSV"),
        br(),
        br(),
        DTOutput(ns("alert_table"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "班级/科室级预警与机构内散发提示",
        status = "danger",
        solidHeader = TRUE,
        DTOutput(ns("micro_alert_table"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "预警场所病例卡片明细",
        status = "warning",
        solidHeader = TRUE,
        DTOutput(ns("alert_card_table"))
      )
    )
  )
}

overview_server <- function(id, filtered_data, population_data) {
  moduleServer(id, function(input, output, session) {
    summary_data <- reactive({
      filtered_data()
    })

    output$total_cases <- renderValueBox({
      df <- summary_data()
      valueBox(
        value = comma(nrow(df)),
        subtitle = "选定时间段总病例数",
        icon = icon("medkit"),
        color = "aqua"
      )
    })

    output$disease_count <- renderValueBox({
      df <- summary_data()
      valueBox(
        value = comma(n_distinct(df$标准病种[!is.na(df$标准病种)])),
        subtitle = "涉及标准病种数",
        icon = icon("th-list"),
        color = "green"
      )
    })

    output$district_count <- renderValueBox({
      df <- summary_data()
      valueBox(
        value = comma(n_distinct(df$区县[!is.na(df$区县)])),
        subtitle = "涉及区县数",
        icon = icon("map-marker"),
        color = "yellow"
      )
    })

    output$summary_text <- renderUI({
      df <- summary_data()

      validate(need(nrow(df) > 0, "当前筛选条件下暂无病例数据。"))

      period_start <- min(df$筛选日期, na.rm = TRUE)
      period_end <- max(df$筛选日期, na.rm = TRUE)
      top_disease_text <- format_top_items(df, 标准病种, n = 5)
      top_occupation_text <- format_top_items(df, 人群分类, n = 5)
      top_district_text <- format_top_items(df %>% filter(!is.na(区县)), 区县, n = 5)
      top_route_text <- format_top_items(df, 传播途径, n = 3)
      top_rate_text <- df %>%
        filter(!is.na(.data$区县)) %>%
        count(区县, name = "病例数") %>%
        left_join(
          population_analysis_units(population_data()) %>%
            rename(区县 = .data$地区名称),
          by = "区县"
        ) %>%
        filter(!is.na(.data$人口数), .data$人口数 > 0) %>%
        mutate(发病率 = .data$病例数 / .data$人口数 * 100000) %>%
        arrange(desc(.data$发病率), desc(.data$病例数)) %>%
        slice_head(n = 5) %>%
        transmute(label = glue("{区县}（{number(发病率, accuracy = 0.01)}/10万）")) %>%
        pull(.data$label) %>%
        paste(collapse = "、")

      if (!nzchar(top_rate_text)) {
        top_rate_text <- "暂无数据"
      }

      tags$div(
        class = "info-note",
        tags$p(
          HTML(glue(
            "<b>按当前时间口径统计，{format(period_start, '%Y-%m-%d')} 至 {format(period_end, '%Y-%m-%d')}</b> ",
            "共报告 <b>{comma(nrow(df))}</b> 例病例，涉及 <b>{n_distinct(df$标准病种[!is.na(df$标准病种)])}</b> 种标准病种。"
          ))
        ),
        tags$p(HTML(glue("<b>标准病种构成前 5 位：</b>{top_disease_text}。"))),
        tags$p(HTML(glue("<b>重点人群前 5 位：</b>{top_occupation_text}。"))),
        tags$p(HTML(glue("<b>病例报告前 5 位区县：</b>{top_district_text}。"))),
        tags$p(HTML(glue("<b>发病率前 5 位区县：</b>{top_rate_text}。"))),
        tags$p(HTML(glue("<b>传播途径构成：</b>{top_route_text}。")))
      )
    })

    output$epi_curve <- renderPlotly({
      df <- summary_data()

      validate(need(nrow(df) > 0, "当前筛选条件下暂无病例数据。"))

      curve_df <- df %>%
        mutate(
          时间点 = if (input$curve_unit == "week") .data$筛选周 else .data$筛选日期
        ) %>%
        count(.data$时间点, name = "病例数") %>%
        arrange(.data$时间点)

      x_title <- if (input$curve_unit == "week") "统计周" else "统计日期"

      plot_ly(
        data = curve_df,
        x = ~时间点,
        y = ~病例数,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = plot_colors[["主色"]], width = 3),
        marker = list(color = plot_colors[["暖色"]], size = 7),
        hovertemplate = paste0(
          "<b>", x_title, ":</b> %{x}<br>",
          "<b>病例数:</b> %{y}<extra></extra>"
        )
      ) %>%
        layout(
          margin = list(l = 60, r = 20, b = 50, t = 20),
          xaxis = list(title = x_title, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(title = "病例数", showgrid = TRUE, gridcolor = "#E6EDF2", zeroline = FALSE),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          hovermode = "x unified"
        )
    })

    output$disease_rank_plot <- renderPlotly({
      df <- summary_data()

      validate(need(nrow(df) > 0, "当前筛选条件下暂无病例数据。"))

      disease_df <- df %>%
        filter(!is.na(.data$标准病种), .data$标准病种 != "") %>%
        count(标准病种, sort = TRUE, name = "发病数") %>%
        mutate(
          构成比 = .data$发病数 / sum(.data$发病数),
          标准病种 = fct_reorder(.data$标准病种, .data$发病数)
        )

      validate(need(nrow(disease_df) > 0, "当前筛选条件下暂无可展示的病种数据。"))

      plot_ly(
        data = disease_df,
        x = ~发病数,
        y = ~标准病种,
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~发病数,
          colorscale = list(
            list(0, "#D8EEF0"),
            list(0.45, "#58B7AE"),
            list(1, "#0B3C49")
          ),
          showscale = FALSE
        ),
        text = ~comma(发病数),
        textposition = "outside",
        customdata = ~percent(构成比, accuracy = 0.01),
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "发病数: %{x}例<br>",
          "构成比: %{customdata}<extra></extra>"
        )
      ) %>%
        layout(
          margin = list(l = 150, r = 40, b = 45, t = 20),
          xaxis = list(title = "发病数", showgrid = TRUE, gridcolor = "#E6EDF2", zeroline = FALSE),
          yaxis = list(title = NULL, showgrid = FALSE),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$occupation_plot <- renderPlotly({
      df <- summary_data()

      validate(need(nrow(df) > 0, "当前筛选条件下暂无病例数据。"))

      occupation_df <- df %>%
        count(人群分类, sort = TRUE, name = "病例数") %>%
        filter(!is.na(.data$人群分类), .data$人群分类 != "") %>%
        slice_head(n = 10) %>%
        arrange(.data$病例数)

      plot_ly(
        data = occupation_df,
        x = ~病例数,
        y = ~人群分类,
        type = "bar",
        orientation = "h",
        marker = list(color = plot_colors[["主色"]]),
        text = ~comma(病例数),
        textposition = "outside",
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "病例数: %{x}例<extra></extra>"
        )
      ) %>%
        layout(
          margin = list(l = 90, r = 35, b = 45, t = 20),
          xaxis = list(title = "病例数", showgrid = TRUE, gridcolor = "#E6EDF2", zeroline = FALSE),
          yaxis = list(
            title = NULL,
            showgrid = FALSE,
            categoryorder = "array",
            categoryarray = occupation_df$人群分类
          ),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$pyramid_plot <- renderPlotly({
      df <- summary_data()

      validate(need(nrow(df) > 0, "当前筛选条件下暂无病例数据。"))

      pyramid_df <- df %>%
        filter(!is.na(.data$年龄组), .data$性别标准 %in% c("男", "女")) %>%
        count(年龄组, 性别标准, name = "病例数") %>%
        mutate(
          年龄组 = factor(.data$年龄组, levels = levels(df$年龄组), ordered = TRUE),
          绘图值 = if_else(.data$性别标准 == "男", -.data$病例数, .data$病例数)
        )

      validate(need(nrow(pyramid_df) > 0, "当前筛选条件下暂无可用于人口金字塔的性别/年龄数据。"))

      age_levels <- levels(df$年龄组)
      max_cases <- max(pyramid_df$病例数, na.rm = TRUE)
      axis_limit <- max(pretty(c(0, max_cases)), na.rm = TRUE)
      tick_values <- pretty(c(-axis_limit, axis_limit), n = 5)

      male_df <- pyramid_df %>%
        filter(.data$性别标准 == "男")
      female_df <- pyramid_df %>%
        filter(.data$性别标准 == "女")

      plot_ly() %>%
        add_bars(
          data = male_df,
          x = ~绘图值,
          y = ~年龄组,
          orientation = "h",
          name = "男",
          marker = list(color = plot_colors[["深蓝"]]),
          customdata = ~病例数,
          hovertemplate = paste0(
            "<b>%{y}</b><br>",
            "性别: 男<br>",
            "病例数: %{customdata}例<extra></extra>"
          )
        ) %>%
        add_bars(
          data = female_df,
          x = ~绘图值,
          y = ~年龄组,
          orientation = "h",
          name = "女",
          marker = list(color = plot_colors[["强调"]]),
          customdata = ~病例数,
          hovertemplate = paste0(
            "<b>%{y}</b><br>",
            "性别: 女<br>",
            "病例数: %{customdata}例<extra></extra>"
          )
        ) %>%
        layout(
          barmode = "relative",
          margin = list(l = 75, r = 25, b = 45, t = 20),
          xaxis = list(
            title = "病例数",
            tickvals = tick_values,
            ticktext = comma(abs(tick_values)),
            showgrid = TRUE,
            gridcolor = "#E6EDF2",
            zeroline = TRUE,
            zerolinecolor = "#B8C7D3"
          ),
          yaxis = list(
            title = NULL,
            categoryorder = "array",
            categoryarray = age_levels,
            showgrid = FALSE
          ),
          legend = list(orientation = "h", x = 0, y = 1.12),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })
  })
}

regional_server <- function(id, filtered_data, population_data, admin_map_data, area_label) {
  moduleServer(id, function(input, output, session) {
    district_summary <- reactive({
      units <- population_analysis_units(population_data())

      filtered_data() %>%
        filter(!is.na(.data$区县)) %>%
        count(区县, name = "病例数") %>%
        right_join(units %>% rename(区县 = .data$地区名称), by = "区县") %>%
        mutate(
          病例数 = replace_na(.data$病例数, 0L),
          发病率 = .data$病例数 / .data$人口数 * 100000
        ) %>%
        arrange(desc(.data$发病率), desc(.data$病例数))
    })

    selected_districts <- reactive({
      summary_df <- district_summary()
      selected <- input$focus_districts

      if (is.null(selected) || length(selected) == 0) {
        return(summary_df$区县)
      }

      intersect(selected, summary_df$区县)
    })

    district_summary_selected <- reactive({
      district_summary() %>%
        filter(.data$区县 %in% selected_districts())
    })

    district_map_data <- reactive({
      map_sf <- admin_map_data()
      req(!is.null(map_sf))

      map_sf %>%
        left_join(district_summary(), by = "区县") %>%
        mutate(
          病例数 = replace_na(.data$病例数, 0L),
          人口数 = replace_na(.data$人口数, 0),
          发病率 = replace_na(.data$发病率, 0)
        ) %>%
        filter(.data$区县 %in% selected_districts())
    })

    output$map_note <- renderUI({
      map_sf <- admin_map_data()

      if (is.null(map_sf)) {
        return(
          tags$div(
            class = "info-note",
            HTML(glue(
              "研究范围：<b>{area_label()}</b>。未能根据人口参数匹配到合适地图，",
              "已保留排名图和明细表。请检查地图文件和人口表地区名称是否一致。"
            ))
          )
        )
      }

      metric_label <- get_map_metric_label(input$map_metric)
      source_label <- attr(map_sf, "source_label")
      map_level <- attr(map_sf, "map_level")
      match_count <- attr(map_sf, "match_count")
      expected_count <- attr(map_sf, "expected_count")
      selected_count <- length(selected_districts())

      tags$div(
        class = "info-note",
        HTML(glue(
          "研究范围：<b>{area_label()}</b>；地图层级：<b>{map_level}</b>；",
          "地图数据源：<b>{source_label}</b>；匹配地区：<b>{match_count}/{expected_count}</b>；",
          "当前展示地区：<b>{selected_count}</b> 个；统计口径：<b>{metric_label}</b>。"
        ))
      )
    })

    observeEvent(district_summary(), {
      summary_df <- district_summary()
      current_selection <- isolate(input$focus_districts)

      suggested_selection <- summary_df %>%
        filter(.data$病例数 > 0) %>%
        slice_head(n = 5) %>%
        pull(.data$区县)

      if (length(current_selection) > 0) {
        suggested_selection <- union(intersect(current_selection, summary_df$区县), suggested_selection)
      }

      updateSelectizeInput(
        session,
        "focus_districts",
        choices = summary_df$区县,
        selected = if (length(current_selection) > 0) intersect(current_selection, summary_df$区县) else summary_df$区县
      )
    }, ignoreInit = FALSE)

    output$region_map <- renderPlotly({
      validate(need(!is.null(admin_map_data()), "当前未找到可用地图数据。"))

      map_sf <- district_map_data() %>%
        sf::st_transform(4326)
      validate(need(nrow(map_sf) > 0, "当前筛选条件下暂无地图数据。"))

      metric_col <- get_map_metric_column(input$map_metric)
      metric_label <- get_map_metric_label(input$map_metric)
      map_guides <- build_map_guides(map_sf)
      map_sf <- map_sf %>%
        mutate(
          地图指标 = .data[[metric_col]],
          地图提示 = if (identical(metric_col, "病例数")) {
            glue("<b>{区县}</b><br>发病数: {comma(病例数)}例")
          } else {
            glue("<b>{区县}</b><br>发病率: {number(发病率, accuracy = 0.01)}/10万")
          }
        )
      label_points <- suppressWarnings(
        map_sf %>%
          sf::st_transform(3857) %>%
          sf::st_point_on_surface() %>%
          sf::st_transform(sf::st_crs(map_sf))
      )
      label_df <- label_points %>%
        sf::st_drop_geometry() %>%
        bind_cols(tibble::as_tibble(sf::st_coordinates(label_points)))
      metric_range <- range(map_sf$地图指标, na.rm = TRUE)
      if (!all(is.finite(metric_range))) {
        metric_range <- c(0, 1)
      } else if (metric_range[1] == metric_range[2]) {
        metric_range[2] <- metric_range[2] + 1
      }
      map_colors <- c("#EAF4F4", "#A7D9D5", "#58B7AE", "#0F7C82", "#0B3C49")
      map_color_scale <- list(
        list(0, map_colors[[1]]),
        list(0.25, map_colors[[2]]),
        list(0.50, map_colors[[3]]),
        list(0.75, map_colors[[4]]),
        list(1, map_colors[[5]])
      )
      map_palette <- colorNumeric(map_colors, domain = metric_range, na.color = "#F2F5F7")

      map_plot <- plot_ly()

      for (row_index in seq_len(nrow(map_sf))) {
        map_row <- map_sf[row_index, ]
        map_coords <- sf_feature_plotly_coords(map_row)
        hover_text <- map_row$地图提示[[1]]

        map_plot <- map_plot %>%
          add_trace(
            x = map_coords$x,
            y = map_coords$y,
            type = "scatter",
            mode = "lines",
            fill = "toself",
            fillcolor = map_palette(map_row$地图指标[[1]]),
            line = list(color = "white", width = 1),
            hoveron = "fills",
            text = rep(hover_text, length(map_coords$x)),
            hovertext = rep(hover_text, length(map_coords$x)),
            hoverinfo = "text",
            hovertemplate = paste0(hover_text, "<extra></extra>"),
            name = map_row$区县[[1]],
            showlegend = FALSE
          )
      }

      map_plot <- map_plot %>%
        add_trace(
          x = rep(map_guides$x_range[2], 2),
          y = rep(map_guides$y_range[2], 2),
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 0.1,
            opacity = 0,
            color = metric_range,
            cmin = metric_range[[1]],
            cmax = metric_range[[2]],
            colorscale = map_color_scale,
            showscale = TRUE,
            colorbar = list(
              title = "",
              x = 1.02,
              y = 0.52,
              len = 0.62,
              thickness = 14,
              xanchor = "left",
              yanchor = "middle",
              outlinewidth = 0
            )
          ),
          hoverinfo = "skip",
          showlegend = FALSE
        )

      label_annotations <- if (nrow(label_df) <= 40) build_map_label_annotations(label_df) else list()
      guide_annotations <- list(
        list(
          x = mean(c(map_guides$scale_x0, map_guides$scale_x1)),
          y = map_guides$scale_y,
          text = glue("{map_guides$scale_km} km"),
          showarrow = FALSE,
          yshift = -20,
          font = list(family = km_font_family, size = 11, color = "#183642")
        ),
        list(
          x = map_guides$north_x,
          y = map_guides$north_y,
          text = "N",
          showarrow = FALSE,
          font = list(family = km_font_family, size = 13, color = "#183642")
        ),
        list(
          x = 0.99,
          y = 0.01,
          xref = "paper",
          yref = "paper",
          text = "审图号：GS（2024）0650号",
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "bottom",
          font = list(family = km_font_family, size = 11, color = "#5B6775")
        )
      )

      map_plot %>%
        add_segments(
          x = map_guides$scale_x0,
          xend = map_guides$scale_x1,
          y = map_guides$scale_y,
          yend = map_guides$scale_y,
          line = list(color = "#111111", width = 8),
          hoverinfo = "skip",
          showlegend = FALSE,
          inherit = FALSE
        ) %>%
        add_segments(
          x = c(map_guides$scale_x0, map_guides$scale_x1),
          xend = c(map_guides$scale_x0, map_guides$scale_x1),
          y = map_guides$scale_y - map_guides$scale_tick_height,
          yend = map_guides$scale_y + map_guides$scale_tick_height,
          line = list(color = "#111111", width = 2),
          hoverinfo = "skip",
          showlegend = FALSE,
          inherit = FALSE
        ) %>%
        add_markers(
          x = map_guides$north_x,
          y = map_guides$north_marker_y,
          marker = list(symbol = "triangle-up", size = 20, color = "#183642", line = list(color = "#183642")),
          hoverinfo = "skip",
          showlegend = FALSE,
          inherit = FALSE
        ) %>%
        layout(
          title = list(
            text = glue(
              "<b>{area_label()}传染病疫情数据分析与暴发探测</b><br>",
              "<sup>地区疫情地图；按{metric_label}着色，地图匹配 {attr(admin_map_data(), 'match_count')}/{attr(admin_map_data(), 'expected_count')} 个地区</sup>"
            ),
            font = list(family = km_font_family, color = "#183642", size = 16)
          ),
          annotations = c(label_annotations, guide_annotations),
          xaxis = list(
            range = map_guides$x_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE,
            title = ""
          ),
          yaxis = list(
            range = map_guides$y_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE,
            title = "",
            scaleanchor = "x",
            scaleratio = 1
          ),
          margin = list(l = 8, r = 105, b = 42, t = 64),
          hoverlabel = list(font = list(family = km_font_family)),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$rank_plot <- renderPlotly({
      summary_df <- district_summary_selected()
      validate(need(nrow(summary_df) > 0, "当前筛选条件下暂无地区数据。"))

      metric_col <- get_map_metric_column(input$map_metric)
      metric_label <- get_map_metric_label(input$map_metric)
      plot_df <- summary_df %>%
        arrange(.data[[metric_col]]) %>%
        mutate(
          指标值 = .data[[metric_col]],
          区县 = factor(.data$区县, levels = .data$区县),
          悬停文本 = if (identical(metric_col, "病例数")) {
            glue("<b>{区县}</b><br>发病数: {comma(病例数)}例")
          } else {
            glue("<b>{区县}</b><br>发病率: {number(发病率, accuracy = 0.01)}/10万")
          }
        )

      plot_ly(
        data = plot_df,
        x = ~指标值,
        y = ~区县,
        type = "bar",
        orientation = "h",
        marker = list(color = plot_colors[["主色"]]),
        text = ~if (identical(metric_col, "病例数")) comma(指标值) else number(指标值, accuracy = 0.01),
        textposition = "outside",
        hovertext = ~悬停文本,
        hoverinfo = "text"
      ) %>%
        layout(
          margin = list(l = 80, r = 20, b = 50, t = 20),
          title = list(
            text = glue("按{metric_label}排序"),
            font = list(family = km_font_family, size = 13, color = "#183642")
          ),
          xaxis = list(
            title = metric_label,
            showgrid = TRUE,
            gridcolor = "#E6EDF2",
            zeroline = FALSE
          ),
          yaxis = list(title = NULL, showgrid = FALSE),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$trend_plot <- renderPlotly({
      focus_districts <- selected_districts()
      validate(need(length(focus_districts) > 0, "请至少选择一个重点地区，或留空表示全部地区。"))

      metric_col <- get_map_metric_column(input$map_metric)
      metric_label <- get_map_metric_label(input$map_metric)

      trend_df <- filtered_data() %>%
        filter(.data$区县 %in% focus_districts) %>%
        mutate(时间点 = if (input$trend_unit == "week") .data$筛选周 else .data$筛选日期) %>%
        count(区县, 时间点, name = "病例数") %>%
        left_join(
          population_analysis_units(population_data()) %>% rename(区县 = .data$地区名称),
          by = "区县"
        ) %>%
        mutate(
          发病率 = .data$病例数 / .data$人口数 * 100000,
          指标值 = .data[[metric_col]],
          悬停文本 = if (identical(metric_col, "病例数")) {
            glue("<b>{区县}</b><br>时间: {时间点}<br>发病数: {comma(病例数)}例")
          } else {
            glue("<b>{区县}</b><br>时间: {时间点}<br>发病率: {number(发病率, accuracy = 0.01)}/10万")
          }
        ) %>%
        arrange(.data$时间点, .data$区县)

      validate(need(nrow(trend_df) > 0, "当前筛选条件下，所选区县暂无趋势数据。"))

      plot_ly(
        data = trend_df,
        x = ~时间点,
        y = ~指标值,
        color = ~区县,
        colors = c("#0F7C82", "#2A9D8F", "#E76F51", "#F4A261", "#264653", "#8AB17D"),
        type = "scatter",
        mode = "lines+markers",
        hovertext = ~悬停文本,
        hoverinfo = "text"
      ) %>%
        layout(
          margin = list(l = 60, r = 20, b = 50, t = 20),
          title = list(
            text = glue("{metric_label}时间趋势"),
            font = list(family = km_font_family, size = 13, color = "#183642")
          ),
          xaxis = list(title = NULL, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(title = metric_label, showgrid = TRUE, gridcolor = "#E6EDF2", zeroline = FALSE),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.12)
        )
    })

    output$region_table <- renderDT({
      metric_col <- get_map_metric_column(input$map_metric)
      summary_df <- district_summary_selected() %>%
        arrange(desc(.data[[metric_col]]), desc(.data$病例数)) %>%
        mutate(
          排名 = row_number(),
          发病率 = round(.data$发病率, 2)
        ) %>%
        select(排名, 区县, 人口数, 病例数, 发病率)

      datatable(
        summary_df,
        extensions = "Buttons",
        rownames = FALSE,
        options = list(
          dom = "Bfrtip",
          pageLength = 14,
          buttons = c("copy", "csv", "excel"),
          autoWidth = TRUE
        )
      ) %>%
        formatRound(columns = c("人口数", "病例数"), digits = 0, mark = ",") %>%
        formatRound(columns = "发病率", digits = 2) %>%
        formatStyle(
          "发病率",
          background = styleColorBar(summary_df$发病率, "#D6EEF0"),
          backgroundSize = "95% 70%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
  })
}

outbreak_server <- function(id, filtered_data, global_date_range) {
  moduleServer(id, function(input, output, session) {
    output$alert_date_range_ui <- renderUI({
      date_range <- global_date_range()
      analysis_end <- as.Date(date_range[2])
      analysis_start <- max(as.Date(date_range[1]), analysis_end - days(13), na.rm = TRUE)

      dateRangeInput(
        session$ns("alert_date_range"),
        "预警发病日期范围（默认近 14 天）",
        start = analysis_start,
        end = analysis_end,
        min = as.Date(date_range[1]),
        max = as.Date(date_range[2]),
        separator = " 至 "
      )
    })

    alert_date_range <- reactive({
      req(input$alert_date_range)
      date_range <- as.Date(input$alert_date_range)
      validate(need(length(date_range) == 2L && all(!is.na(date_range)) && date_range[1] <= date_range[2], "请选择有效的预警日期范围。"))
      validate(need(length(input$threshold) == 1L && is.finite(input$threshold) && input$threshold >= 2 && input$threshold == floor(input$threshold), "病例数阈值须为至少 2 的整数。"))
      validate(need(length(input$max_span_days) == 1L && is.finite(input$max_span_days) && input$max_span_days >= 1 && input$max_span_days == floor(input$max_span_days), "时间跨度须为正整数。"))
      pattern_ok <- tryCatch({ stringr::str_detect("学校", input$keywords); TRUE }, error = function(e) FALSE)
      validate(need(pattern_ok, "重点场所关键字表达式无效，请检查括号等符号。"))
      date_range
    })

    outbreak_candidate_cards <- reactive({
      date_range <- alert_date_range()

      build_outbreak_candidate_cards(
        data = filtered_data(),
        start_date = date_range[1],
        end_date = date_range[2],
        keyword_pattern = input$keywords,
        date_col = "发病日期"
      )
    })

    outbreak_alerts <- reactive({
      date_range <- alert_date_range()

      detect_outbreak_clusters(
        data = filtered_data(),
        start_date = date_range[1],
        end_date = date_range[2],
        threshold = input$threshold,
        keyword_pattern = input$keywords,
        date_col = "发病日期",
        max_span_days = input$max_span_days
      )
    })

    outbreak_alert_cards <- reactive({
      filter_outbreak_cards_by_alerts(outbreak_candidate_cards(), outbreak_alerts())
    })

    outbreak_micro_alerts <- reactive({
      detect_outbreak_micro_alerts(outbreak_alert_cards(), threshold = input$threshold)
    })

    output$alert_events <- renderValueBox({
      alerts <- outbreak_alerts()
      valueBox(
        value = comma(nrow(alerts)),
        subtitle = "机构级预警数",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    })

    output$alert_places <- renderValueBox({
      alerts <- outbreak_alerts()
      valueBox(
        value = comma(nrow(distinct(alerts, .data$场所地区, .data$core_institution))),
        subtitle = "涉及机构数（按地区分组）",
        icon = icon("building"),
        color = "aqua"
      )
    })

    output$window_note <- renderUI({
      alerts <- outbreak_alerts()
      micro_alerts <- outbreak_micro_alerts()
      cards <- outbreak_alert_cards()
      date_range <- alert_date_range()
      analysis_start <- as.Date(date_range[1])
      analysis_end <- as.Date(date_range[2])

      tags$div(
        class = "info-note",
        tags$p(
          HTML(glue(
            "<b>预警时间窗口：</b>{format(analysis_start, '%Y-%m-%d')} 至 {format(analysis_end, '%Y-%m-%d')}；",
            "筛查规则为同一地区分组、同一核心机构、同一原始病种名称、发病时间跨度 <b>&lt;= {input$max_span_days}</b> 天，",
            "病例数 <b>&gt;= {input$threshold}</b>。"
          ))
        ),
        tags$p(
          HTML(glue(
            "<b>当前结果：</b>共识别 <b>{comma(nrow(alerts))}</b> 条机构级预警，",
            "其中 <b>{comma(sum(micro_alerts$预警类型 == '班级/科室聚集'))}</b> 条达到班级/科室级聚集；",
            "可导出 <b>{comma(nrow(cards))}</b> 张相关病例卡片明细。"
          ))
        ),
        tags$p("预警始终按发病日期筛查，并沿用左侧病种筛选；不受左侧诊断或订正日期过滤影响。工作单位和住址分别产生线索，预警数不等于独立疫情事件数。地区分组以居住地作为代理，跨地区就学/就医可能拆组；结果须人工核实。")
      )
    })

    output$alert_table <- renderDT({
      alerts <- outbreak_alerts() %>%
        mutate(
          聚集病例数 = as.integer(.data$聚集病例数),
          最早发病日期 = format(.data$最早发病日期, "%Y-%m-%d"),
          最晚发病日期 = format(.data$最晚发病日期, "%Y-%m-%d")
        ) %>%
        select(
          场所类型, 场所地区, 核心机构 = core_institution, 疾病名称, 聚集病例数, 时间跨度天数,
          最早发病日期, 最晚发病日期, 涉及区县, 子部门数, 子部门分布
        )

      datatable(
        alerts,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, autoWidth = TRUE)
      ) %>%
        formatStyle(
          "聚集病例数",
          backgroundColor = styleInterval(
            c(2, 4, 6),
            c("#FFF3CD", "#FDE2B8", "#F7B267", "#F25F5C")
          ),
          color = styleInterval(c(6), c("#183642", "white")),
          fontWeight = "bold"
        )
    })

    output$micro_alert_table <- renderDT({
      micro_alerts <- outbreak_micro_alerts() %>%
        mutate(
          聚集病例数 = as.integer(.data$聚集病例数),
          最早发病日期 = format(.data$最早发病日期, "%Y-%m-%d"),
          最晚发病日期 = format(.data$最晚发病日期, "%Y-%m-%d")
        ) %>%
        select(
          场所类型, 场所地区, 核心机构 = core_institution, 子部门 = sub_level, 疾病名称, 预警类型,
          聚集病例数, 最早发病日期, 最晚发病日期, 涉及区县, 子部门分布
        )

      datatable(
        micro_alerts,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
      ) %>%
        formatStyle(
          "预警类型",
          backgroundColor = styleEqual(
            c("班级/科室聚集", "机构级聚集，子部门未达阈值"),
            c("#F25F5C", "#FFF3CD")
          ),
          color = styleEqual(
            c("班级/科室聚集", "机构级聚集，子部门未达阈值"),
            c("white", "#183642")
          ),
          fontWeight = "bold"
        )
    })

    output$alert_card_table <- renderDT({
      cards <- outbreak_alert_cards() %>%
        mutate(
          发病日期 = format(.data$发病日期, "%Y-%m-%d"),
          诊断时间 = format(.data$诊断时间, "%Y-%m-%d %H:%M:%S"),
          订正终审时间 = format(.data$订正终审时间, "%Y-%m-%d %H:%M:%S"),
          报告卡录入时间 = format(.data$报告卡录入时间, "%Y-%m-%d %H:%M:%S")
        )

      datatable(
        cards,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$download_alert_cards <- downloadHandler(
      filename = function() {
        glue("重点场所预警卡片明细_{format(Sys.Date(), '%Y%m%d')}.csv")
      },
      content = function(file) {
        cards <- outbreak_alert_cards() %>%
          mutate(
            发病日期 = format(.data$发病日期, "%Y-%m-%d"),
            诊断时间 = format(.data$诊断时间, "%Y-%m-%d %H:%M:%S"),
            订正终审时间 = format(.data$订正终审时间, "%Y-%m-%d %H:%M:%S"),
            报告卡录入时间 = format(.data$报告卡录入时间, "%Y-%m-%d %H:%M:%S")
          )

        readr::write_excel_csv(cards, file)
      }
    )
  })
}

app_ui <- function() {
  dashboardPage(
    title = "传染病疫情数据分析与暴发探测",
    skin = "blue",
    dashboardHeader(
      title = uiOutput("app_title", inline = TRUE),
      titleWidth = 420
    ),
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem("常规监测概览", tabName = "overview", icon = icon("line-chart")),
        menuItem("地区发病率排名", tabName = "regional", icon = icon("bar-chart")),
        menuItem("重点场所暴发预警", tabName = "outbreak", icon = icon("bell")),
        menuItem("一键报告中心", tabName = "reports", icon = icon("file-word"))
      ),
      div(
        class = "sidebar-block",
        h4("全局筛选"),
        fileInput(
          "surveillance_upload",
          "上传监测数据（CSV）",
          accept = c(".csv", "text/csv")
        ),
        fileInput(
          "population_upload",
          "上传人口参数（CSV / Excel）",
          accept = c(".csv", ".xlsx", ".xls", "text/csv")
        ),
        radioButtons(
          "global_time_col",
          "时间筛选口径",
          choices = time_filter_choices,
          selected = "发病日期"
        ),
        dateRangeInput(
          "global_date_range",
          "日期范围",
          start = default_date_range[1],
          end = default_date_range[2],
          min = available_date_range[1],
          max = available_date_range[2],
          separator = " 至 "
        ),
        selectizeInput(
          "global_standard_disease",
          "标准病种下拉筛选",
          choices = standard_disease_choices,
          selected = character(0),
          multiple = TRUE,
          options = list(
            placeholder = "留空表示全部病种；输入关键字可搜索",
            plugins = list("remove_button"),
            maxItems = NULL
          )
        ),
        checkboxGroupInput(
          "global_management",
          "管理级别",
          choices = management_filter_choices,
          selected = management_filter_choices
        ),
        checkboxGroupInput(
          "global_route",
          "主要传播方式",
          choices = route_filter_choices,
          selected = route_filter_choices
        ),
        uiOutput("data_source_info")
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML(
          "
          body, .content-wrapper, .right-side {
            font-family: 'PingFang SC', 'Microsoft YaHei', sans-serif;
            background: linear-gradient(180deg, #F4F8FB 0%, #EEF4F7 100%);
          }
          .main-header .logo {
            background-color: #0F4C5C !important;
            font-weight: 700;
            font-family: 'PingFang SC', 'Microsoft YaHei', sans-serif;
            font-size: 16px;
            line-height: 50px;
            white-space: nowrap;
          }
          .main-header .navbar {
            background-color: #0F4C5C !important;
          }
          .skin-blue .main-sidebar {
            background: linear-gradient(180deg, #173F5F 0%, #0B1F33 100%);
          }
          .skin-blue .sidebar-menu > li.active > a,
          .skin-blue .sidebar-menu > li:hover > a {
            background: rgba(255, 255, 255, 0.08) !important;
            border-left-color: #F4A261 !important;
          }
          .sidebar-block {
            padding: 14px 16px 4px 16px;
            color: #F6FBFF;
          }
          .sidebar-block .form-group label,
          .sidebar-block .control-label,
          .sidebar-block h4,
          .sidebar-block .checkbox,
          .sidebar-block .radio {
            color: #F6FBFF !important;
            font-weight: 600;
          }
          .sidebar-block .shiny-input-container {
            margin-bottom: 12px;
          }
          .sidebar-block .btn, 
          .sidebar-block .form-control {
            border-radius: 8px;
          }
          .sidebar-hint {
            margin-top: 12px;
            color: #D7E7F2;
            font-size: 12px;
            line-height: 1.6;
          }
          .box {
            border-radius: 12px;
            border-top: 3px solid #0F7C82;
            box-shadow: 0 10px 24px rgba(15, 76, 92, 0.08);
          }
          .box-header > .box-title {
            font-weight: 700;
          }
          .small-box {
            border-radius: 12px;
            box-shadow: 0 10px 24px rgba(15, 76, 92, 0.10);
          }
          .small-box.bg-aqua { background: #0F7C82 !important; }
          .small-box.bg-green { background: #2A9D8F !important; }
          .small-box.bg-yellow { background: #E9C46A !important; color: #183642 !important; }
          .small-box.bg-red { background: #E76F51 !important; }
          .info-note {
            background: #F7FBFD;
            border-left: 4px solid #0F7C82;
            border-radius: 8px;
            padding: 12px 14px;
          }
          .info-note p:last-child {
            margin-bottom: 0;
          }
          .module-note {
            color: #5B6775;
            line-height: 1.7;
            margin-top: 10px;
          }
          .global-loading-overlay {
            position: fixed;
            inset: 0;
            z-index: 99999;
            display: none;
            align-items: center;
            justify-content: center;
            background: rgba(11, 31, 51, 0.72);
            backdrop-filter: blur(3px);
          }
          .global-loading-card {
            width: min(460px, calc(100vw - 48px));
            padding: 28px 30px;
            border-radius: 18px;
            background: #FFFFFF;
            box-shadow: 0 24px 64px rgba(11, 31, 51, 0.30);
            text-align: center;
          }
          .global-loading-spinner {
            width: 44px;
            height: 44px;
            margin: 0 auto 16px auto;
            border: 4px solid #D8EDF0;
            border-top-color: #0F7C82;
            border-radius: 50%;
            animation: globalLoadingSpin 0.85s linear infinite;
          }
          .global-loading-title {
            font-size: 18px;
            font-weight: 700;
            color: #173F5F;
            margin-bottom: 8px;
          }
          .global-loading-detail {
            min-height: 22px;
            color: #5B6775;
            font-size: 14px;
            line-height: 1.5;
            margin-bottom: 18px;
          }
          .global-loading-progress {
            height: 10px;
            background: #E6EDF2;
            border-radius: 999px;
            overflow: hidden;
          }
          .global-loading-progress-bar {
            width: 0%;
            height: 100%;
            background: linear-gradient(90deg, #0F7C82, #2A9D8F);
            border-radius: 999px;
            transition: width 0.25s ease;
          }
          .global-loading-percent {
            margin-top: 8px;
            color: #0F7C82;
            font-weight: 700;
          }
          @keyframes globalLoadingSpin {
            from { transform: rotate(0deg); }
            to { transform: rotate(360deg); }
          }
          "
        ))
      ),
      tags$script(HTML(
        "
        (function() {
          var manualLoading = false;
          var busyTimer = null;

          function clampProgress(progress) {
            if (progress === null || progress === undefined || isNaN(progress)) return null;
            return Math.max(0, Math.min(1, Number(progress)));
          }

          function showOverlay(text, progress) {
            var overlay = document.getElementById('global-loading-overlay');
            if (!overlay) return;
            var detail = document.getElementById('global-loading-detail');
            var bar = document.getElementById('global-loading-progress-bar');
            var percent = document.getElementById('global-loading-percent');
            var value = clampProgress(progress);

            if (detail) detail.textContent = text || '正在分析数据，请稍候...';
            if (bar && value !== null) bar.style.width = Math.round(value * 100) + '%';
            if (bar && value === null) bar.style.width = '35%';
            if (percent) percent.textContent = value === null ? '处理中...' : Math.round(value * 100) + '%';

            overlay.style.display = 'flex';
          }

          function hideOverlay() {
            var overlay = document.getElementById('global-loading-overlay');
            if (overlay) overlay.style.display = 'none';
          }

          if (window.Shiny) {
            Shiny.addCustomMessageHandler('global-loading-overlay', function(message) {
              if (!message) return;
              if (message.action === 'show') {
                manualLoading = true;
                showOverlay(message.text, message.progress);
              } else if (message.action === 'hide') {
                manualLoading = false;
                hideOverlay();
              } else {
                showOverlay(message.text, message.progress);
              }
            });
          }

          $(document).on('shiny:busy', function() {
            if (busyTimer) clearTimeout(busyTimer);
            busyTimer = setTimeout(function() {
              if (!manualLoading) showOverlay('正在分析数据，请稍候...', null);
            }, 350);
          });

          $(document).on('shiny:idle', function() {
            if (busyTimer) clearTimeout(busyTimer);
            if (!manualLoading) hideOverlay();
          });
        })();
        "
      )),
      tags$div(
        id = "global-loading-overlay",
        class = "global-loading-overlay",
        tags$div(
          class = "global-loading-card",
          tags$div(class = "global-loading-spinner"),
          tags$div(class = "global-loading-title", "正在分析数据"),
          tags$div(id = "global-loading-detail", class = "global-loading-detail", "正在加载数据，请稍候..."),
          tags$div(
            class = "global-loading-progress",
            tags$div(id = "global-loading-progress-bar", class = "global-loading-progress-bar")
          ),
          tags$div(id = "global-loading-percent", class = "global-loading-percent", "0%")
        )
      ),
      tabItems(
        overview_ui("overview"),
        regional_ui("regional"),
        outbreak_ui("outbreak"),
        reports_ui("reports")
      )
    )
  )
}

app_server <- function(input, output, session) {
  active_data_state <- reactiveValues(
    data = default_surveillance_data,
    source_label = surveillance_file,
    source_type = if (identical(surveillance_file, "data/demo_surveillance.csv")) "完全合成演示数据" else "本地指定数据",
    last_error = NULL
  )
  active_population_state <- reactiveValues(
    data = default_population_data,
    source_label = "内置2025昆明市人口",
    source_type = "默认人口参数",
    last_error = NULL
  )

  observeEvent(input$surveillance_upload, {
    req(input$surveillance_upload)

    tryCatch({
      uploaded_data <- withProgress(message = "正在分析监测数据", value = 0, {
        send_loading_overlay(session, action = "show", text = "0% 正在加载上传文件...", progress = 0)
        set_analysis_progress(session, 0.10, "10% 正在读取 CSV 文件...")
        set_analysis_progress(session, 0.35, "35% 正在执行字段标准化和病种映射...")
        uploaded_data <- load_surveillance_data(input$surveillance_upload$datapath)
        if (nrow(uploaded_data) == 0L) stop("清洗后没有有效病例，请检查发病日期、病种和卡片状态。当前数据保持不变。")
        set_analysis_progress(session, 0.80, "80% 正在完成地址清洗和地区匹配...")
        set_analysis_progress(session, 0.95, "95% 正在刷新仪表盘筛选器...")
        uploaded_data
      })

      active_data_state$data <- uploaded_data
      active_data_state$source_label <- input$surveillance_upload$name
      active_data_state$source_type <- "用户上传数据"
      active_data_state$last_error <- NULL
      send_loading_overlay(session, action = "hide")

      showNotification(
        glue("监测数据上传成功：{input$surveillance_upload$name}"),
        type = "message"
      )
    }, error = function(e) {
      active_data_state$last_error <- conditionMessage(e)

      showNotification(
        glue("上传文件读取失败：{conditionMessage(e)}"),
        type = "error",
        duration = NULL
      )
      send_loading_overlay(session, action = "hide")
    })
  })

  active_surveillance_data <- reactive({
    active_data_state$data
  })

  observeEvent(input$population_upload, {
    req(input$population_upload)

    tryCatch({
      uploaded_population <- withProgress(message = "正在分析人口参数", value = 0, {
        send_loading_overlay(session, action = "show", text = "0% 正在加载人口参数...", progress = 0)
        set_analysis_progress(session, 0.20, "20% 正在读取人口参数文件...")
        uploaded_population <- load_population_data(
          input$population_upload$datapath,
          source_label = input$population_upload$name
        )
        set_analysis_progress(session, 0.70, "70% 正在标准化地区名称和人口字段...")
        set_analysis_progress(session, 0.92, "92% 正在匹配研究范围和地图层级...")
        uploaded_population
      })

      active_population_state$data <- uploaded_population
      active_population_state$source_label <- input$population_upload$name
      active_population_state$source_type <- "用户上传人口参数"
      active_population_state$last_error <- NULL
      send_loading_overlay(session, action = "hide")

      showNotification(
        glue("人口参数上传成功：{input$population_upload$name}"),
        type = "message"
      )
    }, error = function(e) {
      active_population_state$last_error <- conditionMessage(e)

      showNotification(
        glue("人口参数读取失败：{conditionMessage(e)}"),
        type = "error",
        duration = NULL
      )
      send_loading_overlay(session, action = "hide")
    })
  })

  active_population_data <- reactive({
    active_population_state$data
  })

  area_label <- reactive({
    unique(active_population_data()$参数名称)[[1]]
  })

  output$app_title <- renderUI({
    span(
      glue("{area_label()}传染病疫情数据分析与暴发探测"),
      style = "font-weight: 700;"
    )
  })

  active_analysis_data <- reactive({
    match_surveillance_to_population(active_surveillance_data(), active_population_data())
  })

  active_admin_map <- reactive({
    map_sf <- load_best_admin_map(active_population_data())

    if (is.null(map_sf)) {
      return(NULL)
    }

    map_sf %>%
      mutate(区县 = .data$地区名称)
  })

  observe({
    current_data <- active_surveillance_data()
    time_col <- if (is.null(input$global_time_col)) "发病日期" else input$global_time_col
    date_values <- get_time_filter_dates(current_data, time_col)
    date_range <- if (all(is.na(date_values))) range(current_data$发病日期, na.rm = TRUE) else range(date_values, na.rm = TRUE)
    default_range <- compute_default_date_range(current_data, time_col)
    management_choices <- build_management_choices(current_data)
    route_choices <- build_route_choices(current_data)
    selected_management <- intersect(isolate(input$global_management), management_choices)
    selected_routes <- intersect(isolate(input$global_route), route_choices)

    if (length(selected_management) == 0 && is.null(isolate(input$global_management))) {
      selected_management <- management_choices
    }
    if (length(selected_routes) == 0 && is.null(isolate(input$global_route))) {
      selected_routes <- route_choices
    }

    disease_choice_info <- build_standard_disease_choices(
      current_data,
      selected_management = selected_management,
      selected_routes = selected_routes
    )
    selected_diseases <- intersect(isolate(input$global_standard_disease), disease_choice_info$all_values)

    updateDateRangeInput(
      session,
      "global_date_range",
      start = default_range[1],
      end = default_range[2],
      min = date_range[1],
      max = date_range[2]
    )

    updateSelectizeInput(
      session,
      "global_standard_disease",
      choices = disease_choice_info$grouped,
      selected = selected_diseases,
      server = TRUE
    )

    updateCheckboxGroupInput(
      session,
      "global_management",
      choices = management_choices,
      selected = selected_management
    )

    updateCheckboxGroupInput(
      session,
      "global_route",
      choices = route_choices,
      selected = selected_routes
    )
  })

  output$data_source_info <- renderUI({
    current_data <- active_surveillance_data()
    analysis_data <- active_analysis_data()
    population_data <- active_population_data()
    population_units <- population_analysis_units(population_data)
    time_col <- if (is.null(input$global_time_col)) "发病日期" else input$global_time_col
    matched_cases <- sum(!is.na(analysis_data$区县))
    map_sf <- active_admin_map()
    map_text <- if (is.null(map_sf)) {
      "未匹配到地图"
    } else {
      glue("{attr(map_sf, 'map_level')} / {attr(map_sf, 'match_count')}/{attr(map_sf, 'expected_count')}")
    }

    tags$p(
      class = "sidebar-hint",
      HTML(glue(
        "支持按 <b>标准病种下拉</b>、<b>管理级别</b>、<b>传播方式</b> 进行组合筛选。<br>",
        "标准病种留空时默认纳入全部病种。<br>",
        "数据模式：<b>{active_data_state$source_type}</b><br>",
        "监测数据：<b>{htmltools::htmlEscape(active_data_state$source_label)}</b><br>",
        "有效卡片：<b>{comma(nrow(current_data))}</b> 条<br>",
        "当前时间口径：<b>{names(time_filter_choices)[match(time_col, time_filter_choices)]}</b><br>",
        "该口径日期缺失：<b>{sum(is.na(get_time_filter_dates(current_data, time_col)))}</b> 条（不参与该口径统计）<br>",
        "人口参数：<b>{active_population_state$source_label}</b><br>",
        "研究范围：<b>{area_label()}</b>；地区数：<b>{nrow(population_units)}</b><br>",
        "病例地区匹配：<b>{comma(matched_cases)}</b> 条；地图匹配：<b>{map_text}</b>",
        if (!is.null(active_data_state$last_error)) {
          glue("<br><span style='color:#FFD6D6;'>监测数据上传错误：{active_data_state$last_error}</span>")
        } else {
          ""
        },
        if (!is.null(active_population_state$last_error)) {
          glue("<br><span style='color:#FFD6D6;'>人口参数上传错误：{active_population_state$last_error}</span>")
        } else {
          ""
        }
      ))
    )
  })

  scope_data <- reactive({
    current_data <- active_analysis_data()
    disease_choice_info <- build_standard_disease_choices(
      current_data,
      selected_management = input$global_management,
      selected_routes = input$global_route
    )

    selected_df <- current_data

    if (length(input$global_management) == 0 || length(input$global_route) == 0) {
      selected_df <- selected_df %>% slice(0)
    } else {
      if (length(input$global_standard_disease) > 0) {
        selected_df <- selected_df %>%
          filter(.data$标准病种 %in% input$global_standard_disease)
      } else {
        selected_df <- selected_df %>%
          filter(.data$标准病种 %in% disease_choice_info$all_values)
      }
    }

    selected_df
  })

  filtered_data <- reactive({
    req(input$global_date_range, input$global_time_col)
    selected_df <- scope_data()
    selected_df %>%
      mutate(筛选日期 = get_time_filter_dates(selected_df, input$global_time_col),
             筛选周 = floor_date(.data$筛选日期, unit = "week", week_start = 1)) %>%
      filter(!is.na(.data$筛选日期),
             .data$筛选日期 >= as.Date(input$global_date_range[1]),
             .data$筛选日期 <= as.Date(input$global_date_range[2]))
  })

  global_date_range <- reactive({
    req(input$global_date_range)
    as.Date(input$global_date_range)
  })

  overview_server(
    "overview",
    filtered_data = filtered_data,
    population_data = active_population_data
  )
  regional_server(
    "regional",
    filtered_data = filtered_data,
    population_data = active_population_data,
    admin_map_data = active_admin_map,
    area_label = area_label
  )
  outbreak_server(
    "outbreak",
    filtered_data = scope_data,
    global_date_range = global_date_range
  )
  reports_server(
    "reports", data = scope_data, all_data = active_analysis_data,
    time_col = reactive(input$global_time_col), region_label = area_label,
    source_label = reactive(paste(active_data_state$source_type, active_data_state$source_label)),
    data_quality = reactive(attr(active_surveillance_data(), "data_quality")),
    population_data = active_population_data, admin_map_data = active_admin_map
  )
}

ui <- app_ui()
server <- app_server

shinyApp(ui = ui, server = server)
