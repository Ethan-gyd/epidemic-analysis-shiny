# 只读取仓库随附的全合成演示数据。生成三种报告的 Word 和 PDF 样例。
# 运行：Rscript scripts/generate_demo_reports.R
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(glue)
})
source("data_preprocessing.R", encoding = "UTF-8")
source("outbreak_detection.R", encoding = "UTF-8")
source("report_generation.R", encoding = "UTF-8")

raw_data <- load_surveillance_data("data/demo_surveillance.csv")
default_population_data <- normalize_population_data(km_population_2025, source_label = "内置2025昆明市人口（用于合成病例演示）")
data <- match_surveillance_to_population(raw_data, default_population_data)
attr(data, "data_quality") <- attr(raw_data, "data_quality")
admin_map <- load_best_admin_map(default_population_data)
if (!is.null(admin_map)) admin_map$区县 <- as.character(admin_map$地区名称)
output <- "output/reports"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
configs <- list(
  list(type = "monthly", dates = as.Date(c("2026-09-01", "2026-09-30")), name = "演示月报_2026年09月", diseases = NULL),
  list(type = "weekly", dates = as.Date(c("2026-10-05", "2026-10-11")), name = "演示周报_20261005至1011", diseases = NULL),
  list(type = "special", dates = as.Date(c("2026-10-01", "2026-10-08")), name = "演示专题_流行性感冒聚集分析", diseases = "流行性感冒（流感）")
)
for (config in configs) {
  selected <- data
  if (!is.null(config$diseases)) selected <- selected[selected$标准病种 %in% config$diseases, , drop = FALSE]
  alerts <- detect_outbreak_clusters(selected, start_date = config$dates[1], end_date = config$dates[2], threshold = 3, max_span_days = 14)
  candidates <- build_outbreak_candidate_cards(selected, start_date = config$dates[1], end_date = config$dates[2])
  micro_alerts <- detect_outbreak_micro_alerts(filter_outbreak_cards_by_alerts(candidates, alerts), threshold = 3)
  payload <- build_report_payload(data, config$dates, report_type = config$type, region_label = "合成演示地区",
    diseases = config$diseases, topic_title = if (config$type == "special") "合成数据流行性感冒专题分析" else NULL,
    outbreak_alerts = alerts, outbreak_params = list(threshold = 3L, max_span_days = 14L),
    outbreak_micro_alerts = micro_alerts, population_data = default_population_data, admin_map_data = admin_map,
    include_place_names = TRUE,
    data_cutoff = as.Date("2026-10-08"), filter_note = "全部数据均由程序合成，仅用于演示与测试，不反映任何真实地区的疫情。")
  for (format in c("docx", "pdf")) {
    if (format == "pdf" && !report_export_capabilities()$pdf) { message("跳过 PDF：", report_export_capabilities()$pdf_message); next }
    path <- file.path(output, paste0(config$name, ".", format))
    write_surveillance_report(payload, path, format)
    cat("Created:", path, "\n")
  }
}
