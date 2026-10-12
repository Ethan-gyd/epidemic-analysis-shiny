source("app.R", encoding = "UTF-8")
shiny::testServer(app_server, {
  session$setInputs(global_time_col = "发病日期", global_date_range = as.Date(c("2026-10-01", "2026-10-08")),
                    global_management = management_filter_choices, global_route = route_filter_choices,
                    global_standard_disease = character())
  stopifnot(nrow(scope_data()) == 330L, nrow(filtered_data()) == 51L)
  session$setInputs(global_time_col = "诊断时间")
  stopifnot(all(filtered_data()$筛选日期 >= as.Date("2026-10-01")))
  session$setInputs(global_management = character())
  stopifnot(nrow(scope_data()) == 0L, nrow(filtered_data()) == 0L)
  # Shiny stores uploads without the original filename extension.
  excel_upload <- tempfile(pattern = "synthetic_population_upload_")
  stopifnot(file.copy("data/population_template.xlsx", excel_upload))
  session$setInputs(population_upload = data.frame(
    name = "synthetic_population.xlsx", size = file.info(excel_upload)$size,
    type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", datapath = excel_upload
  ))
  stopifnot(is.null(active_population_state$last_error))
  stopifnot(active_population_state$source_label == "synthetic_population.xlsx")
  stopifnot(nrow(active_population_state$data) == 3L)
  stopifnot(sum(active_population_state$data$人口数) == 3000000)
  previous_population <- active_population_state$data
  # Conflicting denominators must be rejected without changing active parameters.
  conflict_upload <- tempfile(pattern = "synthetic_population_conflict_")
  readr::write_csv(data.frame(地区名称 = c("五华区", "五华区"),
                              人口数 = c(1000000, 1100000), PAC = c("530102", "530102")), conflict_upload)
  session$setInputs(population_upload = data.frame(
    name = "conflicting_population.csv", size = file.info(conflict_upload)$size,
    type = "text/csv", datapath = conflict_upload
  ))
  stopifnot(!is.null(active_population_state$last_error))
  stopifnot(grepl("五华区", active_population_state$last_error, fixed = TRUE))
  stopifnot(identical(active_population_state$data, previous_population))
  stopifnot(active_population_state$source_label == "synthetic_population.xlsx")
  unlink(c(excel_upload, conflict_upload))
})
demo <- match_surveillance_to_population(default_surveillance_data, default_population_data)
shiny::testServer(reports_server, args = list(
    data = shiny::reactive(demo), all_data = shiny::reactive(demo),
    time_col = shiny::reactive("发病日期"), region_label = shiny::reactive("合成演示"),
    source_label = shiny::reactive("完全合成数据"),
    data_quality = shiny::reactive(attr(default_surveillance_data, "data_quality")),
    population_data = shiny::reactive(default_population_data),
    admin_map_data = shiny::reactive(NULL)), {
  session$setInputs(type = "monthly", anchor = as.Date("2026-09-15"), threshold = 3, span = 14, complete_coverage = FALSE)
  stopifnot(identical(as.character(period()), c("2026-09-01", "2026-09-30")))
  stopifnot(payload()$total_cases == 140L)
  stopifnot(length(payload()$title) == 1L)
  stopifnot(isTRUE(payload()$population_summary$available), is.null(payload()$map_sf))
  stopifnot(sum(payload()$district_rates$病例数) == payload()$total_cases)
  session$setInputs(type = "weekly", anchor = as.Date("2026-10-05"))
  stopifnot(identical(as.character(period()), c("2026-10-05", "2026-10-11")))
  session$setInputs(type = "special", special_dates = as.Date(c("2026-10-01", "2026-10-08")), topic = "合成专题", diseases = "流行性感冒（流感）")
  stopifnot(nrow(alerts()) == 1L, payload()$title == "合成专题")
  stopifnot(all(alerts()$疾病名称 == "流行性感冒"), all(micro_alerts()$疾病名称 == "流行性感冒"))
  stopifnot(sum(micro_alerts()$预警类型 == "班级/科室聚集") == 1L)
  stopifnot(payload()$outbreak_micro_count == 1L)
  # Names are shown by default even before the checkbox input is initialized.
  named_payload <- payload()
  stopifnot(isTRUE(named_payload$include_place_names))
  stopifnot(named_payload$outbreak_details$机构名称 == "演示晨光小学")
  stopifnot(named_payload$outbreak_micro_details$子部门名称 == "3年级2班")
  stopifnot(!any(grepl("SYNTH-CLUSTER-|PRIVATE-", as.character(unlist(named_payload)))))
  session$setInputs(include_place_names = FALSE)
  stopifnot(!any(grepl("演示晨光小学|3年级2班", as.character(unlist(payload())))))
  stopifnot(sum(micro_alerts()$预警类型 == "班级/科室聚集") == 1L)
  session$setInputs(include_place_names = TRUE)
  stopifnot(payload()$outbreak_details$机构名称 == "演示晨光小学")
  # Clearing the topic disease filter restores the complete scoped outbreak set.
  session$setInputs(diseases = character())
  stopifnot(nrow(alerts()) == 3L, sum(micro_alerts()$预警类型 == "班级/科室聚集") == 2L)
  # Missing dates or empty scope do not silently switch statistical definitions.
  session$setInputs(diseases = "不存在的病种")
  stopifnot(nrow(scoped()) == 0L, nrow(alerts()) == 0L, nrow(micro_alerts()) == 0L)
})
cat("Shiny scope and report module integration checks passed.\n")
