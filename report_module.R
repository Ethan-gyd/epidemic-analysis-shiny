# Shiny 报告中心。统计范围独立于左侧日期，病种/管理级别/传播方式沿用左侧筛选。
reports_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "reports",
    shiny::fluidRow(
      shinydashboard::box(width = 4, title = "报告设置", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("type"), "报告类型", c("月报" = "monthly", "周报" = "weekly", "专题分析" = "special")),
        shiny::dateInput(ns("anchor"), "选择所在月份 / 周的任意一天", value = Sys.Date(), language = "zh-CN"),
        shiny::conditionalPanel(sprintf("input['%s'] == 'special'", ns("type")),
          shiny::dateRangeInput(ns("special_dates"), "专题分析日期", start = Sys.Date() - 29, end = Sys.Date(), language = "zh-CN", separator = " 至 "),
          shiny::textInput(ns("topic"), "专题标题", "重点传染病专题分析"),
          shiny::selectizeInput(ns("diseases"), "专题病种（留空沿用左侧选择）", choices = NULL, multiple = TRUE)
        ),
        shiny::numericInput(ns("threshold"), "报告聚集病例阈值", 3, min = 2, max = 20, step = 1),
        shiny::numericInput(ns("span"), "报告最长发病时间跨度（天）", 14, min = 1, max = 30, step = 1),
        shiny::checkboxInput(ns("include_place_names"), "显示具体机构名称和子部门名称", TRUE),
        shiny::p(class = "module-note", "报告默认显示机构及班级、科室等子部门名称，并保留各部门病例数；取消勾选后使用匿名编号。"),
        shiny::checkboxInput(ns("complete_coverage"), "我确认以下期间的数据完整，可计算比较指标", FALSE),
        shiny::conditionalPanel(sprintf("input['%s']", ns("complete_coverage")),
          shiny::dateRangeInput(ns("coverage"), "已核实的完整数据覆盖期", language = "zh-CN", separator = " 至 ")
        ),
        shiny::p(class = "module-note", "月报取自然月，周报取周一至周日。报告沿用左侧病种、管理级别和传播方式，使用本页日期。只有完整覆盖比较期时才计算环比/同比。"),
        shiny::downloadButton(ns("word"), "一键生成 Word", class = "btn-primary"),
        shiny::br(), shiny::br(), shiny::uiOutput(ns("pdf_button")),
        shiny::uiOutput(ns("capabilities"))
      ),
      shinydashboard::box(width = 8, title = "报告内容预览", status = "primary", solidHeader = TRUE,
        shiny::uiOutput(ns("preview")),
        shiny::h4("数据清洗记录"), DT::DTOutput(ns("quality")),
        shiny::h4("报告聚集线索"), DT::DTOutput(ns("alerts")),
        shiny::p(class = "module-note", "Word/PDF 默认生成完整基础疫情分析，图表、统计表与文字解读同步输出。地图和每10万人口指标使用当前人口与地图配置；未确认完整覆盖时标为已纳入病例人口比。缺失配置会在报告中说明。预警采用发病日期，聚集阈值是筛查参数。")
      )
    )
  )
}

reports_server <- function(id, data, all_data, time_col, region_label, source_label, data_quality,
                           population_data = function() NULL, admin_map_data = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(all_data(), {
      dates <- all_data()$发病日期
      if (length(dates) && any(!is.na(dates))) {
        bounds <- range(dates, na.rm = TRUE)
        shiny::updateDateInput(session, "anchor", value = bounds[2])
        shiny::updateDateRangeInput(session, "special_dates", start = max(bounds[1], bounds[2] - 29), end = bounds[2])
        shiny::updateDateRangeInput(session, "coverage", start = bounds[1], end = bounds[2])
        shiny::updateCheckboxInput(session, "complete_coverage", value = FALSE)
      }
    })
    shiny::observe({
      choices <- sort(unique(as.character(data()$标准病种)))
      shiny::updateSelectizeInput(session, "diseases", choices = choices,
        selected = intersect(shiny::isolate(input$diseases), choices), server = TRUE)
    })
    period <- shiny::reactive({
      shiny::req(input$type, input$anchor)
      if (input$type == "special") shiny::req(input$special_dates)
      result <- report_period(input$type, as.Date(input$anchor), date_range = if (input$type == "special") as.Date(input$special_dates) else NULL)
      shiny::validate(shiny::need(length(result) == 2 && all(!is.na(result)) && result[1] <= result[2], "请选择有效日期范围。"))
      shiny::validate(shiny::need(as.integer(result[2] - result[1]) <= 3660, "单份报告最多覆盖 10 年，请缩短区间。"))
      result
    })
    scoped <- shiny::reactive({
      x <- data()
      if (identical(input$type, "special") && length(input$diseases)) x <- x[x$标准病种 %in% input$diseases, , drop = FALSE]
      x
    })
    alerts <- shiny::reactive({
      dates <- period()
      shiny::validate(shiny::need(length(input$threshold) == 1 && is.finite(input$threshold) && input$threshold >= 2 && input$threshold == floor(input$threshold), "病例阈值须为至少 2 的整数。"))
      shiny::validate(shiny::need(length(input$span) == 1 && is.finite(input$span) && input$span >= 1 && input$span <= 30 && input$span == floor(input$span), "时间跨度须为 1 至 30 天的整数。"))
      detect_outbreak_clusters(scoped(), start_date = dates[1], end_date = dates[2], threshold = input$threshold, max_span_days = input$span)
    })
    micro_alerts <- shiny::reactive({
      institution_alerts <- alerts()
      dates <- period()
      candidates <- build_outbreak_candidate_cards(scoped(), start_date = dates[1], end_date = dates[2], date_col = "发病日期")
      cards <- filter_outbreak_cards_by_alerts(candidates, institution_alerts)
      detect_outbreak_micro_alerts(cards, threshold = input$threshold)
    })
    payload <- shiny::reactive({
      shiny::req(time_col())
      x <- scoped()
      attr(x, "data_quality") <- data_quality()
      coverage <- NULL
      if (isTRUE(input$complete_coverage)) {
        shiny::req(input$coverage)
        coverage <- as.Date(input$coverage)
        shiny::validate(shiny::need(length(coverage) == 2 && all(!is.na(coverage)) && coverage[1] <= coverage[2], "请输入已核实的完整数据覆盖期。"))
      }
      build_report_payload(
        data = x, date_range = period(), report_type = input$type, time_col = time_col(),
        region_label = region_label(), outbreak_alerts = alerts(),
        outbreak_micro_alerts = micro_alerts(), population_data = population_data(), admin_map_data = admin_map_data(),
        include_place_names = !identical(input$include_place_names, FALSE),
        outbreak_params = list(threshold = input$threshold, max_span_days = input$span),
        coverage_range = coverage, topic_title = if (input$type == "special") input$topic else NULL,
        filter_note = paste0("数据来源：", source_label(), "。沿用左侧管理级别、传播方式和病种范围；当前范围病种：",
          if (nrow(x)) paste(sort(unique(x$标准病种)), collapse = "、") else "空筛选结果", "。地区未匹配记录保留并列为未知。预警独立按本报告区间的发病日期计算。")
      )
    })
    output$quality <- DT::renderDT({
      DT::datatable(data_quality(), rownames = FALSE, options = list(dom = "t", paging = FALSE), escape = TRUE)
    })
    output$alerts <- DT::renderDT({
      x <- payload()$outbreak_details
      cols <- intersect(c("线索编号", "场所类型", "涉及区县", "机构名称", "疾病名称", "聚集病例数", "时间跨度天数", "子部门分布"), names(x))
      x <- x[, cols, drop = FALSE]
      DT::datatable(x, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE,
        language = list(search = "搜索：", lengthMenu = "每页 _MENU_ 条", info = "共 _TOTAL_ 条线索", infoEmpty = "暂无聚集线索", emptyTable = "该区间未检出达到阈值的聚集线索", paginate = list(previous = "上一页", `next` = "下一页"))), escape = TRUE)
    })
    output$preview <- shiny::renderUI({
      p <- payload()
      dates <- period()
      x <- scoped(); actual_dates <- as.Date(x[[time_col()]], tz = "Asia/Shanghai")
      cases <- sum(!is.na(actual_dates) & actual_dates >= dates[1] & actual_dates <= dates[2])
      shiny::tagList(
        shiny::h3(paste0(region_label(), "传染病疫情", c(monthly = "月报", weekly = "周报", special = "专题分析")[[input$type]])),
        shiny::p(paste("统计期间：", dates[1], "至", dates[2], "；时间口径：", time_col())),
        shiny::p(paste("期间有效病例：", cases, "例；机构聚集线索：", nrow(alerts()), "条；子部门判定结果：", nrow(micro_alerts()), "条。")),
        shiny::p(if (isTRUE(input$complete_coverage)) "比较指标仅在已核实覆盖期内生成。" else "尚未确认数据完整覆盖；报告会标明无法判定环比/同比。"),
        shiny::p(paste("当前来源：", source_label())),
        shiny::h4("完整报告内容（默认全部生成）"),
        shiny::tags$ol(
          shiny::tags$li("总体疫情：病例、病种、地区、人口指标，环比与同比及自动摘要。"),
          shiny::tags$li("管理级别与传播途径：分类构成、病种统计与图表。"),
          shiny::tags$li("病种分析：病例排行、构成比、主要病种对比及前3位病种的时间、地区和人群画像。"),
          shiny::tags$li("时间分析：每日曲线、周期汇总、病种时间分布及高峰解读。"),
          shiny::tags$li("地区分析：病例数与每10万人口指标、地区地图、地区趋势及病种分布。"),
          shiny::tags$li("人群分析：人群分类、性别、年龄组及年龄—性别分布。"),
          shiny::tags$li("重点场所预警：机构与子部门判定、时间窗口、病例数、规则和核查建议。"),
          shiny::tags$li("数据质量与方法：清洗记录、缺失信息、统计口径、数据完整性与局限。")
        ),
        shiny::p(if (is.null(admin_map_data())) "当前未配置可用地图；报告保留地区统计表和图，并说明地图缺失。" else "报告将使用当前已匹配的行政区划地图。"),
        shiny::p(if (!identical(input$include_place_names, FALSE)) "具体机构名称、子部门名称及各部门病例数将写入报告。" else "报告中的场所与子部门采用匿名编号。"),
        shiny::p("聚集线索按场所来源、地区、核心机构和原始病名分别统计；同一病例可能参与不同来源的线索，不能把线索病例数相加作为独立病例数。")
      )
    })
    output$pdf_button <- shiny::renderUI({
      if (isTRUE(report_export_capabilities()$pdf)) {
        shiny::downloadButton(session$ns("pdf"), "一键生成 PDF", class = "btn-primary")
      } else {
        shiny::tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "PDF 环境待配置")
      }
    })
    output$capabilities <- shiny::renderUI({
      cap <- report_export_capabilities()
      shiny::p(class = "module-note", if (isTRUE(cap$pdf)) "PDF 转换环境已就绪。" else "PDF 需要 LibreOffice；Word 可独立生成。转换环境说明见部署文档。")
    })
    for (fmt in c("docx", "pdf")) local({
      format <- fmt
      output[[if (format == "docx") "word" else "pdf"]] <- shiny::downloadHandler(
        filename = function() paste0(c(monthly = "月报", weekly = "周报", special = "专题分析")[[input$type]], "_", format(period()[1], "%Y%m%d"), "_", format(period()[2], "%Y%m%d"), ".", format),
        contentType = if (format == "docx") "application/vnd.openxmlformats-officedocument.wordprocessingml.document" else "application/pdf",
        content = function(file) {
          tryCatch(shiny::withProgress(message = "正在生成报告", value = 0.15, {
            p <- payload(); shiny::setProgress(0.45, detail = "正在排版图表与分析摘要")
            write_surveillance_report(p, file = file, format = format)
            shiny::setProgress(1)
          }), error = function(e) {
            shiny::showNotification(paste("报告生成失败：", conditionMessage(e)), type = "error", duration = NULL)
            stop(e)
          })
        }
      )
    })
  })
}
