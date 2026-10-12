# Run from the project root. All fixtures are synthetic; no business data is read.
source("data_preprocessing.R", encoding = "UTF-8")

run_population_upload_checks <- function() {
  checks <- 0L
  expect <- function(ok, label) {
    if (!isTRUE(ok)) stop("FAIL: ", label, call. = FALSE)
    checks <<- checks + 1L
    cat("PASS:", label, "\n")
  }
  expect_error <- function(expr, pattern, label) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    expect(inherits(error, "error") && grepl(pattern, conditionMessage(error)), label)
  }
  work <- tempfile("population-upload-test-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  fixture <- data.frame(
    地区名称 = c("演示甲区", "演示乙区"), 人口数 = c(100000, 200000),
    PAC = c("990101", "990102"), 匹配模式 = c("演示甲区|甲区", "演示乙区|乙区"),
    check.names = FALSE
  )
  csv <- file.path(work, "population.csv")
  readr::write_excel_csv(fixture, csv)
  normal <- load_population_data(csv)
  expect(nrow(normal) == 2L && sum(normal$人口数) == 300000, "CSV 按人读取两地区人口")
  expect(setequal(normal$PAC6, c("990101", "990102")), "六位 PAC 在导入后保留")
  upload <- file.path(work, "upload-csv")
  file.copy(csv, upload)
  uploaded <- load_population_data(upload, source_label = "合成上传", file_name = "人口.CSV")
  expect(identical(normal$人口数, uploaded$人口数) && all(uploaded$数据源 == "合成上传"),
    "无扩展名 CSV 可传原始文件名并保留数据源")
  expect(identical(as.data.frame(read_population_file(csv)), as.data.frame(read_population_file(upload))),
    "原有无扩展名 CSV 调用保持兼容")
  expect_error(read_population_file(upload, file_name = NA_character_), "原始文件名", "无效文件名报错清楚")

  xlsx <- "data/population_template.xlsx"
  expect(file.exists(xlsx), "合成 Excel 模板存在")
  xlsx_rows <- read_population_file(xlsx)
  xlsx_normal <- load_population_data(xlsx)
  expect(nrow(xlsx_rows) == 3L && all(c("地区名称", "人口数", "PAC") %in% names(xlsx_rows)),
    "Excel 仅读第一表人口参数，不读取第二表填写说明")
  xlsx_upload <- file.path(work, "upload-excel")
  file.copy(xlsx, xlsx_upload)
  expect(identical(xlsx_rows, read_population_file(xlsx_upload, file_name = "人口模板.XLSX")),
    "无扩展名 XLSX 按原始文件名正确读取")
  xlsx_uploaded <- load_population_data(xlsx_upload, file_name = "人口模板.xlsx")
  expect(identical(xlsx_normal$人口数, xlsx_uploaded$人口数) && nrow(xlsx_uploaded) > 0L,
    "无扩展名 XLSX 完成加载和标准化")

  units <- parse_population_value(c("100000", "100,000", "100，000人", "10万", "10万人", "0.5 万人", "1e5"))
  expect(identical(units, c(100000, 100000, 100000, 100000, 100000, 5000, 100000)),
    "纯数字、千位分隔、人与万人单位换算正确")
  expect(all(is.na(parse_population_value(c("10万2", "10亿", "人100", "Inf", "NaN", "1e309", NA_character_)))),
    "非法单位和非有限人口不会误解析成有效人口")
  invalid <- data.frame(地区名称 = c("甲区", "乙区", "丙区", "丁区", "戊区", "己区", "庚区", "辛区", ""),
    人口数 = c("100000", "0", "-1", "Inf", "NaN", "1e309", "缺失", NA, "300000"), check.names = FALSE)
  valid_only <- normalize_population_data(invalid)
  expect(nrow(valid_only) == 1L && valid_only$地区名称 == "甲区" && valid_only$人口数 == 100000,
    "仅有限正人口和非空地区名称参与分析")
  expect_error(normalize_population_data(invalid[-1, ]), "有效", "全部人口无效时拒绝上传")

  repeated <- rbind(fixture, fixture[1, ])
  repeated$PAC[3] <- "156990101"
  repeated$匹配模式[3] <- "甲地"
  deduplicated <- normalize_population_data(repeated)
  expect(nrow(deduplicated) == 2L && sum(deduplicated$人口数) == 300000,
    "同地区同 PAC 重复人口只取一次，不累加分母")
  expect(grepl("甲地", deduplicated$匹配模式[deduplicated$地区名称 == "演示甲区"], fixed = TRUE),
    "相同人口的重复行保留各地址别名")
  repeated$人口数[3] <- 110000
  expect_error(normalize_population_data(repeated), "演示甲区", "重复人口冲突拒绝整表并指出地区")
  readr::write_excel_csv(repeated, csv)
  expect_error(load_population_data(csv), "人口数不一致", "文件上传流程同样拒绝重复人口冲突")

  no_pac <- fixture[, c("地区名称", "人口数", "匹配模式")]
  no_pac_repeated <- normalize_population_data(rbind(no_pac, no_pac[1, ]))
  expect(nrow(no_pac_repeated) == 2L && sum(no_pac_repeated$人口数) == 300000,
    "未提供 PAC 时同地区同值仍去重")
  conflict_no_pac <- rbind(no_pac, no_pac[1, ])
  conflict_no_pac$人口数[3] <- 150000
  expect_error(normalize_population_data(conflict_no_pac), "演示甲区", "未提供 PAC 时重复冲突仍拒绝")
  cases <- data.frame(
    区县 = c("演示乙区", "演示甲区", "演示甲区", "演示乙区", "演示甲区", NA),
    现住地址国标 = c(NA, "", "not-a-code", "990101", "990999", NA),
    现住详细地址 = c("甲区合成路", "无地区信息", "乙区合成路", "乙区合成路", "乙区合成路", NA),
    check.names = FALSE
  )
  no_pac_matched <- match_surveillance_to_population(cases, no_pac_repeated)
  expect(identical(no_pac_matched$区县, c("演示甲区", "演示甲区", "演示乙区", "演示乙区", "演示乙区", NA_character_)),
    "无 PAC 人口表按地址及原区县回退，不误配缺失码")
  mixed <- fixture
  mixed$PAC[2] <- NA_character_
  mixed_matched <- match_surveillance_to_population(cases, normalize_population_data(mixed))
  expect(identical(mixed_matched$区县, c("演示甲区", "演示甲区", "演示乙区", "演示甲区", "演示乙区", NA_character_)),
    "部分 PAC 缺失时只匹配有效码，缺失病例码按地址或原区县回退")
  cases$现住地址国标[1] <- "156990102"
  full_matched <- match_surveillance_to_population(cases, normal)
  expect(full_matched$区县[1] == "演示乙区", "含 156 前缀的有效 PAC 优先于冲突地址")
  cat("Population upload checks passed:", checks, "\n")
}

run_population_upload_checks()
