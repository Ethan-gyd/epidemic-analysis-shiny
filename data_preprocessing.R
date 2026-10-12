# 昆明市传染病疫情自动化监测与分析系统
# 数据预处理、字典映射与通用清洗函数

suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(lubridate)
  library(purrr)
  library(readr)
  library(sf)
  library(stringr)
  library(tibble)
  library(tidyr)
})

# 2025 年昆明市人口基础数据
km_population_2025 <- tibble::tribble(
  ~区县,     ~人口数,
  "昆明市",  8687000,
  "五华区",  1181900,
  "盘龙区",  1027500,
  "官渡区",  1625800,
  "西山区",   982900,
  "东川区",   253100,
  "呈贡区",   725100,
  "晋宁区",   348500,
  "富民县",   152400,
  "宜良县",   377000,
  "石林县",   235900,
  "嵩明县",   434700,
  "禄劝县",   369900,
  "寻甸县",   460900,
  "安宁市",   511400
)

km_district_population <- km_population_2025 %>%
  filter(区县 != "昆明市")

default_county_map_candidates <- c(
  file.path("China_county", "中国_县.shp"),
  "China_county.shp",
  file.path("中国标准行政区划数据GS（2024）0650号_原始数据", "shp格式", "中国_县.shp")
)

default_city_map_candidates <- c(
  file.path("China_county", "中国_市.shp"),
  file.path("中国标准行政区划数据GS（2024）0650号_原始数据", "shp格式", "中国_市.shp")
)

default_province_map_candidates <- c(
  file.path("中国标准行政区划数据GS（2024）0650号_原始数据", "shp格式", "中国_省.shp")
)

# 区县名称别名映射，用于从现住详细地址中提取标准区县名称
district_alias_mapping <- tibble::tribble(
  ~区县,    ~匹配模式,
  "五华区", "五华区",
  "盘龙区", "盘龙区",
  "官渡区", "官渡区",
  "西山区", "西山区",
  "东川区", "东川区",
  "呈贡区", "呈贡区|呈贡县",
  "晋宁区", "晋宁区|晋宁县",
  "富民县", "富民县",
  "宜良县", "宜良县",
  "石林县", "石林彝族自治县|石林县",
  "嵩明县", "嵩明县",
  "禄劝县", "禄劝彝族苗族自治县|禄劝县",
  "寻甸县", "寻甸回族彝族自治县|寻甸县",
  "安宁市", "安宁市"
)

normalize_management_level <- function(x) {
  x <- str_squish(replace_na(as.character(x), ""))

  case_when(
    str_detect(x, "甲") ~ "甲类",
    str_detect(x, "乙") ~ "乙类",
    str_detect(x, "丙") ~ "丙类",
    str_detect(x, "重点") ~ "重点监测",
    str_detect(x, "补充") ~ "补充监测",
    str_detect(x, "其他|其它") ~ "补充监测",
    TRUE ~ x
  )
}

normalize_transmission_route_label <- function(x) {
  x <- str_squish(replace_na(as.character(x), ""))

  case_when(
    str_detect(x, "呼吸") ~ "呼吸道传染病",
    str_detect(x, "消化|肠道|粪") ~ "消化道传染病",
    str_detect(x, "自然疫源|虫媒") ~ "自然疫源地及虫媒传染病",
    str_detect(x, "血源|性传播") ~ "血源及性传播传染病",
    str_detect(x, "分亚型") ~ "分亚型对应",
    TRUE ~ x
  )
}

# 规范病种字典。优先读取同目录 disease_dictionary.csv，缺失时使用此内置兜底表。
default_disease_dictionary <- tibble::tribble(
  ~标准病种, ~管理级别, ~默认传播途径, ~备注, ~匹配模式,
  "鼠疫", "甲类", "自然疫源地及虫媒传染病", "主要通过鼠蚤叮咬传播，肺鼠疫可经呼吸道飞沫传播", "鼠疫",
  "霍乱", "甲类", "消化道传染病", "核心为粪-口途径，通过污染的水源、食物传播", "霍乱",
  "传染性非典型肺炎（SARS）", "乙类", "呼吸道传染病", "主要经呼吸道飞沫、密切接触传播", "传染性非典型肺炎|SARS",
  "艾滋病（AIDS）", "乙类", "血源及性传播传染病", "核心传播途径为性接触、血液/体液、母婴垂直传播", "艾滋病|HIV",
  "病毒性肝炎", "乙类", "分亚型对应", "甲型/戊型肝炎归入消化道；乙型/丙型/丁型肝炎归入血源及性传播", "甲肝|乙肝|丙肝|丁肝|戊肝|病毒性肝炎",
  "脊髓灰质炎（小儿麻痹症）", "乙类", "消化道传染病", "核心为粪-口途径传播", "脊髓灰质炎|小儿麻痹",
  "人感染高致病性禽流感", "乙类", "呼吸道传染病", "主要经禽-人呼吸道传播，仅存在有限人传人", "人感染高致病性禽流感",
  "麻疹", "乙类", "呼吸道传染病", "经呼吸道飞沫传播，传染性极强", "麻疹",
  "肾综合征出血热（流行性出血热）", "乙类", "自然疫源地及虫媒传染病", "主要通过接触鼠类排泄物感染，汉坦病毒为病原体", "肾综合征出血热|流行性出血热|出血热",
  "狂犬病", "乙类", "自然疫源地及虫媒传染病", "主要通过患病动物咬伤、抓伤传播，自然疫源性疾病", "狂犬病",
  "流行性乙型脑炎（乙脑）", "乙类", "自然疫源地及虫媒传染病", "经蚊媒叮咬传播，猪为主要扩增宿主", "流行性乙型脑炎|乙脑",
  "登革热", "乙类", "自然疫源地及虫媒传染病", "经伊蚊叮咬传播，虫媒传播为唯一途径", "登革热",
  "炭疽", "乙类", "自然疫源地及虫媒传染病", "主要通过接触病畜/污染畜产品感染，亦可经呼吸道、消化道传播", "炭疽",
  "细菌性和阿米巴性痢疾", "乙类", "消化道传染病", "核心为粪-口途径传播", "细菌性痢疾|阿米巴性痢疾",
  "肺结核", "乙类", "呼吸道传染病", "主要经呼吸道飞沫传播", "肺结核|肺外结核|结核",
  "伤寒和副伤寒", "乙类", "消化道传染病", "核心为粪-口途径传播", "伤寒|副伤寒",
  "流行性脑脊髓膜炎（流脑）", "乙类", "呼吸道传染病", "经呼吸道飞沫传播", "流行性脑脊髓膜炎|流脑",
  "百日咳", "乙类", "呼吸道传染病", "经呼吸道飞沫传播，儿童为高发人群", "百日咳",
  "白喉", "乙类", "呼吸道传染病", "经呼吸道飞沫传播", "白喉",
  "新生儿破伤风", "乙类", "自然疫源地及虫媒传染病", "主要通过接触自然环境中破伤风梭菌经伤口感染，不洁分娩为主要感染途径", "新生儿破伤风",
  "猩红热", "乙类", "呼吸道传染病", "经呼吸道飞沫传播，病原体为A组溶血性链球菌", "猩红热",
  "布鲁氏菌病（布病）", "乙类", "自然疫源地及虫媒传染病", "主要通过接触病畜/畜产品感染，自然疫源性疾病", "布鲁氏菌病|布病",
  "淋病", "乙类", "血源及性传播传染病", "核心为性接触传播", "淋病",
  "梅毒", "乙类", "血源及性传播传染病", "主要经性接触、母婴垂直传播，亦可经血液传播", "一期梅毒|二期梅毒|三期梅毒|隐性梅毒|梅毒",
  "钩端螺旋体病（钩体病）", "乙类", "自然疫源地及虫媒传染病", "主要通过接触疫水感染，鼠、猪为主要宿主", "钩端|钩体",
  "血吸虫病", "乙类", "自然疫源地及虫媒传染病", "主要通过接触含尾蚴的疫水感染，钉螺为唯一中间宿主", "血吸虫",
  "疟疾", "乙类", "自然疫源地及虫媒传染病", "经按蚊叮咬传播，虫媒传播为核心途径", "间日疟|恶性疟|疟疾|疟",
  "人感染 H7N9 禽流感", "乙类", "呼吸道传染病", "主要经禽-人呼吸道传播，无持续人传人证据", "H7N9",
  "新型冠状病毒感染", "乙类", "呼吸道传染病", "主要经呼吸道飞沫、气溶胶、密切接触传播", "新型冠状病毒感染|新冠",
  "猴痘", "乙类", "血源及性传播传染病", "国内流行以性接触密切传播为核心，亦可经呼吸道飞沫、污染物品传播", "猴痘",
  "流行性感冒（流感）", "丙类", "呼吸道传染病", "经呼吸道飞沫传播，含甲型 H1N1 流感", "流行性感冒|甲型H1N1|甲型 H1N1|流感",
  "流行性腮腺炎", "丙类", "呼吸道传染病", "经呼吸道飞沫传播", "流行性腮腺炎",
  "风疹", "丙类", "呼吸道传染病", "经呼吸道飞沫传播，亦可经母婴垂直传播", "风疹",
  "急性出血性结膜炎", "丙类", "消化道传染病", "主要通过接触污染物品/水源传播，亦可经粪-口途径传播", "急性出血性结膜炎",
  "麻风病", "丙类", "呼吸道传染病", "主要通过长期密切接触、呼吸道飞沫传播", "麻风病",
  "流行性和地方性斑疹伤寒", "丙类", "自然疫源地及虫媒传染病", "流行性斑疹伤寒经人虱传播，地方性斑疹伤寒经鼠蚤传播", "斑疹伤寒",
  "黑热病", "丙类", "自然疫源地及虫媒传染病", "经白蛉叮咬传播", "黑热病",
  "包虫病（棘球蚴病）", "丙类", "自然疫源地及虫媒传染病", "主要通过接触病畜/污染环境感染，自然疫源性疾病", "包虫病|棘球蚴病",
  "丝虫病", "丙类", "自然疫源地及虫媒传染病", "经蚊媒叮咬传播", "丝虫病",
  "其他感染性腹泻病", "丙类", "消化道传染病", "特指除霍乱、菌痢、伤寒副伤寒外的感染性腹泻，核心为粪-口传播", "其他感染性腹泻病|其它感染性腹泻病",
  "手足口病", "丙类", "消化道传染病", "核心为粪-口途径传播，亦可经接触、呼吸道飞沫传播", "手足口病",
  "水痘", "其它传染病", "呼吸道传染病", "全国疾控常规重点监测传染病，主要经呼吸道飞沫、密切接触传播", "水痘"
)

normalize_disease_dictionary <- function(dictionary_data) {
  if (!"标准病种" %in% names(dictionary_data) && "传染病规范名称" %in% names(dictionary_data)) {
    dictionary_data <- dictionary_data %>% rename(标准病种 = all_of("传染病规范名称"))
  }
  if (!"管理级别" %in% names(dictionary_data) && "管理级别分类" %in% names(dictionary_data)) {
    dictionary_data <- dictionary_data %>% rename(管理级别 = all_of("管理级别分类"))
  }
  if (!"默认传播途径" %in% names(dictionary_data) && "主要传播方式分类" %in% names(dictionary_data)) {
    dictionary_data <- dictionary_data %>% rename(默认传播途径 = all_of("主要传播方式分类"))
  }
  if (!"默认传播途径" %in% names(dictionary_data) && "主要传播途径" %in% names(dictionary_data)) {
    dictionary_data <- dictionary_data %>% rename(默认传播途径 = all_of("主要传播途径"))
  }
  if (!"备注" %in% names(dictionary_data) && "备注（次要传播途径 / 特殊说明）" %in% names(dictionary_data)) {
    dictionary_data <- dictionary_data %>% rename(备注 = all_of("备注（次要传播途径 / 特殊说明）"))
  }
  if (!"匹配模式" %in% names(dictionary_data)) {
    dictionary_data <- dictionary_data %>% mutate(匹配模式 = .data$标准病种)
  }

  required_columns <- c("标准病种", "管理级别", "默认传播途径", "备注", "匹配模式")
  missing_columns <- setdiff(required_columns, names(dictionary_data))

  if (length(missing_columns) > 0) {
    stop(glue::glue("disease_dictionary.csv 缺少必要字段：{paste(missing_columns, collapse = '、')}"))
  }

  dictionary_data %>%
    transmute(
      标准病种 = str_squish(as.character(.data$标准病种)),
      匹配模式 = if_else(
        is.na(.data$匹配模式) | str_squish(as.character(.data$匹配模式)) == "",
        as.character(.data$标准病种),
        str_squish(as.character(.data$匹配模式))
      ),
      管理级别 = normalize_management_level(.data$管理级别),
      主要传播途径 = normalize_transmission_route_label(.data$默认传播途径),
      备注 = str_squish(replace_na(as.character(.data$备注), ""))
    ) %>%
    mutate(默认传播途径 = .data$主要传播途径) %>%
    filter(.data$标准病种 != "", .data$管理级别 != "", .data$主要传播途径 != "") %>%
    distinct(.data$标准病种, .keep_all = TRUE)
}

load_disease_dictionary <- function(path = "disease_dictionary.csv") {
  if (file.exists(path)) {
    readr::read_csv(
      path,
      locale = locale(encoding = "UTF-8"),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      normalize_disease_dictionary()
  } else {
    normalize_disease_dictionary(default_disease_dictionary)
  }
}

disease_dictionary <- load_disease_dictionary()

disease_management_levels <- c("甲类", "乙类", "丙类", "重点监测", "补充监测", "未分类")
disease_transmission_routes <- c(
  "呼吸道传染病",
  "消化道传染病",
  "自然疫源地及虫媒传染病",
  "血源及性传播传染病",
  "分亚型对应",
  "未分类"
)

# 对 42 种规范病种之外的附加病种做兜底分类，避免数据遗漏
supplementary_management_rules <- tibble::tribble(
  ~匹配模式, ~管理级别,
  "尖锐湿疣|生殖器疱疹|生殖道沙眼衣原体感染|非淋菌性尿道炎", "未分类",
  "其它疾病|其它:|其他疾病", "未分类"
)

supplementary_transmission_rules <- tibble::tribble(
  ~匹配模式, ~传播途径,
  "尖锐湿疣|生殖器疱疹|生殖道沙眼衣原体感染|非淋菌性尿道炎", "血源及性传播传染病",
  "呼吸道合胞病毒|副流感|腺病毒|肺炎支原体", "呼吸道传染病",
  "诺如|轮状病毒|腹泻", "消化道传染病"
)

# 各类重点场所关键字，用于暴发疫情自动识别
priority_place_keywords <- c(
  "医院", "卫生院", "诊所", "小学", "中学", "学校", "大学", "学院",
  "幼儿园", "托育", "培训机构", "养老院", "福利院", "敬老院", "监狱",
  "看守所", "拘留所", "工地", "厂", "工厂", "宿舍"
)

normalize_text <- function(x) {
  x %>%
    replace_na("") %>%
    str_replace_all("[\r\n\t]+", " ") %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_trim()
}

empty_to_na <- function(x) {
  x <- normalize_text(x)
  x[x %in% c("", ".", "NA", "N/A", "NULL", "null")] <- NA_character_
  x
}

parse_cn_date <- function(x) {
  x <- empty_to_na(x)
  suppressWarnings(
    parse_date_time(
      x,
      orders = c("Ymd", "Y-m-d", "Y/m/d", "Ymd HMS", "Y-m-d H:M:S", "Y/m/d H:M:S"),
      tz = "Asia/Shanghai"
    )
  ) %>%
    as.Date(tz = "Asia/Shanghai")
}

parse_cn_datetime <- function(x) {
  x <- empty_to_na(x)
  suppressWarnings(
    parse_date_time(
      x,
      orders = c(
        "Ymd HMS", "Y-m-d H:M:S", "Y/m/d H:M:S",
        "Ymd HM", "Y-m-d H:M", "Y/m/d H:M",
        "Ymd", "Y-m-d", "Y/m/d"
      ),
      tz = "Asia/Shanghai"
    )
  )
}

standardize_gender <- function(x) {
  x <- normalize_text(x)
  case_when(
    x %in% c("男", "男性", "1") ~ "男",
    x %in% c("女", "女性", "2") ~ "女",
    TRUE ~ "其他/未知"
  )
}

parse_age_years <- function(age_text) {
  age_text <- normalize_text(age_text)
  age_num <- suppressWarnings(as.numeric(str_extract(age_text, "\\d+(?:\\.\\d+)?")))

  case_when(
    is.na(age_num) ~ NA_real_,
    str_detect(age_text, "岁") ~ age_num,
    str_detect(age_text, "月") ~ age_num / 12,
    str_detect(age_text, "天") ~ age_num / 365.25,
    str_detect(age_text, "周") ~ age_num * 7 / 365.25,
    TRUE ~ age_num
  )
}

calculate_age_years <- function(age_text, birth_date, onset_date) {
  age_from_birth <- suppressWarnings(as.numeric(difftime(onset_date, birth_date, units = "days")) / 365.25)
  age_from_text <- parse_age_years(age_text)

  if_else(!is.na(age_from_birth) & age_from_birth >= 0, age_from_birth, age_from_text)
}

make_age_group <- function(age_years) {
  breaks <- c(0, seq(5, 85, by = 5), Inf)
  labels <- c(
    "0-4岁", "5-9岁", "10-14岁", "15-19岁", "20-24岁", "25-29岁", "30-34岁",
    "35-39岁", "40-44岁", "45-49岁", "50-54岁", "55-59岁", "60-64岁",
    "65-69岁", "70-74岁", "75-79岁", "80-84岁", "85岁及以上"
  )

  cut(age_years, breaks = breaks, labels = labels, right = FALSE, ordered_result = TRUE)
}

match_first_rule <- function(values, rules, output_col, default_value) {
  values <- empty_to_na(values)

  map_chr(values, function(value) {
    if (is.na(value)) {
      return(default_value)
    }

    hit <- rules %>%
      filter(str_detect(value, regex(.data$匹配模式))) %>%
      pull({{ output_col }}) %>%
      unique()

    if (length(hit) == 0) {
      default_value
    } else {
      hit[[1]]
    }
  })
}

lookup_disease_dictionary <- function(values) {
  values <- empty_to_na(values)

  purrr::map_dfr(values, function(value) {
    if (is.na(value)) {
      return(
        tibble(
          标准病种 = NA_character_,
          管理级别 = "未分类",
          传播途径 = "未分类",
          病种备注 = NA_character_
        )
      )
    }

    hit <- disease_dictionary %>%
      filter(str_detect(value, regex(.data$匹配模式, ignore_case = TRUE))) %>%
      slice_head(n = 1)

    if (nrow(hit) == 0) {
      fallback_management <- match_first_rule(value, supplementary_management_rules, 管理级别, "未分类")
      fallback_route <- match_first_rule(value, supplementary_transmission_rules, 传播途径, "未分类")

      fallback_standard <- case_when(
        str_detect(value, "其它疾病|其它:|其他疾病") ~ "其他监测病种",
        TRUE ~ value
      )

      return(
        tibble(
          标准病种 = fallback_standard,
          管理级别 = fallback_management,
          传播途径 = fallback_route,
          病种备注 = "未纳入42种规范病种字典，按补充规则归类"
        )
      )
    }

    tibble(
      标准病种 = hit$标准病种,
      管理级别 = hit$管理级别,
      传播途径 = hit$默认传播途径,
      病种备注 = hit$备注
    )
  })
}

resolve_hepatitis_route <- function(disease_name, current_route) {
  disease_name <- normalize_text(disease_name)

  case_when(
    str_detect(disease_name, "甲肝|戊肝") ~ "消化道传染病",
    str_detect(disease_name, "乙肝|丙肝|丁肝") ~ "血源及性传播传染病",
    TRUE ~ current_route
  )
}

resolve_transmission_route <- function(disease_name, standard_disease, current_route) {
  case_when(
    standard_disease == "病毒性肝炎" ~ resolve_hepatitis_route(disease_name, current_route),
    current_route == "分亚型对应" ~ "未分类",
    TRUE ~ current_route
  )
}

extract_km_district <- function(address) {
  address <- empty_to_na(address)

  map_chr(address, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    hit <- district_alias_mapping %>%
      filter(str_detect(value, regex(.data$匹配模式))) %>%
      pull(区县) %>%
      unique()

    if (length(hit) == 0) {
      NA_character_
    } else {
      hit[[1]]
    }
  })
}

standardize_km_district_name <- function(x) {
  x <- empty_to_na(x)

  case_when(
    x %in% c("石林彝族自治县", "石林县") ~ "石林县",
    x %in% c("禄劝彝族苗族自治县", "禄劝县") ~ "禄劝县",
    x %in% c("寻甸回族彝族自治县", "寻甸县") ~ "寻甸县",
    x %in% c("呈贡县", "呈贡区") ~ "呈贡区",
    x %in% c("晋宁县", "晋宁区") ~ "晋宁区",
    TRUE ~ x
  )
}

standardize_admin_name <- function(x) {
  standardize_km_district_name(x)
}

normalize_pac_code <- function(x) {
  x <- normalize_text(as.character(x)) %>%
    str_replace_all("[^0-9]", "")

  if_else(x == "", NA_character_, x)
}

make_pac6 <- function(x) {
  code <- normalize_pac_code(x)

  case_when(
    is.na(code) ~ NA_character_,
    str_length(code) >= 9 & str_starts(code, "156") ~ str_sub(code, 4, 9),
    str_length(code) >= 6 ~ str_sub(code, 1, 6),
    TRUE ~ NA_character_
  )
}

make_full_pac <- function(x) {
  code <- normalize_pac_code(x)

  case_when(
    is.na(code) ~ NA_character_,
    str_length(code) == 6 ~ paste0("156", code),
    str_length(code) >= 9 & str_starts(code, "156") ~ str_sub(code, 1, 9),
    str_length(code) >= 9 ~ paste0("156", str_sub(code, 1, 6)),
    TRUE ~ code
  )
}

detect_population_pac_column <- function(data) {
  candidate_columns <- c(
    "PAC", "pac", "Pac", "行政区划代码", "行政区划编码", "地区编码",
    "区划代码", "区划编码", "国标码", "国标编码", "代码", "编码", "gb", "GB", "adcode"
  )
  hit <- candidate_columns[candidate_columns %in% names(data)]

  if (length(hit) > 0) hit[[1]] else NA_character_
}

detect_population_pattern_column <- function(data) {
  candidate_columns <- c("匹配模式", "匹配规则", "别名", "地区别名", "地址匹配", "alias", "aliases")
  hit <- candidate_columns[candidate_columns %in% names(data)]

  if (length(hit) > 0) hit[[1]] else NA_character_
}

make_admin_aliases <- function(name, pattern = NULL) {
  name <- empty_to_na(name)
  pattern <- if (is.null(pattern)) rep(NA_character_, length(name)) else empty_to_na(pattern)

  purrr::map2(name, pattern, function(value, pattern_value) {
    if (is.na(value)) {
      aliases <- character()
    } else {
      aliases <- c(value, standardize_admin_name(value))
    }

    if (!is.na(pattern_value)) {
      aliases <- c(aliases, unlist(str_split(pattern_value, "\\|"), use.names = FALSE))
    }

    if (!is.na(value) && str_detect(value, "自治县$")) {
      short_name <- str_replace(value, "([一-龥]{1,4}).*自治县$", "\\1县")
      aliases <- c(aliases, short_name)
    }

    if (!is.na(value) && str_detect(value, "自治州$")) {
      short_name <- str_replace(value, "([一-龥]{1,4}).*自治州$", "\\1州")
      aliases <- c(aliases, short_name)
    }

    aliases %>%
      normalize_text() %>%
      unique() %>%
      discard(~ is.na(.x) || .x == "")
  })
}

parse_population_value <- function(x) {
  x <- normalize_text(as.character(x)) %>% str_replace_all("[,，\\s]", "")
  parts <- str_match(x, "^([+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?)(万人?|人)?$")
  number_value <- suppressWarnings(as.numeric(parts[, 2]))
  multiplier <- if_else(!is.na(parts[, 3]) & str_detect(parts[, 3], "万"), 10000, 1)
  value <- number_value * multiplier
  value[!is.finite(value)] <- NA_real_
  value
}

detect_population_name_column <- function(data) {
  candidate_columns <- c(
    "地区名称", "城市", "地区", "行政区划", "行政区划名称", "区县", "县区",
    "县市区", "县区名称", "市州", "州市", "名称", "name", "Name", "NAME"
  )
  hit <- candidate_columns[candidate_columns %in% names(data)]

  if (length(hit) > 0) {
    return(hit[[1]])
  }

  character_columns <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x), logical(1))]

  if (length(character_columns) == 0) {
    stop("人口参数表中未找到地区名称列。")
  }

  character_columns[[1]]
}

detect_population_value_column <- function(data) {
  candidate_columns <- c(
    "人口数", "人口", "常住人口", "户籍人口", "人口总数",
    "2025人口", "2025年人口", "population", "pop", "POP"
  )
  hit <- candidate_columns[candidate_columns %in% names(data)]

  if (length(hit) > 0) {
    return(hit[[1]])
  }

  numeric_score <- purrr::map_dbl(data, function(column) {
    values <- parse_population_value(column)
    sum(!is.na(values) & values > 1000)
  })

  if (max(numeric_score, na.rm = TRUE) == 0) {
    stop("人口参数表中未找到人口数列。")
  }

  names(data)[which.max(numeric_score)]
}

read_population_file <- function(file_path, file_name = basename(file_path)) {
  if (length(file_name) != 1L || is.na(file_name) || !nzchar(file_name)) {
    stop("请提供人口上传文件的原始文件名。", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(file_name))

  if (extension %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("读取 Excel 人口参数需要安装 readxl 包。")
    }

    reader <- if (extension == "xlsx") readxl::read_xlsx else readxl::read_xls
    reader(file_path, .name_repair = "unique") %>%
      as_tibble()
  } else {
    encoding_guess <- tryCatch(
      readr::guess_encoding(file_path, n_max = 10000)$encoding[[1]],
      error = function(e) "GB18030"
    )

    if (is.null(encoding_guess) || is.na(encoding_guess)) {
      encoding_guess <- "GB18030"
    }

    readr::read_csv(
      file_path,
      locale = locale(encoding = encoding_guess),
      col_types = cols(.default = col_character()),
      na = c("", "NA", "NULL"),
      name_repair = "unique_quiet",
      show_col_types = FALSE
    )
  }
}

infer_population_scope <- function(area_names, province_names = NULL, city_names = NULL) {
  area_names <- empty_to_na(area_names) %>%
    discard(is.na)
  if (is.null(province_names)) {
    province_names <- character()
  }
  if (is.null(city_names)) {
    city_names <- character()
  }

  province_names <- empty_to_na(province_names) %>%
    discard(is.na) %>%
    unique()
  city_names <- empty_to_na(city_names) %>%
    discard(is.na) %>%
    unique()

  if (length(area_names) == 0) {
    return("上传地区")
  }

  if (length(city_names) == 1) {
    return(city_names[[1]])
  }

  if (length(province_names) == 1 && length(city_names) > 1) {
    return(province_names[[1]])
  }

  total_markers <- area_names[str_detect(area_names, "^(合计|总计|全市|全省)$")]

  if (length(total_markers) > 0) {
    return(total_markers[[1]])
  }

  province_rows <- area_names[str_detect(area_names, "省$|自治区$")]
  if (length(province_rows) > 0) {
    return(province_rows[[1]])
  }

  if ("昆明市" %in% area_names && any(standardize_admin_name(area_names) %in% km_district_population$区县)) {
    return("昆明市")
  }

  city_rows <- area_names[str_detect(area_names, "市$|自治州$|地区$|盟$")]
  county_like_rows <- area_names[str_detect(area_names, "区$|县$|旗$|市$")]

  if (length(city_rows) == 1 && length(county_like_rows) > 1) {
    return(city_rows[[1]])
  }

  if (all(standardize_admin_name(area_names) %in% km_district_population$区县)) {
    return("昆明市")
  }

  if (length(city_rows) > 1) {
    return("云南省")
  }

  "上传地区"
}

normalize_population_data <- function(data, source_label = "人口参数") {
  name_col <- detect_population_name_column(data)
  population_col <- detect_population_value_column(data)
  pac_col <- detect_population_pac_column(data)
  pattern_col <- detect_population_pattern_column(data)

  population <- data %>%
    transmute(
      地区原名 = normalize_text(as.character(.data[[name_col]])),
      地区名称 = standardize_admin_name(.data$地区原名),
      人口数 = parse_population_value(.data[[population_col]]),
      PAC = if (!is.na(pac_col)) make_full_pac(.data[[pac_col]]) else NA_character_,
      PAC6 = if (!is.na(pac_col)) make_pac6(.data[[pac_col]]) else NA_character_,
      匹配模式 = if (!is.na(pattern_col)) normalize_text(as.character(.data[[pattern_col]])) else .data$地区原名,
      省名称 = if ("ShengName" %in% names(data)) normalize_text(as.character(.data$ShengName)) else NA_character_,
      市名称 = if ("ShiName" %in% names(data)) normalize_text(as.character(.data$ShiName)) else NA_character_
    ) %>%
    filter(!is.na(.data$地区名称), .data$地区名称 != "", is.finite(.data$人口数), .data$人口数 > 0)

  conflicts <- population %>%
    group_by(.data$地区名称, .data$PAC, .data$PAC6) %>%
    summarise(人口值数量 = n_distinct(.data$人口数), .groups = "drop") %>%
    filter(.data$人口值数量 > 1L)
  if (nrow(conflicts) > 0L) {
    stop(paste0("人口参数存在重复地区且人口数不一致：", paste(unique(conflicts$地区名称), collapse = "、"),
      "。同一地区和行政区划代码只能保留一个人口数，请核实年份与人口口径后重新上传。"), call. = FALSE)
  }

  population <- population %>%
    group_by(.data$地区名称, .data$PAC, .data$PAC6) %>%
    summarise(
      地区原名 = first(.data$地区原名),
      匹配模式 = paste(unique(na.omit(.data$匹配模式)), collapse = "|"),
      省名称 = first(na.omit(.data$省名称), default = NA_character_),
      市名称 = first(na.omit(.data$市名称), default = NA_character_),
      人口数 = first(.data$人口数),
      .groups = "drop"
    )

  if (nrow(population) == 0) {
    stop("人口参数表未解析到有效的地区名称和人口数。")
  }

  scope_label <- infer_population_scope(
    population$地区名称,
    province_names = population$省名称,
    city_names = population$市名称
  )

  population %>%
    mutate(
      是否汇总行 = .data$地区名称 %in% c(scope_label, "合计", "总计", "全市", "全省"),
      参数名称 = scope_label,
      数据源 = source_label
    ) %>%
    arrange(desc(.data$是否汇总行), .data$地区名称)
}

load_population_data <- function(file_path, source_label = basename(file_path), file_name = basename(file_path)) {
  read_population_file(file_path, file_name = file_name) %>%
    normalize_population_data(source_label = source_label)
}

population_analysis_units <- function(population_data) {
  population_data %>%
    filter(!.data$是否汇总行) %>%
    select(any_of(c("地区名称", "人口数", "地区原名", "PAC", "PAC6", "匹配模式", "省名称", "市名称")))
}

extract_area_from_address <- function(address, population_units) {
  address <- empty_to_na(address)

  aliases <- population_units %>%
    transmute(
      地区名称,
      alias = make_admin_aliases(.data$地区原名, .data$匹配模式)
    ) %>%
    tidyr::unnest(cols = "alias") %>%
    bind_rows(
      population_units %>%
        transmute(
          地区名称,
          alias = make_admin_aliases(.data$地区名称, .data$匹配模式)
        ) %>%
        tidyr::unnest(cols = "alias")
    ) %>%
    distinct(.data$地区名称, .data$alias) %>%
    filter(!is.na(.data$alias), .data$alias != "") %>%
    arrange(desc(str_length(.data$alias)))

  map_chr(address, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    hit <- aliases %>%
      filter(str_detect(value, fixed(.data$alias))) %>%
      pull(.data$地区名称) %>%
      unique()

    if (length(hit) == 0) NA_character_ else hit[[1]]
  })
}

match_surveillance_to_population <- function(data, population_data) {
  units <- population_analysis_units(population_data)

  if (nrow(units) == 0) {
    return(data %>% mutate(原始区县 = .data$区县, 区县 = NA_character_))
  }

  address_pac6 <- if ("现住地址国标" %in% names(data)) make_pac6(data$现住地址国标) else rep(NA_character_, nrow(data))
  matched_by_pac <- rep(NA_character_, nrow(data))
  if ("PAC6" %in% names(units)) {
    valid_units <- !is.na(units$PAC6) & nzchar(units$PAC6)
    valid_addresses <- !is.na(address_pac6) & nzchar(address_pac6)
    matched_by_pac[valid_addresses] <- units$地区名称[valid_units][
      match(address_pac6[valid_addresses], units$PAC6[valid_units])
    ]
  }
  matched_area <- extract_area_from_address(data$现住详细地址, units)
  existing_area <- if ("区县" %in% names(data)) data$区县 else rep(NA_character_, nrow(data))
  existing_area <- if_else(existing_area %in% units$地区名称, existing_area, NA_character_)

  data %>%
    mutate(
      原始区县 = existing_area,
      地址PAC6 = address_pac6,
      区县 = coalesce(matched_by_pac, matched_area, existing_area)
    )
}

has_complete_shapefile <- function(shp_path) {
  base_path <- tools::file_path_sans_ext(shp_path)
  required_files <- c(
    paste0(base_path, ".shp"),
    paste0(base_path, ".dbf"),
    paste0(base_path, ".shx")
  )

  all(file.exists(required_files))
}

resolve_first_complete_shapefile <- function(candidates) {
  hit <- candidates[file.exists(candidates) & vapply(candidates, has_complete_shapefile, logical(1))]

  if (length(hit) == 0) {
    return(NA_character_)
  }

  normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
}

detect_name_column <- function(sf_data) {
  candidate_columns <- c("name", "NAME", "Name", "县", "县区", "区县", "COUNTY", "county")
  hit <- candidate_columns[candidate_columns %in% names(sf_data)]

  if (length(hit) > 0) {
    return(hit[[1]])
  }

  character_columns <- names(sf_data)[vapply(sf_data, is.character, logical(1))]

  if (length(character_columns) == 0) {
    stop("地图数据中未找到可用于区县名称匹配的字符字段。")
  }

  character_columns[[1]]
}

detect_map_pac_column <- function(sf_data) {
  candidate_columns <- c(
    "PAC", "pac", "Pac", "gb", "GB", "adcode", "ADCODE",
    "code", "CODE", "行政区划代码", "区划代码", "国标码", "编码"
  )
  hit <- candidate_columns[candidate_columns %in% names(sf_data)]

  if (length(hit) > 0) hit[[1]] else NA_character_
}

load_kunming_county_map <- function(
  county_candidates = default_county_map_candidates,
  city_candidates = default_city_map_candidates
) {
  county_path <- resolve_first_complete_shapefile(county_candidates)
  city_path <- resolve_first_complete_shapefile(city_candidates)

  if (is.na(county_path)) {
    stop("未找到可用的县级 shp 文件。请确保 shp/dbf/shx 配套齐全。")
  }

  county_sf <- st_read(county_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
    st_make_valid()

  county_name_col <- detect_name_column(county_sf)

  county_sf <- county_sf %>%
    mutate(
      原始区县名称 = .data[[county_name_col]],
      区县 = standardize_km_district_name(.data$原始区县名称)
    )

  if (!is.na(city_path)) {
    city_sf <- st_read(city_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
      st_make_valid()

    city_name_col <- detect_name_column(city_sf)

    kunming_city <- city_sf %>%
      filter(.data[[city_name_col]] == "昆明市")

    if (nrow(kunming_city) > 0) {
      county_sf <- county_sf %>%
        st_filter(kunming_city, .predicate = st_intersects)
    }
  }

  county_sf %>%
    filter(.data$区县 %in% km_district_population$区县) %>%
    distinct(.data$区县, .keep_all = TRUE) %>%
    st_transform(4326) %>%
    structure(
      source_path = county_path,
      source_label = basename(county_path)
    )
}

load_admin_map_layer <- function(candidates, level_label) {
  shp_path <- resolve_first_complete_shapefile(candidates)

  if (is.na(shp_path)) {
    return(NULL)
  }

  sf_data <- st_read(shp_path, quiet = TRUE, options = "ENCODING=UTF-8") %>%
    st_make_valid()

  name_col <- detect_name_column(sf_data)
  pac_col <- detect_map_pac_column(sf_data)

  sf_data %>%
    mutate(
      地图原名 = .data[[name_col]],
      地区名称 = standardize_admin_name(.data$地图原名),
      PAC = if (!is.na(pac_col)) make_full_pac(.data[[pac_col]]) else NA_character_,
      PAC6 = if (!is.na(pac_col)) make_pac6(.data[[pac_col]]) else NA_character_,
      地图层级 = level_label
    ) %>%
    st_transform(4326) %>%
    structure(source_path = shp_path, source_label = basename(shp_path))
}

load_best_admin_map <- function(population_data) {
  units <- population_analysis_units(population_data)

  if (nrow(units) == 0) {
    return(NULL)
  }

  layers <- list(
    县级 = load_admin_map_layer(default_county_map_candidates, "县级"),
    市级 = load_admin_map_layer(default_city_map_candidates, "市级"),
    省级 = load_admin_map_layer(default_province_map_candidates, "省级")
  ) %>%
    compact()

  if (length(layers) == 0) {
    return(NULL)
  }

  scored_layers <- imap(layers, function(layer, layer_name) {
    matched_by_pac <- if ("PAC6" %in% names(layer) && "PAC6" %in% names(units) && any(!is.na(units$PAC6))) {
      layer %>%
        inner_join(
          units %>%
            filter(!is.na(.data$PAC6), .data$PAC6 != "") %>%
            distinct(.data$PAC6, .keep_all = TRUE) %>%
            transmute(
              PAC6,
              人口地区名称 = .data$地区名称,
              人口地区原名 = .data$地区原名
            ),
          by = "PAC6"
        ) %>%
        mutate(
          地图原名 = coalesce(.data$地图原名, .data$人口地区原名),
          地区名称 = .data$人口地区名称
        ) %>%
        select(-any_of(c("人口地区名称", "人口地区原名"))) %>%
        distinct(.data$PAC6, .keep_all = TRUE)
    } else {
      layer %>% slice(0)
    }

    matched_by_name <- layer %>%
      filter(.data$地区名称 %in% units$地区名称) %>%
      distinct(.data$地区名称, .keep_all = TRUE)

    matched <- if (nrow(matched_by_pac) > 0) matched_by_pac else matched_by_name

    list(
      name = layer_name,
      data = matched,
      source_label = attr(layer, "source_label"),
      source_path = attr(layer, "source_path"),
      matches = nrow(matched)
    )
  })

  best_index <- which.max(map_int(scored_layers, "matches"))
  best_layer <- scored_layers[[best_index]]

  if (best_layer$matches == 0) {
    return(NULL)
  }

  best_layer$data %>%
    structure(
      source_path = best_layer$source_path,
      source_label = best_layer$source_label,
      map_level = best_layer$name,
      match_count = best_layer$matches,
      expected_count = nrow(units)
    )
}

normalize_place_name <- function(x) {
  x %>%
    empty_to_na() %>%
    str_replace_all("[,，;；。\\.]", " ") %>%
    str_replace_all("[()（）]", " ") %>%
    str_replace_all("[[:space:]]+", " ") %>%
    str_trim()
}

is_valid_case <- function(card_status, audit_status, delete_time, delete_reason) {
  card_status <- normalize_text(card_status)
  audit_status <- normalize_text(audit_status)
  delete_time <- normalize_text(delete_time)
  delete_reason <- normalize_text(delete_reason)

  !str_detect(card_status, "删除|退卡|作废") &
    !str_detect(audit_status, "删除|退卡|作废|未纳入统计") &
    (delete_time %in% c("", ".")) &
    delete_reason == ""
}

add_surveillance_column <- function(data, column_name, aliases = character()) {
  if (column_name %in% names(data)) {
    return(data)
  }

  alias_hit <- aliases[aliases %in% names(data)]

  data[[column_name]] <- if (length(alias_hit) > 0) {
    data[[alias_hit[[1]]]]
  } else {
    NA_character_
  }

  data
}

standardize_surveillance_columns <- function(data) {
  alias_map <- list(
    "（删除/标注）时间" = c("删除时间", "删除日期", "标注时间"),
    "（删除/未纳入统计）原因" = c("删除原因", "未纳入统计原因", "删除/未纳入统计原因"),
    "（删除/标注）用户" = c("删除用户", "标注用户"),
    "（删除/标注）用户所属单位" = c("删除用户所属单位", "标注用户所属单位")
  )

  required_columns <- c(
    "卡片ID", "卡片编号", "卡片状态", "审核状态", "疾病名称", "病例分类",
    "发病日期", "诊断时间", "订正终审时间", "报告卡录入时间", "出生日期",
    "性别", "年龄", "人群分类", "现住详细地址", "现住地址国标", "患者工作单位",
    "（删除/标注）时间", "（删除/未纳入统计）原因", "（删除/标注）用户",
    "（删除/标注）用户所属单位"
  )

  for (column_name in required_columns) {
    aliases <- alias_map[[column_name]]
    if (is.null(aliases)) {
      aliases <- character()
    }

    data <- add_surveillance_column(
      data,
      column_name = column_name,
      aliases = aliases
    )
  }

  data
}

read_surveillance_csv <- function(file_path) {
  # Validate Chinese headers before accepting an encoding; Excel exports may be GB18030 or UTF-8.
  for (encoding in c("UTF-8", "GB18030")) {
    result <- tryCatch(suppressWarnings(readr::read_csv(
      file_path, locale = locale(encoding = encoding),
      col_types = cols(.default = col_character()), na = c("", "NA", "NULL"),
      name_repair = "unique_quiet", show_col_types = FALSE
    )), error = function(e) NULL)
    if (!is.null(result) && all(c("疾病名称", "发病日期") %in% names(result))) {
      attr(result, "source_encoding") <- encoding
      return(result)
    }
  }
  stop("CSV 必须包含“疾病名称”和“发病日期”列，并使用 UTF-8 或 GB18030 编码。")
}

load_surveillance_data <- function(file_path) {
  imported <- read_surveillance_csv(file_path)
  input_rows <- nrow(imported)
  if (input_rows == 0L) stop("CSV 只有表头，没有可分析的数据行。")
  source_encoding <- attr(imported, "source_encoding")
  raw_df <- imported %>%
    select(-matches("^\\.\\.\\.[0-9]+$")) %>%
    standardize_surveillance_columns() %>%
    mutate(across(everything(), ~ empty_to_na(as.character(.x)))) %>%
    distinct()
  exact_duplicates <- input_rows - nrow(raw_df)
  # A later deletion/correction supersedes an earlier valid card. Do not filter validity first.
  raw_df <- raw_df %>%
    mutate(
      .input_order = row_number(),
      .card_key = case_when(
        !is.na(.data$卡片ID) ~ paste0("id:", .data$卡片ID),
        !is.na(.data$卡片编号) ~ paste0("number:", .data$卡片编号),
        TRUE ~ paste0("row:", .data$.input_order)
      ),
      .version_time = coalesce(parse_cn_datetime(.data$订正终审时间), parse_cn_datetime(.data$报告卡录入时间))
    ) %>%
    arrange(desc(.data$.version_time), desc(.data$.input_order)) %>%
    distinct(.data$.card_key, .keep_all = TRUE) %>%
    arrange(.data$.input_order) %>%
    select(-all_of(c(".input_order", ".card_key", ".version_time")))
  older_versions <- input_rows - exact_duplicates - nrow(raw_df)

  cleaned_df <- raw_df %>%
    bind_cols(lookup_disease_dictionary(raw_df$疾病名称)) %>%
    mutate(
      across(everything(), ~ if_else(is.na(.x), NA_character_, normalize_text(.x))),
      发病日期 = parse_cn_date(.data$发病日期),
      诊断时间 = parse_cn_datetime(.data$诊断时间),
      订正终审时间 = parse_cn_datetime(.data$订正终审时间),
      报告卡录入时间 = parse_cn_datetime(.data$报告卡录入时间),
      出生日期 = parse_cn_date(.data$出生日期),
      `（删除/标注）时间` = empty_to_na(.data$`（删除/标注）时间`),
      `（删除/未纳入统计）原因` = empty_to_na(.data$`（删除/未纳入统计）原因`),
      是否有效卡 = is_valid_case(
        card_status = .data$卡片状态,
        audit_status = .data$审核状态,
        delete_time = .data$`（删除/标注）时间`,
        delete_reason = .data$`（删除/未纳入统计）原因`
      ),
      管理级别 = factor(.data$管理级别, levels = disease_management_levels),
      传播途径 = resolve_transmission_route(.data$疾病名称, .data$标准病种, .data$传播途径),
      传播途径 = factor(.data$传播途径, levels = disease_transmission_routes),
      区县 = extract_km_district(.data$现住详细地址),
      性别标准 = standardize_gender(.data$性别),
      年龄岁 = calculate_age_years(.data$年龄, .data$出生日期, .data$发病日期),
      年龄组 = make_age_group(.data$年龄岁),
      工作单位标准 = normalize_place_name(.data$患者工作单位),
      地址标准 = normalize_place_name(.data$现住详细地址),
      发病年 = year(.data$发病日期),
      发病月 = month(.data$发病日期),
      发病周 = floor_date(.data$发病日期, unit = "week", week_start = 1)
    )

  invalid_status <- sum(!cleaned_df$是否有效卡)
  invalid_date <- sum(cleaned_df$是否有效卡 & is.na(cleaned_df$发病日期))
  missing_disease <- sum(cleaned_df$是否有效卡 & !is.na(cleaned_df$发病日期) &
                           (is.na(cleaned_df$疾病名称) | cleaned_df$疾病名称 == ""))
  cleaned_df <- cleaned_df %>%
    filter(.data$是否有效卡, !is.na(.data$发病日期), !is.na(.data$疾病名称), .data$疾病名称 != "")
  attr(cleaned_df, "data_quality") <- tibble::tibble(
    项目 = c("原始记录", "完全重复记录", "旧版本卡片", "删除或无效卡", "发病日期缺失或无效", "病种缺失", "纳入分析"),
    记录数 = c(input_rows, exact_duplicates, older_versions, invalid_status, invalid_date, missing_disease, nrow(cleaned_df))
  )
  attr(cleaned_df, "source_encoding") <- source_encoding

  cleaned_df
}
