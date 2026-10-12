# 精确白名单材料包。不会读取原始监测数据，也不会执行任何网络发布。
# 项目根目录运行：Rscript scripts/prepare_public_release.R [目标目录]
args <- commandArgs(trailingOnly = TRUE)
destination <- if (length(args)) args[[1]] else "output/public-release"
required <- c(
  "app.R", "data_preprocessing.R", "outbreak_detection.R", "report_generation.R", "report_analysis.R", "report_module.R",
  "README.md", "CONTRIBUTING.md", ".gitignore", ".dockerignore", "Dockerfile",
  "disease_dictionary.csv", "data/demo_surveillance.csv",
  "docs/DETECTION_RULES.md", "docs/DEPLOYMENT.md",
  "scripts/install_dependencies.R", "scripts/make_demo_data.R", "scripts/prepare_public_release.R",
  ".github/ISSUE_TEMPLATE/bug_report.yml", ".github/ISSUE_TEMPLATE/feature_request.yml", ".github/workflows/r-check.yml",
  "tests/test_detection.R", "tests/test_reports.R", "tests/test_report_analysis.R", "tests/test_app_integration.R", "scripts/generate_demo_reports.R"
)
# 不按整个 tests/ 目录复制。许可证仅在作者已自行添加时纳入。
optional <- "LICENSE"
files <- c(required, optional[file.exists(optional)])
missing <- required[!file.exists(required)]
if (length(missing)) stop("材料尚未完成，缺少：", paste(missing, collapse = "、"))
if (normalizePath(destination, mustWork = FALSE) == normalizePath(".")) stop("目标目录不能是项目根目录。")
if (dir.exists(destination)) {
  # Finder may add metadata while the user reviews this generated directory.
  metadata <- list.files(destination, pattern = "^\\.DS_Store$", recursive = TRUE, all.files = TRUE, full.names = TRUE)
  if (length(metadata)) unlink(metadata)
  existing <- list.files(destination, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  unexpected <- setdiff(existing, c(files, "PUBLIC_RELEASE_FILES.txt"))
  if (length(unexpected)) stop("目标目录包含不在白名单的文件。请使用新目录：", paste(unexpected, collapse = "、"))
}
for (path in files) {
  target <- file.path(destination, path)
  dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
  if (!file.copy(path, target, overwrite = TRUE)) stop("复制失败：", path)
}
writeLines(c(
  "公开材料文件清单（精确白名单；病例数据为合成样本）",
  paste0("生成日期：", Sys.Date()), "",
  sort(files), "", "未包含真实病例、卡片明细、人口上传文件、PPT、报告、地图或备份。",
  "未执行 Git 初始化、创建远端仓库或网络发布。"
), file.path(destination, "PUBLIC_RELEASE_FILES.txt"), useBytes = TRUE)
cat("公开材料包已准备：", normalizePath(destination), "\n", sep = "")
cat("共 ", length(files), " 个白名单文件；请先审阅，再自行发布。\n", sep = "")
