# 在项目根目录执行：Rscript scripts/install_dependencies.R
# 仅检查：Rscript scripts/install_dependencies.R --check
# 加装发布工具：Rscript scripts/install_dependencies.R --deploy
args <- commandArgs(trailingOnly = TRUE)
packages <- c(
  "DT", "dplyr", "forcats", "ggplot2", "glue", "ggspatial", "htmltools",
  "htmlwidgets", "leaflet", "lubridate", "plotly", "scales", "shiny",
  "shinydashboard", "stringr", "tidyr", "purrr", "readr", "readxl", "sf",
  "tibble", "systemfonts", "officer", "flextable", "ragg", "png"
)
if ("--deploy" %in% args) packages <- c(packages, "rsconnect", "renv")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) && !"--check" %in% args) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(missing)
}
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("仍缺少 R 包：", paste(missing, collapse = ", "),
       "。Linux 的 sf/ragg 需系统库，参见 docs/DEPLOYMENT.md。")
}
cat(R.version.string, "\n")
print(data.frame(package = packages, version = vapply(packages, function(p) as.character(packageVersion(p)), character(1))), row.names = FALSE)
cat("R 包检查通过。Word 可直接导出；PDF 还需 LibreOffice 和中文字体。\n")
