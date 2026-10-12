# 本地运行、GitHub 发布与在线部署

核对日期：2026-10-08。本文提供可执行的准备步骤；不表示已经创建 GitHub 仓库或完成云端部署。

## 1. 先选你要发布的内容

| 目标 | 所需操作 |
| --- | --- |
| 让大家阅读代码、反馈、贡献 | 将公开材料包上传到 GitHub；在仓库中启用 Issues，需要讨论区时启用 Discussions |
| 让大家在浏览器中筛选数据、下载报告 | 另外部署 Shiny R 服务，例如 Posit Connect Cloud 或自有服务器 |
| 在比赛电脑离线演示 | 本机启动 Shiny，并提前准备 Word/PDF 示例；地图底图等外部资源可能仍需要网络 |

GitHub Pages 是静态 HTML/CSS/JavaScript 托管服务，不能直接运行本项目的 R 服务端。[GitHub 官方说明](https://docs.github.com/en/pages/getting-started-with-github-pages/what-is-github-pages)

## 2. 本机准备

推荐以 R 4.5.3（本次验证环境）开始。应用依赖未锁定为一个跨平台环境；安装脚本只安装缺失包，不升级已有包。首次准备需要网络，后续可以使用已安装的包。

```sh
Rscript scripts/install_dependencies.R
Rscript scripts/install_dependencies.R --check
R -e "shiny::runApp('.', launch.browser = TRUE)"
```

Linux 安装 `sf` 需要 GDAL、GEOS、PROJ 与 UDUNITS 等系统依赖，`ragg` 需要字体和图像相关开发库。Dockerfile 提供了 Ubuntu 系列安装示例；各平台安装细节以 [sf 官方安装说明](https://r-spatial.github.io/sf/#installing) 为准。

### PDF 与中文字体

Word 由 `officer` + `flextable` 直接生成；PDF 使用 LibreOffice 将该 Word 文档转换为 PDF。没有 LibreOffice 时仍能启动软件并导出 Word，PDF 功能会提示缺少转换工具。

安装 LibreOffice 后，确保 `soffice` 在 PATH，或在启动 R 应用前指定可执行文件路径，例如：

```r
# macOS 示例，实际路径以本机安装位置为准
Sys.setenv(SURVEILLANCE_SOFFICE = "/Applications/LibreOffice.app/Contents/MacOS/soffice")
shiny::runApp(".")
```

安装可用中文字体（如 Noto Sans CJK SC、思源黑体或系统自带中文字体），并在目标电脑上检查输出。转换过程使用独立临时用户配置目录；macOS 转换时还会使用临时 Fontconfig 配置读取本机中文字体，避免精简转换运行时只识别西文字体。该配置与缓存只在临时目录中生成，不修改系统字体。不需要 LaTeX 或 Pandoc。LibreOffice 的 `--headless`、`--convert-to pdf`、独立 `UserInstallation` 参数见 [官方命令行说明](https://help.libreoffice.org/latest/en-US/text/shared/guide/start_parameters.html)。

## 3. 生成适合公开的材料包

在项目根目录执行：

```sh
Rscript scripts/prepare_public_release.R
```

输出目录为 `output/public-release`。脚本按照明确文件白名单复制，并生成 `PUBLIC_RELEASE_FILES.txt`；只放入合成病例 CSV 与病种配置字典，不包含真实病例、人口上传数据、卡片明细、办公报告、PPT、地图和备份。`.gitignore` 是第二层防误传，不能替代检查文件列表。

材料包已经包含 `.gitignore`、README、算法说明、部署说明及贡献指南。地图文件没有打包；主要分析和地区表格仍可运行，地图视图需另行配置授权数据。

在 GitHub 网页新建仓库后，仅上传该材料包中的内容（含 `.gitignore`）；不要上传整个业务工作目录。也可在该目录初始化 Git 再推送到自己创建的仓库。添加作者确定的许可证后，才可明确标注开源许可；本材料没有代选许可证。

## 4. 推荐的公开演示：Posit Connect Cloud

Posit 当前官方提供从 GitHub 部署 Shiny 的流程。准备一个只含公开文件的 GitHub 仓库，并在干净材料包中生成依赖清单。Connect Cloud 接受 `manifest.json` 或 `renv.lock`。[官方 Shiny 部署教程](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html)

在 `output/public-release` 目录中执行以下 R 代码，只生成本地清单，不会发布：

```r
install.packages("rsconnect")  # 已安装时可跳过
app_files <- c(
  "app.R", "data_preprocessing.R", "outbreak_detection.R",
  "report_generation.R", "report_analysis.R", "report_module.R",
  "disease_dictionary.csv", "data/demo_surveillance.csv"
)
rsconnect::writeManifest(appDir = ".", appFiles = app_files)
```

把生成的 `manifest.json` 加入仓库。随后登录 Connect Cloud，选择 Publish → Shiny → 对应仓库及分支，以 `app.R` 为入口，查看构建日志，完成后测试得到的链接。清单 API 参数参见 [writeManifest 官方文档](https://rstudio.github.io/rsconnect/reference/writeManifest.html)。

应用本身可部署并不保证云端具备 LibreOffice 或中文字体。发布后分别验证上传、筛选、Word、PDF；若平台无法配置转换工具，可先提供 Word 下载，或选择下一节容器方案。当前尚未在云端验收，也不在此承诺特定套餐、价格、地区网络速度或 PDF 支持。

若已有 shinyapps.io 账号，可按其官方流程配置 `rsconnect` 并使用相同 `appFiles` 白名单调用 `deployApp()`。不要把 Token / Secret 放进仓库。该平台正在迁移到 Connect Cloud，新项目应先阅读 [迁移说明](https://docs.posit.co/shinyapps.io/guide/migration/)；具体发布参数见 [deployApp 官方文档](https://rstudio.github.io/rsconnect/reference/deployApp.html)。

## 5. 需要稳定 PDF：自有服务器 / Docker

项目提供 Dockerfile 与严格白名单 `.dockerignore`。镜像会安装 R 包、LibreOffice 和中文字体，以非 root 用户运行 Shiny。示例命令：

```sh
docker build -t surveillance-demo .
docker run --rm -p 127.0.0.1:3838:3838 surveillance-demo
```

浏览器打开 `http://127.0.0.1:3838`。该命令只绑定本机；对外提供服务时，应由管理者配置 HTTPS 反向代理、访问控制和资源限额。**Docker 配置是部署起点，本次未在目标 Linux/Docker 环境构建验收。** 镜像固定 R 版本，但 CRAN 包版本未锁定；长期维护可在验证环境中生成 `renv.lock`。

也可使用 Shiny Server 或 Posit Connect 在自有环境运行。不同方案的职责和官方入口见 [Posit 托管说明](https://shiny.posit.co/r/deploy.html)。

## 6. 发布前最后一次验收

- 默认启动显示完全合成数据，而非真实监测文件；检查所有将上传文件的内容。
- 运行清洗与规则测试，并使用合成数据检查 3 条机构预警。
- 选择周报、月报、专题各导出一次；核对期间、筛选和图表，打开 Word/PDF 检查中文与分页。
- 在准备演示的电脑上重新启动应用；检查本机端口、浏览器、中文字体和 LibreOffice。
- 在线公开演示仅上传合成样本；真实业务版本应部署在有访问控制且符合组织要求的环境。

仓库路径和在线演示 URL 需在真正发布成功后填入 README。没有可用链接时不要加入虚构占位“在线演示”。
