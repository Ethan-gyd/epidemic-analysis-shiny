# 参与改进

欢迎围绕数据兼容性、规则可解释性、报告模板和界面体验提出 Issue 或 Pull Request。

## 提交问题

请说明：R 与操作系统版本、操作步骤、预期结果、实际结果及报错文字。附最小合成 CSV 和不含个人信息的截图即可。不要上传真实姓名、身份证、电话、详细住址、病例卡片或原始工作报告。不要把账号 Token、服务器密码写入 Issue。

算法问题还请写明日期范围、窗口天数、阈值、场所字段及预期分组。可以修改 `scripts/make_demo_data.R` 构造能复现问题的例子。

## 提交代码

1. Fork 仓库并创建修改分支。
2. 执行 `Rscript scripts/install_dependencies.R`，确认应用能用合成数据启动。
3. 保持 `app.R` 的交互编排与清洗、探测、报告模块职责分离。
4. 修改规则时同步更新 `docs/DETECTION_RULES.md`，补充对应边界示例或必要测试；不能仅凭合成例子宣称真实监测效果。
5. 提交 Pull Request，说明改了什么、为什么、如何验证。避免加入生成报告、真实数据和大型地图文件。

统计数值、报告期间、图表与下载文件应使用同一套筛选语义。日期转换应明确 Asia/Shanghai。不要把未知值自动填成零病例，也不要把规则预警写成确认暴发。

## 自动检查

GitHub Actions 配置会安装 R 4.5.3、所需 R/系统依赖、LibreOffice 与中文字体，并运行 `tests/test_detection.R`、`tests/test_reports.R` 与 `tests/test_app_integration.R`。报告格式冒烟测试使用临时合成数据，结束后清理输出。依赖安装依据 [r-lib 官方 action 文档](https://github.com/r-lib/actions/tree/v2/setup-r-dependencies)，使用显式 `packages` 列表以适配无 DESCRIPTION 的 Shiny 项目。

工作流仅完成配置，尚未在 GitHub 执行；首次上传后请检查 Actions 日志。

## 适合共建的方向

- 可审核的病名和机构映射表，以及校区、分院的稳定标识。
- 按病种、场所类型配置窗口和阈值。
- 报告模板字段与地区人口分母维护。
- 更多编码/缺失值/重复订正/空数据场景的验证。
- 经过授权的回顾性事件评价与误报原因分析。

仓库尚未确定许可证。提交或复用前请先确认作者公布的许可与贡献条款；不要在项目中添加来源或授权不明的第三方数据。
