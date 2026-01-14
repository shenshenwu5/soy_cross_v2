## 目标
- 将采集簿生成中的“命名名称（crossed_name）”功能整体迁移到“杂交配置”模块中，放置在 C 组合矩阵的右侧。
- 保留并迁移所需参数选项：Batch、Prefix、Start N、Digits。
- 使用已有的批量命名函数 update_cross_names，在杂交配置内执行并反馈结果。

## 改动范围
- UI与逻辑迁移：从 [mod_book_app.R](file:///e:/FangCloudSync/R_WD360/Project/soy_cross_v2/R/mod_book_app.R) 移除命名相关控件与触发逻辑。
- 新增UI与逻辑：在 [mod_cross_app.R](file:///e:/FangCloudSync/R_WD360/Project/soy_cross_v2/R/mod_cross_app.R) 的 C 组合矩阵页面右侧新增命名参数面板与“执行命名”按钮，并在服务端调用 [update_cross_names](file:///e:/FangCloudSync/R_WD360/Project/soy_cross_v2/R/mod_cross.R#L284-332)。

## UI调整（C 组合矩阵右侧）
- 在 C 页布局中新增右侧列（如 column(width=3)），包含：
  - 批次：沿用现有 input$batch（下拉或文本）。
  - 前缀 Prefix：textInput，默认空。
  - 起始编号 Start N：numericInput，默认 1。
  - 位数 Digits：numericInput，默认 3。
  - 执行命名按钮：actionButton("run_crossed_name", "命名杂交名称")。
  - 命名结果预览：DT::dataTableOutput("tbl_named_preview")（可选）。

## 服务端逻辑
- observeEvent(input$run_crossed_name)：
  - 参数校验：batch非空、prefix非空，start_n/digits为正整数。
  - 调用 update_cross_names(batch=input$batch, prefix=input$prefix, start_n=input$start_n, digits=input$digits, db_path=db_path)。
  - 成功后：
    - showNotification("命名完成")；
    - 刷新矩阵/批次列表；
    - 加载当前批次已命名记录作为预览：可复用 [get_named_preview](file:///e:/FangCloudSync/R_WD360/Project/soy_cross_v2/R/mod_book.R#L12-25)。
- 输出渲染：output$tbl_named_preview 渲染命名后的 name、ma、pa。

## 数据与规则
- 命名规则沿用现有实现：正交 name = 前缀 + 指定位数递增 + "F0"；反交 name = 前缀 + 指定位数递增 + "RF0"，并与正交一一对应更新。
- 按批次、仅正交记录作为序列来源；反交使用正交的同一序号反向更新。

## 验证流程
- 在 C 页选择 batch、设置 prefix/start_n/digits 后点击“命名杂交名称”。
- 检查通知与预览表是否显示更新后的名称；
- 重新打开矩阵，确认名称列已更新；
- 通过数据库查询验证：SELECT name FROM crosses WHERE batch = ?。

## 迁移与兼容
- 从采集簿生成移除命名按钮与回写逻辑（如 btn_save_db_name 对应 observeEvent），仅保留预览与排图功能。
- 保持 update_cross_names 的位置与签名不变，避免影响其他调用。

## 交互细节
- 执行前弹出确认（modalDialog）以避免误操作。
- 执行后提供详细摘要（更新条数、时间戳）。

请确认以上方案，我将按此实施修改并交付可运行版本。