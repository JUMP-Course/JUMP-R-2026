# =========================================================
# utils_functions.R
# 项目通用函数
# 目的：
# 1. 统一图形主题
# 2. 统一保存图形
# 3. 统一 UTF-8 输出
# 4. 封装常用统计检验函数
# =========================================================

library(tidyverse)

options(encoding = "UTF-8")


# ---------------------------------------------------------
# 1. 项目统一图形主题
# ---------------------------------------------------------

theme_project <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}


# ---------------------------------------------------------
# 2. 统一保存图片
# ---------------------------------------------------------

save_project_plot <- function(plot, filename, width = 7, height = 5) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}


# ---------------------------------------------------------
# 3. 统一保存 UTF-8 CSV
# ---------------------------------------------------------

save_csv_utf8 <- function(data, path) {
  readr::write_csv(
    data,
    file = path,
    na = ""
  )
}


# ---------------------------------------------------------
# 4. 统一保存 UTF-8 Markdown / 文本
# ---------------------------------------------------------

save_text_utf8 <- function(lines, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(lines, con = con, useBytes = TRUE)
}


# ---------------------------------------------------------
# 5. 计算比例和近似95%CI
# ---------------------------------------------------------

calc_prop_ci <- function(x, n) {
  p <- x / n
  se <- sqrt(p * (1 - p) / n)
  
  tibble(
    rate = p,
    ci_low = pmax(p - 1.96 * se, 0),
    ci_high = pmin(p + 1.96 * se, 1)
  )
}


# ---------------------------------------------------------
# 6. 分类变量组间比较：卡方检验或 Fisher 精确检验
# ---------------------------------------------------------

run_chisq_or_fisher <- function(data, row_var, col_var) {
  tab <- table(data[[row_var]], data[[col_var]], useNA = "no")
  
  chi_result <- suppressWarnings(chisq.test(tab))
  
  if (any(chi_result$expected < 5)) {
    test_result <- fisher.test(tab)
    test_name <- "Fisher exact test"
  } else {
    test_result <- chi_result
    test_name <- "Chi-square test"
  }
  
  tibble(
    row_variable = row_var,
    col_variable = col_var,
    test = test_name,
    p_value = test_result$p.value
  )
}

# ---------------------------------------------------------
# 7. 连续变量多组比较：Kruskal-Wallis 检验
# ---------------------------------------------------------

run_kruskal_test <- function(data, value_var, group_var) {
  formula_text <- paste(value_var, "~", group_var)
  test_result <- kruskal.test(as.formula(formula_text), data = data)
  
  tibble(
    value_variable = value_var,
    group_variable = group_var,
    test = "Kruskal-Wallis test",
    p_value = test_result$p.value
  )
}