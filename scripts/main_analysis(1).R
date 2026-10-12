# Практичне завдання: аналіз ринкового ризику
# VaR та Expected Shortfall для Apple (AAPL) і S&P 500 (^GSPC)

# 0. Визначаємо кореневу папку проєкту
find_project_root <- function() {
  candidates <- unique(normalizePath(c(getwd(), file.path(getwd(), "..")),
                                     winslash = "/", mustWork = FALSE))

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "README.md")) &&
        file.exists(file.path(candidate, "scripts", "main_analysis.R"))) {
      return(candidate)
    }
  }

  # Додаткова перевірка, якщо скрипт запускають безпосередньо з редактора RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(script_path) && file.exists(script_path)) {
      candidate <- normalizePath(file.path(dirname(script_path), ".."),
                                 winslash = "/", mustWork = FALSE)
      if (file.exists(file.path(candidate, "README.md")) &&
          file.exists(file.path(candidate, "scripts", "main_analysis.R"))) {
        return(candidate)
      }
    }
  }

  stop("Не вдалося визначити папку проєкту. Відкрий папку risk-management-hw1 в RStudio та запусти scripts/main_analysis.R.")
}

project_root <- find_project_root()
data_dir <- file.path(project_root, "data")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# 1. Встановлюємо (за потреби) та підключаємо пакети
packages <- c("quantmod", "PerformanceAnalytics", "ggplot2")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Пакет '%s' не встановлено. Перевір підключення до інтернету та повтори запуск.", package))
  }
  library(package, character.only = TRUE)
}

# 2. Задаємо активи та період аналізу
tickers <- c("AAPL", "^GSPC")
to_date <- Sys.Date()
from_date <- seq(to_date, by = "-3 years", length.out = 2)[2]

cat("Період аналізу:", format(from_date), "-", format(to_date), "\n")

# 3. Допоміжна функція для оновлення секцій у README.md
replace_readme_section <- function(text, start_marker, end_marker, replacement) {
  start_match <- regexpr(start_marker, text, fixed = TRUE)
  end_match <- regexpr(end_marker, text, fixed = TRUE)
  if (start_match[1] < 0 || end_match[1] < 0 || end_match[1] < start_match[1]) {
    warning(sprintf("У README.md не знайдено маркери %s / %s; секцію не оновлено.",
                    start_marker, end_marker))
    return(text)
  }
  start_end <- start_match[1] + attr(start_match, "match.length") - 1
  prefix <- substr(text, 1, start_end)
  suffix <- substr(text, end_match[1], nchar(text))
  paste0(prefix, "\n", replacement, "\n", suffix)
}

# 4. Завантажуємо ціни, розраховуємо дохідності та метрики ризику
results <- data.frame(
  Asset = character(),
  Observations = integer(),
  Historical_VaR_95_pct = numeric(),
  Historical_VaR_99_pct = numeric(),
  Parametric_VaR_95_pct = numeric(),
  Parametric_VaR_99_pct = numeric(),
  Historical_ES_95_pct = numeric(),
  stringsAsFactors = FALSE
)

for (ticker in tickers) {
  cat("\nЗавантаження даних для", ticker, "...\n")

  price_data <- tryCatch(
    getSymbols(ticker, src = "yahoo", from = from_date, to = to_date,
               auto.assign = FALSE, warnings = FALSE),
    error = function(e) stop(sprintf("Не вдалося завантажити %s з Yahoo Finance: %s",
                                     ticker, conditionMessage(e)))
  )

  prices <- Ad(price_data)
  if (NROW(prices) == 0 || all(is.na(prices))) {
    stop(sprintf("Для активу %s не отримано коректних скоригованих цін.", ticker))
  }

  # Логарифмічні щоденні дохідності; видаляємо пропущені спостереження
  returns <- CalculateReturns(prices, method = "log")
  returns <- na.omit(returns)
  if (NROW(returns) < 30) {
    stop(sprintf("Для активу %s отримано недостатньо спостережень для оцінки ризику.", ticker))
  }

  # VaR (95% і 99%): історичний та параметричний (нормальний) методи.
  # У PerformanceAnalytics від'ємні результати відображають збиток у хвості розподілу.
  var_hist_95 <- as.numeric(VaR(returns, p = 0.95, method = "historical"))
  var_hist_99 <- as.numeric(VaR(returns, p = 0.99, method = "historical"))
  var_norm_95 <- as.numeric(VaR(returns, p = 0.95, method = "gaussian"))
  var_norm_99 <- as.numeric(VaR(returns, p = 0.99, method = "gaussian"))
  es_hist_95 <- as.numeric(ES(returns, p = 0.95, method = "historical"))

  asset_label <- if (ticker == "^GSPC") "^GSPC" else ticker
  results <- rbind(results, data.frame(
    Asset = asset_label,
    Observations = NROW(returns),
    Historical_VaR_95_pct = round(var_hist_95 * 100, 2),
    Historical_VaR_99_pct = round(var_hist_99 * 100, 2),
    Parametric_VaR_95_pct = round(var_norm_95 * 100, 2),
    Parametric_VaR_99_pct = round(var_norm_99 * 100, 2),
    Historical_ES_95_pct = round(es_hist_95 * 100, 2),
    stringsAsFactors = FALSE
  ))

  # Гістограма дохідностей із лініями історичного VaR 95% та 99%
  plot_data <- data.frame(Return = as.numeric(returns))
  var_lines <- data.frame(
    VaR = c(var_hist_95, var_hist_99),
    Level = factor(c("VaR 95%", "VaR 99%"), levels = c("VaR 95%", "VaR 99%"))
  )

  chart <- ggplot(plot_data, aes(x = Return)) +
    geom_histogram(aes(y = after_stat(density)), bins = 60,
                   fill = "grey75", color = "white") +
    geom_vline(data = var_lines, aes(xintercept = VaR, color = Level),
               linewidth = 1) +
    scale_color_manual(values = c("VaR 95%" = "orange", "VaR 99%" = "red")) +
    labs(
      title = paste("Розподіл дохідностей та історичний VaR:", ticker),
      x = "Логарифмічна щоденна дохідність",
      y = "Щільність",
      color = NULL
    ) +
    theme_minimal()

  chart_file <- paste0("var_chart_", gsub("\\^", "", ticker), ".png")
  ggsave(filename = file.path(data_dir, chart_file), plot = chart,
         width = 8, height = 6, dpi = 120)
  cat("Графік збережено:", file.path("data", chart_file), "\n")
}

# 5. Зберігаємо результати у CSV
results_file <- file.path(data_dir, "risk_results.csv")
write.csv(results, results_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n--- РЕЗУЛЬТАТИ АНАЛІЗУ РИЗИКУ ---\n")
print(results, row.names = FALSE)
cat("\nCSV із результатами збережено в data/risk_results.csv\n")

# 6. Автоматично оновлюємо таблицю та графіки в README.md
readme_file <- file.path(project_root, "README.md")
if (file.exists(readme_file)) {
  readme_text <- paste(readLines(readme_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  table_lines <- c(
    "| Актив | Спостережень | Історичний VaR 95% | Історичний VaR 99% | Параметричний VaR 95% | Параметричний VaR 99% | Історичний ES 95% |",
    "|---|---:|---:|---:|---:|---:|---:|"
  )
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    table_lines <- c(table_lines, sprintf(
      "| %s | %d | %.2f%% | %.2f%% | %.2f%% | %.2f%% | %.2f%% |",
      row$Asset, row$Observations, row$Historical_VaR_95_pct,
      row$Historical_VaR_99_pct, row$Parametric_VaR_95_pct,
      row$Parametric_VaR_99_pct, row$Historical_ES_95_pct
    ))
  }
  table_content <- paste(table_lines, collapse = "\n")
  chart_content <- paste(
    "### Apple (AAPL)",
    "![Розподіл дохідностей AAPL та історичний VaR](data/var_chart_AAPL.png)",
    "",
    "### S&P 500 (^GSPC)",
    "![Розподіл дохідностей S&P 500 та історичний VaR](data/var_chart_GSPC.png)",
    sep = "\n"
  )

  readme_text <- replace_readme_section(readme_text, "<!-- RESULTS_START -->",
                                        "<!-- RESULTS_END -->", table_content)
  readme_text <- replace_readme_section(readme_text, "<!-- CHARTS_START -->",
                                        "<!-- CHARTS_END -->", chart_content)
  writeLines(readme_text, readme_file, useBytes = TRUE)
  cat("README.md оновлено: додано таблицю результатів та графіки.\n")
}
