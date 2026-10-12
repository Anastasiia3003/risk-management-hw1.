# Homework 1: Market risk analysis using VaR and Expected Shortfall
# Assets: Apple Inc. (AAPL) and S&P 500 (^GSPC)
# Data source: Yahoo Finance via quantmod

# 1. Install and load required packages ---------------------------------------
required_packages <- c("quantmod", "PerformanceAnalytics", "ggplot2")

for (package_name in required_packages) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, repos = "https://cloud.r-project.org")
  }
}

suppressPackageStartupMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
})

# 2. Project folders and analysis period --------------------------------------
# Run this script from the project root in RStudio (open the .Rproj/project).
project_root <- getwd()

# Also support running the script while the working directory is scripts/.
if (!file.exists(file.path(project_root, "README.md")) &&
    file.exists(file.path(project_root, "..", "README.md"))) {
  project_root <- normalizePath(file.path(project_root, ".."), winslash = "/")
}

data_dir <- file.path(project_root, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

end_date <- Sys.Date() + 1
start_date <- Sys.Date() - round(3 * 365.2425)

assets <- c(
  AAPL = "AAPL",
  SP500 = "^GSPC"
)
asset_labels <- c(
  AAPL = "Apple (AAPL)",
  SP500 = "S&P 500 (^GSPC)"
)

# 3. Download adjusted closing prices and calculate log returns ---------------
prices_list <- list()
returns_list <- list()

for (asset_name in names(assets)) {
  ticker <- unname(assets[[asset_name]])
  message(sprintf("Downloading %s (%s) from Yahoo Finance...", asset_name, ticker))

  download_env <- new.env(parent = emptyenv())
  object_name <- tryCatch(
    quantmod::getSymbols(
      Symbols = ticker,
      src = "yahoo",
      from = start_date,
      to = end_date,
      env = download_env,
      auto.assign = TRUE
    ),
    error = function(e) {
      stop(sprintf(
        "Could not download %s from Yahoo Finance. Check your internet connection and ticker symbol. Details: %s",
        ticker, conditionMessage(e)
      ), call. = FALSE)
    }
  )

  if (length(object_name) < 1L || !exists(object_name[[1]], envir = download_env, inherits = FALSE)) {
    stop(sprintf("No data object was returned for ticker %s.", ticker), call. = FALSE)
  }

  raw_prices <- get(object_name[[1]], envir = download_env, inherits = FALSE)
  adjusted_prices <- quantmod::Ad(raw_prices)
  colnames(adjusted_prices) <- asset_name
  adjusted_prices <- adjusted_prices[!duplicated(zoo::index(adjusted_prices))]

  # Log returns: ln(P_t / P_(t-1)); remove observations with missing values.
  asset_returns <- diff(log(adjusted_prices))
  asset_returns <- stats::na.omit(asset_returns)

  if (NROW(asset_returns) < 100L) {
    stop(sprintf("Too few valid return observations were downloaded for %s.", ticker), call. = FALSE)
  }

  prices_list[[asset_name]] <- adjusted_prices
  returns_list[[asset_name]] <- asset_returns

  # Save the input data and calculated returns to data/.
  prices_df <- data.frame(
    Date = as.Date(zoo::index(adjusted_prices)),
    Adjusted_Close = as.numeric(adjusted_prices[[1]])
  )
  returns_df <- data.frame(
    Date = as.Date(zoo::index(asset_returns)),
    Log_Return = as.numeric(asset_returns[[1]])
  )

  utils::write.csv(
    prices_df,
    file = file.path(data_dir, paste0(asset_name, "_adjusted_prices.csv")),
    row.names = FALSE
  )
  utils::write.csv(
    returns_df,
    file = file.path(data_dir, paste0(asset_name, "_log_returns.csv")),
    row.names = FALSE
  )
}

# 4. Calculate VaR and Expected Shortfall -------------------------------------
# PerformanceAnalytics returns loss magnitudes as positive values when invert=TRUE.
calculate_risk_metrics <- function(asset_name, asset_returns) {
  var_hist_95 <- as.numeric(PerformanceAnalytics::VaR(
    asset_returns, p = 0.95, method = "historical", invert = TRUE
  ))[1]
  var_hist_99 <- as.numeric(PerformanceAnalytics::VaR(
    asset_returns, p = 0.99, method = "historical", invert = TRUE
  ))[1]
  var_gaussian_95 <- as.numeric(PerformanceAnalytics::VaR(
    asset_returns, p = 0.95, method = "gaussian", invert = TRUE
  ))[1]
  var_gaussian_99 <- as.numeric(PerformanceAnalytics::VaR(
    asset_returns, p = 0.99, method = "gaussian", invert = TRUE
  ))[1]
  es_hist_95 <- as.numeric(PerformanceAnalytics::ES(
    asset_returns, p = 0.95, method = "historical", invert = TRUE
  ))[1]

  data.frame(
    Asset = unname(asset_labels[[asset_name]]),
    Ticker = unname(assets[[asset_name]]),
    Start_Date = as.character(as.Date(zoo::index(asset_returns)[1])),
    End_Date = as.character(as.Date(zoo::index(asset_returns)[NROW(asset_returns)])),
    Observations = NROW(asset_returns),
    Historical_VaR_95_pct = var_hist_95 * 100,
    Historical_VaR_99_pct = var_hist_99 * 100,
    Parametric_VaR_95_pct = var_gaussian_95 * 100,
    Parametric_VaR_99_pct = var_gaussian_99 * 100,
    Historical_ES_95_pct = es_hist_95 * 100,
    check.names = FALSE
  )
}

metrics <- do.call(
  rbind,
  lapply(names(returns_list), function(asset_name) {
    calculate_risk_metrics(asset_name, returns_list[[asset_name]])
  })
)

utils::write.csv(
  metrics,
  file = file.path(data_dir, "risk_metrics_summary.csv"),
  row.names = FALSE
)

# 5. Create a histogram with historical 95% and 99% VaR thresholds ------------
for (asset_name in names(returns_list)) {
  asset_returns <- returns_list[[asset_name]]
  returns_numeric <- as.numeric(asset_returns[[1]])
  asset_metrics <- metrics[metrics$Ticker == unname(assets[[asset_name]]), , drop = FALSE]

  # VaR lines mark the empirical left-tail return quantiles (negative return values).
  cutoff_95 <- as.numeric(stats::quantile(returns_numeric, probs = 0.05, na.rm = TRUE, names = FALSE))
  cutoff_99 <- as.numeric(stats::quantile(returns_numeric, probs = 0.01, na.rm = TRUE, names = FALSE))

  var_lines <- data.frame(
    threshold = c(cutoff_95 * 100, cutoff_99 * 100),
    risk_level = c(
      sprintf("Historical VaR 95%% (%.2f%% loss)", asset_metrics$Historical_VaR_95_pct),
      sprintf("Historical VaR 99%% (%.2f%% loss)", asset_metrics$Historical_VaR_99_pct)
    )
  )

  plot_data <- data.frame(Return_pct = returns_numeric * 100)
  line_colors <- stats::setNames(c("#D55E00", "#0072B2"), unique(var_lines$risk_level))

  risk_plot <- ggplot(plot_data, aes(x = Return_pct)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 50,
      fill = "grey80",
      color = "white",
      boundary = 0
    ) +
    geom_density(color = "grey30", linewidth = 0.8, na.rm = TRUE) +
    geom_vline(
      data = var_lines,
      aes(xintercept = threshold, color = risk_level),
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_color_manual(values = line_colors, name = "Risk thresholds") +
    labs(
      title = paste("Daily log-return distribution:", asset_labels[[asset_name]]),
      subtitle = sprintf(
        "Period: %s to %s | Observations: %s",
        asset_metrics$Start_Date,
        asset_metrics$End_Date,
        asset_metrics$Observations
      ),
      x = "Daily log return (%)",
      y = "Density",
      caption = "Source: Yahoo Finance; calculations by the author. VaR lines use historical return quantiles."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(size = 8, color = "grey35")
    )

  ggplot2::ggsave(
    filename = file.path(data_dir, paste0("var_chart_", asset_name, ".png")),
    plot = risk_plot,
    width = 10,
    height = 6.5,
    dpi = 150,
    bg = "white"
  )
}

# 6. Print results and refresh the results table in README.md ------------------
format_pct <- function(value) sprintf("%.2f%%", value)

cat("\n--- MARKET RISK ANALYSIS ---\n")
cat(sprintf("Analysis period: %s to %s\n", start_date, Sys.Date()))
cat("VaR and ES are shown as positive loss magnitudes.\n\n")
print(metrics, row.names = FALSE, digits = 5)
cat(sprintf("\nCSV summary saved to: %s\n", file.path(data_dir, "risk_metrics_summary.csv")))
cat(sprintf("Charts saved to: %s\n", data_dir))

readme_path <- file.path(project_root, "README.md")
if (file.exists(readme_path)) {
  readme_lines <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")
  start_marker <- "<!-- RISK_RESULTS_START -->"
  end_marker <- "<!-- RISK_RESULTS_END -->"
  start_index <- match(start_marker, readme_lines)
  end_index <- match(end_marker, readme_lines)

  if (!is.na(start_index) && !is.na(end_index) && start_index < end_index) {
    result_table <- c(
      "| Актив | Історичний VaR 95% | Історичний VaR 99% | Параметричний VaR 95% | Параметричний VaR 99% | Історичний ES 95% |",
      "|---|---:|---:|---:|---:|---:|",
      vapply(seq_len(nrow(metrics)), function(i) {
        sprintf(
          "| %s | %s | %s | %s | %s | %s |",
          metrics$Asset[i],
          format_pct(metrics$Historical_VaR_95_pct[i]),
          format_pct(metrics$Historical_VaR_99_pct[i]),
          format_pct(metrics$Parametric_VaR_95_pct[i]),
          format_pct(metrics$Parametric_VaR_99_pct[i]),
          format_pct(metrics$Historical_ES_95_pct[i])
        )
      }, character(1))
    )

    readme_updated <- c(
      readme_lines[seq_len(start_index)],
      result_table,
      readme_lines[seq.int(end_index, length(readme_lines))]
    )
    writeLines(readme_updated, con = readme_path, useBytes = TRUE)
    message("The results table in README.md has been updated.")
  } else {
    message("README.md result markers were not found; update the results table manually if needed.")
  }
}

cat("\nAnalysis completed successfully.\n")
