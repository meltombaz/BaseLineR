# Shiny Baseline Characteristics App with Interactive Filtering and Advanced Controls Toggle
# Author: meltombaz
# Description: Upload patient-level TXT/CSV/Excel data and build a baseline characteristics
# table by group. Variable type detection and override, appropriate stats/tests, missing %,
# interactive filtering, pretty UI, and collapsible "Advanced Controls" section.

# --- Packages ---
packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "readr", "readxl", "dplyr", "tidyr",
  "purrr", "stringr", "broom", "DT", "janitor", "forcats", "tibble"
)
missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
lapply(packages, library, character.only = TRUE)

# --- Helpers ---
is_effectively_categorical <- function(x, max_unique = 10) {
  if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
  if (is.numeric(x)) {
    nunique <- dplyr::n_distinct(na.omit(x))
    return(nunique <= max_unique)
  }
  FALSE
}
shapiro_safe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  if (length(x) > 5000) x <- sample(x, 5000)
  tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
}
fmt_mean_sd <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_character_)
  sprintf("%.2f ± %.2f", mean(v), stats::sd(v))
}
fmt_med_iqr <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_character_)
  q <- stats::quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf("%.2f [%.2f, %.2f]", q[2], q[1], q[3])
}
chisq_or_fisher <- function(tab) {
  test <- tryCatch({
    suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  }, error = function(e) NULL)
  if (!is.null(test)) {
    exp_ok <- all(test$expected >= 5)
    if (exp_ok) return(list(method = "Chi-square", p.value = unname(test$p.value)))
  }
  f <- tryCatch({
    suppressWarnings(stats::fisher.test(tab, workspace = 2e8))
  }, error = function(e) NULL)
  if (is.null(f)) {
    return(list(method = "Fisher's exact (failed)", p.value = NA_real_))
  } else {
    return(list(method = "Fisher's exact", p.value = unname(f$p.value)))
  }
}
missing_pct <- function(df, var) {
  v <- df[[var]]
  pct <- 100 * sum(is.na(v)) / length(v)
  sprintf("%.1f%%", pct)
}
summarize_categorical <- function(df, var, group) {
  var_sym <- rlang::sym(var)
  grp_sym <- rlang::sym(group)
  df <- df %>%
    mutate(!!var_sym := forcats::fct_explicit_na(as.factor(!!var_sym), na_level = "Missing"))
  counts <- df %>%
    count(!!grp_sym, !!var_sym, .drop = FALSE) %>%
    group_by(!!grp_sym) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup() %>%
    mutate(cell = sprintf("%d (%.1f%%)", n, pct)) %>%
    select(!!grp_sym, !!var_sym, cell) %>%
    tidyr::pivot_wider(names_from = !!grp_sym, values_from = cell)
  tab <- df %>% select(!!grp_sym, !!var_sym) %>% table()
  test <- chisq_or_fisher(tab)
  levels_df <- counts %>% arrange(!!var_sym)
  levels_names <- levels_df %>% pull(!!var_sym) %>% as.character()
  levels_df <- levels_df %>% mutate(Variable = paste0(var, ": ", levels_names))
  pval <- rep(NA_character_, nrow(levels_df))
  pval[1] <- formatC(test$p.value, format = "f", digits = 4)
  method <- rep(NA_character_, nrow(levels_df))
  method[1] <- test$method
  missing_pct_col <- rep("", nrow(levels_df))
  missing_pct_col[1] <- missing_pct(df, var)
  levels_df %>%
    select(Variable, everything()) %>%
    mutate(`Missing (%)` = missing_pct_col, `Test` = method, `p-value` = pval)
}
summarize_numeric <- function(df, var, group, normality_alpha = 0.05) {
  var_sym <- rlang::sym(var)
  grp_sym <- rlang::sym(group)
  normality <- df %>% filter(!is.na(!!var_sym), !is.na(!!grp_sym)) %>%
    group_by(!!grp_sym) %>% summarise(shapiro_p = shapiro_safe(!!var_sym), .groups = "drop")
  all_normal <- all(normality$shapiro_p > normality_alpha, na.rm = TRUE)
  summary_wide <- df %>% group_by(!!grp_sym) %>%
    summarise(
      mean_sd = fmt_mean_sd(!!var_sym),
      med_iqr = fmt_med_iqr(!!var_sym),
      n = sum(!is.na(!!var_sym)),
      .groups = "drop"
    ) %>%
    mutate(value = if (all_normal) mean_sd else med_iqr) %>%
    select(!!grp_sym, value) %>% pivot_wider(names_from = !!grp_sym, values_from = value)
  g_n <- df %>% filter(!is.na(.data[[group]])) %>% pull(.data[[group]]) %>% unique() %>% length()
  valid_df <- df %>% select(!!grp_sym, !!var_sym) %>% filter(is.finite(!!var_sym), !is.na(!!grp_sym))
  if (g_n == 2) {
    if (all_normal) {
      test <- tryCatch(stats::t.test(as.formula(paste(var, "~", group)), data = valid_df), error = function(e) NULL)
      method <- "Welch t-test"
      pval <- if (!is.null(test)) unname(test$p.value) else NA_real_
    } else {
      test <- tryCatch(stats::wilcox.test(as.formula(paste(var, "~", group)), data = valid_df, exact = FALSE), error = function(e) NULL)
      method <- "Wilcoxon rank-sum"
      pval <- if (!is.null(test)) unname(test$p.value) else NA_real_
    }
  } else if (g_n > 2) {
    if (all_normal) {
      fit <- tryCatch(stats::aov(as.formula(paste(var, "~", group)), data = valid_df), error = function(e) NULL)
      method <- "One-way ANOVA"
      pval <- if (!is.null(fit)) summary(fit)[[1]]["Pr(>F)"][1, 1] else NA_real_
    } else {
      test <- tryCatch(stats::kruskal.test(as.formula(paste(var, "~", group)), data = valid_df), error = function(e) NULL)
      method <- "Kruskal-Wallis"
      pval <- if (!is.null(test)) unname(test$p.value) else NA_real_
    }
  } else {
    method <- "N/A"
    pval <- NA_real_
  }
  tibble::tibble(
    Variable = var,
    !!!summary_wide,
    `Missing (%)` = missing_pct(df, var),
    Test = method,
    `p-value` = ifelse(is.na(pval), NA_character_, formatC(pval, format = "f", digits = 4)),
    Normality = ifelse(all_normal, "Normal (Shapiro > 0.05)", "Non-normal")
  )
}
make_baseline <- function(df, group, vars, type_override = NULL, max_unique = 10, normality_alpha = 0.05) {
  stopifnot(group %in% names(df))
  df <- janitor::clean_names(df)
  group <- janitor::make_clean_names(group)
  vars <- janitor::make_clean_names(vars)
  df <- df %>% select(any_of(c(group, vars)))
  out <- list()
  for (v in vars) {
    x <- df[[v]]
    type <- if (!is.null(type_override) && v %in% names(type_override)) type_override[[v]] else "auto"
    if (type == "cat" || (type == "auto" && is_effectively_categorical(x, max_unique = max_unique))) {
      out[[v]] <- summarize_categorical(df, v, group)
    } else {
      out[[v]] <- summarize_numeric(df, v, group, normality_alpha)
    }
  }
  bind_rows(out)
}

# --- UI ---
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f7f7f7;
        border-radius: 10px;
        padding: 15px;
        box-shadow: 0 2px 5px rgba(120,120,120,0.07);
        margin-top: 10px;
        max-height: 85vh;
        overflow-y: auto;
      }
      .main-panel {
        margin-top: 10px;
      }
      .shiny-output-error-validation {
        color: #e74c3c;
        font-weight: bold;
      }
      .control-label { font-weight: 600; }
      .selectize-input, .form-control, .btn { border-radius: 5px; }
      h1 { color: #337ab7; }
      .advanced-toggle { margin-top: 15px; }
      .advanced-panel { margin-top: 10px; padding: 10px; background: #eef3fa; border-radius: 8px; }
    "))
  ),
  titlePanel(
    div(
      icon("table"), "Baseline Characteristics Table Builder",
      style="margin-bottom:10px;"
    )
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      class = "sidebar",
      fileInput("file", "Upload Data (CSV/TXT/Excel)",
                accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")),
      checkboxInput("header", "Header row (CSV/TXT)", TRUE),
      radioButtons("sep", "Separator (CSV/TXT)",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      uiOutput("sheet_ui"),
      hr(),
      uiOutput("group_ui"),
      uiOutput("vars_ui"),
      actionButton("toggle_advanced", "Show Advanced Controls", class = "btn btn-secondary advanced-toggle"),
      conditionalPanel(
        condition = "input.toggle_advanced % 2 == 1",
        div(
          class = "advanced-panel",
          uiOutput("type_ui"),
          numericInput("max_unique", "Treat numeric as categorical if unique values ≤", 10, min = 2, max = 50),
          numericInput("alpha", "Normality alpha (Shapiro)", 0.05, min = 0.001, max = 0.2, step = 0.001),
          h4("Filter Data"),
          uiOutput("filter_ui"),
          actionButton("apply_filter", "Apply Filters", class="btn btn-info"),
        )
      ),
      br(),
      actionButton("run", "Build Table", class = "btn btn-primary"),
      hr(),
      downloadButton("download_csv", "Download Table (CSV)", class="btn btn-success")
    ),
    shiny::mainPanel(
      class = "main-panel",
      tabsetPanel(
        tabPanel("Preview Data", DTOutput("preview") %>% shinycssloaders::withSpinner(type = 3, color="#337ab7", color.background="#f7f7f7")),
        tabPanel("Baseline Table", DTOutput("baseline") %>% shinycssloaders::withSpinner(type = 3, color="#337ab7", color.background="#f7f7f7")),
        tabPanel("Notes",
                 tags$div(
                   tags$h4("How it works"),
                   tags$ul(
                     tags$li("Categorical: character/factor/logical or numeric with ", code("unique ≤ threshold"), "."),
                     tags$li("Categorical test: Chi-square; switches to Fisher if expected counts < 5."),
                     tags$li("Numeric test: If all groups normal by Shapiro ", code("p > alpha"), ": t-test/ANOVA; else Wilcoxon/Kruskal."),
                     tags$li("Numeric summaries: normal → mean ± SD; non-normal → median [IQR]."),
                     tags$li("You can override auto-detection for each variable below variable selection."),
                     tags$li("The table reports the percentage of missing values per variable."),
                     tags$li("You can filter the data interactively before building the baseline table."),
                     tags$li("Sidebar advanced controls can be shown or hidden for convenience.")
                   ),
                   tags$h4("Tips"),
                   tags$ul(
                     tags$li("For large tables, Fisher's test may fail; Chi-square is used when possible."),
                     tags$li("Preview limited to first 50 rows."),
                     tags$li("Supported file types: .csv, .tsv, .txt, .xls, .xlsx.")
                   )
                 ))
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name) %>% tolower()
    path <- input$file$datapath
    if (ext %in% c("csv", "txt", "tsv")) {
      readr::read_delim(path, delim = input$sep, col_names = input$header, guess_max = 5000, show_col_types = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      sheet <- input$sheet %||% 1
      readxl::read_excel(path, sheet = sheet)
    } else {
      validate(need(FALSE, "Unsupported file type."))
    }
  })
  output$sheet_ui <- renderUI({
    file <- input$file
    if (is.null(file)) return(NULL)
    ext <- tools::file_ext(file$name) %>% tolower()
    if (!ext %in% c("xls", "xlsx")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(file$datapath), error = function(e) NULL)
    if (is.null(sheets)) return(NULL)
    selectInput("sheet", "Excel sheet", choices = seq_along(sheets), selected = 1)
  })
  data_clean <- reactive({
    req(raw_data())
    janitor::clean_names(raw_data())
  })
  
  # Filter UI (dynamic controls for selected variables + group)
  output$filter_ui <- renderUI({
    req(data_clean())
    df <- data_clean()
    vars <- c(input$group, input$vars)
    vars <- vars[!is.na(vars) & nzchar(vars)]
    out <- lapply(vars, function(v) {
      v_data <- df[[v]]
      if (is.null(v_data)) return(NULL)
      if (is_effectively_categorical(v_data, max_unique = input$max_unique)) {
        vals <- sort(unique(as.character(v_data)))
        selectInput(paste0("filter_", v), label = sprintf("Filter %s:", v),
                    choices = vals, selected = vals, multiple = TRUE)
      } else {
        rng <- range(v_data, na.rm = TRUE)
        sliderInput(paste0("filter_", v), label = sprintf("Filter %s:", v),
                    min = floor(rng[1]), max = ceiling(rng[2]),
                    value = rng)
      }
    })
    do.call(tagList, out)
  })
  
  # Filtered data (reactive)
  filtered_data <- eventReactive(input$apply_filter, {
    df <- data_clean()
    vars <- c(input$group, input$vars)
    vars <- vars[!is.na(vars) & nzchar(vars)]
    for (v in vars) {
      v_data <- df[[v]]
      if (is.null(v_data)) next
      if (is_effectively_categorical(v_data, max_unique = input$max_unique)) {
        selected <- input[[paste0("filter_", v)]]
        if (!is.null(selected)) {
          df <- df %>% filter(.data[[v]] %in% selected)
        }
      } else {
        rng <- input[[paste0("filter_", v)]]
        if (!is.null(rng) && length(rng) == 2) {
          df <- df %>% filter(.data[[v]] >= rng[1], .data[[v]] <= rng[2])
        }
      }
    }
    df
  }, ignoreNULL = FALSE)
  
  # Data preview (use filtered data)
  output$preview <- renderDT({
    req(filtered_data())
    DT::datatable(head(filtered_data(), 50), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$group_ui <- renderUI({
    df <- data_clean()
    selectInput("group", "Group column (stratify by)", choices = names(df), selected = names(df)[1])
  })
  output$vars_ui <- renderUI({
    req(input$group)
    df <- data_clean()
    choices <- setdiff(names(df), input$group)
    selectizeInput("vars", "Variables to include", choices = choices, multiple = TRUE, options = list(placeholder = 'Choose variables...'))
  })
  output$type_ui <- renderUI({
    req(input$vars)
    tagList(
      lapply(input$vars, function(var) {
        selectInput(
          inputId = paste0("type_", var),
          label = sprintf("Treat '%s' as:", var),
          choices = c("Auto-detect" = "auto", "Categorical" = "cat", "Numeric" = "num"),
          selected = "auto"
        )
      })
    )
  })
  baseline_tbl <- eventReactive(input$run, {
    req(input$group, input$vars)
    df <- filtered_data()
    df <- df %>% filter(!is.na(.data[[input$group]]))
    type_override <- setNames(
      lapply(input$vars, function(var) input[[paste0("type_", var)]]),
      input$vars
    )
    make_baseline(df, group = input$group, vars = input$vars,
                  type_override = type_override,
                  max_unique = input$max_unique, normality_alpha = input$alpha)
  }, ignoreInit = TRUE)
  output$baseline <- renderDT({
    req(baseline_tbl())
    DT::datatable(baseline_tbl(), options = list(scrollX = TRUE, pageLength = 25), rownames = FALSE)
  })
  output$download_csv <- downloadHandler(
    filename = function() sprintf("baseline_table_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      req(baseline_tbl())
      readr::write_csv(baseline_tbl(), file)
    }
  )
}

shinyApp(ui, server)