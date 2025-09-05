# BG Kliniken Baseline Characteristics Table Builder App
# PT Serif font, bold title, footer with GitHub link, Excel sheet fix, Example data in Notes

# --- Packages ---
packages <- c(
  "shiny", "shinythemes", "shinycssloaders", "shinyWidgets", "readr", "readxl", "dplyr", "tidyr",
  "purrr", "stringr", "broom", "DT", "janitor", "forcats", "tibble", "markdown"
)
missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
lapply(packages, library, character.only = TRUE)

brand_logo <- "https://upload.wikimedia.org/wikipedia/de/thumb/9/9c/UniversitaetTuebingen_WortBildMarke.png/960px-UniversitaetTuebingen_WortBildMarke.png"
github_url <- "https://github.com/meltombaz"

# --- Example Data (Markdown for pretty display in Notes tab) ---
example_data <- "
| PatientID | Age | Sex | BMI | Smoker | Group |
|-----------|-----|-----|-----|--------|-------|
| 001       | 34  | F   | 21.8| No     | Control |
| 002       | 56  | M   | 29.2| Yes    | Treatment |
| 003       | 43  | F   | 23.5| No     | Control |
| 004       | 61  | M   | 31.1| Yes    | Treatment |
| 005       | 29  | F   | 19.8| No     | Control |
| 006       | 52  | M   | 27.4| Yes    | Treatment |
"

# --- Helpers ---
is_effectively_categorical <- function(x, max_unique = 10) {
  if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
  if (is.numeric(x)) {
    nunique <- dplyr::n_distinct(na.omit(x))
    return(nunique <= max_unique)
  }
  FALSE
}
shapiro_safe <- function(x) {x <- x[is.finite(x)]; if (length(x) < 3) return(NA_real_); if (length(x) > 5000) x <- sample(x, 5000); tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)}
fmt_mean_sd <- function(v) {v <- v[is.finite(v)]; if (!length(v)) return(NA_character_); sprintf("%.2f ± %.2f", mean(v), stats::sd(v))}
fmt_med_iqr <- function(v) {v <- v[is.finite(v)]; if (!length(v)) return(NA_character_); q <- stats::quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE); sprintf("%.2f [%.2f, %.2f]", q[2], q[1], q[3])}
chisq_or_fisher <- function(tab, lt5_threshold = 0.20) {
  x <- suppressWarnings(try(stats::chisq.test(tab, correct = FALSE), silent = TRUE))
  if (inherits(x, "htest")) {
    exp <- x$expected
    prop_lt5 <- mean(exp < 5)
    any_lt1  <- any(exp < 1)
    if (!any_lt1 && prop_lt5 <= lt5_threshold) {
      return(list(method = "Chi-square", p.value = unname(x$p.value)))
    }
  }
  f <- suppressWarnings(try(stats::fisher.test(tab, workspace = 2e8), silent = TRUE))
  if (inherits(f, "htest")) {
    return(list(method = "Fisher's exact", p.value = unname(f$p.value)))
  }
  f_sim <- suppressWarnings(try(stats::fisher.test(tab, simulate.p.value = TRUE, B = 2e5), silent = TRUE))
  if (inherits(f_sim, "htest")) {
    return(list(method = "Fisher's exact (simulated)", p.value = unname(f_sim$p.value)))
  }
  list(method = "Fisher's exact (failed)", p.value = NA_real_)
}
missing_pct <- function(df, var) {
  v <- df[[var]]
  pct <- 100 * sum(is.na(v)) / length(v)
  sprintf("%.1f%%", pct)
}
summarize_categorical <- function(df, var, group) {
  var_sym <- rlang::sym(var)
  grp_sym <- rlang::sym(group)
  df_all <- df
  df <- df %>% dplyr::filter(!is.na(!!var_sym), !is.na(!!grp_sym))
  counts <- df %>%
    dplyr::count(!!grp_sym, !!var_sym, .drop = FALSE) %>%
    dplyr::group_by(!!grp_sym) %>%
    dplyr::mutate(pct = 100 * n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cell = sprintf("%d (%.1f%%)", n, pct)) %>%
    dplyr::select(!!grp_sym, !!var_sym, cell) %>%
    tidyr::pivot_wider(names_from = !!grp_sym, values_from = cell)
  tab <- df %>% dplyr::select(!!grp_sym, !!var_sym) %>% table()
  test <- chisq_or_fisher(tab)
  levels_df <- counts %>% dplyr::arrange(!!var_sym)
  levels_names <- levels_df %>% dplyr::pull(!!var_sym) %>% as.character()
  levels_df <- levels_df %>% dplyr::mutate(Variable = paste0(var, ": ", levels_names))
  pval <- rep(NA_character_, nrow(levels_df)); pval[1] <- formatC(test$p.value, format = "f", digits = 10)
  method <- rep(NA_character_, nrow(levels_df)); method[1] <- test$method
  levels_df %>%
    dplyr::select(Variable, dplyr::everything()) %>%
    dplyr::mutate(
      `Missing (%)` = {v_all <- df_all[[var]]; sprintf("%.1f%%", 100 * sum(is.na(v_all)) / length(v_all))},
      `Test` = method,
      `p-value` = pval
    )
}
summarize_numeric <- function(df, var, group, normality_alpha = 0.05) {
  var_sym <- rlang::sym(var)
  grp_sym <- rlang::sym(group)
  valid_df <- df %>% dplyr::filter(is.finite(!!var_sym), !is.na(!!grp_sym))
  normality <- valid_df %>% dplyr::group_by(!!grp_sym) %>% dplyr::summarise(shapiro_p = shapiro_safe(!!var_sym), .groups = "drop")
  all_normal <- all(normality$shapiro_p > normality_alpha, na.rm = TRUE)
  summary_wide <- valid_df %>%
    dplyr::group_by(!!grp_sym) %>%
    dplyr::summarise(
      mean_sd = fmt_mean_sd(!!var_sym),
      med_iqr = fmt_med_iqr(!!var_sym),
      n = sum(!is.na(!!var_sym)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(value = if (all_normal) mean_sd else med_iqr) %>%
    dplyr::select(!!grp_sym, value) %>%
    tidyr::pivot_wider(names_from = !!grp_sym, values_from = value)
  g_n <- valid_df %>% dplyr::pull(.data[[group]]) %>% unique() %>% length()
  if (g_n == 2) {
    if (all_normal) {
      test <- tryCatch(stats::t.test(as.formula(paste(var, "~", group)), data = valid_df),
                       error = function(e) NULL)
      method <- "Welch t-test"
      pval <- if (!is.null(test)) unname(test$p.value) else NA_real_
    } else {
      test <- tryCatch(stats::wilcox.test(as.formula(paste(var, "~", group)), data = valid_df, exact = FALSE),
                       error = function(e) NULL)
      method <- "Wilcoxon rank-sum"
      pval <- if (!is.null(test)) unname(test$p.value) else NA_real_
    }
  } else if (g_n > 2) {
    if (all_normal) {
      fit <- tryCatch(stats::aov(as.formula(paste(var, "~", group)), data = valid_df),
                      error = function(e) NULL)
      method <- "One-way ANOVA"
      pval <- if (!is.null(fit)) summary(fit)[[1]]["Pr(>F)"][1, 1] else NA_real_
    } else {
      test <- tryCatch(stats::kruskal.test(as.formula(paste(var, "~", group)), data = valid_df),
                       error = function(e) NULL)
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
    `p-value` = ifelse(is.na(pval), NA_character_, formatC(pval, format = "f", digits = 10)),
    Normality = ifelse(all_normal, "Normal (Shapiro > 0.05)", "Non-normal")
  )
}
make_baseline <- function(df, group, vars, type_override = NULL, max_unique = 10, normality_alpha = 0.05) {
  stopifnot(group %in% names(df))
  df <- janitor::clean_names(df)
  group <- janitor::make_clean_names(group)
  vars <- janitor::make_clean_names(vars)
  df <- df %>% dplyr::select(dplyr::any_of(c(group, vars)))
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
  dplyr::bind_rows(out)
}

# --- UI ---
ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=PT+Serif:wght@400;700&display=swap"),
    tags$link(rel="icon", type="image/svg+xml", href=brand_logo),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    # INIT Bootstrap tooltips (works with shinythemes/Bootstrap 3)
    tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); });")),
    tags$style(HTML("
      body {background-color: #fafbfc; font-family: 'PT Serif', serif;}
      .navbar-brand img {max-height: 40px;}
      .sidebar {
        background-color: #f6f8fa;
        border-radius: 12px;
        padding: 18px 14px;
        box-shadow: 0 3px 10px rgba(120,120,120,0.06);
        margin-top: 12px;
        max-height: 89vh;
        overflow-y: auto;
        font-family: 'PT Serif', serif;
      }
      .main-panel { margin-top: 12px; font-family: 'PT Serif', serif;}
      .shiny-output-error-validation { color: #e74c3c; font-weight: bold; }
      .selectize-input, .form-control, .btn { border-radius: 7px; font-family: 'PT Serif', serif;}
      h1, h2, h3, h4 { color: #2a5783; font-family: 'PT Serif', serif; font-weight: bold; }
      .control-label { font-weight: 600; }
      .btn-primary, .btn-success, .btn-warning, .btn-danger, .btn-default, .btn-royal {font-weight:600; letter-spacing:0.2px;}
      .app-title {display:flex;align-items:center;}
      .app-title img {height:38px;margin-right:15px;}
      .footer {margin-top:40px;padding:12px 0;color:#888;font-size:15px;text-align:center;border-top:1px solid #eaeaea; font-family: 'PT Serif', serif;}
      .footer a {color: #2a5783; text-decoration: underline;}
      .footer a:hover {color: #337ab7;}
      .example-table {margin-top:20px; margin-bottom:20px;}
      /* Make the tooltip cursor look intentional on the icon */
      .info-tip { color:#2a5783; cursor:help; margin-left:6px; }
    "))
  ),
  fluidRow(
    column(
      width = 12,
      div(class="app-title",
          img(src=brand_logo, alt="BG Kliniken Logo"),
          tags$h1("Baseline Characteristics Table Builder")
      ),
      hr()
    )
  ),
  fluidRow(
    column(
      width = 3,
      div(class="sidebar",
          fileInput(
            "file",
            label = tagList(
              "Upload Data (CSV/TXT/Excel)",
              tags$span(
                icon("info-circle"),
                class = "info-tip",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                title = paste(
                  "I don't recommend Excel sheets if you can avoid them.",
                  "CSV/TXT are safer and cleaner.",
                  "If you must use Excel, ensure: one header row, no merged cells,",
                  "no hidden columns/rows, and plain text values."
                )
              )
            ),
            accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")
          ),
          shinyWidgets::useSweetAlert(),
          shinyWidgets::useSweetAlert(),
          shinyWidgets::virtualSelectInput("header", "Header row (CSV/TXT)", choices=c(TRUE, FALSE), selected=TRUE),
          shinyWidgets::radioGroupButtons(
            "sep", "Separator (CSV/TXT)",
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ",",
            status = "primary"
          ),
          uiOutput("sheet_ui"),
          hr(),
          shinyWidgets::pickerInput(
            "group", "Group column (stratify by)", choices = NULL, selected = NULL,
            options = list(`live-search` = TRUE)
          ),
          shinyWidgets::pickerInput(
            "vars", "Variables to include", choices = NULL, multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          ),
          uiOutput("type_ui"),
          br(),
          shinyWidgets::sliderTextInput(
            "max_unique", "Numeric as categorical if unique values ≤", choices=as.character(2:50), selected="10"
          ),
          shinyWidgets::sliderTextInput(
            "alpha", "Normality alpha (Shapiro)", choices=as.character(seq(0.001,0.20,by=0.001)), selected="0.05"
          ),
          hr(),
          h4("Filter Data (Categorical Only)"),
          uiOutput("filter_ui"),
          shinyWidgets::actionBttn("apply_filter", "Apply Filters", style="pill", color="primary", block=TRUE),
          br(),
          shinyWidgets::actionBttn("run", "Build Table", style="unite", color="success", block=TRUE),
          hr(),
          shinyWidgets::downloadBttn("download_csv", "Download Table (CSV)", style="jelly", color="royal", block=TRUE)
      )
    ),
    column(
      width = 8,
      div(class="main-panel",
          tabsetPanel(
            tabPanel("Preview Data",
                     h4("Only first 50 rows of the data are shown."),
                     DTOutput("preview") %>%
                       shinycssloaders::withSpinner(type = 3, color="#2a5783", color.background="#f6f8fa")),
            tabPanel("Baseline Table",
                     DTOutput("baseline") %>%
                       shinycssloaders::withSpinner(type = 3, color="#2a5783", color.background="#f6f8fa")),
            tabPanel("Notes",
                     tags$div(
                       tags$h3("How it works"),
                       tags$ul(
                         tags$li("Categorical: character/factor/logical or numeric with ", code("unique ≤ threshold"), "."),
                         tags$li("Categorical test: Chi-square by default; switches to Fisher if >20% of expected counts < 5 or any expected < 1."),
                         tags$li("Numeric test: If all groups normal by Shapiro ", code("p > alpha"), ": t-test/ANOVA; else Wilcoxon/Kruskal."),
                         tags$li("Numeric summaries: normal → mean ± SD; non-normal → median [IQR]."),
                         tags$li("You can override auto-detection for each variable below variable selection."),
                         tags$li("The table reports the percentage of missing values per variable."),
                         tags$li("You can filter the data interactively (categorical variables only) before building the baseline table.")
                       ),
                       tags$h3("Tips"),
                       tags$ul(
                         tags$li("For large tables, exact Fisher may be computationally heavy; simulated Fisher is used as a fallback."),
                         tags$li("Preview limited to first 50 rows."),
                         tags$li("Supported file types: .csv, .tsv, .txt, .xls, .xlsx.")
                       ),
                       tags$h3("Example Data"),
                       tags$div(class="example-table",
                                HTML(markdown::markdownToHTML(text=example_data, fragment.only=TRUE))
                       ),
                       tags$p("Copy and paste the above table as CSV, TXT, or Excel for testing.")
                     ))
          )
      )
    )
  ),
  div(class="footer",
      HTML(sprintf('Created by <a href="%s" target="_blank">meltombaz</a> &mdash; BG Kliniken 2025', github_url))
  )
)

# --- Server ---
server <- function(input, output, session) {
  # --- Reactive: Raw Data ---
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name) %>% tolower()
    path <- input$file$datapath
    tryCatch({
      if (ext %in% c("csv", "txt", "tsv")) {
        readr::read_delim(path, delim = input$sep, col_names = as.logical(input$header), guess_max = 5000, show_col_types = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        sheet <- input$sheet
        readxl::read_excel(path, sheet = sheet)
      } else {
        shinyWidgets::sendSweetAlert(session, title="Unsupported file type", type="error", text="Please upload CSV, TXT, or Excel files.")
        validate(need(FALSE, "Unsupported file type."))
      }
    }, error = function(e) {
      shinyWidgets::sendSweetAlert(session, title="File could not be read", text=e$message, type="error")
      return(NULL)
    })
  })
  
  # --- UI: Sheet Selection (Excel, now uses sheet names) ---
  output$sheet_ui <- renderUI({
    file <- input$file
    if (is.null(file)) return(NULL)
    ext <- tools::file_ext(file$name) %>% tolower()
    if (!ext %in% c("xls", "xlsx")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(file$datapath), error = function(e) NULL)
    if (is.null(sheets)) return(NULL)
    shinyWidgets::pickerInput("sheet", "Excel sheet", choices = sheets, selected = sheets[1])
  })
  
  # --- Reactive: Cleaned Data ---
  data_clean <- reactive({
    req(raw_data())
    janitor::clean_names(raw_data())
  })
  
  # --- Dynamic Update: Group & Vars Picker Inputs ---
  observeEvent(data_clean(), {
    df <- data_clean()
    choices <- names(df)
    updatePickerInput(session, "group", choices = choices, selected = choices[1])
    updatePickerInput(session, "vars", choices = setdiff(choices, choices[1]), selected = NULL)
  })
  
  # --- Filter UI: Dynamic controls ---
  output$filter_ui <- renderUI({
    req(data_clean(), input$group, input$vars)
    df <- data_clean()
    vars <- c(input$group, input$vars)
    vars <- vars[!is.na(vars) & nzchar(vars)]
    out <- lapply(vars, function(v) {
      v_data <- df[[v]]
      if (is_effectively_categorical(v_data, max_unique = as.numeric(input$max_unique))) {
        vals <- sort(unique(as.character(v_data[!is.na(v_data)])))
        shinyWidgets::pickerInput(
          paste0("filter_", v),
          label = sprintf("Filter %s:", v),
          choices = vals, selected = vals, multiple = TRUE,
          options = list(`actions-box`=TRUE, `live-search`=TRUE)
        )
      } else {
        NULL
      }
    })
    do.call(tagList, out)
  })
  
  # --- Reactive: Filtered Data ---
  filtered_data <- eventReactive(input$apply_filter, {
    req(data_clean(), input$group, input$vars)
    df <- data_clean()
    vars <- c(input$group, input$vars)
    vars <- vars[!is.na(vars) & nzchar(vars)]
    for (v in vars) {
      v_data <- df[[v]]
      if (is_effectively_categorical(v_data, max_unique = as.numeric(input$max_unique))) {
        selected <- input[[paste0("filter_", v)]]
        if (!is.null(selected)) {
          df <- df %>% dplyr::filter(.data[[v]] %in% selected)
        }
      }
    }
    if (nrow(df) == 0) {
      shinyWidgets::sendSweetAlert(session, title="No rows match the filters", type="warning", text="Filter reset to show all rows.")
      return(data_clean()) # fallback to unfiltered
    }
    df
  }, ignoreNULL = FALSE)
  
  # --- Data Preview ---
  output$preview <- renderDT({
    req(filtered_data())
    DT::datatable(
      head(filtered_data(), 50),
      options = list(scrollX = TRUE, pageLength = 10, dom = "Bfrtip", buttons = c("copy","csv","excel","print")),
      extensions = "Buttons",
      rownames = FALSE
    )
  })
  
  # --- Dynamic UI: Variable Type Override ---
  output$type_ui <- renderUI({
    req(input$vars)
    tagList(
      lapply(input$vars, function(var) {
        shinyWidgets::radioGroupButtons(
          inputId = paste0("type_", var),
          label = sprintf("Treat '%s' as:", var),
          choices = c("Auto-detect" = "auto", "Categorical" = "cat", "Numeric" = "num"),
          selected = "auto",
          status = "primary"
        )
      })
    )
  })
  
  # --- Baseline Table Reactive ---
  baseline_tbl <- eventReactive(input$run, {
    req(input$group, input$vars)
    df <- filtered_data()
    df <- df %>% dplyr::filter(!is.na(.data[[input$group]]))
    if (nrow(df) == 0) {
      shinyWidgets::sendSweetAlert(session, title="No data for table", text="Try selecting more rows.", type="error")
      return(NULL)
    }
    type_override <- setNames(
      lapply(input$vars, function(var) input[[paste0("type_", var)]]),
      input$vars
    )
    withProgress(message = "Building baseline table...", value = 0, {
      make_baseline(df, group = input$group, vars = input$vars,
                    type_override = type_override,
                    max_unique = as.numeric(input$max_unique), normality_alpha = as.numeric(input$alpha))
    })
  }, ignoreInit = TRUE)
  
  # --- Baseline Table Output ---
  output$baseline <- renderDT({
    tbl <- baseline_tbl()
    req(tbl)
    DT::datatable(
      tbl,
      options = list(scrollX = TRUE, pageLength = 25, dom = "Bfrtip", buttons = c("copy", "csv", "excel", "print")),
      extensions = "Buttons",
      rownames = FALSE
    )
  })
  
  # --- Download Handler ---
  output$download_csv <- downloadHandler(
    filename = function() sprintf("baseline_table_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      req(baseline_tbl())
      readr::write_csv(baseline_tbl(), file)
    }
  )
}

shinyApp(ui, server)