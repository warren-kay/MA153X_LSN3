library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────
data <- read.csv("EDA_AmesHousing.csv", stringsAsFactors = FALSE)

# Numeric variables suitable for z-score / correlation analysis
num_vars <- c(
  "Sale Price ($)"           = "SalePrice",
  "Above-Grade Living Area (sq ft)" = "Gr.Liv.Area",
  "Overall Quality (1–10)"   = "Overall.Qual",
  "Overall Condition (1–10)" = "Overall.Cond",
  "Year Built"               = "Year.Built",
  "Year Remodeled"           = "Year.Remod.Add",
  "Total Basement Area (sq ft)" = "Total.Bsmt.SF",
  "1st Floor Area (sq ft)"   = "X1st.Flr.SF",
  "Garage Area (sq ft)"      = "Garage.Area",
  "Lot Area (sq ft)"         = "Lot.Area",
  "Bedrooms Above Grade"     = "Bedroom.AbvGr",
  "Full Bathrooms"           = "Full.Bath",
  "Total Rooms Above Grade"  = "TotRms.AbvGrd"
)

# Correlation variable pairs (exclude near-zero variance or non-informative)
corr_vars <- c(
  "Sale Price ($)"           = "SalePrice",
  "Above-Grade Living Area (sq ft)" = "Gr.Liv.Area",
  "Overall Quality (1–10)"   = "Overall.Qual",
  "Overall Condition (1–10)" = "Overall.Cond",
  "Year Built"               = "Year.Built",
  "Total Basement Area (sq ft)" = "Total.Bsmt.SF",
  "Garage Area (sq ft)"      = "Garage.Area",
  "Lot Area (sq ft)"         = "Lot.Area",
  "Bedrooms Above Grade"     = "Bedroom.AbvGr",
  "Full Bathrooms"           = "Full.Bath",
  "Total Rooms Above Grade"  = "TotRms.AbvGrd"
)

# Helper: get display name from column name
display_name <- function(col) {
  idx <- which(num_vars == col)
  if (length(idx) > 0) names(num_vars)[idx] else col
}

# Helper: compute z-scores for a numeric vector
z_scores <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  (x - mu) / s
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; }
    h2 { font-size: 20px; font-weight: 600; margin-top: 0; }
    h4 { font-size: 15px; font-weight: 600; color: #333; margin-top: 18px; margin-bottom: 4px; }
    .well { background: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 6px; padding: 14px; }
    .rq-box { background: #eef4fb; border-left: 4px solid #2c6fad; padding: 10px 14px;
               border-radius: 0 6px 6px 0; margin-bottom: 10px; font-size: 13px; }
    .stat-box { background: #fff; border: 1px solid #ddd; border-radius: 6px;
                padding: 10px 14px; margin-bottom: 8px; }
    .stat-label { font-size: 12px; color: #777; margin-bottom: 2px; }
    .stat-value { font-size: 22px; font-weight: 600; color: #2c6fad; }
    .stat-value-sm { font-size: 17px; font-weight: 600; color: #2c6fad; }
    .nav-tabs > li > a { font-size: 13px; }
    .outlier-unusual { color: #e05c2a; font-weight: 600; }
    .outlier-ok { color: #2ca05a; font-weight: 600; }
    .corr-table td, .corr-table th { text-align: center; padding: 5px 8px; font-size: 12px; }
    .corr-table { border-collapse: collapse; width: 100%; }
    .corr-table th { background: #2c6fad; color: #fff; }
    .corr-table tr:nth-child(even) { background: #f0f4fa; }
  "))),
  
  titlePanel("Ames Housing — EDA II: Z-Scores & Correlation"),
  
  tabsetPanel(
    
    # ── Tab 1: Z-Score Explorer ──────────────────────────────────────────────
    tabPanel("Z-Score Explorer",
             br(),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Controls"),
                        selectInput("zscore_var", "Select Variable:",
                                    choices = num_vars,
                                    selected = "SalePrice"),
                        hr(),
                        p(style="font-size:12px; color:#555;",
                          "Enter a value to compute its z-score and see where it falls in the distribution."),
                        numericInput("zscore_value", "Enter a Value:", value = NULL),
                        actionButton("zscore_btn", "Compute Z-Score", class="btn btn-primary btn-sm")
                      ),
                      wellPanel(
                        h4("Distribution Summary"),
                        div(class="stat-box",
                            div(class="stat-label", "Mean"),
                            div(class="stat-value", textOutput("zs_mean", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "Standard Deviation"),
                            div(class="stat-value", textOutput("zs_sd", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "Min / Max"),
                            div(class="stat-value-sm", textOutput("zs_range", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "# Observations"),
                            div(class="stat-value", textOutput("zs_n", inline=TRUE))
                        )
                      )
               ),
               column(8,
                      plotOutput("zscore_hist", height="320px"),
                      br(),
                      conditionalPanel(
                        condition = "input.zscore_btn > 0",
                        wellPanel(style="background:#fff8f0; border-color:#e05c2a;",
                                  h4("Z-Score Result"),
                                  uiOutput("zscore_result")
                        )
                      ),
                      br(),
                      h4("Research Questions"),
                      div(class="rq-box",
                          "RQ 1. Select 'Sale Price ($)' and enter a value of $215,000. What is the z-score?
             Is this house priced above or below average, and by how many standard deviations?"
                      ),
                      div(class="rq-box",
                          "RQ 2. Now enter $700,000 for Sale Price. Based on the z-score and the histogram,
             would you flag this as an unusual observation? Does the histogram support your conclusion?"
                      ),
                      div(class="rq-box",
                          "RQ 3. Switch to 'Above-Grade Living Area'. Enter 5,000 sq ft.
             How does this z-score compare to the $700,000 sale price? Are very large houses
             equally unusual compared to very expensive ones?"
                      ),
                      div(class="rq-box",
                          "RQ 4. For 'Year Built', enter the year 1900. Is this an unusual year in the dataset?
             Does an unusual z-score necessarily mean the data is erroneous? Explain your reasoning."
                      )
               )
             )
    ),
    
    # ── Tab 2: Outlier Investigation ─────────────────────────────────────────
    tabPanel("Outlier Investigation",
             br(),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Controls"),
                        selectInput("out_var", "Select Variable:",
                                    choices = num_vars,
                                    selected = "SalePrice"),
                        sliderInput("out_thresh", "Flag Threshold (|z| >):",
                                    min = 1.5, max = 4.0, value = 2.5, step = 0.1),
                        hr(),
                        p(style="font-size:12px; color:#555;",
                          "The rule of thumb used in this course: flag observations where |z| > 2.5
               as potentially unusual.")
                      ),
                      wellPanel(
                        h4("Flagged Observations"),
                        div(class="stat-box",
                            div(class="stat-label", "Total Observations"),
                            div(class="stat-value", textOutput("out_total", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "Flagged as Unusual"),
                            div(class="stat-value outlier-unusual", textOutput("out_flagged", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "% Flagged"),
                            div(class="stat-value-sm", textOutput("out_pct", inline=TRUE))
                        )
                      )
               ),
               column(8,
                      plotOutput("outlier_plot", height="320px"),
                      br(),
                      h4("Most Extreme Observations (highest |z-score|)"),
                      tableOutput("out_table"),
                      br(),
                      h4("Research Questions"),
                      div(class="rq-box",
                          "RQ 1. With 'Sale Price ($)' selected and threshold at 2.5, how many homes are flagged
             as unusual? Look at the dot plot — are the flagged points clearly separated from the rest?"
                      ),
                      div(class="rq-box",
                          "RQ 2. Examine the top extreme observations in the table. Do you think the highest-priced
             homes are data errors or real (but rare) sales? What additional information would help
             you decide?"
                      ),
                      div(class="rq-box",
                          "RQ 3. Switch to 'Lot Area (sq ft)' and raise the threshold to 3.0. How does the number
             of flagged observations change? Why might a variable like lot area have more extreme
             outliers than sale price?"
                      )
               )
             )
    ),
    
    # ── Tab 3: Correlation Explorer ──────────────────────────────────────────
    tabPanel("Correlation Explorer",
             br(),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Controls"),
                        selectInput("corr_x", "X Variable:",
                                    choices = corr_vars,
                                    selected = "Gr.Liv.Area"),
                        selectInput("corr_y", "Y Variable:",
                                    choices = corr_vars,
                                    selected = "SalePrice"),
                        checkboxInput("corr_trend", "Show trend line", value = TRUE),
                        checkboxInput("corr_std", "Use standardized (z-score) axes", value = FALSE),
                        hr(),
                        p(style="font-size:12px; color:#555;",
                          "Correlation measures the strength of a LINEAR relationship.
               Values near +1 or -1 indicate strong linear association;
               near 0 means little linear relationship.")
                      ),
                      wellPanel(
                        h4("Correlation Summary"),
                        div(class="stat-box",
                            div(class="stat-label", "Pearson Correlation (r)"),
                            div(class="stat-value", textOutput("corr_r", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "Strength"),
                            div(class="stat-value-sm", textOutput("corr_strength", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "Direction"),
                            div(class="stat-value-sm", textOutput("corr_dir", inline=TRUE))
                        ),
                        div(class="stat-box",
                            div(class="stat-label", "# Complete Pairs"),
                            div(class="stat-value", textOutput("corr_n", inline=TRUE))
                        )
                      )
               ),
               column(8,
                      plotOutput("corr_scatter", height="360px"),
                      br(),
                      h4("Research Questions"),
                      div(class="rq-box",
                          "RQ 1. Plot 'Above-Grade Living Area' (x) vs. 'Sale Price ($)' (y).
             What is the correlation coefficient? Is it positive or negative?
             Does this match your intuition about how size and price are related?"
                      ),
                      div(class="rq-box",
                          "RQ 2. Now check the 'Use standardized axes' box. Does the shape of the
             scatterplot change? Does the correlation coefficient change?
             What does this tell you about z-scores and units?"
                      ),
                      div(class="rq-box",
                          "RQ 3. Try 'Overall Quality' (x) vs. 'Sale Price ($)' (y), then try
             'Overall Condition' vs. 'Sale Price ($)'. Which variable has a stronger
             correlation with price? Does this surprise you?"
                      ),
                      div(class="rq-box",
                          "RQ 4. Find a pair of variables with a correlation near zero.
             Does a near-zero correlation mean the two variables are unrelated,
             or only that they have no LINEAR relationship? What shape might
             a non-linear relationship look like on the scatterplot?"
                      )
               )
             )
    ),
    
    # ── Tab 4: Correlation Table ──────────────────────────────────────────────
    tabPanel("Correlation Table",
             br(),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Select Variables"),
                        checkboxGroupInput("corr_tbl_vars",
                                           "Include in table:",
                                           choices = corr_vars,
                                           selected = c("SalePrice", "Gr.Liv.Area", "Overall.Qual",
                                                        "Year.Built", "Garage.Area", "Full.Bath")
                        )
                      ),
                      wellPanel(
                        h4("Color Guide"),
                        p(style="font-size:12px;",
                          tags$span(style="color:#c0392b; font-weight:600;", "Red / Dark"),
                          " = strong positive (near +1)"),
                        p(style="font-size:12px;",
                          tags$span(style="color:#2980b9; font-weight:600;", "Blue / Dark"),
                          " = strong negative (near -1)"),
                        p(style="font-size:12px;",
                          tags$span(style="color:#888;", "White"),
                          " = weak or no linear relationship (near 0)")
                      )
               ),
               column(8,
                      h4("Correlation Matrix"),
                      uiOutput("corr_table_html"),
                      br(),
                      h4("Research Questions"),
                      div(class="rq-box",
                          "RQ 1. Which variable in the table is most strongly correlated with
             'Sale Price ($)'? Does this match what you found in the Correlation
             Explorer tab?"
                      ),
                      div(class="rq-box",
                          "RQ 2. Find two variables in the table that are strongly correlated
             with each other but neither is Sale Price. Why might two predictors
             be correlated? Could this be a problem if you tried to use both to
             predict price?"
                      ),
                      div(class="rq-box",
                          "RQ 3. The diagonal of the table is always 1. Why? What does a
             variable's correlation with itself always equal?"
                      ),
                      div(class="rq-box",
                          "RQ 4. Does a high correlation between two variables prove that one
             causes the other? Give an example of two variables in this dataset
             that might be strongly correlated but not causally linked."
                      )
               )
             )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Z-Score Tab ─────────────────────────────────────────────────────────────
  zs_data <- reactive({
    col <- input$zscore_var
    x <- data[[col]]
    x[!is.na(x) & is.finite(x)]
  })
  
  output$zs_mean <- renderText({
    x <- zs_data()
    if (input$zscore_var == "SalePrice") scales::dollar(round(mean(x), 0))
    else scales::comma(round(mean(x), 2))
  })
  
  output$zs_sd <- renderText({
    x <- zs_data()
    if (input$zscore_var == "SalePrice") scales::dollar(round(sd(x), 0))
    else scales::comma(round(sd(x), 2))
  })
  
  output$zs_range <- renderText({
    x <- zs_data()
    if (input$zscore_var == "SalePrice") {
      paste0(scales::dollar(min(x)), " – ", scales::dollar(max(x)))
    } else {
      paste0(scales::comma(min(x)), " – ", scales::comma(max(x)))
    }
  })
  
  output$zs_n <- renderText({ scales::comma(length(zs_data())) })
  
  output$zscore_hist <- renderPlot({
    x    <- zs_data()
    col  <- input$zscore_var
    dname <- display_name(col)
    df   <- data.frame(x = x)
    
    p <- ggplot(df, aes(x = x)) +
      geom_histogram(bins = 40, fill = "#2c6fad", color = "white", alpha = 0.85) +
      theme_minimal(base_size = 13) +
      labs(title = paste("Distribution of", dname),
           x = dname, y = "Count") +
      scale_x_continuous(labels = if (col == "SalePrice") scales::dollar else scales::comma)
    
    # Add vertical line for user value if button pressed
    val <- input$zscore_value
    if (input$zscore_btn > 0 && !is.null(val) && !is.na(val)) {
      p <- p + geom_vline(xintercept = val, color = "#e05c2a", linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = val, y = Inf, label = "Your value",
                 color = "#e05c2a", vjust = 2, hjust = -0.1, size = 4)
    }
    p
  })
  
  output$zscore_result <- renderUI({
    req(input$zscore_btn)
    val <- input$zscore_value
    req(!is.null(val), !is.na(val))
    
    x   <- zs_data()
    mu  <- mean(x)
    s   <- sd(x)
    z   <- (val - mu) / s
    col <- input$zscore_var
    dname <- display_name(col)
    unusual <- abs(z) > 2.5
    
    fmt_val <- if (col == "SalePrice") scales::dollar(val) else scales::comma(val)
    fmt_mu  <- if (col == "SalePrice") scales::dollar(round(mu, 0)) else scales::comma(round(mu, 2))
    fmt_s   <- if (col == "SalePrice") scales::dollar(round(s, 0)) else scales::comma(round(s, 2))
    
    dir_txt <- if (z > 0) "above" else "below"
    flag_txt <- if (unusual) {
      tags$span(class = "outlier-unusual",
                paste0("⚠ Potentially unusual (|z| = ", round(abs(z), 2), " > 2.5)"))
    } else {
      tags$span(class = "outlier-ok",
                paste0("✓ Within typical range (|z| = ", round(abs(z), 2), " ≤ 2.5)"))
    }
    
    tagList(
      p(strong("Variable: "), dname),
      p(strong("Your value: "), fmt_val),
      p(strong("Mean: "), fmt_mu, "  |  ", strong("SD: "), fmt_s),
      p(strong("Z-score: "), tags$span(style="font-size:18px; font-weight:600; color:#2c6fad;",
                                       round(z, 3))),
      p(paste0("This value is ", round(abs(z), 2),
               " standard deviations ", dir_txt, " the mean.")),
      p(flag_txt)
    )
  })
  
  # ── Outlier Tab ──────────────────────────────────────────────────────────────
  out_reactive <- reactive({
    col   <- input$out_var
    x     <- data[[col]]
    valid <- !is.na(x) & is.finite(x)
    xv    <- x[valid]
    z     <- z_scores(xv)
    thresh <- input$out_thresh
    flagged <- abs(z) > thresh
    
    list(
      x       = xv,
      z       = z,
      flagged = flagged,
      col     = col,
      thresh  = thresh
    )
  })
  
  output$out_total   <- renderText({ scales::comma(length(out_reactive()$x)) })
  output$out_flagged <- renderText({ scales::comma(sum(out_reactive()$flagged)) })
  output$out_pct     <- renderText({
    d <- out_reactive()
    paste0(round(100 * mean(d$flagged), 1), "%")
  })
  
  output$outlier_plot <- renderPlot({
    d      <- out_reactive()
    col    <- d$col
    dname  <- display_name(col)
    df     <- data.frame(x = d$x, z = d$z, flagged = d$flagged)
    
    # Jittered dot plot colored by flag status
    ggplot(df, aes(x = x, y = 0, color = flagged)) +
      geom_jitter(height = 0.4, alpha = 0.4, size = 1.2) +
      scale_color_manual(values = c("FALSE" = "#2c6fad", "TRUE" = "#e05c2a"),
                         labels = c("FALSE" = "Typical", "TRUE" = "Flagged (unusual)"),
                         name   = NULL) +
      scale_x_continuous(labels = if (col == "SalePrice") scales::dollar else scales::comma) +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            legend.position = "top") +
      labs(title = paste("Dot Plot of", dname, "— Flagged Observations in Orange"),
           x = dname, y = "")
  })
  
  output$out_table <- renderTable({
    d    <- out_reactive()
    col  <- d$col
    dname <- display_name(col)
    df   <- data.frame(Value = d$x, ZScore = round(d$z, 3))
    df   <- df[order(abs(df$ZScore), decreasing = TRUE), ]
    df   <- head(df, 10)
    names(df) <- c(dname, "Z-Score")
    if (col == "SalePrice") {
      df[[dname]] <- scales::dollar(df[[dname]])
    } else {
      df[[dname]] <- scales::comma(df[[dname]])
    }
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  
  # ── Correlation Explorer Tab ──────────────────────────────────────────────
  corr_reactive <- reactive({
    cx <- input$corr_x
    cy <- input$corr_y
    df <- data[, c(cx, cy)]
    df <- df[complete.cases(df) & is.finite(df[[cx]]) & is.finite(df[[cy]]), ]
    if (input$corr_std) {
      df[[cx]] <- z_scores(df[[cx]])
      df[[cy]] <- z_scores(df[[cy]])
    }
    df
  })
  
  output$corr_r <- renderText({
    df <- corr_reactive()
    cx <- input$corr_x
    cy <- input$corr_y
    r  <- cor(df[[cx]], df[[cy]], use = "complete.obs")
    round(r, 3)
  })
  
  output$corr_strength <- renderText({
    df <- corr_reactive()
    cx <- input$corr_x
    cy <- input$corr_y
    r  <- abs(cor(df[[cx]], df[[cy]], use = "complete.obs"))
    if (r >= 0.8) "Very Strong"
    else if (r >= 0.6) "Strong"
    else if (r >= 0.4) "Moderate"
    else if (r >= 0.2) "Weak"
    else "Very Weak / None"
  })
  
  output$corr_dir <- renderText({
    df <- corr_reactive()
    cx <- input$corr_x
    cy <- input$corr_y
    r  <- cor(df[[cx]], df[[cy]], use = "complete.obs")
    if (r > 0.05) "Positive ↑"
    else if (r < -0.05) "Negative ↓"
    else "No clear direction"
  })
  
  output$corr_n <- renderText({
    scales::comma(nrow(corr_reactive()))
  })
  
  output$corr_scatter <- renderPlot({
    df  <- corr_reactive()
    cx  <- input$corr_x
    cy  <- input$corr_y
    std <- input$corr_std
    dnx <- display_name(cx)
    dny <- display_name(cy)
    
    if (std) {
      dnx <- paste0("Z-Score: ", dnx)
      dny <- paste0("Z-Score: ", dny)
    }
    
    r   <- cor(df[[cx]], df[[cy]], use = "complete.obs")
    lbl <- paste0("r = ", round(r, 3))
    
    p <- ggplot(df, aes_string(x = cx, y = cy)) +
      geom_point(alpha = 0.25, color = "#2c6fad", size = 1.2) +
      theme_minimal(base_size = 13) +
      labs(title = paste(dnx, "vs.", dny),
           x = dnx, y = dny) +
      annotate("text", x = -Inf, y = Inf, label = lbl,
               hjust = -0.1, vjust = 1.5, size = 5, color = "#e05c2a", fontface = "bold")
    
    if (!std) {
      if (cx == "SalePrice") p <- p + scale_x_continuous(labels = scales::dollar)
      if (cy == "SalePrice") p <- p + scale_y_continuous(labels = scales::dollar)
    }
    
    if (input$corr_trend) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#e05c2a",
                           fill = "#e05c2a", alpha = 0.15, linewidth = 1)
    }
    p
  })
  
  # ── Correlation Table Tab ─────────────────────────────────────────────────
  output$corr_table_html <- renderUI({
    vars <- input$corr_tbl_vars
    if (length(vars) < 2) {
      return(p("Please select at least 2 variables."))
    }
    
    df  <- data[, vars, drop = FALSE]
    df  <- df[complete.cases(df), ]
    cm  <- cor(df, use = "complete.obs")
    
    # Build HTML table with color cells
    get_color <- function(r) {
      if (is.na(r)) return("#eee")
      if (r == 1)   return("#c0c0c0")
      if (r > 0) {
        intensity <- round(r * 220)
        sprintf("rgb(%d, %d, %d)", 255 - intensity %/% 2, 255 - intensity, 255 - intensity)
      } else {
        intensity <- round(abs(r) * 220)
        sprintf("rgb(%d, %d, %d)", 255 - intensity, 255 - intensity, 255 - intensity %/% 2)
      }
    }
    
    # Get friendly short names
    short_names <- sapply(vars, function(v) {
      idx <- which(corr_vars == v)
      if (length(idx) > 0) {
        nm <- names(corr_vars)[idx]
        # Shorten for table header
        nm <- gsub(" \\(.*\\)", "", nm)   # remove units in parens
        nm <- gsub("Above-Grade ", "", nm)
        nm
      } else v
    })
    
    header_cells <- paste0("<th>", short_names, "</th>", collapse = "")
    header_row   <- paste0("<tr><th></th>", header_cells, "</tr>")
    
    body_rows <- paste0(sapply(seq_along(vars), function(i) {
      cells <- paste0(sapply(seq_along(vars), function(j) {
        r    <- cm[i, j]
        col  <- get_color(r)
        txt  <- if (i == j) "1.000" else sprintf("%.3f", r)
        fw   <- if (abs(r) > 0.5 || i == j) "bold" else "normal"
        sprintf('<td style="background:%s; font-weight:%s;">%s</td>', col, fw, txt)
      }), collapse = "")
      paste0("<tr><th style='text-align:left; font-weight:600;'>",
             short_names[i], "</th>", cells, "</tr>")
    }), collapse = "")
    
    tbl_html <- paste0(
      '<table class="corr-table" style="margin-top:8px;">',
      "<thead>", header_row, "</thead>",
      "<tbody>", body_rows, "</tbody>",
      "</table>"
    )
    
    HTML(tbl_html)
  })
}

shinyApp(ui, server)