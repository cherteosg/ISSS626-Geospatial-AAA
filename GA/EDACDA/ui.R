library(shiny)
library(bslib)

ui <- page_navbar(
  title = "Colombian Fintech – Churn Analysis",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#FF007F",
    secondary  = "#042C53"
  ),
  
  # ── Screen 1: Home ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Home",
    icon  = icon("house"),
    
    layout_columns(
      col_widths = c(8, 4),
      
      # Left: churn probability histogram
      card(
        card_header("Distribution of Churn Probability"),
        plotOutput("hist_churn", height = "350px"),
        card_footer(
          "Hover over bars to explore individual churn probability values."
        )
      ),
      
      # Right: customer info & tenure
      card(
        card_header("Customer Overview"),
        card_body(
          p("This dashboard explores churn probability modelling for a Colombian
            Fintech company with the objective of improving", strong("customer retention.")),
          hr(),
          h6("Dataset Summary"),
          tableOutput("summary_table"),
          hr(),
          h6("Customer Tenure Distribution"),
          plotOutput("tenure_plot", height = "180px"),
          p(em("Hover to view count per tenure bucket."),
            style = "font-size:0.8rem; color:grey;")
        )
      )
    )
  ),
  
  # ── Screen 2: Correlation Matrix ─────────────────────────────────────────────
  nav_panel(
    title = "Correlation",
    icon  = icon("table-cells"),
    
    layout_columns(
      col_widths = c(9, 3),
      
      # Left: heatmap
      card(
        card_header("Correlation Heatmap"),
        plotOutput("corr_heatmap", height = "520px")
      ),
      
      # Right: variable selector + rationale tooltip placeholder
      card(
        card_header("Variable Selection"),
        card_body(
          checkboxGroupInput(
            inputId  = "corr_vars",
            label    = "Select variables to include:",
            choices  = c(
              "age", "household_size", "active_products",
              "feature_usage_diversity", "failed_transactions",
              "tx_count", "base_satisfaction", "tx_satisfaction",
              "product_satisfaction", "satisfaction_score",
              "support_tickets_count", "resolved_tickets_ratio",
              "app_store_rating", "monthly_transaction_count",
              "customer_tenure", "churn_probability"
            ),
            selected = c(
              "age", "active_products", "base_satisfaction",
              "satisfaction_score", "customer_tenure", "churn_probability"
            )
          ),
          hr(),
          h6("Key Observations"),
          tags$ul(
            tags$li("Strongest positive correlation: products × features used."),
            tags$li("Age shows notable correlation with churn probability."),
            tags$li("Base satisfaction and overall satisfaction score are highly correlated.")
          )
        )
      )
    )
  ),
  
  # ── Screen 3: Churn Predictors ───────────────────────────────────────────────
  nav_panel(
    title = "Churn Predictors",
    icon  = icon("chart-line"),
    
    # Draggable / side-by-side panel arrangement simulated with layout_columns
    layout_columns(
      col_widths = c(6, 6),
      
      # Panel A: Age × Gender × Income facet plot
      card(
        card_header("Churn Probability vs Age, Gender & Income"),
        card_body(
          selectInput(
            inputId  = "income_filter",
            label    = "Filter income bracket:",
            choices  = c("All", "Low", "Medium", "High", "Very High"),
            selected = "All"
          ),
          plotOutput("churn_age_plot", height = "320px")
        )
      ),
      
      # Panel B: Coefficient plot from linear regression
      card(
        card_header("Linear Regression – Churn Predictors"),
        card_body(
          checkboxGroupInput(
            inputId  = "lm_predictors",
            label    = "Predictors to include:",
            choices  = c("age", "base_satisfaction", "active_products"),
            selected = c("age", "base_satisfaction", "active_products")
          ),
          plotOutput("coef_plot", height = "250px"),
          uiOutput("r_squared_text")
        )
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      # Panel C: Satisfaction scatter
      card(
        card_header("Satisfaction vs Churn by CLV Segment"),
        plotOutput("sat_scatter", height = "280px")
      ),
      
      # Panel D: Satisfaction over tenure line chart
      card(
        card_header("Satisfaction over Tenure by CLV Segment"),
        plotOutput("sat_tenure", height = "280px")
      )
    )
  ),
  
  # ── Screen 4: Conclusions ────────────────────────────────────────────────────
  nav_panel(
    title = "Conclusions",
    icon  = icon("lightbulb"),
    
    layout_columns(
      col_widths = c(6, 6),
      
      # Statistical findings
      card(
        card_header("Statistical Findings"),
        card_body(
          h6("Pearson Correlation Tests"),
          tableOutput("pearson_table"),
          hr(),
          h6("ANOVA Results"),
          tableOutput("anova_table")
        )
      ),
      
      # Business recommendations
      card(
        card_header("Business Recommendations"),
        card_body(
          tags$ol(
            tags$li(strong("Target older, high-income customers"),
                    " – age is the strongest churn predictor across all segments."),
            tags$li(strong("Drive product adoption"),
                    " – more active products correlates with lower churn."),
            tags$li(strong("Re-engage Bronze customers early"),
                    " – satisfaction declines over tenure for this segment."),
            tags$li(strong("Deprioritise gender as a segmentation variable"),
                    " – ANOVA confirms it is not a significant churn predictor."),
            tags$li(strong("Investigate the 0.5 churn cap"),
                    " – the model ceiling limits identification of the highest-risk customers.")
          ),
          hr(),
          p(em("Overall linear model R² = 0.073, indicating further feature
               engineering or non-linear modelling is recommended."),
            style = "font-size:0.85rem; color:grey;")
        )
      )
    )
  )
)

