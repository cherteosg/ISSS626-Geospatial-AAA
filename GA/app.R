rlibrary(shiny)
library(tidyverse)
library(ggcorrplot)
library(broom)
library(patchwork)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f9f9f9; font-family: 'Georgia', serif; }
    .navbar { background-color: #042C53 !important; }
    h2, h3 { color: #042C53; }
    .well { background-color: #fff; border: 1px solid #eee; }
    .section-title {
      color: #042C53; font-size: 20px; font-weight: bold;
      border-bottom: 2px solid #FF007F; padding-bottom: 6px; margin-bottom: 16px;
    }
  "))),
  
  titlePanel(
    div(style = "color:#042C53; font-weight:bold;",
        "🇨🇴 Colombian Fintech — EDA Dashboard")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "📂 Upload customer_clean.csv", accept = ".csv"),
      hr(),
      helpText("Upload your cleaned CSV to populate all tabs.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        
        # ── Tab 1: Data Preview ──────────────────────────────────────────
        tabPanel("📋 Data Preview",
                 br(),
                 div(class = "section-title", "First 10 Rows"),
                 tableOutput("preview"),
                 br(),
                 div(class = "section-title", "Missing Values"),
                 tableOutput("missing")
        ),
        
        # ── Tab 2: Churn Distribution ────────────────────────────────────
        tabPanel("📊 Churn Distribution",
                 br(),
                 div(class = "section-title", "Distribution of Churn Probability"),
                 plotOutput("churn_hist", height = "400px"),
                 br(),
                 wellPanel(
                   strong("Key Observations:"),
                   tags$ul(
                     tags$li("All customers have churn risk."),
                     tags$li("Most customers are moderate to high risk (right-skewed)."),
                     tags$li("Churn probability appears capped at 0.5.")
                   )
                 )
        ),
        
        # ── Tab 3: Correlation Matrix ─────────────────────────────────────
        tabPanel("🔗 Correlation Matrix",
                 br(),
                 div(class = "section-title", "Correlation Heatmap"),
                 plotOutput("corrplot", height = "500px"),
                 br(),
                 wellPanel(
                   strong("Key Observations:"),
                   tags$ul(
                     tags$li("Strongest positive correlation: active products & feature usage."),
                     tags$li("Age is strongly correlated with churn probability."),
                     tags$li("Base satisfaction and satisfaction score are highly correlated.")
                   )
                 )
        ),
        
        # ── Tab 4: Age / Gender / Income ─────────────────────────────────
        tabPanel("👥 Age, Gender & Income",
                 br(),
                 div(class = "section-title", "Churn Probability vs Age by Gender & Income"),
                 plotOutput("age_gender_income", height = "450px"),
                 br(),
                 wellPanel(
                   strong("Key Observations:"),
                   tags$ul(
                     tags$li("Age is positively correlated with churn across all groups."),
                     tags$li("Effect is stronger at higher income levels, especially for 'Other' gender."),
                     tags$li("Male and Female groups show little differentiation at lower incomes.")
                   )
                 )
        ),
        
        # ── Tab 5: Satisfaction ───────────────────────────────────────────
        tabPanel("😊 Satisfaction",
                 br(),
                 div(class = "section-title", "Satisfaction Analysis"),
                 plotOutput("satisfaction", height = "650px"),
                 br(),
                 wellPanel(
                   strong("Key Observations:"),
                   tags$ul(
                     tags$li("Product satisfaction appears categorical unlike the other two."),
                     tags$li("CLV categories overlap significantly in scatter plots."),
                     tags$li("Bronze customer satisfaction trends downward over tenure.")
                   )
                 )
        ),
        
        # ── Tab 6: Confirmatory ───────────────────────────────────────────
        tabPanel("🔬 Confirmatory Analysis",
                 br(),
                 div(class = "section-title", "Pearson Correlation Tests"),
                 verbatimTextOutput("cor_age"),
                 verbatimTextOutput("cor_products"),
                 verbatimTextOutput("cor_satisfaction"),
                 hr(),
                 div(class = "section-title", "Multiple Linear Regression"),
                 verbatimTextOutput("lm_summary"),
                 plotOutput("lm_plot", height = "350px"),
                 hr(),
                 div(class = "section-title", "ANOVA Tests — Churn Probability"),
                 verbatimTextOutput("anova_income"),
                 verbatimTextOutput("anova_gender"),
                 verbatimTextOutput("anova_interaction"),
                 hr(),
                 div(class = "section-title", "ANOVA Tests — CLV"),
                 verbatimTextOutput("anova_clv"),
                 verbatimTextOutput("anova_sat"),
                 br(),
                 wellPanel(
                   strong("Key Observations:"),
                   tags$ul(
                     tags$li("Age increases churn probability."),
                     tags$li("More product usage reduces churn."),
                     tags$li("Higher satisfaction reduces churn."),
                     tags$li("Overall R² is low (0.073) — other factors drive churn."),
                     tags$li("Gender is insignificant; income is significant but weak.")
                   )
                 )
        )
      )
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output) {
  
  df <- reactive({
    req(input$file)
    read_csv(input$file$datapath, show_col_types = FALSE)
  })
  
  # Data Preview
  output$preview <- renderTable({ head(df(), 10) })
  
  output$missing <- renderTable({
    d <- df()
    data.frame(
      Variable = names(d),
      Missing  = colSums(is.na(d)),
      Percent  = round(colMeans(is.na(d)) * 100, 1)
    ) %>% filter(Missing > 0)
  })
  
  # Churn Histogram
  output$churn_hist <- renderPlot({
    d <- df()
    ggplot(d, aes(x = churn_probability)) +
      geom_histogram(binwidth = 0.01, fill = "#FFB3C6", colour = "white") +
      geom_vline(xintercept = mean(d$churn_probability, na.rm = TRUE),
                 colour = "#FF007F", linetype = "dashed", linewidth = 0.8) +
      annotate("text",
               x = mean(d$churn_probability, na.rm = TRUE) + 0.015,
               y = 500,
               label = "Mean", colour = "#FF007F", size = 3.5) +
      labs(title = "Distribution of churn probability",
           x = "Churn probability", y = "Count") +
      theme_classic()
  })
  
  # Correlation Matrix
  output$corrplot <- renderPlot({
    d <- df()
    variables <- c("age", "household_size", "active_products",
                   "feature_usage_diversity", "failed_transactions",
                   "tx_count", "base_satisfaction", "tx_satisfaction",
                   "product_satisfaction", "satisfaction_score",
                   "support_tickets_count", "resolved_tickets_ratio",
                   "app_store_rating", "monthly_transaction_count",
                   "customer_tenure", "churn_probability")
    vars_present <- intersect(variables, names(d))
    corrmatrix <- cor(d[, vars_present], use = "pairwise.complete.obs")
    ggcorrplot(corrmatrix, method = "square", type = "lower",
               lab = TRUE, lab_size = 2.5, tl.cex = 9,
               colors = c("#FF007F", "#B5D4F4", "#042C53"),
               title = "Correlation Heatmap") +
      theme_classic() +
      theme(plot.title  = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8),
            axis.title  = element_blank(),
            plot.margin = margin(20, 20, 20, 20))
  })
  
  # Age / Gender / Income
  output$age_gender_income <- renderPlot({
    df() %>%
      mutate(income_bracket = factor(income_bracket,
                                     levels = c("Low","Medium","High","Very High"))) %>%
      ggplot(aes(x = age, y = churn_probability, colour = gender)) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ income_bracket) +
      labs(title  = "Churn probability vs age, by gender and income bracket",
           x = "Age", y = "Churn probability", colour = "Gender") +
      theme_classic()
  })
  
  # Satisfaction
  output$satisfaction <- renderPlot({
    d <- df()
    p1 <- d %>%
      select(clv_segment, base_satisfaction, tx_satisfaction,
             product_satisfaction, churn_probability) %>%
      pivot_longer(cols = c(base_satisfaction, tx_satisfaction, product_satisfaction),
                   names_to = "satisfaction_type", values_to = "score") %>%
      mutate(
        satisfaction_type = recode(satisfaction_type,
                                   "base_satisfaction"    = "Base",
                                   "tx_satisfaction"      = "Transaction",
                                   "product_satisfaction" = "Product"),
        clv_segment = factor(clv_segment,
                             levels = c("Bronze","Silver","Gold","Platinum"))
      ) %>%
      ggplot(aes(x = score, y = churn_probability, colour = clv_segment)) +
      geom_point(alpha = 0.05, size = 0.6) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_colour_manual(values = c("#FFB3C6","#FF007F","#B5D4F4","#042C53")) +
      facet_wrap(~ satisfaction_type, ncol = 3, scales = "free_x") +
      labs(title = "Satisfaction vs Churn by CLV",
           x = "Satisfaction score", y = "Churn probability", colour = "CLV segment") +
      theme_classic() +
      theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
            strip.text = element_text(size = 8, face = "bold"),
            legend.position = "none")
    
    p2 <- d %>%
      mutate(
        tenure_bucket = cut(customer_tenure,
                            breaks = c(0,3,6,9,12,Inf),
                            labels = c("0-3m","3-6m","6-9m","9-12m","12m+")),
        clv_segment = factor(clv_segment,
                             levels = c("Bronze","Silver","Gold","Platinum"))
      ) %>%
      group_by(tenure_bucket, clv_segment) %>%
      summarise(avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
                .groups = "drop") %>%
      ggplot(aes(x = tenure_bucket, y = avg_satisfaction,
                 colour = clv_segment, group = clv_segment)) +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      scale_colour_manual(values = c("#FFB3C6","#FF007F","#B5D4F4","#042C53")) +
      labs(title = "Satisfaction over Tenure by CLV",
           x = "Tenure", y = "Avg satisfaction score", colour = "CLV segment") +
      theme_classic() +
      theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
            legend.position = "bottom")
    
    p1 / p2 + plot_annotation(
      title = "Satisfaction Analysis",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  })
  
  # Confirmatory — Correlation tests
  output$cor_age          <- renderPrint({
    cat("── Age vs Churn Probability ──\n")
    cor.test(df()$age, df()$churn_probability, method = "pearson")
  })
  output$cor_products     <- renderPrint({
    cat("── Active Products vs Feature Usage ──\n")
    cor.test(df()$active_products, df()$feature_usage_diversity, method = "pearson")
  })
  output$cor_satisfaction <- renderPrint({
    cat("── Base Satisfaction vs Satisfaction Score ──\n")
    cor.test(df()$base_satisfaction, df()$satisfaction_score, method = "pearson")
  })
  
  # Linear Regression
  churn_model <- reactive({
    lm(churn_probability ~ age + base_satisfaction + active_products, data = df())
  })
  
  output$lm_summary <- renderPrint({ summary(churn_model()) })
  
  output$lm_plot <- renderPlot({
    tidy(churn_model(), conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
      geom_pointrange(colour = "#FF007F") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#042C53") +
      labs(title    = "Churn Probability Predictors",
           subtitle = paste("R² =", round(summary(churn_model())$r.squared, 3)),
           x = "Coefficient estimate", y = NULL) +
      theme_classic() +
      theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5, colour = "#444441"))
  })
  
  # ANOVA — Churn
  output$anova_income       <- renderPrint({
    cat("── One-way ANOVA: Churn ~ Income ──\n")
    summary(aov(churn_probability ~ income_bracket, data = df()))
  })
  output$anova_gender       <- renderPrint({
    cat("── One-way ANOVA: Churn ~ Gender ──\n")
    summary(aov(churn_probability ~ gender, data = df()))
  })
  output$anova_interaction  <- renderPrint({
    cat("── Two-way ANOVA: Churn ~ Income * Gender ──\n")
    summary(aov(churn_probability ~ income_bracket * gender, data = df()))
  })
  
  # ANOVA — CLV
  output$anova_clv <- renderPrint({
    cat("── One-way ANOVA: Churn ~ CLV Segment ──\n")
    summary(aov(churn_probability ~ clv_segment, data = df()))
  })
  output$anova_sat <- renderPrint({
    cat("── One-way ANOVA: Satisfaction ~ CLV Segment ──\n")
    summary(aov(satisfaction_score ~ clv_segment, data = df()))
  })
}

shinyApp(ui = ui, server = server)