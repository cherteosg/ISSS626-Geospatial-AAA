library(shiny)
library(tidyverse)
library(scales)
library(ggcorrplot)
library(broom)
library(lubridate)

# ── Colour palette ─────────────────────────────────────────────────────────────
CLV_COLOURS <- c(
  "Bronze"   = "#FFB3C6",
  "Silver"   = "#FF007F",
  "Gold"     = "#B5D4F4",
  "Platinum" = "#042C53"
)

PINK  <- "#FF007F"
NAVY  <- "#042C53"
LBLUE <- "#B5D4F4"

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Load & cache data ────────────────────────────────────────────────────────
  customer <- reactive({
    read_csv("customer_clean.csv", show_col_types = FALSE) %>%
      mutate(
        clv_segment    = factor(clv_segment,
                                levels = c("Bronze", "Silver", "Gold", "Platinum")),
        income_bracket = factor(income_bracket,
                                levels = c("Low", "Medium", "High", "Very High"))
      )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # SCREEN 1 – Home
  # ════════════════════════════════════════════════════════════════════════════
  
  # Churn probability histogram
  output$hist_churn <- renderPlot({
    df <- customer()
    mn <- mean(df$churn_probability, na.rm = TRUE)
    
    ggplot(df, aes(x = churn_probability)) +
      geom_histogram(binwidth = 0.01, fill = "#FFB3C6", colour = "white") +
      geom_vline(xintercept = mn, colour = PINK,
                 linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = mn + 0.015, y = 500,
               label = "Mean", colour = PINK, size = 3.5) +
      labs(title = "Distribution of Churn Probability",
           x = "Churn Probability", y = "Count") +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  # Dataset summary table
  output$summary_table <- renderTable({
    df <- customer()
    data.frame(
      Metric  = c("Total customers", "Variables",
                  "Avg churn probability", "Avg satisfaction score"),
      Value   = c(
        format(nrow(df), big.mark = ","),
        ncol(df),
        round(mean(df$churn_probability, na.rm = TRUE), 3),
        round(mean(df$satisfaction_score, na.rm = TRUE), 3)
      )
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Tenure distribution bar chart
  output$tenure_plot <- renderPlot({
    customer() %>%
      mutate(tenure_bucket = cut(customer_tenure,
                                 breaks = c(0, 3, 6, 9, 12, Inf),
                                 labels = c("0-3m", "3-6m", "6-9m",
                                            "9-12m", "12m+"))) %>%
      count(tenure_bucket) %>%
      ggplot(aes(x = tenure_bucket, y = n)) +
      geom_col(fill = LBLUE, colour = "white") +
      labs(x = "Tenure bucket", y = "Count") +
      theme_classic() +
      theme(axis.text = element_text(size = 8))
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # SCREEN 2 – Correlation Heatmap
  # ════════════════════════════════════════════════════════════════════════════
  
  output$corr_heatmap <- renderPlot({
    req(length(input$corr_vars) >= 2)
    
    df   <- customer()
    vars <- input$corr_vars
    
    corrmatrix <- cor(df[, vars], use = "pairwise.complete.obs")
    
    ggcorrplot(corrmatrix,
               method   = "square",
               type     = "lower",
               lab      = TRUE,
               lab_size = 2.5,
               tl.cex   = 9,
               colors   = c(PINK, LBLUE, NAVY),
               title    = "Correlation Heatmap") +
      theme_classic() +
      theme(
        plot.title  = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
      )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # SCREEN 3 – Churn Predictors
  # ════════════════════════════════════════════════════════════════════════════
  
  # Panel A: Age × Gender × Income facet
  output$churn_age_plot <- renderPlot({
    df <- customer()
    
    if (input$income_filter != "All") {
      df <- df %>% filter(income_bracket == input$income_filter)
    }
    
    df %>%
      ggplot(aes(x = age, y = churn_probability, colour = gender)) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ income_bracket) +
      labs(title  = "Churn Probability vs Age, by Gender & Income Bracket",
           x      = "Age",
           y      = "Churn Probability",
           colour = "Gender") +
      theme_classic() +
      theme(
        plot.title  = element_text(face = "bold", hjust = 0.5, size = 11),
        strip.text  = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  
  # Panel B: Regression coefficient plot
  churn_model <- reactive({
    req(length(input$lm_predictors) >= 1)
    df      <- customer()
    formula <- as.formula(
      paste("churn_probability ~", paste(input$lm_predictors, collapse = " + "))
    )
    lm(formula, data = df)
  })
  
  output$coef_plot <- renderPlot({
    model <- churn_model()
    
    tidy(model, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      ggplot(aes(x = estimate, y = term,
                 xmin = conf.low, xmax = conf.high)) +
      geom_pointrange(colour = PINK) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = NAVY) +
      labs(title = "Churn Probability Predictors",
           x     = "Coefficient estimate",
           y     = NULL) +
      theme_classic() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
  })
  
  output$r_squared_text <- renderUI({
    r2 <- round(summary(churn_model())$r.squared, 3)
    tags$p(
      tags$em(paste0("Model R² = ", r2,
                     " — a low score suggests additional predictors are needed.")),
      style = "font-size:0.85rem; color:grey;"
    )
  })
  
  # Panel C: Satisfaction scatter
  output$sat_scatter <- renderPlot({
    customer() %>%
      select(clv_segment, base_satisfaction, tx_satisfaction,
             product_satisfaction, churn_probability) %>%
      pivot_longer(cols      = c(base_satisfaction, tx_satisfaction,
                                 product_satisfaction),
                   names_to  = "satisfaction_type",
                   values_to = "score") %>%
      mutate(satisfaction_type = recode(satisfaction_type,
                                        "base_satisfaction"    = "Base",
                                        "tx_satisfaction"      = "Transaction",
                                        "product_satisfaction" = "Product")) %>%
      ggplot(aes(x = score, y = churn_probability, colour = clv_segment)) +
      geom_point(alpha = 0.05, size = 0.6) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_colour_manual(values = CLV_COLOURS) +
      facet_wrap(~ satisfaction_type, ncol = 3, scales = "free_x") +
      labs(title  = "Satisfaction vs Churn by CLV",
           x      = "Satisfaction score",
           y      = "Churn probability",
           colour = "CLV segment") +
      theme_classic() +
      theme(
        plot.title      = element_text(size = 11, face = "bold", hjust = 0.5),
        strip.text      = element_text(size = 8, face = "bold"),
        axis.text       = element_text(size = 7),
        legend.position = "none"
      )
  })
  
  # Panel D: Satisfaction over tenure line chart
  output$sat_tenure <- renderPlot({
    customer() %>%
      mutate(tenure_bucket = cut(customer_tenure,
                                 breaks = c(0, 3, 6, 9, 12, Inf),
                                 labels = c("0-3m", "3-6m", "6-9m",
                                            "9-12m", "12m+"))) %>%
      group_by(tenure_bucket, clv_segment) %>%
      summarise(avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
                .groups = "drop") %>%
      ggplot(aes(x = tenure_bucket, y = avg_satisfaction,
                 colour = clv_segment, group = clv_segment)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      scale_colour_manual(values = CLV_COLOURS) +
      labs(title  = "Satisfaction over Tenure by CLV",
           x      = "Tenure",
           y      = "Avg satisfaction score",
           colour = "CLV segment") +
      theme_classic() +
      theme(
        plot.title      = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.text       = element_text(size = 8),
        legend.position = "bottom",
        legend.title    = element_text(size = 8),
        legend.text     = element_text(size = 7)
      )
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # SCREEN 4 – Conclusions
  # ════════════════════════════════════════════════════════════════════════════
  
  # Pearson correlation results table
  output$pearson_table <- renderTable({
    df <- customer()
    
    tests <- list(
      list(label = "Age vs Churn",
           x = df$age,               y = df$churn_probability),
      list(label = "Active Products vs Feature Usage",
           x = df$active_products,   y = df$feature_usage_diversity),
      list(label = "Base Satisfaction vs Overall Score",
           x = df$base_satisfaction, y = df$satisfaction_score)
    )
    
    map_dfr(tests, function(t) {
      res <- cor.test(t$x, t$y, method = "pearson")
      data.frame(
        Relationship = t$label,
        `Correlation (r)` = round(res$estimate, 3),
        `p-value`         = ifelse(res$p.value < 0.001, "< 0.001",
                                   round(res$p.value, 4)),
        Significant       = ifelse(res$p.value < 0.05, "Yes", "No"),
        check.names = FALSE
      )
    })
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ANOVA results table
  output$anova_table <- renderTable({
    df <- customer()
    
    anova_tests <- list(
      list(label  = "Churn ~ Income Bracket",
           result = summary(aov(churn_probability ~ income_bracket, data = df))),
      list(label  = "Churn ~ Gender",
           result = summary(aov(churn_probability ~ gender, data = df))),
      list(label  = "Churn ~ Income × Gender",
           result = summary(aov(churn_probability ~ income_bracket * gender, data = df))),
      list(label  = "Churn ~ CLV Segment",
           result = summary(aov(churn_probability ~ clv_segment, data = df))),
      list(label  = "Satisfaction ~ CLV Segment",
           result = summary(aov(satisfaction_score ~ clv_segment, data = df)))
    )
    
    map_dfr(anova_tests, function(t) {
      pval <- t$result[[1]]$`Pr(>F)`[1]
      data.frame(
        Test        = t$label,
        `F-value`   = round(t$result[[1]]$`F value`[1], 3),
        `p-value`   = ifelse(pval < 0.001, "< 0.001", round(pval, 4)),
        Significant = ifelse(pval < 0.05, "Yes", "No"),
        check.names = FALSE
      )
    })
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
}
