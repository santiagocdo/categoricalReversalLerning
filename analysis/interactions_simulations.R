library(ggplot2)
library(dplyr)
library(patchwork)

# ------------------------------------------------------------------------------
# 1. DATA SIMULATION
# ------------------------------------------------------------------------------
set.seed(42)

n_per_group <- 30  # 30 subjects per group across 6 blocks
blocks <- 0:5
groups <- c("Group A (0)", "Group B (1)")

sim_data <- expand.grid(
  subject = 1:n_per_group,
  group   = groups,
  block   = blocks
) %>%
  mutate(
    # True Population Parameters:
    # Intercept = 10
    # Block main effect = 1.5 (overall upward trend)
    # Group main effect = -2.0 (Group B starts lower)
    # Block * Group interaction = 2.0 (Group B learns/improves much faster)
    mu = 10 + 
      1.5 * block + 
      -2.0 * (group == "Group B (1)") + 
      2.0 * block * (group == "Group B (1)"),
    
    # Add random individual noise
    y = mu + rnorm(n(), mean = 0, sd = 2.5)
  )

# ------------------------------------------------------------------------------
# 2. MODEL FITTING & PREDICTION GENERATION
# ------------------------------------------------------------------------------
# Model 1: Block effect only
m1 <- lm(y ~ block, data = sim_data)

# Model 2: Group effect only
m2 <- lm(y ~ group, data = sim_data)

# Model 3: Main effects (Additive: block + group)
m3 <- lm(y ~ block + group, data = sim_data)

# Model 4: Full Interaction model (block * group)
m4 <- lm(y ~ block * group, data = sim_data)

# Create a dense grid for smooth model fit line visualization
pred_grid <- expand.grid(
  block = seq(0, 5, length.out = 100),
  group = groups
)

pred_grid <- pred_grid %>%
  mutate(
    pred_m1 = predict(m1, newdata = pred_grid),
    pred_m2 = predict(m2, newdata = pred_grid),
    pred_m3 = predict(m3, newdata = pred_grid),
    pred_m4 = predict(m4, newdata = pred_grid)
  )

# ------------------------------------------------------------------------------
# 3. VISUALIZATION FUNCTION FOR MODEL COMPARISON
# ------------------------------------------------------------------------------
plot_model_fit <- function(pred_col, title_text, subtitle_text) {
  ggplot(sim_data, aes(x = block, y = y, color = group, fill = group)) +
    # Raw data points (jittered horizontally)
    geom_jitter(position = position_jitter(width = 0.15, height = 0), 
                alpha = 0.35, size = 1.8) +
    # Fitted model line
    geom_line(data = pred_grid, aes(y = .data[[pred_col]]), linewidth = 1.2) +
    scale_x_continuous(breaks = 0:5) +
    scale_color_manual(values = c("Group A (0)" = "#2b5c8f", "Group B (1)" = "#d95f02")) +
    scale_fill_manual(values = c("Group A (0)" = "#2b5c8f", "Group B (1)" = "#d95f02")) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Block",
      y = "Outcome (Y)",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "bottom"
    )
}

# Generate 4 Individual Panels
p1 <- plot_model_fit("pred_m1", "Model 1: Y ~ block", "Single slope across all data")
p2 <- plot_model_fit("pred_m2", "Model 2: Y ~ group", "Group means (flat lines across blocks)")
p3 <- plot_model_fit("pred_m3", "Model 3: Y ~ block + group", "Parallel slopes (additive main effects)")
p4 <- plot_model_fit("pred_m4", "Model 4: Y ~ block * group", "Independent slopes (full interaction)")

# Combine 4 plots into a 2x2 grid using patchwork
combined_models_plot <- (p1 + p2) / (p3 + p4) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display 4-Model Comparison Plot
print(combined_models_plot)


# ------------------------------------------------------------------------------
# 4. INTERACTIVE / PARAMETERIZED INTERACTION DEMONSTRATOR
# ------------------------------------------------------------------------------
# Function to simulate data and plot effects based on custom interaction parameters
simulate_and_plot_interaction <- function(b_block = 1.0, 
                                          b_group = 0.0, 
                                          b_interaction = 2.0, 
                                          noise_sd = 0) {
  
  df <- expand.grid(
    subject = 1:30,
    group   = c("Group A (0)", "Group B (1)"),
    block   = 0:5
  ) %>%
    mutate(
      mu = 10 + 
        b_block * block + 
        b_group * (group == "Group B (1)") + 
        b_interaction * block * (group == "Group B (1)"),
      y = mu + rnorm(n(), mean = 0, sd = noise_sd)
    )
  
  # Fit full model on simulated data
  fit <- lm(y ~ block * group, data = df)
  
  grid <- expand.grid(
    block = seq(0, 5, length.out = 100),
    group = c("Group A (0)", "Group B (1)")
  )
  grid$y_hat <- predict(fit, newdata = grid)
  
  ggplot(df, aes(x = block, y = y, color = group)) +
    geom_jitter(position = position_jitter(width = 0.15), alpha = 0.35, size = 1.8) +
    geom_line(data = grid, aes(y = y_hat), linewidth = 1.3) +
    scale_x_continuous(breaks = 0:5) +
    scale_color_manual(values = c("Group A (0)" = "#2b5c8f", "Group B (1)" = "#d95f02")) +
    labs(
      title = paste0("Interaction Demo: b_interaction = ", b_interaction),
      subtitle = paste0("b_block = ", b_block, " | b_group = ", b_group, " | Noise SD = ", noise_sd),
      x = "Block",
      y = "Outcome (Y)",
      color = "Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Example Usage of Interactive Demonstrator:
# 1. Positive interaction (Diverging lines)
p_demo1 <- simulate_and_plot_interaction(b_block = 1.0, b_group = 1, b_interaction = 0)

# 2. Negative interaction (Crossover / Converging lines)
p_demo2 <- simulate_and_plot_interaction(b_block = 1.0, b_group = 1, b_interaction = -.5)
# IMPORTANT: Slope for Group B is b_block + b_interaction (1-.5=.5)

# Display demo comparison
print(p_demo1 + p_demo2 + plot_layout(guides = "collect") & theme(legend.position = "bottom"))

