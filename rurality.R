
# Rurality per education 

# Create tertiles 
df_plot <- df_plot %>%
  mutate(rural_tertile = cut(
    sh_rural_70,
    breaks = quantile(sh_rural_70, c(0, 1/3, 2/3, 1), na.rm = TRUE),
    labels = c("Urban (low rural)", "Mixed", "Rural (high rural)"),
    include.lowest = TRUE
  ))

# Plot: education vs No vote, by tertile
ggplot(df_plot, aes(x = mean_years, y = VoteShareNo, 
                    color = rural_tertile, fill = rural_tertile)) +
  geom_point(alpha = 0.35, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
              alpha = 0.18, linewidth = 1.1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Mean years of education (commune)",
    y = "Vote share No",
    color = "Rurality tertile (1970)",
    fill  = "Rurality tertile (1970)",
    title = "Education and No vote, by rurality tertile",
    subtitle = "Quadratic fits with 95% CIs"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title.position = "plot")

# Distribution of rurality

# Calculate key stats for annotations
n_total <- sum(!is.na(df_plot$sh_rural_70))
n_fully_urban <- sum(df_plot$sh_rural_70 == 0, na.rm = TRUE)
pct_fully_urban <- n_fully_urban / n_total
median_rural <- median(df_plot$sh_rural_70, na.rm = TRUE)

ggplot(df_plot, aes(x = sh_rural_70)) +
  # Histogram in proportions (not density)
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count))),
                 bins = 25, fill = "#4A90C2", color = "white", alpha = 0.9) +
  
  # Shaded zones for substantive categories
  annotate("rect", xmin = -2, xmax = 25, ymin = 0, ymax = Inf,
           fill = "#E63946", alpha = 0.06) +
  annotate("rect", xmin = 60, xmax = 102, ymin = 0, ymax = Inf,
           fill = "#2A9D8F", alpha = 0.06) +
  
  # Median line
  geom_vline(xintercept = median_rural, linetype = "dashed", 
             color = "grey30", linewidth = 0.6) +
  
  # Zone labels at top
  annotate("text", x = 12, y = 0.085, label = "URBAN", 
           color = "#E63946", fontface = "bold", size = 3.8) +
  annotate("text", x = 42, y = 0.085, label = "MIXED", 
           color = "grey40", fontface = "bold", size = 3.8) +
  annotate("text", x = 80, y = 0.085, label = "RURAL", 
           color = "#2A9D8F", fontface = "bold", size = 3.8) +
  
  # Callout for the urban spike
  annotate("curve", x = 15, y = 0.07, xend = 2, yend = pct_fully_urban + 0.003,
           arrow = arrow(length = unit(0.2, "cm")), 
           color = "grey30", curvature = 0.3) +
  annotate("text", x = 22, y = 0.072, 
           label = paste0(round(pct_fully_urban * 100), 
           "% of communes\nare fully urban"),
           hjust = 0, size = 3.3, color = "grey20") +
  
  # Median callout
  annotate("text", x = median_rural + 1, y = 0.06, 
           label = paste0("Median: ", round(median_rural), "%"),
           hjust = 0, size = 3.3, color = "grey20", fontface = "italic") +
  
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = seq(0, 100, 25),
                     labels = c("0%\n(fully urban)", "25%", "50%", "75%", 
                     "100%\n(fully rural)"),
                     expand = c(0.01, 0.01)) +
  
  labs(
    x = "Share of commune population living in rural areas (1970)",
    y = "Share of communes",
    title = "Chilean communes were sharply bimodal in 1970",
    subtitle = "Most communes were either heavily rural or fully urban — 
    few sat in between"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# Rurality by tertile 

# Build rurality tertiles — assuming you have a 1970 rurality variable
# Adjust the variable name (e.g. sh_rural_70) to match your data
df_facet <- df_plot %>%
  filter(!is.na(sh_rural_70)) %>%
  mutate(rurality_tertile = ntile(sh_rural_70, 3),
         rurality_tertile = factor(rurality_tertile,
                                   levels = 1:3,
                                   labels = c("Urban",
                                              "Mixed",
                                              "Rural")))

# Tertile colors matching your original plot
tertile_colors <- c("Urban"  = "#E74C3C",
                    "Mixed" = "#3498DB",
                    "Rural" = "#27AE60")

ggplot(df_facet, aes(x = mean_years, y = VoteShareNo)) +
  # Points: only the panel's own tertile, colored
  geom_point(aes(color = rurality_tertile), alpha = 0.6, size = 1.5) +
  # Quadratic fit: restricted to the panel's tertile
  geom_smooth(aes(color = rurality_tertile, fill = rurality_tertile),
              method = "lm", formula = y ~ x + I(x^2),
              se = TRUE, alpha = 0.2) +
facet_wrap(~ rurality_tertile, nrow = 1, scales = "free_x") +
  scale_color_manual(values = tertile_colors) +
  scale_fill_manual(values = tertile_colors) +
  labs(title = "Education and No vote, by rurality tertile",
       subtitle = "Quadratic fits with 95% CIs",
       x = "Mean years of education (municipality)",
       y = "Vote share No") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(1.2, "lines"))

# Votes and rurality 

ggplot(df_plot, aes(x = sh_rural_70, y = VoteShareNo)) +
  geom_point(aes(color = rural_tertile), alpha = 0.55, size = 1.8) +
  geom_smooth(method = "loess", color = "grey25", fill = "grey25", 
  alpha = 0.15) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Rural share (1970)",
    y = "Vote share No",
    color = "Rurality tertile",
    title = "No vote share by rurality per municipality"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title.position = "plot")
```

