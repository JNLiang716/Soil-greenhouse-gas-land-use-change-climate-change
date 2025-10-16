# Soil-greenhouse-gas-land-use-change-climate-change
Meta-analysis_R
library(ggplot2)
library(metafor)
library(dplyr)

data <- read.csv("N2O_20241015.csv")
max(data$effect_size)

data$pooled_sd <- sqrt(((data$n_Control - 1) * data$SD_Control^2 + (data$n_treatment - 1) * data$SD_treatment^2) /
                         (data$n_Control + data$n_treatment - 2))
data$effect_size <- (data$Treatment - data$Control) / data$pooled_sd
data$variance <- (data$n_Control + data$n_treatment) / (data$n_Control * data$n_treatment) + 
  (data$effect_size^2) / (2 * (data$n_Control + data$n_treatment))

unique_types <- unique(data$AfforestationTypes)
results <- list()
summary_results <- data.frame(AfforestationType=character(), EffectSize=numeric(), LowerCI=numeric(), UpperCI=numeric(), Significant=logical(), Stars=character(), PaperCount=numeric(), Label=character(), stringsAsFactors=FALSE)

for (aff_type in unique_types) {
  subset_data <- subset(data, AfforestationTypes == aff_type)
  meta_analysis <- rma(yi = subset_data$effect_size, vi = subset_data$variance, method = "DL")
  results[[aff_type]] <- meta_analysis
  
  # Determine significance based on whether the CI excludes zero
  significant <- (meta_analysis$ci.lb > 0 | meta_analysis$ci.ub < 0)
  
  # Determine the number of stars based on p-value
  p_value <- meta_analysis$pval
  stars <- ifelse(p_value < 0.001, "***",
                  ifelse(p_value < 0.01, "**",
                         ifelse(p_value < 0.05, "*", "")))
  
  # Count the number of studies for the afforestation type
  paper_count <- nrow(subset_data)
  
  # Create a label that includes the afforestation type and study count
  label <- paste(aff_type, "(", paper_count, ")", sep = "")
  
  # Extracting the effect size, confidence intervals, significance, stars, paper count, and label
  summary_results <- rbind(summary_results, data.frame(
    AfforestationType = aff_type,
    EffectSize = as.numeric(meta_analysis$b),
    LowerCI = meta_analysis$ci.lb,
    UpperCI = meta_analysis$ci.ub,
    Significant = significant,
    Stars = stars,
    PaperCount = paper_count,
    Label = label
     ))
}

# Define x-axis limits based on the range of effect sizes
x_min <- min(summary_results$LowerCI) - 0.1
x_max <- max(summary_results$UpperCI) + 0.1

# Define the custom order for the y-axis labels
custom_order <- c("Barren", "Crop", "Deforestation", "Grass", "Peat")

# Convert AfforestationType to a factor with the custom order
summary_results$AfforestationType <- factor(summary_results$AfforestationType, levels = custom_order)

# Rebuild the Label column with the custom order
summary_results$Label <- paste(summary_results$AfforestationType, "(", summary_results$PaperCount, ")", sep = "")

# Plotting the weighted effect sizes with confidence intervals, significance stars, and study counts in labels
plot <-ggplot(summary_results, aes(x = EffectSize, y = reorder(Label, AfforestationType), color = Significant)) +
  geom_point(size=10, shape=18) +  
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, size=0.8, color="black") +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") +  
  geom_text(aes(label = Stars, x = UpperCI), hjust = -0.2, vjust = 0.5, size = 7, color = "black") + # Adjusting stars location
  labs(
    x = expression(N[2]*"O-SMD"),
    y = "",
    title = ""
  ) +
  xlim(x_min, x_max) +
  scale_color_manual(values = c("TRUE" = "#48C0AA", "FALSE" = "grey")) +  # Color by significance
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 20, face = "bold", hjust = 1),
    axis.text.x = element_text(size = 18, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.5), 
    panel.border = element_rect(colour = "black", linewidth = 0.5, fill = NA),
    legend.position = "none"
  )
print(plot) 
jpeg("N2O_Forestplot.jpeg",width = 2100, height = 1400, units = "px", res = 300, quality = 100)
print(plot + theme(text = element_text(family = "serif")))
dev.off()
