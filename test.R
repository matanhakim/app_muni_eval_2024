library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(forcats)
library(ggridges)

sheet_url <- "https://docs.google.com/spreadsheets/d/1iyLtDl6BWISbKic_nHtakDXeZNgzqkm3Vw7cE4ewpn0/edit"

df <- read_sheet(sheet_url) |> 
  mutate(id = 1:n()) |> 
  pivot_longer(!c(`חותמת זמן`, id), names_to = c("question", "city"), names_sep = " \\[", values_to = "value") |> 
  mutate(
    question = str_sub(question, 4),
    city = factor(str_sub(city, end = -2))
  )

df |> 
  mutate(city = fct_reorder(city, value, .desc = TRUE)) |>
  mutate(
    .by = c(city),
    mean_value = mean(value)
  ) |> 
  ggplot(aes(value, city, group = city, fill = mean_value)) + 
  geom_violin() +
  stat_summary(fun = "mean", geom = "line", color = "black", mapping = aes(group = 1)) +
  theme_minimal()

df |> 
  mutate(city = fct_reorder(city, value, .fun = mean)) |>
  mutate(
    .by = c(city),
    mean_value = mean(value)
  ) |> 
  ggplot(aes(value, city, fill = mean_value)) + 
  geom_density_ridges_gradient(
    quantile_lines = TRUE, quantile_fun = mean
  ) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 3) +
  labs(
    title = "התפלגות מדד משוקלל לפי עיר",
    x = "ערך מדד",
    y = "",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

df |> 
  mutate(city = fct_reorder(city, value, .fun = mean)) |>
  mutate(
    .by = c(city, question),
    mean_value = mean(value)
  ) |> 
  ggplot(aes(value, city, fill = mean_value)) + 
  geom_density_ridges_gradient(
    quantile_lines = TRUE, quantile_fun = mean
  ) + 
  facet_wrap(~question, ncol = 1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 3) +
  labs(
    title = "התפלגות וממוצע מדד לפי עיר",
    x = "ערך מדד",
    y = "",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

df |> 
  mutate(city = fct_reorder(city, value, .desc = TRUE)) |>
  mutate(
    .by = c(city, question),
    mean_value = mean(value)
  ) |> 
  ggplot(aes(1, value, group = question, fill = mean_value)) + 
  geom_violin() +
  geom_hline(aes(yintercept = mean_value)) +
  facet_grid(city ~ question, labeller = labeller(question = label_wrap_gen(width = 25))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 3) +
  theme_minimal() + 
  theme(
    axis.text.x = element_blank()
    # strip.text.y = element_text(angle = 0)
  )
