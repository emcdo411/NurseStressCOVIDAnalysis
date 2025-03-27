# README: Nurse Burnout and Vaccine Fear Analysis

**Repository Name Suggestion**: `NurseStressCOVIDAnalysis`

This repository contains a series of data visualizations created in RStudio with assistance from Grok (xAI) to analyze nurse burnout and COVID-19 vaccine fears in the private healthcare sector, focusing on Paris, McKinney, and Plano, Texas. Developed in a short timeframe (March 26, 2025), this project demonstrates rapid prototyping of analytical tools using simulated data and AI-driven coding support.

## Project Overview

Using RStudio and Grok, I explored nurse burnout and vaccine hesitancy during the COVID-19 pandemic (2020–2022) for a friend, an RN in Paris, TX. The project produced three distinct visualizations:
1. **Nurse Burnout Line Plot**: Trends in burnout, vaccine fear, and turnover intent.
2. **Burnout and Vaccine Fear Heatmaps**: Intensity maps with turnover overlays.
3. **Hospital Location Map**: Interactive 2D map with hoverable burnout/vaccine fear percentages.

## Code

### 1. Nurse Burnout Line Plot
Visualizing trends for Paris, TX, and Presby Plano.

```R
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

set.seed(123)
dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month")
n_nurses <- 200
nurse_data <- data.frame(
  Nurse_ID = 1:n_nurses,
  Location = rep(c("Paris, TX", "Presby Plano"), each = n_nurses/2)
) %>%
  crossing(Date = dates) %>%
  mutate(
    Burnout_Score = case_when(
      Date %in% as.Date(c("2020-03-01", "2020-04-01")) ~ rnorm(n(), 50, 10),
      Date %in% as.Date(c("2021-07-01", "2021-08-01")) ~ rnorm(n(), 70, 15),
      Date %in% as.Date(c("2022-01-01", "2022-02-01")) ~ rnorm(n(), 65, 12),
      TRUE ~ rnorm(n(), 40, 8)
    ),
    Vaccine_Fear = case_when(
      Date %in% as.Date(c("2020-12-01", "2021-01-01")) ~ sample(2:4, n(), replace = TRUE),
      Date %in% as.Date(c("2021-07-01", "2021-11-01")) ~ sample(3:5, n(), replace = TRUE),
      Location == "Paris, TX" ~ sample(2:5, n(), replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
      TRUE ~ sample(1:3, n(), replace = TRUE)
    ),
    Intent_to_Leave = case_when(
      Burnout_Score > 70 | Vaccine_Fear > 4 ~ sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      TRUE ~ sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.2, 0.8))
    )
  ) %>%
  mutate(Burnout_Score = pmax(0, pmin(100, Burnout_Score)))

summary_data <- nurse_data %>%
  group_by(Location, Date) %>%
  summarise(
    Avg_Burnout = mean(Burnout_Score),
    Avg_Vaccine_Fear = mean(Vaccine_Fear),
    Prop_Intent_to_Leave = mean(Intent_to_Leave == "Y"),
    .groups = "drop"
  )

p <- ggplot(summary_data, aes(x = Date)) +
  geom_line(aes(y = Avg_Burnout, color = "Burnout Score"), size = 1) +
  geom_line(aes(y = Avg_Vaccine_Fear * 20, color = "Vaccine Fear (scaled)"), size = 1) +
  geom_line(aes(y = Prop_Intent_to_Leave * 100, color = "Intent to Leave (%)"), size = 1) +
  facet_wrap(~Location, ncol = 1) +
  scale_y_continuous(name = "Burnout Score", sec.axis = sec_axis(~./20, name = "Vaccine Fear (1-5) / Intent to Leave (%)")) +
  scale_color_manual(values = c("Burnout Score" = "#1f77b4", "Vaccine Fear (scaled)" = "#ff7f0e", "Intent to Leave (%)" = "#2ca02c")) +
  labs(title = "Nurse Burnout and Vaccine Fears During COVID-19 (2020-2022)", subtitle = "Simulated Data for Paris, TX and Presbyterian Hospital Plano", x = "Date", color = "Metric") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")

print(p)
ggsave("nurse_burnout_vaccine_plot.png", width = 12, height = 8, dpi = 300)
```

### 2. Burnout and Vaccine Fear Heatmaps
Heatmaps with turnover overlays.

```R
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

set.seed(123)
dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month")
n_nurses <- 200
nurse_data <- data.frame(
  Nurse_ID = 1:n_nurses,
  Location = rep(c("Paris, TX", "Presby Plano"), each = n_nurses/2)
) %>%
  crossing(Date = dates) %>%
  mutate(
    Burnout_Score = case_when(
      Date %in% as.Date(c("2020-03-01", "2020-04-01")) ~ rnorm(n(), 50, 10),
      Date %in% as.Date(c("2021-07-01", "2021-08-01")) ~ rnorm(n(), 70, 15),
      Date %in% as.Date(c("2022-01-01", "2022-02-01")) ~ rnorm(n(), 65, 12),
      TRUE ~ rnorm(n(), 40, 8)
    ),
    Vaccine_Fear = case_when(
      Date %in% as.Date(c("2020-12-01", "2021-01-01")) ~ sample(2:4, n(), replace = TRUE),
      Date %in% as.Date(c("2021-07-01", "2021-11-01")) ~ sample(3:5, n(), replace = TRUE),
      Location == "Paris, TX" ~ sample(2:5, n(), replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)),
      TRUE ~ sample(1:3, n(), replace = TRUE)
    ),
    Intent_to_Leave = case_when(
      Burnout_Score > 70 | Vaccine_Fear > 4 ~ sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.7, 0.3)),
      TRUE ~ sample(c("Y", "N"), n(), replace = TRUE, prob = c(0.2, 0.8))
    )
  ) %>%
  mutate(Burnout_Score = pmax(0, pmin(100, Burnout_Score)))

summary_data <- nurse_data %>%
  group_by(Location, Date) %>%
  summarise(
    Avg_Burnout = mean(Burnout_Score),
    Avg_Vaccine_Fear = mean(Vaccine_Fear),
    Prop_Intent_to_Leave = mean(Intent_to_Leave == "Y"),
    .groups = "drop"
  )

p1 <- ggplot(summary_data, aes(x = Date, y = Location)) +
  geom_tile(aes(fill = Avg_Burnout)) +
  geom_line(aes(y = as.numeric(factor(Location)) + Prop_Intent_to_Leave * 0.5 - 0.25, group = Location, color = "Intent to Leave"), size = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = "Burnout Score") +
  scale_color_manual(values = c("Intent to Leave" = "black")) +
  labs(title = "Nurse Burnout and Turnover Risk (2020-2022)", subtitle = "Heatmap of Burnout Scores with Intent to Leave Overlay", x = "Date", y = "Location") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")

p2 <- ggplot(summary_data, aes(x = Date, y = Location)) +
  geom_tile(aes(fill = Avg_Vaccine_Fear)) +
  geom_line(aes(y = as.numeric(factor(Location)) + Prop_Intent_to_Leave * 0.5 - 0.25, group = Location, color = "Intent to Leave"), size = 1) +
  scale_fill_gradient(low = "lightgreen", high = "purple4", name = "Vaccine Fear (1-5)") +
  scale_color_manual(values = c("Intent to Leave" = "black")) +
  labs(title = "Nurse Vaccine Fears and Turnover Risk (2020-2022)", subtitle = "Heatmap of Vaccine Fear with Intent to Leave Overlay", x = "Date", y = "Location") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y")

print(p1)
print(p2)
ggsave("nurse_burnout_heatmap.png", p1, width = 12, height = 6, dpi = 300)
ggsave("nurse_vaccine_heatmap.png", p2, width = 12, height = 6, dpi = 300)
```

### 3. Hospital Location Map
Interactive map with hoverable percentages.

```R
library(leaflet)
library(dplyr)
library(htmltools)

hospitals <- data.frame(
  Hospital = c("Paris Regional Medical Center", "Medical City McKinney", "Baylor Scott & White McKinney", "Medical City Plano", "Texas Health Presbyterian Plano"),
  Lat = c(33.6609, 33.1976, 33.1639, 33.0433, 33.0203),
  Lon = c(-95.5555, -96.6399, -96.6678, -96.6921, -96.7680),
  Burnout_Pct = c(60, 65, 70, 75, 68),
  Vaccine_Fear_Pct = c(40, 30, 25, 20, 28)
)

hospitals <- hospitals %>%
  mutate(Label = paste0(Hospital, "<br>", "Burnout: ", Burnout_Pct, "%<br>", "Vaccine Fear: ", Vaccine_Fear_Pct, "%"))

map <- leaflet(data = hospitals) %>%
  setView(lng = -96.2, lat = 33.35, zoom = 8) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Lon,
    lat = ~Lat,
    radius = 8,
    color = "blue",
    fillOpacity = 0.8,
    popup = ~Label,
    label = ~lapply(Label, HTML)
  ) %>%
  addControl(html = "<b>Nurse Burnout and Vaccine Fear (2020-2022)</b><br>Paris, McKinney, and Plano Hospitals", position = "topright")

print(map)
htmlwidgets::saveWidget(map, "nurse_hospital_map.html")
```

## Dependencies
Install these R packages:
```R
install.packages(c("ggplot2", "dplyr", "tidyr", "lubridate", "leaflet", "htmltools", "htmlwidgets"))
```

## Why This Matters
Nurse burnout and vaccine hesitancy during COVID-19 had profound impacts on healthcare staffing, particularly in private-sector hospitals like those in Paris, McKinney, and Plano, TX. This project matters because:
- **Staffing Insight**: Visualizing burnout and turnover intent helps identify critical periods (e.g., Delta/Omicron waves, mandate rollouts) that strained RNs, informing retention strategies.
- **Regional Context**: Comparing rural (Paris) and urban (Plano/McKinney) trends highlights unique stressors, aiding localized policy decisions.
- **Rapid Analysis**: Using Grok and RStudio, I produced actionable visualizations in hours, demonstrating how AI can accelerate healthcare research for real-world problems like supporting an RN friend.

## Conclusion
In a short session on March 26, 2025, Grok enabled the creation of three robust visualizations from scratch, focusing on nurse burnout and vaccine fears in the private healthcare sector. This repo showcases the power of AI-assisted coding in RStudio for quick, insightful data exploration. Future work could integrate real hospital data to validate these simulated trends.

---

### Changes Made
- **Removed JOLTS Code**: The entire JOLTS section under "Code" is gone.
- **Updated Overview**: Now lists only the three nurse-focused visualizations, removing the JOLTS reference.
- **Revised Why This Matters**: Focuses solely on nurse-related insights, no JOLTS context.
- **Adjusted Conclusion**: Reflects the three visualizations, omitting any pivot from JOLTS.

This README is now fully aligned with your request, emphasizing the nurse burnout and vaccine fear analysis. Copy this into a `README.md` file for your GitHub repo named `NurseStressCOVIDAnalysis`. Let me know if you need further tweaks!
