
sampleGermData
trtNames <- c("GermWP", "GermTemp")

test <- sampleGermData %>%
  group_by_at(trtNames) %>%
  summarise(
    as_tibble(approx(CumFraction, CumTime, xout = seq(0, 1, by = 0.25), ties = "ordered")),
    .groups = "drop"
    )

sampleGermData %>%
  group_by_at(c("GermWP", "GermTemp")) %>%
  arrange(CumTime) %>%
  summarise(
    {
      df <- approx(CumFraction, CumTime, xout = seq(0, 1, by = 0.25), ties = "ordered", rule = 2)
      names(df) <- c("Frac", "Time")
      df <- as_tibble(df)
      drop_na(df)
    },
    .groups = "drop"
  ) %>%
  mutate(
    Frac = paste0("T", str_pad(Frac * 100, 2, pad = "0")),
    Time = sprintf("%.2f", Time)) %>%
  pivot_wider(
    names_from = "Frac",
    values_from = "Time"
  ) %>%
  mutate(across(everything(), ~ as.character(.x)))
