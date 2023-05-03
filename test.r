
# Germination test ----

sampleGermData


test <- sampleGermData %>%
  group_by_at(trtNames) %>%
  summarise(
    as_tibble(approx(CumFraction, CumTime, xout = seq(0, 1, by = 0.25), ties = "ordered")),
    .groups = "drop"
    )




trts <- c()
trts <- c("GermWP")
trts <- c("GermWP", "GermTemp")


# plot rescaling

sampleGermData %>%
  mutate(across(all_of(trts), ~as.factor(.x))) %>%
  ggplot(aes(
    x = CumTime,
    y = CumFraction,
    group = TrtID,
    color = GermWP,
    shape = GermTemp
  )) +
  geom_line() +
  geom_point(size = 2)


df <- sampleGermData %>%
  mutate(across(all_of(trts), as.factor)) %>%
  group_by(TrtID) %>%
  arrange(TrtID, CumTime, CumFraction) %>%
  mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
  ungroup()

df %>%
  mutate(
    MaxCumFrac = max(CumFraction),
    .by = all_of(trts)) %>%
  arrange(CumTime) %>%
  summarise(
    MaxCumFrac = max(MaxCumFrac),
    FracDiff = sum(FracDiff),
    .by = c(all_of(trts), CumTime)) %>%
  mutate(CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac, .by = all_of(trts))

ggplot(aes(
    x = CumTime,
    y = CumFraction,
    group = TrtID,
    color = GermWP
  )) +
  geom_line() +
  geom_point(size = 2)


# speed table

sampleGermData %>%
  group_by(across(all_of(trts))) %>%
  arrange(CumTime) %>%
  reframe(
    {
      df <- approx(CumFraction, CumTime, xout = seq(0, 1, by = 0.25), ties = "ordered", rule = 2)
      names(df) <- c("Frac", "Time")
      df <- as_tibble(df)
      drop_na(df)
    }
  ) %>%
  mutate(
    Frac = paste0("T", str_pad(Frac * 100, 2, pad = "0")),
    Time = sprintf("%.2f", Time)) %>%
  pivot_wider(
    names_from = "Frac",
    values_from = "Time"
  ) %>%
  mutate(across(everything(), ~ as.character(.x)))

  

# number of levels

sampleGermData %>%
  select(!c("CumTime", "CumFraction")) %>%
  lapply(\(x) length(unique(x))) %>%
  unlist() %>%
  enframe() %>%
  mutate(label = paste0(name, " (n=", value, ")"))
  



# models
GermWP <- sampleGermData$GermWP
GermTemp <- sampleGermData$GermTemp
CumTime <- sampleGermData$CumTime
CumFrac <- sampleGermData$CumFraction

maxCumFrac <- 1

lower <- list(
  HT = 1,
  Tb = 0,
  Psib50 = -5,
  Sigma = 0.0001
)

upper <- list(
  HT = 5000,
  Tb = 15,
  Psib50 = 0,
  Sigma = 10
)

start <- list(
  HT = 800,
  Tb = 1,
  Psib50 = -1,
  Sigma = 0.4
)

HT <- 1000
lower$HT <- upper$HT <- start$HT <- NULL

Tb <- 1
lower$Tb <- upper$Tb <- start$Tb <- NULL

Psib50 <- -1
lower$Psib50 <- upper$Psib50 <- start$Psib50 <- NULL

Sigma <- 0.4
lower$Sigma <- upper$Sigma <- start$Sigma <- NULL

model <- stats::nls(
  CumFrac ~ maxCumFrac * stats::pnorm(
    GermWP - (HT / ((GermTemp - Tb) * CumTime)),
    Psib50,
    Sigma,
    log = FALSE),
  start = start,
  lower = lower,
  upper = upper,
  algorithm = "port")

model <- stats::nls(
  germ ~ maxCumFrac * stats::pnorm(
    wp - (HT / ((temp - Tb) * time)),
    Psib50,
    Sigma,
    log = FALSE),
  algorithm = "port")

model

stats::cor(germ, stats::predict(model)) ^ 2



summary(model)$coefficients %>%
  as_tibble(rownames = "Parameter")



coefs <- as.list(m[,"Estimate"])
coefs$HT

