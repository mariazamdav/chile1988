
library(dplyr)
library(haven)
library(sf)
library(dbplyr)
library(ggplot2)
library(tidyverse)

# Getting people data

Personas_Censo1992 <- read_sav("~/Files/MIT/spring 26/quant 2/chile1988/Censo_1992/SPSS CATEGORIA 3/PERSONA.sav")

# Only observations from people who lived in Chile in 1987

Comunas_Censo_1987 <-Personas_Censo1992 %>%
  filter(Comuna_1987_Origen3 >= 1101 & Comuna_1987_Origen3 <= 13605,
         Comuna_1987_Origen3 %% 100 != 0)

# Comuna codes from 1992 census 

Comunas_Censo1992 <- read_sav("~/Files/MIT/spring 26/quant 2/chile1988/Censo_1992/SPSS CATEGORIA 3/COMUNAS.sav")

# Votes data 

Dataverse <- read_dta("~/Files/MIT/spring 26/quant 2/chile1988/dataverse_files (3)/Paper Replication/Data/FinalDatasetForReplication.dta")

colnames(Comunas_Censo_1987)

summary(Comunas_Censo_1987)

library(data.table)

# Convert your existing dataframe to data.table
dt <- as.data.table(Comunas_Censo_1987)

# Select and summarize
dt[, .(Gender = mean(Sexo, na.rm = TRUE),
       Marit   = sd(var2, na.rm = TRUE),
       count())]

dt[, .N, by = Ocupacion]
dt[Ocupacion == "110", .N]

# Filter to security force workers using the occupation code
# Replace 'OFICIO' and '110' with your actual variable/code
security <- Comunas_Censo_1987 %>%
  filter(Ocupacion == 110)

# Look at the education distribution
# Replace 'NIVEL' with your actual education variable
security %>%
  count(Tipo_educacion) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = 30)

# Visualize it
ggplot(security, aes(x = factor(Tipo_educacion))) +
  geom_bar() +
  labs(x = "Education level", y = "Count",
       title = "Education distribution among security force workers")

colnames(Comunas_Censo_1987)
unique(Comunas_Censo_1987$Tipo_educacion)

security <- security %>%
  mutate(rank_proxy = ifelse(Tipo_educacion >= 12, "officer", "enlisted"))

security %>%
  count(rank_proxy) %>%
  mutate(pct = n / sum(n) * 100)

security %>%
  group_by(Comuna_1987_Origen3, rank_proxy) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = rank_proxy, values_from = n, values_fill = 0) %>%
  summary()

colnames(Comunas_Censo_1987)
# Total population per comuna
comuna_pop <- Comunas_Censo_1987 %>%
  count(Comuna_1987_Origen3, name = "total_pop")

# Security force count per comuna
security_share <- security %>%
  count(Comuna_1987_Origen3, name = "security_n") %>%
  left_join(comuna_pop, by = "Comuna_1987_Origen3") %>%
  mutate(security_pct = security_n / total_pop * 100)

summary(security_share$security_pct)

# Visualize the distribution
ggplot(security_share, aes(x = security_pct)) +
  geom_histogram(bins = 50) +
  labs(x = "Security force workers as % of comuna population",
       y = "Count of comunas",
       title = "Distribution of security force share across comunas")

# Anyone working in public administration sector
state_sector <- Comunas_Censo_1987 %>%
  filter(Rama_actividad %in% c(750, 751, 752, 753))

nrow(state_sector)

state_sector$Rama_actividad

colnames(state_sector)

# Compare to your security-only sample
security_only <- Comunas_Censo_1987 %>%
  filter(Ocupacion == 110)

nrow(security_only)

Comunas_Censo_1987 %>%
  filter(Ocupacion == 110) %>%
  count(Rama_actividad) %>%
  arrange(desc(n))

# Break down the full state sector by education and comuna
state_sector %>%
  mutate(rank_proxy = ifelse(Tipo_educacion >= 12, "officer", "enlisted")) %>%
  group_by(Comuna_1987_Origen3, rank_proxy) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = rank_proxy, values_from = n, values_fill = 0) %>%
  summary()

unique(Comunas_Censo_1987$Quintil_por_puntaje)



colnames(Dataverse)

# Top 5 comunas by VoteShareNo
Dataverse |>
  arrange(desc(VoteShareNo)) |>
  select(comuna, VoteShareNo) |>
  head(5)

# Distribution of state sector observations per comuna
state_sector |>
  count(Comuna_1987_Origen3) |>
  ggplot(aes(x = n)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    x = "Number of state sector workers",
    y = "Number of comunas",
    title = "Distribution of state sector observations per comuna"
  ) +
  theme_minimal()

# VoteShareNo vs lnDistStgo
ggplot(Dataverse, aes(x = VoteShareNo, y = lnDistStgo)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    x = "Vote Share No",
    y = "ln(Distance to Santiago)",
    title = "Distance to Santiago vs Vote Share 'No' by Comuna"
  ) +
  theme_minimal()

# VoteShareNo per comuna
ggplot(Dataverse, aes(x = reorder(as.factor(comuna), VoteShareNo), y = VoteShareNo)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Comuna",
    y = "Vote Share No",
    title = "Vote Share 'No' by Comuna (1988 Chilean Plebiscite)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# Top 5 comunas by VoteShareNo
Dataverse |>
  arrange(desc(VoteShareNo)) |>
  select(comuna, VoteShareNo) |>
  head(5)

# Distribution of state sector observations per comuna (excluding zeros)
Comunas_Censo_1987 |>
  count(Comuna_1987_Origen3, name = "total") |>
  left_join(
    state_sector |> count(Comuna_1987_Origen3, name = "state_n"),
    by = "Comuna_1987_Origen3"
  ) |>
  mutate(state_n = replace_na(state_n, 0)) |>
  filter(state_n > 0) |>
  ggplot(aes(x = state_n)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    x = "Number of state sector workers",
    y = "Number of counties",
    title = "Distribution of state sector workers per county (counties with >0 workers)"
  ) +
  geom_vline(aes(xintercept = 25, linetype = "Threshold (25 workers)"), color = "red") +
  scale_linetype_manual(name = NULL, values = c("Threshold (25 workers)" = "dashed")) +
  theme_minimal()

ggsave("ggplot2_bar_chart_14.png",
       width = 4.5, 
       height = 3, 
       units = "in", 
       dpi = 300)

# VoteShareBuchi89 vs VoteShareNo per comuna
ggplot(Dataverse, aes(x = VoteShareNo, y = share_buchi89)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    x = "Vote Share No",
    y = "Vote Share Presidential Candidate 1989",
    title = "Vote Share No vs Vote Share Pinochet Proxy 1989 by County"
  ) +
  theme_minimal()

# VoteShareNo vs sh_women_70
ggplot(Dataverse, aes(x = sh_women_70, y = VoteShareNo)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    x = "Share of Women (1970)",
    y = "Vote Share No",
    title = "Vote Share No vs Share of Women (1970) by Comuna"
  ) +
  theme_minimal()

# VoteShareNo vs sh_women_70
ggplot(Dataverse, aes(x = sh_women_70, y = VoteShareNo)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    x = "Share of Women (1970)",
    y = "Vote Share No",
    title = "Vote Share No vs Share of Women (1970) by Comuna"
  ) +
  theme_minimal()


# Censo

colnames(Comunas_Censo_1987)

# 1. Proportion of Estado_civil per comuna
estado_labels <- c("1" = "Married", "2" = "Living together", "3" = "Single",
                   "4" = "Widowed", "5" = "Separate", "6" = "Annulled")

Comunas_Censo_1987 |>
  filter(!is.na(Estado_civil)) |>
  count(Comuna_1987_Origen3, Estado_civil) |>
  filter(n > 0) |>
  group_by(Comuna_1987_Origen3) |>
  mutate(
    prop = n / sum(n),
    Estado_civil = recode(as.character(Estado_civil), !!!estado_labels)
  ) |>
  ggplot(aes(x = factor(Comuna_1987_Origen3), y = prop, fill = Estado_civil)) +
  geom_col() +
  labs(
    x = "Comuna", y = "Proportion",
    fill = "Estado Civil Actual",
    title = "Proportion of Estado Civil Actual per Comuna"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# 2. Proportion of Sexo per comuna
Comunas_Censo_1987 |>
  count(Comuna_1987_Origen3, Sexo) |>
  group_by(Comuna_1987_Origen3) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = factor(Comuna_1987_Origen3), y = prop, fill = factor(Sexo))) +
  geom_col() +
  labs(
    x = "Comuna", y = "Proportion",
    fill = "Sexo",
    title = "Proportion of Sexo per Comuna"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



### april 7th 

head(sort(unique(Comunas_Censo_1987$Comuna_1987_Origen3)))
head(sort(unique(Dataverse$comuna)))

