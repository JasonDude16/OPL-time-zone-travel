library(dplyr)
library(ggplot2)
library(maps)
library(grid)   

df_clean %>% 
  mutate(
    Sex = forcats::fct_drop(Sex),
    Sex = ifelse(Sex == "F", "Female", "Male"),
    Sex = fct_rev(Sex)
  ) %>%
  select(
    Sex,
    TotalKg,
    Best3SquatKg,
    Best3BenchKg,
    Best3DeadliftKg
  ) %>% 
  pivot_longer(cols = -Sex) %>% 
  ggplot(aes(Sex, value, fill = Sex)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~name, scale = "free", nrow = 1) +
  ylab("Weight (kg)") +
  theme_classic() + 
  theme(
    axis.title.x = element_blank(),
    text = element_text(face = "bold", size = 16),
    strip.background = element_rect(fill = "gray90")
  )

df_clean %>% 
  mutate(
    Sex = forcats::fct_drop(Sex),
    Sex = ifelse(Sex == "F", "Female", "Male"),
    Sex = fct_rev(Sex)
  ) %>%
  group_by(Sex) %>% 
  count() %>% 
  ggplot(aes(Sex, n)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.line = element_line(linewidth = 1),
    axis.title.y = element_blank(),
    text = element_text(face = "bold", size = 16),
  )

df_clean %>% 
  mutate(Sex = forcats::fct_drop(Sex)) %>%
  filter(Direction != "Local") %>% 
  group_by(StateMeetTzDiff, Direction) %>% 
  summarise(count = n()) %>% 
  mutate(
    Direction = as.factor(Direction),
    StateMeetTzDiff = as.factor(as.character(StateMeetTzDiff)),
  ) %>% 
  ggplot(aes(StateMeetTzDiff, count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Direction, scales = "free_x") +
  theme_classic() +
  theme(
    text = element_text(size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(color = "white"),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

gridExtra::grid.arrange(
  ncol = 1,
  df_clean %>% 
    mutate(
      Sex = forcats::fct_drop(Sex),
      Sex = ifelse(Sex == "F", "Female", "Male"),
      Sex = fct_rev(Sex)
    ) %>%
    group_by(Sex) %>% 
    count() %>% 
    ggplot(aes(Sex, n)) +
    geom_bar(stat = "identity") +
    ylim(0, 15000) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.line = element_line(linewidth = 1),
      axis.title.y = element_blank(),
      text = element_text(face = "bold", size = 16),
    ),
  df_clean %>% 
    group_by(Name) %>% 
    summarise(n_obs = n()) %>% 
    group_by(n_obs) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(n_obs, count)) +
    geom_bar(stat = "identity") + 
    ylab("Count") +
    xlab("Number of observations per person") +
    scale_x_continuous(
      breaks = 1:7,
      labels = as.character(1:7)
    ) +
    theme_classic() +
    theme(
      axis.line = element_line(linewidth = 1),
      axis.title.y = element_blank(),
      text = element_text(face = "bold", size = 16),
    ),
  df_clean %>% 
    group_by(AgeClass) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(AgeClass, count)) +
    geom_bar(stat = "identity") + 
    xlab("Age Class") +
    ylab("Count") +
    theme_classic() +
    theme(
      axis.line = element_line(linewidth = 1),
      axis.title.y = element_blank(),
      text = element_text(face = "bold", size = 16),
    ),
  df_clean %>% 
    group_by(MeetNumber) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(MeetNumber, count)) +
    geom_bar(stat = "identity") +
    ylab("Count") +
    xlab("Meet Number") +
    scale_x_continuous(
      breaks = 2:8,
      labels = as.character(2:8)
    ) +
    theme_classic() +
    theme(
      axis.line = element_line(linewidth = 1),
      axis.title.y = element_blank(),
      text = element_text(face = "bold", size = 16),
    )
)

# -------------------------------------------------------------------------

# Count people per state
state_counts <- df_clean %>% 
  group_by(MeetState) %>% 
  summarise(n = n()) %>% 
  mutate(state_abbr = MeetState) %>% 
  select(-MeetState)

# Built-in lookup table for state abbreviations -> full names
state_lookup <- data.frame(
  state_abbr = state.abb,
  region = tolower(state.name)
)

# Join counts to state names
state_counts_map <- state_counts %>%
  left_join(state_lookup, by = "state_abbr")

# Get US state polygon data
us_map <- map_data("state")

# Join map polygons to your counts
map_df <- us_map %>%
  left_join(state_counts_map, by = "region")

# Plot
ggplot(map_df, aes(long, lat, group = group, fill = n)) +
  geom_polygon(color = "black", linewidth = 0.2) +
  coord_fixed(1.3) +
  scale_fill_gradient(
    low = "#dbeafe",
    high = "#1d4ed8",
    na.value = "grey95",
    name = "Count"
  ) +
  labs(fill = "Count") +
  theme_void()

# Plot
map_df %>% 
  mutate(is_mo = ifelse(region == "missouri", 1, 0)) %>% 
  ggplot(aes(long, lat, group = group, fill = is_mo)) +
  geom_polygon(color = "white", linewidth = 0.2, show.legend = F) +
  coord_fixed(1.3) +
  labs(fill = "Count") +
  theme_void()

# -------------------------------------------------------------------------

# 1) Count routes
routes <- df_clean %>%
  filter(MeetState != State) %>% 
  filter(!is.na(State), !is.na(MeetState)) %>%
  count(State, MeetState, name = "n")

# 2) State center coordinates
centers <- data.frame(
  abb = state.abb,
  lon = state.center$x,
  lat = state.center$y
)

# 3) Join origin/destination coordinates
routes_map <- routes %>%
  left_join(centers, by = c("State" = "abb")) %>%
  rename(lon_from = lon, lat_from = lat) %>%
  left_join(centers, by = c("MeetState" = "abb")) %>%
  rename(lon_to = lon, lat_to = lat) %>%
  filter(
    !is.na(lon_from), !is.na(lat_from),
    !is.na(lon_to), !is.na(lat_to)
  )

# 4) US map background
us_map <- map_data("state")

# 5) Plot
ggplot() +
  geom_polygon(
    data = us_map,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "white",
    linewidth = 0.2
  ) +
  geom_curve(
    data = routes_map,
    aes(
      x = lon_from, y = lat_from,
      xend = lon_to, yend = lat_to,
      linewidth = n
    ),
    curvature = 0.2,
    alpha = 0.35,
    color = "darkred",
    arrow = arrow(length = unit(0.08, "inches"))
  ) +
  coord_fixed(1.3) +
  scale_linewidth(range = c(0.2, 2.5), name = "Count") +
  theme_void()
