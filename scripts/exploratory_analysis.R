# Exploratory analysis of the transaction data collected from the
# TRON blockchain on 2 July 2021

require(dplyr)
require(ggplot2)
require(ggnetwork)
require(igraph)
require(patchwork)
require(purrr)
require(tidyr)


# ------ Download data ------

dat <- readRDS(url("https://chilp.it/eee138c"))

# Check dimensions and names:
dat %>% dim()
names(dat)


# ------ Time between blocks ------

dat$timestamp %>% diff() %>% mean()


# ------ Number of block creators (witnesses) ------

dat$witness_address %>% unique() %>% length()


# ------ Number of transactions per witness ------

dat$witness_address %>% unique() %>% length()
dat$witness_address %>% table() %>% mean()


# ------ Number of transactions per block ------

dat$tx_count %>% summary()

# Figure 1:
dat %>%
  ggplot(aes(tx_count)) +
  geom_histogram(bins = 50, col = "gray50") +
  theme_minimal()


# ------ Distribution of the transaction types ------

dat %>%
  select(tx) %>%
  unnest(cols = c(tx)) %>%
  group_by(contract_type) %>%
  summarise(n = n()) %>%
  mutate(percent = round(n / sum(n) * 100, 3)) %>%
  arrange(-n)


# ------ Block size vs number of transactions ------

# Figure 2:
dat %>%
  ggplot(aes(tx_count, size/1e6)) + geom_point(alpha = 0.1) +
  theme_minimal() +
  labs(x = "Number of transactions per block",
       y = "Block size (Mb)")


# ------ Activity on the blockchain ------

# Figure 3:
dat %>%
  ggplot(aes(timestamp, tx_count)) + geom_point(alpha = 0.1) +
  geom_smooth(col = "red") +
  theme_minimal()

# Figure 4:
dat %>%
  filter(timestamp >= as.POSIXct("2021-07-02 02:55:00", tz = "UTC"),
         timestamp <= as.POSIXct("2021-07-02 03:11:00", tz = "UTC")) %>%
  ggplot(aes(timestamp, tx_count)) +
  geom_vline(
    xintercept = as.POSIXct(
      c("2021-07-02 03:00:00", "2021-07-02 03:06:00"),
      tz = "UTC"
    ),
    col = "red", linetype = 2
  ) +
  annotate(
    "text",
    x = as.POSIXct(
      c("2021-07-02 02:57:00",
        "2021-07-02 03:03:00",
        "2021-07-02 03:08:30"),
      tz = "UTC"
    ),
    y = 1000, col = "red",
    label = c('before shift', 'shift', 'after shift')
  ) +
  geom_point(alpha = 0.4) +
  theme_minimal()

# Addresses that interacted between 03:00 and 03:06,
# ordered by the number of transactions:
dat %>%
  filter(
    timestamp >= as.POSIXct("2021-07-02 03:00:00", tz = "UTC"),
    timestamp < as.POSIXct("2021-07-02 03:06:00", tz = "UTC")
  ) %>%
  select(tx) %>%
  unnest(cols = tx) %>%
  group_by(from_address, to_address) %>%
  count() %>% arrange(-n) %>% head()

# Addresses that interacted between 03:06 and 03:11,
# ordered by the number of transactions:
dat %>%
  filter(
    timestamp >= as.POSIXct("2021-07-02 03:06:00", tz = "UTC"),
    timestamp <= as.POSIXct("2021-07-02 03:11:00", tz = "UTC")
  ) %>%
  select(tx) %>%
  unnest(cols = tx) %>%
  group_by(from_address, to_address) %>%
  count() %>% arrange(-n) %>% head()

# Addresses that interacted between 02:55 and 03:00,
# ordered by the number of transactions:
dat %>%
  filter(
    timestamp >= as.POSIXct("2021-07-02 02:55:00", tz = "UTC"),
    timestamp < as.POSIXct("2021-07-02 03:00:00", tz = "UTC")
  ) %>%
  select(tx) %>%
  unnest(cols = tx) %>%
  group_by(from_address, to_address) %>%
  count() %>% arrange(-n) %>% head()

# Download data on transactions between `THtbMw6byXuiFhsRv1o1BQRtzvube9X1jx` (A)
# and `TSQyuZowokRp3TMRCcrbsDnhnnDSg7gtMT` (B) that took place during the
# regime shift:
shift_tx <- readRDS(url("https://chilp.it/969333f"))

# Check dimensions and names:
shift_tx %>% dim()
names(shift_tx)

# Type of transactions between addresses A and B:
shift_tx$contract_type %>% unique()

# Amount of assets transferred as part of the "main" transactions:
shift_tx$trx_transfer %>% sum()
shift_tx$trc10_transfer %>% sum(na.rm = TRUE)
shift_tx$trc20_transfer %>% sum(na.rm = TRUE)

# Amount of assets transferred due to internal transactions:
shift_tx %>%
  select(internal_tx) %>%
  unnest(internal_tx) %>%
  filter(amount > 0) %>%
  group_by(token_id) %>%
  summarise(total_amount = sum(amount))


# ------ Transaction network analysis ------

# Nodes and edges in the network from the 10th block:
nb <- dat$tx[[10]] %>%
  group_by(from_address, to_address) %>%
  summarise(weight = n()) %>%
  ungroup() %>%
  arrange(-weight)

nb

# Convert to an `igraph` object:
nb_graph <- nb %>% graph_from_data_frame(., directed = TRUE)
summary(nb_graph)

# Figure 5:
ggplot(fortify(nb_graph, arrow.gap = 0.007),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(
    color = "#57cbcc",
    arrow = arrow(length = unit(3, "pt"), type = "closed")
  ) +
  geom_nodes(color = "#343a40") +
  theme_blank()

# Convert all per-block transaction networks to `igraph` objects:
nw_stats <- dat %>%
  select(timestamp, tx) %>%
  group_by(timestamp) %>%
  unnest(tx) %>%
  select(-c(tx_id, contract_type)) %>%
  mutate(to_address = if_else(
    is.na(to_address), from_address, to_address)
  ) %>%
  group_by(timestamp, from_address, to_address) %>%
  summarise(weight = n()) %>%
  group_by(timestamp) %>%
  nest(nw = c(from_address, to_address, weight)) %>%
  mutate(nw = map(nw, graph_from_data_frame))

# Function to calculate quantitative network metrics:
get_nw_stats <- function(g) {
  tibble(
    n_nodes = gorder(g),
    n_edges = gsize(g),
    diameter = diameter(g, directed = TRUE),
    max_in_degree = degree(g, mode = "in",
                           loops = FALSE) %>% max(),
    max_out_degree = degree(g, mode = "out",
                            loops = FALSE) %>% max(),
    avg_degree = n_edges / n_nodes,
    edge_density = edge_density(g, loops = FALSE),
    assortativity = assortativity_degree(g)
  )
}

# Calculate network metrics for each block:
nw_stats <- nw_stats %>%
  mutate(stats = map(nw, get_nw_stats))

glimpse(nw_stats$stats[[1]])

# Unnest `nw_stats` for easier plotting:
nw_stats_flat <- nw_stats %>%
  ungroup() %>%
  select(timestamp, stats) %>%
  unnest(cols = stats)

glimpse(nw_stats_flat)

# Figure 6:
p1 <- nw_stats_flat %>%
  ggplot(aes(timestamp, n_nodes)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") + theme_minimal()

p2 <- nw_stats_flat %>%
  ggplot(aes(timestamp, n_edges)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") + theme_minimal()

p3 <- nw_stats_flat %>%
  ggplot(aes(n_nodes, n_edges)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") + theme_minimal()

(p1 + p2) / p3

# Figure 7:
nw_stats_flat %>%
  select(timestamp, max_in_degree, max_out_degree) %>%
  pivot_longer(cols = c(max_in_degree, max_out_degree),
               names_to = "metric", values_to = "degree") %>%
  ggplot(aes(timestamp, degree)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") +
  facet_wrap(~metric) +
  scale_y_log10() + theme_minimal()

# Figure 8:
p1 <- nw_stats_flat %>%
  ggplot(aes(timestamp, edge_density)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") + scale_y_log10() +
  theme_minimal()

p2 <- nw_stats_flat %>%
  ggplot(aes(n_nodes, edge_density)) +
  geom_point(alpha = 0.1) +
  scale_x_log10() + scale_y_log10() +
  theme_minimal()

p3 <- nw_stats_flat %>%
  ggplot(aes(n_edges, edge_density)) +
  geom_point(alpha = 0.1) +
  scale_x_log10() + scale_y_log10() +
  theme_minimal()

p4 <- nw_stats_flat %>%
  ggplot(aes(max_in_degree, edge_density)) +
  geom_point(alpha = 0.1) +
  scale_x_log10() + scale_y_log10() +
  theme_minimal()

p1 / (p2 + p3 + p4)

# Figure 9:
nw_stats_flat %>%
  ggplot(aes(timestamp, avg_degree)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") + theme_minimal()

# Figure 10:
nw_stats_flat %>%
  ggplot(aes(timestamp, diameter)) +
  geom_point(alpha = 0.1) +
  scale_y_log10() +
  theme_minimal() +
  scale_x_datetime(breaks = scales::pretty_breaks(n = 10))

# Figure 11:
nw_stats_flat %>%
  ggplot(aes(timestamp, assortativity)) +
  geom_point(alpha = 0.1) +
  geom_smooth(col = "red") + theme_minimal()

# Figure 12:
assort_times <- nw_stats_flat %>%
  filter(assortativity == min(assortativity, na.rm = TRUE) |
           assortativity == max(assortativity, na.rm = TRUE)) %>%
  pull(timestamp)

min_assort_nw <- nw_stats %>%
  ungroup() %>%
  filter(timestamp == min(assort_times)) %>%
  select(nw) %>%
  .[[1]] %>% .[[1]]

max_assort_nw <- nw_stats %>%
  ungroup() %>%
  filter(timestamp == max(assort_times)) %>%
  select(nw) %>%
  .[[1]] %>% .[[1]]

p1 <- ggplot(fortify(min_assort_nw, arrow.gap = 0.007),
             aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(
    color = "#57cbcc",
    arrow = arrow(length = unit(3, "pt"), type = "closed")
  ) +
  geom_nodes(color = "#343a40") +
  theme_blank() + ggtitle("Assortativity: -0.53") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(fortify(max_assort_nw, arrow.gap = 0.007),
             aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(
    color = "#57cbcc",
    arrow = arrow(length = unit(3, "pt"), type = "closed")
  ) +
  geom_nodes(color = "#343a40") +
  theme_blank() + ggtitle("Assortativity: 0.79") +
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2
