# This script collects data on transactions that took place on the
# TRON blockchain on 2 July 2021

require(tronr)
require(dplyr)


# ------ Transactions from all blocks generated on 2021-07-02 ------

blocks <- 31570498:31599227 %>% as.character()

r <- list()

# PLEASE NOTE: running the below `for` loop is going to take a LOT of time
# (a few days, in fact). Moreover, the underlying API calls may fail from time
# to time, which will require a manual restart from the point of failure.
# Thus, treat this code rather as an illustration and instead consider
# downloading the ready-to-use dataset directly, e.g.:
# `dat <- readRDS(url("https://chilp.it/eee138c"))`

for (i in 1:length(blocks)) {

  message("\nCollecting data for block ", i, " out of ", length(blocks), "...")

  r[[i]] <- get_block_info(latest = FALSE,
                           block_number = blocks[i])

  message("\nBlock time: ", r[[i]]$timestamp)

  # Be nice to the TRON API and pause periodically:
  if (i %% 50 == 0) {
    sleep <- runif(1, min = 1, max = 3) %>% round(2)
    message("Sleeping now for ", sleep, " seconds\n")
    Sys.sleep(sleep)
    gc() # collect garbage
  }

}

r <- bind_rows(r)

saveRDS(r, file = "data/trx_blocks_tibble_2_jul_2021.rds")


# ------ Transactions that occurred during the regime shift ------

# The article discusses a regime shift in the blockchain activity that
# took place from 03:00 to 03:06 (UTC time zone). The code below collects
# detailed data on transactions between two addresses that were particularly
# active during that period. This code takes a LOT of time to run, so instead
# consider downloading the ready-to-use dataset directly, e.g.:
# `shift_tx <- readRDS(url("https://chilp.it/969333f"))`

tx_shift <- r %>%
  filter(
    timestamp >= as.POSIXct("2021-07-02 03:00:00", tz = "UTC"),
    timestamp < as.POSIXct("2021-07-02 03:06:00", tz = "UTC")
  ) %>%
  select(tx) %>%
  unnest(cols = tx) %>%
  filter(from_address == "THtbMw6byXuiFhsRv1o1BQRtzvube9X1jx" &
           to_address == "TSQyuZowokRp3TMRCcrbsDnhnnDSg7gtMT")

tx_shift_details <- list()

for (i in 1:nrow(tx_shift)) {

  message("Processing record ", i, "/", nrow(tx_shift))

  tx_shift_details[[i]] <- get_tx_info_by_id(tx_id = tx_shift$tx_id[i],
                                             add_contract_data = FALSE)

}

tx_shift_details <- bind_rows(tx_shift_details)

saveRDS(tx_shift_details, file = "data/tx_shift_details.rds")
