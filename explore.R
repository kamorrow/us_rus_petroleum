


library(tidyverse)
library(janitor)

data <- read.csv("comtrade_2709_2710.csv") %>%
  janitor::clean_names()


data_cl <- data %>%
  select(year, trade_flow, reporter, partner, commodity_code, commodity,
         netweight_kg) %>%
  filter(trade_flow %in% c("Import", "Export")) %>%
  mutate(netweight_MT = netweight_kg / 1000000000) %>%
  select(-netweight_kg)

data_wide <- data_cl %>%
  pivot_wider(names_from = trade_flow,
              values_from = netweight_MT)

cl_2709 <- data_cl %>%
  filter(commodity_code == "2709")

cl_2710 <- data_cl %>%
  filter(commodity_code == "2710")

wide_2709 <- cl_2709 %>%
  pivot_wider(names_from = trade_flow,
              values_from = netweight_MT) %>%
  mutate(Export = 0,
         net_import = Import - Export,
         Export = -Export)
  
wide_2710 <- cl_2710 %>%
  pivot_wider(names_from = trade_flow,
              values_from = netweight_MT) %>%
  mutate(Export = case_when(
    is.na(Export) ~ 0,
    !is.na(Export) ~ Export
  )) %>% 
  mutate(net_import = Import - Export,
         Export = -Export)

ggplot(data = wide_2709) +
  geom_col(aes(x = year,
               y = Import),
           fill = "darkgreen") +
  geom_col(aes(x = year,
               y = Export),
           fill = "darkblue") +
  geom_line(aes(x = year,
                y = net_import)) +
  labs(x = "",
       y = "Net Weight (kg)",
       title = "Trade of HS 2709: Crude Petroleum Oils and Oils Obtained from Bituminous Materials",
       subtitle = "USA and Russia, Reported by USA")

ggplot(data = wide_2710) +
  geom_col(aes(x = year,
               y = Import),
           fill = "darkblue") +
  geom_col(aes(x = year,
               y = Export),
           fill = "red") +
  geom_line(aes(x = year,
                y = net_import)) +
  labs(x = "",
       y = "Net Weight (kg)",
       title = "Trade of HS 2710: Non-Crude Petroleum Oils and Oils Obtained from Bituminous Materials",
       subtitle = "USA and Russia, Reported by USA") +
  theme_minimal()
