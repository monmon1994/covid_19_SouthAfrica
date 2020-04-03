# visualize the spread of the virus in relation to governmental intervention measures.

remotes::install_github("joachim-gassen/tidycovid19")


library(tidycovid19)
library(tidyverse)

data <-  download_merged_data(wbank_vars = c("SP.POP.TOTL", "AG.LND.TOTL.K2",
                                             "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD"),
                              wbank_labels = c("population", "land_area_skm", "pop_density",
                                               "pop_largest_city", "life_expectancy", "gdp_capita"),
                              search_term = "coronavirus", silent = FALSE, cached = T)

plot_covid19_spread(data, highlight = c("ITA", "ESP", "FRA", "DEU", "USA", "ZAF"), 
                    intervention = "lockdown")

plot_covid19_spread(
  type = "confirmed", min_cases = 100, min_by_ctry_obs = 7,
  edate_cutoff = 60, per_capita = FALSE,
  highlight = c("BEL", "CHN", "FRA", "DEU", "IRN", "ITA", "KOR",
                "NLD", "ESP", "CHE", "GBR", "USA", "ZAF"),
  intervention = "lockdown"
)

data_1 <- data %>% 
  filter(country %in% c("Italy", "France", "Germany", "South Africa", "Japan", 
                        "Korea, South", "US", "Spain", "Australia", "Russia", 
                        "United Kingdom", "Iran")) %>%
  group_by(country) %>%
  mutate(days_since_100 = as.numeric(date - min(date))) %>%
  ungroup 

ggplot(data = data_1) +
  geom_line(aes(x = days_since_100, y = confirmed, color = country)) +
  geom_path(aes(x = lockdown, y = confirmed))+
  theme_classic() +
  theme(legend.position = "none", axis.text = element_text(family = "serif"), 
        text = element_text(family = "serif")) +
  ggtitle("Confirmed COVID-19 cases and Intervention")
  

ggplot(data = data_1) +
  geom_point(aes(x = date, y = confirmed, color = country, size = soc_dist), alpha = 0.5) +
  scale_y_log10() +
  theme_bw()
  



# Testing vs Confirmed

paste("abc", 100000)
options("scipen"=10)    # set high penalty for scientific display
paste("abc", 100000)


library(readr)
library(readxl)

test <- read_excel("tests-vs-confirmed-cases-covid-19.xlsx")

colnames(test)[4] <- "total_tests"
colnames(test)[5] <- "confirmed"
colnames(test)[1] <- "country"
colnames(test)[2] <- "iso3c"


#test$Date <- as.character(gsub(",", "", test$Date)) # remove commas

#test$Date <- as.Date(test$Date, format = "%b%d%Y") # change to date format

#test <- merge(test, test_new, by.x = "Date", by.y = "country", no.dups = T)

test$total_tests <- as.numeric(test$total_tests)

test_1 <- test %>% 
  filter(country %in% c("Italy", "France", "Germany", "South Africa", "Japan", 
                                        "South Korea", "United States", "Spain", "Australia", "Russia", 
                                        "United Kingdom")) %>%
  group_by(country) %>%
  mutate(days_since_100 = as.numeric(Date - min(Date))) %>%
  ungroup 

breaks=c(10, 50, 100, 1000, 2000, 5000, 10000, 20000, 50000, 750000, 1000000)

ggplot(data = test_1) +
  geom_point(aes(x = log(confirmed), y = log(total_tests), color = country, size = 3), alpha = 0.5) +
  geom_text(aes(x = log(confirmed), y = log(total_tests), label=country),hjust=0.5, vjust=-2, size = 2.8) +
  theme_classic() +
  theme(legend.position = "none", axis.text = element_text(family = "serif"), 
        text = element_text(family = "serif")) +
  ggtitle("Total Tests vs Confirmed COVID19 Cases") 
  

scale_y_log10(expand = expansion(add = c(0,0.1)), 
            breaks = breaks, labels = breaks) +
scale_x_log10(expand = expansion(add = c(0,0.1)), 
            breaks = breaks, labels = breaks) +
  
  
  
# Newer testing data 

test_new <- read_excel("testing for COVID19.xlsx")

test_new <- select(test_new, -source, -...5)

date <- c("2020-04-01", "2020-04-01", "2020-04-01", "2020-03-24", "2020-04-01", "2020-04-01")

test_new$date <- date
test_new$date <- as.Date(test_new$date, "%Y-%m-%d")


