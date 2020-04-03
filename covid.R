remotes::install_github("GuangchuangYu/nCov2019")

 # functions
get_nCov2019() to query online latest information
load_nCov2019() to get historical data
nCov2019_set_country() to set country options
summary and [ to access data
plot to present data on map
dashboard() to open Shiny app dashboard

library('nCov2019')

df <- get_nCov2019(lang = 'en')

head(summary(df))

head(df['global'])

d <- summary(df)

library(ggplot2)
library('tidyr')
library('ggrepel')
library('ggplot2')

source("example.R")

ggplot(d, aes(as.Date(date, "%m.%d"), as.numeric(confirm))) +
    geom_col(fill = 'firebrick') + 
    theme_minimal(base_size = 14) +
    xlab(NULL) + ylab(NULL) + 
    scale_x_date(date_labels = "%Y/%m/%d") +
    labs(caption = paste("accessed date:", time(df)))

# Historical data   
df <- load_nCov2019(lang = 'en')
china <- subset(df['global'], country == 'China')
china <- gather(china, curve, count, -time, -country)

# China plot for practice
ggplot(china, aes(x = time, y = count, colour = curve)) +
    geom_point() +
    geom_line() +
    xlab(NULL) + ylab(NULL) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_text_repel(aes(label = curve), 
                    data = china[china$time == time(df), ], hjust = 1) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", 
                 limits = c(as.Date("2020-01-15"), as.Date("2020-03-20"))) +
    labs(title = "Number of deaths, confirmed and cured in China")

# South Africa
RSA <- subset(df['global'], country == 'South Africa')
RSA <- gather(RSA, curve, count, -time, -country)

ggplot(RSA, aes(x = time, y = count, colour = curve)) +
    geom_point() +
    geom_line() +
    xlab(NULL) + ylab(NULL) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_text_repel(aes(label = curve),
                    data = RSA[RSA$time == time(df), ], hjust = 1) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", 
                 limits = c(as.Date("2020-03-05"), as.Date("2020-03-30"))) +
    labs(title = "Number of deaths, confirmed and cured in South Africa")

    
# italy

ITA <- subset(df["global"], country == "Italy")
ITA <- gather(ITA, curve, count, -time, -country)


ggplot(ITA, aes(x = time, y = count, colour = curve)) +
    geom_point() +
    geom_line() +
    xlab(NULL) + ylab(NULL) +
    theme_classic() +
    theme(legend.position = "none") +
    geom_text_repel(aes(label = curve),
                    data = ITA[ITA$time == time(df), ], hjust = 1) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
    scale_x_date(date_labels = "%Y-%m-%d", 
                 limits = c(as.Date("2020-02-06"), as.Date("2020-03-30"))) +
    labs(title = "Number of deaths, confirmed and cured in Italy")



# Country comparisons

library(extrafont)
loadfonts(device = "win")

df <- load_nCov2019()

df_sa <- df['global']

df_sa <- data.frame(df_sa)

df_sa %>% 
    as_tibble %>% 
    rename(confirm=cum_confirm) %>%
    filter(confirm > 100) %>% 
    group_by(country)



df_g <- df['global'] %>% 
    as_tibble %>%
    rename(confirm=cum_confirm) %>%
    filter(confirm > 100 & country %in% c("Italy", "France", "Germany", "South Africa", "Japan", 
                                          "South Korea", "United States", "Spain", "Australia", "Russia")) %>%
    group_by(country) %>%
    mutate(days_since_100 = as.numeric(time - min(time))) %>%
    ungroup 


breaks=c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)


ggplot(df_g, aes(days_since_100, confirm, color = country)) +
    geom_line(size = 0.8) +
    geom_point(pch = 21, size = 1) +
    geom_text(data = data %>% filter(days_since_100 == last(days_since_100)), aes(label = country, 
                                                                 x = days_since_100 + 0.5, 
                                                                 y = confirm)) +
    scale_y_log10(expand = expansion(add = c(0,0.1)), 
                  breaks = breaks, labels = breaks) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme_minimal(base_size = 14) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(3,20,3,3,"mm"),
        text = element_text(family = "serif"),
        axis.text = element_text(family = "serif")) +
    coord_cartesian(clip = "off") +
    labs(x = "Number of days since 100th case", y = NULL, 
         title = "Top African countries COVID-19 confirmed cases")




#geom_smooth(method='glm', aes(group=1), 
#            color='grey10', linetype='dashed') +

"Italy"


# Africa and the World 

df_africa <- df['global'] %>% 
    as_tibble %>%
    rename(confirm=cum_confirm) %>%
    filter(confirm > 50 & country %in% c("South Africa", "Algeria","Burhina Faso", "Cameroon", 'Democratic Republic of the Congo', "Egypt",
                                          "Ghana", "Morocco", "Mauritius", "Nigeria", "Senegal", "Tunisia", "Côte d'Ivoire")) %>%
    group_by(country) %>%
    mutate(label = if_else(confirm == max(confirm), as.character(country), NA_character_)) %>%
    mutate(days_since_100 = as.numeric(time - min(time))) %>%
    ungroup 

ggplot(df_africa, aes(days_since_100, confirm, color = country, group = country)) +
    geom_line(size = 1) +
    geom_point(pch = 21, size = 1) +
    geom_label_repel(aes(label = label),
                     nudge_x = 2, segment.size = 0.5) +
    scale_y_log10(expand = expansion(add = c(0,0.1)), 
                  breaks = breaks, labels = breaks) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme_classic(base_size = 14) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(3,20,3,3,"mm"),
        text = element_text(family = "serif")) +
    coord_cartesian(clip = "off") +
    labs(x = "Number of days since 50th case", y = NULL, 
         title = "Confirmed COVID-19 cases in Africa")


#    geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, 
#data = . %>% group_by(country) %>% top_n(1, days_since_100), 
#bg.color = "white")

# ONLY AFRICA
library(countrycode)
df_sa$iso3c <- countrycode(df_sa$country, origin = 'country.name', destination = "iso3c")


df_africa <- df_sa %>% 
    as_tibble %>%
    rename(confirm=cum_confirm) %>%
    filter(iso3c %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                        "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                        "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                        "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                        "TGO", "TUN", "UGA", "ZMB", "ZWE")) %>%
    group_by(country) %>%
    mutate(days_since_100 = as.numeric(time - min(time))) %>%
    ungroup 
















