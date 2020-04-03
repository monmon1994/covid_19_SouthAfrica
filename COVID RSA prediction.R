## COVID-19 prediction data 

# Packages 
library(readxl)
library(tidyverse)
library(stats)
library(scales)

# Data

COVID <- read_excel("COVID19.xls")

rsa <- read_excel("COVID_RSA.xlsx")

# Country DF South Africa

rsa <-  as.data.frame(COVID[COVID$CountryExp=="South Africa",])

rsa %>% 
  select(DateRep, NewConfCases)

sum(rsa$NewConfCases)

# fix case numbers in dataframe

rsa$NewConfCases[]

rsa$NewConfCases[2] <- 14

rsa$NewConfCases[1] <- 23

today <- "2020-03-16"

rbind(rsa, list(as.Date(today, "%Y-%m-%d"),  "South Africa", 1, 0, "ZA", NA, "Non-EU/EEA"))

rsa$DateRep <-  format(rsa$DateRep, "%Y-%m-%d")

rsa$DateRep <-  as.Date(as.POSIXct(rsa$DateRep), "%Y-%m-%d")


# Poisson model

rsa_glm <- glm(data = rsa, NewConfCases~DateRep, family="poisson")

rsa$predicted <- predict(object = rsa_glm, newdata=rsa, type="response")

rsa$resid <- rsa$NewConfCases - rsa$predicted

ggplot(rsa) +
  geom_line(aes(x=DateRep,y=predicted),col="red") +
  geom_point(aes(x=DateRep,y=NewConfCases),col="blue") +
  labs(x="Date",y="Cases") +
  labs(subtitle="(using Poisson Regression)",title="Predicted vs. Actual Number of COVID-19 Cases in RSA") +
  theme(plot.title=element_text(hjust=0.5,face="bold")) +
  theme(axis.text.x = element_text(hjust=1),plot.subtitle = element_text(hjust=0.5)) +
  theme_bw()

# Future values
future <- data.frame(DateRep=seq(as.Date("2020-03-18", "%Y-%m-%d"), as.Date("2020-06-30", "%Y-%m-%d"), by="days"))

future$DateRep <- as.POSIXct(future$DateRep, "%Y-%m-%d")

future$futurepredicted <- predict(object = rsa_glm, newdata=future, type="response")

# Residual testing
ggplot(rsa) +
  geom_smooth(aes(x=DateRep, y=resid),method="lm") +
  geom_point(aes(x=DateRep, y=resid))

# Past and future poisson
futurersa <- dplyr::bind_rows(rsa, future)
futurersa <- futurersa[futurersa$DateRep < "2020-03-18",]
futurersa$DateRep <- as.Date(futurersa$DateRep)

ggplot(futurersa) +
  geom_line(aes(x=DateRep, y=predicted), col="red", size=1) +
  geom_line(aes(x=DateRep, y=futurepredicted),linetype="dashed", col="red", size=1) +
  geom_point(aes(x=DateRep, y=futurepredicted), col="green", size=2) +
  geom_point(aes(x=DateRep, y=NewConfCases), col="black", size=2) +
  labs(x="Date", y="Cases") +
  labs(subtitle="(using poisson regression)",title="Actual and Predicted Number of Daily RSA COVID-19 Cases") +
  theme(plot.title = element_text(hjust=0.5, face="bold", family = "serif")) +
  theme(axis.text.x = element_text(hjust=1, family = "serif"), 
        plot.subtitle = element_text(hjust=0.5, family = "serif")) +
  geom_text(aes(x=DateRep, y=futurepredicted, label=round(futurepredicted,0)), vjust = 1, 
            hjust=-0.3, size = 3,col="magenta3") +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-18")), linetype="dashed") +
  scale_x_date(date_labels="%m-%d", date_breaks="3 days") +
  theme_bw()
ggsave("coronaRSA.png")

# Localized by points model
ggplot(rsa) +
  geom_point(aes(x=DateRep,y=NewConfCases),col="red") +
  geom_smooth(aes(x=DateRep,y=NewConfCases),se=F) +
  theme(axis.text.x = element_text(hjust=1, family = "serif"))




