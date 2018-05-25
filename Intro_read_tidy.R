## ----setup, eval=T, message=FALSE, warning=T, include=FALSE, echo=F------

library(tidyverse)
library(rmarkdown)
library(knitr)
library(xml2)
library(gapminder)

knitr::opts_chunk$set(echo = T, 
                      warning = T, 
                      message = F, 
                      eval = T, 
                      include = T
                      )

## ----data_files----------------------------------------------------------

file_fin_risk <- "FMC_T4_read_file_fin_risk.csv"
file_gdppc <- "FMC_T4_read_file_gdppc.csv"
file_US_corp_spread <- "FMC_T4_read_file_US_corp_spread.csv"




## ----data_read, message=T------------------------------------------------

(fin_risk <- readr::read_csv(file_fin_risk)) #file path



## ----data_read_skip------------------------------------------------------

(US_corp_spread <- readr::read_csv(file_US_corp_spread))

(US_corp_spread_skip <- readr::read_csv(file_US_corp_spread, 
                                        skip = 10)
  )


## ----data_read_alt, message=T--------------------------------------------

(gdppc <- readr::read_csv(file_gdppc)) #which format?

head(fin_risk) #which format?


## ----data_tidy_read------------------------------------------------------

(fin_risk_tidy <- fin_risk %>%
  dplyr::rename(risk_foreign = 
                  `Risk Points for Foreign Debt as a % of GDP`) %>%
  dplyr::rename(risk_exchange = 
                  `Risk Points for Exchange Rate Stability`) %>%
  dplyr::rename(risk_debt = 
                  `Risk Points for Debt Service as a % of XGS`) %>%
  dplyr::rename(risk_CA = 
                  `Risk Points for Current Account as % of XGS`) %>%
  dplyr::rename(risk_liq = 
                  `Risk Points for International Liquidity`) %>%
  dplyr::rename(risk_agg_fin = `Aggregate Financial Risk`)
 )

(fin_risk_tidy_summ <- fin_risk_tidy %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(risk_agg_min = 
                       min(risk_agg_fin, na.rm = T),
                     risk_agg_max = 
                       max(risk_agg_fin, na.rm = T),
                     risk_agg_med = 
                       median(risk_agg_fin, na.rm = T),
                     risk_agg_mean = 
                       mean(risk_agg_fin, na.rm = T),
                     risk_agg_std = 
                       sd(risk_agg_fin, na.rm = T),
                     risk_agg_iqr =
                       IQR(risk_agg_fin, na.rm = T)
                     )
    
)

ggplot(data = filter(fin_risk_tidy, 
                     Country %in% c("United Kingdom",
                                  "United States",
                                  "China",
                                  "India")
                     ),
       mapping = aes(x = Year, 
                     y = risk_agg_fin,
                     color = Country)) +
  geom_point() +
  geom_line(mapping = aes(linetype = Country))



## ----data_gather---------------------------------------------------------

gdppc %>% head(.)

col_yr_1 <- "1960 [YR1960]"
col_yr_end <- "2016 [YR2016]"

(gdppc_tidy <- gdppc %>%
    dplyr::select(-c(`Series Name`, #why backticks ``?
                     `Series Code`,
                     `Country Code`
                     )
                  ) %>%
    dplyr::rename(Country = `Country Name`) %>%
    tidyr::gather(col_yr_1:col_yr_end,
                  key = "Year",
                  value = "GDP_per_capita"
                  ) %>%
    dplyr::arrange(Country)

  )



## ----data_spread---------------------------------------------------------

(fin_risk_foreign <- fin_risk_tidy %>% 
  dplyr::select(c(Country, Year, risk_foreign))
 )

(fin_risk_spread <- fin_risk_foreign %>%
  tidyr::spread(key = Country, 
                value = risk_foreign
                )
  )


## ----data_mutating_joins-------------------------------------------------

gdppc_tidy$Year <- 1960:2016 #reformatting dates

# Left Joining
(data_join_left <- dplyr::left_join(fin_risk_foreign, gdppc_tidy, 
                                   by = c("Country", "Year")
                                   ) %>%
    dplyr::arrange(Country)
  )

# Right Joining
(data_join_right <- dplyr::right_join(fin_risk_foreign, gdppc_tidy, 
                                   by = c("Country", "Year")
                                   ) %>%
    dplyr::arrange(Country)
  )

# Inner Joining
(data_join_inner <- dplyr::inner_join(fin_risk_foreign, gdppc_tidy, 
                                   by = c("Country", "Year")
                                   ) %>%
    dplyr::arrange(Country)
  )

# Full Joining
(data_join_full <- dplyr::full_join(fin_risk_foreign, gdppc_tidy, 
                                   by = c("Year", "Country")
                                   ) %>%
    dplyr::arrange(Country)
  )



