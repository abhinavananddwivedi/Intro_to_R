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




## ----load_gapminder------------------------------------------------------

(data_gapminder <- gapminder::gapminder)

#what is the format: wide or long?
#what is a tibble?

## ----filter_rows---------------------------------------------------------

(data_1952 <- data_gapminder %>%
  dplyr::filter(year == 1952) #extract the rows for year 1952
 ) #note == as opposed to =

(data_2007 <- data_gapminder %>%
  dplyr::filter(year == 2007) #extract the rows for year 2007
 )


## ----select_columns------------------------------------------------------

(data_1952_gdppc <- data_1952 %>%
  dplyr::select(country, year, gdpPercap)
 )
(data_2007_gdppc <- data_2007 %>%
  dplyr::select(country, year, gdpPercap)
 )
(data_1952_life_exp <- data_1952 %>%
  dplyr::select(country, year, lifeExp)
 )
(data_2007_life_exp <- data_2007 %>%
  dplyr::select(country, year, lifeExp)
 )

# = dplyr::select(-c(continent, pop, gdpPercap))

## ----arrange_gdppc-------------------------------------------------------

(data_1952_rich <- data_1952_gdppc %>%
  dplyr::arrange(desc(gdpPercap)) #note the use of desc()
 )

(data_2007_rich <- data_2007_gdppc %>%
  dplyr::arrange(desc(gdpPercap)) #note the use of desc()
 )


## ----arrange_life_pre00--------------------------------------------------

(data_life_pre00 <- data_gapminder %>%
  dplyr::select(country, year, lifeExp) %>%
  dplyr::filter(year <= 2000) %>%
  dplyr::arrange(desc(lifeExp))
 )


## ----arrange_life_post00-------------------------------------------------

(data_life_post00 <- data_gapminder %>%
  dplyr::select(country, year, lifeExp) %>%
  dplyr::filter(year > 2000) %>%
  dplyr::arrange(desc(lifeExp))
 )


## ----mutate_GDP_tot------------------------------------------------------

(data_GDP_tot <- data_gapminder %>%
   dplyr::mutate(GDP_total = pop*gdpPercap/10^9) #in USD billions
 )


## ----GDP_tot_most--------------------------------------------------------

(data_GDP_tot %>% 
   dplyr::filter(year == 1952) %>%
   dplyr::arrange(desc(GDP_total))
 )

(data_GDP_tot %>% 
   dplyr::filter(year == 2007) %>%
   dplyr::arrange(desc(GDP_total))
 )

## ----GDP_tot_top_5-------------------------------------------------------

(data_GDP_tot %>% 
   dplyr::filter(year == 2007) %>%
   dplyr::arrange(GDP_total) %>%
   dplyr::filter(rank(GDP_total) <= 5)
 )

## ----summ_life_exp_eur---------------------------------------------------

(life_exp_summ_eur <- data_gapminder %>%
  dplyr::filter(continent == "Europe") %>%
  dplyr::summarise(average = mean(lifeExp),
                   med = median(lifeExp),
                   std = sd(lifeExp),
                   variance = var(lifeExp),
                   iqr = IQR(lifeExp)
                   )
 )



## ----summ_cont-----------------------------------------------------------

(summ_life_exp <- data_gapminder %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise(average = mean(lifeExp),
                   med = median(lifeExp),
                   std = sd(lifeExp),
                   variance = var(lifeExp),
                   iqr = IQR(lifeExp)
                   )
 )

ggplot(data = summ_life_exp, 
       aes(x = continent, y = average)) +
  geom_bar(stat = "identity")


(summ_year_life_exp <- data_gapminder %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(average = mean(lifeExp),
                   med = median(lifeExp),
                   std = sd(lifeExp),
                   variance = var(lifeExp),
                   iqr = IQR(lifeExp)
                   )
 )

ggplot(data = summ_year_life_exp, 
       aes(x = year, y = average)) + 
  geom_point()




