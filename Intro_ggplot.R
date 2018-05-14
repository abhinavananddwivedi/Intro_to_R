## ----library_declaration, eval=T, message=FALSE, warning=T, include=FALSE, echo=F----

library(tidyverse)
library(rmarkdown)
library(knitr)
library(xml2)
library(gapminder)


## ----data_load, eval=T, message=F, warning=T, include=T, echo=T----------

data_gapminder <- gapminder::gapminder 


## ----plot_eur, eval=T, echo=T, message=F, warning=T, include=T-----------

data_eur_2007 <- data_gapminder %>% 
  dplyr::filter(year == 2007) %>% #isolates variables for year 2007
  dplyr::filter(continent == "Europe") 

(plot_eur <- ggplot(data = data_eur_2007) +
  geom_point(mapping = aes(x = gdpPercap, y = country))
  ) #why parentheses around the command?


## ----plot_asia, eval=T, echo=T, message=F, warning=T, include=T----------

data_asia_2007 <- data_gapminder %>% 
  dplyr::filter(year == 2007) %>% #isolates variables for year 2007
  dplyr::filter(continent == "Asia") #collect only Asian countries

(plot_asia <- ggplot(data = data_asia_2007) +
  geom_point(mapping = aes(x = gdpPercap, y = country)) +
  labs(x = "GDP/capita (USD)", 
       y = "Country",
       title = "GDP per capita in Asia",
       subtitle = "Put subtitle here"))


## ----CIFG, message=F, warning=T, eval=T, echo=T, include=T---------------

data_CIFG <- data_gapminder %>%
  dplyr::filter(country %in% c("China", 
                               "India", 
                               "France", 
                               "Germany"
                               )
                )

(plot_CIFG_life_exp <- ggplot(data = data_CIFG) +
  geom_line(mapping = aes(x = year, 
                          y = lifeExp, 
                          color = country)
            )
  )


## ----CIFG_Facet, warning=T, message=F, include=T, eval=T, echo=T---------

(plot_CIFG_life_cont <- ggplot(data = data_CIFG) +
  geom_line(mapping = aes(x = year, 
                          y = lifeExp, 
                          color = country)
            ) +
  facet_wrap(~ continent)
 )


## ----CIFG_Mult_Geom, warning=T, message=F, include=T, echo=T, eval=T-----
(plot_CIFG_mult_geom <- ggplot(data = data_CIFG) +
   geom_point(mapping = aes(x = year, y = lifeExp, color = country)) +
   geom_line(mapping = aes(x = year, y = lifeExp, linetype = country))
  )


## ----CIFG_Mult_Geom_2, warning=F, include=T, eval=T, echo=T, message=F----

plot_CIFG_mult_geom_2 <- ggplot(data = data_CIFG, 
                                mapping = aes(x = year, 
                                              y = lifeExp)) +
  geom_point(mapping =  aes(color = country)) +
  geom_line(mapping = aes(linetype = country))
 


## ----CIFG_pop_Ind, include=T, echo=T, eval=T, message=F, warning=T-------

(plot_CIFG_pop_Ind <- ggplot(data = filter(data_CIFG, #why no +?
                                           country == "India"), 
                         mapping = aes(x = year, y = pop)) +
  geom_point(mapping = aes(color = country)) +
  geom_smooth(method = "lm", se = F) #se = "standard errors"
 )


## ----CIFG_pop_China, include=T, echo=T, eval=T, message=F, warning=T-----

(plot_CIFG_pop_China <- ggplot(data = filter(data_CIFG, 
                                             country == "China"), 
                         mapping = aes(x = year, y = pop)) +
  geom_point(mapping = aes(color = country)) +
  geom_smooth(method = "lm") #se = T by default
 )


## ----CIFG_pop_Asia_loess, include=T, echo=T, eval=T, message=F, warning=T----

(plot_CIFG_pop_Ind <- ggplot(data = filter(data_CIFG, 
                                           continent == "Asia"), 
                         mapping = aes(x = year, y = pop)) +
  geom_point(mapping = aes(color = country)) +
  geom_smooth() 
 )


## ----CIFG_pop_2007, include=T, echo=T, eval=T, message=F, warning=T------

(plot_CIFG_pop_2007 <- ggplot(data = filter(data_CIFG,
                                            year == 2007),
                              mapping = aes(x = country, 
                                            y = gdpPercap)) + 
   geom_bar(mapping = aes(fill = country), #what does this mean?
            stat = "identity") #what does this mean?
 )


## ----plot_CIFG_box, include=T, echo=T, eval=T, message=F, warning=T------

(plot_CIFG_box <- ggplot(data = data_CIFG,
                         mapping = aes(x = year, 
                                       y = lifeExp,
                                       color = country)) +
   geom_boxplot()
   )


## ----plot_eur_box_flip, include=T, echo=T, eval=T, message=F, warning=T----

(plot_eur_box_flip <- ggplot(data = filter(data_gapminder,
                                           continent == "Europe"),
                         mapping = aes(x = year, 
                                       y = lifeExp,
                                       color = country)) +
   geom_boxplot() +
   coord_flip()
   )


## ----CIFG_theme, warning=F, include=T, eval=T, echo=T, message=F---------

(plot_CIFG_theme <- ggplot(data = data_CIFG, 
                                mapping = aes(x = year, 
                                              y = lifeExp)) +
  geom_point(mapping =  aes(color = country)) +
  geom_line(mapping = aes(linetype = country)) +
   labs(x = "Years", 
        y = "Life Expectancy",
        title = "Illustration of Different Themes") +
  theme_classic() #note the classic theme
 )
 
(plot_CIFG_theme <- ggplot(data = data_CIFG, 
                                mapping = aes(x = year, 
                                              y = lifeExp)) +
  geom_point(mapping =  aes(color = country)) +
  geom_line(mapping = aes(linetype = country)) +
   labs(x = "Years", 
        y = "Life Expectancy",
        title = "Illustration of Different Themes") +
  theme_bw() #note the bw theme (black and white)
 )

(plot_CIFG_theme <- ggplot(data = data_CIFG, 
                                mapping = aes(x = year, 
                                              y = lifeExp)) +
  geom_point(mapping =  aes(color = country)) +
  geom_line(mapping = aes(linetype = country)) +
   labs(x = "Years", 
        y = "Life Expectancy",
        caption = "Insert caption here",
        title = "Illustration of Different Themes") +
  theme_minimal() #note the minimal theme
 )


## ----wide_vs_long, warning=F, include=T, eval=T, echo=T, message=F-------

temp_w <- cbind(1991:2010, 
                rnorm(20, 0, 1), 
                rnorm(20, 2, 5),
                rnorm(20, 1, 9)) %>%
  tibble::as_tibble()
names(temp_w) <- c("year", "N01", "N25", "N19") #data in "wide" format

head(temp_w)

(ggplot(data = temp_w,
        mapping = aes(x = year)) + 
    geom_line(mapping = aes(y = N01), color = "red") +
    geom_line(mapping = aes(y = N25), color = "blue") +
    geom_line(mapping = aes(y = N19), color = "green")
  ) #this method of plotting is not encouraged

temp_l <- tidyr::gather(temp_w, c("N01", "N25", "N19"), 
                        key = "Normal RV", 
                        value = "Normal Realization"
                        ) #converting data from wide to "long"

head(temp_l)

(ggplot(data = temp_l,
        mapping = aes(x = year, 
                      y = `Normal Realization`,
                      color = `Normal RV`)) +
    geom_line()
  ) #the preferred way of plotting is in the long format


