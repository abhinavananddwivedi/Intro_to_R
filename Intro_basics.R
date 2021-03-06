## ----setup, eval=T, message=FALSE, warning=T, include=T, echo=T----------

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

## ----calculator----------------------------------------------------------

(t <- sin(pi/4)) #enclosing in parentheses prints output
(2.342929*1.19483)/4.9802244
x <- 3*10.4293939
round(x, digits = 4)


## ----basic_operations----------------------------------------------------

1:10

seq(1:10) #the sequence generator function


## ----library-------------------------------------------------------------

library(tidyverse)


## ----pipe_0--------------------------------------------------------------

x <- 1:100 #initial vector

x_1 <- sin(x) #note vectorized function

x_2 <- (mean(x_1))^2 #take mean, square

if(x_2 >= 1) #note the syntax for the if statement
{
  print("more than 1")
} else
{
  print("less than 1")
}

# or more succinctly in one line

if(mean(sin(x))^2 >= 1)
{
  print("more than 1")
} else
{
  print("less than 1")
}


## ----pipe_1--------------------------------------------------------------

x_3 <- x %>% sin(.) %>% mean(.) %>% .^2 
# read as: take x, then take sine 
# then take mean, then square

if(x_3 >= 1)
{
  print("more than 10")
} else
{
  print("less than 1")
}


## ----vector--------------------------------------------------------------

1 + 1:10

#is the same operation as 

rep(1, 10) + 1:10 #note the highly useful rep() function

## ----vector_scalar-------------------------------------------------------

1:3 + 1:15

#is the same as

rep(1:3, 5) + 1:15


## ----concatenate---------------------------------------------------------

y <- 1:4

(y_2 <- c(5:10, y)) #concatenate

(y_3 <- y_2[c(3:7)] )#note the square brackets
(y_4 <- y_2[-c(1, 4)]) #note the - sign



## ----mat_array-----------------------------------------------------------

c_1 <- c(4, 9, 10, 12)
c_2 <- c(10, 3, 1, 10)

(mat_c <- cbind(c_1, c_2)) #column-bind
(mat_r <- rbind(c_1, c_2)) #row-bind


## ----data_frames---------------------------------------------------------

df <- data.frame(a = runif(1:10), 
                 b = rnorm(10, 0, 1),
                 c = 11:20
                 )

df$a %>% head()

names(df)


## ----data_frames_subsets-------------------------------------------------

df[, 1] %>% head() #column 1

df[3, ] %>% head() #row 3

df[c(1,2), "c"]


## ----data_frame_subset_log-----------------------------------------------

df[df$b < 0.5, ] #only those rows whose column b has values < 0.5



## ----data_frame_subset_assgn---------------------------------------------

df[df$c >= 14, "a"] <- NA

df


## ----function_0----------------------------------------------------------

t_1 <- rnorm(100, 5, 10) #number of points, mean, sigma
t_2 <- rnorm(100, 10, 100)
t_3 <- rnorm(100, 20, 200)

t_1_s <- (t_1-5)/10 
t_2_s <- (t_1-10)/100
t_3_s <- (t_1-20)/200


## ----function_1----------------------------------------------------------

norm_std <- function(t, mu, sigma) #note the syntax
{
  t_std <- (t - mu)/sigma
  
  return(t_std) #note the return function
}

norm_std(t_1, 5, 10) %>% head(.) #what does this mean?
norm_std(t_2, 10, 100) %>% head(.)
norm_std(t_3, 20, 200) %>% head(.)


