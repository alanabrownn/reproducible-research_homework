#Install important packages
install.packages("ggplot2")
install.packages("gridExtra")

#attaching packages to current R session
library(ggplot2)
library(gridExtra)

#creating a function called random_walk 
random_walk  <- function (n_steps) { #function where you imput n_steps
  
  set.seed(23) #makes work reproducible 
  
  df <- data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  #creates a data frame: x, y, and time columns 
  
  df[1,] <- c(0,0,1) #sets first row values
  
  for (i in 2:n_steps) { #creates for loop 
    
    h <- 0.25 #step size
    
    angle <- runif(1, min = 0, max = 2*pi) #pick random angle 0-360 degrees
    
    df[i,1] <- df[i-1,1] + cos(angle)*h #creating next x value based on angle
    
    df[i,2] <- df[i-1,2] + sin(angle)*h #creating next y value based on angle
    
    df[i,3] <- i
  }
  
  return(df) 
  
}

data1 <- random_walk(500)  #now we are applying the function, for t=500

#plotting the results for this specific function
plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")


data2 <- random_walk(500) #applying the function again for t=500
#if we didnt have set.seed(), this would produce a different path output

plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

grid.arrange(plot1, plot2, ncol=2)


