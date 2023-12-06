#Import dataset from gitHub
virus_data<- read.csv("question-5-data/Cui_etal2014.csv")
print(virus_data)

#Finding the numbers of rows and columns, using the functions: 
n_rows <- nrow(virus_data) #=33
n_columns <- ncol(virus_data) #=13


#log-transforming the values into new columns
virus_data$log.Genome.length..kb.<- log(virus_data$Genome.length..kb.)
virus_data$log.Virion.volume..nm.nm.nm. <- log(virus_data$Virion.volume..nm.nm.nm.)

#fitting a linear model, using the transformed data
linear_model <- lm(log.Virion.volume..nm.nm.nm. ~ log.Genome.length..kb., data = virus_data)
summary(linear_model)

#Intercept =7.0748
#Slope =  1.5152 

#Back-transforming the intercept:

exp(7.0748)

#1181.807


#Plotting initial graph: x-axis (genome length), y-axis (viron volume) 
ggplot(aes(x=Genome.length..kb., y=Virion.volume..nm.nm.nm.), data = virus_data) +
  geom_point() + 
  ggtitle("The relationship between Genome length and Viron volume in dsDNA viruses") +
  xlab("Genome length (kb)") +
  ylab("Virion volume (nm3)") +
  theme_light()


#Plotting the log-transformed graph
#First, I must create new columns for virus_data containing log-transformed data

virus_data$log.Genome.length..kb.<- log(virus_data$Genome.length..kb.)
virus_data$log.Virion.volume..nm.nm.nm. <- log(virus_data$Virion.volume..nm.nm.nm.)

#The plot:  x-axis (log(genome length), y-axis (log(viron volume))

ggplot(aes(x=log.Genome.length..kb., y=log.Virion.volume..nm.nm.nm.), data = virus_data) +
  geom_point() +
  geom_smooth(method = "lm", colour = '#4361eE', linewidth = 0.5, se = TRUE, fill = '#A3B5C0', alpha = 0.4) + 
  xlab("log [Genome length (kb)]") +
  ylab("log [Virion volume (nm3)]") +
  theme_light()

#predict estimated volume of a 300 kb dsDNA virus
prediction_300kb <- predict(linear_model, data.frame(log.Genome.length..kb. = log(300)))
prediction_300kb #=13.71733

#to obtain actual value, you back-transform the prediction 
actual_prediction <- exp(prediction_300kb) #e^
actual_prediction #=6698076 


