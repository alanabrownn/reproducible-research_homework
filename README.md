# Reproducible research: version control and R

## Question 4) Modelling Brownian motion 

### A

*A script for simulating a random walk is provided in the question-4-code folder of this repo. Execute the code to produce the paths of two random walks. What do you observe?*

My graphs:
![Random path plots](https://github.com/lb23092/reproducible-research_homework/blob/f865e87e9b9d04e83195154ba850fb8a8451cd52/question-4-code/Q4_random_path_plots.png)
These graphs are also stored in my `random_path_plots.png` file.

These graphs demonstrate Brownian motion in 2 dimensions. They each show a single particle which starts at the position vector (0,0), and then takes 500 steps of equal length (h=0.25) in a random direction. For each step, an angle between 0-2pi (360 degrees) is drawn according to a uniform random distribution, and because of this, each movement direction is independent from one another. The legend for these graph shows the time point at which each vector was obtained, and they are coloured in a gradient from black to blue to allow for easy visualization of the direction of movement. 

When comparing across the two graphs, you can see that there is a significant difference in the paths the particles take. The only similarity is that they both start at the origin, but from then on, the particle on the left graph largely stays in the negative x-coordinates (between -5 and 2), whereas the particle on the right graph mainly stays in the positive x-coordinates (-1 to 5). I also observe that the the path on the left graph is ultimately more circular with, after 500 time points, the particle finishing closer to the origin. In contrast, particle in the right graph ends up further away from the origin. Therefore, random movement generated in such a way could ultimately produce very different paths with very different destinations. 

### B 

*Investigate the term random seeds. What is a random seed and how does it work?*

When you ask a programming language to generate random numbers, although the output appears random and unpredictable, it is in fact only *pseudorandom*. They are instead being generated by an algorithm called *Pseudorandom Number Generator (PRNG)*, whose output is determined by an initial value, known as the random-seed. Within code, you are able to set this random-seed at a specific value e.g. random.seed(23) prior to generating numbers. This sets the starting point of the algorithm to '23', and whilst the numbers generated following this imput are random, when you run the same seed again the output will be identical. Therefore, setting the random seeds allows for reproductability. When making alterations in your own code or somebody else's, you want to be sure that the changes *you* are making are impacting the results, and not the different set of numbers that are being randomly generated at the start. This technique is particularly useful in these circumstances. 

### C

*Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked reproducible-research homework repo.*

To make the Brownian motion simulation reproducible, I added the function set.seed() to my random_walk function. That way, every time I use this function to generate random moment, the same series of angles will be selected resulting in the same path. The input value I chose was 23. This is an arbitrary number which I selected for no particular reason. You can see how I applied this function by looking at the latest commit to the file `random_walk.R` in this repo, within the `question-4-code` file.

As expected, the graphs produced by this edited code are the same, thanks to set.seed():
![same graphs](https://github.com/lb23092/reproducible-research_homework/blob/f865e87e9b9d04e83195154ba850fb8a8451cd52/question-4-code/Q4_reproducible_path_plots.png)
These graphs are also stored in my `reproducible_path_plots.png` file.

### D

*Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the README.md of the fork)*

My screenshot of the comparison view for the original code versus the edited 
![comparing commits screenshot](https://github.com/lb23092/reproducible-research_homework/blob/f865e87e9b9d04e83195154ba850fb8a8451cd52/question-4-code/Q4_comparing_commits.png)
This image is also stored in my `Comparing_commits.png` file.

## Question 5) Viral data 

### A

*Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the question-5-data folder). How many rows and columns does the table have?*

To import and store the data, I used the following function:

```
virus_data<- read.csv("question-5-data/Cui_etal2014.csv")
```

You can count the number of rows and columns by visual inspection of the table. Or, to make things quicker, you can use the functions: 

```
n_rows <- nrow(virus_data) 
n_columns <- ncol(virus_data)
```
The output of these functions tells us that: 
- number of rows = 33 (excluding the header)
- number of columns = 13

### B

*What transformation can you use to fit a linear model to the data? Apply the transformation.*

To fit a linear model to this data, you can log-transform both variables. I decided to specifically take the natural logarithm (ln) of Viron volume and Genome length. For this, I first created two additional columns in my virus_data dataframe, and then I applied the log() function to my initial data to output the transformed values. The code to achieve this was:

```
virus_data$log.Genome.length..kb.<- log(virus_data$Genome.length..kb.)
virus_data$log.Virion.volume..nm.nm.nm. <- log(virus_data$Virion.volume..nm.nm.nm.)
```
Once I had obtained the log-transformed values which were now stored in my dataset, I used them to fit a linear model:

```
linear_model <- lm(log.Virion.volume..nm.nm.nm. ~ log.Genome.length..kb., data = virus_data)
```
### C

*Find the exponent (α) and scaling factor (β) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in Table 2 of the paper, did you find the same values?*

The exponent and the scaling fatcor for dsDNA viruses are given in the summary of the linear model just created. The function summary(linear_model) can be used to call this:

```
Call:
lm(formula = log.Virion.volume..nm.nm.nm. ~ log.Genome.length..kb., 
    data = virus_data)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.8523 -1.2530 -0.1026  1.0739  2.0193 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)              7.0748     0.7693   9.196 2.28e-10 ***
log.Genome.length..kb.   1.5152     0.1725   8.784 6.44e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.263 on 31 degrees of freedom
Multiple R-squared:  0.7134,	Adjusted R-squared:  0.7042 
F-statistic: 77.16 on 1 and 31 DF,  p-value: 6.438e-10
```

But how can this be interpreted? Like I said previously, to be able to fit a linear model to an allometric equation, we have to log-transform it. Taking the natural logarithm of both sides of the initial equation and then simplifying it, produces the final equation: 

```math 
V = βL^α
```
```math
ln(V) = ln(βL^α)
```
```math
ln(V) = ln(β) + ln(L^α)
```
```math
ln(V) = ln(β) + αln(L)
```
As you can see, this equation is in a standard linear form  y= mx + c. For this reason, the Intercept estimate represents β (scaling factor), and the Slope represents α (exponent). However, as you can see, the estimate for β must be back-transformed by raising it to the power of e, to obtain the actual value. In contrast, the estimate for α already denotes the actual value. The code used to do this was:

```
exp(7.0748)
```

**So in conclusion, the constants:**

- β (scaling factor) = 1181.807
- α (exponent) = 1.5152 

**P-values**

- β = 2.28e-10***
- α = 6.44e-10 ***

**Interpretation**

The values for these constants are statistically significant because both p-values are lower than 0.01 . This means that there are significantly different from 0. 

**Comparison with paper**

These values are the same as those given in the dsDNA row in table 2 of the paper - which is a great sign that our analysis has worked!

### D

*Write the code to reproduce the figure shown in the question PDF*

The code I used was:
```
ggplot(aes(x=log.Genome.length..kb., y=log.Virion.volume..nm.nm.nm.), data = virus_data) +
  geom_point() +
  geom_smooth(method = "lm", colour = '#4361eE', linewidth = 0.5, se = TRUE, fill = '#A3B5C0', alpha = 0.4) + 
  xlab("log [Genome length (kb)]") +
  ylab("log [Virion volume (nm3)]") +
  theme_light()
  ```
Although not exactly the same, this is my best attempt at reproducing the figure. As you can see within the geom_smooth() function, I picked a hex code for the regression line and the standard error which produce a similar colour scheme to that found on the graph provided. I also narrowed the width of my line, and labelled my axes with the same titles. Finally, I selected the light theme because I believe it is the most similar to what is used. The graph I produced can be seen below:

![Reproduced graph](https://github.com/lb23092/reproducible-research_homework/blob/971f19681696119b3cb6f7816951a1a34d8b6011/Q5_reproduced_plot.png)

### E

*What is the estimated volume of a 300 kb dsDNA virus?*

We can use the predict() function in ggplot2 to make predictions from our model fit:
```
prediction_300kb <- predict(linear_model, data.frame(log.Genome.length..kb. = log(300)))
```
The model prediction = 15.71733

Once we have obtained the prediction, we must back-transform it to get the actual value:
```
exp(prediction_300kb) #e^
```
Actual prediction: 6698076

In conclusion, our model estimates that a genome length of 300kb will have a predicted viron volume of 6698076mm3.

## Bonus question 
*Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to en- hance the reproducibility and replicability of your work? what limitations do they have?*















## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points (plus an optional bonus question worth 10 extra points). First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers. All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   - A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points)
   - Investigate the term **random seeds**. What is a random seed and how does it work? (5 points)
   - Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points)
   - Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points)

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \beta L^{\alpha}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   - Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)
   - What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points)
   - Find the exponent ($\alpha$) and scaling factor ($\beta$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points)
   - Write the code to reproduce the figure shown below. (10 points)

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  - What is the estimated volume of a 300 kb dsDNA virus? (4 points)

## **Bonus** Explain the difference between reproducibility and replicability in scientific research. How can git and GitHub be used to enhance the reproducibility and replicability of your work? what limitations do they have? (e.g. check the platform [protocols.io](https://www.protocols.io/)).
