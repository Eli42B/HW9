####################################################33
# HW 9 
#Elizabeth Braatz 
####################################################


#---------------------------------
#Objective 1 

#Setup 
library(tidyverse) #installing packages 
set.seed(7) #setting the seed so rnorm is consistent 

#A: 

#Equation : yi = a + Bxi + ei

#Creating variables 
x = 1:100 
a = 15 
B = 0.5 
e_1 = rnorm(100, mean = 0, sd = 1)
e_10 = rnorm(100, mean = 0, sd = 10)
e_25 = rnorm(100, mean = 0, sd = 25) 
y_1 = a + B*x + e_1 
y_2 = a + B*x + e_10 
y_3 = a + B*x + e_25 

d = data.frame(x = x, y_1 = y_1, y_2 = y_2, y_3 = y_3) #creating the dataframe 

#B: 

#Time to plot 

#Plotting with multiple lines: https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/ 
#facet wrapping: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html 
#labels: https://ggplot2.tidyverse.org/reference/labs.html 


#We're going to need to rearrange this data to make ggplot2 happy 
d2 = d %>% 
  select(x, y_1, y_2, y_3) %>% 
  gather(key = "variable", value = "value", -x)

#Visualization 

#Base plot 
base_plot = ggplot(d2, aes(x = x, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred","steelblue","purple"))+ 
  labs(
    title = "x and y with different sd", 
    y = "y") 

base_plot

#Base plot but facet wrapped 
facet_plot = base_plot + 
  facet_wrap(~variable, nrow = 1)
facet_plot

#C: How does the ability to visually detect a relationship between y and x change as observation error increases (no answer or analysis needed – just think about this)
#Way harder to pick out a pattern when the error gets bigger. More 'noise'. 

#---------------------------------
#Objective 2 

#Understanding the binomial command 
#https://www.geeksforgeeks.org/r-language/a-guide-to-dbinom-pbinom-qbinom-and-rbinom-in-r/ 

#dbinom = probability of success of getting N successes 
dbinom(x = 17, size = 20, prob = 0.5)  #what is the probability of getting heads exactly 7 times if the coin is tossed fairly (prob = 0.5) 20 times

#pbinom = probability of getting more or less than N successes
pbinom(3, size = 10, prob = 0.5, lower.tail = FALSE) #what is the probability of getting heads more than 3 times if the coin is flipped fairly 10 times 

pbinom(30, size = 50, prob = 0.7) #what is the probability of a man scoring 30 or fewer strikes while bowling, if he bowls 50 times and his probability of scoring a strike is 30%?) 

#qnorm = what is the Nth quartile of a binomial distribution 
qbinom(0.19, size = 30, prob = 0.6) #what is the 19th quartile of a binomial distribution with 30 trials and a probabiliyt o fsuccess of 0.5

#rbinom 
gfg = rbinom(500, size = 100, prob = 0.6) #Imagine that we run 500 experiments, each with 100 little repetitions, and the probability of success for each repetition is 0.6. Generate a vector of 500 wiht the number of successes we would expect to see from each experiment.

#OK, we are going to want to use the pbinom 
y = pbinom(1, size = 20, prob = 0.55, lower.tail = FALSE) #what is hte probability of getting heads more than once if hte coin is flipped 20 times, and the coin is a bit unfair p = 0.55? 
y = pbinom(20, size = 20, prob = 0.55, lower.tail = FALSE)
#So we will want a vector if y's from getting at least 1 coin flip of heads to all 20 


# Part A 

#setting up the data 
x = 1:20 
y_55 = 1- pbinom(x, size = 20, prob = 0.55, lower.tail = FALSE)
coins = data.frame(x = x, y = y_55)

#plotting for p = 0.55 
plot = ggplot(coins, aes(x = x, y = y_55)) + 
  geom_point() + 
  labs(
    title = "Probability we think Don Corleone is cheating", 
    x = "Number of coin flips", 
    y = "Probability he's cheating"
  )
plot

#Part B 

#setting up the data 
x = 1:20 
y_50 = pbinom(x, size = 20, prob = 0.50, lower.tail = FALSE)
y_55 = pbinom(x, size = 20, prob = 0.55, lower.tail = FALSE)
y_6 = pbinom(x, size = 20, prob = 0.6, lower.tail = FALSE)
y_65 = pbinom(x, size = 20, prob = 0.65, lower.tail = FALSE)
coins = tibble(x = x,y_50, y_55 = y_55, y_6 = y_6, y_65 = y_65)

#We're going to need to rearrange this data to make ggplot2 happy 
coins2 = coins %>% 
  select(x, y_50, y_55, y_6, y_65) %>% 
  gather(key = "variable", value = "value", -x)

#plot 
p = ggplot(data = coins2, aes(x = x, y = value)) + 
  geom_line(aes(color = variable)) + 
  labs(
    title = "Probability that we will get heads N times in a row", 
    y = "Probability of having heads continuously after X flips", 
    x = "Number of coin flips") + 
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") + 
  theme_bw()
p 

#this is super not right, we actually need the probability tha the's cheating, and this isn't telling us the probability of successfully figuring out if hte coin is unfair, just hte actual probability that the coin would be heads multiple times in a row. 

# Nathan said to do a loop and include the probability from 1:100 simulations, and increment it up by 1 every time you passed the 0.05 threshold. Consulted the Oracle (Chat GPT) with those instructions and it pooped out this: 

set.seed(123)      # reproducible results
sims <- 100        # "out of 100" simulations per n (as you requested)
ns <- 1:20         # number of flips
ps <- c(0.55, 0.60, 0.65)

# empty list to collect results
res_list <- list()

for (p in ps) {
  for (n in ns) {
    # simulate `sims` experiments each with `n` flips and success probability `p`
    successes <- rbinom(n = sims, size = n, prob = p)
    # compute p-values for one-sided test H0: p=0.5 vs H1: p>0.5
    pvals <- sapply(successes, function(k) binom.test(k, n, p = 0.5, alternative = "greater")$p.value)
    power_est <- mean(pvals < 0.05)   # fraction of sims where we reject H0
    res_list[[length(res_list) + 1]] <- tibble(n = n, p = p, power = power_est)
  }
}

results <- bind_rows(res_list)

# make p a factor for plotting colors / legend order
results <- results %>% mutate(p = factor(p))

# Plot: simulated power curves
ggplot(results, aes(x = n, y = power, color = p)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = ns) +
  labs(
    title = "Simulated probability of detecting an unfair coin (one-sided binomial test)",
    subtitle = "Each point = proportion (out of 100 simulations) where p-value < 0.05",
    x = "Number of coin flips (n)",
    y = "Probability of detecting unfairness (power)",
    color = "True p"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

#yep, Nathan and the oracle were right and I was wrong, this looks much more realistic 
#The y axis looks really weird becuase it's the probability of successfully detecting unfairness when teh coin is actually unfair 
#So, we actually are terrible at figuring out if coins are unfair 
#The more unfair hte coin (0.65) the highe rour probability of successfully detecting it's unfair, but we never get above 30% likelihood of successfully doing it because we only have 20 flips to count out of 
#This seems wild 