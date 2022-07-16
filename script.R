# Simulations for experiment design and power analysis
# (c) Kevin Bairos-Novak, July 2022
# Inspired by a simulation workshop by Malika Ihle (found here: https://malikaihle.github.io/Introduction-Simulations-in-R/)


## Basic simulations
# Install the tidyverse if you haven't already! use: install.packages("tidyverse")
library(tidyverse); theme_set(theme_classic())
library(VGAM)


# Power analysis

# I want to design an experiment:
# Effect of a new treatment drug (‘treatment’) vs. placebo (‘control’) on the 
# rate of skin cancer growth (can range in theory from -infinity to +infinity, so we
# assume a normal distribution)


# Skin cancer growth in control (placebo) group, without treatment:
control_mean = 0.12 # mm/month
control_sd = 0.25  # mm/month

# Skin cancer growth in drug treatment group:
treatment_mean1 = -0.01 # biologically important effect (mean negative growth of cancer)
treatment_mean2 =  0.11 # smallest detectable effect (smallest change in cancer growth that we can detect)
treatment_sd    = control_sd # assume similar SDs (unless we have information otherwise)

# Pick any reasonable clinical trial size for now (# people in each sub-group)
N = 30

# Simulate cancer growth for all populations using rnorm() function:
control_pop    <- rnorm(n = N, mean = control_mean,    sd = control_sd)
treatment_pop1 <- rnorm(n = N, mean = treatment_mean1, sd = treatment_sd)
treatment_pop2 <- rnorm(n = N, mean = treatment_mean2, sd = treatment_sd)

# Run the t-test for the control vs. biologically important effect, and
# control vs. smallest detectable effect
t.test(control_pop, treatment_pop1, paired=FALSE, conf.level = 0.95)
t.test(control_pop, treatment_pop2, paired=FALSE, conf.level = 0.95)


# Put into replicate to replicate across 1000 simulations
p_vals <- replicate(1000, {
	y1 <- rnorm(n=N, 0, sigma)
	y2 <- rnorm(n=N, delta, sigma)
	t.test(y1, y2, paired=FALSE, conf.level = 0.95) %>%
		broom::tidy() %>%
		pull(p.value)
})
hist(p_vals)

# Calculate proportion of p-values:
sum(p_vals < 0.05) / length(p_vals) # 0.989 or 98.9% significant!


# Put into for-loop to get the final power for each sample size:
power <- c() # initialize an output vector of powers
for(N in 2:100) {
	
	# Put into replicate to replicate across 1000 simulations
	p_vals <- replicate(1000, {
		y1 <- rnorm(n=N, mpg, sigma)
		y2 <- rnorm(n=N, mpg+delta, sigma)
		t.test(y1, y2) %>%
			broom::tidy() %>%
			pull(p.value)
	})
	
	# Calculate proportion of p-values:
	power <- c(power, sum(p_vals < 0.05) / length(p_vals))
	
}

# Plot it!
sims <- cbind(N = seq(2:100), power) %>% as.data.frame()

# Get minimum N required to 
min_N <- sims %>%
	filter(power >= 0.8) %>%
	pull(N) %>%
	min()

p1 <- sims %>%
	ggplot(aes(x = N, y = power)) +
	geom_line() +
	geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")

p1 + 
	annotate(geom = "text", label = paste("power = 0.8"), hjust = 1,
			 x = max(N), y = 0.85, color = "red") +
	annotate(geom = "segment", 
			 x = min_N, xend = min_N, y = 0, yend = 0.8,
			 color = "red", linetype = "dashed") +
	annotate(geom = "text", label = paste("n =",min_N), hjust = 0,
			 x = min_N+2, y = 0.05, color = "red")



## Poisson hurdle model

# Can use VGAM::rzapois(n=100, lambda = 3, pobs0 = 0.2) to simulate, from package VGAM


