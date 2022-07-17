# Simulations for experiment design and power analysis
# (c) Kevin Bairos-Novak, July 2022
# Inspired by a simulation workshop by Malika Ihle (found here: https://malikaihle.github.io/Introduction-Simulations-in-R/)


## Basic simulations
# Install the tidyverse if you haven't already! use: install.packages("tidyverse")
library(tidyverse); theme_set(theme_classic())
library(VGAM)


###########################

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
t.test(control_pop, treatment_pop1, paired=FALSE, conf.level = 0.95) # usually significant
t.test(control_pop, treatment_pop2, paired=FALSE, conf.level = 0.95) # usually not significant

# Clear difference in test result depending on which effect size we are considering!
# Biologically significant tends to be significant, while minimally detectable is not at N = 30


###########################

# Step 4:

# We can use the replicate function and examine the proportion of significant test results (i.e. power)!

# Put into replicate() to replicate tests across 1000 simulations:
p_vals1 <- replicate(1000, {
	
	# Simulate cancer growth for all populations using rnorm() function:
	control_pop    <- rnorm(n = N, mean = control_mean,    sd = control_sd)
	treatment_pop1 <- rnorm(n = N, mean = treatment_mean1, sd = treatment_sd)

	# Run the t-test for the control vs. biologically important effect, and
	# control vs. smallest detectable effect
	treatment_pvals1 <- t.test(control_pop, treatment_pop1, paired=FALSE, conf.level = 0.95) %>%
		broom::tidy() %>% # Clean up test results summary using broom package
		pull(p.value)
	treatment_pvals1 # print at the end to save values into vector
})
hist(p_vals1)

# Same thing for smallest detectable size
p_vals2 <- replicate(1000, {
	
	control_pop    <- rnorm(n = N, mean = control_mean,    sd = control_sd)
	treatment_pop2 <- rnorm(n = N, mean = treatment_mean2, sd = treatment_sd)
	treatment_pvals2 <- t.test(control_pop, treatment_pop2, paired=FALSE, conf.level = 0.95) %>%
		broom::tidy() %>% 
		pull(p.value)
	treatment_pvals2
})
hist(p_vals2)


# Calculate proportion of p-values:
sum(p_vals1 < 0.05) / length(p_vals1) # 0.50 or ~50% of all tests significant = 50% power (medium power)
sum(p_vals2 < 0.05) / length(p_vals2) # 0.06 or ~6% of all tests significant = 6% power (very low power)

# Therefore, we have about 50:50 chance of detecting a biologically significant difference
# if one exists given N = 30, and almost no chance of detecting our smallest detectable effect size!


###########################



# Step 5 (power analysis): 
# At what sample size do we attain 80% power? We can use a for-loop to determine optimal sample size!

# Put into for-loop to get the final power for each sample size:
power1 <- c(); power2 <- c() # initialize two output vectors
for(N in 2:100) { # for a sample size of N = 2 to 100...
	
	# Put into replicate to replicate across 1000 simulations
	p_vals1 <- replicate(1000, {
		control_pop    <- rnorm(n = N, mean = control_mean,    sd = control_sd)
		treatment_pop1 <- rnorm(n = N, mean = treatment_mean1, sd = treatment_sd)
		treatment_pvals1 <- t.test(control_pop, treatment_pop1, paired=FALSE, conf.level = 0.95) %>%
			broom::tidy() %>% 
			pull(p.value)
		treatment_pvals1
	})
	p_vals2 <- replicate(1000, {
		control_pop    <- rnorm(n = N, mean = control_mean,    sd = control_sd)
		treatment_pop2 <- rnorm(n = N, mean = treatment_mean2, sd = treatment_sd)
		treatment_pvals2 <- t.test(control_pop, treatment_pop2, paired=FALSE, conf.level = 0.95) %>%
			broom::tidy() %>% 
			pull(p.value)
		treatment_pvals2
	})
	
	# Calculate power of both types of effect sizes (1: biologically significant effect size, 2: minimally detectable effect size):
	power1 <- c(power1, sum(p_vals1 < 0.05) / length(p_vals1))
	power2 <- c(power2, sum(p_vals2 < 0.05) / length(p_vals2))
	
}

sims <- cbind(N = 2:100, power1, power2) %>% 
	as.data.frame() %>%
	pivot_longer(cols = power1:power2, names_to = "effect_size", values_to = "power") %>%
	mutate(effect_size = fct_recode(effect_size, "Biologically significant" = "power1", "Minimum detectable" = "power2"))
# save(sims, file = "sims.RData") # save the simulations


# Plot it!

# Get minimum N required to 
min_N1 <- sims %>%
	filter(power >= 0.8) %>%
	pull(N) %>%
	min()

p1 <- sims %>%
	ggplot(aes(x = N, y = power, color = effect_size)) +
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


