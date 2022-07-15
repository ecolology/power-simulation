

require(tidyverse)
theme_set(theme_light())

mpg = 10
delta = 0.5
sigma = 0.5
N = 10

# Code that prints the p-value of a single simulation:
y1 <- rnorm(n=N, mpg, sigma)
y2 <- rnorm(n=N, mpg+delta, sigma)
t.test(y1, y2) %>%
	broom::tidy() %>%
	pull(p.value)

# Put into replicate to replicate across 1000 simulations
p_vals <- replicate(1000, {
	y1 <- rnorm(n=N, mpg, sigma)
	y2 <- rnorm(n=N, mpg+delta, sigma)
	t.test(y1, y2) %>%
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

	

