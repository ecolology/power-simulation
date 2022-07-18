require(tidyverse)

N <- 300000 # ~300 million americans, assume 0.3 million to be able to plot it all
cut_N <- round(N/450) # ~450 NBA players, so sample only top 450 players

height <- rnorm(N)
scoring <- rnorm(N, mean = height)
prob_basketballer <- rnorm(N, mean = height + scoring)
discrete_basketballer <- as.numeric(cut_number(prob_basketballer,cut_N)) # divide into 100 parts, 10 being heighest!
dat <- tibble(height, scoring, prob_basketballer, discrete_basketballer) %>%
	mutate(nba_player = ifelse(discrete_basketballer == cut_N, TRUE, FALSE))

dat %>%
	filter(nba_player == TRUE) %>%
	ggplot(aes(x = height, y = scoring)) +
	geom_point(alpha = 0.4) +
	geom_smooth(method = "lm", color = "black")

dat %>%
	# slice_sample(n=10000) %>%
	ggplot(aes(x = height, y = scoring)) +
	geom_point(aes(color = nba_player), alpha = 0.1) +
	scale_color_manual(values = c("grey", "black")) +
	geom_smooth(aes(color = nba_player), method = "lm") +
	geom_smooth(method = "lm")
ggsave(filename=  "plot.png")
