library(plotly)
library(tidyverse)


# data: odds & results ----
df <- merge(
	# odds
	read_csv("odds/201707.csv") %>% 
		mutate(
			# names concatenated in alphabetical order
			rikishi = ifelse(
				rikishi1 < rikishi2,
				paste(rikishi1, rikishi2),
				paste(rikishi2, rikishi1)
			),
			# implied probability (first name wins)
			p = ifelse(
				rikishi1 < rikishi2,
				odds2 / (odds1 + odds2),
				odds1 / (odds1 + odds2)
			)
		),
	# results
	read_csv("results/201707.csv") %>% 
		transmute(
			day,
			# names concatenated in alphabetical order
			rikishi = ifelse(
				shikona1 < shikona2,
				paste(shikona1, shikona2),
				paste(shikona2, shikona1)
			),
			# outcome (first name won/lost -> 1/0)
			win = ifelse(
				shikona1 < shikona2,
				win1,
				win2
			)
		)
)


# faceted plot ----
subplot(
	lapply(
		unique(df$day),
		function(x) plot_ly(
			df %>% 
				group_by(day, ts) %>% 
				filter(n() > 9) %>% 
				summarise(y = mean(-(log(p) * win + log(1 - p) * (1 - win)))) %>% 
				filter(day == x),
			x = ~ts,
			y = ~y
		) %>% 
		add_lines(
			color = I("black"),
			hoverinfo = "x+y"
		) %>% 
		layout(
			hovermode = "x",
			showlegend = FALSE,
			xaxis = list(
				fixedrange = TRUE,
				tickfont = list(size = 8),
				title = ""
			),
			yaxis = list(
				dtick = .1,
				fixedrange = TRUE,
				range = c(.41, .79),
				tickfont = list(size = 8),
				title = ""
			)
		) %>% 
		config(displayModeBar = FALSE)
	),
	nrows = 3,
	shareY = TRUE
)


# aggregate plot ----
df %>% 
	# x - minutes since odds went live
	group_by(rikishi) %>% 
	group_by(x = round(as.numeric(difftime(ts, min(ts), units = "mins")))) %>% 
	# remove data points with 99 or fewer observations (noisy)
	filter(n() > 99) %>% 
	# y - cross-entropy error
	summarise(y = mean(-(log(p) * win + log(1 - p) * (1 - win)))) %>% 
	# plot
	plot_ly(x = ~x, y = ~y) %>% 
	add_lines(color = I("black")) %>% 
	layout(
		hovermode = "x",
		xaxis = list(
			dtick = 60,
			fixedrange = TRUE,
			title = "minutes",
			zeroline = FALSE
		),
		yaxis = list(
			dtick = .01,
			fixedrange = TRUE,
			title = "cross-entropy error"
		)
	) %>%
	config(displayModeBar = FALSE)
