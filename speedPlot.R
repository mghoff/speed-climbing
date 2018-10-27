rm(list=ls())

require(rvest)
require(dplyr)
require(stringr)
require(ggplot2)

url <- 'https://en.wikipedia.org/wiki/The_Nose_(El_Capitan)'
html_file <- download.file(url, destfile = 'El Cap Wiki.html')

wiki <- read_html('El Cap Wiki.html')

spd_table <- wiki %>%
    xml_nodes(".wikitable") %>%
    .[2] %>%
    xml_nodes("tr") %>%
    xml_text()

df <- NULL
for(i in 1:(length(spd_table)-1)) {
    if (str_detect(spd_table[i+1],"\n\n") == TRUE)
        {split <- strsplit(spd_table[i+1], "\n\n")}
    else {split <- strsplit(spd_table[i+1], "\n")}
    # split <- strsplit(spd_table[i+1], "\n")
    date <- split[[1]][1]
    team <- split[[1]][2]
    t <- split[[1]][3]
    df <- rbind(df, data.frame(date, team, t))
}
names(df) <- c("date", "team", "t")
df

cdf <- df %>%
    mutate(year = as.numeric(substr(date, 1, 4))) %>%
    mutate(t2 = gsub("\\s*\\([^\\)]+\\)","", t)) %>%
    mutate(t3 = gsub("\\[[^*]+\\]","", t2)) %>%
    mutate(t4 = ifelse(year <= 1992, paste0(t3,":00"), t3)) %>%
    mutate(time = str_pad(t4, 8, pad="0")) %>%
    select(date, year, team, time) %>%
    mutate(duration = as.numeric(substr(time,1,2))*3600 +
                        as.numeric(substr(time,4,5))*60 +
                        as.numeric(substr(time,7,8))) %>%
    mutate(hours = duration / 3600)
str(cdf)

exp.fit <- lm(log(hours) ~ year, data = cdf)
summary(exp.fit)
timevalues <- seq(1973, 2068, .1)
exp.pred <- exp(predict(exp.fit, list(year=timevalues)))

plot(hours~year, data=cdf,
    main = "Nose Speed Records (Since First Single-Day Ascent)",
    xlab = "Year", ylab = "Hours",
    xlim = c(1973, 2068), ylim = c(0, 20))
lines(timevalues, exp.pred, col = "red")
