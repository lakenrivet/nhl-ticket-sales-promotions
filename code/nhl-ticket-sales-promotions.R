# NHL Ticket Sales and Promotions
# Laken Rivet

# Load necessary libraries 
library(tidyverse)
library(rvest)
library(openxlsx)
library(readxl)
library(janitor)
library(anytime)
library(ggrepel)
library(car)
library(lattice)

# turn off scientific notation for larger numbers
options(scipen = 999)

# RELATIONSHIP BETWEEN RELEVANT FACTORS AND TICKET SALES

# scrape overall attendance for 2022-2023

# # create df to append
# total_attendance <- data.frame(t(seq(1,7,1)))
# 
# # site to scrape
# url <- read_html("https://www.hockey-reference.com/friv/attendance.cgi")
# 
# # what part of site to scrape
# stats <- url %>%
#   html_nodes("#all_stats+ #all_stats .right , #all_stats+ #all_stats .left ,
#              #all_stats+ #all_stats .center") %>%
#   html_text()
# 
# # for loop to add scraped info to df
# for (i in seq(1, length(split(stats, ceiling(seq_along(stats)/7))))) {
#   div <- split(stats, ceiling(seq_along(stats)/7))
#   if (i == 1) {
#     vars <- div[i]
#     vars <- unlist(vars)
#     colnames(total_attendance) <- c(vars)
#   }
#   else if (i > 1) {
#     team <- t(data.frame(div[i]))
#     colnames(team) <- c(vars)
#     total_attendance <- rbind(total_attendance, team)
#   }
# }
# 
# # drop first row of df
# total_attendance <- total_attendance %>%
#   filter(Team != 1)
# 
# # identify columns to convert to numeric
# colstoconvert <- c(3:7)
# 
# # apply parse number to all specified columns (convert to numeric)
# total_attendance[ , colstoconvert] <- apply(total_attendance[ ,colstoconvert], 2,
#                                    function(x) parse_number(x))
# 
# # clean column names
# total_attendance <- total_attendance %>%
#   clean_names()
# 
# # write to excel file for easy access
# write.xlsx(total_attendance, "raw_data/total_attendance.xlsx")

# read in saved excel file of scraped data
total_attendance <- read.xlsx("raw_data/total_attendance.xlsx")

# scrape individual team attendance

# # create list of teams for merge later
# list_of_teams <- c("new-york-rangers", 
#                    "toronto-maple-leafs",
#                    "montreal-canadiens",
#                    "chicago-blackhawks",
#                    "boston-bruins",
#                    "los-angeles-kings",
#                    "edmonton-oilers",
#                    "philadelphia-flyers",
#                    "washington-capitals",
#                    "seattle-kraken",
#                    "detroit-red-wings",
#                    "new-york-islanders",
#                    "vancouver-canucks",
#                    "tampa-bay-lightning",
#                    "pittsburgh-penguins",
#                    "vegas-golden-knights",
#                    "new-jersey-devils",
#                    "dallas-stars",
#                    "st-louis-blues",
#                    "colorado-avalanche",
#                    "calgary-flames",
#                    "minnesota-wild",
#                    "nashville-predators",
#                    "ottawa-senators",
#                    "san-jose-sharks",
#                    "anaheim-ducks",
#                    "winnipeg-jets",
#                    "carolina-hurricanes",
#                    "columbus-blue-jackets",
#                    "buffalo-sabres",
#                    "florida-panthers",
#                    "arizona-coyotes")
# 
# # put list of teams into same format as other team
# list_of_teams <- chartr("-", " ", list_of_teams)
# list_of_teams <- str_to_title(list_of_teams)
# 
# # create list of abbreviations for URLs
# abbrevs <- c("NYR", 
#              "TOR",
#              "MTL",
#              "CHI",
#              "BOS",
#              "LAK",
#              "EDM",
#              "PHI",
#              "WSH",
#              "SEA",
#              "DET",
#              "NYI",
#              "VAN",
#              "TBL",
#              "PIT",
#              "VEG",
#              "NJD",
#              "DAL",
#              "STL",
#              "COL",
#              "CGY",
#              "MIN",
#              "NSH",
#              "OTT",
#              "SJS",
#              "ANA",
#              "WPG",
#              "CAR",
#              "CBJ",
#              "BUF",
#              "FLA",
#              "ARI")
# 
# # create list of target pages
# target_pages <- paste0("https://www.hockey-reference.com/teams/", abbrevs)
# target_pages <- paste0(target_pages, "/2023_games.html")
# 
# # create empty data frame
# teams_attendance <- data.frame(t(seq(1,18,1)))
# 
# # create iterator to add team names and abbrevs to data
# iterate = 0
# 
# # for loop that iterates through URLs, collects information, reformats and
# # appends to data frame
# for (i in target_pages) {
#   
#   iterate = iterate + 1
#   
#   url <- read_html(i)
#   
#   games <- url %>%
#     html_nodes(".right , .left , .center") %>%
#     html_text()
#   
#   for (i in seq(1, length(split(games, ceiling(seq_along(games)/16))))) {
#     div <- split(games, ceiling(seq_along(games)/16))
#     if (i == 1) {
#       vars <- div[i]
#       vars <- append(unlist(vars), "Team")
#       vars <- append(unlist(vars), "Abbreviation")
#       vars[4] <- "Away"
#       vars[8] <- "Result"
#       vars[9] <- "Extra Time"
#       colnames(teams_attendance) <- c(vars)
#     }
#     else if (i <= 83) {
#       stats <- div[i]
#       stats <- append(unlist(stats), list_of_teams[iterate])
#       stats <- append(stats, abbrevs[iterate])
#       stats <- t(data.frame(stats))
#       colnames(stats) <- c(vars)
#       teams_attendance <- rbind(teams_attendance, stats)
#     }
#     else if (i >= 84) next
#   }
# }

# write teams_attendance to excel file to stop 429 errors
# write.xlsx(teams_attendance, "raw_data/teams_attendance_sabres.xlsx")

# read in excel file with info up until sabres
partial_teams_attendance <- read.xlsx("raw_data/teams_attendance_sabres.xlsx")

# # write for loop for last two teams because error happens at 30th team
# list_of_teams_2 <- c("florida-panthers", "arizona-coyotes")
# list_of_teams_2 <- chartr("-", " ", list_of_teams_2)
# list_of_teams_2 <- str_to_title(list_of_teams_2)
# abbrevs_2 <- c("FLA", "ARI")
# target_pages_2 <- paste0("https://www.hockey-reference.com/teams/", abbrevs_2)
# target_pages_2 <- paste0(target_pages_2, "/2023_games.html")
# teams_attendance_2 <- data.frame(t(seq(1,18,1)))
# iterate = 0
# # for loop that iterates through URLs, collects information, reformats and
# # appends to data frame
# for (i in target_pages_2) {
#   
#   iterate = iterate + 1
#   
#   url <- read_html(i)
#   
#   games <- url %>%
#     html_nodes(".right , .left , .center") %>%
#     html_text()
#   
#   for (i in seq(1, length(split(games, ceiling(seq_along(games)/16))))) {
#     div <- split(games, ceiling(seq_along(games)/16))
#     if (i == 1) {
#       vars <- div[i]
#       vars <- append(unlist(vars), "Team")
#       vars <- append(unlist(vars), "Abbreviation")
#       vars[4] <- "Away"
#       vars[8] <- "Result"
#       vars[9] <- "Extra Time"
#       colnames(teams_attendance_2) <- c(vars)
#     }
#     else if (i <= 83) {
#       stats <- div[i]
#       stats <- append(unlist(stats), list_of_teams_2[iterate])
#       stats <- append(stats, abbrevs_2[iterate])
#       stats <- t(data.frame(stats))
#       colnames(stats) <- c(vars)
#       teams_attendance_2 <- rbind(teams_attendance_2, stats)
#     }
#     else if (i >= 84) next
#   }
# }

# write teams_attendance to excel file to stop 429 errors
# write.xlsx(teams_attendance_2, "raw_data/teams_attendance_coyotes.xlsx")

# read in excel file with info up until sabres
partial_teams_attendance_2 <- read.xlsx("raw_data/teams_attendance_coyotes.xlsx")

# combine partial team attendances into one data set
teams_attendance <- full_join(partial_teams_attendance, partial_teams_attendance_2)

# clean up column names, convert date column to date format, remove first row
teams_attendance <- teams_attendance %>%
  clean_names() %>%
  mutate(date = anydate(date)) %>%
  filter(opponent != 5)

# 2022-2023 promotional schedule not available and thus commented out
# see README.md file

# # grab red wings promotional schedule
# url <- read_html("https://www.nhl.com/redwings/tickets/promotions")
#   
# promos <- url %>%
#   html_nodes(".past-game b") %>%
#   html_text()
# 
# # select only date from scraped data
# promos <- sapply(str_split(promos, " vs."), function(x) x[1])
# 
# # attach year to date 
# for (i in 1:length(promos)) {
#   if (grepl(c("Oct|Nov|Dec"), promos[i]) == TRUE) {
#    promos[i] <- paste0(promos[i], " 2022")
#   }
#   else if (grepl(c("Oct|Nov|Dec"), promos[i]) == FALSE) {
#    promos[i] <- paste0(promos[i], " 2023")
#   }
#   else {
#     print("ERROR")
#   }
# }
# 
# # convert to date format
# promos <- anydate(promos)

# add opponent original six variable
teams_attendance$opponent_original_six <- "No"
teams_attendance$opponent_original_six[teams_attendance$opponent %in% 
                           c("Boston Bruins",
                             "Chicago Blackhawks", 
                             "Detroit Red Wings", 
                             "Toronto Maple Leafs", 
                             "New York Rangers", 
                             "Montreal Canadiens")] <- "Yes"

# add opponent division variable
teams_attendance$opponent_division <- "Metropolitan"
teams_attendance$opponent_division[teams_attendance$opponent %in%
                                     c("Boston Bruins",
                                       "Toronto Maple Leafs",
                                       "Tampa Bay Lightning",
                                       "Florida Panthers",
                                       "Buffalo Sabres",
                                       "Ottawa Senators",
                                       "Detroit Red Wings",
                                       "Montreal Canadiens")] <- "Atlantic"
teams_attendance$opponent_division[teams_attendance$opponent %in%
                                     c("Colorado Avalanche",
                                       "Dallas Stars",
                                       "Minnesota Wild",
                                       "Winnipeg Jets",
                                       "Nashville Predators",
                                       "St. Louis Blues",
                                       "Arizona Coyotes",
                                       "Chicago Blackhawks")] <- "Central"
teams_attendance$opponent_division[teams_attendance$opponent %in%
                                     c("Vegas Golden Knights",
                                       "Edmonton Oilers",
                                       "Los Angeles Kings",
                                       "Seattle Kraken",
                                       "Calgary Flames",
                                       "Vancouver Canucks",
                                       "San Jose Sharks",
                                       "Anaheim Ducks")] <- "Pacific"

# add opponent conference variable
teams_attendance$opponent_conference <- "Eastern"
teams_attendance$opponent_conference[teams_attendance$opponent %in%
                                       c("Colorado Avalanche",
                                         "Dallas Stars",
                                         "Minnesota Wild",
                                         "Winnipeg Jets",
                                         "Nashville Predators",
                                         "St. Louis Blues",
                                         "Arizona Coyotes",
                                         "Chicago Blackhawks",
                                         "Vegas Golden Knights",
                                         "Edmonton Oilers",
                                         "Los Angeles Kings",
                                         "Seattle Kraken",
                                         "Calgary Flames",
                                         "Vancouver Canucks",
                                         "San Jose Sharks",
                                         "Anaheim Ducks")] <- "Western"

# add home or away (location) column
teams_attendance$location <- "Home"
teams_attendance$location[teams_attendance$away == "@"] <- "Away"

# specify start time into afternoon, evening, night
teams_attendance$start_time <- "Afternoon"
teams_attendance$start_time[teams_attendance$time %in% c('5:00 PM',
                                                         '5:30 PM',
                                                         '6:00 PM',
                                                         '6:30 PM')] <- "Evening"
teams_attendance$start_time[teams_attendance$time %in% c('7:00 PM',
                                                         '7:30 PM',
                                                         '8:00 PM',
                                                         '8:30 PM',
                                                         '9:00 PM',
                                                         '9:30 PM',
                                                         '10:00 PM',
                                                         '10:30 PM')] <- "Night"

# specify streak into groups
teams_attendance$streak_grp <- "Lose 5+"
teams_attendance$streak_grp[teams_attendance$streak %in% c('L 3',
                                                           'L 4',
                                                           'L 5')] <- "Lose 3-5"
teams_attendance$streak_grp[teams_attendance$streak %in% c('L 1',
                                                           'L 2')] <- "Lose 1-2"
teams_attendance$streak_grp[teams_attendance$streak %in% c('W 1',
                                                           'W 2')] <- "Win 1-2"
teams_attendance$streak_grp[teams_attendance$streak %in% c('W 3',
                                                           'W 4',
                                                           'W 5')] <- "Win 3-5"
teams_attendance$streak_grp[teams_attendance$streak %in% c('W 6',
                                                           'W 7',
                                                           'W 8',
                                                           'W 9',
                                                           'W 13')] <- "Win 5+"

# drop away column
teams_attendance <- teams_attendance %>%
  select(-c("away"))

# create weekday column
teams_attendance$weekday <- weekdays(teams_attendance$date)

# create months column
teams_attendance$month <- months(teams_attendance$date)

# drop games played in other arenas
teams_attendance <- teams_attendance %>%
  filter(notes == "")

# drop away games
teams_attendance <- teams_attendance %>%
  filter(location == "Home")

# fix st. louis blues discrepancy
teams_attendance$team[teams_attendance$team == "St Louis Blues"] <- "St. Louis Blues"

# parse numeric variables
colstoconvert <- c(1, 5, 6, 9:11, 13)

# apply parse number to all specified columns (convert to numeric)
teams_attendance[ , colstoconvert] <- apply(teams_attendance[ ,colstoconvert], 2,
                                   function(x) parse_number(x))

# create a capacity variable for each team
teams_attendance$team <- as.factor(teams_attendance$team)
max_cap <- do.call(rbind, lapply(split(teams_attendance, teams_attendance$team), function(x) {return(x[which.max(x$att),])}))
max_cap <- max_cap %>%
  select(c('team', 'att')) %>%
  rename('capacity' = 'att')
teams_attendance <- left_join(teams_attendance, max_cap, by = 'team')

# create percent capacity variable
teams_attendance$percent_capacity <- round((teams_attendance$att / teams_attendance$capacity) * 100, 2)

# drop notes variable
teams_attendance <- teams_attendance %>%
  select(-c('notes'))

# factor to create order
teams_attendance$weekday <- factor(teams_attendance$weekday, levels = c("Monday", 
                                                                        "Tuesday", 
                                                                        "Wednesday",
                                                                        "Thursday",
                                                                        "Friday",
                                                                        "Saturday",
                                                                        "Sunday"))

teams_attendance$month <- factor(teams_attendance$month, levels = c("October", 
                                                                    "November", 
                                                                    "December",
                                                                    "January",
                                                                    "February",
                                                                    "March",
                                                                    "April"))

teams_attendance$start_time <- factor(teams_attendance$start_time, levels = c('Afternoon',
                                                                              'Evening',
                                                                              'Night'))

teams_attendance$streak_grp <- factor(teams_attendance$streak_grp, levels = c('Lose 5+',
                                                                              'Lose 3-5',
                                                                              'Lose 1-2',
                                                                              'Win 1-2',
                                                                              'Win 3-5',
                                                                              'Win 5+'))

# read in age data from hockey-reference.com
team_ages <- read.xlsx("raw_data/age_data.xlsx")

# join ages to team attendance data
teams_attendance <- left_join(teams_attendance, team_ages, by = "team")

# convert average age to numeric
teams_attendance$average_age <- as.numeric(teams_attendance$average_age)

# bin ages
teams_attendance$age_grp <- cut(teams_attendance$average_age, breaks = seq(26, 31, 1),
                                labels = c('26-27', '27-28', '28-29', '29-30', '30+'))

# create table for appendix
appendix_table <- left_join(max_cap, total_attendance[,c("team", "arena_name")], by = "team")

# write to xlsx
write.xlsx(appendix_table, "output/appendix_table.xlsx")

# aggregate team data for league-wide comparison
team_totals <- teams_attendance %>%
  group_by(., team) %>%
  summarise(total = sum(att),
            avg_att = mean(att),
            avg_pt_cap = mean(percent_capacity))

# visualization of percent capacity
league_pct_cap <- ggplot(data = teams_attendance) +
  geom_bar(data = teams_attendance,
           aes(x = team, y = percent_capacity), stat = "summary") +
  coord_flip()

league_pct_cap

# highlight red wings
highlight <- team_totals[team_totals$team == "Detroit Red Wings", ]

# visualization of percent capacity versus total attendance
league_cap_total <- ggplot(data = team_totals, aes(x = avg_pt_cap, y = total)) +
  geom_point(data = team_totals, aes(x = avg_pt_cap, y = total)) +
  geom_point(data = highlight, aes(x = avg_pt_cap, y = total), color = "red") +
  labs(title = "Average Percent Capacity vs. Total Tickets Sold",
       x = "Average Percent Capacity",
       y = "Total Tickets Sold") +
  geom_hline(yintercept = mean(team_totals$total), color = 'blue', linetype = "dashed") +
  geom_vline(xintercept = mean(team_totals$avg_pt_cap), color = 'blue', linetype = "dashed") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  geom_label_repel(data = highlight, color = "red", aes(label = paste(team))) +
  scale_y_continuous(labels = scales::comma)

league_cap_total

# save visualization
ggsave(filename = "output/league_cap_total.pdf")

# create boxplot by day of week and month for entire league
league_dow <- ggplot(data = teams_attendance, aes(x = weekday, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Weekday",
       x = "Weekday",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_dow

# save visualization
ggsave(filename = "output/league_dow.pdf")

# create boxplot by month for entire league
league_month <- ggplot(data = teams_attendance, aes(x = month, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Month",
       x = "Month",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_month

# save visualization
ggsave(filename = "output/league_month.pdf")

# create boxplot by start time for entire league
league_start <- ggplot(data = teams_attendance, aes(x = start_time, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Start Time",
       x = "Start Time",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_start

# save visualization
ggsave(filename = "output/league_start.pdf")

# create boxplot by streak for entire league
league_streak <- ggplot(data = teams_attendance, aes(x = streak_grp, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Streak",
       x = "Streak",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_streak

# save visualization
ggsave(filename = "output/league_streak.pdf")

# create boxplot by opponent (original 6) for entire league
league_og_6 <- ggplot(data = teams_attendance, aes(x = opponent_original_six, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Opponent in Original Six",
       x = "Opponent In Original Six",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_og_6

# save visualization
ggsave(filename = "output/league_og_6.pdf")

# create boxplot by opponent (conference) for entire league
league_conf <- ggplot(data = teams_attendance, aes(x = opponent_conference, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Opponent Conference",
       x = "Conference",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_conf

# save visualization
ggsave(filename = "output/league_conf.pdf")

# create boxplot by opponent (division) for entire league
league_div <- ggplot(data = teams_attendance, aes(x = opponent_division, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Percent Capacity by Opponent Division",
       x = "Division",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_div

# save visualization
ggsave(filename = "output/league_div.pdf")

# create boxplot by team age for entire league
league_age <- ggplot(data = teams_attendance, aes(x = age_grp, y = percent_capacity)) +
  geom_boxplot(fill = "light blue") +
  labs(title = "League-Wide Average Percent Capacity by Average Team Age",
       x = "Average Age",
       y = "Average Percent Capacity") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25))

league_age

# save visualization
ggsave(filename = "output/league_age.pdf")

# run linear regression for league-wide factors
# create test and train data sets
set.seed(1234)
training_test <- c(rep(1, length = trunc((2/3) * nrow(teams_attendance))), 
                   rep(2, length = (nrow(teams_attendance) - trunc((2/3) * nrow(teams_attendance)))))
teams_attendance$training_test <- sample(training_test)
teams_attendance$training_test <- factor(teams_attendance$training_test,
                                         levels = c(1, 2),
                                         labels = c('TRAIN', 'TEST'))
teams_attendance.train <- subset(teams_attendance, training_test == 'TRAIN')
print(str(teams_attendance.train))
teams_attendance.test <- subset(teams_attendance, training_test == 'TEST')
print(str(teams_attendance.test))

# create and test model
my.model <- {percent_capacity  ~ team + opponent_original_six + opponent_conference + opponent_division +
    weekday + month + start_time + streak_grp + age_grp}
train.model.fit <- lm(my.model, data = teams_attendance.train)
print(summary(train.model.fit))
teams_attendance.train$predict_capacity <- predict(train.model.fit)
teams_attendance.test$predict_capacity <- predict(train.model.fit, newdata = teams_attendance.test)
cat("\n", "Proportion of Test Set Variance Accounted for: ", 
    round((with(teams_attendance.test, cor(percent_capacity, predict_capacity)^2)), digits = 3),
    "\n", sep = "")

# create red wings data subset
red_wings <- teams_attendance[teams_attendance$team == "Detroit Red Wings", ]

# # create promo variable for red wings
# red_wings$promo <- ifelse(red_wings$date %in% promos, 'Yes', 'No')

# re-create visualizations for Red Wings
rw_dow <- ggplot(data = red_wings, aes(x = weekday, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Weekday",
       x = "Weekday",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_dow

# save visualization
ggsave(filename = "output/rw_dow.pdf")

rw_month <- ggplot(data = red_wings, aes(x = month, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Month",
       x = "Month",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_month

# save visualization
ggsave(filename = "output/rw_month.pdf")

rw_starttime <- ggplot(data = red_wings, aes(x = start_time, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Start Time",
       x = "Start Time",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_starttime

# save visualization
ggsave(filename = "output/rw_starttime.pdf")

rw_streak <- ggplot(data = red_wings, aes(x = streak_grp, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Streak",
       x = "Streak",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_streak

# save visualization
ggsave(filename = "output/rw_streak.pdf")

rw_og_six <- ggplot(data = red_wings, aes(x = opponent_original_six, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Opponent in Original Six",
       x = "Opponent in Original Six",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_og_six

# save visualization
ggsave(filename = "output/rw_og_six.pdf")

rw_conference <- ggplot(data = red_wings, aes(x = opponent_conference, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Opponent Conference",
       x = "Opponent Conference",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_conference

# save visualization
ggsave(filename = "output/rw_conference.pdf")

rw_division <- ggplot(data = red_wings, aes(x = opponent_division, y = att)) +
  geom_boxplot(fill = "red") +
  labs(title = "Red Wings Attendance by Opponent Division",
       x = "Opponent Division",
       y = "Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma)

rw_division

# save visualization
ggsave(filename = "output/rw_division.pdf")

# rw_promo <- ggplot(data = red_wings, aes(x = promo, y = att)) +
#   geom_boxplot(fill = "red") +
#   labs(title = "Red Wings Attendance by Promotional Games",
#        x = "Promotional Game",
#        y = "Attendance") +
#   theme_minimal() + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
#   scale_y_continuous(labels = scales::comma)
# 
# rw_promo

# run linear regression for red wings factors
# drop training_test variable to recreate for just red wings
red_wings <- red_wings %>% 
  select(-c('training_test'))
# create test and train data sets
set.seed(1234)
training_test <- c(rep(1, length = trunc((2/3) * nrow(red_wings))), 
                   rep(2, length = (nrow(red_wings) - trunc((2/3) * nrow(red_wings)))))
red_wings$training_test <- sample(training_test)
red_wings$training_test <- factor(red_wings$training_test,
                                         levels = c(1, 2),
                                         labels = c('TRAIN', 'TEST'))
red_wings.train <- subset(red_wings, training_test == 'TRAIN')
print(str(red_wings.train))
red_wings.test <- subset(red_wings, training_test == 'TEST')
print(str(red_wings.test))

# create and test model
my.model <- {att ~ opponent_original_six + opponent_conference + opponent_division +
    weekday + month + start_time + streak_grp } # previously '+ promo' at end
train.model.fit <- lm(my.model, data = red_wings.train)
print(summary(train.model.fit))
red_wings.train$predict_att <- predict(train.model.fit)
red_wings.test$predict_att <- predict(train.model.fit, newdata = red_wings.test)
cat("\n", "Proportion of Test Set Variance Accounted for: ", 
    round((with(red_wings.test, cor(att, predict_att)^2)), digits = 3),
    "\n", sep = "")

# allow model to train on entire data set
my.model.fit <- lm(my.model, data = red_wings)
print(summary(my.model.fit))
print(anova(my.model.fit))
plot(my.model.fit)
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

red_wings$predicted_att <- predict(my.model.fit)

# create graphic to visually evaluate how predictive model performs
predict_vs_actual <- ggplot(data = red_wings, aes(x = att, y = predicted_att)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  labs(title = "Actual vs Predicted Attendance",
       x = "Actual Attendance",
       y = "Predicted Attendance") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.25)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

predict_vs_actual

# save visualization
ggsave(filename = "output/predict_vs_acutal.pdf")

# create separate df of model coefficents 
require(broom)
coeffs <- data.frame(tidy(my.model.fit))

# write to excel file to put in report
write.xlsx(coeffs, 'output/model_coeffs.xlsx')

