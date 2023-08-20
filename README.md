Included in this repo is a project examining how various factors impact ticket sales across the NHL and for the Detroit Red Wings. Said factors were then used to create a linear regression model to predict fan attendance of games throughout the season. 

Be sure to check out the write-up in 'documentation' for a detailed report of data acquisition, methods, analysis, and results! The write-up for this project is unique in that it evaluates literature from the MLB suggesting factors that may impact fan attendance, applies them to the NHL, and considers how a dynamic pricing model would be beneficial to the Detroit Red Wings. 

Data used in the analysis was provided by hockey-reference.com and nhl.com/redwings/tickets/promotions. 

Note that due to the hockey-reference.com's reuse of the attendance page URL season-to-season, attendance numbers for the 2022-2023 NHL season are no longer available to scrape. Thus, the saved files with said attendance data are included in 'raw_data' for replicability. Additionally, when scraping individual team attendance data from hockey-reference.com, a 429 error capped the initial scrape at 30 of the 32 teams, so a separate, second scrape was performed to acquire the final 2 teams data. Again, to aid replicability of this project, both saved files with the scraped data were included in 'raw_data.'

The same issue of reuse of URL was observed with the Detroit Red Wings promotional schedule webpage, but no file was created to save the 2022-2023 season promotional schedule so that aspect of the analysis cannot be replicated. However, the analysis can be re-run with the promotional schedule code on the 2023-2024 season attendance data when it becomes available.
