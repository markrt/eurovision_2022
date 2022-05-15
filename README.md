# Eurovision 2022

This is a repo hosting data and analysis from Eurovision 2022. The basic idea is that I suspect loads of people will be wanting to play around with Eurovision data both before and after the Grand Final, and there's no point everyone converting PDFs into CSVs (etc) separately when we could just do it all in one go.

My code's all in R but hopefully the files I produce will be usable regardless of language.

The most useful files for most people will, I suspect, be eurovision_2022_clean.csv and eurovision_2022_clean_with_points.csv. The latter is just the former, but with columns for points (rather than just rankings) included. If you want it in Excel format with a sheet for each country's votes, you can find that in results_2022.

Those files comprise copied-and-pasted data from pages like [this one](https://eurovision.tv/event/turin-2022/grand-final/results/albania), so you've got the overall jury ranking and televote ranking on a country-to-country basis, individual jury scores, that sort of thing.

Please note that there are six countries where the data quality is different: Azerbaijan, Georgia, Montenegro, Poland, Romania, and San Marino. Full results are yet to be published for these countries due to "irregularities". I'm staying out of it. But depending on your analysis you'll have to think about how you're dealing with missing data.

A few other scripts:

import_convert_data.R gets you from the Excel sheet to the CSVs. 

jury_rank_distribution is an illustration of the jury rankings each country received.

eurovision_spotify_data_2022.csv contains metadata from Spotify's "Eurovision 2022" playlist as of the afternoon of 2022.05.14. So if you want to know what key something's in or the BPM, you can get that here. You can also get estimates of things like "liveness", "acousticness", and so on. 

spotify features 2022.R is a script to produce that. To run your own, you'll need access to the Spotify API: this was straightforward when I set it up years ago but I can't comment on what it's like now.

semi qualifiers info.R adds variables for whether or not countries qualified, and which semi-final they were in.
