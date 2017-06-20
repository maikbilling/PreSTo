### Readme ### Raingenerator Version 1.0 by Maik Billing

For a detailed description see: 

Documentation: raingenerator.pdf


A short "how to use":

0. Install packages: "beepr", "VGAM", "zoo", "fitdistrplus", "MASS", "ggplot2"

1. run sampple.code.R to take a look at some sample output.

2. open "mainfile.R"

3. change path to your directory (that contains the R files) 

4. change number of years to simulate

5. load your data in a data frame with 4 columns named:
	- "year" contains the year
	- "month" contains the month 
	- "day" contains the day of the month
	- "pre" contains the daily precipitation amount

6. run "estimate.models()" to get the best fitting models for wetness and amount models

7. run "estimate.bias()" (takes quite long, let run preferably over night, has to be done for a dataset ONLY ONCE!)
	... or take bias you already found out...

8. load climate change vectors for scenario modeling (each entry (12 entries) of the vector contains the mean change in percipitation in percent for a month)

9. run raingen() to generate data
	- choose mode, sound, climate change vector
	- use models, bias and nyears from the steps 3, 5 and 6

10. run raingen.summary() to take a look at your generated data

11. save your generated data (raingen.list$gen.data)

Note: "one.function.R" combines all functions of the generator in a single one.

In case of any questions or comments please don not hesitate writing me: maik.billing@gmx.de
Thank you!