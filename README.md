# Resource Partitioning by Bumble Bees

Exercise and data for BI 063 students to study resource partitioning.

Students will use data from Pyke 1982 and other sources to analyze resource partitioning among four species of *Bombus* bumble bees: *B. appositus*, *B. kirbyellus*, *B. bifarius*, and *B. frigidus*.


#### Goals
1. Simulate data from Macior (1974; if available) and Pyke (1982) to calculate mean proboscis lengths for six species of Bombus. (Done)
	

1. Use data from Pkye et al. (2012) to determine relationship between flower corolla size and bumble bee proboscis length.  (Done)

3. Use data from Pyke (1982) to show resource partitioning between four bumble bee species. (Done)

	Data to recreate the bumble bee partitioning figure from [this Nature Scitable article](https://www.nature.com/scitable/knowledge/library/resource-partitioning-and-why-it-matters-17362658/) is taken from [Pkye (1982).](https://www.jstor.org/stable/1938970)

4. Use data from Pyke (1982) to show differences in altitude for the five bumble species. (Done)

#### Analysis steps

* Students will calculate mean, standard deviation, and standard error for all six species. They will either make a histogram by hand for 1-2 species, or use software to make histograms for all six species.

	Data file: bombus\_queens\_proboscis\_lengths.csv
	
	**To do:** consider writing a Shiny app for students to upload and plot histograms.
	
* Students make a scatter plot showing relationship between proboscis length and corolla length. (Is this best position for this step?)

	Data file: proboscis\_corolla\_lengths.csv
	
* Students sum the number of visits by each *Bombus* species for four classes of corolla length. They then make a column graph to show the number of visits for each species. This will mimic the Nature Scitable figure linked above. 

	Data file: bombus\_flower\_visits.csv
	
	**To do:** Data file currently has all species. Remove *B. flavifrons* to simplify. Still have to decide whether to keep *B. sylvicola* for further simplification. 

*	Students plot relative abundances vs sites from two transects along an altitudinal gradient to show that *Bombus* species with similar proboscis lengths occur at different altitudes.

	Data file: transects.xlsx (need csv files?)
	
	
**Note:* All csv files could be combined into a single Excel file for student use. Should I do this?

### Literature Cited

Macior, L. W. 1974. Pollination ecology of the front range of the Colorado Rocky Mountains. Melanderia 15.

Pkye, G. H. 1982. Local geographic distributions of bumblebees near Crested Butte, Colorado: competition and community structure. Ecology 63: 555–573.

Pyke, G. H., D. W. Inouye, and J. D. Thompson. 2012. Local geographic distributions of bumblebees near Crested Butte, Colorado: competition and community structure revisited. Environmental Entolmology 41: 1332–1349. 