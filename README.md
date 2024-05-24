# Spatio-temporal modelling of the Brazilian wildfires: Codes and Datasets

Directory created to share the main files and the codes used in the construction of the data tables of the article "SBayesian spatio-temporal modeling of the Brazilian wildfire hotspots: The influence of human and meteorological variables" of the journal "Scientific Reports".

Abstract - Wildfires are among the most common natural disasters in many world regions and actively impact life quality. These events have become frequent due to climate change, other local policies, and human behavior. Fire spots are areas where the temperature is significantly higher than in the surrounding areas, and they are often used to identify wildfires. This study considers the historical data with the geographical locations of all the ``fire spots'' detected by the reference satellites covering the Brazilian territory between January 2011 and December 2022, comprising more than 2.2 million fire spots. This data was modeled with a spatio-temporal generalized linear model for areal unit data, whose inferences about its parameters are made in a Bayesian framework and use meteorological variables (precipitation, air temperature, humidity, and wind speed) and a human variable  (land-use transition and occupation) as covariates. The change in land use from green areas to farming significantly impacts the number of fire spots for all six Brazilian biomes.

## Files

### Control.csv

The data table "Control.csv" is a control file used to link the databases. It contains the name of the Brazilian municipalities (with and without accents and ç), the respective IBGE code for each municipality, the state to which each municipality belongs, and the biome present in each of the municipalities. In the case of biomes, the same municipality may have two or more different biomes in its territory, and for this reason, some municipalities appear more than once in the base.

### Fires_Biomes.csv

The data table "Fires_Biomes.csv" presents the number of daily fires per Brazilian biome over the 10 years of studies.

### Fires_Mun_PartA.csv and Fires_Mun_PartB.csv

The data tables "Fires_Mun_PartA.csv" and "Fires_Mun_PartB.csv" present the number of daily outbreaks per Brazilian municipality over the 10 years of studies. In this case there are two tables just by the size limit of github which is 25MB.

### Fires_Month_Biomes.csv

The data table "Fires_Month_Biomes.csv" presents the number of monthly fires per Brazilian biome over the 10 years of study.

### Fires_Month_Mun.csv

The data table "Fires_Month_Mun.csv" presents the number of monthly outbreaks per Brazilian municipality over the 10 years of study.

### Export.R

The "Export.R" file presents the code used to transform the data collected from the INPE website https://queimadas.dgi.inpe.br/queimadas/bdqueimadas/ into daily data. Originally, each row of the database represents a source of fire, however, in order to work with these data, it was necessary to group daily, and later monthly, to use in the model.

### Export_Month.R

The file "Export_Month.R" presents only the steps necessary to generate the bases with the monthly data from the daily data.
