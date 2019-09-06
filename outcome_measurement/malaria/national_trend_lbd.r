library(raster)
library(data.table)

dir = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/Pre-2019 Publication/Africa Cube Public Data/ACT_use_rasters/rasters/rasters/'
files = paste0(dir, files)

popFile = "J:/WORK/11_geospatial/01_covariates/10_Processed_WorldPop/africa/africa_pop_0005F_2015_5km.tif"

shapeFileCOD = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/map/../../../mapping/cod/COD_adm0.shp'
map = shapefile(shapeFileCOD)

for(y in seq(2000, 2015)) {
	data = raster(paste0(dir, y, '.ACT.stable.tif'))
	data = data.table(as.data.frame(data, xy=TRUE))
	if (y==2000) val = data[round(x)==25 & round(y)==-5][1][[3]]
	if (y>2000) val = c(val, data[round(x)==25 & round(y)==-5][1][[3]])
}

data = data.table(year=seq(2000,2015), act_coverage=val)

ggplot(data[year>=2005], aes(y=act_coverage*100, x=year)) + 
	geom_line() + 
	scale_x_continuous(breaks = seq(2005, 2015, by = 1)) + 
	labs(title='Estimated Population-Level Treatment Coverage', 
		y='Under-5 ACT Coverage (Percentage of Cases of Fever)', x='', 
		caption='Bhatt S et al. The effect of malaria control on Plasmodium falciparum in Africa between 2000 and 2015. Nature: 2015') + 
	theme_bw()
		
		