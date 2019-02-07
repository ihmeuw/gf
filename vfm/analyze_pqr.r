# ----------------------------------------------
# David Phillips
# 
# 12/18/2018
# Prep PQR data for analysis
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
# --------------------


# -----------------------------------------------------
# Settings and parameters

# country to graph ("all" will do all eight PCE countries
c = 'all'
# -----------------------------------------------------


# -----------------------------------------------------
# Files and directories

# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# input file
dir = paste0(j, '/Project/Evaluation/GF/vfm/')
inFile = paste0(dir, 'outputs/pqr_data_prepped.rdata')

# output files
outFile = paste0(dir, 'visualizations/unit_cost_analysis.pdf')
if (c!='all') outFile = gsub('.pdf',paste0('_', c,'.pdf'),outFile)
# -----------------------------------------------------


# -------------------------------------------
# Load/set up data

# load
data = readRDS(inFile)

# compute difference and ratio
data[, ratio:=unit_cost/mean_reference]
data[, difference:=unit_cost-mean_reference]

# subset to country if not "all"
if (c!='all') data = data[country==c]
# -------------------------------------------


# ------------------------------------------------------------------------
# Print numbers

# cases that overpaid by a lot
displayVars = c('country','product','description','grant_number','year',
		'unit_cost','mean_reference','median_reference', 'ratio','difference')
data[ratio > 1.5, displayVars, with=F]

# cases that got a good deal
data[ratio < 0.5, displayVars, with=F]

# averages
data[type=='arv', .(ratio=mean(ratio), difference=mean(difference)), by=c('country','year')][order(country, year)]

# trend
coefs = do.call('rbind', lapply(unique(data$product), function(p) {
	coef = coef(lm(unit_cost~year, data[product==p]))[2]
	return(data.table(p,coef))
}))

# trend in economy
coefs = do.call('rbind', lapply(unique(data$product), function(p) {
	coef = coef(lm(difference~year, data[product==p]))[2]
	return(data.table(p,coef))
}))

# trend in economy by country
coefs = do.call('rbind', lapply(unique(data$product), function(p) {
	coef = coef(lm(difference~year, data[product==p]))[2]
	return(data.table(p,coef))
}))

# suppliers
tmp = data[, sum(total_product_cost)/1000000, by=c('supplier','type')][order(type,-V1)]
tmp[, total:=sum(V1), by='type']
tmp[, pct:=V1/total*100]
tmp
summary(lm(difference~supplier, data=data))
summary(lm(difference~supplier+product+year, data=data))

# drugs
data[year==2018, sum(total_product_cost)/1000000, by=c('type')][order(type,-V1)]
data[, sum(total_product_cost)/1000000, by=c('product','type')][order(type,-V1)]

# percent of ARVs that are the top 3
num = sum(data[type=='arv', sum(total_product_cost), by='product'][order(-V1)][1:3,V1])
den = sum(data[type=='arv', sum(total_product_cost), by='product'][order(-V1)]$V1)
num/den
num
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Graph

# drop uncommon commodities
data[, n:=.N, by='product']
data = data[n>6]

# shorten names
data[country=='Congo (Democratic Republic)', country:='DRC']
data[, product:=str_wrap(product,26)]

# set up to graph
breaks = rev(seq(2018, 2008, by=-3))
colors = brewer.pal(length(unique(data$country)), 'Paired')
names(colors) <- unique(data$country)[order(unique(data$country))]
if (length(unique(data$country))==1) colors = 'black'
pointsize = .75
titlesize = 14
stripsize = 8
ysize = 12

# graph ARVs
arvGraph = ggplot(data[type=='arv'], aes(y=difference, x=year, color=country)) + 
	geom_hline(yintercept=0, color='grey35') + 
	geom_smooth(aes(y=difference, x=year), inherit.aes=FALSE, color='black') + 
	geom_point(size=pointsize) + 
	facet_wrap(~product, scales='free_y') + 
	scale_x_continuous(breaks=breaks) + 
	scale_color_manual('', values=colors) + 
	labs(title='Economy in ARV Procurement', x='', caption='All values are in USD',
		y='Unit Price Secured by PR\nMinus Global Reference Price') + 
	theme_bw() + 
	theme(plot.title=element_text(size=titlesize), axis.title.y=element_text(size=ysize), 
		strip.text=element_text(size=stripsize), legend.position=c(0.8, 0.25),
		legend.background = element_rect(fill=NA, colour=NA)) 

# graph TB Meds
tbGraph = ggplot(data[type=='tb_meds'], aes(y=difference, x=year, color=country)) + 
	geom_hline(yintercept=0, color='grey35') + 
	geom_smooth(aes(y=difference, x=year), inherit.aes=FALSE, color='black') + 
	geom_point(size=pointsize) + 
	facet_wrap(~product, scales='free_y') + 
	scale_x_continuous(breaks=breaks) + 
	scale_color_manual('', values=colors) + 
	labs(title='Economy in TB Medication Procurement', x='', caption='All values are in USD',
		y='Unit Price Secured by PR\nMinus Global Reference Price') + 
	theme_bw() + 
	theme(plot.title=element_text(size=titlesize), axis.title.y=element_text(size=ysize), 
		strip.text=element_text(size=stripsize)) 

# graph antimalarials
malGraph = ggplot(data[type=='antimalarials'], aes(y=difference, x=year, color=country)) + 
	geom_hline(yintercept=0, color='grey35') + 
	geom_smooth(aes(y=difference, x=year), inherit.aes=FALSE, color='black') + 
	geom_point(size=pointsize) + 
	facet_wrap(~product, scales='free_y') + 
	scale_x_continuous(breaks=breaks) + 
	scale_color_manual('', values=colors) + 
	labs(title='Economy in Antimalarial Procurement', x='', caption='All values are in USD',
		y='Unit Price Secured by PR\nMinus Global Reference Price') + 
	theme_bw() + 
	theme(plot.title=element_text(size=titlesize), axis.title.y=element_text(size=ysize), 
		strip.text=element_text(size=stripsize)) 
# ------------------------------------------------------------------------


# --------------------------------
# Save
pdf(outFile, height=5.5, width=9)
arvGraph
tbGraph
malGraph
dev.off()
# --------------------------------
