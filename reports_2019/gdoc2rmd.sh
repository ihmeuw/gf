
echo "Starting gdoc2rmd..."

cd("C:/Users/elineb/Documents/gf/reports_2019")
echo "Converting plain text to r markdown using java processor"
java gdoc2rmd

echo "R markdown output created - 'gdoc2rmd_out.rmd'"
C:/Users/elineb/AppData/Local/CONTIN~1/ANACON~1/envs/rstudio/lib/R/bin/x64/R.exe do.call(source('run_r.R'))

echo "Output PDF created." 