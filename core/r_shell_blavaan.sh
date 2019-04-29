#$ -S /bin/sh
/share/singularity-images/health_fin/forecasting/best_new.img Rscript <$1 --no-save $@
