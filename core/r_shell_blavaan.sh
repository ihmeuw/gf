#$ -S /bin/sh
singularity exec /share/singularity-images/health_fin/forecasting/best.img Rscript <$1 --no-save $@
