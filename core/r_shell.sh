#$ -S /bin/sh
singularity exec /share/singularity-images/health_fin/forecasting/best_new.img R <$1 --no-save $@
