#$ -S /bin/sh

#ls /ihme/* 1>/dev/null
#ls /home/j 1>/dev/null

env_mkl="$1"
run_file="$2"; shift 2

# echo SINGULARITYENV_MKL_NUM_THREADS="${env_mkl}" singularity exec /share/singularity-images/health_fin/forecasting/best.img /usr/local/bin/Rscript "$run_file" "$@"
SINGULARITYENV_OMP_NUM_THREADS="${env_mkl}" SINGULARITYENV_MKL_NUM_THREADS="${env_mkl}" singularity exec /share/singularity-images/health_fin/forecasting/best.img Rscript "$run_file" "$@"

EXITCODE=$?

## Put the current date into a variable and report it before we exit.
GSITSENDDATE=`/bin/date`
echo "**** JOB DONE, EXITING ${EXITCODE} AT $GSITSENDDATE"
##

## Exit with R exit code
exit ${EXITCODE}
