# test script for passing arguments to an array job
task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

args = commandArgs()

print(task_id)
print(args)

