
parallel_wrapper = function(func, ...) {
  # initializing parallel process
  numCores = detectCores()
  cl = makeCluster(floor(numCores * 0.8))
  registerDoParallel(cl)
  # main parallel computing
  res = func(...)
  # shut down the workers
  stopCluster(cl)
  # garbage collection
  invisible(gc())
  return(res)
}
