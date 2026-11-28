# Quiet TensorFlow's C++ logging for the keras3 tests (which only run off CRAN).
# Do not load keras or tensorflow here: their `.onLoad()` hooks ask reticulate
# to find or provision Python, which prints messages and runs `uv` on machines
# (like CRAN's) that do not have TensorFlow.
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = '3')
