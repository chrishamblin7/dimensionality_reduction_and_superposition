if(!require("pacman")) {install.packages("pacman"); require("pacman")} 
p_load(
       "RcppCNPy",
       "reticulate")

#get numpy data into R
np <- import("numpy")

files <- list.files('./numpy_data/')
files
# data reading
all_data <- vector(mode="list", length=length(files))
names(all_data) <- files

for (file in files) {
  mat <- np$load(paste("numpy_data",file,sep='/'))
  all_data[[file]] <- mat
  }

all_data
save(all_data, file = "all_data.Rdata")
