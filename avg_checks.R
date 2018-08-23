#---
#title: function for computing average yield from nearby checks plots
#author: agustin
#---

# Sample data: 6 plots 
set.seed(1)
yld <- data.frame(
  Id = 1:10,
  trt1check = c('C1', 'V1', 'V2', 'V3', 'C1', 'V4', 'V5', 'V6', 'C1', 'V7'),
  trt2checks = c('C1', 'C2', 'V1', 'V2', 'C1', 'C2', 'V3', 'V4', 'C1', 'C2'),
  yield = round(runif(n = 10, min = 10, max = 30), 0)
)



# The function
avg_ctrl <- function(Id, trt, y, ctrl) {
  
  # number of controls
  n <- length(ctrl)
  
  # Results bucket
  res <- numeric(length = length(Id))
  
  for (i in 1:length(Id)) {
    
    # Get controls plot ids
    Id_ctrls <- Id[trt %in% ctrl]
    
    # Find distances from controls
    dif_ids <- Id_ctrls - i
    
    # distancia a n los controles más cercanos
    ref_vector <- c(sort(dif_ids[dif_ids > 0])[1:n], sort(dif_ids[dif_ids < 0], dec = T)[1:n])
    
    Ids <- if (trt[i] %in% ctrl) {
      Id[i]
    } else {
      Id_ctrls[which(dif_ids %in% ref_vector)]
    }
    res[i] <- mean(y[Ids])
  }
  res
}

# Apply
library(dplyr)
mutate(yld, rend_ctrl = avg_ctrl(Id, trt2checks, yield, ctrl = c("C1", "C2")))
  