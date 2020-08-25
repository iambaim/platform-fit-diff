install.packages(c("iterators", "ggplot2", "gridExtra", "rlang", "scales"))
install.packages(c("FLCore", "ggplotFL", "FLBRP", "FLash"), repos="http://flr-project.org/R")

library(FLCore)
library(FLBRP)

# Predict recruitment
predict_recruitment <- function(stk, model) {

  # Create the model
  newFL <- fmle(as.FLSR(stk, model=model))

  # Get the latest ssb
  ssb <- tail(ssb(stk), 1)

  # Predict
  prm <- params(newFL)

  s <- a <- prm[[1]] # Extract 'a' parameter

  if(length(prm) > 1)
    R0 <- b <- params(newFL)[[2]] # Extract 'b' parameter

  if(length(prm) > 2)
    v <- c <- params(newFL)[[3]] # Extract 'c' parameter

  # Use the model formula in FLCore
  rec <- eval(parse(text = as.character(model()$model[3])))

  return(list(srmodel = newFL, rec = as.numeric(rec)))
}

stk1 <- readRDS("stk.2002.linux.rds")
stk2 <- readRDS("stk.2002.macos.rds")

pr1 <- predict_recruitment(stk1, shepherd)
pr2 <- predict_recruitment(stk2, shepherd)

print(pr1$rec)
print(pr2$rec)

print(all.equal(stk1, stk2, tolerance=0))
