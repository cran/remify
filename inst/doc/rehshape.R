## -----------------------------------------------------------------------------
library(remify)
data(randomREH) 
reh_remify <- remify::remify(edgelist = randomREH$edgelist, model = "tie")
reh_remify

## -----------------------------------------------------------------------------
reh_rem <- remify::rehshape(data = reh_remify, 
                      output_format = c("relevent-rem"))
names(reh_rem)                     

## -----------------------------------------------------------------------------
reh_rem.dyad <- remify::rehshape(data = reh_remify, 
                      output_format = c("relevent-rem.dyad"))
names(reh_rem.dyad)                     

