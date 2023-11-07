## -----------------------------------------------------------------------------
library(remify) # loading library
data(randomREH) # loading data
names(randomREH) # objects inside the list 'randomREH'

## -----------------------------------------------------------------------------
head(randomREH$edgelist)

## -----------------------------------------------------------------------------
randomREH$actors

## -----------------------------------------------------------------------------
randomREH$types

## -----------------------------------------------------------------------------
randomREH$origin

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[1]]$time # start and stop time point defining the time window of interest

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[1]]$dyad # dyads to be removed from the time points defined by the interval in `time`

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[2]]$time # start and stop time point defining the time window of interest

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[2]]$dyad # dyads to be removed from the time points defined by the interval in `time`

## -----------------------------------------------------------------------------
edgelist_reh <- remify(edgelist = randomREH$edgelist,
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # model with waiting times
                    model = "tie", # tie-oriented modeling
                    actors = randomREH$actors,
                    types = randomREH$types, 
                    riskset = "manual",
                    origin = randomREH$origin,
                    omit_dyad = randomREH$omit_dyad)
                    

## -----------------------------------------------------------------------------
names(edgelist_reh)

## -----------------------------------------------------------------------------
edgelist_reh$M

## -----------------------------------------------------------------------------
edgelist_reh$N

## -----------------------------------------------------------------------------
edgelist_reh$C

## -----------------------------------------------------------------------------
edgelist_reh$D

## -----------------------------------------------------------------------------
head(edgelist_reh$intereventTime)

## -----------------------------------------------------------------------------
head(edgelist_reh$edgelist)

## -----------------------------------------------------------------------------
edgelist_reh$omit_dyad$riskset[,1:10] # printing out the risk set modifications of only the first 10 columns (dyads). A total number of 2 modifications of the risk set are observed (by row)

## -----------------------------------------------------------------------------
edgelist_reh$omit_dyad$time[1:10] # printing out the first 10 time points. We can see that in none of the 10 time points any modification takes place (-1)

## -----------------------------------------------------------------------------
edgelist_reh <- remify(edgelist = randomREH$edgelist,
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # model with waiting times
                    model = "tie", # tie-oriented modeling
                    actors = randomREH$actors,
                    types = randomREH$types, 
                    riskset = "active",
                    origin = randomREH$origin)
                    

## -----------------------------------------------------------------------------
names(attributes(edgelist_reh))

## -----------------------------------------------------------------------------
attr(edgelist_reh, "names")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "class")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "with_type")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "weighted")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "directed")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "ordinal")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "model")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "riskset")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "dictionary")

## -----------------------------------------------------------------------------
str(attr(edgelist_reh, "origin")) # printing out only the str() of the attribute since the data.frame `value` is large

## -----------------------------------------------------------------------------
attr(edgelist_reh, "ncores")

## -----------------------------------------------------------------------------
attr(edgelist_reh, "dyadID")[[1]] # printing out dyads ID's observed at the first time point

## -----------------------------------------------------------------------------
attr(edgelist_reh, "actor1ID")[[1]] # printing out the actor1's/senders ID's observed at the first time point

## -----------------------------------------------------------------------------
attr(edgelist_reh, "actor2ID")[[1]] # printing out the actor2's/receivers ID's observed at the first time point

## -----------------------------------------------------------------------------
attr(edgelist_reh, "typeID")[[1]] # printing out the types ID's observed at the first time point

## -----------------------------------------------------------------------------
attr(edgelist_reh, "dyadIDactive")[[1]] # printing out the ID's of the active dyads at the first time point

## -----------------------------------------------------------------------------
summary(edgelist_reh) # same output as `print(edgelist_reh)` or just `edgelist_reh`

## -----------------------------------------------------------------------------
dim(edgelist_reh)

## -----------------------------------------------------------------------------
getRiskset(x = edgelist_reh)$riskset[,1:10] # printing out the risk set modifications of only the first 10 columns (dyads). A total number of 2 modifications of the risk set are observed (by row)

## -----------------------------------------------------------------------------
getActorName(x = edgelist_reh, actorID = c(1,13,20))

## -----------------------------------------------------------------------------
getTypeName(x = edgelist_reh, typeID = c(1,3))

## -----------------------------------------------------------------------------
getDyad(x = edgelist_reh, dyadID = c(1,10,100), active = FALSE)

## -----------------------------------------------------------------------------
getActorID(x = edgelist_reh, actorName = c("Michaela","Alexander","Lexy"))

## -----------------------------------------------------------------------------
getTypeID(x = edgelist_reh, typeName = "cooperation")

## -----------------------------------------------------------------------------
getDyadID(x = edgelist_reh, actor1 = "Alexander", actor2 = "Charles", type = "cooperation")

## -----------------------------------------------------------------------------
op <- par(no.readonly = TRUE)
par(mai=rep(0.8,4), cex.main=0.9, cex.axis=0.75)
plot(x=edgelist_reh,which=1,n_intervals=13) # histogram of inter-event times
plot(x=edgelist_reh,which=2,n_intervals=13) # tile plot (counts of dyadic events) with in-/out- degree of actors on the sides
plot(x=edgelist_reh,which=3,n_intervals=13) # (normalized) in-degree and out-degree of actors
plot(x=edgelist_reh,which=4,n_intervals=13) # per time interval: number of events, proportion of observed dyads, proportion of active senders and active receivers
plot(x=edgelist_reh,which=5,n_intervals=13,igraph.edge.color="#cfcece",igraph.vertex.color="#7bbfef") # networks
par(op)

## -----------------------------------------------------------------------------
edgelist_reh <- remify(edgelist = randomREH$edgelist,
                    directed = FALSE, # events are now considered undirected
                    model = "tie")    
#op <- par(no.readonly = TRUE)
#par(mai, rep(0.8,4), cex.main=0.9, cex.axis=0.75)           
#plot(x=edgelist_reh,which=1:5,n_intervals=13)
#par(op) 

