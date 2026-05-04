## -----------------------------------------------------------------------------
library(remify) # loading library
data(randomREH) # loading data
names(randomREH) # objects inside the list 'randomREH'

## -----------------------------------------------------------------------------
head(randomREH$edgelist)

## -----------------------------------------------------------------------------
randomREH$actors

## ----eval = FALSE-------------------------------------------------------------
# # Example: restrict the risk set to a specific set of dyads
# my_riskset <- data.frame(
#   actor1 = c("Alexander", "Colton", "Lexy"),
#   actor2 = c("Kayla",     "Lexy",   "Alexander")
# )
# 
# reh_manual <- remify(
#   edgelist      = randomREH$edgelist,
#   directed      = TRUE,
#   model         = "tie",
#   riskset       = "manual",
#   manual.riskset = my_riskset
# )

## -----------------------------------------------------------------------------
randomREH$origin

## -----------------------------------------------------------------------------
edgelist_reh <- remify(
  edgelist  = randomREH$edgelist,
  directed  = TRUE,   # events are directed
  ordinal   = FALSE,  # model with waiting times
  model     = "tie",  # tie-oriented modeling
  actors    = randomREH$actors,
  riskset   = "full",
  origin    = randomREH$origin
)

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
names(edgelist_reh$meta)

## -----------------------------------------------------------------------------
edgelist_reh$meta$directed
edgelist_reh$meta$model
edgelist_reh$meta$dictionary

## -----------------------------------------------------------------------------
# dyad ID of the first event
edgelist_reh$ids$dyad[1]

# sender ID of the first event
edgelist_reh$ids$actor1[1]

## -----------------------------------------------------------------------------
head(edgelist_reh$riskset_info$included)

## -----------------------------------------------------------------------------
reh_actor <- remify(
  edgelist = randomREH$edgelist,
  directed = TRUE,
  ordinal  = FALSE,
  model    = "actor",
  actors   = randomREH$actors,
  riskset  = "full",
  origin   = randomREH$origin
)
reh_actor$activeN
head(reh_actor$index$sender_map)

## -----------------------------------------------------------------------------
summary(edgelist_reh)

## -----------------------------------------------------------------------------
dim(edgelist_reh)

## ----out.width="50%", fig.align = "center", dev=c("jpeg"), fig.alt = "summary plots", dev.args = list(bg = "white")----
op <- par(no.readonly = TRUE)
par(mai=rep(0.8,4), cex.main=0.9, cex.axis=0.75)
plot(x = edgelist_reh, which = 1, n_intervals = 13)
plot(x = edgelist_reh, which = 2, n_intervals = 13)
plot(x = edgelist_reh, which = 3, n_intervals = 13)
plot(x = edgelist_reh, which = 4, n_intervals = 13)
plot(x = edgelist_reh, which = 5, n_intervals = 13,
     igraph.edge.color = "#cfcece", igraph.vertex.color = "#7bbfef")
par(op)

## -----------------------------------------------------------------------------
edgelist_reh_undir <- remify(
  edgelist = randomREH$edgelist,
  directed = FALSE,
  model    = "tie"
)

