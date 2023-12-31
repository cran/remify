## tests on function remify::remify() - tie-oriented modeling ##
out <- remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie")

# expectations on output object features               
expect_inherits(out, "remify")
expect_true(is.list(out))
expect_equal(length(out), 7)

# expectations on objects inside the 'remify' object
expect_identical(names(out),c("M","N","C","D","intereventTime","edgelist","omit_dyad"))
expect_equal(out$M, dim(randomREH$edgelist)[1])
expect_equal(out$edgelist[,1],randomREH$edgelist[,1])

# expectations on attributes of the 'remify' object 
expect_identical(names(attributes(out)),c("names","class","with_type","weighted","directed","ordinal","model","riskset","dictionary","origin","ncores","dyadID","actor1ID","actor2ID","typeID"))
expect_false(attr(out,"ordinal")) 
expect_true(attr(out,"directed"))
expect_identical(attr(out,"model"),"tie")

# if the input `edgelist`` does not have a type column
reh_loc <- randomREH
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = NULL, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = NULL,
                  model = "tie")
expect_null(out$C)
expect_null(out$edgelist$type)
expect_false(attr(out,"with_type"))

# if the input `edgelist` has a type column
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = NULL, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_equal(out$C,3)
expect_true(attr(out,"with_type"))

# if the input `edgelist`` does not have a weight column
reh_loc <- randomREH
reh_loc$edgelist$weight <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_null(out$edgelist$weight)
expect_false(attr(out,"weighted"))

# if the input `edgelist`` has a weight column
reh_loc <- randomREH
reh_loc$edgelist$weight <- rnorm(n=dim(reh_loc$edgelist)[1])
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_equal(out$edgelist$weight,reh_loc$edgelist$weight)
expect_true(attr(out,"weighted"))

# processing the input `omit_dyad` when the network is undirected
reh_loc <- randomREH
reh_loc$omit_dyad[[2]]$dyad <- rbind(reh_loc$omit_dyad[[2]]$dyad,c("Megan","Zackary",NA),c(NA,"Alexander",NA))
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = FALSE, # events are not directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_true(!is.null(out$omit_dyad))

# testing sorting of actor1 and actor2 in omit_dyad objects when directed = FALSE
reh_loc$omit_dyad[[2]]$dyad <- rbind(reh_loc$omit_dyad[[2]]$dyad,c("Zackary","Megan",NA))
expect_silent(remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = FALSE, # events are not directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"))

# if the input `omit_dyad` contains time intervals of the type [start = NA, stop = x] or [start = x, stop = NA]
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time[1] <- NA
reh_loc$omit_dyad[[2]]$time[2] <- NA
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_true(!is.null(out$omit_dyad))


# if riskset = "active" and model = "actor"
reh_loc <- randomREH
expect_silent(remify(edgelist = reh_loc$edgelist,
                  model = "actor",
                  riskset = "active"))


### here the new test
# if the input `omit_dyad`` object contains dyads specified in other ways that the default inside randomREH object
reh_loc <- randomREH
reh_loc$omit_dyad[[2]]$dyad <- data.frame(actor1=c("Megan","Richard",NA,"Derek"),actor2=c("Zackary",NA,"Crystal","Lexy"),type=c("conflict","conflict","conflict",NA))

## tested for the tie-oriented framework
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_true(!is.null(out$omit_dyad))

## tested for the actor-oriented framework
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "actor")
expect_true(!is.null(out$omit_dyad))

# test omit_dyad object with sequence with simultaneous events
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$edgelist$time[9910:9915] <- reh_loc$edgelist$time[9910:9915]+c(1:6)
reh_loc$origin <- as.Date(reh_loc$origin)-1
reh_loc$omit_dyad[[1]]$time <- as.Date(reh_loc$omit_dyad[[1]]$time)
reh_loc$omit_dyad[[2]]$time <- as.Date(reh_loc$omit_dyad[[1]]$time)
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_identical(names(attributes(out)),c("names","class","with_type","weighted","directed","ordinal","model","riskset","dictionary","origin","ncores","dyadID","actor1ID","actor2ID","typeID","evenly_spaced_interevent_time","indices_simultaneous_events"))

# `time` in 'omit_dyad' defined as c(NA,NA)
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time <- c(NA,NA)
expect_silent(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"))

# test on unsorted time variable (Rcpp level) 
reh_loc <- randomREH
reh_loc$edgelist$time <- reh_loc$edgelist$time[sample(1:dim(reh_loc$edgelist)[1],size=dim(reh_loc$edgelist)[1],replace=FALSE)]
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = NULL,
                  omit_dyad = NULL,
                  model = "tie"))
expect_equal(sort(reh_loc$edgelist$time),out$edgelist[,1])


# test on argument 'ncores' argument
expect_silent(remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie",
                  ncores = NULL))

# test on argument `riskset` as NULL (default is 'full')
expect_silent(remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  riskset = NULL,
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie"))                  

# check snapshots [[At the moment tonly testing that the expectation of an stdout - should compare with a snapshot defined in pattern]]

# test (1) on method print()"
reh_loc <- randomREH
expect_stdout(
remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie") )



# test (1) on method summary()
reh_loc <- randomREH
expect_stdout(
summary(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")))

# test (1) on method print.remify()
reh_loc <- randomREH
expect_stdout(
print(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")))


# test (2) on method summary()
## without input 'origin'
reh_loc <- randomREH
expect_stdout(
summary(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = NULL,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie"))
)



# test (3) on method summary()"
## time input set to Date
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
reh_loc$omit_dyad <- NULL
expect_stdout(
summary(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie"))
)



## tests on function remify::remify() - actor-oriented modeling ##
out <- remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "actor")

# expectations on output object features               
expect_inherits(out, "remify")
expect_true(is.list(out))
expect_equal(length(out), 7)

# expectations on objects inside the 'remify' object
expect_identical(names(out),c("M","N","C","D","intereventTime","edgelist","omit_dyad"))
expect_equal(out$M, dim(randomREH$edgelist)[1])
expect_equal(out$edgelist[,1],randomREH$edgelist[,1])

# expectations on attributes of the 'remify' object 
expect_identical(names(attributes(out)),c("names","class","with_type","weighted","directed","ordinal","model","riskset","dictionary","origin","ncores","actor1ID","actor2ID","typeID"))
expect_false(attr(out,"ordinal")) 
expect_true(attr(out,"directed"))
expect_identical(attr(out,"model"),"actor")

## tests on ordinal = TRUE ##
reh_loc_ordinal <- randomREH$edgelist
reh_loc_ordinal$time  <- rep(1:(9915/5),each=5)
expect_silent(remify(edgelist = reh_loc_ordinal, ordinal = TRUE, model = "tie"))
expect_silent(remify(edgelist = reh_loc_ordinal, ordinal = TRUE, model = "actor"))

#

# tests on edgelist processing without self-loops removal

#

## weighted 
reh_loc <- randomREH
reh_loc$edgelist$weight <- as.numeric(reh_loc$edgelist$time)**0.5 # adding a fake weight

### weighted - C>1 - tie-oriented model
expect_silent(remify(edgelist = reh_loc$edgelist, model = "tie"))

### weighted - C>1 - actor-oriented model
expect_silent(remify(edgelist = reh_loc$edgelist, model = "actor"))

### weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_silent(remify(edgelist = reh_loc$edgelist, model = "tie"))

### weighted - C=1 - actor-oriented model
expect_silent(remify(edgelist = reh_loc$edgelist, model = "actor"))

## not weighted 
reh_loc <- randomREH

### not weighted - C>1 - tie-oriented model
expect_silent(remify(edgelist = reh_loc$edgelist, model = "tie"))

### not weighted - C>1 - actor-oriented model
expect_silent(remify(edgelist = reh_loc$edgelist, model = "actor"))

### not weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_silent(remify(edgelist = reh_loc$edgelist, model = "tie"))

### not weighted - C=1 - actor-oriented model
expect_silent(remify(edgelist = reh_loc$edgelist, model = "actor"))

#

# tests on edgelist processing with self-loops removal

#

## weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]
reh_loc$edgelist$weight <- as.numeric(reh_loc$edgelist$time)**0.5 # adding a fake weight

### weighted - C>1 - tie-oriented model
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "tie"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

### weighted - C>1 - actor-oriented model
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "actor"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

### weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "tie"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

### weighted - C=1 - actor-oriented model
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "actor"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

## not weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]

### not weighted - C>1 - tie-oriented model
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "tie"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

### not weighted - C>1 - actor-oriented model
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "actor"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

### not weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "tie"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)

### not weighted - C=1 - actor-oriented model
out <- suppressWarnings(remify(edgelist = reh_loc$edgelist, model = "actor"))
expect_equal(dim(reh_loc$edgelist)[1]-50,out$M)