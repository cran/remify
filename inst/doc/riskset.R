## -----------------------------------------------------------------------------
library(remify) # loading package
data(randomREHsmall) # data

# processing the edgelist 
reh <- remify(edgelist = randomREHsmall$edgelist,
                          directed = TRUE, # events are directed
                          ordinal = FALSE, # model with waiting times
                          model = "tie", # tie-oriented modeling   
                          actors = randomREHsmall$actors,
                          origin = randomREHsmall$origin,
                          omit_dyad = NULL)

# summary(reh)                                

## ----include = TRUE-----------------------------------------------------------
randomREHsmall$edgelist[1:5,]

## ----include = TRUE-----------------------------------------------------------
# method getDyad(), see more in ?remify::getDyad
getDyad(x = reh, dyadID = c(1:5)) 

## ----include = TRUE-----------------------------------------------------------
# sorted vector of actors' names
sorted_actors <- sort(randomREHsmall$actors)
sorted_actors

# number of actors in the network
N <- length(randomREHsmall$actors)

## ----include = TRUE-----------------------------------------------------------
# no event type, we set it to an empty string
sorted_types <- c(" ") 

# C = 1 for 'randomREHsmall'
C <- length(sorted_types) 

## ----include = TRUE-----------------------------------------------------------
# IDs of actors will consist of an integer number from 1 to N
names(sorted_actors) <- 1:N
sorted_actors

# IDs of types will be an integer number from 1 to C
names(sorted_types) <- 1:C # in this case is one (artificial) event type
sorted_types

## ----include = TRUE-----------------------------------------------------------
# initializing matrix object where to store the dyads as [actor1,actor2,type]
dyad_mat <- matrix(NA, nrow = N*(N-1)*C, ncol = 3)
colnames(dyad_mat) <- c("actor1","actor2","type")
rownames(dyad_mat) <- 1:(N*(N-1)*C)

# initializing position index
d <- 1 

# start three loops
for(type in sorted_types){ # loop over event types, 
  for(actor1 in sorted_actors){ # loop over actor1
    for(actor2 in sorted_actors){ # loop over actor2
      if(actor1!=actor2){ # avoid self-loops
        dyad_mat[d,] <- c(actor1,actor2,type)
        d <- d + 1
      }
    }
  }
}

 # same result as showed above by using the method `getDyad()`
dyad_mat[1:5,]

# checking the size of the _full_ risk set that is 20
dim(dyad_mat)[1] 

## ----include = TRUE-----------------------------------------------------------
# accessing the first values of the attribute "dyad" 
# (attribute available only for tie-oriented modeling)
head(attr(reh,"dyad"))

## ----echo = FALSE, dev=c("jpeg")----------------------------------------------
risk_set <- expand.grid(sorted_actors,sorted_actors)
dyad_occurred <- c(11,4,11,11)

# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

# ... creating layout
layout_matrix <- matrix(c(1,2,3,4), ncol=2, byrow=TRUE) # 0 can become 4 for a legend of the colors
layout(layout_matrix, widths=c(1/2,1/2), heights=c(1/2,1/2))
# ... starting plotting
par(oma=c(2,0,2,0))
par(mar=c(6,6,1,0))
par(mgp=c(6,1,0))
par(mfrow=c(2,2))
for(m in 1:4){
value <- rep(NA,dim(risk_set)[1])
for(d in 1:length(value)){
    if(risk_set[d,1]!=risk_set[d,2]){
        if(d == dyad_occurred[m]){
            value[d] <- "#2ffd20"
        }
        else{
            value[d] <- "#b2b2b2"
        }
    }
    else{
       value[d] <- "#ffffff"
    }
}
dat <- data.frame(row=as.numeric(risk_set[,1]),col=as.numeric(risk_set[,2]),value=value)
# tile plot
plot.new()
plot.window(xlim=c(0.5,N+0.5),ylim=c(0.5,N+0.5),asp=1)
with(dat,{
rect(col-0.5,row-0.5,col+0.5,row+0.5,col=value,border="#f1f1f1")  
#segments(x0=c(1:N)+0.5,y0=c(1:N)-0.5,x1=c(1:N)-0.5,y1=c(1:N)+0.5,col="#eae8e8")
#segments(x0=0.5,y0=0.5,x1=(N+0.5),y1=(N+0.5),col="#eae8e8")
# actor names
text(x = c(1:N), y = 0, labels = sorted_actors, srt = 90, pos = 1, xpd = TRUE,  adj = c(0.5,0), offset = 1.5,cex = 0.8) 
text(x = 0, y = c(1:N), labels = sorted_actors, srt = 0, pos = 2, xpd = TRUE,  adj = c(1,0.5), offset = -0.5, cex = 0.8)
# axes names 
mtext(text  = "actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2),cex = 0.6)
mtext(text = "actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1,cex = 0.6)
mtext(text = bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
})
}

par(op)

## ----echo = FALSE, out.width="50%", dev=c("jpeg")-----------------------------
# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

dyad_occurred <- c(NA,NA,16) 
for(m in 3){
value <- rep(NA,dim(risk_set)[1])
for(d in 1:dim(risk_set)[1]){
    if(risk_set[d,1]!=risk_set[d,2]){
        if(d == dyad_occurred[m]){
            value[d] <- "#2ffd20"
        }
        else if(as.character(risk_set[d,1])<as.character(risk_set[d,2])){
            value[d] <- "#b2b2b2"
        }
        else{
            value[d] <- "#ffffff"
        }
    }
}
dat <- data.frame(row=as.numeric(risk_set[,1]),col=as.numeric(risk_set[,2]),value=value)
# tile plot
plot.new()
plot.window(xlim=c(0.5,N+0.5),ylim=c(0.5,N+0.5),asp=1)
with(dat,{
rect(col-0.5,row-0.5,col+0.5,row+0.5,col=value,border="#f1f1f1")  
#segments(x0 = c(rep(seq(1.5,4.5,by=1),each=2),5.5), y0 = c(0.5,rep(seq(1.5,4.5,by=1),each=2)) , x1 = c(rep(0.5,5),seq(1.5,4.5,by=1)) , y1 =c(seq(1.5,5.5,by=1),rep(5.5,4)),col="#eae8e8")
#segments(x0 = rep(0.5,5), y0 = seq(0.5,4.5,by=1), x1 = seq(5.5,1.5,by=-1), y1 = rep(5.5,5), col="#eae8e8")

# actor names
text(x = c(1:N), y = 0, labels = sorted_actors, srt = 90, pos = 1, xpd = TRUE,  adj = c(0.5,0), offset = 1.5,cex = 0.8) 
text(x = 0, y = c(1:N), labels = sorted_actors, srt = 0, pos = 2, xpd = TRUE,  adj = c(1,0.5), offset = -0.5, cex = 0.8)
# axes names 
mtext(text  = "actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2))
mtext(text = "actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
mtext(text = bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
})
}

par(op)

## ----echo = FALSE, dev=c("jpeg")----------------------------------------------
dyad_occurred <- c(11,4,11,11) 

# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

# ... creating layout
layout_matrix <- matrix(c(1,2,3,4), ncol=2, byrow=TRUE) # 0 can become 4 for a legend of the colors
layout(layout_matrix, widths=c(1/2,1/2), heights=c(1/2,1/2))
# ... starting plotting
par(oma=c(2,0,2,0))
par(mar=c(6,6,1,0))
par(mgp=c(6,1,0))
par(mfrow=c(2,2))
for(m in 1:4){
value <- rep(NA,dim(risk_set)[1])
for(d in 1:length(value)){
    if(risk_set[d,1]!=risk_set[d,2]){
        if(d == dyad_occurred[m]){
            value[d] <- "#2ffd20"
        }
        else{
            value[d] <- "#b2b2b2"
        }
        if(risk_set[d,1] %in% c("Richard","Francesca") | risk_set[d,2] %in% c("Richard","Francesca")){
          value[d] <- "#ffffff"
        }
    }
    else{
       value[d] <- "#ffffff"
    }
}
dat <- data.frame(row=as.numeric(risk_set[,1]),col=as.numeric(risk_set[,2]),value=value)
# tile plot
plot.new()
plot.window(xlim=c(0.5,N+0.5),ylim=c(0.5,N+0.5),asp=1)
with(dat,{
rect(col-0.5,row-0.5,col+0.5,row+0.5,col=value,border="#f1f1f1")  

#segments(x0=c(1:N)+0.5,y0=c(1:N)-0.5,x1=c(1:N)-0.5,y1=c(1:N)+0.5,col="#eae8e8")
#segments(x0=0.5,y0=0.5,x1=(N+0.5),y1=(N+0.5),col="#eae8e8")

# actor names
text(x = c(1:N), y = 0, labels = sorted_actors, srt = 90, pos = 1, xpd = TRUE,  adj = c(0.5,0), offset = 1.5,cex = 0.8) 
text(x = 0, y = c(1:N), labels = sorted_actors, srt = 0, pos = 2, xpd = TRUE,  adj = c(1,0.5), offset = -0.5, cex = 0.8)
# axes names 
mtext(text  = "actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2),cex = 0.6)
mtext(text = "actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1,cex = 0.6)
mtext(text = bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
})
}

par(op)

## ----echo = FALSE, out.width="50%", dev=c("jpeg")-----------------------------
# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

dyad_occurred <- c(NA,NA,16) 
for(m in 3){
value <- rep(NA,dim(risk_set)[1])
for(d in 1:length(value)){
    if(risk_set[d,1]!=risk_set[d,2]){
        if(d == dyad_occurred[m]){
            value[d] <- "#2ffd20"
        }
        else if(as.character(risk_set[d,1])<as.character(risk_set[d,2])){
            value[d] <- "#b2b2b2"
        }
        if(risk_set[d,1] %in% c("Richard","Francesca") | risk_set[d,2] %in% c("Richard","Francesca")){
          value[d] <- "#ffffff"
        }
    }
    else{
       value[d] <- "#ffffff"
    }
}
dat <- data.frame(row=as.numeric(risk_set[,1]),col=as.numeric(risk_set[,2]),value=value)
# tile plot
plot.new()
plot.window(xlim=c(0.5,N+0.5),ylim=c(0.5,N+0.5),asp=1)
with(dat,{
rect(col-0.5,row-0.5,col+0.5,row+0.5,col=value,border="#f1f1f1")  
# actor names
text(x = c(1:N), y = 0, labels = sorted_actors, srt = 90, pos = 1, xpd = TRUE,  adj = c(0.5,0), offset = 1.5,cex = 0.8) 
text(x = 0, y = c(1:N), labels = sorted_actors, srt = 0, pos = 2, xpd = TRUE,  adj = c(1,0.5), offset = -0.5, cex = 0.8)
# axes names 
mtext(text  = "actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2))
mtext(text = "actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
mtext(text = bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
})
}

par(op)

## -----------------------------------------------------------------------------
data(randomREH)

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[1]]$time # start and stop time point defining the time window of interest

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[1]]$dyad # dyads to be removed from the time points defined by the interval in `time`

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[2]]$time

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[2]]$dyad

## ----echo=FALSE---------------------------------------------------------------
randomREH$omit_dyad[[3]] <- list()
randomREH$omit_dyad[[3]]$time <- c(randomREH$edgelist$time[3500],randomREH$edgelist$time[3500])
randomREH$omit_dyad[[3]]$time[1] <- NA
randomREH$omit_dyad[[3]]$dyad <- data.frame(actor1=c("Maya","Alexander","Richard","Charles",rep(NA,4)),actor2= c(rep(NA,4),"Maya","Alexander","Richard","Charles"),type=rep(NA,8))

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[3]]$time 

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[3]]$dyad 

## ----echo=FALSE---------------------------------------------------------------
randomREH$omit_dyad[[4]] <- list()
randomREH$omit_dyad[[4]]$time <- c(randomREH$edgelist$time[2800],randomREH$edgelist$time[8700])
randomREH$omit_dyad[[4]]$dyad <- data.frame(actor1=c("Breanna",NA),actor2= c(NA,"Breanna"),type=rep(NA,2))

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[4]]$time

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[4]]$dyad

## ----echo=FALSE---------------------------------------------------------------
randomREH$omit_dyad[[5]] <- list()
randomREH$omit_dyad[[5]]$time <- c(randomREH$edgelist$time[7000],randomREH$edgelist$time[7500])
randomREH$omit_dyad[[5]]$dyad <- data.frame(actor1=c("Megan",NA),actor2= c(NA,"Megan"),type=rep(NA,2))

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[5]]$time

## -----------------------------------------------------------------------------
randomREH$omit_dyad[[5]]$dyad

## ----echo=FALSE, out.width="50%", dev=c("jpeg")-------------------------------
# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

min_max_omit_dyad <-range(randomREH$edgelist$time)
plot(min_max_omit_dyad,c(0,length(randomREH$omit_dyad)+1),type="n",yaxt="n",xlab="time",ylab="modification",main="manual risk set \n (input modifications declared with the omit_dyad argument)")
axis(2, at=c(1:length(randomREH$omit_dyad)))
for(r in 1:length(randomREH$omit_dyad)){
    if(is.na(randomREH$omit_dyad[[r]]$time[1])){ # if start time is not specified
        randomREH$omit_dyad[[r]]$time <- c(randomREH$edgelist$time[1],randomREH$omit_dyad[[r]]$time[2])
    }
    if(is.na(randomREH$omit_dyad[[r]]$time[2])){ # if stop time is not specified
        randomREH$omit_dyad[[r]]$time <- c(randomREH$omit_dyad[[r]]$time[1],randomREH$edgelist$time[dim(randomREH$edgelist)[1]])
    }
    segments(x0=randomREH$omit_dyad[[r]]$time[1],y0=r,x1=randomREH$omit_dyad[[r]]$time[2],y1=r,col="black", lwd = 2)
    #randomREH$omit_dyad[[r]]$time
}

par(op)

## -----------------------------------------------------------------------------
edgelist_reh <- remify::remify(edgelist = randomREH$edgelist,
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # model with waiting times
                    model = "tie", # tie-oriented modeling
                    actors = randomREH$actors,
                    types = randomREH$types, 
                    riskset = "manual",
                    origin = randomREH$origin,
                    omit_dyad = randomREH$omit_dyad)
                                        

## ----echo=FALSE, out.width="50%", dev=c("jpeg")-------------------------------
# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

min_max_omit_dyad <-range(randomREH$edgelist$time)
plot(min_max_omit_dyad,c(0,length(randomREH$omit_dyad)+1),type="n",yaxt="n",xlab="time",ylab="modification",main="manual risk set \n (before processing)")
axis(2, at=c(1:length(randomREH$omit_dyad)))
for(r in 1:length(randomREH$omit_dyad)){
    if(is.na(randomREH$omit_dyad[[r]]$time[1])){ # if start time is not specified
        randomREH$omit_dyad[[r]]$time <- c(randomREH$edgelist$time[1],randomREH$omit_dyad[[r]]$time[2])
    }
    if(is.na(randomREH$omit_dyad[[r]]$time[2])){ # if stop time is not specified
        randomREH$omit_dyad[[r]]$time <- c(randomREH$omit_dyad[[r]]$time[1],randomREH$edgelist$time[dim(randomREH$edgelist)[1]])
    }
    segments(x0=randomREH$omit_dyad[[r]]$time[1],y0=r,x1=randomREH$omit_dyad[[r]]$time[2],y1=r,col="black", lwd = 2)
    #randomREH$omit_dyad[[r]]$time
}

par(op)

## ----echo = FALSE-------------------------------------------------------------
# processing here the output for the plot below 
modification_idx <- sort(unique(edgelist_reh$omit_dyad$time))
start_stop_times <- data.frame(start = rep(randomREH$edgelist$time[1],length(modification_idx)),
                               stop = rep(randomREH$edgelist$time[1],length(modification_idx)))
for(i in 1:length(modification_idx)){
 start_stop_idx <- range(which(edgelist_reh$omit_dyad$time == modification_idx[i]))
 start_stop_times[i,1] <- randomREH$edgelist$time[start_stop_idx[1]]
 start_stop_times[i,2] <- randomREH$edgelist$time[start_stop_idx[2]]
}

## ----echo=FALSE, out.width="50%", dev=c("jpeg")-------------------------------
# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

# plotting vertical lines and different colour for the intersected intervals
plot(min_max_omit_dyad,c(0,length(randomREH$omit_dyad)+1),type="n",yaxt="n",xlab="time",ylab="modification",main="manual risk set \n (while processing)")
axis(2, at=c(1:length(randomREH$omit_dyad)))
for(r in 1:length(randomREH$omit_dyad)){
    segments(x0=randomREH$omit_dyad[[r]]$time[1],y0=r,x1=randomREH$omit_dyad[[r]]$time[2],y1=r,col="black", lwd = 2)
}
abline(v = unique(c(start_stop_times$start, start_stop_times$stop)) , lwd = 1, lty = 2, col = "red")

par(op)

## ----echo=FALSE, out.width="50%", dev=c("jpeg")-------------------------------
# ... saving current graphical parameters
op <- par(no.readonly = TRUE)

plot(min_max_omit_dyad,c(0,length(modification_idx)+1),type="n",yaxt="n",xlab="time",ylab="modification",main="manual risk set \n (after processing)")
axis(2, at=c(1:length(modification_idx)))
for(r in 1:length(modification_idx)){
    segments(x0=start_stop_times[r,1],y0=r,x1=start_stop_times[r,2],y1=r, lwd = 2,col="black")
    #randomREH$omit_dyad[[r]]$time
}

par(op)

