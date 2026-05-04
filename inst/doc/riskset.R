## -----------------------------------------------------------------------------
library(remify) # loading package
data(randomREHsmall) # data

# processing the edgelist 
reh <- remify(
  edgelist = randomREHsmall$edgelist,
  directed = TRUE,
  ordinal  = FALSE,
  model    = "tie",
  actors   = randomREHsmall$actors,
  origin   = randomREHsmall$origin
)

## ----include = TRUE-----------------------------------------------------------
randomREHsmall$edgelist[1:5,]

## ----include = TRUE-----------------------------------------------------------
head(reh$index$dyad_map)

## ----include = TRUE-----------------------------------------------------------
sorted_actors <- sort(randomREHsmall$actors)
sorted_actors

N <- length(randomREHsmall$actors)

# IDs are 1 to N
names(sorted_actors) <- 1:N
sorted_actors

## ----include = TRUE-----------------------------------------------------------
sorted_types <- c(" ") # no event type in randomREHsmall
C <- length(sorted_types)

dyad_mat <- matrix(NA, nrow = N*(N-1)*C, ncol = 3)
colnames(dyad_mat) <- c("actor1", "actor2", "type")
rownames(dyad_mat) <- 1:(N*(N-1)*C)

d <- 1
for(type in sorted_types){
  for(actor1 in sorted_actors){
    for(actor2 in sorted_actors){
      if(actor1 != actor2){
        dyad_mat[d,] <- c(actor1, actor2, type)
        d <- d + 1
      }
    }
  }
}

dyad_mat[1:5,]
dim(dyad_mat)[1] # full risk set size = 20

## ----include = TRUE-----------------------------------------------------------
head(reh$ids$dyad) # dyadID of the first few observed events

## ----echo = FALSE, dev=c("jpeg"), dev.args = list(bg = "white"), fig.alt = "Visualizing risk set composition at each time point"----
risk_set <- expand.grid(sorted_actors, sorted_actors)
dyad_occurred <- c(11, 4, 11, 11)

op <- par(no.readonly = TRUE)
layout_matrix <- matrix(c(1,2,3,4), ncol=2, byrow=TRUE)
layout(layout_matrix, widths=c(1/2,1/2), heights=c(1/2,1/2))
par(oma=c(2,0,2,0))
par(mar=c(6,6,1,0))
par(mgp=c(6,1,0))
par(mfrow=c(2,2))
for(m in 1:4){
  value <- rep(NA, dim(risk_set)[1])
  for(d in 1:length(value)){
    if(risk_set[d,1] != risk_set[d,2]){
      if(d == dyad_occurred[m]) value[d] <- "#2ffd20"
      else value[d] <- "#b2b2b2"
    } else {
      value[d] <- "#ffffff"
    }
  }
  dat <- data.frame(row=as.numeric(risk_set[,1]), col=as.numeric(risk_set[,2]), value=value)
  plot.new()
  plot.window(xlim=c(0.5,N+0.5), ylim=c(0.5,N+0.5), asp=1)
  with(dat, {
    rect(col-0.5, row-0.5, col+0.5, row+0.5, col=value, border="#f1f1f1")
    text(x=c(1:N), y=0, labels=sorted_actors, srt=90, pos=1, xpd=TRUE, adj=c(0.5,0), offset=1.5, cex=0.8)
    text(x=0, y=c(1:N), labels=sorted_actors, srt=0, pos=2, xpd=TRUE, adj=c(1,0.5), offset=-0.5, cex=0.8)
    mtext(text="actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2), cex=0.6)
    mtext(text="actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1, cex=0.6)
    mtext(text=bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
  })
}
par(op)

## ----echo = FALSE, out.width="50%", dev=c("jpeg"), dev.args = list(bg = "white"), fig.alt = "Visualizing full risk set for undirected network"----
op <- par(no.readonly = TRUE)
dyad_occurred <- c(NA, NA, 16)
for(m in 3){
  value <- rep(NA, dim(risk_set)[1])
  for(d in 1:dim(risk_set)[1]){
    if(risk_set[d,1] != risk_set[d,2]){
      if(d == dyad_occurred[m]) value[d] <- "#2ffd20"
      else if(as.character(risk_set[d,1]) < as.character(risk_set[d,2])) value[d] <- "#b2b2b2"
      else value[d] <- "#ffffff"
    }
  }
  dat <- data.frame(row=as.numeric(risk_set[,1]), col=as.numeric(risk_set[,2]), value=value)
  plot.new()
  plot.window(xlim=c(0.5,N+0.5), ylim=c(0.5,N+0.5), asp=1)
  with(dat, {
    rect(col-0.5, row-0.5, col+0.5, row+0.5, col=value, border="#f1f1f1")
    text(x=c(1:N), y=0, labels=sorted_actors, srt=90, pos=1, xpd=TRUE, adj=c(0.5,0), offset=1.5, cex=0.8)
    text(x=0, y=c(1:N), labels=sorted_actors, srt=0, pos=2, xpd=TRUE, adj=c(1,0.5), offset=-0.5, cex=0.8)
    mtext(text="actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2))
    mtext(text="actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
    mtext(text=bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
  })
}
par(op)

## -----------------------------------------------------------------------------
reh_active <- remify(
  edgelist = randomREHsmall$edgelist,
  directed = TRUE,
  ordinal  = FALSE,
  model    = "tie",
  actors   = randomREHsmall$actors,
  riskset  = "active",
  origin   = randomREHsmall$origin
)

# number of dyads in full vs active risk set
reh_active$D
reh_active$activeD

# the active dyads
reh_active$riskset_info$included

## ----echo = FALSE, out.width="50%", dev=c("jpeg"), dev.args = list(bg = "white"), fig.alt = "Visualizing active risk set at time t3"----
op <- par(no.readonly = TRUE)

# active dyads: those observed in randomREHsmall
active_dyads <- unique(randomREHsmall$edgelist[, c("actor1","actor2")])

dyad_occurred_m3 <- 11 # Colton -> Kayla at t3
value <- rep(NA, dim(risk_set)[1])
for(d in 1:length(value)){
  if(risk_set[d,1] != risk_set[d,2]){
    a1 <- as.character(risk_set[d,1])
    a2 <- as.character(risk_set[d,2])
    is_active <- any(active_dyads$actor1 == a1 & active_dyads$actor2 == a2)
    if(d == dyad_occurred_m3) value[d] <- "#2ffd20"
    else if(is_active) value[d] <- "#b2b2b2"
    else value[d] <- "#ffffff"
  } else {
    value[d] <- "#ffffff"
  }
}
dat <- data.frame(row=as.numeric(risk_set[,1]), col=as.numeric(risk_set[,2]), value=value)
plot.new()
plot.window(xlim=c(0.5,N+0.5), ylim=c(0.5,N+0.5), asp=1)
with(dat, {
  rect(col-0.5, row-0.5, col+0.5, row+0.5, col=value, border="#f1f1f1")
  text(x=c(1:N), y=0, labels=sorted_actors, srt=90, pos=1, xpd=TRUE, adj=c(0.5,0), offset=1.5, cex=0.8)
  text(x=0, y=c(1:N), labels=sorted_actors, srt=0, pos=2, xpd=TRUE, adj=c(1,0.5), offset=-0.5, cex=0.8)
  mtext(text="actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2), cex=0.6)
  mtext(text="actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1, cex=0.6)
  mtext(text=bquote(t[3]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
})
par(op)

## -----------------------------------------------------------------------------
reh_sat <- remify(
  edgelist = randomREHsmall$edgelist,
  directed = TRUE,
  model    = "tie",
  riskset  = "active_saturated",
  actors   = randomREHsmall$actors,
  origin   = randomREHsmall$origin
)

reh_sat$activeD
reh_sat$riskset_info$included

## -----------------------------------------------------------------------------
data(randomREH)

# Suppose we want to restrict the risk set to dyads among three actors only,
# while all other dyads are considered structurally impossible.
manual_rs <- data.frame(
  actor1 = c("Alexander", "Alexander", "Colton",    "Colton",    "Lexy",      "Lexy"),
  actor2 = c("Colton",    "Lexy",      "Alexander", "Lexy",      "Alexander", "Colton")
)

reh_manual <- remify(
  edgelist       = randomREH$edgelist,
  directed       = TRUE,
  ordinal        = FALSE,
  model          = "tie",
  actors         = randomREH$actors,
  riskset        = "manual",
  manual.riskset = manual_rs,
  origin         = randomREH$origin
)

# Active (manual) risk set size
reh_manual$activeD

# Inspect the decoded risk set
reh_manual$riskset_info$included

## ----echo = FALSE, out.width="50%", dev=c("jpeg"), dev.args = list(bg = "white"), fig.alt = "Visualizing manual risk set at time t3"----
op <- par(no.readonly = TRUE)

dyad_occurred_m3 <- 11
value <- rep(NA, dim(risk_set)[1])
for(d in 1:length(value)){
  if(risk_set[d,1] != risk_set[d,2]){
    if(d == dyad_occurred_m3) value[d] <- "#2ffd20"
    else value[d] <- "#b2b2b2"
    if(risk_set[d,1] %in% c("Richard","Francesca") | risk_set[d,2] %in% c("Richard","Francesca")){
      value[d] <- "#ffffff"
    }
  } else {
    value[d] <- "#ffffff"
  }
}
dat <- data.frame(row=as.numeric(risk_set[,1]), col=as.numeric(risk_set[,2]), value=value)
plot.new()
plot.window(xlim=c(0.5,N+0.5), ylim=c(0.5,N+0.5), asp=1)
with(dat, {
  rect(col-0.5, row-0.5, col+0.5, row+0.5, col=value, border="#f1f1f1")
  text(x=c(1:N), y=0, labels=sorted_actors, srt=90, pos=1, xpd=TRUE, adj=c(0.5,0), offset=1.5, cex=0.8)
  text(x=0, y=c(1:N), labels=sorted_actors, srt=0, pos=2, xpd=TRUE, adj=c(1,0.5), offset=-0.5, cex=0.8)
  mtext(text="actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2), cex=0.6)
  mtext(text="actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1, cex=0.6)
  mtext(text=bquote(t[3]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
})
par(op)

## ----echo = FALSE, out.width="50%", dev=c("jpeg"), dev.args = list(bg = "white"), fig.alt = "Visualizing manual risk set at time t3 - undirected"----
op <- par(no.readonly = TRUE)
dyad_occurred <- c(NA, NA, 16)
for(m in 3){
  value <- rep(NA, dim(risk_set)[1])
  for(d in 1:length(value)){
    if(risk_set[d,1] != risk_set[d,2]){
      if(d == dyad_occurred[m]) value[d] <- "#2ffd20"
      else if(as.character(risk_set[d,1]) < as.character(risk_set[d,2])) value[d] <- "#b2b2b2"
      if(risk_set[d,1] %in% c("Richard","Francesca") | risk_set[d,2] %in% c("Richard","Francesca")){
        value[d] <- "#ffffff"
      }
    } else {
      value[d] <- "#ffffff"
    }
  }
  dat <- data.frame(row=as.numeric(risk_set[,1]), col=as.numeric(risk_set[,2]), value=value)
  plot.new()
  plot.window(xlim=c(0.5,N+0.5), ylim=c(0.5,N+0.5), asp=1)
  with(dat, {
    rect(col-0.5, row-0.5, col+0.5, row+0.5, col=value, border="#f1f1f1")
    text(x=c(1:N), y=0, labels=sorted_actors, srt=90, pos=1, xpd=TRUE, adj=c(0.5,0), offset=1.5, cex=0.8)
    text(x=0, y=c(1:N), labels=sorted_actors, srt=0, pos=2, xpd=TRUE, adj=c(1,0.5), offset=-0.5, cex=0.8)
    mtext(text="actor2", side=1, line=4, outer=FALSE, adj=0, at=floor(N/2))
    mtext(text="actor1", side=2, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
    mtext(text=bquote(t[.(m)]), side=3, line=0, outer=FALSE, adj=1, at=floor(N/2)+1)
  })
}
par(op)

## -----------------------------------------------------------------------------
reh_active <- remify(
  edgelist       = randomREH$edgelist,
  directed       = TRUE,
  ordinal        = FALSE,
  model          = "tie",
  actors         = randomREH$actors,
  riskset        = "active",
  origin         = randomREH$origin,
  riskset_decode = "labels"
)

head(reh_active$riskset_info$included)
nrow(reh_active$riskset_info$included) # number of active dyads

## -----------------------------------------------------------------------------
# dyadID of the first 10 observed events
head(reh_active$ids$dyad, 10)

