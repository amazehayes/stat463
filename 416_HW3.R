# Addison Hayes - Stat 416 Homework 3 R Code

library(expm)
#2.1
P = matrix(c(.3679, .3679, .1839, .0613, .0153, .0031, .0005, .0001,
             .3679, .3679, .1839, .0613, .0153, .0031, .0005, .0001,
             0, .3679, .3679, .1839, .0613, .0153, .0031, .0006,
             0, 0, .3679, .3679, .1839, .0613, .0153, .0037,
             0, 0, 0, .3679, .3679, .1839, .0613, .0190,
             0, 0, 0, 0, .3679, .3679, .1839, .0803,
             0, 0, 0, 0, 0, .3679, .3679, .2642,
             0, 0, 0, 0, 0, 0, .3679, .6321), nrow = 8, ncol = 8, byrow =TRUE)

P_sq = P%*%P
P_sq

# Problem 2.4

brand <- matrix(c(.1,.3,.6,
                  .1,.5,.4,
                  .3,.2,.5), nrow =3, byrow = TRUE)
brand

brand%^%9


# Problem 2.15

# Part a
amat <- matrix(c(.1,.3,.2,.4,
                 .1,.3,.4,.2,
                 .3,.3,.1,.3,
                 .15,.25,.35,.25), nrow = 4, byrow = TRUE)
n <- 4
t <- 10
Mt <- diag(n)
for(i in 1:t){
  Mt <- Mt+amat%^%i
}
Mt

# Part b
bmat <- matrix(c(0,1,0,0,
                 0,0,1,0,
                 0,0,0,1,
                 1,0,0,0), nrow = 4, byrow = TRUE)
n <- 4
t <- 10
Mt <- diag(n)
for(i in 1:t){
  Mt <- Mt+bmat%^%i
}
Mt

# Part c
cmat <- matrix(c(.1,0,.9,0,
                 0,.3,0,.7,
                 .3,0,.7,0,
                 0,.25,0,.75), nrow = 4, byrow = TRUE)

n <- 4
t <- 10
Mt <- diag(n)
for(i in 1:t){
  Mt <- Mt+cmat%^%i
}
Mt

# Part d
dmat <- matrix(c(.1,.3,0,.6,
                 .1,.3,0,.6,
                 .3,.1,.1,.5,
                 .5,.25,0,.25), nrow = 4, byrow = TRUE)

n <- 4
t <- 10
Mt <- diag(n)
for(i in 1:t){
  Mt <- Mt+dmat%^%i
}
Mt



# Problem 2.16

pc <- matrix(c(0.0498,0,0,.9502,
               .1494,.0498,0,.8008,
               .2240,.1494,.0498,.5768,
               .2240,.2240,.1494,.4026), nrow = 4, byrow = TRUE)
n <- 4
t <- 52
Mt <- diag(n)
for(i in 1:t){
  Mt <- Mt+pc%^%i
}
Mt


# Additional Problem 1
mc.est <- function(x){if(is.list(x)){## This case is for a "list" of multiple MCs.
  n.chains=length(x)
  all.x=integer()
  init.states=integer()
  end.states=integer()
  for(i in 1:n.chains){
    x.chain=x[[i]]
    all.x=c(all.x,x.chain)
    init.states=c(init.states,x.chain[-length(x.chain)])
    end.states=c(end.states,x.chain[-1])
  }
  states=sort(unique(all.x))}
  else{## This case is for one MC.
    init.states=x[-length(x)]
    end.states=x[-1]
    states=sort(unique(x))
  }
  n.states=length(states)
  transitions=data.frame(init.states,end.states)
  P=as.matrix(table(transitions))
  n.s=apply(P,1,sum)
  P.hat=P/n.s
  colnames(P.hat) <- states
  rownames(P.hat) <- states
  P.hat
}

mc.sim <- function(P,starting.state,T){## NOTE: this orders states starting at ONE (not at zero)!##     The first row of the "P" matrix corresponds to state=1##     The 2nd   row of the "P" matrix corresponds to state=2##     ...
  N=nrow(P)
  X=rep(NA,T+1)
  X[1] <- starting.state
  for(t in 1:T){
    X[t+1]=sample(N,size=1,prob=P[X[t],])
  }
  X
  }

load("hiv.Rdata")
p <- mc.est(hiv)
p
p%^%3

n <- 6
t <- 20
Mt <- diag(n)
for(i in 1:t){
  Mt <- Mt+p%^%i
}
Mt

