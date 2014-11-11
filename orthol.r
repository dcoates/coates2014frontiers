require("mgcv")

#dev.new()
#subn <- "S7"
#ord <- 6 
dat_all <- read.csv( "chung2011_plus.csv", header=TRUE, sep=",")

dat = subset( dat_all, sub==subn )

dat$block_fact = factor( dat$block )
dat$durfact = factor( dat$dur )
dat1 <- dat
alldurs <- sort ( unique( dat$dur ) )
numdurs <- length( alldurs )

Blocks <- ordered( dat1$block )
Blocks <- C( Blocks, contr.poly, order)
modl_fixed<- glm(p ~ log10(dur)+Blocks, binomial(probit), data = dat1, weights = n)
modl_var<- glm(p ~ Blocks + log10(dur)*Blocks, binomial(probit), data = dat1, weights = n)

# plotting:
#plot( dat$block, ( predict(modl_fixed ) ), col="gray", ylim=c(-2,3)  )
#points( dat$block, ( predict(modl_var ) ), col="black"  )
#title(subn)
#grid()
#print( summary( modl ) )

coef1 <- 1:(numdurs*3)
dim(coef1) <- c(numdurs,3)
# loop for individual durations
#for (n in 0:0) #)(numdurs) )
#{
    #thisdur <- alldurs[n]
    #print(thisdur)
#
    #asub = subset( dat1, dur==thisdur )
    #Blocks <- ordered( asub$block )
    #Blocks <- C( Blocks, contr.poly, order)
    #modl <- glm(p ~ log10(dur)+Blocks, binomial(probit), data = asub, weights = n)
    ##lines( asub$block, qnorm(modl$fitted.values), col="orange", lty=2 )
#}

blocks = max( dat$block )
intercepts=NULL
slopes=NULL
for (n in 1:blocks) {
    choose = dat$block==n
    lm.block = glm(formula =  modl_var$fitted[choose] ~ log10(dat$dur[choose]), family=binomial(probit))
    intercepts[n] = lm.block[[1]][1]
    slopes[n] = lm.block[[1]][2]
}
