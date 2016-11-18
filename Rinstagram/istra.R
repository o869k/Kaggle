M = 10 #number of reapeted simualtons
K = 100000 #num of simualtions
sig = 1
alpha = 0.8
N = 100 #num of rounds per simualtino

m = rep(0,M)
for (t in 1:M)
{
    cat(t,"\n")
    k = rep(0,K)
    for (j in 1:K)
    {
        P = rep(0,N)
        for (i in c(1:(N-1))) 
        {
            P[i+1] = alpha*P[i]+rnorm(1,0,sig)
        }
        #plot(P,type="l")
        #hist(P,100); box()
        k[j] = max(sign(P[N])*P)
    }
    #plot(k,type="l")
    #hist(k,100); box()
    m[t] <- mean(k)
}
plot(m,type="l")
summary(m)
hist(m,100); box()
