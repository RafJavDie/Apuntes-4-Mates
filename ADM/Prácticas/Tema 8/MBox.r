MBox<- function(X,g)
{
 ni<- table(g)
 n<- sum(ni)
 ng<- length(ni)
 vdet<- numeric(ng)
 p<- ncol(X)
 Sigma<- matrix(0,p,p)
 for (i in 1:ng)
  {
  sigmai<- cov(X[g==levels(g)[i],])
  vdet[i]<- det(sigmai)
  Sigma<-    Sigma+ (ni[i]-1)*sigmai
  }
 Sigma<- Sigma/(n-ng)
 M<-(n-ng)*log(det(Sigma))-sum((ni-1)*log(vdet))
 f1<- (ng-1)*p*(p+1)/2
 pho<- 1-((2*p^2+3*p-1)/(6*(p+1)*(ng-1))*(sum(1/(ni-1))-(1/(n-ng))))
 tau<- -((p-1)*(p+2))/(6*(ng-1))*(sum(1/((ni-1)^2))-(1/((n-ng)^2)))
 f2<- (f1+2)/abs(tau-(1-pho)^2)
 gamma<- (pho-f1/f2)/f1
 pvalor=pf(gamma*M,f1,f2,lower.tail=FALSE)
 names(M)<- names(pvalor)<- NULL
 return(list(M=M,pvalor=pvalor,gl1=f1,gl2=f2))
}
