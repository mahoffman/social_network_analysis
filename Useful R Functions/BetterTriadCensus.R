# TriadCensus.R - written by J. Skvoretz (Skvoretz@cas.usf.edu) 
# Computes the observed triad census of a directed graph, the expected number of triad types, the standard deviation of the expected number of triad types, and the zscore statistic called tau corresponding to a weighting vector over the triad types.  Modified from a SAS program written by Megumi Omori implementing matrix methods published in Moody (1998). 
# Moody, J.  1998.  "Matrix Methods for Calculating the Triad Census."  Social Networks 20: 291-299.

TriadCensus<-function(dat,weight){
  if (is(dat,"character")==T) {x=read.table(dat); ndat=dat} else {x=dat; ndat=substitute(dat)}
  if (is(weight,"character")==T) {wt=read.table(weight); nweight=weight} else {wt=weight; nweight=substitute(weight)}
  cat("\n-------------")
  cat("*  Triad Census  *")
  cat("--------------\n\n")
  cat(paste('Dataset:',ndat));cat('\n')
  cat(paste(' Wt Vec:',nweight));cat('\n')
  
  a=as.matrix(x)
  #a<-as.sociomatrix.sna(a)
  wt<-as.matrix(wt)
  nr=nrow(a)
  
  at=t(a)
  e=a+at-(a*at)	
  
  i=diag(nr)
  u=array(1,c(nr,nr))
  u=u-i  
  eb=as.matrix(u-e)		
  
  l=t(wt)
  m=as.matrix(a*at)
  cx=as.matrix(a-m)	
  tc=t(cx)
  tx=array(0, c(16,4))  
  
  #* initialize the 1 by 16 triad census vector
  
  t201=((m%*%m)*eb)
  t021d=((tc%*%cx)*eb)
  t021u=((cx%*%tc)*eb)
  tx[7,1]=sum(((a%*%at)*eb)-t201-t021u)/2
  tx[8,1]=sum(((at%*%a)*eb)-t201-t021d)/2
  
  tx[1,1]=sum(diag(eb%*%eb%*%eb))/6
  tx[2,1]=sum((eb%*%eb)*(cx+tc))/2
  tx[3,1]=sum((eb%*%eb)*m)/2
  tx[4,1]=sum((tc%*%cx)*eb)/2
  tx[5,1]=sum((cx%*%tc)*eb)/2
  tx[6,1]=sum((cx%*%cx)*eb)
  tx[9,1]=sum((cx%*%cx)*cx)
  tx[10,1]=sum(diag(cx%*%cx%*%cx))/3
  tx[11,1]=sum((m%*%m)*eb)/2
  tx[12,1]=sum((tc%*%cx)*m)/2
  tx[13,1]=sum((cx%*%tc)*m)/2
  tx[14,1]=sum((cx%*%cx)*m)
  tx[15,1]=sum((m%*%m)*(cx+tc))/2
  tx[16,1]=sum(diag(m%*%m%*%m))/6
  
  pg=nr
  
  g2=pg*(pg-1)/2
  g23=g2*(g2-1)*(g2-2)
  g25=g23*(g2-3)*(g2-4)
  g26=g25*(g2-5)
  g3=pg*(pg-1)*(pg-2)/6
  g32=g3*(g3-1)
  
  #MC,AC,NC ARE DESCENDING FACTORIALS OF M,A,N RESPECTIVELY
  
  mc=array(0,c(6,1))
  ac=array(0,c(6,1))
  nc=array(0,c(6,1))
  mc[1]=sum(m)/2 
  ac[1]=sum(cx)
  nc[1]=sum(eb)/2
  for (i in 2:6) {mc[i]=mc[i-1]*(mc[1]-i+1); ac[i]=ac[i-1]*(ac[1]-i+1);  nc[i]=nc[i-1]*(nc[1]-i+1)} 
  
  #NOW PX[I) WHICH IS EXPECTATION OF TRIAD TYPE I *
  
  px=array(0,c(16,1))
  px[1]=nc[3]
  px[2]=3*ac[1]*nc[2]
  px[3]=3*mc[1]*nc[2]
  px[4]=.75*ac[2]*nc[1]
  px[5]=px[4]
  px[6]=2*px[5]
  px[7]=3*mc[1]*ac[1]*nc[1]
  px[8]=px[7]
  px[9]=.75*ac[3]
  px[10]=px[9]/3
  px[11]=3*mc[2]*nc[1]
  px[12]=.75*mc[1]*ac[2]
  px[13]=px[12]
  px[14]=2*px[13]
  px[15]=3*mc[2]*ac[1]
  px[16]=mc[3]
  
  #NOW PO(X,Y) */
  
  poxy=array(0,c(16,16))
  poxy[1,1]=nc[6]
  poxy[2,1]=3*ac[1]*nc[5]
  poxy[3,1]=3*mc[1]*nc[5]
  poxy[4,1]=.75*ac[2]*nc[4]
  poxy[5,1]=poxy[4,1]
  poxy[6,1]=2*poxy[5,1]
  poxy[7,1]=3*mc[1]*ac[1]*nc[4]
  poxy[8,1]=poxy[7,1]
  poxy[9,1]=.75*ac[3]*nc[3]
  
  poxy[10,1]=poxy[9,1]/3
  poxy[11,1]=3*mc[2]*nc[4]
  poxy[12,1]=.75*mc[1]*ac[2]*nc[3]
  poxy[13,1]=poxy[12,1]
  poxy[14,1]=2*poxy[13,1]
  poxy[15,1]=3*mc[2]*ac[1]*nc[3]
  poxy[16,1]=mc[3]*nc[3]
  poxy[2,2]=12*poxy[4,1]
  poxy[3,2]=3*poxy[7,1]
  poxy[4,2]=3*poxy[9,1]
  poxy[5,2]=poxy[4,2]
  poxy[6,2]=2*poxy[5,2]
  poxy[7,2]=12*poxy[12,1]
  poxy[8,2]=poxy[7,2]
  poxy[9,2]=2.25*ac[4]*nc[2]
  poxy[10,2]=poxy[9,2]/3
  
  poxy[11,2]=3*poxy[15,1]
  poxy[12,2]=2.25*mc[1]*ac[3]*nc[2]
  poxy[13,2]=poxy[12,2]
  poxy[14,2]=2*poxy[13,2]
  poxy[15,2]=9*mc[2]*ac[2]*nc[2]
  poxy[16,2]=3*mc[3]*ac[1]*nc[2]
  poxy[3,3]=9*mc[2]*nc[4]
  poxy[4,3]=poxy[8,2]/4
  poxy[5,3]=poxy[4,3]
  poxy[6,3]=2*poxy[5,3]
  poxy[7,3]=poxy[11,2]
  poxy[8,3]=poxy[7,3]
  poxy[9,3]=poxy[12,2]
  poxy[10,3]=poxy[9,3]/3
  poxy[11,3]=9*poxy[16,1]
  poxy[12,3]=poxy[15,2]/4
  poxy[13,3]=poxy[12,3]
  poxy[14,3]=2*poxy[12,3]
  poxy[15,3]=3*poxy[16,2]
  poxy[16,3]=3*mc[4]*nc[2]
  poxy[4,4]=poxy[9,2]/4
  poxy[5,4]=poxy[4,4]
  poxy[6,4]=2*poxy[5,4]
  poxy[7,4]=poxy[12,2]
  poxy[8,4]=poxy[7,4]
  poxy[9,4]=.5625*ac[5]*nc[1]
  poxy[10,4]=poxy[9,4]/3
  poxy[11,4]=poxy[12,3]
  poxy[12,4]=.5625*mc[1]*ac[4]*nc[1]
  poxy[13,4]=poxy[12,4]
  poxy[14,4]=2*poxy[13,4]
  poxy[15,4]=2.25*mc[2]*ac[3]*nc[1]
  poxy[16,4]=.75*mc[3]*ac[2]*nc[1]
  
  for (i in 5:16) {poxy[i,5]=poxy[i,4]} 
  for (i in 6:16) {poxy[i,6]=2*poxy[i,5]} 
  
  poxy[7,7]=4*poxy[12,3]
  poxy[8,7]=poxy[7,7]
  poxy[9,7]=4*poxy[12,4]
  poxy[10,7]=poxy[9,7]/3
  poxy[11,7]=poxy[15,3]
  poxy[12,7]=poxy[15,4]
  poxy[13,7]=poxy[12,7]
  poxy[14,7]=2*poxy[13,7]
  poxy[15,7]=6*poxy[16,6]
  poxy[16,7]=3*mc[4]*ac[1]*nc[1]
  
  for (i in 8:16) {poxy[i,8]=poxy[i,7]}
  
  poxy[9,9]=.5625*ac[6]
  poxy[10,9]=poxy[9,9]/3
  poxy[11,9]=poxy[15,4]
  poxy[12,9]=.5625*mc[1]*ac[5]
  poxy[13,9]=poxy[12,9]
  poxy[14,9]=2*poxy[13,9]
  poxy[15,9]=2.25*mc[2]*ac[4]
  poxy[16,9]=.75*mc[3]*ac[3]
  
  for (i in 10:16) {poxy[i,10]=poxy[i,9]/3}
  
  poxy[11,11]=3*poxy[16,3]
  poxy[12,11]=3*poxy[16,4]
  poxy[13,11]=poxy[12,11]
  poxy[14,11]=2*poxy[13,11]
  poxy[15,11]=3*poxy[16,7]
  poxy[16,11]=3*mc[5]*nc[1]
  poxy[12,12]=.5625*mc[2]*ac[4]
  poxy[13,12]=poxy[12,12]
  poxy[14,12]=2*poxy[13,12]
  poxy[15,12]=2.25*mc[3]*ac[3]
  poxy[16,12]=.75*mc[4]*ac[2]
  
  for (i in 13:16) {poxy[i,13]=poxy[i,12]}
  for (i in 14:16) {poxy[i,14]=2*poxy[i,13]} 
  
  poxy[15,15]=9*mc[4]*ac[2]
  poxy[16,15]=3*mc[5]*ac[1]
  poxy[16,16]=mc[6]
  
  
  #NOW P2XY */
  
  p2xy=array(0,c(16,16))
  p2xy[1,1]=nc[5]
  p2xy[2,1]=2*ac[1]*nc[4]
  p2xy[3,1]=2*mc[1]*nc[4]
  p2xy[4,1]=.25*ac[2]*nc[3]
  p2xy[5,1]=p2xy[4,1]
  p2xy[6,1]=2*p2xy[5,1]
  p2xy[7,1]=mc[1]*ac[1]*nc[3]
  p2xy[8,1]=p2xy[7,1]
  p2xy[9,1]=0
  p2xy[10,1]=0
  p2xy[11,1]=mc[2]*nc[3]
  p2xy[12,1]=0
  p2xy[13,1]=0
  p2xy[14,1]=0
  p2xy[15,1]=0
  p2xy[16,1]=0
  p2xy[2,2]=ac[1]*nc[3]*(4*ac[1]+nc[1]-7)
  p2xy[3,2]=4*p2xy[7,1]
  p2xy[4,2]=.5*ac[2]*nc[2]*(ac[1]+nc[1]-4)
  p2xy[5,2]=p2xy[4,2]
  p2xy[6,2]=2*p2xy[5,2]
  p2xy[7,2]=mc[1]*ac[1]*nc[2]*(2*ac[1]+nc[1]-4)
  p2xy[8,2]=p2xy[7,2]
  p2xy[9,2]=.75*ac[3]*nc[2]
  p2xy[10,2]=p2xy[9,2]/3
  p2xy[11,2]=2*mc[2]*ac[1]*nc[2]
  p2xy[12,2]=.5*mc[1]*ac[2]*nc[2]
  p2xy[13,2]=p2xy[12,2]
  p2xy[14,2]=2*p2xy[13,2]
  p2xy[15,2]=mc[2]*ac[1]*nc[2]
  p2xy[16,2]=0
  p2xy[3,3]=mc[1]*nc[3]*(4*mc[1]+nc[1]-7)
  p2xy[4,3]=p2xy[12,2]
  p2xy[5,3]=p2xy[4,3]
  p2xy[6,3]=p2xy[5,3]+p2xy[4,3]
  p2xy[7,3]=mc[1]*ac[1]*nc[2]*(2*mc[1]+nc[1]-4)
  p2xy[8,3]=p2xy[7,3]
  p2xy[9,3]=0
  p2xy[10,3]=0
  p2xy[11,3]=2*mc[2]*nc[2]*(mc[1]+nc[1]-4)
  p2xy[12,3]=p2xy[12,2]/2
  p2xy[13,3]=p2xy[12,3]
  p2xy[14,3]=p2xy[12,2]
  p2xy[15,3]=2*p2xy[15,2]
  p2xy[16,3]=mc[3]*nc[2]
  p2xy[4,4]=.0625*ac[3]*nc[1]*(ac[1]+4*nc[1]-7)
  p2xy[5,4]=p2xy[4,4]
  p2xy[6,4]=p2xy[4,4]+p2xy[5,4]
  p2xy[7,4]=.25*mc[1]*ac[2]*nc[1]*(ac[1]+2*nc[1]-4)
  p2xy[8,4]=p2xy[7,4]
  p2xy[9,4]=.375*ac[4]*nc[1]
  p2xy[10,4]=p2xy[9,4]/3
  p2xy[11,4]=.25*mc[2]*ac[2]*nc[1]
  p2xy[12,4]=.25*mc[1]*ac[3]*nc[1]
  p2xy[13,4]=p2xy[12,4]
  p2xy[14,4]=p2xy[12,4]+p2xy[13,4]
  p2xy[15,4]=.5*mc[2]*ac[2]*nc[1]
  p2xy[16,4]=0
  
  for (i in 5:16) {p2xy[i,5]=p2xy[i,4]} 
  for (i in 6:16) {p2xy[i,6]=p2xy[i,4]+p2xy[i,5]}
  
  p2xy[7,7]=p2xy[14,2]+p2xy[11,2]/2+4*p2xy[11,4]
  p2xy[8,7]=p2xy[7,7]
  p2xy[9,7]=3*p2xy[12,4]
  p2xy[10,7]=p2xy[12,4]
  p2xy[11,7]=mc[2]*ac[1]*nc[1]*(mc[1]+2*nc[1]-4)
  p2xy[12,7]=.25*mc[1]*ac[2]*nc[1]*(ac[1]+2*mc[1]-4)
  p2xy[13,7]=p2xy[12,7]
  p2xy[14,7]=p2xy[12,7]+p2xy[13,7]
  p2xy[15,7]=mc[2]*ac[1]*nc[1]*(mc[1]+2*ac[1]-4)
  p2xy[16,7]=mc[3]*ac[1]*nc[1]
  
  for (i in 8:16) {p2xy[i,8]=p2xy[i,7]}
  
  p2xy[9,9]=.5625*ac[5]
  p2xy[10,9]=p2xy[9,9]/3
  p2xy[11,9]=0
  p2xy[12,9]=.375*mc[1]*ac[4]
  p2xy[13,9]=p2xy[12,9]
  p2xy[14,9]=p2xy[12,9]+p2xy[13,9]
  p2xy[15,9]=.75*mc[2]*ac[3]
  p2xy[16,9]=0
  
  for (i in 10:16) {p2xy[i,10]=p2xy[i,9]/3}
  
  p2xy[11,11]=mc[3]*nc[1]*(mc[1]+4*nc[1]-7)
  p2xy[12,11]=p2xy[11,6]
  p2xy[13,11]=p2xy[12,11]
  p2xy[14,11]=p2xy[12,11]+p2xy[13,11]
  p2xy[15,11]=4*mc[3]*ac[1]*nc[1]
  p2xy[16,11]=2*mc[4]*nc[1]
  p2xy[12,12]=.0625*mc[1]*ac[3]*(ac[1]+4*mc[1]-7)
  p2xy[13,12]=p2xy[12,12]
  p2xy[14,12]=p2xy[12,12]+p2xy[13,12]
  p2xy[15,12]=.5*mc[2]*ac[2]*(mc[1]+ac[1]-4)
  p2xy[16,12]=.25*mc[3]*ac[2]
  
  for (i in 13:16) {p2xy[i,13]=p2xy[i,12]} 
  for (i in 14:16) {p2xy[i,14]=p2xy[i,12]+p2xy[i,13]}
  
  p2xy[15,15]=mc[3]*ac[1]*(mc[1]+4*ac[1]-7)
  p2xy[16,15]=2*mc[4]*ac[1]
  p2xy[16,16]=mc[5]
  
  #NOW DIVIDE BY APPROPRIATE DENOMINATOR */
  
  for (i in 1:16) {px[i]=px[i]/g23} 
  for (j in 1:16) {for (i in j:16) {poxy[i,j]=poxy[i,j]/g26}}
  for (j in 1:16) {for (i in j:16) {p2xy[i,j]=p2xy[i,j]/g25}} 
  
  #THE COVARIANCE MATRIX */
  #I IS ROW */
  #J IS COLUMN */
  #print(px)
  #print(poxy)
  
  cov=array(0,c(16,16))
  
  for (j in 1:16)
  {cov[j,j]=(1-px[j])*g3*px[j]+3*(pg-3)*g3*(p2xy[j,j]-poxy[j,j])
  cov[j,j]=cov[j,j]+g32*(poxy[j,j]-px[j]^2)
  j1=j+1
  if (j<16)
  {for (i in j1:16)
  {cov[i,j]=-g3*px[i]*px[j]+3*(pg-3)*g3*(p2xy[i,j]-poxy[i,j])
  cov[i,j]=cov[i,j]+g32*(poxy[i,j]-px[i]*px[j])
  cov[j,i]=cov[i,j]}}} 
  
  #THE EXPECTATION VALUE OF TRIAD TYPE I */
  #print(cov)
  
  tx[,2]=g3*px 
  
  
  #ASSIGN COMPUTED COVARIANCE MATRIX TO OUTPUT ARRAY Y */
  #AND THE CORRELATION MATRIX IN ARRAY X */
  
  tx[,3]=sqrt(diag(cov))
  
  
  tx[,4]=l
  
  dummy=array(0,c(16,1))
  for (i in 1:16) {for (j in 1:16) {dummy[i]=dummy[i]+l[j]*cov[j,i]}} 
  
  tau1=0
  tau2=0
  tau3=0
  
  for (i in 1:16) {tau1=tau1+(l[i]*tx[i,1]); tau2=tau2+l[i]*tx[i,2]; tau3=tau3+l[i]*dummy[i]} 
  tau=(tau1-tau2)/sqrt(tau3)
  pvalue=2*(1-pnorm(abs(tau)))
  
  PARM<-matrix(c(0),nr=4,nc=1)
  PARM[1,1]<-nr
  PARM[2,1]<-sum(a)
  PARM[3,1]<-tau
  PARM[4,1]<-pvalue
  tx=format(tx,digits=2,format='f',sci=FALSE)
  
  
  
  cat("\n---------------------------------------------\n")
  cat("Triad Census\n")
  colnames(tx)=c('Observed','Expected','Std.Dev.','Weight')
  rownames(tx)=c("t003","t012","t102","t021D","t021U","t021C","t111D","t111U","t030T","t030C","t201","t120D","t120U","t120C","t210","t300")
  print.noquote(tx)
  cat("\n")
  
  cat("Triad census (statistics):\n")
  rownames(PARM)<-c("Nr of actors: ","Nr of arcs: ","Tau (z-score):","P-value")
  colnames(PARM)<-c(" ")
  printCoefmat(PARM,digits=max(10,getOption("digits")-3),scipen=100)
  cat("\n")
  
  
  
  
}