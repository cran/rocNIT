rocNIT=function(BNO=200,DATA=data,cVAL1=3,cVAL2=2,cGOLD=1,DELTA=0.05,
                ALPHA=0.05,seed=2016){

  set.seed(seed)

  DATA[,cGOLD]=as.factor(DATA[,cGOLD])
  DATA[,cGOLD]=as.numeric(DATA[,cGOLD])

  VAL1=DATA[,cVAL1]
  VAL2=DATA[,cVAL2]
  GOLD=DATA[,cGOLD]

  XBAR1=mean(VAL1[which(GOLD==1)]);SA1=sd(VAL1[which(GOLD==1)])
  XBAR2=mean(VAL2[which(GOLD==1)]);SA2=sd(VAL2[which(GOLD==1)])
  YBAR1=mean(VAL1[which(GOLD==2)]);SN1=sd(VAL1[which(GOLD==2)])
  YBAR2=mean(VAL2[which(GOLD==2)]);SN2=sd(VAL2[which(GOLD==2)])

  DLAMBDA=(XBAR1-YBAR1)/sqrt(SA1^2+SN1^2)-(XBAR2-YBAR2)/sqrt(SA2^2+SN2^2)
  AREA1=pnorm((XBAR1-YBAR1)/sqrt(SA1^2+SN1^2))
  AREA2=pnorm((XBAR2-YBAR2)/sqrt(SA2^2+SN2^2))

  VALUEB=data.frame(XBAR1,XBAR2,YBAR1,YBAR2,DLAMBDA,AREA1,AREA2)

  VALUE2=VALUEB
  VALUE2[1,]=NA

  for(i in 1:BNO){
    DATA1=DATA[which(GOLD==1),]
    DATA2=DATA[which(GOLD==2),]
    BOOTSAMPLE=rbind(DATA1[sample(1:nrow(DATA1),nrow(DATA1),replace=TRUE),],
                     DATA2[sample(1:nrow(DATA2),nrow(DATA2),replace=TRUE),])

    VAL1=BOOTSAMPLE[,cVAL1]
    VAL2=BOOTSAMPLE[,cVAL2]
    GOLD=BOOTSAMPLE[,cGOLD]

    XBAR1=mean(VAL1[which(GOLD==1)]);SA1=sd(VAL1[which(GOLD==1)])
    XBAR2=mean(VAL2[which(GOLD==1)]);SA2=sd(VAL2[which(GOLD==1)])
    YBAR1=mean(VAL1[which(GOLD==2)]);SN1=sd(VAL1[which(GOLD==2)])
    YBAR2=mean(VAL2[which(GOLD==2)]);SN2=sd(VAL2[which(GOLD==2)])

    DLAMBDA=(XBAR1-YBAR1)/sqrt(SA1^2+SN1^2)-(XBAR2-YBAR2)/sqrt(SA2^2+SN2^2)
    AREA1 =pnorm((XBAR1-YBAR1)/sqrt(SA1^2+SN1^2))
    AREA2 =pnorm((XBAR2-YBAR2)/sqrt(SA2^2+SN2^2))

    VALUE1=data.frame(XBAR1,XBAR2,YBAR1,YBAR2,DLAMBDA,AREA1,AREA2)
    VALUE2=rbind(VALUE2,VALUE1)
  }
  VALUE2=VALUE2[-1,]

  DLAMBDASE=sd(VALUE2$DLAMBDA)
  AREASE1=sd(VALUE2$AREA1)
  AREASE2=sd(VALUE2$AREA2)

  DL=VALUEB$DLAMBDA+qnorm(ALPHA)*DLAMBDASE
  DU=VALUEB$DLAMBDA-qnorm(ALPHA)*DLAMBDASE
  Z=(VALUEB$DLAMBDA+qnorm(VALUEB$AREA2)-qnorm(VALUEB$AREA2-DELTA))/DLAMBDASE
  P=1-pnorm(Z)

  LAST=data.frame(AREA1=VALUEB$AREA1,AREASE1,AREA2=VALUEB$AREA2,AREASE2,
                  bCIL=DL,bCIU=DU,Z=Z,P=P)

  return(LAST)
}
