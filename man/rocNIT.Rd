\name{rocNIT}
\alias{rocNIT}
\title{Non-Inferiority Test for Paired ROC Curves}
\description{
  Non-inferiority test and diagnostic test are very important in clinical trails.
  This package is to get a p value of the non-inferiority test for ROC curves from diagnostic test using the bootstrap method.

}
\keyword{Non-inferiority test, receiver operating characteristic (ROC)}
\usage{
rocNIT(BNO,DATA,cVAL1,cVAL2,cGOLD,DELTA,ALPHA,seed)
}
\arguments{
  \item{BNO}{the times of the sampling of bootstrap}
  \item{DATA}{the dataset including 3 variables at least: VAL1,VAL2,GOLD}
  \item{cVAL1}{the column number of the first numeric variable}
  \item{cVAL2}{the column number of the second numeric variable}
  \item{cGOLD}{the column number of the golden variable which should be binary}
  \item{DELTA}{the non-inferiority margin, default is 0.05}
  \item{ALPHA}{the significant level, default is 0.05}
  \item{seed}{the seed for the sampling of bootstrap, default is 2016}
}
\value{
  \item{AREA1}{the AUC for the first variable}
  \item{AREASE1}{the SE of the AUC for the first variable}
  \item{AREA2}{the AUC for the second variable}
  \item{AREASE2}{the SE of the AUC for the second variable}
  \item{bCIL}{the lower limit of CI from bootstrap sampling}
  \item{bCIU}{the upper limit of CI from bootstrap sampling}
  \item{Z}{the statistic to get the p value}
  \item{P}{the p value of the non-inferiority test, P < ALPHA means a significant non-inferiority}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Referenced from Jen-Pei Liu et al. Tests of equivalence and non-inferiority for diagnostic accuracy based on the paired areas under ROC curves. STATISTICS IN MEDICINE. DOI: 10.1002/sim.2358
}
\seealso{
nothing
}
\examples{
VAL1=c(rnorm(50,mean=0),rnorm(50,mean=0)+0.5)
VAL2=c(rnorm(50,mean=0),rnorm(50,mean=0)+1)
GOLD=rep(1:2,each=50)
data=data.frame(GOLD,VAL1,VAL2)
rocNIT(BNO=200,DATA=data,cVAL1=2,cVAL2=3,cGOLD=1,DELTA=0.05,ALPHA=0.05,seed=2016)
}

