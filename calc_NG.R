require(graphics); require(stats)
require(utils)
## ggplot--
library("proto",lib.loc="/Users/josem/software/Rsoftware/")
library("ggplot2")
library("nls2",lib.loc="/Users/josem/software/Rsoftware/")


### Para sacar resoluciones masa--


#################################################
### Constantes de transformacion de Gadget!!--
#################################################

N=300000.0

ng=(N^(1/6.0)/7.23)^(0.33)
ng

### De Ruslan --
x=c(4321,6121,8673,12289,17412)
y=c(300000.,600000.,1.2e6,2.4e6,4.8e6)

### Para lo mio --
#x=c(261,732,1337)
#y=c(1.8e6,4.3e6,8.9e6)

## 
#ynew=x^(1.5)
#ynew2=(7.23*x^(0.33))^(-1.0/6.0)



#### Seccion de fitteo --
######################################################
### Un grid de busqueda..#############################
######################################################

###
ai = 8.0
af = 2.e1
step_a00=1.0
###
bi=0.0
bf=3.0
step_b00=0.5
###
ci = 0.5
cf = 2.5
#ci = 0.1
#cf = 2.5
step_c00=0.08
###
grid.Observ1 <- expand.grid(list (
a = seq(ai, af, by = step_a00),
b = seq(bi, bf, by = step_b00),
c = seq(ci, cf, by = step_c00)))

######################################################
######################################################
######################################################

## Mi formula aqui!!. ################
myformula1 <- y~a+b*x^c
### Defino unos errores aqui --
y.err<-sqrt(y)

print("Las X's")
x

fit1 <- nls2(myformula1,
  weigths=y.err,
  control=nls.control(maxiter = 2000, tol = 1e-10, minFactor = 1.e-20,
  printEval = F, warnOnly = T),
  start=grid.Observ1, alg = "plinear-brute",
#  start=grid.Observ1, alg = "plinear",
  trace=F)

summary(fit1)


### Saco los Parametros --
utils::str(fit1)
#### Best a,b,c
a_best      <- summary(fit1)$coefficients[1, 1]
b_best      <- summary(fit1)$coefficients[2, 1]
c_best      <- summary(fit1)$coefficients[3, 1]
my_lin      <- summary(fit1)$coefficients[4, 1]
print(c("Los parametros:",a_best,b_best,c_best,my_lin))
###
# Nueva forma de predecir!!!...
#
xmodel = seq(0.,18000.0, by = 100. )
##########################################################
##########################################################
### Para ver --
### .lin es para toda la expresion!!--
ymodel <- my_lin * (a_best + b_best * xmodel^c_best)
print("Para probar!!:")

### Para ver los vecinos de 10 mil--
my_ng=((600000/my_lin-a_best)/b_best)^(1.0/c_best)
cat("El numero de vecinos: ",my_ng)
### paper sigalotti --
my_N=625
my_ng2=2.81*my_N^(0.675)
print(my_ng2)


################################################################################
################################################################################
## Abre un dispositivo..

file_root<-"NvsNG"

myfile_wt1<-paste(as.character(file_root),"_Ave",
".eps",sep = "", collapse = NULL)
#pdf(myfile_wt1,width=14.,height=14.,pointsize=18)
postscript(myfile_wt1, width=9.,height=9.,pointsize=15,
           onefile = FALSE,
           horizontal=F,
           encoding = "TeXtext.enc")
####
#### Sector de Ploteo!!
####

par(mfrow=c(1,1))
## margin for side 2 is 7 lines in size
op <- par(mar = c(5,6,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
####################################
####################################
xmin1=0.
xmax1=18000.0
ymin1=0.
ymax1=9.e6

plot(x, y, type="p", lwd=.5,
pch=22, col="black",
cex.axis=2.0,
cex = 1.5,
xlim=c(xmin1,xmax1),
ylim=c(ymin1,ymax1),
ylab=expression(paste("N")),
xlab=expression(paste(n[gb])),
)

###
#lines(x,ynew,col="red",lwd=5.0)
#lines(x,ynew2,col="blue",lwd=5.0)
## Los errores aqui!!..#######
#####
lines(xmodel,ymodel,col="red",lwd=5,lty=2)



##########################################################
##########################################################
### Para ver -- (2)
### .lin es para toda la expresion!!--

#ymodel <- my_lin * (a_best + b_best * xmodel^c_best)
#print("Para probar!!:")


#################################
#### Super calculo de NG
#[1] "Los parametros:"   "20"                "0.5"              
#[4] "2.02"              "0.026137151696438"
#mylin=0.026137151696438
mylin=0.35
mya=20.0
myb=0.5
myc=2.02
###
NCube=c(1.8e6,4.3e6,8.9e6)
myNg=((NCube/mylin-mya)/(myb))^(1.0/myc)
print("Para probar!!: (2)")
myNg
NCube1=c(261,732,1337)
ymodel1 <- my_lin * (a_best + b_best * NCube1^c_best)
#lines(NCube1,NCube,col="blue",lwd=5,lty=2)
points(NCube1,NCube,lwd=3,pch=4,col="blue")
###
myc=2.53
myNg=((NCube/mylin-mya)/(myb))^(1.0/myc)
lines(myNg,NCube,col="black",lwd=5,lty=1)
#################################
#################################
