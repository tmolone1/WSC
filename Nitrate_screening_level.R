### Simplot Calculation for Nitrate site specific MTGW screening level

EqnB<-function(MCL, DAF, Kd, thetaw, thetaa, Hprime, rhob) {
  Ct = (MCL*DAF)*(Kd+(thetaw+thetaa*Hprime)/rhob)
  return(Ct)
}
DAFEqn<-function(K,i,d,I,L) {
  DAF = 1 + (K*i*d)/(I*L)
  return(DAF)
}
Eqn4<-function(L,da,I,K,i) {
  d = (0.0112*L^2)^0.5 + da*(1-exp(1)^((-L*I)/(K*i*da)))
  return(d)
}
da<-42.7 #depth of aquifer, meters
I<-0.01057 #infiltration, meters/year
K<-6812 #hydraulic conductivity, meters/year
i<-0.0023 #average hydraulic gradient
L<-100 #length of source parallel to groundwater flow (site specific) 
d<-Eqn4(L,da,I,K,i)
DAF<-DAFEqn(K,i,d,I,L)
MCL<-10 #nitrate, mg/L
Kd<-0
thetaw<-0.35
thetaa <- 0.13
Hprime <- 0
rhob <- 1.5
Ct<-EqnB(MCL, DAF, Kd, thetaw, thetaa, Hprime, rhob)
Ct

MCL<-0.5 #ammonia, mg/L
Kd<-0
thetaw<-0.35
thetaa <- 0.13
Hprime <- 0
rhob <- 1.5
Ct<-EqnB(MCL, DAF, Kd, thetaw, thetaa, Hprime, rhob)
Ct


###WSC site specifc calculation
da<-42.7 #depth of aquifer, meters, SIMPLOT
I<-0.01057 #infiltration, meters/year, SIMPLOT
K<-6812 #hydraulic conductivity, meters/year, SIMPLOT
i<-0.0018 #average hydraulic gradient, WSC SITE SPECIFIC
L<-866 #length of source parallel to groundwater flow, WSC SITE SPECIIFC
d<-Eqn4(L,da,I,K,i)
DAF<-DAFEqn(K,i,d,I,L)
MCL<-10 #nitrate, mg/L
Kd<-0 #assumed
thetaw<-0.3 #FACT SHEET 25 default
thetaa <- 0.13 #FACT SHEET 25 default
Hprime <- 0 #FACT SHEET 25 default
rhob <- 1.5 #FACT SHEET 25 default
Ct<-EqnB(MCL, DAF, Kd, thetaw, thetaa, Hprime, rhob)
Ct

MCL<-0.5 #ammonia, mg/L
Kd<-0.82 #L/kg, Trihydro 1995?/2018 - WSC Specific
thetaw<-0.3 #FACT SHEET 25 default
thetaa <- 0.13 #FACT SHEET 25 default
Hprime <- 0 #FACT SHEET 25 default
rhob <- 1.5 #FACT SHEET 25 default
Ct<-EqnB(MCL, DAF, Kd, thetaw, thetaa, Hprime, rhob)
Ct
