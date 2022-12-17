integer MAXNGC, MAXNC, NCOL,MAXMAT,MAXTR,MAXR,MAXEL,MAXNG,MAXMIX,MAXT,MAXPV 

Parameter(MAXNGC = 600)
Parameter(MAXNC = 300)
Parameter(NCOL = 8)
Parameter(MAXMAT = 50)
Parameter(MAXTR = 40)
Parameter(MAXR = 24)
Parameter(MAXEL = 20)
Parameter(MAXNG = 500)
Parameter(MAXMIX = 52)
Parameter(MAXT = 51)
Parameter(MAXPV = 26)

integer IOINP, IOOUT, IOSCH, IOTHM, IOPLT, IOTRN
Parameter(IOINP = 7)
Parameter(IOOUT = 8)
Parameter(IOSCH = 13)
Parameter(IOTHM = 14)
Parameter(IOPLT = 15)
Parameter(IOTRN = 18)

integer Nreac,Nomit, Nonly, Nplt, Nsert, Iplt, Ip, It, Nc, Ng, Ngpl, Nlm, Nof
integer Np, Npr, Npt, Ngc, Nspr, Nspx, Nt
integer Jcond(45), Jx(MAXEL), Nfla(MAXR), Ifz(MAXNC)
integer Imat, Iql, Isv, Jliq, Jsol, Lsave, Msing
integer Jray(MAXR)
integer Iopt,Isup,Nfz,Npp,Nsub,Nsup
integer Nsk

Real*8 Enn, Ennl, Enlsav, Ensave, Sumn
Real*8 Deln(MAXNGC), Enln(MAXNGC), Sln(MAXNGC)
Real*8 En(MAXNGC, NCOL)
Real*8 Cpmix, Wmix, Bcheck
Real*8 Am(2), Hpp(2), Vmin(2), Vpls(2), Wp(2), Atmwt(100), Oxf(MAXMIX)
Real*8 P(MAXPV), Rh(2), T(MAXT), V(MAXPV), Valnce(100)
Real*8 BOp(MAXEL,2)

REAL*8 Avgdr,Boltz,Eqrat,Hsub0,Oxfl,Pi,Pp,R,Rr,Size,S0,Tln,Tm
Real*8 Trace,Tt,Viscns,Vv
REAL*8 Atwt(MAXEL),B0(MAXEL),X(MAXMAT)
REAL*8 A(MAXEL,MAXNGC),G(MAXMAT,MAXMAT+1)

REAL*8 Acat,Awt,Cstr,Tcest,Ma
REAL*8 Aeat(NCOL),App(NCOL),Pcp(2*NCOL),Sonvel(NCOL),Spim(NCOL)
Real*8 Subar(13),Supar(13),Vmoc(NCOL)

Real*8 Dens(MAXR),Enth(MAXR),Pecwt(MAXR),Rmw(MAXR),Rtemp(MAXR)
Real*8 Rnum(MAXR, 12)
REAL*8 U1(NCOL),Mach1(NCOL),A1,Gamma1

Logical Massf, short, Siunit, Debug(NCOL), Newr, Trnspt
Logical Detdbg
Logical Tp, Hp, Sp, Vol, Eql, Shock
Logical Detn, Ions, Moles
LOGICAL Area,Debugf,Fac,Froz,Page1,Rkt
LOGICAL Incdeq,Incdfz,Refleq,Reflfz,Shkdbg

character*200 Pfile
CHARACTER*2 Elmt(MAXEL),Ratom(MAXR,12),Symbol(100)
CHARACTER*4 Fmt(30)
CHARACTER*8 Fox(MAXR)
CHARACTER*10 Thdate
character*15 Case, Energy(MAXR), Prod(0:MAXNGC), Omit(0:MAXNGC), Pltvar(20), Rname(MAXR)