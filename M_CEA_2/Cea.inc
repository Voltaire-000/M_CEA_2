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

Real*8 Enn, Ennl, Enlsav, Ensave, Sumn
Real*8 Deln(MAXNGC), Enln(MAXNGC), Sln(MAXNGC)
Real*8 En(MAXNGC, NCOL)
Real*8 Trace
Real*8 Cpmix, Wmix, Bcheck
Real*8 Am(2), Hpp(2), Vmin(2), Vpls(2), Wp(2), Atmwt(100), Oxf(MAXMIX)
Real*8 P(MAXPV), Rh(2), T(MAXT), V(MAXPV), Valnce(100)
Real*8 BOp(MAXEL,2)

Logical Massf, short, Siunit, Debug(NCOL), Newr, Trnspt
Logical Debugf, Shkdbg,Detdbg
Logical Tp, Hp, Sp, Vol, Rkt, Eql, Fac, Froz, Shock
Logical Detn, Ions, Moles

character*200 Pfile
character*15 Prod(0:MAXNGC), Omit(0:MAXNGC), Pltvar(20)