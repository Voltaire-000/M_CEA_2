integer MAXNGC, NCOL, Nomit, Nonly, Nplt, Nsert, Iplt

Real*8 Trace

Parameter(MAXNGC = 600)
Parameter(NCOL = 8)

integer IOINP, IOOUT, IOSCH, IOTHM, IOPLT, IOTRN
Parameter(IOINP = 7)
Parameter(IOOUT = 8)
Parameter(IOSCH = 13)
Parameter(IOTHM = 14)
Parameter(IOPLT = 15)
Parameter(IOTRN = 18)

Logical Massf, short, Siunit, Debug(NCOL), Newr, Trnspt

character*200 Pfile
character*15 Prod(0:MAXNGC), Omit(0:MAXNGC)