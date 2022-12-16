    !  M_CEA_2.f90
    !
    !  FUNCTIONS:
    !  M_CEA_2 - Entry point of console application.
    !

    !****************************************************************************
    !
    !  PROGRAM: M_CEA_2
    !
    !  PURPOSE:  Entry point for the console application.
    !
    !****************************************************************************

    program M_CEA_2

    implicit none
    include 'Cea.inc'

    ! Variables
    character*15 ensert(20)
    character*200 infile, ofile
    character*196 prefix
    logical caseok, ex, readok
    integer i, inc, iof, j, ln, n
    integer INDEX
    real*8 xi, xln
    real*8 DLOG

    Save caseok,ensert,ex,i,inc,infile,Pfile,iof,j,ln,n,ofile,prefix,readok,xi,xln

    Write(*, 99001)
    Read(*, 99002)prefix
    ln = INDEX(prefix, ' ') - 1
    infile = prefix(1:ln)//'.inp'
    ofile = prefix(1:ln)//'.out'
    Pfile = prefix(1:ln)//'.plt'
    inquire(File = infile, Exist=ex)
    if(.not.ex)then
        print *, infile, 'file not found'
        Goto 400
    end if

    if(ex)then
        print *, infile, 'file found'
    end if

    open (IOINP, File = infile, Status = 'old', Form = 'formatted')
    open (IOOUT, File = ofile, Status = 'unknown', Form = 'formatted')
    open (IOSCH, Status = 'scratch', Form = 'unformatted')

    Write ( IOOUT, 99006)
    Write ( IOOUT, 99007)

    readok = .true.
    Newr = .false.
100 Iplt = 0
    Nplt = 0

    CALL INPUT(readok, caseok, ensert)

300 Close (IOINP)
    Close (IOOUT)
    Close (IOSCH)

    Read(*,*)

400 stop

99001 FORMAT (//'Enter file name'//)
99002 FORMAT (a)
99003 FORMAT (1X, A16, 'Inserted')

99006 FORMAT (/' ***************************')
99007 FORMAT (/, 9x, 'NASA=Glenn CEA')

    end program M_CEA_2

    SUBROUTINE INPUT(readok, caseok, ensert)
    implicit none
    include 'Cea.inc'
    ! dummy arguments
    logical readok, caseok
    logical turnoff
    character*15 ensert(20)
    ! local variables
    character*15 cin(MAXNGC), cx15
    character*4 code, cx4
    character*1 cx1
    character*2 cx2
    character*3 cx3
    character*26 lc, uc
    logical eqrats, incd, phi, pltdat, reacts, refl
    integer i, ifrmla, ii, in, iv, ix, j, jj, k, lcin(MAXNGC), ncin, nmix
    integer index
    real*8 denmtr, dpin(MAXNGC), eratio, hr, mix(MAXNGC), ur, xyz
    real*8 DABS, DMIN1, DSQRT
    Save cin, code, cx1, cx15, cx2, cx3, cx4, denmtr, dpin, eqrats, eratio, hr, i
    Save ifrmla, ii, in, incd, iv, ix, j, jj, k, lcin, mix, ncin, nmix, phi, pltdat
    Save reacts, refl, ur, xyz

    turnoff = .true.
    if(turnoff)then
        print *, 'Entered Input subroutine'
    end if

    DATA uc/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
    DATA lc/'abcdefghijklmnopqrstuvwxyz'/
    write (IOOUT, 99001)
    caseok = .true.
    Nonly = 0
    Nomit = 0
    Nsert = 0
    reacts = .false.
    Trace = 0
    Short = .false.
    Massf = .false.
    Do i = 1, NCOL
        Debug(i) = .false.
    end do
    Nplt = 0
    Siunit = .true.
    pltdat = .false.

    print *, 'calling infree'

    Call INFREE(readok, cin, ncin, lcin, dpin)

99001 FORMAT(/,/)

    END

    SUBROUTINE INFREE(readok, cin, ncin, lcin, dpin)
    implicit none
    include 'Cea.inc'
    ! dummy arguments
    character*15 cin(MAXNGC)
    integer ncin
    integer lcin(MAXNGC)
    logical readok
    real*8 dpin(MAXNGC)
    logical turnoff
    ! local variables
    character*1 ch1(132), cx, nums(13)
    character*24 cnum
    character*3 fmtl(3)
    
    ! TODO change back to 24
    character*2 numg(24)
    character*4 wl
    integer i, ich1, j, kcin, nb, nch1, nx
    data fmtl/'(g', '16', '.0)'/
    data nums/'+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'/
    data numg/'1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24'/
    Ncin = 1
    Lcin(1) = 0
    kcin = 0
    Dpin(1) = 0

100 nb = 1
    nx = 0
    cnum = ''
    Cin(Ncin) = ''
    ch1(1) = ' '
    nch1 = 1

    print *, nch1, 'nch1'
    END


