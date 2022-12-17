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
!*******************************************
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

100 Call INFREE(Readok, cin, ncin, lcin, dpin)
    if( .NOT.Readok) GOTO 400
    code = cin(1)
    if(code.NE.'    ')then
        if(code.EQ.'only')then
            Nonly = MIN(MAXNGC, ncin-1)
            Do i = 1, Nonly
                Prod(i) = cin(i+1)
            EndDo
        ElseIf(code.EQ.'inse')then
            Nsert = MIN(20, ncin-1)
            Do i = 1, Nsert
                Ensert(i) = cin(i+1)
            EndDo
        ElseIf(code.EQ.'omit')then
            Nomit = MIN(MAXNGC, ncin-1)
            Do i = 1, Nomit
                Omit(i) = cin(i+1)
            EndDo
        Elseif (code.EQ.'ther')then
            Newr = .true.
            REWIND IOTHM
            Call UTHERM(Readok)
            if( .NOT.Readok)then
                Write (IOOUT, 99025)
            GOTO 400
            end if
        Elseif (code.EQ'tran')then
            Call UTRAN(readok)
            if( .NOT.Readok)then
                Write (IOOUT, 99025)
                GOTO 400
            end if
            Elseif (code.EQ.'outp')then
            DO 120 i = 2, ncin
                if(lcin(i).LT.0)then
                    cx2 = cin(i) (1:2)
                    cx3 = cin(i) (1:3)
                    cx4 = cin(i) (1:4)
                    if(cx3.EQ'cal')then
                        Siunit = .false.
                    Elseif (cx4.EQ.'tran'.OR.cx3.EQ.'trn')then
                        Trnspt = .true.
                    Elseif(cx4.EQ.'trac')then
                        Trace = dpin(i+1)
                    Elseif ( cin(i)(:5).EQ.'short' )then
                        Short = .true.
                    Elseif ( cin(i)(:5).EQ.'massf' )then
                        Massf = .true.
                    Elseif ( cx3.EQ.'deb'.OR.cx3.EQ.'dbg' )then
                        DO j = i + 1,ncin
                            IF ( lcin(j).NE.i ) GOTO 120
                            k = dpin(j)
                            IF ( k.LE.NCOL ) Debug(k) = .true.
                            lcin(j) = 0
                        EndDo
                    Elseif ( cx2.EQ.'si' )then
                        Siunit = .true.
                    Elseif ( pltdat.AND.Nplt.LT.20 )then
                        Nplt = Nplt + 1
                        Pltvar(Nplt) = cin(i)
                    Elseif ( cx2.EQ.'pl' )then
                        pltdat = .true.
                    ELSE
                        WRITE (IOOUT,99002) cin(i)
                    ENDIF
                Endif
                
120 Continue                

400 Return    
99001 FORMAT(/,/)

    END
!***************************************************
    SUBROUTINE INFREE(readok, cin, Ncin, lcin, dpin)
    implicit none
    include 'Cea.inc'
    ! dummy arguments
    character*15 cin(MAXNGC)
    integer Ncin
    integer Lcin(MAXNGC)
    logical Readok
    real*8 Dpin(MAXNGC)
    !logical turnoff
    ! local variables
    character*1 ch1(132), cx, nums(13)
    character*24 cnum
    character*3 fmtl(3)
    character*2 numg(24)
    character*4 wl
    integer i, ich1, j, kcin, nb, nch1, nx
    data fmtl/'(g', '16', '.0)'/
    data nums/'+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'/
    data numg/'1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24'/
    Ncin = 1
    Lcin(1) = 0
    kcin = 0
    Dpin(1) = 0.D0

100 nb = 1
    nx = 0
    cnum = ''
    Cin(Ncin) = ' '
    ch1(1) = ' '
    nch1 = 1

    print *, nch1, 'nch1'
    !   read characters 1 at a time
    Read(IOINP, 99001, END=500, ERR=500)ch1
    ! find first and last non-blank character
    DO i = 132,1, -1
        nch1 = i
        if(ch1(i).NE.' '.AND.ch1(i).NE.' ') GOTO 200
    ENDDO
    
200 DO i = 1, nch1
        ich1 = i
        if(ch1(i).NE.' '.AND.ch1(i).NE.' ') GOTO 300
    ENDDO
    
300 if(nch1.EQ.1.OR.ch1(ich1).EQ.'#'.OR.ch1(ich1).EQ.'!')then
        write(IOOUT, 99002) (ch1(i),i=1, nch1)
        GOTO 100
        end if
    
500 readok = .false.    
    
99001 FORMAT (132A1)    
99002 FORMAT (1x, 80A1)    
99025 FORMAT (/' Fatal error in dataset (INPUT)')      
    END
!***********************************************
    SUBROUTINE UTHERM(readok)
    Stop
    END
    
    !*******************************************
    SUBROUTINE UTRAN(readok)
    Stop
    END
    !********************************************


