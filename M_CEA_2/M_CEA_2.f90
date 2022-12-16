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
    
    Save caseok, ensert, ex, i, inc, infile, Pfile, iof, j, ln, n, ofile, prefix, readok, xi, xln
    
    Write(*, 99001)
    Read(*, 99002)prefix
    ln = INDEX(prefix, ' ')-1
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
    
    readok = .true.
    Newr = .false.
    

    ! Body of M_CEA_2
    
    
    Read(*,*)
    
400 stop    
    
99001 FORMAT (//'Enter file name'//)
99002 FORMAT (1X, A16, 'Inserted')      

    end program M_CEA_2

