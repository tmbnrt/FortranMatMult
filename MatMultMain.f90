program MatMult90
    implicit none

    integer::ioin  = 10                     ! input channel
    integer::ioout = 11                     ! output channel (not required)

    character(256)::infile = 'mmult.in'     ! string: input file name
!   character(256)::outfile = 'mmult.out'

    integer::mattotal                       ! total number of matrices
    integer::matcount                       ! matrix counter
    integer::mult                           !
    integer::i,j,x                          ! loop helpers

    real(8),allocatable,dimension(:,:)::a
    real(8),allocatable,dimension(:,:)::b
    real(8),allocatable,dimension(:,:)::c

    integer,dimension(2)::nDimA
    integer,dimension(2)::nDimB
    integer,dimension(2)::nDimC

    integer::ioerr

    integer::IREADMATNUMB,IREADMATDIM,IREADMAT

    mult     = 0
    matcount = 1
    !default filenames

    if (iargc()>0) then
         call getarg(1,infile)
    end if

!    if(iargc()>1) then
!            call getarg(2,outfile)
!    end if

    write(*,*) "input file :",infile(1:len_trim(infile))
    !write(*,*) "output file:",outfile(1:len_trim(outfile))


    open(ioin,file=infile,status='old',iostat=ioerr)
    if (ioerr/=0)then
        write(*,*) "*** Error :", infile(1:len_trim(infile)),"cannot be opened ***"
        stop
    end if

    !open(ioout,file=outfile,status='replace',iostat=ioerr)
    !if (ioerr/=0) then
    !    write(*,*)"*** Error:",outfile(1:len_trim(outfile)),"can't be created"
    !end if

    if (IREADMATNUMB(ioin,mattotal) .ge. 1)then
        write(*,*)" *** Error: Could not read the total number of matrices! ***"
    end if

    if (mattotal .le. 1)then
        write(*,*)" *** Error: Invalid number of matrices! ***"
        stop
    end if

    write(*,'(a,i3)') "Number of multiplications:",(mattotal-1)
    write(*,'(a,i3)') "Number of matrices:",mattotal


    !if(.not.allocated(a))then
    !    write(*,*) "Array is not allocated"
    !end if

    ioerr  = IREADMATDIM(ioin,nDimA)
    if(ioerr/=0) then
        write(*,*) " *** Error reading matrix Dimensions of A,code:",ioerr,"! ***"
        stop
    end if

    allocate(a(nDimA(1),nDimA(2)),stat=ioerr)
    if (ioerr/=0) then
        write(*,*) "*** Error: Matrix A is not allocatable! ***"
    end if

    if (allocated(a)) then
        !write(*,*) "Array A is allocated"
        write(*,*) "  "
    else
        write(*,*) "*** Error: Array A is not allocated! ***"
    end if

!______________________List dimensions of matrix A_____________________
    !write(*,'(a,i3)') "Size of A ......:",size(a)
    !write(*,'(a,i3)') "Size of A(x,)...:",size(a,dim=1)
    !write(*,'(a,i3)') "Size of A(,x)...:",size(a,dim=2)
    !write(*,'(4(a,i3))')"Index range of A.....:",  &
    !                     lbound(a,dim=1),':',ubound(a,dim=1),&
    !                 ",",lbound(a,dim=2),':',ubound(a,dim=2)
!______________________________________________________________________

    ioerr = IREADMAT(ioin,nDimA,a)
    if (ioerr/=0) then
        write(*,*) "*** Error reading Matrix A:"
        stop
    end if

    call LISTMAT(ioout,"Matrix A:",nDimA,a)



!   RUN THE LOOP
    do x = 1,(mattotal-1)

      matcount = matcount + 1

      ioerr  = IREADMATDIM(ioin,nDimB)
      if(ioerr/=0) then
          write(*,*) " *** Error reading matrix Dimensions of B,code:",ioerr,"! ***"
          stop
      end if

      allocate(b(nDimB(1),nDimB(2)),stat=ioerr)
      if (ioerr/=0) then
           write(*,*) "*** Error Matrix B is not allocatable ***"
      end if

      if (allocated(b)) then
           write(*,*) "   "
      else
           write(*,*) "*** Error: Array B is not allocated! ***"
      end if


!______________________List dimensions of matrix B_____________________
   !!    write(*,'(a,i3)') "size of b ......:",size(b)
   !!    write(*,'(a,i3)') "size of b(x,)...:",size(b,dim=1)
   !!    write(*,'(a,i3)') "size of b(,x)...:",size(b,dim=2)
   !!    write(*,'(4(a,i3))')"index range of b.....:",  &
   !!                         lbound(b,dim=1),':',ubound(b,dim=1),&
   !!                     ",",lbound(b,dim=2),':',ubound(b,dim=2)
!______________________________________________________________________


      ioerr = IREADMAT(ioin,nDimB,b)
      if (ioerr/=0) then
          write(*,*) "*** Error reading Matrix B:"
          stop
      end if

      call LISTMATB(ioout,matcount,"-> Matrix B",nDimB,b)


!------------------------------------------------------------------------------------
!Error check for the indices 2nd dimension of A = first Dimension of B
!------------------------------------------------------------------------------------


       !check the dimensions
      if (nDimA(2)/=nDimB(1)) then
        write(*,*) "*** Error: Dimensions in the input file! ***"
        stop
      end if


      allocate(c(nDimA(1),nDimB(2)),stat=ioerr)
      if (ioerr/=0) then
        write(*,*) "*** Error: Matrix C is not allocatable ***"
      end if

      write(*,*) ""

!     Matrix Multiplication
      nDimC(1) = nDimA(1)
      nDimC(2) = nDimB(2)

      c = matmul(a,b)

      ! next multiplication
      mult = mult + 1



      if (mult .lt. mattotal-1) then
         call LISTMAT(ioout,"Intermediate Matrix C:",nDimC,c)
      else
         write(*,*) "_____________________________"
         call LISTMAT(ioout,"Final Matrix Product C:",nDimC,c)
         write(*,*) "_____________________________"
      endif

      ! deallocate matrices A and B
      deallocate(a)
      deallocate(b)

      ! set new dimensions for matrix A
      nDimA(1) = nDimC(1)
      nDimA(2) = nDimC(2)

      ! allocate the memory for the new matrix A
      allocate(a(nDimA(1),nDimA(2)),stat=ioerr)
      if (ioerr /= 0) then
          write(*,'(a,i3,a)') "*** Error: Allocation of Matrix ",(mattotal+1)," failed! ***"
          stop
      endif


      ! Write C to A
      do i = 1,nDimC(1)
         do j = 1,nDimC(2)
                 a(i,j) = c(i,j)
         end do
      end do



   !! if (allocated(c)) write (*,*) "*** Array c is not deallocated!"

      deallocate(c,stat=ioerr)

    end do   !end main loop



end program
