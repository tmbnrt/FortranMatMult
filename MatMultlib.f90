integer function IREADMATNUMB(io,matnumb)
    integer::io
    integer::matnumb

    read(io,*,iostat=ioerr)matnumb
    if(ioerr /= 0) then
        IREADMATNUMB = 1
        return
    end if


    if (matnumb<1 .or. matnumb<2)then
       ! write(*,*) "*** Error matnumb should be positive "
       IREADMATNUMB = -2
    end if

    IREADMATNUMB = 0
end function


integer function IREADMATDIM(io,nDim)
    implicit none
    integer::io
    integer,dimension(2)::nDim
    integer::ioERR
    read(io,*,iostat=ioERR) nDim(1),nDim(2)
    if (ioERR/=0)then
        IREADMATDIM = -3
        return
    end if



  if (nDim(1)<1 .or. nDim(2)<1) then
    IREADMATDIM = -4
    return
  end if

  IREADMATDIM = 0

end function


integer function IREADMAT(io,nDim,mat)
    integer::io
    integer::nDim(2)
    real(8),dimension(nDim(1),nDim(2))::mat
    integer::ioERR,i,j

    do i = 1,nDim(1)
        read(io,*,iostat=ioERR) (mat(i,j),j=1,nDim(2))
        if (ioERR/=0) then
            IREADMAT = -5
            return
        end if
    end do
    IREADMAT = 0
end function

subroutine LISTMAT(io,title,nDim,mat)
    implicit none
    integer::io                                     ! output channel
    character(*)::title                             ! title /header
    integer,dimension(2)::nDim                      ! matrix's Dimension
    real(8),dimension(nDim(1),nDim(2))::mat         ! matrix
    integer::i,j                                    ! loop variables
    character(64)::ofmt                             ! dynamic format



       !                  |character
       !                   |number
       !                    |character--------->
       !
       !desired format : (2(1x,f8.2))
       !                  |number of columns
       write(ofmt,'(a,i4,a)') "(",nDim(2),"(1x,f9.2))"



       !write on the screen
       write(*,*) title
    do i = 1,nDim(1)
        write(*,ofmt) (mat(i,j),j=1,nDim(2))
    end do


    write(io,*) title
    do i = 1,nDim(1)
        write(io,ofmt) (mat(i,j),j=1,nDim(2))
    end do

end subroutine

subroutine LISTMATB(io,matcount,title,nDim,mat)
    implicit none
    integer::io                                     ! output channel
    character(*)::title                             ! title /header
    integer::matcount
    integer,dimension(2)::nDim                      ! matrix's Dimension
    real(8),dimension(nDim(1),nDim(2))::mat         ! matrix
    integer::i,j                                    ! loop variables
    character(64)::ofmt                             ! dynamic format



       !                  |character
       !                   |number
       !                    |character--------->
       !
       !desired format : (2(1x,f8.2))
       !                  |number of columns
       write(ofmt,'(a,i4,a)') "(",nDim(2),"(1x,f9.2))"



       !write on the screen
       write(*,*) matcount,title
    do i = 1,nDim(1)
        write(*,ofmt) (mat(i,j),j=1,nDim(2))
    end do


    write(io,*) title
    do i = 1,nDim(1)
        write(io,ofmt) (mat(i,j),j=1,nDim(2))
    end do

end subroutine
