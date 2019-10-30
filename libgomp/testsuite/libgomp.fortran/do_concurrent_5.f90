! { dg-do  run }
! PR 83064 - this used to give wrong results.
! { dg-additional-options "-O1 -ftree-parallelize-loops=2" }
! Original test case by Christian Felter

program main
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: nsplit = 4
    integer(int64), parameter :: ne = 2**20
    integer(int64) :: stride, low(nsplit), high(nsplit), i
    real(real64), dimension(nsplit) :: pi
    integer(int64), dimension(:), allocatable :: edof 

    allocate (edof(ne))
    edof(1::4) = 1
    edof(2::4) = 2
    edof(3::4) = 3
    edof(4::4) = 4
    
    stride = ceiling(real(ne)/nsplit)
    do i = 1, nsplit
        high(i) = stride*i
    end do
    do i = 2, nsplit
        low(i) = high(i-1) + 1
    end do
    low(1) = 1
    high(nsplit) = ne

    pi = 0
    do concurrent (i = 1:nsplit)
        pi(i) = sum(compute( low(i), high(i) ))
    end do
    if (abs (sum(pi) - atan(1.0d0)) > 1e-5) stop 1
    
contains
    
    pure function compute( low, high ) result( ttt )        
        integer(int64), intent(in) :: low, high
        real(real64), dimension(nsplit) :: ttt
        integer(int64) :: j, k
        
        ttt = 0

        ! Unrolled loop
!         do j = low, high, 4
!             k = 1
!             ttt(k) = ttt(k) + (-1)**(j+1) / real( 2*j-1 )                            
!             k = 2
!             ttt(k) = ttt(k) + (-1)**(j+2) / real( 2*j+1 )                            
!             k = 3
!             ttt(k) = ttt(k) + (-1)**(j+3) / real( 2*j+3 )                            
!             k = 4
!             ttt(k) = ttt(k) + (-1)**(j+4) / real( 2*j+5 )                            
!         end do
        
        ! Loop with modulo operation
!         do j = low, high
!             k = mod( j, nsplit ) + 1
!             ttt(k) = ttt(k) + (-1)**(j+1) / real( 2*j-1 )                                        
!         end do
        
        ! Loop with subscripting via host association
        do j = low, high
            k = edof(j)
            ttt(k) = ttt(k) + (-1.0_real64)**(j+1) / real( 2*j-1 )                                        
        end do
    end function
    
end program main
