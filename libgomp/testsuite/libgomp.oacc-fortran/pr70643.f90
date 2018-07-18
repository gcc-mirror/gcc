MODULE reduction_test

CONTAINS

SUBROUTINE reduction_kernel(x_min,x_max,y_min,y_max,arr,sum)

  IMPLICIT NONE

  INTEGER      :: x_min,x_max,y_min,y_max
  REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: arr
  REAL(KIND=8) :: sum

  INTEGER      :: j,k

!$ACC DATA PRESENT(arr) COPY(sum)
!$ACC PARALLEL LOOP REDUCTION(+ : sum)
  DO k=y_min,y_max
    DO j=x_min,x_max
      sum=sum+arr(j,k)
    ENDDO
  ENDDO
!$ACC END PARALLEL LOOP
!$ACC END DATA
END SUBROUTINE reduction_kernel

END MODULE reduction_test

program main
    use reduction_test

    integer :: x_min,x_max,y_min,y_max
    real(kind=8), dimension(1:10,1:10) :: arr
    real(kind=8) :: sum

    x_min = 1
    x_max = 2
    y_min = 1
    y_max = 2

    arr(:,:) = 1.0

    sum = 0.0

    !$acc data copy(arr)

    call reduction_kernel(x_min,x_max,y_min,y_max,arr,sum)

    !$acc end data

    if (sum .ne. 4.0) STOP 1
end program
