! { dg-do run }

program main
  integer, parameter :: n = 40
  integer, allocatable :: ar(:,:,:)
  integer :: i

  allocate (ar(1:n,0:n-1,0:n-1))
  !$acc enter data copyin (ar)

  !$acc update host (ar)

  !$acc update device (ar)

  call update_ar (ar, n)

  !$acc exit data copyout (ar)
end program main

subroutine update_ar (ar, n)
  integer :: n
  integer, dimension (1:n,0:n-1,0:n-1) :: ar

  !$acc update host (ar)

  !$acc update device (ar)
end subroutine update_ar
