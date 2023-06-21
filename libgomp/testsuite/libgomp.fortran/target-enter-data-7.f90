module m
  implicit none
  character(len=:), allocatable :: strA(:), strA2
  character(len=:), pointer :: strP(:), strP2
  !$omp declare target enter(strA,strA2,strP,strP2)
contains
  subroutine opt_map(str1, str2, str3)
    character(len=:), allocatable :: str1, str2, str3, str4
    optional :: str2, str3 

    if (.not.present(str2)) error stop
    if (present(str3))  error stop

    !$omp target map(str1,str2,str3,str4)
      if (allocated(str1)) error stop
      if (allocated(str2)) error stop
      if (present(str3)) error stop
      if (allocated(str4)) error stop
    !$omp end target
  end
  subroutine call_opt()
    character(len=:), allocatable :: str1, str2
    call opt_map(str1, str2)
  end
  subroutine test
   !$omp declare target
   if (.not. allocated(strA)) error stop
   !if (.not. allocated(strA2)) error stop
   if (.not. associated(strP)) error stop
   !if (.not. associated(strP2)) error stop

    ! ensure length was updated as well
    if (len(strA) /= 3) error stop
    if (len(strA2) /= 5) error stop
    if (len(strP) /= 4) error stop
    if (len(strP2) /= 8) error stop
!    if (any (strA /= ['Hav', 'e f', 'un!'])) error stop
!    if (strA2 /= 'Hello') error stop
!    if (any (strP /= ['abcd', 'efgh', 'ijkl'])) error stop
!    if (strP2 /= 'TestCase') error stop
!
!    strA = ['123', '456', '789']
!    strA2 = 'World'
!    strP = ['ABCD', 'EFGH', 'IJKL']
!    strP2 = 'Passed!!'
  end
end

program main
  use m
  implicit none
  call call_opt

  strA = ['Hav', 'e f', 'un!']
  strA2 = 'Hello'
  allocate(character(len=4) :: strP(3))
  strP = ['abcd', 'efgh', 'ijkl']
  allocate(character(len=8) :: strP2)
  strP2 = 'TestCase'

  !$omp target enter data map(always, to: strA, strA2)
  !$omp target enter data map(to: strP, strP2)
  !$omp target
    call test()
  !$omp end target
  !$omp target exit data map(always, from: strA, strA2, strP, strP2)

  if (len(strA) /= 3) error stop
  if (len(strA2) /= 5) error stop
  if (len(strP) /= 4) error stop
  if (len(strP2) /= 8) error stop
!  if (any (strA /= ['123', '456', '789'])) error stop
!  if (strA2 /= 'World') error stop
!  if (any(strP /= ['ABCD', 'EFGH', 'IJKL'])) error stop
!  if (strP2 /= 'Passed!!') error stop

!  deallocate(strP, strP2, strA, strA2)
end
