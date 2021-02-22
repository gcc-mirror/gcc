! { dg-do run }
!
! PR fortran/99171
!
! Check dummy procedure arguments, especially optional ones
!
module m
  use iso_c_binding
  implicit none (type, external)
  integer :: cnt
  integer :: cnt2
contains
  subroutine proc()
    cnt = cnt + 1
  end subroutine

  subroutine proc2()
    cnt2 = cnt2 + 1
  end subroutine

  subroutine check(my_proc)
    procedure(proc) :: my_proc
    cnt = 42
    call my_proc()
    if (cnt /= 43) stop 1

    !$omp parallel
      call my_proc()
    !$omp end parallel
    if (cnt <= 43) stop 2 
  end

  subroutine check_opt(my_proc)
    procedure(proc), optional :: my_proc
    logical :: is_present
    is_present = present(my_proc)
    cnt = 55
    if (present (my_proc)) then
      call my_proc()
      if (cnt /= 56) stop 3
    endif

    !$omp parallel
      if (is_present .neqv. present (my_proc)) stop 4
      if (present (my_proc)) then
        call my_proc()
        if (cnt <= 56) stop 5
      end if
    !$omp end parallel
    if (is_present) then
      if (cnt <= 56) stop 6
    else if (cnt /= 55) then
      stop 7
    end if
  end

  subroutine check_ptr(my_proc)
    procedure(proc), pointer :: my_proc
    logical :: is_assoc
    integer :: mycnt
    is_assoc = associated (my_proc)

    cnt = 10
    cnt2 = 20
    if (associated (my_proc)) then
      call my_proc()
      if (cnt /= 11 .or. cnt2 /= 20) stop 8
    endif

    !$omp parallel
      if (is_assoc .neqv. associated (my_proc)) stop 9
      if (associated (my_proc)) then
        if (.not. associated (my_proc, proc)) stop 10
        call my_proc()
        if (cnt <= 11 .or. cnt2 /= 20) stop 11
      else if (cnt /= 10 .or. cnt2 /= 20) then
        stop 12
      end if
    !$omp end parallel
    if (is_assoc .neqv. associated (my_proc)) stop 13
    if (associated (my_proc)) then
      if (cnt <= 11 .or. cnt2 /= 20) stop 14
    else if (is_assoc .and. (cnt /= 11 .or. cnt2 /= 20)) then
      stop 15
    end if

    cnt = 30
    cnt2 = 40
    mycnt = 0
    !$omp parallel shared(mycnt)
      !$omp critical
         my_proc => proc2
         if (.not.associated (my_proc, proc2)) stop 17
         mycnt = mycnt + 1
         call my_proc()
         if (cnt2 /= 40 + mycnt .or. cnt /= 30) stop 18
      !$omp end critical
    !$omp end parallel
    if (.not.associated (my_proc, proc2)) stop 19
    if (cnt2 /= 40 + mycnt .or. cnt /= 30) stop 20
  end

  subroutine check_ptr_opt(my_proc)
    procedure(proc), pointer, optional :: my_proc
    logical :: is_assoc, is_present
    integer :: mycnt
    is_assoc = .false.
    is_present = present(my_proc)

    cnt = 10
    cnt2 = 20
    if (present (my_proc)) then
      is_assoc = associated (my_proc)
      if (associated (my_proc)) then
        call my_proc()
        if (cnt /= 11 .or. cnt2 /= 20) stop 21
      endif
   end if

    !$omp parallel
      if (is_present .neqv. present (my_proc)) stop 22
      if (present (my_proc)) then
        if (is_assoc .neqv. associated (my_proc)) stop 23
        if (associated (my_proc)) then
          if (.not. associated (my_proc, proc)) stop 24
          call my_proc()
          if (cnt <= 11 .or. cnt2 /= 20) stop 25
        else if (cnt /= 10 .or. cnt2 /= 20) then
          stop 26
        end if
      end if
    !$omp end parallel
    if (present (my_proc)) then
      if (is_assoc .neqv. associated (my_proc)) stop 27
      if (associated (my_proc)) then
        if (cnt <= 11 .or. cnt2 /= 20) stop 28
      else if (is_assoc .and. (cnt /= 11 .or. cnt2 /= 20)) then
        stop 29
      end if
    end if

    cnt = 30
    cnt2 = 40
    mycnt = 0
    !$omp parallel shared(mycnt)
      if (is_present .neqv. present (my_proc)) stop 30
      !$omp critical
         if (present (my_proc)) then
           my_proc => proc2
           if (.not.associated (my_proc, proc2)) stop 31
           mycnt = mycnt + 1
           call my_proc()
           if (cnt2 /= 40 + mycnt .or. cnt /= 30) stop 32
         end if
      !$omp end critical
    !$omp end parallel
    if (present (my_proc)) then
      if (.not.associated (my_proc, proc2)) stop 33
      if (cnt2 /= 40 + mycnt .or. cnt /= 30) stop 34
    end if
  end

  ! ----------------------

  subroutine cfun_check(my_cfun)
    type(c_funptr) :: my_cfun
    procedure(proc), pointer :: pptr
    logical :: has_cfun

    has_cfun = c_associated (my_cfun)
    pptr => null()
    cnt = 42
    call c_f_procpointer (my_cfun, pptr)
    if (has_cfun) then
      call pptr()
      if (cnt /= 43) stop 35
    end if

    pptr => null()
    !$omp parallel
      if (has_cfun .neqv. c_associated (my_cfun)) stop 36
      !$omp critical
        call c_f_procpointer (my_cfun, pptr)
      !$omp end critical
      if (has_cfun) then
        call pptr()
        if (cnt <= 43) stop 37
      else
        if (associated (pptr)) stop 38
      end if
    !$omp end parallel
  end

  subroutine cfun_check_opt(my_cfun)
    type(c_funptr), optional :: my_cfun
    procedure(proc), pointer :: pptr
    logical :: has_cfun, is_present

    has_cfun = .false.
    is_present = present (my_cfun)
    if (is_present) has_cfun = c_associated (my_cfun)

    cnt = 1
    pptr => null()
    !$omp parallel
      if (is_present .neqv. present (my_cfun)) stop 39
      if (is_present) then
        if (has_cfun .neqv. c_associated (my_cfun, c_funloc(proc))) stop 40
        !$omp critical
          call c_f_procpointer (my_cfun, pptr)
        !$omp end critical
        if (has_cfun) then
          call pptr()
          if (cnt <= 1) stop 41
        else
          if (associated (pptr)) stop 42
        end if
      end if
    !$omp end parallel
  end

  subroutine cfun_check_ptr(my_cfun)
    type(c_funptr), pointer :: my_cfun
    procedure(proc), pointer :: pptr
    logical :: has_cfun, is_assoc

    has_cfun = .false.
    is_assoc = associated (my_cfun)
    if (is_assoc) has_cfun = c_associated (my_cfun)

    cnt = 1
    pptr => null()
    !$omp parallel
      if (is_assoc .neqv. associated (my_cfun)) stop 43
      if (is_assoc) then
        if (has_cfun .neqv. c_associated (my_cfun, c_funloc(proc))) stop 44
        !$omp critical
          call c_f_procpointer (my_cfun, pptr)
        !$omp end critical
        if (has_cfun) then
          call pptr()
          if (cnt <= 1) stop 45
        else
          if (associated (pptr)) stop 46
        end if
      end if
    !$omp end parallel

    cnt = 42
    cnt2 = 1
    pptr => null()
    !$omp parallel
      if (is_assoc .neqv. associated (my_cfun)) stop 47
      if (is_assoc) then
        !$omp critical
          my_cfun = c_funloc (proc2)
          call c_f_procpointer (my_cfun, pptr)
        !$omp end critical
        if (.not. associated (pptr, proc2)) stop 48
        if (.not. c_associated (my_cfun, c_funloc(proc2))) stop 49
        call pptr()
        if (cnt /= 42 .or. cnt2 <= 1) stop 50
      end if
    !$omp end parallel
    if (is_assoc) then
      if (.not. associated (pptr, proc2)) stop 51
      if (.not. c_associated (my_cfun, c_funloc(proc2))) stop 52
    else
      if (associated (pptr)) stop 53
    end if
  end

  subroutine cfun_check_ptr_opt (my_cfun)
    type(c_funptr), pointer, optional :: my_cfun
    procedure(proc), pointer :: pptr
    logical :: is_present, has_cfun, is_assoc

    has_cfun = .false.
    is_assoc = .false.
    is_present = present (my_cfun)
    if (is_present) then
      is_assoc = associated (my_cfun)
      if (is_assoc) has_cfun = c_associated (my_cfun)
    end if

    cnt = 1
    pptr => null()
    !$omp parallel
      if (is_present .neqv. present (my_cfun)) stop 54
      if (is_present) then
        if (is_assoc .neqv. associated (my_cfun)) stop 55
        if (is_assoc) then
          if (has_cfun .neqv. c_associated (my_cfun, c_funloc(proc))) stop 56
          !$omp critical
            call c_f_procpointer (my_cfun, pptr)
          !$omp end critical
          if (has_cfun) then
            call pptr()
            if (cnt <= 1) stop 57
          else
            if (associated (pptr)) stop 58
          end if
        end if
      end if
    !$omp end parallel

    cnt = 42
    cnt2 = 1
    pptr => null()
    !$omp parallel
      if (is_present .neqv. present (my_cfun)) stop 59
      if (is_present) then
        if (is_assoc .neqv. associated (my_cfun)) stop 60
        if (is_assoc) then
          !$omp critical
            my_cfun = c_funloc (proc2)
            call c_f_procpointer (my_cfun, pptr)
          !$omp end critical
          if (.not. associated (pptr, proc2)) stop 61
          if (.not. c_associated (my_cfun, c_funloc(proc2))) stop 62
          call pptr()
          if (cnt /= 42 .or. cnt2 <= 1) stop 63
        end if
      end if
    !$omp end parallel
    if (is_present .and. is_assoc) then
      if (.not. associated (pptr, proc2)) stop 64
      if (.not. c_associated (my_cfun, c_funloc(proc2))) stop 65
    else
      if (associated (pptr)) stop 66
    end if
  end
end module m



program main
  use m
  implicit none (type, external)
  procedure(proc), pointer :: pptr
  type(c_funptr), target :: cfun
  type(c_funptr), pointer :: cfun_ptr

  call check(proc)
  call check_opt()
  call check_opt(proc)

  pptr => null()
  call check_ptr(pptr)
  pptr => proc
  call check_ptr(pptr)

  call check_ptr_opt()
  pptr => null()
  call check_ptr_opt(pptr)
  pptr => proc
  call check_ptr_opt(pptr)

  ! -------------------
  pptr => null()

  cfun = c_funloc (pptr)
  call cfun_check(cfun)

  cfun = c_funloc (proc)
  call cfun_check(cfun)

  call cfun_check_opt()

  cfun = c_funloc (pptr)
  call cfun_check_opt(cfun)

  cfun = c_funloc (proc)
  call cfun_check_opt(cfun)

  ! - - - -
  cfun_ptr => null()
  call cfun_check_ptr (cfun_ptr)

  cfun = c_funloc (proc)
  cfun_ptr => cfun
  call cfun_check_ptr (cfun_ptr)

  ! - - - -
  call cfun_check_ptr_opt ()

  cfun_ptr => null()
  call cfun_check_ptr_opt (cfun_ptr)

  cfun = c_funloc (proc)
  cfun_ptr => cfun
  call cfun_check_ptr_opt (cfun_ptr)
end program
