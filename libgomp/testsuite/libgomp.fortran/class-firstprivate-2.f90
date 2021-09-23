! FIRSTPRIVATE: CLASS(t) + derived types
program select_type_openmp
  implicit none
  type t
  end type t
  type, extends(t) :: t_int
    integer :: i
  end type 
  type, extends(t) :: t_char1
    character(len=:, kind=1), allocatable :: str
  end type 
  type, extends(t) :: t_char4
    character(len=:, kind=4), allocatable :: str
  end type 
  class(t), allocatable :: val1, val1a, val2, val3

  call sub() ! local var

  call sub2(val1, val1a, val2, val3) ! allocatable args

  allocate(val1, source=t_int(7))
  allocate(val1a, source=t_int(7))
  allocate(val2, source=t_char1("abcdef"))
  allocate(val3, source=t_char4(4_"zyx4"))
  call sub3(val1, val1a, val2, val3)  ! nonallocatable vars
  deallocate(val1, val1a, val2, val3)
contains
subroutine sub()
  class(t), allocatable :: val1, val1a, val2, val3
  allocate(val1a, source=t_int(7))
  allocate(val2, source=t_char1("abcdef"))
  allocate(val3, source=t_char4(4_"zyx4"))

  if (allocated(val1)) stop 1

  !$OMP PARALLEL firstprivate(val1, val1a, val2, val3)
    if (allocated(val1)) stop 2
    if (.not.allocated(val1a)) stop 3
    if (.not.allocated(val2)) stop 4
    if (.not.allocated(val3)) stop 5

    allocate(val1, source=t_int(7))

    select type (val1)
      type is (t_int)
        if (val1%i /= 7) stop 6
        val1%i = 8
      class default
        stop 7
    end select

    select type (val1a)
      type is (t_int)
        if (val1a%i /= 7) stop 8
        val1a%i = 8
      class default
        stop 9
    end select

    select type (val2)
      type is (t_char1)
        if (len(val2%str) /= 6) stop 10
        if (val2%str /= "abcdef") stop 11
        val2%str = "123456"
      class default
        stop 12
    end select

    select type (val3)
      type is (t_char4)
        if (len(val3%str) /= 4) stop 13
        if (val3%str /= 4_"zyx4") stop 14
        val3%str = 4_"AbCd"
      class default
        stop 15
    end select

    select type (val3)
      type is (t_char4)
        if (len(val3%str) /= 4) stop 16
        if (val3%str /= 4_"AbCd") stop 17
        val3%str = 4_"1ab2"
      class default
        stop 18
    end select

    select type (val2)
      type is (t_char1)
        if (len(val2%str) /= 6) stop 19
        if (val2%str /= "123456") stop 20
        val2%str = "A2C4E6"
      class default
        stop 21
    end select

    select type (val1)
      type is (t_int)
        if (val1%i /= 8) stop 22
        val1%i = 9
      class default
        stop 23
    end select

    select type (val1a)
      type is (t_int)
        if (val1a%i /= 8) stop 24
        val1a%i = 9
      class default
        stop 25
    end select
  !$OMP END PARALLEL

  if (allocated(val1)) stop 26
  if (.not. allocated(val1a)) stop 27
  if (.not. allocated(val2)) stop 28

  select type (val2)
    type is (t_char1)
      if (len(val2%str) /= 6) stop 29
      if (val2%str /= "abcdef") stop 30
    class default
      stop 31
  end select
  select type (val3)
    type is (t_char4)
      if (len(val3%str) /= 4) stop 32
      if (val3%str /= 4_"zyx4") stop 33
    class default
      stop 34
  end select
  deallocate(val1a,val2, val3)
end subroutine sub

subroutine sub2(val1, val1a, val2, val3)
  class(t), allocatable :: val1, val1a, val2, val3
  optional :: val1a
  allocate(val1a, source=t_int(7))
  allocate(val2, source=t_char1("abcdef"))
  allocate(val3, source=t_char4(4_"zyx4"))
 
  if (allocated(val1)) stop 35

  !$OMP PARALLEL firstprivate(val1, val1a, val2, val3)
    if (allocated(val1)) stop 36
    if (.not.allocated(val1a)) stop 37
    if (.not.allocated(val2)) stop 38
    if (.not.allocated(val3)) stop 39

    allocate(val1, source=t_int(7))

    select type (val1)
      type is (t_int)
        if (val1%i /= 7) stop 40
        val1%i = 8
      class default
        stop 41
    end select

    select type (val1a)
      type is (t_int)
        if (val1a%i /= 7) stop 42
        val1a%i = 8
      class default
        stop 43
    end select

    select type (val2)
      type is (t_char1)
        if (len(val2%str) /= 6) stop 44
        if (val2%str /= "abcdef") stop 45
        val2%str = "123456"
      class default
        stop 46
    end select

    select type (val3)
      type is (t_char4)
        if (len(val3%str) /= 4) stop 47
        if (val3%str /= 4_"zyx4") stop 48
        val3%str = "AbCd"
      class default
        stop 49
    end select

    select type (val3)
      type is (t_char4)
        if (len(val3%str) /= 4) stop 50
        if (val3%str /= 4_"AbCd") stop 51
        val3%str = 4_"1ab2"
      class default
        stop 52
    end select

    select type (val2)
      type is (t_char1)
        if (len(val2%str) /= 6) stop 53
        if (val2%str /= "123456") stop 54
        val2%str = "A2C4E6"
      class default
        stop 55
    end select

    select type (val1)
      type is (t_int)
        if (val1%i /= 8) stop 56
        val1%i = 9
      class default
        stop 57
    end select

    select type (val1a)
      type is (t_int)
        if (val1a%i /= 8) stop 58
        val1a%i = 9
      class default
        stop 59
    end select
  !$OMP END PARALLEL

  if (allocated(val1)) stop 60
  if (.not. allocated(val1a)) stop 61
  if (.not. allocated(val2)) stop 62

  select type (val2)
    type is (t_char1)
      if (len(val2%str) /= 6) stop 63
      if (val2%str /= "abcdef") stop 64
    class default
        stop 65
  end select

  select type (val3)
    type is (t_char4)
      if (len(val3%str) /= 4) stop 66
      if (val3%str /= 4_"zyx4") stop 67
      val3%str = 4_"AbCd"
    class default
      stop 68
  end select
  deallocate(val1a, val2, val3)
end subroutine sub2

subroutine sub3(val1, val1a, val2, val3)
  class(t) :: val1, val1a, val2, val3
  optional :: val1a

  !$OMP PARALLEL firstprivate(val1, val1a, val2, val3)
    select type (val1)
      type is (t_int)
        if (val1%i /= 7) stop 69
        val1%i = 8
      class default
        stop 70
    end select

    select type (val1a)
      type is (t_int)
        if (val1a%i /= 7) stop 71
        val1a%i = 8
      class default
        stop 72
    end select

    select type (val2)
      type is (t_char1)
        if (len(val2%str) /= 6) stop 73
        if (val2%str /= "abcdef") stop 74
        val2%str = "123456"
      class default
        stop 75
    end select

    select type (val3)
      type is (t_char4)
        if (len(val3%str) /= 4) stop 76
        if (val3%str /= 4_"zyx4") stop 77
        val3%str = 4_"AbCd"
      class default
        stop 78
    end select

    select type (val3)
      type is (t_char4)
        if (len(val3%str) /= 4) stop 79
        if (val3%str /= 4_"AbCd") stop 80
        val3%str = 4_"1ab2"
      class default
        stop 81
    end select

    select type (val2)
      type is (t_char1)
        if (len(val2%str) /= 6) stop 82
        if (val2%str /= "123456") stop 83
        val2%str = "A2C4E6"
      class default
        stop 84
    end select

    select type (val1)
      type is (t_int)
        if (val1%i /= 8) stop 85
        val1%i = 9
      class default
        stop 86
    end select

    select type (val1a)
      type is (t_int)
        if (val1a%i /= 8) stop 87
        val1a%i = 9
      class default
        stop 88
    end select
  !$OMP END PARALLEL

  select type (val2)
    type is (t_char1)
      if (len(val2%str) /= 6) stop 89
      if (val2%str /= "abcdef") stop 90
    class default
      stop 91
  end select

  select type (val3)
    type is (t_char4)
      if (len(val3%str) /= 4) stop 92
      if (val3%str /= 4_"zyx4") stop 93
      val3%str = 4_"AbCd"
    class default
      stop 94
  end select
end subroutine sub3
end program select_type_openmp
