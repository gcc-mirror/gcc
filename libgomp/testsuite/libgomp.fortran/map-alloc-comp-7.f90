module m
  implicit none (type, external)
  type t
    integer, allocatable :: arr(:,:)
    integer :: var
    integer, allocatable :: slr
  end type t

contains

  subroutine check_it (is_present, dummy_alloced, inner_alloc, &
                       scalar, array, a_scalar, a_array, &
                       l_scalar, l_array, la_scalar, la_array, &
                       opt_scalar, opt_array, a_opt_scalar, a_opt_array)
    type(t), intent(inout) :: &
            scalar, array(:,:), opt_scalar, opt_array(:,:), a_scalar, a_array(:,:), &
            a_opt_scalar, a_opt_array(:,:), &
            l_scalar, l_array(:,:), la_scalar, la_array(:,:)
    optional :: opt_scalar, opt_array, a_opt_scalar, a_opt_array
    allocatable :: a_scalar, a_array, a_opt_scalar, a_opt_array, la_scalar, la_array
    logical, value :: is_present, dummy_alloced, inner_alloc
    integer :: i, j, k, l

    ! CHECK VALUE
    if (scalar%var /= 42) stop 1
    if (l_scalar%var /= 42) stop 1
    if (is_present) then
      if (opt_scalar%var /= 42) stop 2
    end if
    if (any (shape(array) /= [3,2])) stop 1
    if (any (shape(l_array) /= [3,2])) stop 1
    if (is_present) then
      if (any (shape(opt_array) /= [3,2])) stop 1
    end if
    do j = 1, 2
      do i = 1, 3
        if (array(i,j)%var /= i*97 + 100*41*j) stop 3
        if (l_array(i,j)%var /= i*97 + 100*41*j) stop 3
        if (is_present) then
          if (opt_array(i,j)%var /= i*97 + 100*41*j) stop 4
        end if
      end do
    end do

    if (dummy_alloced) then
      if (a_scalar%var /= 42) stop 1
      if (la_scalar%var /= 42) stop 1
      if (is_present) then
        if (a_opt_scalar%var /= 42) stop 1
      end if
      if (any (shape(a_array) /= [3,2])) stop 1
      if (any (shape(la_array) /= [3,2])) stop 1
      if (is_present) then
        if (any (shape(a_opt_array) /= [3,2])) stop 1
      end if
      do j = 1, 2
        do i = 1, 3
          if (a_array(i,j)%var /= i*97 + 100*41*j) stop 1
          if (la_array(i,j)%var /= i*97 + 100*41*j) stop 1
          if (is_present) then
            if (a_opt_array(i,j)%var /= i*97 + 100*41*j) stop 1
          end if
        end do
      end do
    else
      if (allocated (a_scalar)) stop 1
      if (allocated (la_scalar)) stop 1
      if (allocated (a_array)) stop 1
      if (allocated (la_array)) stop 1
      if (is_present) then
        if (allocated (a_opt_scalar)) stop 1
        if (allocated (a_opt_array)) stop 1
      end if
    end if

    if (inner_alloc) then
      if (scalar%slr /= 467) stop 5
      if (l_scalar%slr /= 467) stop 5
      if (a_scalar%slr /= 467) stop 6
      if (la_scalar%slr /= 467) stop 6
      if (is_present) then
        if (opt_scalar%slr /= 467) stop 7
        if (a_opt_scalar%slr /= 467) stop 8
      end if
      do j = 1, 2
        do i = 1, 3
          if (array(i,j)%slr /= (i*97 + 100*41*j)  + 467) stop 9
          if (l_array(i,j)%slr /= (i*97 + 100*41*j)  + 467) stop 9
          if (a_array(i,j)%slr /= (i*97 + 100*41*j)  + 467) stop 10
          if (la_array(i,j)%slr /= (i*97 + 100*41*j)  + 467) stop 10
          if (is_present) then
            if (opt_array(i,j)%slr /= (i*97 + 100*41*j)  + 467) stop 11
            if (a_opt_array(i,j)%slr /= (i*97 + 100*41*j)  + 467) stop 12
          end if
        end do
      end do

      do l = 1, 5
        do k = 1, 4
          if (any (shape(scalar%arr) /= [4,5])) stop 1
          if (any (shape(l_scalar%arr) /= [4,5])) stop 1
          if (any (shape(a_scalar%arr) /= [4,5])) stop 1
          if (any (shape(la_scalar%arr) /= [4,5])) stop 1
          if (scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467) stop 13
          if (l_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467) stop 13
          if (a_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467) stop 14
          if (la_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467) stop 14
          if (is_present) then
            if (any (shape(opt_scalar%arr) /= [4,5])) stop 1
            if (any (shape(a_opt_scalar%arr) /= [4,5])) stop 1
            if (opt_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467) stop 15
            if (a_opt_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467) stop 16
          end if
        end do
      end do
      do j = 1, 2
        do i = 1, 3
          if (any (shape(array(i,j)%arr) /= [i,j])) stop 1
          if (any (shape(l_array(i,j)%arr) /= [i,j])) stop 1
          if (any (shape(a_array(i,j)%arr) /= [i,j])) stop 1
          if (any (shape(la_array(i,j)%arr) /= [i,j])) stop 1
          if (is_present) then
            if (any (shape(opt_array(i,j)%arr) /= [i,j])) stop 1
            if (any (shape(a_opt_array(i,j)%arr) /= [i,j])) stop 1
          endif
          do l = 1, j
            do k = 1, i
              if (array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l) stop 17
              if (l_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l) stop 17
              if (a_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l) stop 18
              if (la_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l) stop 18
              if (is_present) then
                if (opt_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l) stop 19
                if (a_opt_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l) stop 20
              end if
            end do
          end do
        end do
      end do
    else if (dummy_alloced) then
      if (allocated (scalar%slr)) stop 1
      if (allocated (l_scalar%slr)) stop 1
      if (allocated (a_scalar%slr)) stop 1
      if (allocated (la_scalar%slr)) stop 1
      if (is_present) then
        if (allocated (opt_scalar%slr)) stop 1
        if (allocated (a_opt_scalar%slr)) stop 1
      endif
      if (allocated (scalar%arr)) stop 1
      if (allocated (l_scalar%arr)) stop 1
      if (allocated (a_scalar%arr)) stop 1
      if (allocated (la_scalar%arr)) stop 1
      if (is_present) then
        if (allocated (opt_scalar%arr)) stop 1
        if (allocated (a_opt_scalar%arr)) stop 1
      endif
    end if

    ! SET VALUE
    scalar%var = 42 + 13
    l_scalar%var = 42 + 13
    if (is_present) then
      opt_scalar%var = 42 + 13
    endif
    do j = 1, 2
      do i = 1, 3
        array(i,j)%var = i*97 + 100*41*j + 13
        l_array(i,j)%var = i*97 + 100*41*j + 13
        if (is_present) then
          opt_array(i,j)%var = i*97 + 100*41*j + 13
        end if
      end do
    end do

    if (dummy_alloced) then
      a_scalar%var = 42 + 13
      la_scalar%var = 42 + 13
      if (is_present) then
        a_opt_scalar%var = 42 + 13
      endif
      do j = 1, 2
        do i = 1, 3
          a_array(i,j)%var = i*97 + 100*41*j + 13
          la_array(i,j)%var = i*97 + 100*41*j + 13
          if (is_present) then
            a_opt_array(i,j)%var = i*97 + 100*41*j + 13
          endif
        end do
      end do
    end if

    if (inner_alloc) then
      scalar%slr = 467 + 13
      l_scalar%slr = 467 + 13
      a_scalar%slr = 467 + 13
      la_scalar%slr = 467 + 13
      if (is_present) then
        opt_scalar%slr = 467 + 13
        a_opt_scalar%slr = 467 + 13
      end if
      do j = 1, 2
        do i = 1, 3
          array(i,j)%slr = (i*97 + 100*41*j)  + 467 + 13
          l_array(i,j)%slr = (i*97 + 100*41*j)  + 467 + 13
          a_array(i,j)%slr = (i*97 + 100*41*j)  + 467 + 13
          la_array(i,j)%slr = (i*97 + 100*41*j)  + 467 + 13
          if (is_present) then
            opt_array(i,j)%slr = (i*97 + 100*41*j)  + 467 + 13
            a_opt_array(i,j)%slr = (i*97 + 100*41*j)  + 467 + 13
          end if
        end do
      end do

      do l = 1, 5
        do k = 1, 4
          scalar%arr(k,l) = (i*27 + 1000*11*j) + 467 + 13
          l_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467 + 13
          a_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467 + 13
          la_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467 + 13
          if (is_present) then
            opt_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467 + 13
            a_opt_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467 + 13
          end if
        end do
      end do
      do j = 1, 2
        do i = 1, 3
          do l = 1, j
            do k = 1, i
              array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l + 13
              l_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l + 13
              a_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l + 13
              la_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l + 13
              if (is_present) then
                opt_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l + 13
                a_opt_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l + 13
              end if
            end do
          end do
        end do
      end do
    end if

  end subroutine
  subroutine check_reset (is_present, dummy_alloced, inner_alloc, &
                          scalar, array, a_scalar, a_array, &
                          l_scalar, l_array, la_scalar, la_array, &
                          opt_scalar, opt_array, a_opt_scalar, a_opt_array)
    type(t), intent(inout) :: &
            scalar, array(:,:), opt_scalar, opt_array(:,:), a_scalar, a_array(:,:), &
            a_opt_scalar, a_opt_array(:,:), &
            l_scalar, l_array(:,:), la_scalar, la_array(:,:)
    optional :: opt_scalar, opt_array, a_opt_scalar, a_opt_array
    allocatable :: a_scalar, a_array, a_opt_scalar, a_opt_array, la_scalar, la_array
    logical, value :: is_present, dummy_alloced, inner_alloc
    integer :: i, j, k, l

    ! CHECK VALUE
    if (scalar%var /= 42 + 13) stop 1
    if (l_scalar%var /= 42 + 13) stop 1
    if (is_present) then
      if (opt_scalar%var /= 42 + 13) stop 2
    end if
    if (any (shape(array) /= [3,2])) stop 1
    if (any (shape(l_array) /= [3,2])) stop 1
    if (is_present) then
      if (any (shape(opt_array) /= [3,2])) stop 1
    end if
    do j = 1, 2
      do i = 1, 3
        if (array(i,j)%var /= i*97 + 100*41*j + 13) stop 3
        if (l_array(i,j)%var /= i*97 + 100*41*j + 13) stop 3
        if (is_present) then
          if (opt_array(i,j)%var /= i*97 + 100*41*j + 13) stop 4
        end if
      end do
    end do

    if (dummy_alloced) then
      if (a_scalar%var /= 42 + 13) stop 1
      if (la_scalar%var /= 42 + 13) stop 1
      if (is_present) then
        if (a_opt_scalar%var /= 42 + 13) stop 1
      end if
      if (any (shape(a_array) /= [3,2])) stop 1
      if (any (shape(la_array) /= [3,2])) stop 1
      if (is_present) then
        if (any (shape(a_opt_array) /= [3,2])) stop 1
      end if
      do j = 1, 2
        do i = 1, 3
          if (a_array(i,j)%var /= i*97 + 100*41*j + 13) stop 1
          if (la_array(i,j)%var /= i*97 + 100*41*j + 13) stop 1
          if (is_present) then
            if (a_opt_array(i,j)%var /= i*97 + 100*41*j + 13) stop 1
          end if
        end do
      end do
    else
      if (allocated (a_scalar)) stop 1
      if (allocated (la_scalar)) stop 1
      if (allocated (a_array)) stop 1
      if (allocated (la_array)) stop 1
      if (is_present) then
        if (allocated (a_opt_scalar)) stop 1
        if (allocated (a_opt_array)) stop 1
      end if
    end if

    if (inner_alloc) then
      if (scalar%slr /= 467 + 13) stop 5
      if (l_scalar%slr /= 467 + 13) stop 5
      if (a_scalar%slr /= 467 + 13) stop 6
      if (la_scalar%slr /= 467 + 13) stop 6
      if (is_present) then
        if (opt_scalar%slr /= 467 + 13) stop 7
        if (a_opt_scalar%slr /= 467 + 13) stop 8
      end if
      do j = 1, 2
        do i = 1, 3
          if (array(i,j)%slr /= (i*97 + 100*41*j)  + 467 + 13) stop 9
          if (l_array(i,j)%slr /= (i*97 + 100*41*j)  + 467 + 13) stop 9
          if (a_array(i,j)%slr /= (i*97 + 100*41*j)  + 467 + 13) stop 10
          if (la_array(i,j)%slr /= (i*97 + 100*41*j)  + 467 + 13) stop 10
          if (is_present) then
            if (opt_array(i,j)%slr /= (i*97 + 100*41*j)  + 467 + 13) stop 11
            if (a_opt_array(i,j)%slr /= (i*97 + 100*41*j)  + 467 + 13) stop 12
          end if
        end do
      end do

      do l = 1, 5
        do k = 1, 4
          if (any (shape(scalar%arr) /= [4,5])) stop 1
          if (any (shape(l_scalar%arr) /= [4,5])) stop 1
          if (any (shape(a_scalar%arr) /= [4,5])) stop 1
          if (any (shape(la_scalar%arr) /= [4,5])) stop 1
          if (scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467 + 13) stop 13
          if (l_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467 + 13) stop 13
          if (a_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467 + 13) stop 14
          if (la_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467 + 13) stop 14
          if (is_present) then
            if (any (shape(opt_scalar%arr) /= [4,5])) stop 1
            if (any (shape(a_opt_scalar%arr) /= [4,5])) stop 1
            if (opt_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467 + 13) stop 15
            if (a_opt_scalar%arr(k,l) /= (i*27 + 1000*11*j) + 467 + 13) stop 16
          end if
        end do
      end do
      do j = 1, 2
        do i = 1, 3
          if (any (shape(array(i,j)%arr) /= [i,j])) stop 1
          if (any (shape(l_array(i,j)%arr) /= [i,j])) stop 1
          if (any (shape(a_array(i,j)%arr) /= [i,j])) stop 1
          if (any (shape(la_array(i,j)%arr) /= [i,j])) stop 1
          if (is_present) then
            if (any (shape(opt_array(i,j)%arr) /= [i,j])) stop 1
            if (any (shape(a_opt_array(i,j)%arr) /= [i,j])) stop 1
          endif
          do l = 1, j
            do k = 1, i
              if (array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l + 13) stop 17
              if (l_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l + 13) stop 17
              if (a_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l + 13) stop 18
              if (la_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l + 13) stop 18
              if (is_present) then
                if (opt_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l + 13) stop 19
                if (a_opt_array(i,j)%arr(k,l) /= i*27 + 1000*11*j + 467 + 3*k +53*l + 13) stop 20
              end if
            end do
          end do
        end do
      end do
    else if (dummy_alloced) then
      if (allocated (scalar%slr)) stop 1
      if (allocated (l_scalar%slr)) stop 1
      if (allocated (a_scalar%slr)) stop 1
      if (allocated (la_scalar%slr)) stop 1
      if (is_present) then
        if (allocated (opt_scalar%slr)) stop 1
        if (allocated (a_opt_scalar%slr)) stop 1
      endif
      if (allocated (scalar%arr)) stop 1
      if (allocated (l_scalar%arr)) stop 1
      if (allocated (a_scalar%arr)) stop 1
      if (allocated (la_scalar%arr)) stop 1
      if (is_present) then
        if (allocated (opt_scalar%arr)) stop 1
        if (allocated (a_opt_scalar%arr)) stop 1
      endif
    end if

    ! (RE)SET VALUE
    scalar%var = 42
    l_scalar%var = 42
    if (is_present) then
      opt_scalar%var = 42
    endif
    do j = 1, 2
      do i = 1, 3
        array(i,j)%var = i*97 + 100*41*j
        l_array(i,j)%var = i*97 + 100*41*j
        if (is_present) then
          opt_array(i,j)%var = i*97 + 100*41*j
        end if
      end do
    end do

    if (dummy_alloced) then
      a_scalar%var = 42
      la_scalar%var = 42
      if (is_present) then
        a_opt_scalar%var = 42
      endif
      do j = 1, 2
        do i = 1, 3
          a_array(i,j)%var = i*97 + 100*41*j
          la_array(i,j)%var = i*97 + 100*41*j
          if (is_present) then
            a_opt_array(i,j)%var = i*97 + 100*41*j
          endif
        end do
      end do
    end if

    if (inner_alloc) then
      scalar%slr = 467
      l_scalar%slr = 467
      a_scalar%slr = 467
      la_scalar%slr = 467
      if (is_present) then
        opt_scalar%slr = 467
        a_opt_scalar%slr = 467
      end if
      do j = 1, 2
        do i = 1, 3
          array(i,j)%slr = (i*97 + 100*41*j)  + 467
          l_array(i,j)%slr = (i*97 + 100*41*j)  + 467
          a_array(i,j)%slr = (i*97 + 100*41*j)  + 467
          la_array(i,j)%slr = (i*97 + 100*41*j)  + 467
          if (is_present) then
            opt_array(i,j)%slr = (i*97 + 100*41*j)  + 467
            a_opt_array(i,j)%slr = (i*97 + 100*41*j)  + 467
          end if
        end do
      end do

      do l = 1, 5
        do k = 1, 4
          scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
          l_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
          a_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
          la_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
          if (is_present) then
            opt_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
            a_opt_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
          end if
        end do
      end do
      do j = 1, 2
        do i = 1, 3
          do l = 1, j
            do k = 1, i
              array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
              l_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
              a_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
              la_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
              if (is_present) then
                opt_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
                a_opt_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
              end if
            end do
          end do
        end do
      end do
    end if
  end subroutine

  subroutine test(scalar, array, a_scalar, a_array, opt_scalar, opt_array, &
                  a_opt_scalar, a_opt_array)
    type(t) :: scalar, array(:,:), opt_scalar, opt_array(:,:), a_scalar, a_array(:,:)
    type(t) :: a_opt_scalar, a_opt_array(:,:)
    type(t) :: l_scalar, l_array(3,2), la_scalar, la_array(:,:)
    allocatable :: a_scalar, a_array, a_opt_scalar, a_opt_array, la_scalar, la_array
    optional :: opt_scalar, opt_array, a_opt_scalar, a_opt_array

    integer :: i, j, k, l
    logical :: is_present, dummy_alloced, local_alloced, inner_alloc
    is_present = present(opt_scalar)
    dummy_alloced = allocated(a_scalar)
    inner_alloc = allocated(scalar%slr)

    l_scalar%var = 42
    do j = 1, 2
      do i = 1, 3
        l_array(i,j)%var = i*97 + 100*41*j
      end do
    end do

    if (dummy_alloced) then
      allocate(la_scalar, la_array(3,2))
      a_scalar%var = 42
      la_scalar%var = 42
      do j = 1, 2
        do i = 1, 3
          l_array(i,j)%var = i*97 + 100*41*j
          la_array(i,j)%var = i*97 + 100*41*j
        end do
      end do
    end if

    if (inner_alloc) then
      l_scalar%slr = 467
      la_scalar%slr = 467
      do j = 1, 2
        do i = 1, 3
          l_array(i,j)%slr = (i*97 + 100*41*j)  + 467
          la_array(i,j)%slr = (i*97 + 100*41*j)  + 467
        end do
      end do

      allocate(l_scalar%arr(4,5), la_scalar%arr(4,5))
      do l = 1, 5
        do k = 1, 4
          l_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
          la_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
        end do
      end do
      do j = 1, 2
        do i = 1, 3
          allocate(l_array(i,j)%arr(i,j), la_array(i,j)%arr(i,j))
          do l = 1, j
            do k = 1, i
              l_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
              la_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
            end do
          end do
        end do
      end do
    end if

    ! implicit mapping
    !$omp target
      if (is_present) then
        call check_it (is_present, dummy_alloced, inner_alloc, &
                       scalar, array, a_scalar, a_array, &
                       l_scalar, l_array, la_scalar, la_array, &
                       opt_scalar, opt_array, a_opt_scalar, a_opt_array)
      else
        call check_it (is_present, dummy_alloced, inner_alloc, &
                       scalar, array, a_scalar, a_array, &
                       l_scalar, l_array, la_scalar, la_array)
      end if
    !$omp end target

    if (is_present) then
      call check_reset (is_present, dummy_alloced, inner_alloc, &
                        scalar, array, a_scalar, a_array, &
                        l_scalar, l_array, la_scalar, la_array, &
                        opt_scalar, opt_array, a_opt_scalar, a_opt_array)
    else
      call check_reset (is_present, dummy_alloced, inner_alloc, &
                        scalar, array, a_scalar, a_array, &
                        l_scalar, l_array, la_scalar, la_array)
    endif

    ! explicit mapping
    !$omp target map(scalar, array, opt_scalar, opt_array, a_scalar, a_array) &
    !$omp&       map(a_opt_scalar, a_opt_array) &
    !$omp&       map(l_scalar, l_array, la_scalar, la_array)
      if (is_present) then
        call check_it (is_present, dummy_alloced, inner_alloc, &
                       scalar, array, a_scalar, a_array, &
                       l_scalar, l_array, la_scalar, la_array, &
                       opt_scalar, opt_array, a_opt_scalar, a_opt_array)
      else
        call check_it (is_present, dummy_alloced, inner_alloc, &
                       scalar, array, a_scalar, a_array, &
                       l_scalar, l_array, la_scalar, la_array)
      endif
    !$omp end target

    if (is_present) then
      call check_reset (is_present, dummy_alloced, inner_alloc, &
                        scalar, array, a_scalar, a_array, &
                        l_scalar, l_array, la_scalar, la_array, &
                        opt_scalar, opt_array, a_opt_scalar, a_opt_array)
    else
      call check_reset (is_present, dummy_alloced, inner_alloc, &
                        scalar, array, a_scalar, a_array, &
                        l_scalar, l_array, la_scalar, la_array)
    endif
  end subroutine
end module

program main
  use m
  implicit none (type, external)
  type(t) :: scalar, array(3,2), opt_scalar, opt_array(3,2), a_scalar, a_array(:,:)
  type(t) :: a_opt_scalar, a_opt_array(:,:)
  allocatable :: a_scalar, a_array, a_opt_scalar, a_opt_array
  integer :: i, j, k, l, n

  scalar%var = 42
  opt_scalar%var = 42
  do j = 1, 2
    do i = 1, 3
      array(i,j)%var = i*97 + 100*41*j
      opt_array(i,j)%var = i*97 + 100*41*j
    end do
  end do

  ! unallocated
  call test (scalar, array, a_scalar, a_array)
  call test (scalar, array, a_scalar, a_array, opt_scalar, opt_array, a_opt_scalar, a_opt_array)

  ! allocated
  allocate(a_scalar, a_opt_scalar, a_array(3,2), a_opt_array(3,2))
  a_scalar%var = 42
  a_opt_scalar%var = 42
  do j = 1, 2
    do i = 1, 3
      a_array(i,j)%var = i*97 + 100*41*j
      a_opt_array(i,j)%var = i*97 + 100*41*j
    end do
  end do

  call test (scalar, array, a_scalar, a_array)
  call test (scalar, array, a_scalar, a_array, opt_scalar, opt_array, a_opt_scalar, a_opt_array)

  ! comps allocated
  scalar%slr = 467
  a_scalar%slr = 467
  opt_scalar%slr = 467
  a_opt_scalar%slr = 467
  do j = 1, 2
    do i = 1, 3
      array(i,j)%slr = (i*97 + 100*41*j)  + 467
      a_array(i,j)%slr = (i*97 + 100*41*j)  + 467
      opt_array(i,j)%slr = (i*97 + 100*41*j)  + 467
      a_opt_array(i,j)%slr = (i*97 + 100*41*j)  + 467
    end do
  end do

  allocate(scalar%arr(4,5), a_scalar%arr(4,5), opt_scalar%arr(4,5), a_opt_scalar%arr(4,5))
  do l = 1, 5
    do k = 1, 4
      scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
      a_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
      opt_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
      a_opt_scalar%arr(k,l) = (i*27 + 1000*11*j) + 467
    end do
  end do
  do j = 1, 2
    do i = 1, 3
      allocate(array(i,j)%arr(i,j), a_array(i,j)%arr(i,j), opt_array(i,j)%arr(i,j), a_opt_array(i,j)%arr(i,j))
      do l = 1, j
        do k = 1, i
          array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
          a_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
          opt_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
          a_opt_array(i,j)%arr(k,l) = i*27 + 1000*11*j + 467 + 3*k +53*l
        end do
      end do
    end do
  end do

  call test (scalar, array, a_scalar, a_array)
  call test (scalar, array, a_scalar, a_array, opt_scalar, opt_array, a_opt_scalar, a_opt_array)

  deallocate(a_scalar, a_opt_scalar, a_array, a_opt_array)
end
