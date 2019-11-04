! { dg-do run }

module udr11
  type dt
    integer :: x = 0
  end type
end module udr11
  use udr11, only : dt
!$omp declare reduction(+:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(-:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(*:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(.and.:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(.or.:dt:omp_out%x=omp_out%x+3*omp_in%x)
!$omp declare reduction(.eqv.:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(.neqv.:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(min:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(max:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(iand:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(ior:dt:omp_out%x=omp_out%x+omp_in%x)
!$omp declare reduction(ieor:dt:omp_out%x=omp_out%x+omp_in%x)
  interface operator(.and.)
    function addme1 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme1
    end function addme1
  end interface
  interface operator(.or.)
    function addme2 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme2
    end function addme2
  end interface
  interface operator(.eqv.)
    function addme3 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme3
    end function addme3
  end interface
  interface operator(.neqv.)
    function addme4 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme4
    end function addme4
  end interface
  interface operator(+)
    function addme5 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme5
    end function addme5
  end interface
  interface operator(-)
    function addme6 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme6
    end function addme6
  end interface
  interface operator(*)
    function addme7 (x, y)
      use udr11, only : dt
      type (dt), intent (in) :: x, y
      type(dt) :: addme7
    end function addme7
  end interface
  type(dt) :: j, k, l, m, n, o, p, q, r, s, t, u
  integer :: i
!$omp parallel do reduction(.and.:j) reduction(.or.:k) &
!$omp & reduction(.eqv.:l) reduction(.neqv.:m) &
!$omp & reduction(min:n) reduction(max:o) &
!$omp & reduction(iand:p) reduction(ior:q) reduction (ieor:r) &
!$omp & reduction(+:s) reduction(-:t) reduction(*:u)
  do i = 1, 100
    j%x = j%x + i
    k%x = k%x + 2 * i
    l%x = l%x + 3 * i
    m%x = m%x + i
    n%x = n%x + 2 * i
    o%x = o%x + 3 * i
    p%x = p%x + i
    q%x = q%x + 2 * i
    r%x = r%x + 3 * i
    s%x = s%x + i
    t%x = t%x + 2 * i
    u%x = u%x + 3 * i
  end do
  if (j%x /= 5050 .or. k%x /= 30300 .or. l%x /= 15150) stop 1
  if (m%x /= 5050 .or. n%x /= 10100 .or. o%x /= 15150) stop 2
  if (p%x /= 5050 .or. q%x /= 10100 .or. r%x /= 15150) stop 3
  if (s%x /= 5050 .or. t%x /= 10100 .or. u%x /= 15150) stop 4
end
