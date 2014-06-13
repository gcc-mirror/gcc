! { dg-do run }

!$omp declare reduction (foo : character(kind=1, len=*) &
!$omp & : omp_out = trim(omp_out) // omp_in) initializer (omp_priv = '')
!$omp declare reduction (bar : character(kind=1, len=:) &
!$omp & : omp_out = trim(omp_in) // omp_out) initializer (omp_priv = '')
!$omp declare reduction (baz : character(kind=1, len=1) &
!$omp & : omp_out = char (ichar (omp_out) + ichar (omp_in) &
!$omp & - ichar ('0'))) initializer (omp_priv = '0')
!$omp declare reduction (baz : character(kind=1, len=2) &
!$omp & : omp_out = char (ichar (omp_out(1:1)) + ichar (omp_in(1:1)) &
!$omp & - ichar ('0')) // char (ichar (omp_out(2:2)) + &
!$omp & ichar (omp_in(2:2)) - ichar ('0'))) initializer (omp_priv = '00')
  character(kind=1, len=64) :: c(-3:-2,1:1,7:8), d(2:3,-7:-5)
  character(kind = 1, len=1) :: e(2:4)
  character(kind = 1, len=1+1) :: f(8:10,9:10)
  integer :: i, j, k
  c = ''
  d = ''
  e = '0'
  f = '00'
!$omp parallel do reduction (foo : c) reduction (bar : d) &
!$omp & reduction (baz : e, f) private (j, k)
  do i = 1, 64
    forall (j = -3:-2, k = 7:8) &
      c(j,1,k) = trim(c(j,1,k)) // char (ichar ('0') + i)
    d = char (ichar ('0') + i) // d
    e = char (ichar (e) + mod (i, 3))
    f = char (ichar (f(:,:)(1:1)) + mod (i, 2)) &
&	// char (ichar (f(:,:)(2:2)) + mod (i, 3))
  end do
  do i = 1, 64
    if (any (index (c, char (ichar ('0') + i)) .eq. 0)) call abort
    if (any (index (d, char (ichar ('0') + i)) .eq. 0)) call abort
  end do
  if (any (e.ne.char (ichar ('0') + 64))) call abort
  if (any (f(:,:)(1:1).ne.char (ichar ('0') + 32))) call abort
  if (any (f(:,:)(2:2).ne.char (ichar ('0') + 64))) call abort
end
