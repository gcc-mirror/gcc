! { dg-do run { target vect_simd_clones } }
! { dg-options "-fno-inline" }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module declare_simd_1_mod
  contains
    real function foo (a, b, c)
      !$omp declare simd (foo) simdlen (4) uniform (a) linear (b : 5)
      double precision, value :: a
      real, value :: c
      !$omp declare simd (foo)
      integer, value :: b
      foo = a + b * c
    end function foo
end module declare_simd_1_mod
  use declare_simd_1_mod
  interface
    function bar (a, b, c)
      !$omp declare simd (bar)
      integer, value :: b
      real, value :: c
      real :: bar
      !$omp declare simd (bar) simdlen (4) linear (b : 2)
      double precision, value :: a
    end function bar
  end interface
  integer :: i
  double precision :: a(128)
  real :: b(128), d(128)
  data d /171., 414., 745., 1164., 1671., 2266., 2949., 3720., 4579., &
  &       5526., 6561., 7684., 8895., 10194., 11581., 13056., 14619., &
  &       16270., 18009., 19836., 21751., 23754., 25845., 28024., &
  &       30291., 32646., 35089., 37620., 40239., 42946., 45741., &
  &       48624., 51595., 54654., 57801., 61036., 64359., 67770., &
  &       71269., 74856., 78531., 82294., 86145., 90084., 94111., &
  &       98226., 102429., 106720., 111099., 115566., 120121., 124764., &
  &       129495., 134314., 139221., 144216., 149299., 154470., 159729., &
  &       165076., 170511., 176034., 181645., 187344., 193131., 199006., &
  &       204969., 211020., 217159., 223386., 229701., 236104., 242595., &
  &       249174., 255841., 262596., 269439., 276370., 283389., 290496., &
  &       297691., 304974., 312345., 319804., 327351., 334986., 342709., &
  &       350520., 358419., 366406., 374481., 382644., 390895., 399234., &
  &       407661., 416176., 424779., 433470., 442249., 451116., 460071., &
  &       469114., 478245., 487464., 496771., 506166., 515649., 525220., &
  &       534879., 544626., 554461., 564384., 574395., 584494., 594681., &
  &       604956., 615319., 625770., 636309., 646936., 657651., 668454., &
  &       679345., 690324., 701391., 712546., 723789., 735120./
  !$omp simd
  do i = 1, 128
    a(i) = 7.0 * i + 16.0
    b(i) = 5.0 * i + 12.0
  end do
  !$omp simd
  do i = 1, 128
    b(i) = foo (a(i), 3, b(i))
  end do
  !$omp simd
  do i = 1, 128
    b(i) = bar (a(i), 2 * i, b(i))
  end do
  if (any (b.ne.d)) call abort
  !$omp simd
  do i = 1, 128
    b(i) = i * 2.0
  end do
  !$omp simd
  do i = 1, 128
    b(i) = baz (7.0_8, 2, b(i))
  end do
  do i = 1, 128
    if (b(i).ne.(7.0 + 4.0 * i)) call abort
  end do
contains
  function baz (x, y, z)
    !$omp declare simd (baz) simdlen (8) uniform (x, y)
    !$omp declare simd (baz)
    integer, value :: y
    real, value :: z
    real :: baz
    double precision, value :: x
    baz = x + y * z
  end function baz
end
function bar (a, b, c)
  integer, value :: b
  real, value :: c
  real :: bar
  double precision, value :: a
  !$omp declare simd (bar)
  !$omp declare simd (bar) simdlen (4) linear (b : 2)
  bar = a + b * c
end function bar
