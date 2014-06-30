! { dg-do run }

module target4
contains
  subroutine foo (a,m,n)
    integer :: m,n,i,j
    double precision :: a(m, n), t
    !$omp target data map(a) map(to: m, n)
    do i=1,n
      t = 0.0d0
      !$omp target
        !$omp parallel do reduction(+:t)
          do j=1,m
            t = t + a(j,i) * a(j,i)
          end do
      !$omp end target
      t = 2.0d0 * t
      !$omp target
        !$omp parallel do
          do j=1,m
            a(j,i) = a(j,i) * t
          end do
      !$omp end target
    end do
    !$omp end target data
  end subroutine foo
end module target4
  use target4, only : foo
  integer :: i, j
  double precision :: a(8, 9), res(8, 9)
  do i = 1, 8
    do j = 1, 9
      a(i, j) = i + j
    end do
  end do
  call foo (a, 8, 9)
  res = reshape ((/ 1136.0d0, 1704.0d0, 2272.0d0, 2840.0d0, 3408.0d0, 3976.0d0, &
&   4544.0d0, 5112.0d0, 2280.0d0, 3040.0d0, 3800.0d0, 4560.0d0, 5320.0d0, 6080.0d0, &
&   6840.0d0, 7600.0d0, 3936.0d0, 4920.0d0, 5904.0d0, 6888.0d0, 7872.0d0, 8856.0d0, &
&   9840.0d0, 10824.0d0, 6200.0d0, 7440.0d0, 8680.0d0, 9920.0d0, 11160.0d0, 12400.0d0, &
&   13640.0d0, 14880.0d0, 9168.0d0, 10696.0d0, 12224.0d0, 13752.0d0, 15280.0d0, 16808.0d0, &
&   18336.0d0, 19864.0d0, 12936.0d0, 14784.0d0, 16632.0d0, 18480.0d0, 20328.0d0, 22176.0d0, &
&   24024.0d0, 25872.0d0, 17600.0d0, 19800.0d0, 22000.0d0, 24200.0d0, 26400.0d0, 28600.0d0, &
&   30800.0d0, 33000.0d0, 23256.0d0, 25840.0d0, 28424.0d0, 31008.0d0, 33592.0d0, 36176.0d0, &
&   38760.0d0, 41344.0d0, 30000.0d0, 33000.0d0, 36000.0d0, 39000.0d0, 42000.0d0, 45000.0d0, &
&   48000.0d0, 51000.0d0 /), (/ 8, 9 /))
  if (any (a /= res)) call abort
end
