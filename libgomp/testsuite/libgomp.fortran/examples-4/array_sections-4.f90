! { dg-do run }

  call foo ()
contains
  subroutine foo ()
    integer, target :: A(30)
    integer, pointer :: p(:)
    !$omp target data map(A(1:10))
      p => A
      !$omp target map(p(4:10)) map(A(1:10))
        A(3) = 777
        p(9) = 777
        A(9) = 999
      !$omp end target
    !$omp end target data
    if (A(3) /= 777 .or. A(9) /= 999) stop 1
  end subroutine
end
