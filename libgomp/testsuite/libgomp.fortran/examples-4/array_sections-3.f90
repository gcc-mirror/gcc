! { dg-do run }

  call foo ()
contains
  subroutine foo ()
    integer, target :: A(30)
    integer, pointer :: p(:)
    !$omp target data map(A(1:4))
      p => A
      !$omp target map(p(8:27)) map(A(1:4))
        A(3) = 777
        p(9) = 777
      !$omp end target
    !$omp end target data
    if (A(3) /= 777 .or. A(9) /= 777) call abort
  end subroutine
end
