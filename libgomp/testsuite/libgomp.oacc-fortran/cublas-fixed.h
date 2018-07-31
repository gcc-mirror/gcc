! CUDA BLAS interface binding for SAXPY.
      
      use iso_c_binding
      interface
        subroutine cublassaxpy(N, alpha, x, incx, y, incy)
     1    bind(c, name="cublasSaxpy")
          use iso_c_binding
          integer(kind=c_int), value :: N
          real(kind=c_float), value :: alpha
          type(*), dimension(*) :: x
          integer(kind=c_int), value :: incx
          type(*), dimension(*) :: y
          integer(kind=c_int), value :: incy
        end subroutine cublassaxpy
      end interface

