! { dg-do run }

program main
  implicit none

  integer :: v
  !$omp target map(from:v)
  v = on ()
  !$omp end target

  select case (v)
    case default
      write (*,*) "Host fallback or unknown offloading"
    case (1)
      write (*,*) "Offloading to NVidia PTX"
    case (2)
      write (*,*) "Offloading to AMD GCN"
  end select
contains
  integer function on_nvptx ()
    on_nvptx = 1
  end function

  integer function on_gcn ()
    on_gcn = 2
  end function

  integer function on ()
    !$omp declare variant (on_nvptx) match(construct={target},device={arch(nvptx)})
    !$omp declare variant (on_gcn) match(construct={target},device={arch(gcn)})
    on = 0
  end function
end program
