! { dg-do run }

module m
  integer :: x = 6
  integer :: w, y
  target :: y

contains
  function foo ()
    integer, pointer :: foo
    if (w /= 0) &
      error stop
    foo => y
  end
end module 

program main
  use m
  implicit none
  integer :: v, r
  !$omp atomic
  x = min (8, x)
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop

  !$omp atomic compare
  if (x == 6) x = 7
  !$omp atomic read
  v = x
  if (v /= 7) &
    error stop

  !$omp atomic
  x = min (x, 4)
  !$omp atomic read
  v = x
  if (v /= 4) &
    error stop
  !$omp atomic capture
  x = max(x, 8)
  v = x
  if (v /= 8) &
    error stop

  !$omp atomic read
  v = x
  if (x /= 8) &
    error stop
  !$omp atomic capture
  v = x
  x = max (12, x)
  if (v /= 8) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic capture
    v = x
    x = max(4, x)
  if (v /= 12) &
    error stop
  !$omp atomic read
    v = x
  if (v /= 12) &
    error stop
  !$omp atomic capture compare
  if (x == 4) then
    x = 4
  else
    v = x
  endif
  if (v /= 12) &
    error stop
  !$omp atomic write
  x = -32
  !$omp atomic capture seq_cst fail(relaxed)
    x = max(x, 12_8)
    v = x
  if (v /= 12) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic compare
  if (x == 12) x = 16
  !$omp atomic read
  v = x
  if (v /= 16) &
    error stop
  r = 57
  !$omp atomic compare capture
  if (x == 16) then
    x = r + 7
  else
    v = x
  endif
  if (v /= 16) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 64) &
    error stop
  !$omp atomic compare capture
  v = x
  if (x == 64) x = 16
  if (v /= 64) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 16) &
    error stop
  
  !$omp atomic capture, update, compare seq_cst fail(acquire)
    v = x
    if (x == 73_8 - r) x = 12_2
  if (v /= 16) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic update, compare, capture
    if (x == 69_2 - r) x = 6_8
    v = x
  if (v /= 6) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop
  !$omp atomic
    x = min(x, 8)
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop
  !$omp atomic compare
  if (x == 6) x = 8
  !$omp atomic read
  v = x
  if (v /= 8) &
    error stop
  !$omp atomic
  x = min(4,x)
  !$omp atomic read
  v = x
  if (v /= 4) &
    error stop
  !$omp atomic capture
  x = max(8_2, x)
  v = x
  if (v /= 8) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 8) &
    error stop
  !$omp atomic capture
  v = x
  x = max(12_1, x)
  if (v /= 8) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic capture
  v = x
  x = max(x, 4_1)
  if (v /= 12) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic write
  x = -32
  !$omp atomic capture ,seq_cst fail ( relaxed )
  x = max(10_1 + 2_8, x)
  v = x
  !$omp end atomic
  if (v /= 12) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic compare
  if (x == 12) x = 16
  !$omp atomic read
  v = x
  if (v /= 16) &
    error stop
  r = 57
  !$omp atomic compare capture
  if (x == 15) x = r + 7; v = x
  if (v /= 16) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 16) &
    error stop
  !$omp atomic capture, update, compare seq_cst fail(acquire)
  v = x; if (x == 73_8 - r) x = 12_8
  !$omp end atomic
  if (v /= 16) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 12) &
    error stop
  !$omp atomic update, compare, capture
  if (x == 69_2 - r) x = 6_1; v = x
  if (v /= 6) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop
  v = 24
  !$omp atomic compare capture
  if (x == 12) then; x = 16; else; v = x; endif
  if (v /= 6) &
    error stop
  v = 32
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop
  v = 147
  !$omp atomic capture compare
  if (x == 6) then; x = 57; else; v = x; endif
  if (v /= 147) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 57) &
    error stop
  !$omp atomic update, compare, weak, seq_cst, fail (relaxed)
  if (x == 137) x = 174
  !$omp atomic read
  v = x
  if (v /= 57) &
    error stop
  !$omp atomic compare fail (relaxed)
  if (x == 57_2) x = 6_8
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop
  v = -5
  !$omp atomic capture compare
  if (x == 17) then; x = 25; else; v = x; endif
  if (v /= 6) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 6) &
    error stop
  v = 15
  !$omp atomic capture compare
  if (x == 6) then; x = 23; else; v = x; endif
  if (v /= 15) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 23) &
    error stop
  w = 1
  !$omp atomic compare capture
  ! if (x == 23) then; x = 57; else; foo () = x; endif  ! OpenMP 6
  if (x == 23) then; x = 57; else; y = x; endif
  !$omp atomic read
  v = x
  if (v /= 57) &
    error stop
  !$omp atomic capture update compare
  ! if (x == 57) then; x = 23; else; foo () = x; endif  ! OpenMP 6
  if (x == 57) then; x = 23; else; y = x; endif
  !$omp atomic read
  v = x
  if (v /= 23) &
    error stop
  w = 0
  !$omp atomic compare capture
  ! if (x == 24) then; x = 57; else; foo () = x; endif  ! OpenMP 6
  if (x == 24) then; x = 57; else; y = x; endif
  if (y /= 23) &
    error stop
  !$omp atomic read
  v = x
  if (v /= 23) &
    error stop
  y = -5
  !$omp atomic capture update compare
    if (x == 57) then
      x = 27
    else
      ! foo () = x  ! OpenMP 6
      y = x
    end if
  !$omp end atomic
  if (y /= 23) &
    error stop 
  !$omp atomic read
  v = x
  if (v /= 23) &
    error stop
end
