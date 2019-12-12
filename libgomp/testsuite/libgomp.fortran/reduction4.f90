! { dg-do run }
!$ use omp_lib

  integer (kind = 4) :: i, ia (6), j, ja (6), k, ka (6), ta (6), n, cnt, x
  logical :: v

  i = int(Z'ffff0f')
  ia = int(Z'f0ff0f')
  j = int(Z'0f0000')
  ja = int(Z'0f5a00')
  k = int(Z'055aa0')
  ka = int(Z'05a5a5')
  v = .false.
  cnt = -1
  x = not(0)

!$omp parallel num_threads (3) private (n) reduction (.or.:v) &
!$omp & reduction (iand:i, ia) reduction (ior:j, ja) reduction (ieor:k, ka)
!$ if (i .ne. x .or. any (ia .ne. x)) v = .true.
!$ if (j .ne. 0 .or. any (ja .ne. 0)) v = .true.
!$ if (k .ne. 0 .or. any (ka .ne. 0)) v = .true.
  n = omp_get_thread_num ()
  if (n .eq. 0) then
    cnt = omp_get_num_threads ()
    i = int(Z'ff7fff')
    ia(3:5) = int(Z'fffff1')
    j = int(Z'078000')
    ja(1:3) = 1
    k = int(Z'78')
    ka(3:6) = int(Z'f0f')
  else if (n .eq. 1) then
    i = int(Z'ffff77')
    ia(2:5) = int(Z'ffafff')
    j = int(Z'007800')
    ja(2:5) = 8
    k = int(Z'57')
    ka(3:4) = int(Z'f0108')
  else
    i = int(Z'777fff')
    ia(1:2) = int(Z'fffff3')
    j = int(Z'000780')
    ja(5:6) = int(Z'f00')
    k = int(Z'1000')
    ka(6:6) = int(Z'777')
  end if
!$omp end parallel
  if (v) stop 1
  if (cnt .eq. 3) then
    ta = (/int(Z'f0ff03'), int(Z'f0af03'), int(Z'f0af01'), int(Z'f0af01'), int(Z'f0af01'), int(Z'f0ff0f')/)
    if (i .ne. int(Z'777f07') .or. any (ia .ne. ta)) stop 2
    ta = (/int(Z'f5a01'), int(Z'f5a09'), int(Z'f5a09'), int(Z'f5a08'), int(Z'f5f08'), int(Z'f5f00')/)
    if (j .ne. int(Z'fff80') .or. any (ja .ne. ta)) stop 3
    ta = (/int(Z'5a5a5'), int(Z'5a5a5'), int(Z'aaba2'), int(Z'aaba2'), int(Z'5aaaa'), int(Z'5addd')/)
    if (k .ne. int(Z'54a8f') .or. any (ka .ne. ta)) stop 4
  end if
end
