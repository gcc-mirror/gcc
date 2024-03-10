program main
  use iso_c_binding
  use omp_lib
  implicit none (type, external)

  integer(c_size_t), parameter :: sizeof_int = 4
  integer, parameter :: sk = c_size_t
  logical, allocatable :: isshared(:)
  integer, allocatable :: maxdim(:,:)
  integer :: ndev

  ndev = omp_get_num_devices()
  call init_isshared
  call init_maxdim

  call one
  call two
  call three
  call four

  deallocate(isshared, maxdim)
contains

  subroutine init_maxdim
    integer :: dev, dev2, r
    integer(c_size_t), parameter :: nl = 0

    allocate(maxdim(0:ndev,0:ndev))
    do dev = 0, ndev
      do dev2 = 0, ndev
        r = omp_target_memcpy_rect (c_null_ptr, c_null_ptr, nl, &
                                    num_dims=1_c_int, volume=[nl], &
                                    dst_offsets=[nl], src_offsets=[nl], &
                                    dst_dimensions=[nl], src_dimensions=[nl], &
                                    dst_device_num=dev, src_device_num=omp_initial_device)
        if (r < 3) stop 1             ! OpenMP requirement
        if (r < huge(0_c_int)) stop 2 ! GCC implementation
        maxdim(dev2,dev) = r
      end do
    end do
  end subroutine

  subroutine init_isshared
    integer :: dev
    logical :: dev_isshared

    allocate(isshared(0:ndev))
    do dev = 0, ndev
      dev_isshared = .false.
      !$omp target device(dev) map(to: dev_isshared)
        dev_isshared = .true.
      !$omp end target
      isshared(dev) = dev_isshared
    end do
  end subroutine


  subroutine one
    integer(c_size_t), parameter :: N1 = 30
    integer, target :: host_data(N1)
    type(c_ptr) :: dev_cptr(0:ndev), cptr, tmp_cptr
    integer :: dev, dev2, i, r

    do dev = 0, ndev
      dev_cptr(dev) = omp_target_alloc (N1*sizeof_int, dev)
      if (.not. c_associated (dev_cptr(dev))) stop 11
    end do

    do i = 1, N1
      host_data(i) = i
    end do

    ! copy full array host -> all devices + check value + set per-device value
    do dev = 0, ndev
      r = omp_target_memcpy_rect (dev_cptr(dev), c_loc(host_data), sizeof_int, &
                                  num_dims=1_c_int, volume=[N1], &
                                  dst_offsets=[0_sk], src_offsets=[0_sk], &
                                  dst_dimensions=[N1], src_dimensions=[N1], &
                                  dst_device_num=dev, src_device_num=omp_initial_device)
      if (r /= 0) stop 12
      cptr = dev_cptr(dev)
      !$omp target device(dev) is_device_ptr(cptr)
      block
        integer, pointer, contiguous :: fptr(:)
        call c_f_pointer(cptr, fptr, [N1])
        do i = 1, N1
           if (fptr(i) /= i) stop 13
           fptr(i) = i*100 + 10000 * (dev+3)
        end do
      end block
    end do

    ! Test strided data - forth and back - same array sizes
    do dev = 0, ndev
      do dev2 = 0, ndev
        tmp_cptr = omp_target_alloc (N1*sizeof_int, dev)
        if (.not. c_associated (tmp_cptr)) stop 14

        !$omp target device(dev) is_device_ptr(tmp_cptr)
        block
          integer, pointer, contiguous :: fptr(:)
          call c_f_pointer(tmp_cptr, fptr, [N1])
          do i = 1, N1
            fptr(i) = i*100 + 10000*(dev+1)
          end do
        end block

        if (N1-17 > N1 - max(12,13)) stop 18
        r = omp_target_memcpy_rect (dev_cptr(dev2), tmp_cptr, sizeof_int, &
                                    num_dims=1_c_int, volume=[N1-17], &
                                    dst_offsets=[12_sk], src_offsets=[13_sk], &
                                    dst_dimensions=[N1], src_dimensions=[N1], &
                                    dst_device_num=dev2, src_device_num=dev)
        if (r /= 0) stop 15

        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          logical :: checked(N1)
          integer, pointer, contiguous :: fptr(:)
          call c_f_pointer(cptr, fptr, [N1])
          checked = .false.
          do i = 1, N1-17
            if (fptr(i+12) /= (i+13)*100 + 10000 * (dev+1))  stop 16
             checked(i+12) = .true.
          end do
          ! original device value
          do i = 1, N1
            if (.not. checked(i)) then
              if (fptr(i) /= i*100 + 10000 * (dev2+3)) stop 17
            end if
          end do
        end block
        call omp_target_free (tmp_cptr, dev)
      end do

      ! reset to original value
      do dev2 = 0, ndev
        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          integer, pointer, contiguous :: fptr(:)
          call c_f_pointer(cptr, fptr, [N1])
          do i = 1, N1
                fptr(i) = i*100 + 10000 * (dev2+3)
          end do
        end block
      end do
    end do

    do dev = 0, ndev
      call omp_target_free (dev_cptr(dev), dev)
    end do
  end subroutine


  subroutine two
    integer(c_size_t), parameter :: N = 10, M = 30
    integer, target :: host_data(N,M)
    type(c_ptr) :: dev_cptr(0:ndev), cptr, tmp_cptr
    integer :: dev, dev2, i, j, r

    do dev = 0, ndev
      dev_cptr(dev) = omp_target_alloc (N*M*sizeof_int, dev)
      if (.not. c_associated (dev_cptr(dev))) stop 21
    end do

    do i = 1, M
      do j = 1, N
        host_data(j,i) = i*100 + j
      end do
    end do

    ! copy full array host -> all devices + check value + set per-device value
    do dev = 0, ndev
      r = omp_target_memcpy_rect (dev_cptr(dev), c_loc(host_data), sizeof_int, &
                                  num_dims=2_c_int, volume=[M, N], &
                                  dst_offsets=[0_sk, 0_sk], src_offsets=[0_sk, 0_sk], &
                                  dst_dimensions=[M, N], src_dimensions=[M,N], &
                                  dst_device_num=dev, src_device_num=omp_initial_device)
      if (r /= 0) stop 22
      cptr = dev_cptr(dev)
      !$omp target device(dev) is_device_ptr(cptr)
      block
        integer, pointer, contiguous :: fptr(:,:)
        call c_f_pointer(cptr, fptr, [N,M])
        do i = 1, M
          do j = 1, N
            if (fptr(j,i) /= i*100 + j) stop 23
            fptr(j,i) = i*100 + j + 1000 * dev
          end do
        end do
      end block
    end do

    ! Test strided data - forth and back - same array sizes
    do dev = 0, ndev
      do dev2 = 0, ndev
        tmp_cptr = omp_target_alloc (N*M*sizeof_int, dev)
        if (.not. c_associated (tmp_cptr)) stop 24

        !$omp target device(dev) is_device_ptr(tmp_cptr)
        block
          integer, pointer, contiguous :: fptr(:,:)
          call c_f_pointer(tmp_cptr, fptr, [N,M])
          do i = 1, M
            do j = 1, N
              fptr(j,i) = i*100 + j + 100000 * (dev+1)
            end do
          end do
        end block

        if (M-14 > M - max(5,2) &
            .or. N-3 > N - max(2,1)) stop 28
        r = omp_target_memcpy_rect (dev_cptr(dev2), tmp_cptr, sizeof_int, &
                                    num_dims=2_c_int, volume=[M-14, N-3], &
                                    dst_offsets=[5_sk, 3_sk], src_offsets=[2_sk, 1_sk], &
                                    dst_dimensions=[M, N], src_dimensions=[M,N], &
                                    dst_device_num=dev2, src_device_num=dev)
        if (r /= 0) stop 25

        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          logical :: checked(N,M)
          integer, pointer, contiguous :: fptr(:,:)
          call c_f_pointer(cptr, fptr, [N,M])
          checked = .false.
          do i = 1, M-14
            do j = 1, N-3
              if (fptr(j+3, i+5) /= (i+2)*100 + (j+1) + 100000 * (dev+1)) stop 26
              checked(j+3, i+5) = .true.
            end do
          end do
          ! original device value
          do i = 1, M
            do j = 1, N
              if (.not. checked(j,i)) then
                if (fptr(j,i) /= i*100 + j + 1000 * dev2) stop 27
              end if
            end do
          end do
        end block
        call omp_target_free (tmp_cptr, dev)
      end do

      ! reset to original value
      do dev2 = 0, ndev
        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          integer, pointer, contiguous :: fptr(:,:)
          call c_f_pointer(cptr, fptr, [N,M])
          do i = 1, M
            do j = 1, N
              fptr(j,i) = i*100 + j + 1000 * dev2
            end do
          end do
        end block
      end do
    end do

    do dev = 0, ndev
      call omp_target_free (dev_cptr(dev), dev)
    end do
  end subroutine


  subroutine three
    integer(c_size_t), parameter :: N1 = 10, N2 = 30, N3 = 15
    integer, target :: host_data(N3,N2,N1)
    type(c_ptr) :: dev_cptr(0:ndev), cptr, tmp_cptr
    integer :: dev, dev2, i, j, k, r

    do dev = 0, ndev
      dev_cptr(dev) = omp_target_alloc (N1*N2*N3*sizeof_int, dev)
      if (.not. c_associated (dev_cptr(dev))) stop 31
    end do

    do i = 1, N1
      do j = 1, N2
        do k = 1, N3
          host_data(k, j,i) = i*1000 + 100*j + k
        end do
      end do
    end do

    ! copy full array host -> all devices + check value + set per-device value
    do dev = 0, ndev
      r = omp_target_memcpy_rect (dev_cptr(dev), c_loc(host_data), sizeof_int, &
                                  num_dims=3_c_int, volume=[N1, N2, N3], &
                                  dst_offsets=[0_sk, 0_sk, 0_sk], src_offsets=[0_sk, 0_sk, 0_sk], &
                                  dst_dimensions=[N1, N2, N3], src_dimensions=[N1, N2, N3], &
                                  dst_device_num=dev, src_device_num=omp_initial_device)
      if (r /= 0) stop 32
      cptr = dev_cptr(dev)
      !$omp target device(dev) is_device_ptr(cptr)
      block
        integer, pointer, contiguous :: fptr(:,:,:)
        call c_f_pointer(cptr, fptr, [N3,N2,N1])
        do i = 1, N1
          do j = 1, N2
            do k = 1, N3
              if (fptr(k, j,i) /= i*1000 + 100*j + k) stop 33
              fptr(k,j,i) = i*1000 + 100*j + k + 1000 * dev
            end do
          end do
        end do
      end block
    end do

    ! Test strided data - forth and back - same array sizes
    do dev = 0, ndev
      do dev2 = 0, ndev
        tmp_cptr = omp_target_alloc (N1*N2*N3*sizeof_int, dev)
        if (.not. c_associated (tmp_cptr)) stop 34

        !$omp target device(dev) is_device_ptr(tmp_cptr)
        block
          integer, pointer, contiguous :: fptr(:,:,:)
          call c_f_pointer(tmp_cptr, fptr, [N3,N2,N1])
          do i = 1, N1
            do j = 1, N2
              do k = 1, N3
               fptr(k,j,i) = i*1000 + 100*j + k + 100000 * (dev+1)
              end do
            end do
          end do
        end block

        if (N1-5 > N1 - max(5,2) &
            .or. N2-13 > N2 - max(3,1) &
            .or. N3-5  > N3 - max(2,4)) stop 38
        r = omp_target_memcpy_rect (dev_cptr(dev2), tmp_cptr, sizeof_int, &
                                    num_dims=3_c_int, volume=[N1-5, N2-13,N3-5], &
                                    dst_offsets=[5_sk, 3_sk,2_sk], src_offsets=[2_sk, 1_sk,4_sk], &
                                    dst_dimensions=[N1,N2,N3], src_dimensions=[N1,N2,N3], &
                                    dst_device_num=dev2, src_device_num=dev)
        if (r /= 0) stop 35

        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          logical :: checked(N3,N2,N1)
          integer, pointer, contiguous :: fptr(:,:,:)
          call c_f_pointer(cptr, fptr, [N3,N2,N1])
          checked = .false.
          do i = 1, N1-5
            do j = 1, N2-13
              do k = 1, N3-5
                if (fptr(k+2, j+3, i+5) /= (i+2)*1000 + 100*(j+1) + (k+4) + 100000 * (dev+1))  stop 36
                checked(k+2, j+3, i+5) = .true.
              end do
            end do
          end do
          ! original device value
          do i = 1, N1
            do j = 1, N2
              do k = 1, N3
                if (.not. checked(k,j,i)) then
                  if (fptr(k,j,i) /= i*1000 + 100*j + k + 1000 * dev2) stop 37
                end if
              end do
            end do
          end do
        end block
        call omp_target_free (tmp_cptr, dev)
      end do

      ! reset to original value
      do dev2 = 0, ndev
        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          integer, pointer, contiguous :: fptr(:,:,:)
          call c_f_pointer(cptr, fptr, [N3,N2,N1])
          do i = 1, N1
            do j = 1, N2
              do k = 1, N3
                fptr(k,j,i) = i*1000 + 100*j + k + 1000 * dev2
              end do
            end do
          end do
        end block
      end do
    end do

    do dev = 0, ndev
      call omp_target_free (dev_cptr(dev), dev)
    end do
  end subroutine


  subroutine four
    integer(c_size_t), parameter :: N1 = 10, N2 = 30, N3 = 15, N4 = 25
    integer, target :: host_data(N4, N3,N2,N1)
    type(c_ptr) :: dev_cptr(0:ndev), cptr, tmp_cptr
    integer :: dev, dev2, i, j, k, ll, r

    do dev = 0, ndev
      dev_cptr(dev) = omp_target_alloc (N1*N2*N3*N4*sizeof_int, dev)
      if (.not. c_associated (dev_cptr(dev))) stop 41
    end do

    do i = 1, N1
      do j = 1, N2
        do k = 1, N3
          do ll = 1, N4
            host_data(ll, k, j,i) = i*1000 + 100*j + k*10 + ll
          end do
        end do
      end do
    end do

    ! copy full array host -> all devices + check value + set per-device value
    do dev = 0, ndev
      r = omp_target_memcpy_rect (dev_cptr(dev), c_loc(host_data), sizeof_int, &
                                  num_dims=4_c_int, volume=[N1, N2, N3, N4], &
                                  dst_offsets=[0_sk, 0_sk, 0_sk, 0_sk], src_offsets=[0_sk, 0_sk, 0_sk, 0_sk], &
                                  dst_dimensions=[N1, N2, N3, N4], src_dimensions=[N1, N2, N3, N4], &
                                  dst_device_num=dev, src_device_num=omp_initial_device)
      if (r /= 0) stop 42
      cptr = dev_cptr(dev)
      !$omp target device(dev) is_device_ptr(cptr)
      block
        integer, pointer, contiguous :: fptr(:,:,:,:)
        call c_f_pointer(cptr, fptr, [N4,N3,N2,N1])
        do i = 1, N1
          do j = 1, N2
            do k = 1, N3
              do ll = 1, N4
                if (fptr(ll, k, j,i) /= i*1000 + 100*j + k*10 + ll) stop 43
                fptr(ll,k,j,i) = i*1000 + 100*j + k*10 + ll + 1000 * dev
              end do
            end do
          end do
        end do
      end block
    end do

    ! Test strided data - forth and back - same array sizes
    do dev = 0, ndev
      do dev2 = 0, ndev
        tmp_cptr = omp_target_alloc (N1*N2*N3*N4*sizeof_int, dev)
        if (.not. c_associated (tmp_cptr)) stop 44

        !$omp target device(dev) is_device_ptr(tmp_cptr)
        block
          integer, pointer, contiguous :: fptr(:,:,:,:)
          call c_f_pointer(tmp_cptr, fptr, [N4,N3,N2,N1])
          do i = 1, N1
            do j = 1, N2
              do k = 1, N3
                do ll = 1, N4
                  fptr(ll,k,j,i) = i*1000 + 100*j + k*10 + ll + 100000 * (dev+1)
                end do
              end do
            end do
          end do
        end block

        if (N1-5 > N1 - max(5,2) &
            .or. N2-13 > N2 - max(3,1) &
            .or. N3-5  > N3 - max(2,4) &
            .or. N4-11  > N4 - max(7,5)) stop 48
        r = omp_target_memcpy_rect (dev_cptr(dev2), tmp_cptr, sizeof_int, &
                                    num_dims=4_c_int, volume=[N1-5, N2-13,N3-5,N4-11], &
                                    dst_offsets=[5_sk, 3_sk,2_sk,7_sk], src_offsets=[2_sk, 1_sk,4_sk,5_sk], &
                                    dst_dimensions=[N1,N2,N3,N4], src_dimensions=[N1,N2,N3,N4], &
                                    dst_device_num=dev2, src_device_num=dev)
        if (r /= 0) stop 45

        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          logical, allocatable :: checked(:,:,:,:) ! allocatble to reduce stack size
          integer, pointer, contiguous :: fptr(:,:,:,:)
          call c_f_pointer(cptr, fptr, [N4,N3,N2,N1])
          allocate (checked(N4,N3,N2,N1), source=.false.)
          do i = 1, N1-5
            do j = 1, N2-13
              do k = 1, N3-5
                do ll = 1, N4-11
                  if (fptr(ll+7, k+2, j+3, i+5) /= (i+2)*1000 + 100*(j+1) + (k+4)*10 + ll+5 + 100000 * (dev+1))  stop 46
                  checked(ll+7, k+2, j+3, i+5) = .true.
                end do
              end do
            end do
          end do
          ! original device value
          do i = 1, N1
            do j = 1, N2
              do k = 1, N3
                do ll = 1, N4
                  if (.not. checked(ll,k,j,i)) then
                    if (fptr(ll,k,j,i) /= i*1000 + 100*j + k*10 + ll + 1000 * dev2) stop 47
                  end if
                end do
              end do
            end do
          end do
          deallocate (checked)
        end block
        call omp_target_free (tmp_cptr, dev)
      end do

      ! reset to original value
      do dev2 = 0, ndev
        cptr = dev_cptr(dev2)
        !$omp target device(dev2) is_device_ptr(cptr)
        block
          integer, pointer, contiguous :: fptr(:,:,:,:)
          call c_f_pointer(cptr, fptr, [N4,N3,N2,N1])
          do i = 1, N1
            do j = 1, N2
              do k = 1, N3
                do ll = 1, N4
                  fptr(ll,k,j,i) = i*1000 + 100*j + k*10 + ll + 1000 * dev2
                end do
              end do
            end do
          end do
        end block
      end do
    end do

    do dev = 0, ndev
      call omp_target_free (dev_cptr(dev), dev)
    end do
  end subroutine
end program
