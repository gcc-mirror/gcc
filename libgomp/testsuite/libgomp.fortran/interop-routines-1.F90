      program main
        use iso_c_binding, only: c_intptr_t, c_ptr, c_associated
#ifndef USE_OMP_HEADER
        use omp_lib
#endif
        implicit none (type, external)

#ifdef USE_OMP_HEADER
        include "omp_lib.h"
#endif

        integer(omp_interop_kind) :: interop = omp_interop_none
        integer(omp_interop_rc_kind) :: ret_code
        integer(omp_interop_fr_kind) :: fr
        integer(omp_interop_property_kind) :: ipr

        integer(c_intptr_t) :: ival
        type(c_ptr) :: ptr
        character(len=:), pointer :: str

        if (omp_irc_no_value /= 1) stop 1
        if (omp_irc_success /= 0) stop 2
        if (omp_irc_empty /= -1) stop 3
        if (omp_irc_out_of_range /= -2) stop 4
        if (omp_irc_type_int /= -3) stop 5
        if (omp_irc_type_ptr /= -4) stop 6
        if (omp_irc_type_str /= -5) stop 7
        if (omp_irc_other /= -6) stop 8

        ! Check values, including invalid values.
        do ret_code = omp_irc_other - 1, omp_irc_no_value + 1
          str => omp_get_interop_rc_desc (interop, ret_code)
          if (ret_code < omp_irc_other                                          &
     &        .or. ret_code > omp_irc_no_value) then
            ! Assume disassociated for an invalid value.
            if (associated (str)) stop 9
          else if (ret_code == omp_irc_other) then
            ! Likely not to exist in an implementation; esp. not for
            ! omp_interop_none. Thus, assume disassociated.
            ! In GCC, omp_irc_other is used on the device side, only, to
            ! complain about omp_get_interop_{int,ptr,str} usage.
            if (associated (str)) stop 10
          else
            ! Assume that omp_get_interop_rc_desc handles all of those and
            ! not only omp_irc_empty (and possibly omp_irc_out_of_range),
            ! which do occur for omp_interop_none.
            ! Assume some sensible message, i.e. at least 5 characters.
            if (len_trim (str) <= 5) stop 11
          end if
        end do

        if (omp_ifr_last < omp_ifr_hsa) stop 12

        do fr = omp_ifr_cuda, omp_ifr_last
          select case (fr)
            ! Expect the id values from the additional-definition document.
            case (omp_ifr_cuda)
              if (fr /= 1) stop 13
            case (omp_ifr_cuda_driver)
              if (fr /= 2) stop 14
            case (omp_ifr_opencl)
              if (fr /= 3) stop 15
            case (omp_ifr_sycl)
              if (fr /= 4) stop 16
            case (omp_ifr_hip)
              if (fr /= 5) stop 17
            case (omp_ifr_level_zero)
              if (fr /= 6) stop 18
            case (omp_ifr_hsa)
              if (fr /= 7) stop 19
            case default
              ! Valid, but unexpected to have more interop types.
              stop 20
          end select
        end do

        if (omp_ipr_first > omp_ipr_targetsync                                  &
     &      .or. (omp_ipr_fr_id                                                 &
     &            >= omp_get_num_interop_properties (interop)))                 &
     &    stop 21

        do ipr = omp_ipr_first,                                                 &
     &           omp_get_num_interop_properties (interop) - 1
          ! As interop == omp_interop_none, NULL is permissible;
          ! nonetheless, require != NULL for the GCC implementation.
          str => omp_get_interop_name (interop, ipr)
          select case (ipr)
            case (omp_ipr_fr_id)
              if (ipr /= -1 .or. str /= "fr_id")                                &
     &          stop 21
            case (omp_ipr_fr_name)
              if (ipr /= -2 .or. str /= "fr_name")                              &
     &          stop 22
            case (omp_ipr_vendor)
              if (ipr /= -3 .or. str /= "vendor")                               &
     &          stop 23
            case (omp_ipr_vendor_name)
              if (ipr /= -4 .or. str /= "vendor_name")                          &
     &          stop 24
            case (omp_ipr_device_num)
              if (ipr /= -5 .or. str /= "device_num")                           &
     &          stop 25
            case (omp_ipr_platform)
              if (ipr /= -6 .or. str /= "platform")                             &
     &          stop 26
            case (omp_ipr_device)
              if (ipr /= -7 .or. str /= "device")                               &
     &          stop 27
            case (omp_ipr_device_context)
              if (ipr /= -8 .or. str /= "device_context")                       &
     &          stop 28
            case (omp_ipr_targetsync)
              if (ipr /= -9 .or. str /= "targetsync")                           &
     &          stop 29
            case default
              ! Valid, but unexpected to have more interop types,
              ! especially not for interop == omp_interop_none.
              stop 30
          end select

          ! As interop == omp_interop_none, expect NULL.
          if (associated (omp_get_interop_type_desc (interop, ipr)))            &
     &      stop 31

          ret_code = omp_irc_success
          ival = omp_get_interop_int (interop, ipr, ret_code)
          if (ret_code /= omp_irc_empty) stop 32
          if (ival /= 0) stop 33  ! Implementation choice
          str => omp_get_interop_rc_desc (interop, ret_code)
          if (len_trim (str) <= 5) stop 34
          if (str /= "provided interoperability object is equal to "            &
     &               // "omp_interop_none")                                     &
     &      stop 35  ! GCC implementation choice.
          ival = omp_get_interop_int (interop, ipr)
          if (ival /= 0) stop 33  ! Implementation choice

          ret_code = omp_irc_success
          ptr = omp_get_interop_ptr (interop, ipr, ret_code)
          if (ret_code /= omp_irc_empty) stop 36
          if (c_associated (ptr)) stop 37  ! Obvious implementation choice.
          str => omp_get_interop_rc_desc (interop, ret_code)
          if (len_trim (str) <= 5) stop 38
          if (str /= "provided interoperability object is equal to "            &
     &               // "omp_interop_none")                                     &
     &      stop 39  ! GCC implementation choice.
          ptr = omp_get_interop_ptr (interop, ipr)
          if (c_associated (ptr)) stop 37  ! Obvious implementation choice.

          ret_code = omp_irc_success
          str => omp_get_interop_str (interop, ipr, ret_code)
          if (ret_code /= omp_irc_empty) stop 40
          if (associated (str)) stop 41  ! Obvious mplementation choice
          str => omp_get_interop_rc_desc (interop, ret_code)
          if (len_trim (str) <= 5) stop 42
          if (str /= "provided interoperability object is equal to "            &
     &               // "omp_interop_none")                                     &
     &      stop 43  ! GCC implementation choice.
          str => omp_get_interop_str (interop, ipr)
          if (associated (str)) stop 41  ! Obvious mplementation choice
        end do

        ! Invalid ipr.
        ! Valid are either omp_irc_empty (due to omp_interop_none) or
        ! omp_irc_out_of_range; assume omp_irc_out_of_range with GCC.

        ! omp_ipr_targetsync-1, i.e < lower bound.

        ret_code = omp_irc_success
        ival = omp_get_interop_int (interop, omp_ipr_targetsync-1,              &
     &                              ret_code)
        if (ret_code /= omp_irc_out_of_range) stop 44
        if (ival /= 0) stop 45  ! Implementation choice.
        str => omp_get_interop_rc_desc (interop, ret_code)
        if (len_trim (str) <= 5) stop 46
        ! GCC implementation choice.
        if (str /= "property ID is out of range") stop 47
        ival = omp_get_interop_int (interop, omp_ipr_targetsync-1)
        if (ival /= 0) stop 45  ! Implementation choice.

        ret_code = omp_irc_success
        ptr = omp_get_interop_ptr (interop, omp_ipr_targetsync-1,               &
     &                             ret_code)
        if (ret_code /= omp_irc_out_of_range) stop 48
        if (c_associated (ptr)) stop 49 ! Obvious implementation choice.
        str => omp_get_interop_rc_desc (interop, ret_code)
        if (len_trim (str) <= 5) stop 50
        ! GCC implementation choice.
        if (str /= "property ID is out of range") stop 51
        ptr = omp_get_interop_ptr (interop, omp_ipr_targetsync-1)
        if (c_associated (ptr)) stop 49 ! Obvious implementation choice.

        ret_code = omp_irc_success
        str => omp_get_interop_str (interop, omp_ipr_targetsync-1,              &
     &                              ret_code)
        if (ret_code /= omp_irc_out_of_range) stop 52
        if (associated (str)) stop 53  ! Obvious implementation choice.
        str => omp_get_interop_rc_desc (interop, ret_code)
        if (len_trim (str) <= 5) stop 54
        ! GCC implementation choice.
        if (str /= "property ID is out of range") stop 55
        str => omp_get_interop_str (interop, omp_ipr_targetsync-1)
        if (associated (str)) stop 53  ! Obvious implementation choice.

        ! omp_get_num_interop_properties (), i.e > upper bound.

        ret_code = omp_irc_success
        ival = omp_get_interop_int (interop,                                    &
     &            omp_get_num_interop_properties (interop),                     &
     &                              ret_code)
        if (ret_code /= omp_irc_out_of_range) stop 56
        if (ival /= 0) stop 57  ! Implementation choice.
        str => omp_get_interop_rc_desc (interop, ret_code)
        if (len_trim (str) <= 5) stop 58
        ! GCC implementation choice.
        if (str /= "property ID is out of range") stop 59

        ret_code = omp_irc_success
        ptr = omp_get_interop_ptr (interop,                                     &
     &          omp_get_num_interop_properties (interop), ret_code)
        if (ret_code /= omp_irc_out_of_range) stop 60
        if (c_associated (ptr)) stop 61 ! Obvious implementation choice.
        str => omp_get_interop_rc_desc (interop, ret_code)
        if (len_trim (str) <= 5) stop 62
        ! GCC implementation choice.
        if (str /= "property ID is out of range") stop 63

        ret_code = omp_irc_success
        str => omp_get_interop_str (interop,                                    &
     &           omp_get_num_interop_properties (interop), ret_code)
        if (ret_code /= omp_irc_out_of_range) stop 64
        if (associated (str)) stop 65  ! Obvious implementation choice.
        str => omp_get_interop_rc_desc (interop, ret_code)
        if (len_trim (str) <= 5) stop 66
        ! GCC implementation choice.
        if (str /= "property ID is out of range") stop 67
      end
