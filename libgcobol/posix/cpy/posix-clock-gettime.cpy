      >>PUSH SOURCE FORMAT
      >>SOURCE FIXED
        identification division.
        function-id. posix-clock-gettime prototype.
        data division.
        linkage section.
          77 return-value binary-long signed.
          01 lk-timespec.
            05 tv_sec     usage is binary-double  unsigned.
            05 tv_nsec    usage is binary-double  unsigned.
          01 lk-clockid_t usage is binary-double.
        procedure division using
             by value lk-clockid_t
             by reference lk-timespec
             returning return-value.
        end function posix-clock-gettime.
      >>POP SOURCE FORMAT
