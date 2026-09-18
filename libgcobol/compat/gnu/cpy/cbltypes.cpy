        >> PUSH source format
        >>SOURCE format is fixed

      * The information is returned to the file-info argument, which
      * is defined as the following 16-byte area:

        01  cblt-fileexist-buf    typedef.
          03  cblte-fe-filesize   PIC X(8) COMP-X.
          03  cblte-fe-date.
            05 cblte-fe-day       PIC X COMP-X.
            05 cblte-fe-month     PIC X COMP-X.
            05 cblte-fe-year      PIC X(2) comp-x.
          03  cblte-fe-time.
            05 cblte-fe-hours     PIC X COMP-X.
            05 cblte-fe-minutes   PIC X COMP-X.
            05 cblte-fe-seconds   PIC X COMP-X.
            05 cblte-fe-hundreths PIC X COMP-X.

         01 cblt-prog-info-params typedef.
          03 cblte-gpi-size       pic x(4) comp-5.
          03 cblte-gpi-flags      pic x(4) comp-5.
          03 cblte-gpi-handle     usage pointer.
          03 cblte-gpi-prog-id    usage pointer.
          03 cblte-gpi-attrs      pic x(4) comp-5.

        >> POP source format
