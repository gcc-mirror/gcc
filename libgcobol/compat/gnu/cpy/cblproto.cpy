       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
       identification division.
       program-id. CBL_ALLOC_MEM prototype.
       data division.
       linkage section.
       01 mem-pointer usage pointer.
       01 mem-size pic x(8) comp-5.
       01 flags pic x(8) comp-5.
       77 status-code pic x(2) comp-5.
       procedure division using mem-pointer
                          by value mem-size
                          by value flags
                          returning status-code.
       end program CBL_ALLOC_MEM.

       identification division.
       program-id. CBL_FREE_MEM prototype.
       data division.
       linkage section.
       01 mem-pointer usage pointer.
       77 status-code pic x(2) comp-5.
       procedure division using by value mem-pointer
                          returning status-code.
       end program CBL_FREE_MEM.

       identification division.
       program-id. CBL_CREATE_FILE prototype.
       data division.
       linkage section.
       01 filename pic x any length.
       01 access-mode pic x comp-x.
       01 deny-mode pic x comp-x.
       01 device pic x comp-x.
       01 file-handle pic x(4) comp-5.
       procedure division using filename
                                access-mode
                                deny-mode
                                device
                                file-handle.
       end program CBL_CREATE_FILE.

       identification division.
       program-id. CBL_DELETE_FILE prototype.
       data division.
       linkage section.
       01 filename pic x any length.
       77 status-code pic x(2) comp-5.
       procedure division using filename
                          returning status-code.
       end program CBL_DELETE_FILE.

       identification division.
       program-id. CBL_OPEN_FILE prototype.
       data division.
       linkage section.
       01 filename pic x any length.
       01 access-mode pic x comp-x.
       01 deny-mode pic x comp-x.
       01 device pic x comp-x.
       01 file-handle pic x(4) comp-5.
       procedure division using filename
                                access-mode
                                deny-mode
                                device
                                file-handle.
       end program CBL_OPEN_FILE.

       identification division.
       program-id. CBL_READ_FILE prototype.
       data division.
       linkage section.
       01 file-handle pic x(4) comp-5.
       01 file-offset pic x(8) comp-x.
       01 byte-count pic x(4) comp-x.
       01 flags pic x comp-x.
       01 buffer pic x any length.
       procedure division using file-handle
                                file-offset
                                byte-count
                                flags
                                buffer.
       end program CBL_READ_FILE.

       identification division.
       program-id. CBL_WRITE_FILE prototype.
       data division.
       linkage section.
       01 file-handle pic x(4) comp-5.
       01 file-offset pic x(8) comp-x.
       01 byte-count pic x(4) comp-x.
       01 flags pic x comp-x.
       01 buffer pic x any length.
       procedure division using file-handle
                                file-offset
                                byte-count
                                flags
                                buffer.
       end program CBL_WRITE_FILE.

       identification division.
       program-id. CBL_CLOSE_FILE prototype.
       data division.
       linkage section.
       01 file-handle pic x(4) comp-5.
       procedure division using file-handle.
       end program CBL_CLOSE_FILE.

       identification division.
       program-id. CBL_CHECK_FILE_EXIST prototype.
       data division.
       linkage section.
        COPY "cbltypes.cpy".
       01 filename pic x any length.
       01 file-details type cblt-fileexist-buf.
       77 status-code pic x(2) comp-5.
       procedure division using filename
                                file-details
                                returning status-code.
       end program CBL_CHECK_FILE_EXIST.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_GET_PROGRAM_INFO PROTOTYPE.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "cbltypes.cpy".
       01 fn pic x(4) comp-5.
       01 param-block type cblt-prog-info-params.
       01 return-buf pic x any length.
       01 return-buf-len pic x(4) comp-5.
       77 status-code pic x(2) comp-5.
       procedure division using by value fn
                                by reference param-block
                                by reference return-buf
                                by reference return-buf-len
                                returning status-code.

       END PROGRAM CBL_GET_PROGRAM_INFO.
       >>POP SOURCE FORMAT

       >>PUSH SOURCE FORMAT
       >>SOURCE FIXED
        Identification Division.
        Function-Id. COBRT-FILE-STATUS prototype.
        Data Division.
        Linkage Section.
        01 ERRNO BINARY-LONG.
        01 FILE-STATUS PIC X(2) COMP-5.

        Procedure Division
            Returning FILE-STATUS.
       End Function COBRT-FILE-STATUS.
       >>POP SOURCE FORMAT
