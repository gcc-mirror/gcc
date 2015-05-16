        .macro  e r=0
        .global __epilog_$r\r
__epilog_$r\r:
        pop     $r\r
        .endm

        e       28
        e       27
        e       26
        e       25
        e       24
        e       23
        e       22
        e       21
        e       20
        e       19
        e       18
        e       17
        e       16
        e       15
        e       14
        e       13
        return

        .global __epilog24
__epilog24:
        add     $sp,$sp,24
        return

        .macro  f r=0
        .global __epilog24_$r\r
__epilog24_$r\r:
        add     $sp,$sp,24
        jmp     __epilog_$r\r
        .endm

        f       13
        f       14
        f       15
        f       16
        f       17
        f       18
        f       19
        f       20
        f       21
        f       22
        f       23
        f       24
        f       25
        f       26
        f       27
        f       28
