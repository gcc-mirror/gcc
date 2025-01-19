# ieee754 sf routines for FT32

/* Copyright (C) 1995-2025 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

# See http://www.ens-lyon.fr/LIP/Pub/Rapports/PhD/PhD2006/PhD2006-02.pdf
# for implementation details of all except division which is detailed below
#

#ifdef L_fp_tools
// .global __cmpsf2_
nan:            .long 0x7FFFFFFF    # also abs mask
inf:            .long 0x7F800000
sign_mask:      .long 0x80000000
m_mask:         .long 0x007FFFFF
exp_bias:       .long 127
edge_case:      .long 0x00FFFFFF
smallest_norm:  .long 0x00800000    # implicit bit
high_FF:        .long 0xFF000000
high_uint:      .long 0xFFFFFFFF

ntz_table:
    .byte   32,0,1,12,2,6,0,13,3,0,7,0,0,0,0,14
    .byte   10,4,0,0,8,0,0,25,0,0,0,0,0,21,27,15
    .byte   31,11,5,0,0,0,0,0,9,0,0,24,0,0,20,26
    .byte   30,0,0,0,0,23,0,19,29,0,22,18,28,17,16,0

#endif

# Supply a few 'missing' instructions

# not
.macro      not rd,r1
    xor     \rd,\r1,-1
.endm

# negate
.macro      neg x
    not     \x, \x
    add     \x, \x, 1
.endm

# set $cc from the result of "ashl reg,dist"
.macro  ashlcc reg,dist
    .long   0x5de04008 | (\reg << 15) | (\dist << 4)
.endm


# converts an unsigned number x to a signed rep based on the bits in sign
# sign should be 0x00000000 or 0xffffffff.
.macro      to_signed x, sign
    add     \x,\x,\sign    # conditionally decrement x
    xor     \x,\x,\sign    # conditionally complement x
.endm


.macro  ld32    r,v
    ldk     \r,(\v>>10)
    ldl     \r,\r,(\v & 1023)
.endm

# calculate trailing zero count in x, also uses scr.
# Using Seal's algorithm
.macro      ntz x, scr
    not     \scr, \x
    add     \scr, \scr, 1
    and     \x, \x, \scr
    ashl    \scr, \x, 4
    add     \x, \scr, \x
    ashl    \scr, \x, 6
    add     \x, \scr, \x
    ashl    \scr, \x, 16
    sub     \x, \scr, \x
    lshr    \x, \x, 26
    ldk     \scr, ntz_table
    add     \x, \x, \scr
    lpmi.b  \x, \x, 0
.endm

# calculate leading zero count
.macro      nlz x, scr
    flip    \x, \x, 31
    ntz     \x, \scr
.endm


# Round 26 bit mantissa to nearest
# | 23 bits frac | G | R | S |
.macro      round m,  s1, s2
    ldk     \s1,0xc8
    and     \s2,\m,7
    lshr    \s1,\s1,\s2
    and     \s1,\s1,1
    lshr    \m,\m,2
    add     \m,\m,\s1
.endm

# If NZ, set the LSB of reg
.macro      sticky reg
    jmpc    z,1f
    or      \reg,\reg,1             # set the sticky bit to 1
1:
.endm

##########################################################################
##########################################################################
## addition & subtraction

#if defined(L_subsf3) || defined(L_addsub_sf)
.global __subsf3
__subsf3:
    # this is subtraction, so we just change the sign of r1
    lpm     $r2,sign_mask
    xor     $r1,$r1,$r2
    jmp     __addsf3
#endif

#if defined(L_addsf3) || defined(L_addsub_sf)
.global __addsf3
__addsf3:
    # x in $r0, y in $r1, result z in $r0       --||| 100 instructions +/- |||--
    # unpack e, calc d
    bextu   $r2,$r0,(8<<5)|23   # ex in r2
    bextu   $r3,$r1,(8<<5)|23   # ey in r3
    sub     $r5,$r2,$r3         # d = ex - ey

    # Special values are 0x00 and 0xff in ex and ey.
    # If (ex&ey) != 0 or (xy|ey)=255 then there may be
    # a special value.
    tst     $r2,$r3
    jmpc    nz,1f
    jmp     slow
1:  or      $r4,$r2,$r3
    cmp     $r4,255
    jmpc    nz,no_special_vals
slow:
    # Check for early exit
    cmp     $r2,0
    jmpc    z,test_if_not_255
    cmp     $r3,0
    jmpc    nz,no_early_exit
test_if_not_255:
    cmp     $r2,255
    jmpc    z,no_early_exit
    cmp     $r3,255
    jmpc    z,no_early_exit
    or      $r6,$r2,$r3
    cmp     $r6,0
    jmpc    nz,was_not_zero
    and     $r0,$r0,$r1
    lpm     $r1,sign_mask
    and     $r0,$r0,$r1
    return
was_not_zero:
    cmp     $r2,0
    jmpc    nz,ret_x
    move    $r0,$r1
    return
ret_x:
    return
no_early_exit:
    # setup to test for special values
    sub     $r6,$r2,1
    and     $r6,$r6,0xFE
    sub     $r7,$r3,1
    and     $r7,$r7,0xFE
    # test for special values
    cmp     $r6,$r7
    jmpc    gte,ex_spec_is_gte
    move    $r6,$r7
ex_spec_is_gte:
    cmp     $r6,0xFE
    jmpc    nz,no_special_vals
    cmp     $r5,0
    jmpc    ns,d_gte_0
    cmp     $r3,0xFF
    jmpc    z,ret_y
    cmp     $r2,0
    jmpc    z,ret_y
ret_y:
    move    $r0,$r1
    return
d_gte_0:
    cmp     $r5,0
    jmpc    z,d_is_0
    cmp     $r2,0xFF
    jmpc    z,ret_x
    cmp     $r3,0
    jmpc    z,ret_x
d_is_0:
    cmp     $r2,0xFF
    jmpc    nz,no_special_vals
    ashl    $r6,$r0,9           # clear all except x frac
    ashl    $r7,$r1,9           # clear all except y frac
    or      $r6,$r6,$r7
    cmp     $r6,0
    jmpc    nz,ret_nan
    lshr    $r4,$r0,31          # sx in r4
    lshr    $r5,$r1,31          # sy in r4
    cmp     $r4,$r5
    jmpc    nz,ret_nan
    return
ret_nan:
    lpm     $r0,nan
    return
no_special_vals:
    ldk     $r8,(1<<10)|(9<<5)|26   # setup implicit bit and mask for e
    #----------------------
    ashr    $r4,$r0,31              # sx in r4
    ashl    $r0,$r0,3               # shift mx 3 for GRS bits
    bins    $r0,$r0,$r8             # clear sx, ex and add implicit bit mx
    # change mx to signed mantissa
    to_signed $r0,$r4
    #----------------------
    ashr    $r4,$r1,31              # sy in r4
    ashl    $r1,$r1,3               # shift my 3 for GRS bits
    bins    $r1,$r1,$r8             # clear sy, ey and add implicit bit my
    # change my to signed mantissa
    to_signed $r1,$r4
    #----------------------
    # test if we swap ms based on d sign
    cmp     $r5,0
    jmpc    gte,noswap
    # swap mx & my
    xor     $r0,$r0,$r1
    xor     $r1,$r0,$r1
    xor     $r0,$r0,$r1
    # d positive means that ex>=ey, so ez = ex
    # d negative means that ey>ex, so ez = ey
    move    $r2,$r3
    # |d|
    neg     $r5
noswap:
                                    # now $r2 = ez = max(ex,ey)
    cmp     $r5,26                  # max necessary alignment shift is 26
    jmpc    lt,under_26
    ldk     $r5,26
under_26:
    ldk     $r7,-1
    ashl    $r7,$r7,$r5             # create inverse of mask for test of S bit value in discarded my
    not     $r7,$r7
    tst     $r1,$r7                 # determine value of sticky bit
    # shift my >> |d|
    ashr    $r1,$r1,$r5
    sticky  $r1

    # add ms
    add     $r0,$r0,$r1

    # $r4 = sign(mx), mx = |mx|
    ashr    $r4,$r0,31
    xor     $r0,$r0,$r4
    sub     $r0,$r0,$r4

    # realign mantissa using leading zero count
    flip    $r7,$r0,31
    ntz     $r7,$r8
    ashl    $r0,$r0,$r7
    btst    $r0,(6<<5)|0            # test low bits for sticky again
    lshr    $r0,$r0,6
    sticky  $r0

    # update exponent
    add     $r2,$r2,5
    sub     $r2,$r2,$r7

    # Round to nearest
    round   $r0,$r7,$r6

    # detect_exp_update
    lshr    $r6,$r0,24
    add     $r2,$r2,$r6

    # final tests
    # mz == 0? if so, we just bail with a +0
    cmp     $r0,0
    jmpc    nz,msum_not_zero
    ldk     $r0,0
    return
msum_not_zero:
    # Combined check that (1 <= ez <= 254)
    sub     $r3,$r2,1
    cmp     $r3,254
    jmpc    b,no_special_ret
    # underflow?
    cmp     $r2,0
    jmpc    gt,no_under
    ldk     $r0,0
    jmp     pack_sz
no_under:
    # overflow?
    cmp     $r2,255
    jmpc    lt,no_special_ret
    ldk     $r0,0x7F8
    ashl    $r0,$r0,20
    jmp     pack_sz
no_special_ret:
    # Pack ez
    ldl     $r2,$r2,(8<<5)|23
    bins    $r0,$r0,$r2             # width = 8, pos = 23 pack ez
    # Pack sz
pack_sz:
    ldl     $r4,$r4,(1<<5)|31
    bins    $r0,$r0,$r4             # width = 1, pos = 31 set sz to sy
    return
#endif

##########################################################################
##########################################################################
## multiplication

#ifdef  L_mulsf3
.global __mulsf3
__mulsf3:
    # x in $r0, y in $r1, result z in $r0       --||| 61 instructions +/- |||--

    # unpack e
    bextu   $r2,$r0,(8<<5)|23   # ex in r2
    bextu   $r3,$r1,(8<<5)|23   # ey in r3
    # calc result sign
    xor     $r4,$r0,$r1
    lpm     $r5,sign_mask
    and     $r4,$r4,$r5         # sz in r4

    # unpack m add implicit bit
    ldk     $r5,(1<<10)|(9<<5)|23   # setup implicit bit and mask for e
    #----------------------
    bins    $r0,$r0,$r5             # clear sx, ex and add implicit bit mx

    sub     $r6,$r2,1
    cmp     $r6,254
    jmpc    b,1f
    jmp     slow_mul
1:  sub     $r6,$r3,1
    cmp     $r6,254
    jmpc    b,no_special_vals_mul

slow_mul:
    # Check for early exit
    cmp     $r2,0
    jmpc    z,op_is_zero
    cmp     $r3,0
    jmpc    nz,no_early_exit_mul
op_is_zero:
    cmp     $r2,255
    jmpc    z,no_early_exit_mul
    cmp     $r3,255
    jmpc    z,no_early_exit_mul
    move    $r0,$r4
    return
no_early_exit_mul:
    # setup to test for special values
    sub     $r6,$r2,1
    and     $r6,$r6,0xFE
    sub     $r7,$r3,1
    and     $r7,$r7,0xFE
    # test for special values
    cmp     $r6,$r7
    jmpc    gte,ex_spec_is_gte_ey_mul
    move    $r6,$r7
ex_spec_is_gte_ey_mul:
    cmp     $r6,0xFE
    jmpc    nz,no_special_vals_mul
    cmp     $r2,0xFF
    jmpc    nz,ex_not_FF_mul
    ashl    $r6,$r0,9
    cmp     $r6,0
    jmpc    nz,ret_nan
    cmp     $r3,0
    jmpc    z,ret_nan
    ashl    $r6,$r1,1
    lpm     $r7,high_FF
    cmp     $r6,$r7
    jmpc    a,ret_nan
    cmp     $r6,0
    jmpc    z,ret_nan
    # infinity
    lpm     $r0,inf
    or      $r0,$r0,$r4
    return
ex_not_FF_mul:
    cmp     $r2,0
    jmpc    nz,no_nan_mul
    cmp     $r3,0xFF
    jmpc    nz,no_nan_mul
    jmp     ret_nan
no_nan_mul:
    lpm     $r0,nan
    and     $r0,$r0,$r1
    or      $r0,$r0,$r4
    return

ret_nan:
    lpm     $r0,nan
    return

no_special_vals_mul:
    bins    $r1,$r1,$r5         # clear sy, ey and add implicit bit my
    # calc ez
    add     $r3,$r2,$r3
    sub     $r3,$r3,127         # ez in r3

    # (r1,r2) = R0 * R1
    mul     $r2,$r0,$r1
    muluh   $r1,$r0,$r1

    btst    $r1,(1<<5)|15       # XXX use jmpx
    jmpc    z,mul_z0

    # mz is 1X.XX...X
    # 48-bit product is in (r1,r2). The low 22 bits of r2
    # are discarded.
    lshr    $r0,$r2,22
    ashl    $r1,$r1,10
    or      $r0,$r0,$r1         # r0 = (r1,r2) >> 22
    ashlcc  2,10
    sticky  $r0
    add     $r3,$r3,1           # bump exponent

    # Round to nearest
    round   $r0, $r1, $r2
    lshr    $r6,$r0,24
    add     $r3,$r3,$r6

    sub     $r6,$r3,1
    cmp     $r6,254
    jmpc    b,no_special_ret_mul

special_ret_mul:
    # When the final exponent <= 0, result is flushed to 0 except
    # for the border case 0x00FFFFFF which is promoted to next higher
    # FP no., that is, the smallest "normalized" number.
    cmp     $r3,0
    jmpc    gt,exp_normal
    # Pack ez
    ldl     $r3,$r3,(8<<5)|23
    bins    $r0,$r0,$r3 # width = 8, pos = 23 pack ez
    lpm     $r2,edge_case
    cmp     $r0,$r2
    jmpc    nz,no_edge_case
    lpm     $r0,smallest_norm
    jmp     pack_sz_mul
no_edge_case:
    ldk     $r0,0
    jmp     pack_sz_mul
exp_normal:
    # overflow?
    cmp     $r3,255
    jmpc    lt,no_special_ret_mul
    ldk     $r0,0x7F8
    ashl    $r0,$r0,20
    jmp     pack_sz_mul
no_special_ret_mul:
    # Pack ez
    ldl     $r3,$r3,(8<<5)|23
    bins    $r0,$r0,$r3 # width = 8, pos = 23 pack ez
    # Pack sz
pack_sz_mul:
    or    $r0,$r0,$r4
    return

mul_z0:
    # mz is 0X.XX...X
    # 48-bit product is in (r1,r2). The low 21 bits of r2
    # are discarded.
    lshr    $r0,$r2,21
    ashl    $r1,$r1,11
    or      $r0,$r0,$r1         # r0 = (r1,r2) >> 22
    ashlcc  2,11
    sticky  $r0
    # Round to nearest
    round   $r0, $r1, $r2
    lshr    $r6,$r0,24
    add     $r3,$r3,$r6

    sub     $r6,$r3,1
    cmp     $r6,254
    jmpc    b,no_special_ret_mul
    jmp     special_ret_mul
#endif

##########################################################################
##########################################################################
## division

## See http://perso.ens-lyon.fr/gilles.villard/BIBLIOGRAPHIE/PDF/arith19.pdf
## for implementation details




#ifdef  L_divsf3
dc_1: .long             0xffffe7d7
dc_2: .long             0xffffffe8
dc_3: .long             0xffbad86f
dc_4: .long             0xfffbece7
dc_5: .long             0xf3672b51
dc_6: .long             0xfd9d3a3e
dc_7: .long             0x9a3c4390
dc_8: .long             0xd4d2ce9b
dc_9: .long             0x1bba92b3
dc_10: .long            0x525a1a8b
dc_11: .long            0x0452b1bf
dc_12: .long            0xFFFFFFC0
spec_val_test:  .long   0x7F7FFFFF

.global __divsf3
__divsf3:
    push    $r13
    # x in $r0, y in $r1, result z in $r0       --||| 73 instructions +/- |||-
    bextu   $r10,$r0,(8<<5)|23   # ex in r2
    bextu   $r11,$r1,(8<<5)|23   # ey in r3
    lpm     $r6, m_mask
    and     $r2, $r0, $r6        # mx
    and     $r3, $r1, $r6        # my
    cmp     $r2,$r3
    bextu   $r2,$r30,(1<<5)|4   # c = Tx >= T;
    ashl    $r3,$r3,9           # T = X << 9;
    lpm     $r13, sign_mask
    ashl    $r4,$r0,8           # X8 = X << 8;
    or      $r4,$r4,$r13        # Mx = X8 | 0x80000000;
    lshr    $r5,$r4,$r2         # S = Mx >> c;
    # calc D
    sub     $r2, $r11, $r2
    add     $r12, $r10, 125
    sub     $r2, $r12, $r2      # int D = (Ex + 125) - (Ey - c);
    # calc result sign
    xor     $r12,$r0,$r1
    and     $r12,$r12,$r13      # Sr = ( X ˆ Y ) & 0x80000000;
    # check early exit
    cmp     $r10, 0
    jmpc    nz, no_early_ret_dev
    cmp     $r11, 0
    jmpc    z, no_early_ret_dev
    cmp     $r11, 255
    jmpc    z, no_early_ret_dev
    move    $r0, $r12
    pop     $r13
    return
no_early_ret_dev:
 # setup to test for special values
    sub     $r8,$r10,1
    and     $r8,$r8,0xFE
    sub     $r9,$r11,1
    and     $r9,$r9,0xFE
    # test for special values
    cmp     $r8, $r9
    jmpc    gte, absXm1_gte_absYm1
    move    $r8, $r9
absXm1_gte_absYm1:
    cmp     $r8, 0xFE
    jmpc    nz, no_spec_ret_div
    cmp     $r10, 0xFF
    jmpc    nz, ex_not_FF_div
    lpm     $r6, m_mask
    and     $r2, $r0, $r6        # mx
    cmp     $r2, 0
    jmpc    nz, ret_nan_div
    cmp     $r11, 0xFF
    jmpc    z, ret_nan_div
    jmp     ret_inf_div
ex_not_FF_div:
    cmp     $r11, 0xFF
    jmpc    nz, ey_not_FF_div
    ashl    $r13, $r1, 9
    cmp     $r13, 0
    jmpc    nz, ret_nan_div
    move    $r0, $r12
    pop     $r13
    return
ey_not_FF_div:
    or      $r10, $r10, $r11
    cmp     $r10, 0
    jmpc    z, ret_nan_div
ret_inf_div:
    lpm     $r6, inf
    move    $r0, $r6
    or      $r0, $r0, $r12
    pop     $r13
    return
ret_nan_div:
    lpm     $r0, nan
    pop     $r13
    return

no_spec_ret_div:
# check for overflow
    ldk     $r6, 0xFE
    cmp     $r2, $r6
    jmpc    lt, no_overflow_div
    lpm     $r6, inf
    or      $r0, $r12, $r6
    pop     $r13
    return
no_overflow_div:
# check for underflow
    cmp     $r2, 0
    jmpc    ns, no_underflow_div
    xnor    $r6, $r6, $r6       # -1
    cmp     $r2, $r6
    jmpc    nz, ret_sr_div
    ldk     $r7, 0xFF
    xor     $r6, $r6, $r7       # 0xFF ^ -1 = 0xFFFFFF00
    cmp     $r4, $r6
    jmpc    nz, ret_sr_div
    lpm     $r6, sign_mask
    cmp     $r4, $r6
    jmpc    nz, ret_sr_div
    lshr    $r0, $r6, 8
    or      $r0, $r0, $r12
    pop     $r13
    return
ret_sr_div:
    move    $r0, $r12
    pop     $r13
    return
no_underflow_div:
    lpm     $r6, dc_1
    muluh   $r7, $r3, $r6       # i0 = mul( T , 0xffffe7d7 );
    lpm     $r6, dc_2
    sub     $r7, $r6, $r7       # i1 = 0xffffffe8 - i0;
    muluh   $r7, $r5, $r7       # i2 = mul( S , i1 );
    add     $r7, $r7, 0x20      # i3 = 0x00000020 + i2;
    muluh   $r8, $r3, $r3       # i4 = mul( T , T );
    muluh   $r9, $r5, $r8       # i5 = mul( S , i4 );
    lpm     $r6, dc_3
    muluh   $r10, $r3, $r6      # i6 = mul( T , 0xffbad86f );
    lpm     $r6, dc_4
    sub     $r10, $r6, $r10     # i7 = 0xfffbece7 - i6;
    muluh   $r10, $r9, $r10     # i8 = mul( i5 , i7 );
    add     $r7, $r7, $r10      # i9 = i3 + i8;
    muluh   $r9, $r8, $r9       # i10 = mul( i4 , i5 );
    lpm     $r6, dc_5
    muluh   $r10, $r3, $r6      # i11 = mul( T , 0xf3672b51 );
    lpm     $r6, dc_6
    sub     $r10, $r6, $r10     # i12 = 0xfd9d3a3e - i11;
    lpm     $r6, dc_7
    muluh   $r11, $r3, $r6      # i13 = mul( T , 0x9a3c4390 );
    lpm     $r6, dc_8
    sub     $r11, $r6, $r11     # i14 = 0xd4d2ce9b - i13
    muluh   $r11, $r8, $r11     # i15 = mul( i4 , i14 );
    add     $r10, $r10, $r11    # i16 = i12 + i15;
    muluh   $r10, $r9, $r10     # i17 = mul( i10 , i16 )
    add     $r7, $r7, $r10      # i18 = i9 + i17;
    muluh   $r10, $r8, $r8      # i19 = mul( i4 , i4 );
    lpm     $r6, dc_9
    muluh   $r11, $r3, $r6      # i20 = mul( T , 0x1bba92b3 );
    lpm     $r6, dc_10
    sub     $r11, $r6, $r11     # i21 = 0x525a1a8b - i20;
    lpm     $r6, dc_11
    muluh   $r8, $r8, $r6       # i22 = mul( i4 , 0x0452b1bf );
    add     $r8, $r11, $r8      # i23 = i21 + i22;
    muluh   $r8, $r10, $r8      # i24 = mul( i19 , i23 );
    muluh   $r8, $r9, $r8       # i25 = mul( i10 , i24 );
    add     $r3, $r7, $r8       # V = i18 + i25;
# W = V & 0xFFFFFFC0;
    lpm     $r6, dc_12
    and     $r3, $r3, $r6   # W
# round and pack final values
    ashl    $r0, $r2, 23        # pack D
    or      $r0, $r0, $r12      # pack Sr
    ashl    $r12, $r1, 8
    or      $r12, $r12, $r13    # My
    muluh   $r10, $r3, $r12
    lshr    $r11, $r5, 1
    cmp     $r10, $r11
    jmpc    gte, div_ret_1
    add     $r3, $r3, 0x40
div_ret_1:
    lshr    $r3, $r3, 7
    add     $r0, $r0, $r3
    pop     $r13
    return
#endif

##########################################################################
##########################################################################
## Negate

#ifdef L_negsf
.global __negsf
__negsf:
    lpm     $r1, sign_mask
    xor     $r0, $r0, $r1
    return
#endif

##########################################################################
##########################################################################
## float to int & unsigned int

#ifdef L_fixsfsi
.global __fixsfsi
__fixsfsi: # 20 instructions
    bextu   $r1,$r0,(8<<5)|23   # e in r1
    lshr    $r2,$r0,31          # s in r2
    lpm     $r3, m_mask
    and     $r0,$r0,$r3         # m in r0
    # test nan
    cmp     $r1,0xFF
    jmpc    nz, int_not_nan
    cmp     $r0,0
    jmpc    z, int_not_nan
    ldk     $r0,0
    return
int_not_nan:
    # test edges
    cmp     $r1, 127
    jmpc    gte, int_not_zero   # lower limit
    ldk     $r0,0
    return
int_not_zero:
    cmp     $r1, 158
    jmpc    lt, int_not_max    # upper limit
    lpm     $r0, nan
    cmp     $r2, 0
    jmpc    z, int_positive
    xnor    $r0, $r0, 0
    return
int_not_max:
    lpm     $r3, smallest_norm
    or      $r0, $r0, $r3       # set implicit bit
    sub     $r1, $r1, 150
    cmp     $r1, 0
    jmpc    s, shift_right
    ashl    $r0, $r0, $r1
    jmp     set_int_sign
shift_right:
    xnor    $r1, $r1, 0
    add     $r1, $r1, 1
    lshr    $r0, $r0, $r1
set_int_sign:
    cmp     $r2, 0
    jmpc    z, int_positive
    xnor    $r0, $r0, 0
    add     $r0, $r0, 1
int_positive:
    return
#endif

#ifdef L_fixunssfsi
.global __fixunssfsi
__fixunssfsi: # 19 instructions
    lshr    $r2, $r0, 31          # s in r2
    cmp     $r2, 0
    jmpc    z, uint_not_neg
    ldk     $r0, 0
    return
uint_not_neg:
    bextu   $r1, $r0, (8<<5)|23   # e in r1
    sub     $r1, $r1, 127
    lpm     $r3, m_mask
    and     $r0, $r0, $r3         # m in r0
    # test nan
    cmp     $r1, 0xFF
    jmpc    nz, uint_not_nan
    cmp     $r0, 0
    jmpc    z, uint_not_nan
    ldk     $r0, 0
    return
uint_not_nan:
    # test edges
    cmp     $r1, 0
    jmpc    ns, uint_not_zero   # lower limit
    ldk     $r0, 0
    return
uint_not_zero:
    lpm     $r3, smallest_norm
    or      $r0, $r0, $r3       # set implicit bit
    cmp     $r1, 23
    jmpc    lt, shift_uint_right
    sub     $r1, $r1, 23
    ashl    $r0, $r0, $r1
    return
shift_uint_right:
    ldk     $r3, 23
    sub     $r1, $r3, $r1
    lshr    $r0, $r0, $r1
    return
#endif

##########################################################################
##########################################################################
## int & unsigned int to float


.macro  i2f x, s1, s2, s3, lbl
    move    \s1, \x
    nlz     \s1, \s2
    cmp     \s1, 8
    jmpc    s, float_round\lbl
    sub     \s2, \s1, 8
    ashl    \x, \x, \s2
    jmp     float_no_round\lbl
float_round\lbl:
    cmp     \s1, 6
    jmpc    s, float_shift_right\lbl
    sub     \s2, \s1, 6
    ashl    \x, \x, \s2
    jmp     float_round_and_pack\lbl
float_shift_right\lbl:
    ldk     \s2, 6
    sub     \s2, \s2, \s1
    xnor    \s3, \s3 ,\s3           # 0xFFFFFFFF
    ashl    \s3, \s3 ,\s2           # create inverse of mask for test of S bit value in discarded my
    xnor    \s3, \s3 ,0             # NOT
    tst     \x, \s3                # determine value of sticky bit
    lshr    \x, \x, \s2
    jmpc    z,float_round_and_pack\lbl
    or      \x, \x, 1               # set the sticky bit to 1
float_round_and_pack\lbl:
    bextu   \s2, \x, (1<<5)|2      # extract low bit of m
    or      \x, \x, \s2           # or p into r
    add     \x, \x, 1
    lshr    \x, \x, 2
    btst    \x, (1<<5)|24          # test for carry from round
    jmpc    z, float_no_round\lbl
    sub     \s1, \s1, 1             # inc e for carry (actually dec nlz)
    lshr    \x, \x, 1
float_no_round\lbl:
    ldk     \s2, 158
    sub     \s1, \s2, \s1
    # Pack e
    ldl     \s1, \s1, (8<<5)|23
    bins    \x, \x, \s1
.endm


#ifdef L_floatsisf
.global __floatsisf
__floatsisf:                       # 32 instructions
    cmp     $r0, 0
    jmpc    nz, float_not_zero
    return
float_not_zero:
    ashr    $r1, $r0, 31            # s in r1
    xor     $r0, $r0, $r1           # cond neg
    sub     $r0, $r0, $r1
    i2f     $r0, $r2, $r3, $r4, 1
    ldl     $r1, $r1, (1<<5)|31
    bins    $r0, $r0, $r1
    return
#endif

#ifdef L_floatunsisf
.global __floatunsisf
__floatunsisf:                        # 26 instructions
    cmp     $r0, 0
    jmpc    nz, float_not_zero2
    return
float_not_zero2:
    i2f     $r0, $r1, $r2, $r3, 2
    return
#endif

#if 0
##########################################################################
##########################################################################
## float compare


__cmpsf2_:
    # calc abs vals
    lpm     $r3, nan                # also abs mask
    and     $r2, $r0, $r3
    and     $r3, $r1, $r3
    # test if either abs is nan
    lpm     $r4, inf
    cmp     $r2, $r4
    jmpc    gt, cmp_is_gt
    cmp     $r3, $r4
    jmpc    gt, cmp_is_gt
    # test if both are 0
    or      $r2, $r2, $r3
    cmp     $r2, 0
    jmpc    z, cmp_is_eq
    # test if eq
    cmp     $r0, $r1
    jmpc    z, cmp_is_eq
    # -- if either is pos
    and     $r2, $r0, $r1
    cmp     $r2, 0
    jmpc    s, cmp_both_neg
    cmp     $r0, $r1
    jmpc    gt, cmp_is_gt
    # r0 < r1
    lpm     $r0, high_uint
    return
cmp_both_neg:
    cmp     $r0, $r1
    jmpc    lt, cmp_is_gt
    # r0 < r1
    lpm     $r0, high_uint
    return
cmp_is_gt:
    ldk     $r0, 1
    return
cmp_is_eq:
    ldk     $r0, 0
    return
#endif

#ifdef  L_udivsi3
.global __udivsi3
__udivsi3:
	# $r0 is dividend
	# $r1 is divisor
	ldk	$r2,0
	push	$r28
	ldk	$r28,-32
0:
	lshr	$r3,$r0,31	# Shift $r2:$r0 left one
	ashl	$r0,$r0,1
	ashl	$r2,$r2,1
	or	$r2,$r2,$r3
	cmp	$r2,$r1
	jmpc	b,1f
2:
	sub	$r2,$r2,$r1
	add	$r0,$r0,1
1:
	add	$r28,$r28,1
	jmpx	31,$r28,1,0b
	pop	$r28
	# $r0: quotient
	# $r2: remainder
	return
#endif

#ifdef	L_umodsi3
.global	__umodsi3
__umodsi3:
	call	__udivsi3
	move	$r0,$r2
	return
#endif

#ifdef	L_divsi3
.global	__divsi3
__divsi3:
	xor	$r5,$r0,$r1	# $r5 is sign of result
	ashr	$r2,$r0,31	# $r0 = abs($r0)
	xor	$r0,$r0,$r2
	sub	$r0,$r0,$r2
	ashr	$r2,$r1,31	# $r1 = abs($r1)
	xor	$r1,$r1,$r2
	sub	$r1,$r1,$r2
	call	__udivsi3
	ashr	$r5,$r5,31
	xor	$r0,$r0,$r5
	sub	$r0,$r0,$r5
	return
	
#endif

#ifdef	L_modsi3
.global	__modsi3
__modsi3:
	move	$r5,$r0		# $r5 is sign of result
	ashr	$r2,$r0,31	# $r0 = abs($r0)
	xor	$r0,$r0,$r2
	sub	$r0,$r0,$r2
	ashr	$r2,$r1,31	# $r1 = abs($r1)
	xor	$r1,$r1,$r2
	sub	$r1,$r1,$r2
	call	__umodsi3
	ashr	$r5,$r5,31
	xor	$r0,$r0,$r5
	sub	$r0,$r0,$r5
	return
#endif
