.LFB1:
.LCFI0:
.LCFI1:
.LCFI2:
.LCFI3:
.LCFI4:
.LCFI5:
.LCFI6:
.LFE1:

      .section  ".eh_frame","aw"
__FRAME_BEGIN__:
      .4byte    .LECIE1-.LSCIE1  /*  Length of Common Information Entry */
.LSCIE1:
      .4byte    0x0      /*  CIE Identifier Tag */
      .byte     0x1      /*  CIE Version */
      .ascii "\0"        /*  CIE Augmentation */
      .byte     0x1      /*  uleb128 0x1; CIE Code Alignment Factor */
      .byte     0x7c     /*  sleb128 -4; CIE Data Alignment Factor */
      .byte     0x41     /*  CIE RA Column */
      .byte     0xc      /*  DW_CFA_def_cfa */
      .byte     0x1      /*  uleb128 0x1 */
      .byte     0x0      /*  uleb128 0x0 */
      .align 2
.LECIE1:
.LSFDE1:
      .4byte    .LEFDE1-.LASFDE1         /*  FDE Length */
.LASFDE1:
      .4byte    .LASFDE1-__FRAME_BEGIN__         /*  FDE CIE offset */
      .4byte    .LFB1    /*  FDE initial location */
      .4byte    .LFE1-.LFB1      /*  FDE address range */
      .byte     0x4      /*  DW_CFA_advance_loc4 */
      .4byte    .LCFI0-.LFB1
      .byte     0xd      /*  DW_CFA_def_cfa_register */
      .byte     0x08     /*  uleb128 0x08 */
      .byte     0x4      /*  DW_CFA_advance_loc4 */
      .4byte    .LCFI5-.LCFI0
      .byte     0x2f     /*  DW_CFA_GNU_negative_offset_extended */
      .byte     0x41     /*  uleb128 0x41 */
      .byte     0x1      /*  uleb128 0x1 */
      .byte     0x9f     /*  DW_CFA_offset, column 0x1f */
      .byte     0x1      /*  uleb128 0x1 */
      .byte     0x9e     /*  DW_CFA_offset, column 0x1e */
      .byte     0x2      /*  uleb128 0x2 */
      .byte     0x9d     /*  DW_CFA_offset, column 0x1d */
      .byte     0x3      /*  uleb128 0x3 */
      .byte     0x9c     /*  DW_CFA_offset, column 0x1c */
      .byte     0x4      /*  uleb128 0x4 */
      .byte     0x4      /*  DW_CFA_advance_loc4 */
      .4byte    .LCFI6-.LCFI5
      .byte     0xd      /*  DW_CFA_def_cfa_register */
      .byte     0x1c     /*  uleb128 0x1c */
      .align 2
.LEFDE1:
