/* This file contains definitions for the data structures and codes used in
   VMS debugging information.  */

#ifndef VMSDBG_H
#define VMSDBG_H 1
	
typedef unsigned short int DST_DTYPE;
#define DST_K_SOURCE 155
#define DST_K_PROLOG 162
#define DST_K_BLKBEG 176
#define DST_K_BLKEND 177
#define DST_K_LINE_NUM 185
#define DST_K_MODBEG 188
#define DST_K_MODEND 189
#define DST_K_RTNBEG 190
#define DST_K_RTNEND 191

typedef struct _DST_HEADER {
  union  {
    unsigned short int dst_w_length;
    unsigned short int dst_x_length;
  } dst__header_length;
  union  {
    DST_DTYPE dst_w_type;
    DST_DTYPE dst_x_type;
  } dst__header_type;
} DST_HEADER;
#define DST_K_DST_HEADER_SIZE 4
	
typedef unsigned int DST_LANGUAGE;
#define DST_K_FORTRAN 1
#define DST_K_C 7
#define DST_K_ADA 9
#define DST_K_UNKNOWN 10
#define DST_K_CXX 15
typedef struct _DST_MODULE_BEGIN {
  DST_HEADER dst_a_modbeg_header;
  struct  {
    unsigned dst_v_modbeg_hide : 1;
    unsigned dst_v_modbeg_version : 1;
    unsigned dst_v_modbeg_unused : 6;
  } dst_b_modbeg_flags;
  unsigned char dst_b_modbeg_unused;
  DST_LANGUAGE dst_l_modbeg_language;
  unsigned short int dst_w_version_major;
  unsigned short int dst_w_version_minor;
  unsigned char dst_b_modbeg_name;
} DST_MODULE_BEGIN;
#define DST_K_MODBEG_SIZE 15
	
typedef struct _DST_MB_TRLR {
  unsigned char dst_b_compiler;
} DST_MB_TRLR;
#define DST_K_MB_TRLR_SIZE 1
#define DST_K_VERSION_MAJOR 1
#define DST_K_VERSION_MINOR 13
typedef struct _DST_MODULE_END {
  DST_HEADER dst_a_modend_header;
} DST_MODULE_END;
#define DST_K_MODEND_SIZE 4
typedef struct _DST_ROUTINE_BEGIN {
  DST_HEADER dst_a_rtnbeg_header;
  struct  {
    unsigned dst_v_rtnbeg_unused : 4;
    unsigned dst_v_rtnbeg_unalloc : 1;
    unsigned dst_v_rtnbeg_prototype : 1;
    unsigned dst_v_rtnbeg_inlined : 1;
    unsigned dst_v_rtnbeg_no_call : 1;
  } dst_b_rtnbeg_flags;
  int *dst_l_rtnbeg_address;
  int *dst_l_rtnbeg_pd_address;
  unsigned char dst_b_rtnbeg_name;
} DST_ROUTINE_BEGIN;
#define DST_K_RTNBEG_SIZE 14
typedef struct _DST_ROUTINE_END {
  DST_HEADER dst_a_rtnend_header;
  char dst_b_rtnend_unused;
  unsigned int dst_l_rtnend_size;
} DST_ROUTINE_END;
#define DST_K_RTNEND_SIZE 9
typedef struct _DST_BLOCK_BEGIN {
  DST_HEADER dst_a_blkbeg_header;
  unsigned char dst_b_blkbeg_unused;
  int *dst_l_blkbeg_address;
  unsigned char dst_b_blkbeg_name;
} DST_BLOCK_BEGIN;
#define DST_K_BLKBEG_SIZE 10
typedef struct _DST_BLOCK_END {
  DST_HEADER dst_a_blkend_header;
  unsigned char dst_b_blkend_unused;
  unsigned int dst_l_blkend_size;
} DST_BLOCK_END;
#define DST_K_BLKEND_SIZE 9
typedef struct _DST_LINE_NUM_HEADER {
  DST_HEADER dst_a_line_num_header;
} DST_LINE_NUM_HEADER;
#define DST_K_LINE_NUM_HEADER_SIZE 4

typedef struct _DST_PCLINE_COMMANDS {
  char dst_b_pcline_command;
  union  {
    unsigned int dst_l_pcline_unslong;
    unsigned short int dst_w_pcline_unsword;
    unsigned char dst_b_pcline_unsbyte;
  } dst_a_pcline_access_fields;
} DST_PCLINE_COMMANDS;
#define DST_K_PCLINE_COMMANDS_SIZE 5
#define DST_K_PCLINE_COMMANDS_SIZE_MIN 2
#define DST_K_PCLINE_COMMANDS_SIZE_MAX 5
#define DST_K_DELTA_PC_LOW -128
#define DST_K_DELTA_PC_HIGH 0
#define DST_K_DELTA_PC_W 1
#define DST_K_INCR_LINUM 2
#define DST_K_INCR_LINUM_W 3
#define DST_K_SET_LINUM 9
#define DST_K_SET_ABS_PC 16
#define DST_K_DELTA_PC_L 17
#define DST_K_INCR_LINUM_L 18
#define DST_K_SET_LINUM_B 19
#define DST_K_SET_LINUM_L 20
typedef struct _DST_SOURCE_CORR {
  DST_HEADER dst_a_source_corr_header;
} DST_SOURCE_CORR;
#define DST_K_SOURCE_CORR_HEADER_SIZE 4
	
#define DST_K_SRC_DECLFILE 1
#define DST_K_SRC_SETFILE 2
#define DST_K_SRC_SETREC_L 3
#define DST_K_SRC_SETREC_W 4
#define DST_K_SRC_SETLNUM_L 5
#define DST_K_SRC_SETLNUM_W 6
#define DST_K_SRC_INCRLNUM_B 7
#define DST_K_SRC_DEFLINES_W 10
#define DST_K_SRC_DEFLINES_B 11
#define DST_K_SRC_FORMFEED 16
#define DST_K_SRC_MIN_CMD 1
#define DST_K_SRC_MAX_CMD 16
typedef struct _DST_SRC_COMMAND {
  unsigned char dst_b_src_command;
  union  {
    struct  {
      unsigned char dst_b_src_df_length;
      unsigned char dst_b_src_df_flags;
      unsigned short int dst_w_src_df_fileid;
#ifdef __GNUC__
      long long dst_q_src_df_rms_cdt;
#else
      __int64 dst_q_src_df_rms_cdt;
#endif
      unsigned int dst_l_src_df_rms_ebk;
      unsigned short int dst_w_src_df_rms_ffb;
      unsigned char dst_b_src_df_rms_rfo;
      unsigned char dst_b_src_df_filename;
    } dst_a_src_decl_src;
    unsigned int dst_l_src_unslong;
    unsigned short int dst_w_src_unsword;
    unsigned char dst_b_src_unsbyte;
  } dst_a_src_cmd_fields;
} DST_SRC_COMMAND;
#define DST_K_SRC_COMMAND_SIZE 21
	
typedef struct _DST_SRC_CMDTRLR {
  unsigned char dst_b_src_df_libmodname;
} DST_SRC_CMDTRLR;
#define DST_K_SRC_CMDTRLR_SIZE 1
	
typedef struct _DST_PROLOG {
  DST_HEADER dst_a_prolog_header;
  unsigned int dst_l_prolog_bkpt_addr;
} DST_PROLOG;
#define DST_K_PROLOG_SIZE 8
	
#endif /* VMSDBG_H */
