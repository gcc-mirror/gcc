/* Header file for unwinding stack frames for exception handling.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@cygnus.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Number of hardware registers known to the compiler.  
   We have 128 general registers, 128 floating point registers, 64 predicate
   registers, 8 branch registers, and one frame pointer register.  */

/* ??? Should add ar.lc, ar.ec and probably also ar.pfs.  */

#define FIRST_PSEUDO_REGISTER 330

#ifndef DWARF_FRAME_REGISTERS
#define DWARF_FRAME_REGISTERS FIRST_PSEUDO_REGISTER
#endif

typedef struct frame_state
{
  void *cfa;
  void *eh_ptr;
  long cfa_offset;
  long args_size;
  long reg_or_offset[DWARF_FRAME_REGISTERS+1];
  unsigned short cfa_reg;
  unsigned short retaddr_column;
  char saved[DWARF_FRAME_REGISTERS+1];
} frame_state;

/* Values for 'saved' above.  */
#define REG_UNSAVED 0
#define REG_SAVED_OFFSET 1
#define REG_SAVED_REG 2

/* The representation for an "object" to be searched for frame unwind info.
   For targets with named sections, one object is an executable or shared
   library; for other targets, one object is one translation unit.

   A copy of this structure declaration is printed by collect2.c;
   keep the copies synchronized!  */

struct object {
#ifdef IA64_UNWIND_INFO
  void *pc_base;        /* This field will be set by __do_frame_setup. */
#endif
  void *pc_begin;
  void *pc_end;
  struct dwarf_fde *fde_begin;
  struct dwarf_fde **fde_array;
  size_t count;
  struct object *next;
};

/* Called from __throw to find the registers to restore for a given
   PC_TARGET.  The caller should allocate a local variable of `struct
   frame_state' (declared in frame.h) and pass its address to STATE_IN.
   Returns NULL on failure, otherwise returns STATE_IN.  */

extern struct frame_state *__frame_state_for (void *, struct frame_state *);

#ifdef IA64_UNWIND_INFO

/* This is the information required for unwind records in an ia64
   object file. This is required by GAS and the compiler runtime. */

/* These are the starting point masks for the various types of
   unwind records. To create a record of type R3 for instance, one
   starts by using the value UNW_R3 and or-ing in any other required values. 
   These values are also unique (in context), so they can be used to identify 
   the various record types as well. UNW_Bx and some UNW_Px do have the
   same value, but Px can only occur in a prologue context, and Bx in
   a body context.  */

#define UNW_R1	0x00
#define UNW_R2	0x40
#define UNW_R3	0x60
#define UNW_P1	0x80
#define UNW_P2	0xA0
#define UNW_P3	0xB0
#define UNW_P4	0xB8
#define UNW_P5	0xB9
#define UNW_P6	0xC0
#define UNW_P7	0xE0
#define UNW_P8	0xF0
#define UNW_P9	0xF1
#define UNW_P10	0xFF
#define UNW_X1	0xF9
#define UNW_X2	0xFA
#define UNW_X3	0xFB
#define UNW_X4	0xFC
#define UNW_B1	0x80
#define UNW_B2	0xC0
#define UNW_B3	0xE0
#define UNW_B4	0xF0

/* These are all the various types of unwind records.  */

typedef enum
{
  prologue, prologue_gr, body, mem_stack_f, mem_stack_v, psp_gr, psp_sprel,
  rp_when, rp_gr, rp_br, rp_psprel, rp_sprel, pfs_when, pfs_gr, pfs_psprel,
  pfs_sprel, preds_when, preds_gr, preds_psprel, preds_sprel,
  fr_mem, frgr_mem, gr_gr, gr_mem, br_mem, br_gr, spill_base, spill_mask,
  unat_when, unat_gr, unat_psprel, unat_sprel, lc_when, lc_gr, lc_psprel,
  lc_sprel, fpsr_when, fpsr_gr, fpsr_psprel, fpsr_sprel, 
  priunat_when_gr, priunat_when_mem, priunat_gr, priunat_psprel, 
  priunat_sprel, bsp_when, bsp_gr, bsp_psprel, bsp_sprel, bspstore_when,
  bspstore_gr, bspstore_psprel, bspstore_sprel, rnat_when, rnat_gr,
  rnat_psprel, rnat_sprel, epilogue, label_state, copy_state,
  spill_psprel, spill_sprel, spill_reg, spill_psprel_p, spill_sprel_p,
  spill_reg_p
} unw_record_type;


/* These structures declare the fields that can be used in each of the 
   4 record formats, R, P, B and X.  */

typedef struct unw_r_record
{
  unsigned long rlen;
  unsigned short mask;
  unsigned short grsave;
} unw_r_record;

typedef struct unw_p_record
{
  void *imask;
  unsigned long t;
  unsigned long size;
  unsigned long spoff;
  unsigned long br;
  unsigned long pspoff;
  unsigned short gr;
  unsigned short rmask;
  unsigned short grmask;
  unsigned long frmask;
  unsigned short brmask;
} unw_p_record;

typedef struct unw_b_record
{
  unsigned long t;
  unsigned long label;
  unsigned short ecount;
} unw_b_record;

typedef struct unw_x_record
{
  unsigned long t;
  unsigned long spoff;
  unsigned long pspoff;
  unsigned short reg;
  unsigned short treg;
  unsigned short qp;
  unsigned short xy;   /* Value of the XY field..  */
} unw_x_record;

/* This structure is used to determine the specific record type and 
   its fields.  */
typedef struct unwind_record
{
  unw_record_type type;
  union {
    unw_r_record r;
    unw_p_record p;
    unw_b_record b;
    unw_x_record x;
  } record;
} unwind_record;

/* This structure represents the start of an unwind information pointer.  
   'unwind_descriptors' is the beginninng of the unwind descriptors, which
   use up 'length' bytes of storage.  */

typedef struct unwind_info_ptr 
{
  unsigned short version;
  unsigned short flags;
  unsigned int length;
  unsigned char unwind_descriptors[1];
} unwind_info_ptr;


#define IA64_UNW_LOC_TYPE_NONE		0
#define IA64_UNW_LOC_TYPE_MEM		1
#define IA64_UNW_LOC_TYPE_GR		2
#define IA64_UNW_LOC_TYPE_FR		3
#define IA64_UNW_LOC_TYPE_BR		4
#define IA64_UNW_LOC_TYPE_SPOFF		5
#define IA64_UNW_LOC_TYPE_PSPOFF	6
#define IA64_UNW_LOC_TYPE_OFFSET	7
#define IA64_UNW_LOC_TYPE_SPILLBASE	8

typedef struct ia64_reg_loc 
{
  long when;		/* PC relative offset from start of function. */
  union {		/* In memory or another register?  */
    void *mem;
    int regno;
    int offset;
  } l;
  short loc_type;	/* Where to find value.  */
  short reg_size;
} ia64_reg_loc;

/* Frame information record.  */

typedef struct ia64_frame_state
{
  ia64_reg_loc gr[4];	/* gr4 to  gr7.  */
  ia64_reg_loc fr[20];	/* fr2 to fr5, fr16 to fr31.  */
  ia64_reg_loc br[5];	/* br1 to  br5.  */
  ia64_reg_loc rp;
  ia64_reg_loc fpsr;
  ia64_reg_loc bsp;
  ia64_reg_loc bspstore;
  ia64_reg_loc rnat;
  ia64_reg_loc pfs;
  ia64_reg_loc unat;
  ia64_reg_loc lc;
  ia64_reg_loc pr;
  ia64_reg_loc priunat;
  ia64_reg_loc sp;
  ia64_reg_loc psp;
  ia64_reg_loc spill_base;
  void *my_sp;
  void *my_bsp;
} ia64_frame_state;


extern unwind_info_ptr *build_ia64_frame_state (unsigned char *, ia64_frame_state *, 
						void *, void *);
extern void *get_real_reg_value (ia64_reg_loc *);
extern void *get_personality (unwind_info_ptr *);
extern void *get_except_table (unwind_info_ptr *);
extern void set_real_reg_value (ia64_reg_loc *, void *);
void *calc_caller_bsp (long, unsigned char *);

#endif   /* IA64_UNWIND_INFO  */

/* Note the following routines are exported interfaces from libgcc; do not
   change these interfaces.  Instead create new interfaces.  Also note
   references to these functions may be made weak in files where they
   are referenced.  */

extern void __register_frame (void * );
extern void __register_frame_table (void *);
extern void __deregister_frame (void *);

/* Called either from crtbegin.o or a static constructor to register the
   unwind info for an object or translation unit, respectively.  */

extern void __register_frame_info (void *, struct object *);

/* Similar, but BEGIN is actually a pointer to a table of unwind entries
   for different translation units.  Called from the file generated by
   collect2.  */
extern void __register_frame_info_table (void *, struct object *);

/* Called from crtend.o to deregister the unwind info for an object.  */

extern void *__deregister_frame_info (void *);


