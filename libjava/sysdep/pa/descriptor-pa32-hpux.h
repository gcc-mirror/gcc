/* descriptor-pa32-hpux.h - Given a function pointer, extract and return the
   actual code address of the corresponding function.

   This is done by checking if the plabel bit is set. If it's not set,
   return the function pointer.  If it's set, mask it off and extract
   the address from the function descriptor.  This address may point
   to an export stub.  If so, extract the branch target from the stub
   and return it.  Otherwise, the address from the function descriptor
   is returned.

   Copyright (C) 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#define UNWRAP_FUNCTION_DESCRIPTOR pa_unwrap_function_descriptor

#ifdef __cplusplus
extern "C" {
#endif

/* Extract bit field from word using HP's numbering (MSB = 0).  */
#define GET_FIELD(X, FROM, TO)					\
  ((X) >> (31 - (TO)) & ((1 << ((TO) - (FROM) + 1)) - 1))

static inline int
sign_extend (int x, int len)
{
  int signbit = (1 << (len - 1));
  int mask = (signbit << 1) - 1;
  return ((x & mask) ^ signbit) - signbit;
}

/* Extract a 17-bit signed constant from branch instructions.  */
static inline int
extract_17 (unsigned word)
{
  return sign_extend (GET_FIELD (word, 19, 28)
		      | GET_FIELD (word, 29, 29) << 10
		      | GET_FIELD (word, 11, 15) << 11
		      | (word & 0x1) << 16, 17);
}

/* Extract a 22-bit signed constant from branch instructions.  */
static inline int
extract_22 (unsigned word)
{
  return sign_extend (GET_FIELD (word, 19, 28)
		      | GET_FIELD (word, 29, 29) << 10
		      | GET_FIELD (word, 11, 15) << 11
		      | GET_FIELD (word, 6, 10) << 16
		      | (word & 0x1) << 21, 22);
}

static void *
pa_unwrap_function_descriptor (void *addr)
{
  unsigned int *tmp_addr;

  /* Check if plabel bit is set in function pointer.  */
  if (!((unsigned int) addr & 2))
    return addr;

  tmp_addr = *(unsigned int **) ((unsigned int) addr & ~3);

  /* If TMP_ADDR points to an export stub, adjust it so that it points
     to the branch target of the stub.  */
  if ((*tmp_addr & 0xffe0e002) == 0xe8400000		/* bl x,r2 */
      && *(tmp_addr + 1) == 0x08000240			/* nop */
      && *(tmp_addr + 2) == 0x4bc23fd1			/* ldw -18(sp),rp */
      && *(tmp_addr + 3) == 0x004010a1			/* ldsid (rp),r1 */
      && *(tmp_addr + 4) == 0x00011820			/* mtsp r1,sr0 */
      && *(tmp_addr + 5) == 0xe0400002)			/* be,n 0(sr0,rp) */
    /* Extract target address from PA 1.x 17-bit branch.  */
    tmp_addr += extract_17 (*tmp_addr) + 2;
  else if ((*tmp_addr & 0xfc00e002) == 0xe800a000	/* b,l x,r2 */
	   && *(tmp_addr + 1) == 0x08000240		/* nop */
	   && *(tmp_addr + 2) == 0x4bc23fd1		/* ldw -18(sp),rp */
	   && *(tmp_addr + 3) == 0xe840d002)		/* bve,n (rp) */
    /* Extract target address from PA 2.0 22-bit branch.  */
    tmp_addr += extract_22 (*tmp_addr) + 2;

  return (void *) tmp_addr;
}

#ifdef __cplusplus
}
#endif
