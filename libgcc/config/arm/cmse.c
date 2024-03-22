/* ARMv8-M Security Extensions routines.
   Copyright (C) 2015-2024 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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


#if __ARM_FEATURE_CMSE & 1

#include <arm_cmse.h>

/* ARM intrinsic function to perform a permission check on a given
   address range.  See ACLE changes for ARMv8-M.  */

void *
__attribute__ ((warn_unused_result))
cmse_check_address_range (void *p, size_t size, int flags)
{
  cmse_address_info_t permb, perme;
  char *pb = (char *) p, *pe;

  /* Check if the range wraps around.  */
  if (__UINTPTR_MAX__ - (__UINTPTR_TYPE__) p < size)
    return NULL;

  /* Check if an unknown flag is present.  */
  int known = CMSE_MPU_UNPRIV | CMSE_MPU_READWRITE | CMSE_MPU_READ;
  int known_secure_level = CMSE_MPU_UNPRIV;
#if __ARM_FEATURE_CMSE & 2
  known |= CMSE_AU_NONSECURE | CMSE_MPU_NONSECURE;
  known_secure_level |= CMSE_MPU_NONSECURE;
#endif
  if (flags & (~known))
    return NULL;

  /* Execute the right variant of the TT instructions.  */
  pe = pb + size - 1;
  const int singleCheck
    = (((__UINTPTR_TYPE__) pb ^ (__UINTPTR_TYPE__) pe) < 32);
  switch (flags & known_secure_level)
    {
    case 0:
      permb = cmse_TT (pb);
      perme = singleCheck ? permb : cmse_TT (pe);
      break;
    case CMSE_MPU_UNPRIV:
      permb = cmse_TTT (pb);
      perme = singleCheck ? permb : cmse_TTT (pe);
      break;
#if __ARM_FEATURE_CMSE & 2
    case CMSE_MPU_NONSECURE:
      permb = cmse_TTA (pb);
      perme = singleCheck ? permb : cmse_TTA (pe);
      break;
    case CMSE_MPU_UNPRIV | CMSE_MPU_NONSECURE:
      permb = cmse_TTAT (pb);
      perme = singleCheck ? permb : cmse_TTAT (pe);
      break;
#endif
    default:
      /* Invalid flag, eg.  CMSE_MPU_NONSECURE specified but
	 __ARM_FEATURE_CMSE & 2 == 0.  */
      return NULL;
    }

  /* Check that the range does not cross MPU, SAU, or IDAU boundaries.  */
  if (permb.value != perme.value)
    return NULL;

  /* Check the permissions on the range.  */
  switch (flags & (~known_secure_level))
    {
#if __ARM_FEATURE_CMSE & 2
    case CMSE_MPU_READ | CMSE_MPU_READWRITE | CMSE_AU_NONSECURE:
    case		 CMSE_MPU_READWRITE | CMSE_AU_NONSECURE:
      return permb.flags.nonsecure_readwrite_ok	? p : NULL;
    case CMSE_MPU_READ | CMSE_AU_NONSECURE:
      return permb.flags.nonsecure_read_ok	? p : NULL;
    case CMSE_AU_NONSECURE:
      return permb.flags.secure			? NULL : p;
#endif
    case CMSE_MPU_READ | CMSE_MPU_READWRITE:
    case		 CMSE_MPU_READWRITE:
      return permb.flags.readwrite_ok		? p : NULL;
    case CMSE_MPU_READ:
      return permb.flags.read_ok		? p : NULL;
    default:
      return NULL;
    }
}


#endif /* __ARM_FEATURE_CMSE & 1.  */
