/* SSA operand allocation and finalizing.
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


/* This file contains common code which is used by each of the 5 operand 
   types.  Macros are defined to specify the varying components.

   FINALIZE_FUNC - name of finalize function.
   FINALIZE_ALLOC - name of allocation routine.
   FINALIZE_FREE - name of free list.
   FINALIZE_TYPE - type of node.
   FINALIZE_OPS - Lead element in list.
   FINALIZE_USE_PTR - How to get the use_operand_p, if this is a use operand.
   FINALIZE_INITIALIZE - How to initialize an element.
   FINALIZE_ELEM - How to retrieve an element.
   FINALIZE_BASE - How to retrieve the base variable of an element.
   FINALIZE_BASE_TYPE - Type of the base variable.
   FINALIZE_OPBUILD - Opbuild array for these nodes.
   FINALIZE_OPBUILD_ELEM - How to get an element from the opbuild list.
   FINALIZE_OPBUILD_BASE - How to get an element base from the opbuild list.
   FINALIZE_BASE_ZERO - How to zero an element.  */


/* This routine will either pick up a node from the free list, or allocate a
   new one if need be.  */

static inline FINALIZE_TYPE *
FINALIZE_ALLOC (void)
{
  FINALIZE_TYPE *ret;
  if (FINALIZE_FREE)
    {
      ret = FINALIZE_FREE;
      FINALIZE_FREE = FINALIZE_FREE->next;
    }
  else
    ret = (FINALIZE_TYPE *)ssa_operand_alloc (sizeof (FINALIZE_TYPE));
  return ret;
}



/* This routine will take the new operands from FINALIZE_OPBUILD and turn them
   into the new operands for STMT.  All required linking and deleting is u
   performed here.  */
static inline void
FINALIZE_FUNC (tree stmt)
{
  int new_i;
  FINALIZE_TYPE *old_ops, *ptr, *last;
  FINALIZE_BASE_TYPE old_base;
  FINALIZE_TYPE new_list;

  new_list.next = NULL;
  last = &new_list;

  old_ops = FINALIZE_OPS (stmt);
  if (old_ops)
    old_base = FINALIZE_BASE (FINALIZE_ELEM (old_ops));
  else
    old_base = FINALIZE_BASE_ZERO;

  new_i = opbuild_first (&FINALIZE_OPBUILD);
  while (old_ops && new_i != OPBUILD_LAST)
    {
      FINALIZE_BASE_TYPE new_base = FINALIZE_OPBUILD_BASE (new_i);
      if (old_base == new_base)
        {
	  /* if variables are the same, reuse this node.  */
	  last->next = old_ops;
	  last = old_ops;
#ifdef FINALIZE_USE_PTR
	  correct_use_link (FINALIZE_USE_PTR (last), stmt);
#endif
	  old_ops = old_ops->next;
	  new_i = opbuild_next (&FINALIZE_OPBUILD, new_i);
	}
      else
        if (old_base < new_base)
	  {
	    /* if old is less than new, old goes to the free list.  */
#ifdef FINALIZE_USE_PTR
	    use_operand_p use_p = FINALIZE_USE_PTR (old_ops);
	    delink_imm_use (use_p);
#endif
	    ptr = old_ops;
	    old_ops = old_ops->next;
	    ptr->next = FINALIZE_FREE;
	    FINALIZE_FREE = ptr;
	  }
	else
	  {
	    /* This is a new operand.  */
	    ptr = FINALIZE_ALLOC ();
	    FINALIZE_INITIALIZE (ptr, FINALIZE_OPBUILD_ELEM (new_i), stmt);
	    last->next = ptr;
	    last = ptr;
	    new_i = opbuild_next (&FINALIZE_OPBUILD, new_i);
	  }
      if (old_ops)
        old_base = FINALIZE_BASE (FINALIZE_ELEM (old_ops));
    }

  /* If there is anything remaining in the opbuild list, simply emit them.  */
  for ( ; 
	new_i != OPBUILD_LAST; 
	new_i = opbuild_next (&FINALIZE_OPBUILD, new_i))
    {
      ptr = FINALIZE_ALLOC ();
      FINALIZE_INITIALIZE (ptr, FINALIZE_OPBUILD_ELEM (new_i), stmt);
      last->next = ptr;
      last = ptr;
    }

  last->next = NULL;

  /* If there is anything in the old list, free them.  */
  if (old_ops)
    {
#ifdef FINALIZE_USE_PTR
      for (ptr = old_ops; ptr; ptr = ptr->next)
	{
	  use_operand_p use_p = FINALIZE_USE_PTR (ptr);
	  delink_imm_use (use_p);
	}
#endif
      old_ops->next = FINALIZE_FREE;
      FINALIZE_FREE = old_ops;
    }

  /* NOw set the stmt's operands.  */
  FINALIZE_OPS (stmt) = new_list.next;

#ifdef ENABLE_CHECKING
  {
    unsigned x = 0;
    for (ptr = FINALIZE_OPS (stmt); ptr; ptr = ptr->next)
      x++;

    gcc_assert (x == opbuild_num_elems (&FINALIZE_OPBUILD));
  }
#endif
}

#undef FINALIZE_FUNC
#undef FINALIZE_ALLOC
#undef FINALIZE_FREE
#undef FINALIZE_TYPE
#undef FINALIZE_OPS
#undef FINALIZE_USE_PTR
#undef FINALIZE_INITIALIZE
#undef FINALIZE_ELEM
#undef FINALIZE_BASE
#undef FINALIZE_BASE_TYPE
#undef FINALIZE_OPBUILD
#undef FINALIZE_OPBUILD_ELEM
#undef FINALIZE_OPBUILD_BASE
#undef FINALIZE_BASE_ZERO
