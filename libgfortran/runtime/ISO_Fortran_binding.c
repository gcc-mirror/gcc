/* Functions to convert descriptors between CFI and gfortran
   and the CFI function declarations whose prototypes appear
   in ISO_Fortran_binding.h.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by Daniel Celis Garza  <celisdanieljr@gmail.com>
	       and Paul Thomas  <pault@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"
#include "ISO_Fortran_binding.h"
#include <string.h>
#include <inttypes.h>   /* for PRIiPTR */

extern void cfi_desc_to_gfc_desc (gfc_array_void *, CFI_cdesc_t **);
export_proto(cfi_desc_to_gfc_desc);

/* NOTE: Since GCC 12, the FE generates code to do the conversion
   directly without calling this function.  */
void
cfi_desc_to_gfc_desc (gfc_array_void *d, CFI_cdesc_t **s_ptr)
{
  int n;
  index_type kind;
  CFI_cdesc_t *s = *s_ptr;

  if (!s)
    return;

  GFC_DESCRIPTOR_DATA (d) = s->base_addr;
  GFC_DESCRIPTOR_TYPE (d) = (signed char)(s->type & CFI_type_mask);
  kind = (index_type)((s->type - (s->type & CFI_type_mask)) >> CFI_type_kind_shift);

  /* Correct the unfortunate difference in order with types.  */
  if (GFC_DESCRIPTOR_TYPE (d) == BT_CHARACTER)
    GFC_DESCRIPTOR_TYPE (d) = BT_DERIVED;
  else if (GFC_DESCRIPTOR_TYPE (d) == BT_DERIVED)
    GFC_DESCRIPTOR_TYPE (d) = BT_CHARACTER;

  if (!s->rank || s->dim[0].sm == (CFI_index_t)s->elem_len)
    GFC_DESCRIPTOR_SIZE (d) = s->elem_len;
  else if (GFC_DESCRIPTOR_TYPE (d) != BT_DERIVED)
    GFC_DESCRIPTOR_SIZE (d) = kind;
  else
    GFC_DESCRIPTOR_SIZE (d) = s->elem_len;

  d->dtype.version = 0;
  GFC_DESCRIPTOR_RANK (d) = (signed char)s->rank;

  d->dtype.attribute = (signed short)s->attribute;

  if (s->rank)
    {
      if ((size_t)s->dim[0].sm % s->elem_len)
	d->span = (index_type)s->dim[0].sm;
      else
	d->span = (index_type)s->elem_len;
    }

  d->offset = 0;
  if (GFC_DESCRIPTOR_DATA (d))
    for (n = 0; n < GFC_DESCRIPTOR_RANK (d); n++)
      {
	CFI_index_t lb = 1;

	if (s->attribute != CFI_attribute_other)
	  lb = s->dim[n].lower_bound;

	GFC_DESCRIPTOR_LBOUND(d, n) = (index_type)lb;
	GFC_DESCRIPTOR_UBOUND(d, n) = (index_type)(s->dim[n].extent + lb - 1);
	GFC_DESCRIPTOR_STRIDE(d, n) = (index_type)(s->dim[n].sm / s->elem_len);
	d->offset -= GFC_DESCRIPTOR_STRIDE(d, n) * GFC_DESCRIPTOR_LBOUND(d, n);
      }
}

extern void gfc_desc_to_cfi_desc (CFI_cdesc_t **, const gfc_array_void *);
export_proto(gfc_desc_to_cfi_desc);

/* NOTE: Since GCC 12, the FE generates code to do the conversion
   directly without calling this function.  */
void
gfc_desc_to_cfi_desc (CFI_cdesc_t **d_ptr, const gfc_array_void *s)
{
  int n;
  CFI_cdesc_t *d;

  /* Play it safe with allocation of the flexible array member 'dim'
     by setting the length to CFI_MAX_RANK. This should not be necessary
     but valgrind complains accesses after the allocated block.  */
  if (*d_ptr == NULL)
    d = calloc (1, (sizeof (CFI_cdesc_t)
		    + (CFI_type_t)(CFI_MAX_RANK * sizeof (CFI_dim_t))));
  else
    d = *d_ptr;

  d->base_addr = GFC_DESCRIPTOR_DATA (s);
  d->elem_len = GFC_DESCRIPTOR_SIZE (s);
  d->version = CFI_VERSION;
  d->rank = (CFI_rank_t)GFC_DESCRIPTOR_RANK (s);
  d->attribute = (CFI_attribute_t)s->dtype.attribute;

  if (GFC_DESCRIPTOR_TYPE (s) == BT_CHARACTER)
    d->type = CFI_type_Character;
  else if (GFC_DESCRIPTOR_TYPE (s) == BT_DERIVED)
    d->type = CFI_type_struct;
  else
    d->type = (CFI_type_t)GFC_DESCRIPTOR_TYPE (s);

  if (GFC_DESCRIPTOR_TYPE (s) != BT_DERIVED)
    d->type = (CFI_type_t)(d->type
		+ ((CFI_type_t)d->elem_len << CFI_type_kind_shift));

  if (d->base_addr)
    /* Full pointer or allocatable arrays retain their lower_bounds.  */
    for (n = 0; n < GFC_DESCRIPTOR_RANK (s); n++)
      {
	if (d->attribute != CFI_attribute_other)
	  d->dim[n].lower_bound = (CFI_index_t)GFC_DESCRIPTOR_LBOUND(s, n);
	else
	  d->dim[n].lower_bound = 0;

	/* Assumed size arrays have gfc ubound == 0 and CFI extent = -1.  */
	if (n == GFC_DESCRIPTOR_RANK (s) - 1
	    && GFC_DESCRIPTOR_LBOUND(s, n) == 1
	    && GFC_DESCRIPTOR_UBOUND(s, n) == 0)
	  d->dim[n].extent = -1;
	else
	  d->dim[n].extent = (CFI_index_t)GFC_DESCRIPTOR_UBOUND(s, n)
			     - (CFI_index_t)GFC_DESCRIPTOR_LBOUND(s, n) + 1;
	d->dim[n].sm = (CFI_index_t)(GFC_DESCRIPTOR_STRIDE(s, n) * s->span);
      }

  if (*d_ptr == NULL)
    *d_ptr = d;
}

void *CFI_address (const CFI_cdesc_t *dv, const CFI_index_t subscripts[])
{
  int i;
  char *base_addr = (char *)dv->base_addr;

  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptor must not be NULL. */
      if (dv == NULL)
	{
	  fprintf (stderr, "CFI_address: C descriptor is NULL.\n");
	  return NULL;
	}

      /* Base address of C descriptor must not be NULL. */
      if (dv->base_addr == NULL)
	{
	  fprintf (stderr, "CFI_address: base address of C descriptor "
		   "must not be NULL.\n");
	  return NULL;
	}
    }

  /* Return base address if C descriptor is a scalar. */
  if (dv->rank == 0)
    return dv->base_addr;

  /* Calculate the appropriate base address if dv is not a scalar. */
  else
    {
      /* Base address is the C address of the element of the object
	 specified by subscripts. */
      for (i = 0; i < dv->rank; i++)
	{
	  CFI_index_t idx = subscripts[i] - dv->dim[i].lower_bound;
	  if (unlikely (compile_options.bounds_check)
	      && ((dv->dim[i].extent != -1 && idx >= dv->dim[i].extent)
		  || idx < 0))
	    {
	      fprintf (stderr, "CFI_address: subscripts[%d] is out of "
		       "bounds. For dimension = %d, subscripts = %d, "
		       "lower_bound = %" PRIiPTR ", upper bound = %" PRIiPTR
		       ", extent = %" PRIiPTR "\n",
		       i, i, (int)subscripts[i],
		       (ptrdiff_t)dv->dim[i].lower_bound,
		       (ptrdiff_t)(dv->dim[i].extent - dv->dim[i].lower_bound),
		       (ptrdiff_t)dv->dim[i].extent);
              return NULL;
            }

	  base_addr = base_addr + (CFI_index_t)(idx * dv->dim[i].sm);
	}
    }

  return (void *)base_addr;
}


int
CFI_allocate (CFI_cdesc_t *dv, const CFI_index_t lower_bounds[],
	      const CFI_index_t upper_bounds[], size_t elem_len)
{
  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptor must not be NULL. */
      if (dv == NULL)
	{
	  fprintf (stderr, "CFI_allocate: C descriptor is NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      /* The C descriptor must be for an allocatable or pointer object. */
      if (dv->attribute == CFI_attribute_other)
	{
	  fprintf (stderr, "CFI_allocate: The object of the C descriptor "
		   "must be a pointer or allocatable variable.\n");
	  return CFI_INVALID_ATTRIBUTE;
	}

      /* Base address of C descriptor must be NULL. */
      if (dv->base_addr != NULL)
	{
	  fprintf (stderr, "CFI_allocate: Base address of C descriptor "
		   "must be NULL.\n");
	  return CFI_ERROR_BASE_ADDR_NOT_NULL;
	}
    }

  /* If the type is a Fortran character type, the descriptor's element
     length is replaced by the elem_len argument. */
  if (dv->type == CFI_type_char || dv->type == CFI_type_ucs4_char)
    dv->elem_len = elem_len;

  /* Dimension information and calculating the array length. */
  size_t arr_len = 1;

  /* If rank is greater than 0, lower_bounds and upper_bounds are used. They're
     ignored otherwise. */
  if (dv->rank > 0)
    {
      if (unlikely (compile_options.bounds_check)
	  && (lower_bounds == NULL || upper_bounds == NULL))
	{
	  fprintf (stderr, "CFI_allocate: The lower_bounds and "
		   "upper_bounds arguments must be non-NULL when "
		   "rank is greater than zero.\n");
	  return CFI_INVALID_EXTENT;
	}

      for (int i = 0; i < dv->rank; i++)
	{
	  dv->dim[i].lower_bound = lower_bounds[i];
	  dv->dim[i].extent = upper_bounds[i] - dv->dim[i].lower_bound + 1;
	  dv->dim[i].sm = dv->elem_len * arr_len;
	  arr_len *= dv->dim[i].extent;
        }
    }

  dv->base_addr = calloc (arr_len, dv->elem_len);
  if (dv->base_addr == NULL)
    {
      fprintf (stderr, "CFI_allocate: Failure in memory allocation.\n");
      return CFI_ERROR_MEM_ALLOCATION;
    }

  return CFI_SUCCESS;
}


int
CFI_deallocate (CFI_cdesc_t *dv)
{
  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptor must not be NULL */
      if (dv == NULL)
	{
	  fprintf (stderr, "CFI_deallocate: C descriptor is NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      /* Base address must not be NULL. */
      if (dv->base_addr == NULL)
	{
	  fprintf (stderr, "CFI_deallocate: Base address is already NULL.\n");
	  return CFI_ERROR_BASE_ADDR_NULL;
	}

      /* C descriptor must be for an allocatable or pointer variable. */
      if (dv->attribute == CFI_attribute_other)
	{
	  fprintf (stderr, "CFI_deallocate: C descriptor must describe a "
		  "pointer or allocatable object.\n");
	  return CFI_INVALID_ATTRIBUTE;
	}
    }

  /* Free and nullify memory. */
  free (dv->base_addr);
  dv->base_addr = NULL;

  return CFI_SUCCESS;
}


int CFI_establish (CFI_cdesc_t *dv, void *base_addr, CFI_attribute_t attribute,
		   CFI_type_t type, size_t elem_len, CFI_rank_t rank,
		   const CFI_index_t extents[])
{
  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptor must not be NULL. */
      if (dv == NULL)
	{
	  fprintf (stderr, "CFI_establish: C descriptor is NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      /* Rank must be between 0 and CFI_MAX_RANK. */
      if (rank < 0 || rank > CFI_MAX_RANK)
	{
	  fprintf (stderr, "CFI_establish: Rank must be between 0 and %d, "
		   "0 < rank (0 !< %d).\n", CFI_MAX_RANK, (int)rank);
	  return CFI_INVALID_RANK;
	}

      /* If base address is not NULL, the established C descriptor is for a
	  nonallocatable entity. */
      if (attribute == CFI_attribute_allocatable && base_addr != NULL)
	{
	  fprintf (stderr, "CFI_establish: If base address is not NULL, "
		   "the established C descriptor must be "
		   "for a nonallocatable entity.\n");
	  return CFI_INVALID_ATTRIBUTE;
	}
    }

  dv->base_addr = base_addr;

  if (type == CFI_type_char || type == CFI_type_ucs4_char
      || type == CFI_type_struct || type == CFI_type_other)
    {
      /* Note that elem_len has type size_t, which is unsigned.  */
      if (unlikely (compile_options.bounds_check) && elem_len == 0)
	{
	  fprintf (stderr, "CFI_establish: The supplied elem_len must "
		   "be greater than zero.\n");
	  return CFI_INVALID_ELEM_LEN;
	}
      dv->elem_len = elem_len;
    }
  else if (type == CFI_type_cptr)
    dv->elem_len = sizeof (void *);
  else if (type == CFI_type_cfunptr)
    dv->elem_len = sizeof (void (*)(void));
  else if (unlikely (compile_options.bounds_check) && type < 0)
    {
      fprintf (stderr, "CFI_establish: Invalid type (type = %d).\n",
	       (int)type);
      return CFI_INVALID_TYPE;
    }
  else
    {
      /* base_type describes the intrinsic type with kind parameter. */
      size_t base_type = type & CFI_type_mask;
      /* base_type_size is the size in bytes of the variable as given by its
       * kind parameter. */
      size_t base_type_size = (type - base_type) >> CFI_type_kind_shift;
      /* Kind type 10 maps onto the 80-bit long double encoding on x86.
	 Note that this has different storage size for -m32 than -m64.  */
      if (base_type_size == 10)
	base_type_size = sizeof (long double);
      /* Complex numbers are twice the size of their real counterparts. */
      if (base_type == CFI_type_Complex)
	base_type_size *= 2;
      dv->elem_len = base_type_size;
    }

  dv->version = CFI_VERSION;
  dv->rank = rank;
  dv->attribute = attribute;
  dv->type = type;

  /* Extents must not be NULL if rank is greater than zero and base_addr is not
     NULL */
  if (rank > 0 && base_addr != NULL)
    {
      if (unlikely (compile_options.bounds_check) && extents == NULL)
        {
	  fprintf (stderr, "CFI_establish: Extents must not be NULL "
		   "if rank is greater than zero and base address is "
		   "not NULL.\n");
	  return CFI_INVALID_EXTENT;
	}

      for (int i = 0; i < rank; i++)
	{
	  /* The standard requires all dimensions to be nonnegative.
	     Apparently you can have an extent-zero dimension but can't
	     construct an assumed-size array with -1 as the extent
	     of the last dimension.  */
	  if (unlikely (compile_options.bounds_check) && extents[i] < 0)
	    {
	      fprintf (stderr, "CFI_establish: Extents must be nonnegative "
		       "(extents[%d] = %" PRIiPTR ").\n",
		       i, (ptrdiff_t)extents[i]);
	      return CFI_INVALID_EXTENT;
	    }
	  dv->dim[i].lower_bound = 0;
	  dv->dim[i].extent = extents[i];
	  if (i == 0)
	    dv->dim[i].sm = dv->elem_len;
	  else
	    {
	      CFI_index_t extents_product = 1;
	      for (int j = 0; j < i; j++)
		extents_product *= extents[j];
	      dv->dim[i].sm = (CFI_index_t)(dv->elem_len * extents_product);
	    }
	}
    }

  return CFI_SUCCESS;
}


int CFI_is_contiguous (const CFI_cdesc_t *dv)
{
  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptor must not be NULL. */
      if (dv == NULL)
	{
	  fprintf (stderr, "CFI_is_contiguous: C descriptor is NULL.\n");
	  return 0;
	}

      /* Base address must not be NULL. */
      if (dv->base_addr == NULL)
	{
	  fprintf (stderr, "CFI_is_contiguous: Base address of C descriptor "
		   "is already NULL.\n");
	  return 0;
	}

      /* Must be an array. */
      if (dv->rank <= 0)
	{
	  fprintf (stderr, "CFI_is_contiguous: C descriptor must describe "
		   "an array.\n");
	  return 0;
	}
    }

  /* Assumed size arrays are always contiguous.  */
  if (dv->rank > 0 && dv->dim[dv->rank - 1].extent == -1)
    return 1;

  /* If an array is not contiguous the memory stride is different to
     the element length. */
  for (int i = 0; i < dv->rank; i++)
    {
      if (i == 0 && dv->dim[i].sm == (CFI_index_t)dv->elem_len)
	continue;
      else if (i > 0
	       && dv->dim[i].sm == (CFI_index_t)(dv->dim[i - 1].sm
				   * dv->dim[i - 1].extent))
	continue;

      return 0;
    }

  /* Array sections are guaranteed to be contiguous by the previous test.  */
  return 1;
}


int CFI_section (CFI_cdesc_t *result, const CFI_cdesc_t *source,
		 const CFI_index_t lower_bounds[],
		 const CFI_index_t upper_bounds[], const CFI_index_t strides[])
{
  /* Dimension information. */
  CFI_index_t lower[CFI_MAX_RANK];
  CFI_index_t upper[CFI_MAX_RANK];
  CFI_index_t stride[CFI_MAX_RANK];
  int zero_count = 0;

  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptors must not be NULL. */
      if (source == NULL)
	{
	  fprintf (stderr, "CFI_section: Source must not be NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      if (result == NULL)
	{
	  fprintf (stderr, "CFI_section: Result must not be NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      /* Base address of source must not be NULL. */
      if (source->base_addr == NULL)
	{
	  fprintf (stderr, "CFI_section: Base address of source must "
		   "not be NULL.\n");
	  return CFI_ERROR_BASE_ADDR_NULL;
	}

      /* Result must not be an allocatable array. */
      if (result->attribute == CFI_attribute_allocatable)
	{
	  fprintf (stderr, "CFI_section: Result must not describe an "
		   "allocatable array.\n");
	  return CFI_INVALID_ATTRIBUTE;
	}

      /* Source must be some form of array (nonallocatable nonpointer array,
	 allocated allocatable array or an associated pointer array). */
      if (source->rank <= 0)
	{
	  fprintf (stderr, "CFI_section: Source must describe an array.\n");
	  return CFI_INVALID_RANK;
	}

      /* Element lengths of source and result must be equal. */
      if (result->elem_len != source->elem_len)
	{
	  fprintf (stderr, "CFI_section: The element lengths of "
		   "source (source->elem_len = %" PRIiPTR ") and result "
		   "(result->elem_len = %" PRIiPTR ") must be equal.\n",
		   (ptrdiff_t)source->elem_len, (ptrdiff_t)result->elem_len);
	  return CFI_INVALID_ELEM_LEN;
	}

      /* Types must be equal. */
      if (result->type != source->type)
	{
	  fprintf (stderr, "CFI_section: Types of source "
		   "(source->type = %d) and result (result->type = %d) "
		   "must be equal.\n", source->type, result->type);
	  return CFI_INVALID_TYPE;
	}
    }

  /* Stride of zero in the i'th dimension means rank reduction in that
     dimension. */
  for (int i = 0; i < source->rank; i++)
    {
      if (strides[i] == 0)
	zero_count++;
    }

  /* Rank of result must be equal the the rank of source minus the number of
   * zeros in strides. */
  if (unlikely (compile_options.bounds_check)
      && result->rank != source->rank - zero_count)
    {
      fprintf (stderr, "CFI_section: Rank of result must be equal to the "
		       "rank of source minus the number of zeros in strides "
		       "(result->rank = source->rank - zero_count, %d != %d "
		       "- %d).\n", result->rank, source->rank, zero_count);
      return CFI_INVALID_RANK;
    }

  /* Lower bounds. */
  if (lower_bounds == NULL)
    {
      for (int i = 0; i < source->rank; i++)
	lower[i] = source->dim[i].lower_bound;
    }
  else
    {
      for (int i = 0; i < source->rank; i++)
	lower[i] = lower_bounds[i];
    }

  /* Upper bounds. */
  if (upper_bounds == NULL)
    {
      if (unlikely (compile_options.bounds_check)
	  && source->dim[source->rank - 1].extent == -1)
        {
	  fprintf (stderr, "CFI_section: Source must not be an assumed-size "
		   "array if upper_bounds is NULL.\n");
	  return CFI_INVALID_EXTENT;
	}

      for (int i = 0; i < source->rank; i++)
	upper[i] = source->dim[i].lower_bound + source->dim[i].extent - 1;
    }
  else
    {
      for (int i = 0; i < source->rank; i++)
	upper[i] = upper_bounds[i];
    }

  /* Stride */
  if (strides == NULL)
    {
      for (int i = 0; i < source->rank; i++)
	stride[i] = 1;
    }
  else
    {
      for (int i = 0; i < source->rank; i++)
	{
	  stride[i] = strides[i];
	  /* If stride[i] == 0 then lower[i] and upper[i] must be equal. */
	  if (unlikely (compile_options.bounds_check)
	      && stride[i] == 0 && lower[i] != upper[i])
	    {
	      fprintf (stderr, "CFI_section: If strides[%d] = 0, then "
		       "lower_bounds[%d] = %" PRIiPTR " and "
		       "upper_bounds[%d] = %" PRIiPTR " must be equal.\n",
		       i, i, (ptrdiff_t)lower_bounds[i], i,
		       (ptrdiff_t)upper_bounds[i]);
	      return CFI_ERROR_OUT_OF_BOUNDS;
	    }
	}
    }

  /* Check that section upper and lower bounds are within the array bounds. */
  if (unlikely (compile_options.bounds_check))
    for (int i = 0; i < source->rank; i++)
      {
	bool assumed_size
	  = (i == source->rank - 1 && source->dim[i].extent == -1);
	CFI_index_t ub
	  = source->dim[i].lower_bound + source->dim[i].extent - 1;
	if (lower_bounds != NULL
	    && (lower[i] < source->dim[i].lower_bound
		|| (!assumed_size && lower[i] > ub)))
	  {
	    fprintf (stderr, "CFI_section: Lower bounds must be within "
		     "the bounds of the Fortran array "
		     "(source->dim[%d].lower_bound "
		     "<= lower_bounds[%d] <= source->dim[%d].lower_bound "
		     "+ source->dim[%d].extent - 1, "
		     "%" PRIiPTR " <= %" PRIiPTR " <= %" PRIiPTR ").\n",
		     i, i, i, i,
		     (ptrdiff_t)source->dim[i].lower_bound,
		     (ptrdiff_t)lower[i],
		     (ptrdiff_t)ub);
	    return CFI_ERROR_OUT_OF_BOUNDS;
	  }

	if (upper_bounds != NULL
	    && (upper[i] < source->dim[i].lower_bound
		|| (!assumed_size && upper[i] > ub)))
	  {
	    fprintf (stderr, "CFI_section: Upper bounds must be within "
		     "the bounds of the Fortran array "
		     "(source->dim[%d].lower_bound "
		     "<= upper_bounds[%d] <= source->dim[%d].lower_bound "
		     "+ source->dim[%d].extent - 1, "
		     "%" PRIiPTR " !<= %" PRIiPTR " !<= %" PRIiPTR ").\n",
		     i, i, i, i,
		     (ptrdiff_t)source->dim[i].lower_bound,
		     (ptrdiff_t)upper[i],
		     (ptrdiff_t)ub);
	    return CFI_ERROR_OUT_OF_BOUNDS;
	  }

	if (upper[i] < lower[i] && stride[i] >= 0)
	  {
	    fprintf (stderr, "CFI_section: If the upper bound is smaller than "
		     "the lower bound for a given dimension (upper[%d] < "
		     "lower[%d], %" PRIiPTR " < %" PRIiPTR "), then the "
		     "stride for said dimension must be negative "
		     "(stride[%d] < 0, %" PRIiPTR " < 0).\n",
		     i, i, (ptrdiff_t)upper[i], (ptrdiff_t)lower[i],
		     i, (ptrdiff_t)stride[i]);
	    return CFI_INVALID_STRIDE;
	  }
      }

  /* Set the base address.  We have to compute this first in the case
     where source == result, before we overwrite the dimension data.  */
  result->base_addr = CFI_address (source, lower);

  /* Set the appropriate dimension information that gives us access to the
   * data. */
  for (int i = 0, o = 0; i < source->rank; i++)
    {
      if (stride[i] == 0)
	continue;
      result->dim[o].lower_bound = 0;
      result->dim[o].extent = 1 + (upper[i] - lower[i])/stride[i];
      result->dim[o].sm = stride[i] * source->dim[i].sm;
      o++;
    }

  return CFI_SUCCESS;
}


int CFI_select_part (CFI_cdesc_t *result, const CFI_cdesc_t *source,
		     size_t displacement, size_t elem_len)
{
  if (unlikely (compile_options.bounds_check))
    {
      /* C descriptors must not be NULL. */
      if (source == NULL)
	{
	  fprintf (stderr, "CFI_select_part: Source must not be NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      if (result == NULL)
	{
	  fprintf (stderr, "CFI_select_part: Result must not be NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}

      /* Attribute of result will be CFI_attribute_other or
	 CFI_attribute_pointer. */
      if (result->attribute == CFI_attribute_allocatable)
	{
	  fprintf (stderr, "CFI_select_part: Result must not describe an "
		   "allocatable object (result->attribute != %d).\n",
		   CFI_attribute_allocatable);
	  return CFI_INVALID_ATTRIBUTE;
	}

      /* Base address of source must not be NULL. */
      if (source->base_addr == NULL)
	{
	  fprintf (stderr, "CFI_select_part: Base address of source must "
		   "not be NULL.\n");
	  return CFI_ERROR_BASE_ADDR_NULL;
	}

      /* Source and result must have the same rank. */
      if (source->rank != result->rank)
	{
	  fprintf (stderr, "CFI_select_part: Source and result must have "
		   "the same rank (source->rank = %d, result->rank = %d).\n",
		   (int)source->rank, (int)result->rank);
	  return CFI_INVALID_RANK;
	}

      /* Nonallocatable nonpointer must not be an assumed size array. */
      if (source->rank > 0 && source->dim[source->rank - 1].extent == -1)
	{
	  fprintf (stderr, "CFI_select_part: Source must not describe an "
		   "assumed size array  (source->dim[%d].extent != -1).\n",
		   source->rank - 1);
	  return CFI_INVALID_DESCRIPTOR;
	}
    }

  /* Element length is ignored unless result->type specifies a Fortran
     character type.  */
  if (result->type == CFI_type_char || result->type == CFI_type_ucs4_char)
    result->elem_len = elem_len;

  if (unlikely (compile_options.bounds_check))
    {
      /* Ensure displacement is within the bounds of the element length
	 of source.*/
      if (displacement > source->elem_len - 1)
	{
	  fprintf (stderr, "CFI_select_part: Displacement must be within the "
		   "bounds of source (0 <= displacement <= source->elem_len "
		   "- 1, 0 <= %" PRIiPTR " <= %" PRIiPTR ").\n",
		   (ptrdiff_t)displacement,
		   (ptrdiff_t)(source->elem_len - 1));
	  return CFI_ERROR_OUT_OF_BOUNDS;
	}

      /* Ensure displacement and element length of result are less than or
	 equal to the element length of source. */
      if (displacement + result->elem_len > source->elem_len)
	{
	  fprintf (stderr, "CFI_select_part: Displacement plus the element "
		   "length of result must be less than or equal to the "
		   "element length of source (displacement + result->elem_len "
		   "<= source->elem_len, "
		   "%" PRIiPTR " + %" PRIiPTR " = %" PRIiPTR " <= %" PRIiPTR
		   ").\n",
		   (ptrdiff_t)displacement, (ptrdiff_t)result->elem_len,
		   (ptrdiff_t)(displacement + result->elem_len),
		   (ptrdiff_t)source->elem_len);
	  return CFI_ERROR_OUT_OF_BOUNDS;
	}
    }

  if (result->rank > 0)
    {
      for (int i = 0; i < result->rank; i++)
	{
	  result->dim[i].lower_bound = source->dim[i].lower_bound;
	  result->dim[i].extent = source->dim[i].extent;
	  result->dim[i].sm = source->dim[i].sm;
        }
    }

  result->base_addr = (char *) source->base_addr + displacement;
  return CFI_SUCCESS;
}


int CFI_setpointer (CFI_cdesc_t *result, CFI_cdesc_t *source,
		    const CFI_index_t lower_bounds[])
{
  /* Result must not be NULL and must be a Fortran pointer. */
  if (unlikely (compile_options.bounds_check))
    {
      if (result == NULL)
	{
	  fprintf (stderr, "CFI_setpointer: Result is NULL.\n");
	  return CFI_INVALID_DESCRIPTOR;
	}
      
      if (result->attribute != CFI_attribute_pointer)
	{
 	  fprintf (stderr, "CFI_setpointer: Result shall be the address of a "
		   "C descriptor for a Fortran pointer.\n");
 	  return CFI_INVALID_ATTRIBUTE;
 	}
    }
      
  /* If source is NULL, the result is a C descriptor that describes a
   * disassociated pointer. */
  if (source == NULL)
    {
      result->base_addr = NULL;
      result->version  = CFI_VERSION;
    }
  else
    {
      /* Check that the source is valid and that element lengths, ranks
	 and types of source and result are the same. */
      if (unlikely (compile_options.bounds_check))
	{
	  if (source->base_addr == NULL
	      && source->attribute == CFI_attribute_allocatable)
	    {
	      fprintf (stderr, "CFI_setpointer: The source is an "
		       "allocatable object but is not allocated.\n");
	      return CFI_ERROR_BASE_ADDR_NULL;
	    }
	  if (source->rank > 0
	      && source->dim[source->rank - 1].extent == -1)
	    {
	      fprintf (stderr, "CFI_setpointer: The source is an "
		       "assumed-size array.\n");
	      return CFI_INVALID_EXTENT;
	    }
	  if (result->elem_len != source->elem_len)
	    {
	      fprintf (stderr, "CFI_setpointer: Element lengths of result "
		       "(result->elem_len = %" PRIiPTR ") and source "
		       "(source->elem_len = %" PRIiPTR ") "
		       " must be the same.\n",
		       (ptrdiff_t)result->elem_len,
		       (ptrdiff_t)source->elem_len);
	      return CFI_INVALID_ELEM_LEN;
	    }

	  if (result->rank != source->rank)
	    {
	      fprintf (stderr, "CFI_setpointer: Ranks of result "
		       "(result->rank = %d) and source (source->rank = %d) "
		       "must be the same.\n", result->rank, source->rank);
	      return CFI_INVALID_RANK;
	    }

	  if (result->type != source->type)
	    {
	      fprintf (stderr, "CFI_setpointer: Types of result "
		       "(result->type = %d) and source (source->type = %d) "
		       "must be the same.\n", result->type, source->type);
	      return CFI_INVALID_TYPE;
	    }
	}

      /* If the source is a disassociated pointer, the result must also
	 describe a disassociated pointer. */
      if (source->base_addr == NULL
	  && source->attribute == CFI_attribute_pointer)
	result->base_addr = NULL;
      else
	result->base_addr = source->base_addr;

      /* Assign components to result. */
      result->version = source->version;

      /* Dimension information. */
      for (int i = 0; i < source->rank; i++)
	{
	  if (lower_bounds != NULL)
	    result->dim[i].lower_bound = lower_bounds[i];
	  else
	    result->dim[i].lower_bound = source->dim[i].lower_bound;

	  result->dim[i].extent = source->dim[i].extent;
	  result->dim[i].sm = source->dim[i].sm;
	}
    }

  return CFI_SUCCESS;
}
