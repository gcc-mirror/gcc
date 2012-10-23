/* Copyright (c) 2011-2012, Sandia Corporation.
   All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 . Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
 . Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
 . Neither the name of the Sandia Corporation nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 THE POSSIBILITY OF SUCH DAMAGE.  */

/**
 * @file gupcr_runtime.c
 * GUPC Portals4 Runtime.
 */

/**
 * @addtogroup RUNTIME GUPCR PMI
 * @{
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <portals4.h>
#include <portals4/pmi.h>

struct map_t
{
  ptl_handle_ni_t handle;
  ptl_process_t *mapping;
};

/** Process rank */
static int rank = -1;
/** Number of processes */
static int size = 0;
struct map_t maps[4] = {
  {PTL_INVALID_HANDLE, NULL},
  {PTL_INVALID_HANDLE, NULL},
  {PTL_INVALID_HANDLE, NULL},
  {PTL_INVALID_HANDLE, NULL}
};

static int max_name_len, max_key_len, max_val_len;
static char *name, *key, *val;

static int
encode (const void *inval, int invallen, char *outval, int outvallen)
{
  static unsigned char encodings[] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  };
  int i;

  if (invallen * 2 + 1 > outvallen)
    {
      return 1;
    }

  for (i = 0; i < invallen; i++)
    {
      outval[2 * i] = encodings[((unsigned char *) inval)[i] & 0xf];
      outval[2 * i + 1] = encodings[((unsigned char *) inval)[i] >> 4];
    }

  outval[invallen * 2] = '\0';

  return 0;
}

static int
decode (const char *inval, void *outval, int outvallen)
{
  int i;
  char *ret = (char *) outval;

  if (outvallen != (int) (strlen (inval) / 2))
    {
      return 1;
    }

  for (i = 0; i < outvallen; ++i)
    {
      if (*inval >= '0' && *inval <= '9')
	{
	  ret[i] = *inval - '0';
	}
      else
	{
	  ret[i] = *inval - 'a' + 10;
	}
      inval++;
      if (*inval >= '0' && *inval <= '9')
	{
	  ret[i] |= ((*inval - '0') << 4);
	}
      else
	{
	  ret[i] |= ((*inval - 'a' + 10) << 4);
	}
      inval++;
    }

  return 0;
}

int
gupcr_runtime_init (void)
{
  int initialized;

  if (PMI_SUCCESS != PMI_Initialized (&initialized))
    {
      return 1;
    }

  if (0 == initialized)
    {
      if (PMI_SUCCESS != PMI_Init (&initialized))
	{
	  return 2;
	}
    }

  if (PMI_SUCCESS != PMI_Get_rank (&rank))
    {
      return 3;
    }

  if (PMI_SUCCESS != PMI_Get_size (&size))
    {
      return 4;
    }

  /* Initialize key/val work strings.  */

  if (PMI_SUCCESS != PMI_KVS_Get_name_length_max (&max_name_len))
    {
      return 5;
    }
  name = (char *) malloc (max_name_len);
  if (NULL == name)
    return 5;

  if (PMI_SUCCESS != PMI_KVS_Get_key_length_max (&max_key_len))
    {
      return 5;
    }
  key = (char *) malloc (max_key_len);
  if (NULL == key)
    return 5;

  if (PMI_SUCCESS != PMI_KVS_Get_value_length_max (&max_val_len))
    {
      return 5;
    }
  val = (char *) malloc (max_val_len);
  if (NULL == val)
    return 5;

  if (PMI_SUCCESS != PMI_KVS_Get_my_name (name, max_name_len))
    {
      return 5;
    }

  return 0;
}

int
gupcr_runtime_fini (void)
{
  int i;

  for (i = 0; i < 4; ++i)
    {
      if (NULL != maps[i].mapping)
	{
	  free (maps[i].mapping);
	}
    }

  PMI_Finalize ();

  return 0;
}

ptl_process_t *
gupcr_runtime_get_mapping (ptl_handle_ni_t ni_h)
{
  int i, ret;
  ptl_process_t my_id;
  struct map_t *map = NULL;

  for (i = 0; i < 4; ++i)
    {
      if (maps[i].handle == ni_h)
	{
	  return maps[i].mapping;
	}
    }

  for (i = 0; i < 4; ++i)
    {
      if (PTL_INVALID_HANDLE == maps[i].handle)
	{
	  map = &maps[i];
	  break;
	}
    }

  if (NULL == map)
    return NULL;

  ret = PtlGetPhysId (ni_h, &my_id);
  if (PTL_OK != ret)
    return NULL;

  /* put my information */
  snprintf (key, max_key_len, "libgupc-%lu-%lu-nid",
	    (long unsigned) ni_h, (long unsigned) rank);
  if (0 != encode (&my_id.phys.nid, sizeof (my_id.phys.nid), val,
		   max_val_len))
    {
      return NULL;
    }
  if (PMI_SUCCESS != PMI_KVS_Put (name, key, val))
    {
      return NULL;
    }

  snprintf (key, max_key_len, "libgupc-%lu-%lu-pid",
	    (long unsigned) ni_h, (long unsigned) rank);
  if (0 != encode (&my_id.phys.pid, sizeof (my_id.phys.pid), val,
		   max_val_len))
    {
      return NULL;
    }
  if (PMI_SUCCESS != PMI_KVS_Put (name, key, val))
    {
      return NULL;
    }

  if (PMI_SUCCESS != PMI_KVS_Commit (name))
    {
      return NULL;
    }

  if (PMI_SUCCESS != PMI_Barrier ())
    {
      return NULL;
    }

  /* get everyone's information */
  map->mapping = malloc (sizeof (ptl_process_t) * size);
  if (NULL == map->mapping)
    return NULL;

  for (i = 0; i < size; ++i)
    {
      snprintf (key, max_key_len, "libgupc-%lu-%lu-nid",
		(long unsigned) ni_h, (long unsigned) i);
      if (PMI_SUCCESS != PMI_KVS_Get (name, key, val, max_val_len))
	{
	  return NULL;
	}
      if (0 != decode (val, &(map->mapping)[i].phys.nid,
		       sizeof ((map->mapping)[i].phys.nid)))
	{
	  return NULL;
	}

      snprintf (key, max_key_len, "libgupc-%lu-%lu-pid",
		(long unsigned) ni_h, (long unsigned) i);
      if (PMI_SUCCESS != PMI_KVS_Get (name, key, val, max_val_len))
	{
	  return NULL;
	}
      if (0 != decode (val, &(map->mapping)[i].phys.pid,
		       sizeof ((map->mapping)[i].phys.pid)))
	{
	  return NULL;
	}
    }

  return map->mapping;
}

/**
 * Return this process rank.
 */
int
gupcr_runtime_get_rank (void)
{
  return rank;
}

/**
 * Return number of processes in the system.
 */
int
gupcr_runtime_get_size (void)
{
  return size;
}

/**
 * Runtime barrier.
 */
void
gupcr_runtime_barrier (void)
{
  PMI_Barrier ();
}

/** @} */
