/* Copyright (C) The GNU Toolchain Authors.
   Contributed by Mohamed Atef <mohamedatef1698@gmail.com>.
   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).
   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.
   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.
   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains the implementation of functions defined in
   section 5.5.1, 5.5.2.  */

#include "ompd-helper.h"

/* Per OMPD initialization and finalization.  */

__UINT64_TYPE__ gompd_state;
const ompd_callbacks_t *callbacks;

ompd_rc_t
ompd_initialize (ompd_word_t api_version,
		 const ompd_callbacks_t *callbacks_table)
{
  if (callbacks_table == NULL)
    return ompd_rc_bad_input;

  ompd_word_t version;
  ompd_rc_t ret = ompd_get_api_version (&version);

  if (version != api_version)
    return ompd_rc_unsupported;

  callbacks = callbacks_table;
  return ret;
}

ompd_rc_t
ompd_get_api_version (ompd_word_t *version)
{
  if (version == NULL)
    return ompd_rc_bad_input;

  *version = OMPD_VERSION;
  return ompd_rc_ok;
}

ompd_rc_t
ompd_get_version_string (const char **string)
{
  if (string == NULL)
    return ompd_rc_bad_input;
  static const char tmp_string[]
    = "GNU OpenMP runtime implementing OMPD version "
      stringize (OMPD_VERSION) " Debugging library";
  *string = tmp_string;
  return ompd_rc_ok;
}

ompd_rc_t
ompd_finalize (void)
{
  return ompd_rc_ok;
}

/* Per process initialization and finalization.  */

ompd_rc_t
ompd_process_initialize (ompd_address_space_context_t *context,
			 ompd_address_space_handle_t **handle)
{
  if (context == NULL || handle == NULL)
    return ompd_rc_bad_input;

  ompd_rc_t ret = gompd_get_sizes (context);
  if (ret != ompd_rc_ok)
    return ret;

  /* Naive way to read from memory.  */
  ompd_address_t symbol_addr = {OMPD_SEGMENT_UNSPECIFIED, 0};
  GET_VALUE (context, NULL, "gompd_state", gompd_state, gompd_state,
	     target_sizes.sizeof_long_long, 1, ret, symbol_addr);

  ret = callbacks->alloc_memory (sizeof (ompd_address_space_handle_t),
				 (void **) (handle));

  if (ret != ompd_rc_ok)
    return ret;

  if (handle == NULL)
    return ompd_rc_error;

  (*handle)->context = context;
  (*handle)->kind = OMPD_DEVICE_KIND_HOST;
  return ret;
}

/* OMPD will not support GPUs for now.  */

ompd_rc_t
ompd_device_initialize (ompd_address_space_handle_t *process_handle,
			ompd_address_space_context_t *device_context,
			ompd_device_t kind, ompd_size_t sizeof_id, void *id,
			ompd_address_space_handle_t **device_handle)
{
  if (device_context == NULL)
    return ompd_rc_bad_input;

  return ompd_rc_unsupported;
}


ompd_rc_t
ompd_rel_address_space_handle (ompd_address_space_handle_t *handle)
{
  if (handle == NULL)
    return ompd_rc_stale_handle;

  ompd_rc_t ret = callbacks->free_memory ((void *) handle);
  return ret;
}
