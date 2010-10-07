/* LTO plugin for gold.
   Copyright (C) 2010 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Common definitions that the object format dependent code needs.  */
#include "lto-plugin.h"

/* Callback used by gold to check if the plugin will claim FILE. Writes
   the result in CLAIMED. */

enum ld_plugin_status
claim_file_handler (const struct ld_plugin_input_file *file, int *claimed)
{
  /* To be implemented; for now, simply do nothing.  */
  return LDPS_OK;
}

/* Method called first thing at onload time to perform sanity checks.  */

enum ld_plugin_status
onload_format_checks (struct ld_plugin_tv *tv)
{
  return LDPS_OK;
}

