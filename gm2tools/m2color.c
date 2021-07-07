/* m2color.c interface to gcc colorization.

Copyright (C) 2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define m2color_c

extern "C" {

const char *m2color_colorize_start (bool show_color, char *name, unsigned int name_len)
{
  return "";
}

const char *m2color_colorize_stop (bool show_color)
{
  return "";
}


void _M2_m2color_init ()
{
}


void  _M2_m2color_finish ()
{
}

}
