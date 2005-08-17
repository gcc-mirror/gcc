/* Implement the SELECT statement for character variables.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"

typedef struct
{
  char *low;
  int low_len;
  char *high;
  int high_len;
  void *address;
}
select_struct;

extern void * select_string (select_struct *table, int table_len,
			     void *default_jump, const char *selector,
			     int selector_len);
export_proto(select_string);


/* select_string()-- Given a selector string and a table of
 * select_struct structures, return the address to jump to. */

void *
select_string (select_struct *table, int table_len, void *default_jump,
	       const char *selector, int selector_len)
{
  select_struct *t;
  int i, low, high, mid;

  if (table_len == 0)
    return default_jump;

  /* Record the default address if present */

  if (table->low == NULL && table->high == NULL)
    {
      default_jump = table->address;

      table++;
      table_len--;
      if (table_len == 0)
        return default_jump;
    }

  /* Try the high and low bounds if present. */

  if (table->low == NULL)
    {
      if (compare_string (table->high_len, table->high,
			        selector_len, selector) >= 0)
        return table->address;

      table++;
      table_len--;
      if (table_len == 0)
        return default_jump;
    }

  t = table + table_len - 1;

  if (t->high == NULL)
    {
      if (compare_string (t->low_len, t->low,
			        selector_len, selector) <= 0)
        return t->address;

      table_len--;
      if (table_len == 0)
        return default_jump;
    }

  /* At this point, the only table entries are bounded entries.  Find
     the right entry with a binary chop. */

  low = -1;
  high = table_len;

  while (low + 1 < high)
    {
      mid = (low + high) / 2;

      t = table + mid;
      i = compare_string (t->low_len, t->low, selector_len, selector);

      if (i == 0)
        return t->address;

      if (i < 0)
        low = mid;
      else
        high = mid;
    }

  /* The string now lies between the low indeces of the now-adjacent
     high and low entries.  Because it is less than the low entry of
     'high', it can't be that one.  If low is still -1, then no
     entries match.  Otherwise, we have to check the high entry of
     'low'. */

  if (low == -1)
    return default_jump;

  t = table + low;
  if (compare_string (selector_len, selector,
			    t->high_len, t->high) <= 0)
    return t->address;

  return default_jump;
}
