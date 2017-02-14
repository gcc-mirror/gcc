/* Implement the SELECT statement for character variables.
   Copyright (C) 2008-2017 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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

#define select_string SUFFIX(select_string)
#define select_struct SUFFIX(select_struct)
#define compare_string SUFFIX(compare_string)

typedef struct
{
  CHARTYPE *low;
  gfc_charlen_type low_len;
  CHARTYPE *high;
  gfc_charlen_type high_len;
  int address;
}
select_struct;

extern int select_string (select_struct *table, int table_len,
			  const CHARTYPE *selector,
			  gfc_charlen_type selector_len);
export_proto(select_string);


/* select_string()-- Given a selector string and a table of
 * select_struct structures, return the address to jump to. */

int
select_string (select_struct *table, int table_len, const CHARTYPE *selector,
	       gfc_charlen_type selector_len)
{
  select_struct *t;
  int i, low, high, mid;
  int default_jump = -1;

  if (table_len == 0)
    return -1;

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
      if (compare_string (t->low_len, t->low, selector_len, selector) <= 0)
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
  if (compare_string (selector_len, selector, t->high_len, t->high) <= 0)
    return t->address;

  return default_jump;
}
