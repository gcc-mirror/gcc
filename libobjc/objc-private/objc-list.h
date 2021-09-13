/* Generic single linked list to keep various information 
   Copyright (C) 1993-2021 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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

#ifndef __GNU_OBJC_LIST_H
#define __GNU_OBJC_LIST_H

struct objc_list
{
  void *head;
  struct objc_list *tail;
};

/* Return a cons cell produced from (head . tail).  */
static inline struct objc_list* 
list_cons (void* head, struct objc_list* tail)
{
  struct objc_list* cell;

  cell = (struct objc_list*)objc_malloc (sizeof (struct objc_list));
  cell->head = head;
  cell->tail = tail;
  return cell;
}

/* Remove the element at the head by replacing it by its
   successor.  */
static inline void
list_remove_head (struct objc_list** list)
{
  if ((*list)->tail)
    {
      /* Fetch next.  */
      struct objc_list* tail = (*list)->tail;

      /* Copy next to list head.  */
      *(*list) = *tail;

      /* Free next.  */
      objc_free (tail);
    }
  else
    {
      /* Inly one element in list.  */
      objc_free (*list);
      (*list) = 0;
    }
}


/* Map FUNCTION over all elements in LIST.  */
static inline void
list_mapcar (struct objc_list* list, void(*function)(void*))
{
  while (list)
    {
      (*function) (list->head);
      list = list->tail;
    }
}

/* Free list (backwards recursive).  */
static inline void
list_free (struct objc_list* list)
{
  if(list)
    {
      list_free (list->tail);
      objc_free (list);
    }
}

#endif /* not __GNU_OBJC_LIST_H */
