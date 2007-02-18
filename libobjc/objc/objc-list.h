/* Generic single linked list to keep various information 
   Copyright (C) 1993, 1994, 1996 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with files compiled with
   GCC to produce an executable, this does not cause the resulting executable
   to be covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU General Public License.  */

#ifndef __GNU_OBJC_LIST_H
#define __GNU_OBJC_LIST_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct objc_list {
  void *head;
  struct objc_list *tail;
};

/* Return a cons cell produced from (head . tail) */

static inline struct objc_list* 
list_cons(void* head, struct objc_list* tail)
{
  struct objc_list* cell;

  cell = (struct objc_list*)objc_malloc(sizeof(struct objc_list));
  cell->head = head;
  cell->tail = tail;
  return cell;
}

/* Return the length of a list, list_length(NULL) returns zero */

static inline int
list_length(struct objc_list* list)
{
  int i = 0;
  while(list)
    {
      i += 1;
      list = list->tail;
    }
  return i;
}

/* Return the Nth element of LIST, where N count from zero.  If N 
   larger than the list length, NULL is returned  */

static inline void*
list_nth(int indx, struct objc_list* list)
{
  while(indx-- != 0)
    {
      if(list->tail)
	list = list->tail;
      else
	return 0;
    }
  return list->head;
}

/* Remove the element at the head by replacing it by its successor */

static inline void
list_remove_head(struct objc_list** list)
{
  if ((*list)->tail)
    {
      struct objc_list* tail = (*list)->tail; /* fetch next */
      *(*list) = *tail;		/* copy next to list head */
      objc_free(tail);			/* free next */
    }
  else				/* only one element in list */
    {
      objc_free(*list);
      (*list) = 0;
    }
}


/* Remove the element with `car' set to ELEMENT */

static inline void
list_remove_elem(struct objc_list** list, void* elem)
{
  while (*list) {
    if ((*list)->head == elem)
      list_remove_head(list);
    list = &((*list)->tail);
  }
}

/* Map FUNCTION over all elements in LIST */

static inline void
list_mapcar(struct objc_list* list, void(*function)(void*))
{
  while(list)
    {
      (*function)(list->head);
      list = list->tail;
    }
}

/* Return element that has ELEM as car */

static inline struct objc_list**
list_find(struct objc_list** list, void* elem)
{
  while(*list)
    {
    if ((*list)->head == elem)
      return list;
    list = &((*list)->tail);
    }
  return NULL;
}

/* Free list (backwards recursive) */

static inline void
list_free(struct objc_list* list)
{
  if(list)
    {
      list_free(list->tail);
      objc_free(list);
    }
}

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* not __GNU_OBJC_LIST_H */
