/* Manipulate doubly linked lists.
   Copyright (C) 2025 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */


#ifndef _DOUBLY_LINKED_LIST_H
#define _DOUBLY_LINKED_LIST_H

/* Doubly linked list implementation enforcing typing.

   This implementation of doubly linked list tries to achieve the enforcement of
   typing similarly to C++ templates, but without encapsulation.

   All the functions are prefixed with the type of the value: "AType_xxx".
   Some functions are prefixed with "_AType_xxx" and are not part of the public
   API, so should not be used, except for _##LTYPE##_merge_sort with a caveat
   (see note above its definition).

   Each function (### is a placeholder for method name) has a macro for:
   (1) its invocation LINKED_LIST_###(LTYPE).
   (2) its prototype LINKED_LIST_DECL_###(A, A2, scope). To add in a header
       file, or a source file for forward declaration. 'scope' should be set
       respectively to 'extern', or 'static'.
   (3) its definition LINKED_LIST_DEFN_###(A, A2, scope). To add in a source
       file with the 'scope' set respectively to nothing, or 'static' depending
       on (2).

   Data structures requirements:
   - LTYPE corresponds to the node of a doubly linked list. It needs to define
     attributes 'prev' and 'next' which are pointers on the type of a node.
     For instance:
       struct my_list_node
       {
	 T value;
	 struct my_list_node *prev;
	 struct my_list_node *next;
       };
   - LWRAPPERTYPE is a structure wrapping the nodes and others metadata (first,
     last, size).
 */


/* Mutative operations:
    - append
    - prepend
    - insert_before
    - pop_front
    - pop_back
    - remove
    - swap
   The header and body of each of those operation can be declared individually,
   or as a whole via LINKED_LIST_MUTATIVE_OPS_PROTOTYPE for the prototypes, and
   LINKED_LIST_MUTATIVE_OPS_DECL for the implementations.  */

/* Append the given node new_ to the exising list.
   Precondition: prev and next of new_ must be NULL.  */
#define LINKED_LIST_APPEND(LTYPE)		LTYPE##_append

#define LINKED_LIST_DECL_APPEND(LWRAPPERTYPE, LTYPE, EXPORT)		\
  EXPORT void								\
  LTYPE##_append (LWRAPPERTYPE *wrapper, LTYPE *new_)

#define LINKED_LIST_DEFN_APPEND(LWRAPPERTYPE, LTYPE, EXPORT)		\
EXPORT void								\
LTYPE##_append (LWRAPPERTYPE *wrapper, LTYPE *new_)			\
{									\
  if (wrapper->last == NULL)						\
    wrapper->first = new_;						\
  else									\
    {									\
      new_->prev = wrapper->last;					\
      wrapper->last->next = new_;					\
    }									\
  wrapper->last = new_;							\
  ++wrapper->size;							\
}

/* Prepend the given node new_ to the existing list.
   Precondition: prev and next of new_ must be NULL.  */
#define LINKED_LIST_PREPEND(LTYPE)		LTYPE##_prepend

#define LINKED_LIST_DECL_PREPEND(LWRAPPERTYPE, LTYPE, EXPORT)		\
  EXPORT void								\
  LTYPE##_prepend (LWRAPPERTYPE *wrapper, LTYPE *new_)

#define LINKED_LIST_DEFN_PREPEND(LWRAPPERTYPE, LTYPE, EXPORT)		\
EXPORT void								\
LTYPE##_prepend (LWRAPPERTYPE *wrapper, LTYPE *new_)			\
{									\
  if (wrapper->first == NULL)						\
    wrapper->last = new_;						\
  else									\
    {									\
      new_->next = wrapper->first;					\
      wrapper->first->prev = new_;					\
    }									\
  wrapper->first = new_;						\
  ++wrapper->size;							\
}

/* Insert the given node new_ before 'where' in the existing list.
   If where == NULL, the insertion is equivalent to an append.
   If where == first, the insertion is equivalent to a prepend.  */
#define LINKED_LIST_INSERT_BEFORE(LTYPE)	LTYPE##_insert_before

#define LINKED_LIST_DECL_INSERT_BEFORE(LWRAPPERTYPE, LTYPE, EXPORT)	\
  EXPORT void								\
  LTYPE##_insert_before (LWRAPPERTYPE *wrapper,				\
			 LTYPE *new_,					\
			 LTYPE *where)

#define LINKED_LIST_DEFN_INSERT_BEFORE(LWRAPPERTYPE, LTYPE, EXPORT)	\
EXPORT void								\
LTYPE##_insert_before (LWRAPPERTYPE *wrapper,				\
		       LTYPE *new_,					\
		       LTYPE *where)					\
{									\
  if (where == wrapper->first)						\
    LTYPE##_prepend (wrapper, new_);					\
  else if (where == NULL)						\
    LTYPE##_append (wrapper, new_);					\
  else									\
    {									\
      where->prev->next = new_;						\
      new_->prev = where->prev;						\
      where->prev = new_;						\
      new_->next = where;						\
      ++wrapper->size;							\
    }									\
}

/* Pop the first node of the list.  */
#define LINKED_LIST_POP_FRONT(LTYPE)		LTYPE##_pop_front

#define LINKED_LIST_DECL_POP_FRONT(LWRAPPERTYPE, LTYPE, EXPORT)		\
  EXPORT LTYPE *							\
  LTYPE##_pop_front (LWRAPPERTYPE *wrapper)

#define LINKED_LIST_DEFN_POP_FRONT(LWRAPPERTYPE, LTYPE, EXPORT)		\
EXPORT LTYPE *								\
LTYPE##_pop_front (LWRAPPERTYPE *wrapper)				\
{									\
  LTYPE *front_node = wrapper->first;					\
  if (front_node != NULL)						\
    {									\
      wrapper->first = front_node->next;				\
      if (wrapper->last == front_node)					\
	wrapper->last = NULL;						\
      else								\
	{								\
	  front_node->next->prev = NULL;				\
	  front_node->next = NULL;					\
	}								\
      front_node->next = NULL;						\
      --wrapper->size;							\
    }									\
  return front_node;							\
}

/* Pop the last node of the list.  */
#define LINKED_LIST_POP_BACK(LTYPE)		LTYPE##_pop_back

#define LINKED_LIST_DECL_POP_BACK(LWRAPPERTYPE, LTYPE, EXPORT)		\
  EXPORT LTYPE *							\
  LTYPE##_pop_back (LWRAPPERTYPE *wrapper)

#define LINKED_LIST_DEFN_POP_BACK(LWRAPPERTYPE, LTYPE, EXPORT)		\
EXPORT LTYPE *								\
LTYPE##_pop_back (LWRAPPERTYPE *wrapper)				\
{									\
  LTYPE *back_node = wrapper->last;					\
  if (back_node != NULL)						\
    {									\
      wrapper->last = back_node->prev;					\
      if (wrapper->first == back_node)					\
	wrapper->first = NULL;						\
      else								\
	{								\
	  back_node->prev->next = NULL;					\
	  back_node->prev = NULL;					\
	}								\
      back_node->prev = NULL;						\
      --wrapper->size;							\
    }									\
  return back_node;							\
}

/* Remove the given node from the existing list, and return the previous
   node.  */
#define LINKED_LIST_REMOVE(LTYPE)		LTYPE##_remove

#define LINKED_LIST_DECL_REMOVE(LWRAPPERTYPE, LTYPE, EXPORT)		\
  EXPORT LTYPE *							\
  LTYPE##_remove (LWRAPPERTYPE *wrapper, LTYPE *node)

#define LINKED_LIST_DEFN_REMOVE(LWRAPPERTYPE, LTYPE, EXPORT)		\
EXPORT LTYPE *								\
LTYPE##_remove (LWRAPPERTYPE *wrapper, LTYPE *node)			\
{									\
  LTYPE *previous = NULL;						\
									\
  if (node->prev != NULL)						\
    {									\
      node->prev->next = node->next;					\
      if (node->next == NULL)						\
	wrapper->last = node->prev;					\
      else								\
	node->next->prev = node->prev;					\
      previous = node->prev;						\
      node->next = NULL;						\
      node->prev = NULL;						\
      --wrapper->size;							\
    }									\
  else									\
    LTYPE##_pop_front (wrapper);					\
									\
  return previous;							\
}

/* Generic swap.  */
#define LINKED_LIST_SWAP(LTYPE)			LTYPE##_swap

#define LINKED_LIST_DECL_SWAP(LWRAPPERTYPE, LTYPE, EXPORT)		\
  EXPORT void								\
  LTYPE##_swap (LWRAPPERTYPE *wrapper, LTYPE *node1, LTYPE *node2)

/* Swap two nodes in a list.  */
#define LINKED_LIST_DEFN_SWAP(LWRAPPERTYPE, LTYPE, EXPORT)		\
EXPORT void								\
LTYPE##_swap (LWRAPPERTYPE *wrapper, LTYPE *node1, LTYPE *node2)	\
{									\
  LTYPE *prev1 = node1->prev;						\
  LTYPE *next1 = node1->next;						\
  LTYPE *prev2 = node2->prev;						\
  LTYPE *next2 = node2->next;						\
									\
  if (prev1 != NULL)							\
    prev1->next = node2;						\
  else									\
    wrapper->first = node2;						\
  if (prev2 != NULL)							\
    prev2->next = node1;						\
  else									\
    wrapper->first = node1;						\
									\
  if (next1 != NULL)							\
    next1->prev = node2;						\
  else									\
    wrapper->last = node2;						\
  if (next2 != NULL)							\
    next2->prev = node1;						\
  else									\
    wrapper->last = node1;						\
									\
  {									\
    LTYPE *temp = node1->next;						\
    node1->next = node2->next;						\
    node2->next = temp;							\
  }									\
  {									\
    LTYPE *temp = node1->prev;						\
    node1->prev = node2->prev;						\
    node2->prev = temp;							\
  }									\
}

/* Note: all the mutative operations below also update the data in the wrapper,
   i.e. first, last and size.  */
#define LINKED_LIST_MUTATIVE_OPS_PROTOTYPE(LWRAPPERTYPE, LTYPE, EXPORT)	\
  LINKED_LIST_DECL_APPEND(LWRAPPERTYPE, LTYPE, EXPORT);			\
  LINKED_LIST_DECL_PREPEND(LWRAPPERTYPE, LTYPE, EXPORT);		\
  LINKED_LIST_DECL_INSERT_BEFORE(LWRAPPERTYPE, LTYPE, EXPORT);		\
  LINKED_LIST_DECL_POP_FRONT(LWRAPPERTYPE, LTYPE, EXPORT);		\
  LINKED_LIST_DECL_POP_BACK(LWRAPPERTYPE, LTYPE, EXPORT);		\
  LINKED_LIST_DECL_REMOVE(LWRAPPERTYPE, LTYPE, EXPORT);			\
  LINKED_LIST_DECL_SWAP(LWRAPPERTYPE, LTYPE, EXPORT)

#define LINKED_LIST_MUTATIVE_OPS_DECL(LWRAPPERTYPE, LTYPE, EXPORT)	\
  LINKED_LIST_DEFN_APPEND(LWRAPPERTYPE, LTYPE, EXPORT)			\
  LINKED_LIST_DEFN_PREPEND(LWRAPPERTYPE, LTYPE, EXPORT)			\
  LINKED_LIST_DEFN_INSERT_BEFORE(LWRAPPERTYPE, LTYPE, EXPORT)		\
  LINKED_LIST_DEFN_POP_FRONT(LWRAPPERTYPE, LTYPE, EXPORT)		\
  LINKED_LIST_DEFN_POP_BACK(LWRAPPERTYPE, LTYPE, EXPORT)		\
  LINKED_LIST_DEFN_REMOVE(LWRAPPERTYPE, LTYPE, EXPORT)			\
  LINKED_LIST_DEFN_SWAP(LWRAPPERTYPE, LTYPE, EXPORT)


/* Sorting.  */

#define LINKED_LIST_MERGE_SORT_(LTYPE)	LTYPE##_merge_sort_

#define LINKED_LIST_MERGE_SORT(LTYPE)	LTYPE##_merge_sort

#define LINKED_LIST_MERGE_SORT_PROTOTYPE_(LTYPE, EXPORT)		\
  EXPORT LTYPE *							\
  LTYPE##_merge_sort_ (LTYPE *node,					\
		       int (*fn_cmp) (const LTYPE *, const LTYPE *))

#define LINKED_LIST_MERGE_SORT_PROTOTYPE(LWRAPPERTYPE, LTYPE, EXPORT)	\
  EXPORT void								\
  LTYPE##_merge_sort (LWRAPPERTYPE *wrapper,				\
		      int (*fn_cmp) (const LTYPE *, const LTYPE *))

/* Note: all the functions and macros below starting with "_" should be
   considered private.  */

/* Compute the middle element of the list based on the turtle and hare
   approach, i.e. the hare runs twice faster than the turtle.  */
#define _MERGE_SORT_IMPL_COMPUTE_TURTLE(LTYPE)				\
static inline LTYPE *							\
LTYPE##_merge_sort_compute_turtle_ (LTYPE *node)			\
{									\
  if (node == NULL)							\
    return node;							\
									\
  LTYPE *turtle = node, *hare = node->next;				\
  while (hare != NULL && hare->next != NULL)				\
    {									\
      turtle = turtle->next;						\
      hare = hare->next->next;						\
    }									\
  return turtle;							\
}

/* Append n at the end of l_out, and return the next node after n.
   l_out and l_last should be ideally encapsulated into a list structure
   but this is overkill for what we need here.  */
#define _MERGE_SORT_IMPL_OUT_APPEND(LTYPE)				\
static inline LTYPE *							\
LTYPE##_merge_sort_out_append_ (LTYPE **l_out, LTYPE **l_last,		\
				LTYPE *n)				\
{									\
  if (*l_last == NULL)							\
    {									\
      *l_last = n;							\
      *l_out = n;							\
      n->prev = NULL;							\
    }									\
  else									\
    {									\
      (*l_last)->next = n;						\
      n->prev = *l_last;						\
      *l_last = n;							\
    }									\
									\
  return n->next;							\
}

/* Merge two sorted lists together.
   The returned value corresponds to the first element of the list.
   Note: both input lists are invalidated after the call.  */
#define _MERGE_SORT_IMPL_MERGE(LTYPE)					\
static inline LTYPE *							\
LTYPE##_merge_sort_merge_ (LTYPE *l_left, LTYPE *l_right,		\
			   int (*fn_cmp) (const LTYPE *, const LTYPE *))\
{									\
  if (l_left == NULL)							\
    return l_right;							\
  else if (l_right == NULL)						\
    return l_left;							\
									\
  LTYPE *l_out = NULL, *l_last = NULL;					\
									\
  LTYPE *l_l = l_left, *l_r = l_right;					\
  while (l_l != NULL && l_r != NULL)					\
    {									\
      int cmp = fn_cmp (l_l, l_r);					\
      if (cmp <= 0)							\
	l_l = LTYPE##_merge_sort_out_append_ (&l_out, &l_last, l_l);	\
      else								\
	l_r = LTYPE##_merge_sort_out_append_ (&l_out, &l_last, l_r);	\
    }									\
									\
  LTYPE *l_remaining = (l_l != NULL) ? l_l : l_r;			\
  while (l_remaining != NULL)						\
    l_remaining =							\
      LTYPE##_merge_sort_out_append_ (&l_out, &l_last, l_remaining);	\
									\
  return l_out;								\
}

/* Merge sort implementation taking the first node of the list to sort,
   and the comparison function. Returns the first node of the sorted list.
   Note: use this if you don't care about updating the information in the
   wrapper.  */
#define _MERGE_SORT_DEFN_SORT(LTYPE, EXPORT)				\
EXPORT LTYPE *								\
LTYPE##_merge_sort_ (LTYPE *node,					\
		     int (*fn_cmp)(const LTYPE *, const LTYPE *))	\
{									\
  if (node == NULL)							\
    return NULL;							\
  else if (node->next == NULL)						\
    return node;							\
									\
  LTYPE *left_end = LTYPE##_merge_sort_compute_turtle_ (node);		\
  LTYPE *left_begin = node;						\
  LTYPE *right_begin = left_end->next;					\
  /* break the list. */							\
  left_end->next = NULL;						\
  right_begin->prev = NULL;						\
									\
  left_begin = LTYPE##_merge_sort_ (left_begin, fn_cmp);		\
  right_begin = LTYPE##_merge_sort_ (right_begin, fn_cmp);		\
  return LTYPE##_merge_sort_merge_ (left_begin, right_begin, fn_cmp);	\
}

/* Merge sort wrapper that the end-user should be using as it updates the
   first and last metadata of the list in wrapper as well.
   If the user does not want to pay the cost of the update of the data,
   it can directly use _##LTYPE##_merge_sort_merge.  */
#define _MERGE_SORT_DEFN_WRAPPER_SORT(LWRAPPERTYPE, LTYPE, EXPORT)	\
EXPORT void								\
LTYPE##_merge_sort (LWRAPPERTYPE *wrapper,				\
		    int (*fn_cmp) (const LTYPE *, const LTYPE *))	\
{									\
  wrapper->first = LTYPE##_merge_sort_ (wrapper->first, fn_cmp);	\
									\
  if (wrapper->first == NULL || wrapper->first->next == NULL)		\
    wrapper->last = wrapper->first;					\
  else									\
    for (LTYPE *node = wrapper->first;					\
	 node != NULL;							\
	 node = node->next)						\
      wrapper->last = node;						\
}

#define LINKED_LIST_MERGE_SORT_DECL(LWRAPPERTYPE, LTYPE, EXPORT)	\
  _MERGE_SORT_IMPL_COMPUTE_TURTLE(LTYPE)				\
  _MERGE_SORT_IMPL_OUT_APPEND(LTYPE)					\
  _MERGE_SORT_IMPL_MERGE(LTYPE)						\
  _MERGE_SORT_DEFN_SORT(LTYPE, EXPORT)					\
  _MERGE_SORT_DEFN_WRAPPER_SORT(LWRAPPERTYPE, LTYPE, EXPORT)

#endif /* _DOUBLY_LINKED_LIST_H */
