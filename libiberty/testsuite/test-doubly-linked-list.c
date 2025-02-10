#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "doubly-linked-list.h"

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/* Implementation */

typedef int T;

typedef struct ListNodeType
{
  T value;
  struct ListNodeType *next;
  struct ListNodeType *prev;
} ListNodeType;

ListNodeType * l_new_node (T value)
{
  ListNodeType *n = malloc (sizeof (ListNodeType));
  n->next = NULL;
  n->prev = NULL;
  n->value = value;
  return n;
}

typedef struct LinkedListWrapperType
{
  ListNodeType *first;
  ListNodeType *last;
  size_t size;
} LinkedListWrapperType;

int compare_nodes (const ListNodeType *n1, const ListNodeType *n2)
{
  if (n1->value == n2->value)
    return 0;
  else if (n1->value < n2->value)
    return -1;
  else
    return 1;
}

LINKED_LIST_MUTATIVE_OPS_PROTOTYPE (LinkedListWrapperType, ListNodeType, static);
LINKED_LIST_MERGE_SORT_PROTOTYPE (LinkedListWrapperType, ListNodeType, static);

LINKED_LIST_MUTATIVE_OPS_DECL (LinkedListWrapperType, ListNodeType, static)
LINKED_LIST_MERGE_SORT_DECL (LinkedListWrapperType, ListNodeType, static)

ListNodeType * find_last_node (ListNodeType *head)
{
  if (head == NULL)
    return NULL;

  ListNodeType *n = head;
  while (n->next != NULL)
    n = n->next;

  return n;
}

void l_print (ListNodeType *node)
{
  for (ListNodeType *l = node; l != NULL; l = l->next)
    printf ("%d ", l->value);
  printf ("\n");
}

void l_reverse_print (ListNodeType *last_node)
{
  for (ListNodeType *l = last_node; l != NULL; l = l->prev)
    printf ("%d ", l->value);
  printf ("\n");
}

struct test_data_t
{
  T const *content;
  size_t size;
};

bool run_test (const struct test_data_t *expect,
	       LinkedListWrapperType *current,
	       bool reversed)
{
  ListNodeType *node = (reversed) ? current->last : current->first;
  bool passed = true;
  for (int i=0; i<expect->size && node != NULL; ++i)
    {
      if (reversed)
	{
	  if (expect->content[expect->size - 1 - i] != node->value)
	    {
	      printf ("FAIL: mismatching expected (%d) VS current (%d).\n",
		      expect->content[expect->size - 1 - i], node->value);
	      passed = false;
	    }
	  if (node->prev == NULL && current->first != node)
	    {
	      printf ("FAIL: first is not matching the first node.\n");
	      passed = false;
	    }
	}
      else
	{
	  if (expect->content[i] != node->value)
	    {
	      printf ("FAIL: mismatching expected (%d) VS current (%d).\n",
		      expect->content[i], node->value);
	      passed = false;
	    }
	  if (node->next == NULL && current->last != node)
	    {
	      printf ("FAIL: last_ is not matching the last node.\n");
	      passed = false;
	    }
	}

      if (!passed)
	return false;

      if (reversed)
	node = node->prev;
      else
	node = node->next;
    }

  if (node != NULL)
    {
      printf ("FAIL: the list is longer than expected.\n");
      passed = false;
    }
  if (expect->size != current->size)
    {
      printf ("FAIL: size (%ld) is not matching the real size of the list (%ld).\n",
	      current->size, expect->size);
      passed = false;
    }

  return passed;
}

bool check(const char *op,
	  const struct test_data_t *expect,
	  LinkedListWrapperType *wrapper)
{
  bool success = true;
  bool res;

  l_print (wrapper->first);
  res = run_test (expect, wrapper, false);
  printf ("%s: test-linked-list::%s: check forward conformity\n",
	  res ? "PASS": "FAIL", op);
  success &= res;

  l_reverse_print (wrapper->last);
  res = run_test (expect, wrapper, true);
  printf ("%s: test-linked-list::%s: check backward conformity\n",
	  res ? "PASS": "FAIL", op);
  success &= res;

  printf("\n");

  return success;
}

const int EXPECT_0 [] = { 10, 4, 3, 1, 9, 2 };
const int EXPECT_1 [] = { 1, 2, 3, 4, 9, 10 };
const int EXPECT_2 [] = { 11, 1, 2, 3, 4, 9, 10 };
const int EXPECT_3 [] = { 11, 1, 2, 3, 4, 9, 8, 10 };
const int EXPECT_4 [] = { 11, 2, 3, 4, 9, 8, 10 };
const int EXPECT_5 [] = { 10, 2, 3, 4, 9, 8, 11 };
const int EXPECT_6 [] = { 10, 3, 2, 4, 9, 8, 11 };
const int EXPECT_7 [] = { 10, 9, 2, 4, 3, 8, 11 };
const int EXPECT_8 [] = { 2, 3, 4, 8, 9, 10, 11 };
const int EXPECT_9 [] = { 3, 4, 8, 9, 10, 11 };
const int EXPECT_10 [] = { 3, 4, 8, 9, 10 };
const struct test_data_t test_data[] = {
  { .content = EXPECT_0, .size = sizeof(EXPECT_0) / sizeof(EXPECT_0[0]) },
  { .content = EXPECT_1, .size = sizeof(EXPECT_1) / sizeof(EXPECT_1[0]) },
  { .content = EXPECT_2, .size = sizeof(EXPECT_2) / sizeof(EXPECT_2[0]) },
  { .content = EXPECT_3, .size = sizeof(EXPECT_3) / sizeof(EXPECT_3[0]) },
  { .content = EXPECT_4, .size = sizeof(EXPECT_4) / sizeof(EXPECT_4[0]) },
  { .content = EXPECT_5, .size = sizeof(EXPECT_5) / sizeof(EXPECT_5[0]) },
  { .content = EXPECT_6, .size = sizeof(EXPECT_6) / sizeof(EXPECT_6[0]) },
  { .content = EXPECT_7, .size = sizeof(EXPECT_7) / sizeof(EXPECT_7[0]) },
  { .content = EXPECT_8, .size = sizeof(EXPECT_8) / sizeof(EXPECT_8[0]) },
  { .content = EXPECT_9, .size = sizeof(EXPECT_9) / sizeof(EXPECT_9[0]) },
  { .content = EXPECT_10, .size = sizeof(EXPECT_10) / sizeof(EXPECT_10[0]) },
};

int main (void)
{
  int failures = 0;

  LinkedListWrapperType wrapper = {
    .first = NULL,
    .last = NULL,
    .size = 0,
  };

  /* Append nodes.  */
  LINKED_LIST_APPEND(ListNodeType) (&wrapper, l_new_node (10));
  LINKED_LIST_APPEND(ListNodeType) (&wrapper, l_new_node (4));
  LINKED_LIST_APPEND(ListNodeType) (&wrapper, l_new_node (3));
  LINKED_LIST_APPEND(ListNodeType) (&wrapper, l_new_node (1));
  LINKED_LIST_APPEND(ListNodeType) (&wrapper, l_new_node (9));
  LINKED_LIST_APPEND(ListNodeType) (&wrapper, l_new_node (2));

  failures += ! check ("append", &test_data[0], &wrapper);

  /* Sort nodes (without updating wrapper).  */
  wrapper.first =
    LINKED_LIST_MERGE_SORT_(ListNodeType) (wrapper.first, compare_nodes);
  wrapper.last = find_last_node (wrapper.first);

  failures += ! check ("sort", &test_data[1], &wrapper);

  /* Save a reference to this node for later.  */
  ListNodeType *n_to_remove = wrapper.first;

  /* Prepend node.  */
  LINKED_LIST_PREPEND(ListNodeType) (&wrapper, l_new_node (11));
  failures += ! check ("prepend", &test_data[2], &wrapper);

  /* Insert node.  */
  LINKED_LIST_INSERT_BEFORE(ListNodeType) (&wrapper, l_new_node (8), wrapper.last);
  failures += ! check ("insert_before", &test_data[3], &wrapper);

  /* Remove a node.  */
  LINKED_LIST_REMOVE(ListNodeType) (&wrapper, n_to_remove);
  failures += ! check ("remove", &test_data[4], &wrapper);

  /* Swap first and last.  */
  LINKED_LIST_SWAP(ListNodeType) (&wrapper, wrapper.first, wrapper.last);
  failures += ! check ("swap first and last", &test_data[5], &wrapper);

  /* Swap adjacent nodes.  */
  LINKED_LIST_SWAP(ListNodeType) (&wrapper, wrapper.first->next,
				  wrapper.first->next->next);
  failures += ! check ("swap adjacent nodes", &test_data[6], &wrapper);

  /* Swap non-adjacent nodes, but neither first nor last.  */
  LINKED_LIST_SWAP(ListNodeType) (&wrapper, wrapper.first->next,
				  wrapper.first->next->next->next->next);
  failures += ! check ("swap non-adjacent nodes", &test_data[7], &wrapper);

  /* Sort nodes.  */
  LINKED_LIST_MERGE_SORT(ListNodeType) (&wrapper, compare_nodes);
  failures += ! check ("sort", &test_data[8], &wrapper);

  /* Pop front.  */
  LINKED_LIST_POP_FRONT(ListNodeType) (&wrapper);
  failures += ! check ("pop_front", &test_data[9], &wrapper);

  /* Pop back.  */
  LINKED_LIST_POP_BACK(ListNodeType) (&wrapper);
  failures += ! check ("pop_back", &test_data[10], &wrapper);

  exit (failures ? EXIT_FAILURE : EXIT_SUCCESS);
}
