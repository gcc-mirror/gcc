/* A splay-tree datatype.  
   Copyright (C) 1998, 1999, 2000, 2001, 2004 Free Software Foundation, Inc.
   Contributed by Mark Mitchell (mark@markmitchell.com).
   Adapted for libmudflap from libiberty.

This file is part of GNU CC.
   
GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
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

GNU CC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* For an easily readable description of splay-trees, see:

     Lewis, Harry R. and Denenberg, Larry.  Data Structures and Their
     Algorithms.  Harper-Collins, Inc.  1991.  */

#include <stdlib.h>
#include <stdio.h>
#include "splay-tree.h"


static void splay_tree_delete_helper (splay_tree, splay_tree_node);
static void splay_tree_splay (splay_tree, splay_tree_key);
static splay_tree_node splay_tree_splay_helper (splay_tree,
                                                splay_tree_key,
                                                splay_tree_node *,
                                                splay_tree_node *,
                                                splay_tree_node *);
static void *splay_tree_xmalloc (size_t size);
static void splay_tree_free (void *object);



/* Inline comparison function specialized for libmudflap's key type.  */
static inline int
compare_uintptr_t (splay_tree_key k1, splay_tree_key k2)
{
  if ((uintptr_t) k1 < (uintptr_t) k2)
    return -1;
  else if ((uintptr_t) k1 > (uintptr_t) k2)
    return 1;
  else
    return 0;
}


/* Help splay SP around KEY.  PARENT and GRANDPARENT are the parent
   and grandparent, respectively, of NODE.  */

static splay_tree_node
splay_tree_splay_helper (splay_tree sp,
                         splay_tree_key key,
                         splay_tree_node * node,
                         splay_tree_node * parent,
                         splay_tree_node * grandparent)
{
  splay_tree_node *next;
  splay_tree_node n;
  int comparison;

  n = *node;

  if (!n)
    return *parent;

  comparison = compare_uintptr_t (key, n->key);

  if (comparison == 0)
    /* We've found the target.  */
    next = 0;
  else if (comparison < 0)
    /* The target is to the left.  */
    next = &n->left;
  else
    /* The target is to the right.  */
    next = &n->right;

  if (next)
    {
      /* Check whether our recursion depth is too high.  Abort this search,
         and signal that a rebalance is required to continue.  */
      if (sp->depth > sp->max_depth)
        {
          sp->rebalance_p = 1;
          return n;
         }

      /* Continue down the tree.  */
      sp->depth ++;
      n = splay_tree_splay_helper (sp, key, next, node, parent);
      sp->depth --;

      /* The recursive call will change the place to which NODE
         points.  */
      if (*node != n || sp->rebalance_p)
        return n;
    }

  if (!parent)
    /* NODE is the root.  We are done.  */
    return n;

  /* First, handle the case where there is no grandparent (i.e.,
   *PARENT is the root of the tree.)  */
  if (!grandparent)
    {
      if (n == (*parent)->left)
        {
          *node = n->right;
          n->right = *parent;
        }
      else
        {
          *node = n->left;
          n->left = *parent;
        }
      *parent = n;
      return n;
    }

  /* Next handle the cases where both N and *PARENT are left children,
     or where both are right children.  */
  if (n == (*parent)->left && *parent == (*grandparent)->left)
    {
      splay_tree_node p = *parent;

      (*grandparent)->left = p->right;
      p->right = *grandparent;
      p->left = n->right;
      n->right = p;
      *grandparent = n;
      return n;
    }
  else if (n == (*parent)->right && *parent == (*grandparent)->right)
    {
      splay_tree_node p = *parent;

      (*grandparent)->right = p->left;
      p->left = *grandparent;
      p->right = n->left;
      n->left = p;
      *grandparent = n;
      return n;
    }

  /* Finally, deal with the case where N is a left child, but *PARENT
     is a right child, or vice versa.  */
  if (n == (*parent)->left)
    {
      (*parent)->left = n->right;
      n->right = *parent;
      (*grandparent)->right = n->left;
      n->left = *grandparent;
      *grandparent = n;
      return n;
    }
  else
    {
      (*parent)->right = n->left;
      n->left = *parent;
      (*grandparent)->left = n->right;
      n->right = *grandparent;
      *grandparent = n;
      return n;
    }
}



static int
splay_tree_rebalance_helper1 (splay_tree_node n, void *array_ptr)
{
  splay_tree_node **p = array_ptr;
  *(*p) = n;
  (*p)++;
  return 0;
}


static splay_tree_node
splay_tree_rebalance_helper2 (splay_tree_node * array, unsigned low,
                              unsigned high)
{
  unsigned middle = low + (high - low) / 2;
  splay_tree_node n = array[middle];

  /* Note that since we're producing a balanced binary tree, it is not a problem
     that this function is recursive.  */
  if (low + 1 <= middle)
    n->left = splay_tree_rebalance_helper2 (array, low, middle - 1);
  else
    n->left = NULL;

  if (middle + 1 <= high)
    n->right = splay_tree_rebalance_helper2 (array, middle + 1, high);
  else
    n->right = NULL;

  return n;
}


/* Rebalance the entire tree.  Do this by copying all the node
   pointers into an array, then cleverly re-linking them.  */
void
splay_tree_rebalance (splay_tree sp)
{
  splay_tree_node *all_nodes, *all_nodes_1;

  if (sp->num_keys <= 2)
    return;

  all_nodes = splay_tree_xmalloc (sizeof (splay_tree_node) * sp->num_keys);

  /* Traverse all nodes to copy their addresses into this array.  */
  all_nodes_1 = all_nodes;
  splay_tree_foreach (sp, splay_tree_rebalance_helper1,
                      (void *) &all_nodes_1);

  /* Relink all the nodes.  */
  sp->root = splay_tree_rebalance_helper2 (all_nodes, 0, sp->num_keys - 1);

  splay_tree_free (all_nodes);
}


/* Splay SP around KEY.  */
static void
splay_tree_splay (splay_tree sp, splay_tree_key key)
{
  if (sp->root == 0)
    return;

  /* If we just splayed the tree with the same key, do nothing.  */
  if (sp->last_splayed_key_p &&
      compare_uintptr_t (sp->last_splayed_key, key) == 0)
    return;

  /* Compute a maximum recursion depth for a splay tree with NUM nodes.
     The idea is to limit excessive stack usage if we're facing
     degenerate access patterns.  Unfortunately such patterns can occur
     e.g. during static initialization, where many static objects might
     be registered in increasing address sequence, or during a case where
     large tree-like heap data structures are allocated quickly. 

     On x86, this corresponds to roughly 200K of stack usage. 
     XXX: For libmudflapth, this could be a function of __mf_opts.thread_stack.  */
  sp->max_depth = 2500;
  sp->rebalance_p = sp->depth = 0;

  splay_tree_splay_helper (sp, key, &sp->root, NULL, NULL);
  if (sp->rebalance_p)
    {
      splay_tree_rebalance (sp);

      sp->rebalance_p = sp->depth = 0;
      splay_tree_splay_helper (sp, key, &sp->root, NULL, NULL);

      if (sp->rebalance_p)
        abort ();
    }


  /* Cache this splay key. */
  sp->last_splayed_key = key;
  sp->last_splayed_key_p = 1;
}



/* Allocate a new splay tree.  */
splay_tree
splay_tree_new ()
{
  splay_tree sp = splay_tree_xmalloc (sizeof (struct splay_tree_s));
  sp->root = NULL;
  sp->last_splayed_key_p = 0;
  sp->num_keys = 0;

  return sp;
}



/* Insert a new node (associating KEY with DATA) into SP.  If a
   previous node with the indicated KEY exists, its data is replaced
   with the new value.  Returns the new node.  */
splay_tree_node
splay_tree_insert (splay_tree sp, splay_tree_key key, splay_tree_value value)
{
  int comparison = 0;

  splay_tree_splay (sp, key);

  if (sp->root)
    comparison = compare_uintptr_t (sp->root->key, key);

  if (sp->root && comparison == 0)
    {
      /* If the root of the tree already has the indicated KEY, just
         replace the value with VALUE.  */
      sp->root->value = value;
    }
  else
    {
      /* Create a new node, and insert it at the root.  */
      splay_tree_node node;

      node = splay_tree_xmalloc (sizeof (struct splay_tree_node_s));
      node->key = key;
      node->value = value;
      sp->num_keys++;
      if (!sp->root)
        node->left = node->right = 0;
      else if (comparison < 0)
        {
          node->left = sp->root;
          node->right = node->left->right;
          node->left->right = 0;
        }
      else
        {
          node->right = sp->root;
          node->left = node->right->left;
          node->right->left = 0;
        }

      sp->root = node;
      sp->last_splayed_key_p = 0;
    }

  return sp->root;
}

/* Remove KEY from SP.  It is not an error if it did not exist.  */

void
splay_tree_remove (splay_tree sp, splay_tree_key key)
{
  splay_tree_splay (sp, key);
  sp->last_splayed_key_p = 0;
  if (sp->root && compare_uintptr_t (sp->root->key, key) == 0)
    {
      splay_tree_node left, right;
      left = sp->root->left;
      right = sp->root->right;
      /* Delete the root node itself.  */
      splay_tree_free (sp->root);
      sp->num_keys--;
      /* One of the children is now the root.  Doesn't matter much
         which, so long as we preserve the properties of the tree.  */
      if (left)
        {
          sp->root = left;
          /* If there was a right child as well, hang it off the 
             right-most leaf of the left child.  */
          if (right)
            {
              while (left->right)
                left = left->right;
              left->right = right;
            }
        }
      else
        sp->root = right;
    }
}

/* Lookup KEY in SP, returning VALUE if present, and NULL 
   otherwise.  */

splay_tree_node
splay_tree_lookup (splay_tree sp, splay_tree_key key)
{
  splay_tree_splay (sp, key);
  if (sp->root && compare_uintptr_t (sp->root->key, key) == 0)
    return sp->root;
  else
    return 0;
}

/* Return the node in SP with the greatest key.  */

splay_tree_node
splay_tree_max (splay_tree sp)
{
  splay_tree_node n = sp->root;
  if (!n)
    return NULL;
  while (n->right)
    n = n->right;
  return n;
}

/* Return the node in SP with the smallest key.  */

splay_tree_node
splay_tree_min (splay_tree sp)
{
  splay_tree_node n = sp->root;
  if (!n)
    return NULL;
  while (n->left)
    n = n->left;
  return n;
}

/* Return the immediate predecessor KEY, or NULL if there is no
   predecessor.  KEY need not be present in the tree.  */

splay_tree_node
splay_tree_predecessor (splay_tree sp, splay_tree_key key)
{
  int comparison;
  splay_tree_node node;
  /* If the tree is empty, there is certainly no predecessor.  */
  if (!sp->root)
    return NULL;
  /* Splay the tree around KEY.  That will leave either the KEY
     itself, its predecessor, or its successor at the root.  */
  splay_tree_splay (sp, key);
  comparison = compare_uintptr_t (sp->root->key, key);
  /* If the predecessor is at the root, just return it.  */
  if (comparison < 0)
    return sp->root;
  /* Otherwise, find the rightmost element of the left subtree.  */
  node = sp->root->left;
  if (node)
    while (node->right)
      node = node->right;
  return node;
}

/* Return the immediate successor KEY, or NULL if there is no
   successor.  KEY need not be present in the tree.  */

splay_tree_node
splay_tree_successor (splay_tree sp, splay_tree_key key)
{
  int comparison;
  splay_tree_node node;
  /* If the tree is empty, there is certainly no successor.  */
  if (!sp->root)
    return NULL;
  /* Splay the tree around KEY.  That will leave either the KEY
     itself, its predecessor, or its successor at the root.  */
  splay_tree_splay (sp, key);
  comparison = compare_uintptr_t (sp->root->key, key);
  /* If the successor is at the root, just return it.  */
  if (comparison > 0)
    return sp->root;
  /* Otherwise, find the leftmost element of the right subtree.  */
  node = sp->root->right;
  if (node)
    while (node->left)
      node = node->left;
  return node;
}

/* Call FN, passing it the DATA, for every node in SP, following an
   in-order traversal.  If FN every returns a non-zero value, the
   iteration ceases immediately, and the value is returned.
   Otherwise, this function returns 0.
   
   This function simulates recursion using dynamically allocated
   arrays, since it may be called from splay_tree_rebalance(), which
   in turn means that the tree is already uncomfortably deep for stack
   space limits.  */
int
splay_tree_foreach (splay_tree st, splay_tree_foreach_fn fn, void *data)
{
  splay_tree_node *stack1;
  char *stack2;
  unsigned sp;
  int val = 0;
  enum s { s_left, s_here, s_right, s_up };

  if (st->root == NULL) /* => num_keys == 0 */
    return 0;

  stack1 = splay_tree_xmalloc (sizeof (splay_tree_node) * st->num_keys);
  stack2 = splay_tree_xmalloc (sizeof (char) * st->num_keys);

  sp = 0;
  stack1 [sp] = st->root;
  stack2 [sp] = s_left;

  while (1)
    {
      splay_tree_node n;
      enum s s;

      n = stack1 [sp];
      s = stack2 [sp];

      /* Handle each of the four possible states separately.  */

      /* 1: We're here to traverse the left subtree (if any).  */
      if (s == s_left)
        {
          stack2 [sp] = s_here;
          if (n->left != NULL)
            {
              sp ++;
              stack1 [sp] = n->left;
              stack2 [sp] = s_left;
            }
        }

      /* 2: We're here to traverse this node.  */
      else if (s == s_here)
        {
          stack2 [sp] = s_right;
          val = (*fn) (n, data);
          if (val) break;
        }

      /* 3: We're here to traverse the right subtree (if any).  */
      else if (s == s_right)
        {
          stack2 [sp] = s_up;
          if (n->right != NULL)
            {
              sp ++;
              stack1 [sp] = n->right;
              stack2 [sp] = s_left;
            }
        }

      /* 4: We're here after both subtrees (if any) have been traversed.  */
      else if (s == s_up)
        {
          /* Pop the stack.  */
          if (sp == 0) break; /* Popping off the root note: we're finished!  */
          sp --;
        }

      else
        abort ();
    }

  splay_tree_free (stack1);
  splay_tree_free (stack2);
  return val;
}
