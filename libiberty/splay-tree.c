/* A splay-tree datatype.  
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Mark Mitchell (mark@markmitchell.com).

This file is part of GNU CC.
   
GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#include "libiberty.h"
#include "splay-tree.h"

static void splay_tree_delete_helper    PARAMS((splay_tree, 
						splay_tree_node));
static void splay_tree_splay            PARAMS((splay_tree,
						splay_tree_key));
static splay_tree_node splay_tree_splay_helper     
                                        PARAMS((splay_tree,
						splay_tree_key,
						splay_tree_node*,
						splay_tree_node*,
						splay_tree_node*));
static int splay_tree_foreach_helper    PARAMS((splay_tree,
					        splay_tree_node,
						splay_tree_foreach_fn,
						void*));

/* Deallocate NODE (a member of SP), and all its sub-trees.  */

static void 
splay_tree_delete_helper (sp, node)
     splay_tree sp;
     splay_tree_node node;
{
  if (!node)
    return;

  splay_tree_delete_helper (sp, node->left);
  splay_tree_delete_helper (sp, node->right);

  if (sp->delete_key)
    (*sp->delete_key)(node->key);
  if (sp->delete_value)
    (*sp->delete_value)(node->value);

  (*sp->deallocate) ((char*) node, sp->allocate_data);
}

/* Help splay SP around KEY.  PARENT and GRANDPARENT are the parent
   and grandparent, respectively, of NODE.  */

static splay_tree_node
splay_tree_splay_helper (sp, key, node, parent, grandparent)
     splay_tree sp;
     splay_tree_key key;
     splay_tree_node *node;
     splay_tree_node *parent;
     splay_tree_node *grandparent;
{
  splay_tree_node *next;
  splay_tree_node n;
  int comparison;
  
  n = *node;

  if (!n)
    return *parent;

  comparison = (*sp->comp) (key, n->key);

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
      /* Continue down the tree.  */
      n = splay_tree_splay_helper (sp, key, next, node, parent);

      /* The recursive call will change the place to which NODE
	 points.  */
      if (*node != n)
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
  else if  (n == (*parent)->right && *parent == (*grandparent)->right)
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

/* Splay SP around KEY.  */

static void
splay_tree_splay (sp, key)
     splay_tree sp;
     splay_tree_key key;
{
  if (sp->root == 0)
    return;

  splay_tree_splay_helper (sp, key, &sp->root, 
			   /*grandparent=*/0, /*parent=*/0); 
}

/* Call FN, passing it the DATA, for every node below NODE, all of
   which are from SP, following an in-order traversal.  If FN every
   returns a non-zero value, the iteration ceases immediately, and the
   value is returned.  Otherwise, this function returns 0.  */

static int
splay_tree_foreach_helper (sp, node, fn, data)
     splay_tree sp;
     splay_tree_node node;
     splay_tree_foreach_fn fn;
     void* data;
{
  int val;

  if (!node)
    return 0;

  val = splay_tree_foreach_helper (sp, node->left, fn, data);
  if (val)
    return val;

  val = (*fn)(node, data);
  if (val)
    return val;

  return splay_tree_foreach_helper (sp, node->right, fn, data);
}


/* An allocator and deallocator based on xmalloc.  */
static void *
splay_tree_xmalloc_allocate (size, data)
     int size;
     void *data ATTRIBUTE_UNUSED;
{
  return xmalloc (size);
}

static void
splay_tree_xmalloc_deallocate (object, data)
     void *object;
     void *data ATTRIBUTE_UNUSED;
{
  free (object);
}


/* Allocate a new splay tree, using COMPARE_FN to compare nodes,
   DELETE_KEY_FN to deallocate keys, and DELETE_VALUE_FN to deallocate
   values.  Use xmalloc to allocate the splay tree structure, and any
   nodes added.  */

splay_tree 
splay_tree_new (compare_fn, delete_key_fn, delete_value_fn)
     splay_tree_compare_fn compare_fn;
     splay_tree_delete_key_fn delete_key_fn;
     splay_tree_delete_value_fn delete_value_fn;
{
  return (splay_tree_new_with_allocator
          (compare_fn, delete_key_fn, delete_value_fn,
           splay_tree_xmalloc_allocate, splay_tree_xmalloc_deallocate, 0));
}


/* Allocate a new splay tree, using COMPARE_FN to compare nodes,
   DELETE_KEY_FN to deallocate keys, and DELETE_VALUE_FN to deallocate
   values.  */

splay_tree 
splay_tree_new_with_allocator (compare_fn, delete_key_fn, delete_value_fn,
                               allocate_fn, deallocate_fn, allocate_data)
     splay_tree_compare_fn compare_fn;
     splay_tree_delete_key_fn delete_key_fn;
     splay_tree_delete_value_fn delete_value_fn;
     splay_tree_allocate_fn allocate_fn;
     splay_tree_deallocate_fn deallocate_fn;
     void *allocate_data;
{
  splay_tree sp = (splay_tree) (*allocate_fn) (sizeof (struct splay_tree_s),
                                               allocate_data);
  sp->root = 0;
  sp->comp = compare_fn;
  sp->delete_key = delete_key_fn;
  sp->delete_value = delete_value_fn;
  sp->allocate = allocate_fn;
  sp->deallocate = deallocate_fn;
  sp->allocate_data = allocate_data;

  return sp;
}

/* Deallocate SP.  */

void 
splay_tree_delete (sp)
     splay_tree sp;
{
  splay_tree_delete_helper (sp, sp->root);
  (*sp->deallocate) ((char*) sp, sp->allocate_data);
}

/* Insert a new node (associating KEY with DATA) into SP.  If a
   previous node with the indicated KEY exists, its data is replaced
   with the new value.  Returns the new node.  */

splay_tree_node
splay_tree_insert (sp, key, value)
     splay_tree sp;
     splay_tree_key key;
     splay_tree_value value;
{
  int comparison = 0;

  splay_tree_splay (sp, key);

  if (sp->root)
    comparison = (*sp->comp)(sp->root->key, key);

  if (sp->root && comparison == 0)
    {
      /* If the root of the tree already has the indicated KEY, just
	 replace the value with VALUE.  */
      if (sp->delete_value)
	(*sp->delete_value)(sp->root->value);
      sp->root->value = value;
    } 
  else 
    {
      /* Create a new node, and insert it at the root.  */
      splay_tree_node node;
      
      node = ((splay_tree_node)
              (*sp->allocate) (sizeof (struct splay_tree_node_s),
                               sp->allocate_data));
      node->key = key;
      node->value = value;
      
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
    }

  return sp->root;
}

/* Remove KEY from SP.  It is not an error if it did not exist.  */

void
splay_tree_remove (sp, key)
     splay_tree sp;
     splay_tree_key key;
{
  splay_tree_splay (sp, key);

  if (sp->root && (*sp->comp) (sp->root->key, key) == 0)
    {
      splay_tree_node left, right;

      left = sp->root->left;
      right = sp->root->right;

      /* Delete the root node itself.  */
      if (sp->delete_value)
	(*sp->delete_value) (sp->root->value);
      (*sp->deallocate) (sp->root, sp->allocate_data);

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
splay_tree_lookup (sp, key)
     splay_tree sp;
     splay_tree_key key;
{
  splay_tree_splay (sp, key);

  if (sp->root && (*sp->comp)(sp->root->key, key) == 0)
    return sp->root;
  else
    return 0;
}

/* Return the node in SP with the greatest key.  */

splay_tree_node
splay_tree_max (sp)
     splay_tree sp;
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
splay_tree_min (sp)
     splay_tree sp;
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
splay_tree_predecessor (sp, key)
     splay_tree sp;
     splay_tree_key key;
{
  int comparison;
  splay_tree_node node;

  /* If the tree is empty, there is certainly no predecessor.  */
  if (!sp->root)
    return NULL;

  /* Splay the tree around KEY.  That will leave either the KEY
     itself, its predecessor, or its successor at the root.  */
  splay_tree_splay (sp, key);
  comparison = (*sp->comp)(sp->root->key, key);

  /* If the predecessor is at the root, just return it.  */
  if (comparison < 0)
    return sp->root;

  /* Otherwise, find the leftmost element of the right subtree.  */
  node = sp->root->left;
  if (node)
    while (node->right)
      node = node->right;

  return node;
}

/* Return the immediate successor KEY, or NULL if there is no
   predecessor.  KEY need not be present in the tree.  */

splay_tree_node
splay_tree_successor (sp, key)
     splay_tree sp;
     splay_tree_key key;
{
  int comparison;
  splay_tree_node node;

  /* If the tree is empty, there is certainly no predecessor.  */
  if (!sp->root)
    return NULL;

  /* Splay the tree around KEY.  That will leave either the KEY
     itself, its predecessor, or its successor at the root.  */
  splay_tree_splay (sp, key);
  comparison = (*sp->comp)(sp->root->key, key);

  /* If the successor is at the root, just return it.  */
  if (comparison > 0)
    return sp->root;

  /* Otherwise, find the rightmost element of the left subtree.  */
  node = sp->root->right;
  if (node)
    while (node->left)
      node = node->left;

  return node;
}

/* Call FN, passing it the DATA, for every node in SP, following an
   in-order traversal.  If FN every returns a non-zero value, the
   iteration ceases immediately, and the value is returned.
   Otherwise, this function returns 0.  */

int
splay_tree_foreach (sp, fn, data)
     splay_tree sp;
     splay_tree_foreach_fn fn;
     void *data;
{
  return splay_tree_foreach_helper (sp, sp->root, fn, data);
}

/* Splay-tree comparison function, treating the keys as ints.  */

int
splay_tree_compare_ints (k1, k2)
     splay_tree_key k1;
     splay_tree_key k2;
{
  if ((int) k1 < (int) k2)
    return -1;
  else if ((int) k1 > (int) k2)
    return 1;
  else 
    return 0;
}

/* Splay-tree comparison function, treating the keys as pointers.  */

int
splay_tree_compare_pointers (k1, k2)
     splay_tree_key k1;
     splay_tree_key k2;
{
  if ((char*) k1 < (char*) k2)
    return -1;
  else if ((char*) k1 > (char*) k2)
    return 1;
  else 
    return 0;
}
