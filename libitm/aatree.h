/* Copyright (C) 2009-2023 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Implements an AA tree (http://en.wikipedia.org/wiki/AA_tree) with an
   integer key, and data attached to the node via flexible array member.  */

#ifndef LIBITM_AATREE_H
#define LIBITM_AATREE_H 1

namespace GTM HIDDEN {

template<typename KEY> class aa_tree_key;

class aa_node_base
{
 public:
  static const bool L = false;
  static const bool R = true;

 private:
  typedef unsigned int level_type;

  aa_node_base *m_link[2];
  level_type m_level;

  static const aa_node_base s_nil;

 public:
  aa_node_base(level_type l = 1)
    : m_link { const_cast<aa_node_base *>(&s_nil),
	       const_cast<aa_node_base *>(&s_nil) },
      m_level(l)
  { }

  bool is_nil() const { return this == &s_nil; }

  aa_node_base * link(bool d) { return m_link[d]; }
  void set_link(bool d, aa_node_base *val) { m_link[d] = val; }

  aa_node_base *skew();
  aa_node_base *split();
  void decrease_level();

  static void *operator new (size_t s) { return xmalloc (s); }
  static void operator delete (void *p) { free (p); }
};

template<typename KEY>
struct aa_node_key : public aa_node_base
{
  typedef aa_node_base base;

  KEY key;

  explicit aa_node_key(KEY k) : key(k) { }

  aa_node_key * link(bool d)
  {
    return static_cast<aa_node_key *>(base::link(d));
  }

  aa_node_key *skew() { return static_cast<aa_node_key *>(base::skew()); }
  aa_node_key *split() { return static_cast<aa_node_key *>(base::split()); }
};

template<typename KEY, typename DATA>
struct aa_node : public aa_node_key<KEY>
{
  typedef aa_node_key<KEY> base;

  DATA data;

  explicit aa_node(KEY k) : base(k) { }

  aa_node * link(bool d)
  {
    return static_cast<aa_node *>(base::link(d));
  }
};

template<typename KEY>
class aa_tree_key
{
 public:
  typedef aa_node_key<KEY> node;
  typedef node *node_ptr;

 protected:
  node_ptr m_tree;

 protected:
  aa_tree_key() : m_tree(0) { }

  node_ptr find(KEY k) const;

  static node_ptr insert_1 (node_ptr t, node_ptr n);
  void insert(node_ptr n);

  static node_ptr erase_1 (node_ptr t, KEY k, node_ptr *pfree);
  node_ptr erase(KEY k);
};

extern template class aa_tree_key<uintptr_t>;

template<typename KEY, typename DATA>
class aa_tree : public aa_tree_key<KEY>
{
 public:
  typedef aa_tree_key<KEY> base;
  typedef aa_node<KEY, DATA> node;
  typedef node *node_ptr;

  typedef void (*trav_callback)(KEY, DATA *, void *);

 private:
  static void clear_1 (node_ptr);
  static void traverse_1 (node_ptr, trav_callback, void *);

 public:
  aa_tree() = default;
  ~aa_tree() { clear(); }

  static void *operator new (size_t s, aa_tree<KEY, DATA>* p) { return p; }

  DATA *find(KEY k) const
  {
    node_ptr n = static_cast<node_ptr>(base::find (k));
    return n ? &n->data : 0;
  }

  DATA *insert(KEY k)
  {
    node_ptr n = new node(k);
    base::insert(n);
    return &n->data;
  }

  void erase(KEY k)
  {
    node_ptr n = static_cast<node_ptr>(base::erase (k));
    delete n;
  }

  node_ptr remove(KEY k, DATA** data)
  {
    node_ptr n = static_cast<node_ptr>(base::erase (k));
    *data = (n ? &n->data : 0);
    return n;
  }

  void clear()
  {
    node_ptr n = static_cast<node_ptr>(this->m_tree);
    if (n)
      {
	this->m_tree = 0;
	clear_1 (n);
      }
  }

  void traverse (trav_callback cb, void *cb_data)
  {
    node_ptr t = static_cast<node_ptr>(this->m_tree);
    if (t != 0)
      traverse_1 (t, cb, cb_data);
  }
};


template<typename KEY, typename DATA>
void
aa_tree<KEY, DATA>::clear_1 (node_ptr t)
{
  if (t->is_nil())
    return;
  clear_1 (t->link(node::L));
  clear_1 (t->link(node::R));
  delete t;
}

template<typename KEY, typename DATA>
void
aa_tree<KEY, DATA>::traverse_1 (node_ptr t, trav_callback cb, void *cb_data)
{
  if (t->is_nil())
    return;
  cb (t->key, &t->data, cb_data);
  traverse_1 (t->link(node::L), cb, cb_data);
  traverse_1 (t->link(node::R), cb, cb_data);
}

} // namespace GTM

#endif // LIBITM_AATREE_H
