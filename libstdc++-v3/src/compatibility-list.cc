// Compatibility symbols for previous versions, list bits -*- C++ -*-

// Copyright (C) 2010 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <bits/move.h>

_GLIBCXX_BEGIN_NESTED_NAMESPACE(std, _GLIBCXX_STD_D)

  struct _List_node_base
  {
    _List_node_base* _M_next;
    _List_node_base* _M_prev;

    void
    transfer(_List_node_base * const __first,
	     _List_node_base * const __last) throw ();

    void
    reverse() throw ();

    void
    hook(_List_node_base * const __position) throw ();

    void
    unhook() throw ();
  };

  void
  _List_node_base::transfer(_List_node_base * const __first,
                            _List_node_base * const __last) throw ()
  {
    if (this != __last)
    {
      // Remove [first, last) from its old position.
      __last->_M_prev->_M_next  = this;
      __first->_M_prev->_M_next = __last;
      this->_M_prev->_M_next    = __first;
  
      // Splice [first, last) into its new position.
      _List_node_base* const __tmp = this->_M_prev;
      this->_M_prev                = __last->_M_prev;
      __last->_M_prev              = __first->_M_prev;
      __first->_M_prev             = __tmp;
    }
  }

  void
  _List_node_base::reverse() throw ()
  {
    _List_node_base* __tmp = this;
    do
    {
      std::swap(__tmp->_M_next, __tmp->_M_prev);

      // Old next node is now prev.
      __tmp = __tmp->_M_prev;     
    } 
    while (__tmp != this);
  }

  void
  _List_node_base::hook(_List_node_base* const __position) throw ()
  {
    this->_M_next = __position;
    this->_M_prev = __position->_M_prev;
    __position->_M_prev->_M_next = this;
    __position->_M_prev = this;
  }

  void
  _List_node_base::unhook() throw ()
  {
    _List_node_base* const __next_node = this->_M_next;
    _List_node_base* const __prev_node = this->_M_prev;
    __prev_node->_M_next = __next_node;
    __next_node->_M_prev = __prev_node;
  }

_GLIBCXX_END_NESTED_NAMESPACE
