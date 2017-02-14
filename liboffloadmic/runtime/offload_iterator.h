/*
    Copyright (c) 2014-2016 Intel Corporation.  All Rights Reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of Intel Corporation nor the names of its
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/*! \file
    \brief Iterator of Variable tables list used by the runtime library
*/

#ifndef OFFLOAD_ITERATOR_H_INCLUDED
#define OFFLOAD_ITERATOR_H_INCLUDED

#include <iterator>
#include "offload_table.h"

// The following class is for iteration over var table.
// It was extracted and moved to this offload_iterator.h file from offload_table.h
// to solve the problem with compiling with VS 2010. The problem was in incompatibility
// of STL objects in VS 2010 with ones in later VS versions.

// var table list iterator
class Iterator : public std::iterator<std::input_iterator_tag,
                                          VarTable::Entry> {
    public:
        Iterator() : m_node(0), m_entry(0) {}

        explicit Iterator(TableList<VarTable>::Node *node) {
            new_node(node);
        }

        Iterator& operator++() {
            if (m_entry != 0) {
                m_entry++;
                while (m_entry->name == 0) {
                    m_entry++;
                }
                if (m_entry->name == reinterpret_cast<const char*>(-1)) {
                    new_node(m_node->next);
                }
            }
            return *this;
        }

        bool operator==(const Iterator &other) const {
            return m_entry == other.m_entry;
        }

        bool operator!=(const Iterator &other) const {
            return m_entry != other.m_entry;
        }

        const VarTable::Entry* operator*() const {
            return m_entry;
        }

    private:
        void new_node(TableList<VarTable>::Node *node) {
            m_node = node;
            m_entry = 0;
            while (m_node != 0) {
                m_entry = m_node->table.entries;
                while (m_entry->name == 0) {
                    m_entry++;
                }
                if (m_entry->name != reinterpret_cast<const char*>(-1)) {
                    break;
                }
                m_node = m_node->next;
                m_entry = 0;
            }
        }

    private:
        TableList<VarTable>::Node                *m_node;
        const VarTable::Entry  *m_entry;
};

#endif  // OFFLOAD_ITERATOR_H_INCLUDED
