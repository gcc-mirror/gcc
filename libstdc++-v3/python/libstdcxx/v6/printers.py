# Pretty-printers for libstdc++.

# Copyright (C) 2008-2018 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import gdb
import itertools
import re
import sys

### Python 2 + Python 3 compatibility code

# Resources about compatibility:
#
#  * <http://pythonhosted.org/six/>: Documentation of the "six" module

# FIXME: The handling of e.g. std::basic_string (at least on char)
# probably needs updating to work with Python 3's new string rules.
#
# In particular, Python 3 has a separate type (called byte) for
# bytestrings, and a special b"" syntax for the byte literals; the old
# str() type has been redefined to always store Unicode text.
#
# We probably can't do much about this until this GDB PR is addressed:
# <https://sourceware.org/bugzilla/show_bug.cgi?id=17138>

if sys.version_info[0] > 2:
    ### Python 3 stuff
    Iterator = object
    # Python 3 folds these into the normal functions.
    imap = map
    izip = zip
    # Also, int subsumes long
    long = int
else:
    ### Python 2 stuff
    class Iterator:
        """Compatibility mixin for iterators

        Instead of writing next() methods for iterators, write
        __next__() methods and use this mixin to make them work in
        Python 2 as well as Python 3.

        Idea stolen from the "six" documentation:
        <http://pythonhosted.org/six/#six.Iterator>
        """

        def next(self):
            return self.__next__()

    # In Python 2, we still need these from itertools
    from itertools import imap, izip

# Try to use the new-style pretty-printing if available.
_use_gdb_pp = True
try:
    import gdb.printing
except ImportError:
    _use_gdb_pp = False

# Try to install type-printers.
_use_type_printing = False
try:
    import gdb.types
    if hasattr(gdb.types, 'TypePrinter'):
        _use_type_printing = True
except ImportError:
    pass

# Starting with the type ORIG, search for the member type NAME.  This
# handles searching upward through superclasses.  This is needed to
# work around http://sourceware.org/bugzilla/show_bug.cgi?id=13615.
def find_type(orig, name):
    typ = orig.strip_typedefs()
    while True:
        # Strip cv-qualifiers.  PR 67440.
        search = '%s::%s' % (typ.unqualified(), name)
        try:
            return gdb.lookup_type(search)
        except RuntimeError:
            pass
        # The type was not found, so try the superclass.  We only need
        # to check the first superclass, so we don't bother with
        # anything fancier here.
        field = typ.fields()[0]
        if not field.is_base_class:
            raise ValueError("Cannot find type %s::%s" % (str(orig), name))
        typ = field.type

_versioned_namespace = '__8::'

def is_specialization_of(type, template_name):
    "Test if a type is a given template instantiation."
    global _versioned_namespace
    if _versioned_namespace:
        return re.match('^std::(%s)?%s<.*>$' % (_versioned_namespace, template_name), type) is not None
    return re.match('^std::%s<.*>$' % template_name, type) is not None

def strip_versioned_namespace(typename):
    global _versioned_namespace
    if _versioned_namespace:
        return typename.replace(_versioned_namespace, '')
    return typename

def strip_inline_namespaces(type_str):
    "Remove known inline namespaces from the canonical name of a type."
    type_str = strip_versioned_namespace(type_str)
    type_str = type_str.replace('std::__cxx11::', 'std::')
    expt_ns = 'std::experimental::'
    for lfts_ns in ('fundamentals_v1', 'fundamentals_v2'):
        type_str = type_str.replace(expt_ns+lfts_ns+'::', expt_ns)
    fs_ns = expt_ns + 'filesystem::'
    type_str = type_str.replace(fs_ns+'v1::', fs_ns)
    return type_str

def get_template_arg_list(type_obj):
    "Return a type's template arguments as a list"
    n = 0
    template_args = []
    while True:
        try:
            template_args.append(type_obj.template_argument(n))
        except:
            return template_args
        n += 1

class SmartPtrIterator(Iterator):
    "An iterator for smart pointer types with a single 'child' value"

    def __init__(self, val):
        self.val = val

    def __iter__(self):
        return self

    def __next__(self):
        if self.val is None:
            raise StopIteration
        self.val, val = None, self.val
        return ('get()', val)

class SharedPointerPrinter:
    "Print a shared_ptr or weak_ptr"

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val
        self.pointer = val['_M_ptr']

    def children (self):
        return SmartPtrIterator(self.pointer)

    def to_string (self):
        state = 'empty'
        refcounts = self.val['_M_refcount']['_M_pi']
        if refcounts != 0:
            usecount = refcounts['_M_use_count']
            weakcount = refcounts['_M_weak_count']
            if usecount == 0:
                state = 'expired, weak count %d' % weakcount
            else:
                state = 'use count %d, weak count %d' % (usecount, weakcount - 1)
        return '%s<%s> (%s)' % (self.typename, str(self.val.type.template_argument(0)), state)

class UniquePointerPrinter:
    "Print a unique_ptr"

    def __init__ (self, typename, val):
        self.val = val
        impl_type = val.type.fields()[0].type.tag
        if is_specialization_of(impl_type, '__uniq_ptr_impl'): # New implementation
            self.pointer = val['_M_t']['_M_t']['_M_head_impl']
        elif is_specialization_of(impl_type, 'tuple'):
            self.pointer = val['_M_t']['_M_head_impl']
        else:
            raise ValueError("Unsupported implementation for unique_ptr: %s" % impl_type)

    def children (self):
        return SmartPtrIterator(self.pointer)

    def to_string (self):
        return ('std::unique_ptr<%s>' % (str(self.val.type.template_argument(0))))

def get_value_from_aligned_membuf(buf, valtype):
    """Returns the value held in a __gnu_cxx::__aligned_membuf."""
    return buf['_M_storage'].address.cast(valtype.pointer()).dereference()

def get_value_from_list_node(node):
    """Returns the value held in an _List_node<_Val>"""
    try:
        member = node.type.fields()[1].name
        if member == '_M_data':
            # C++03 implementation, node contains the value as a member
            return node['_M_data']
        elif member == '_M_storage':
            # C++11 implementation, node stores value in __aligned_membuf
            valtype = node.type.template_argument(0)
            return get_value_from_aligned_membuf(node['_M_storage'], valtype)
    except:
        pass
    raise ValueError("Unsupported implementation for %s" % str(node.type))

class StdListPrinter:
    "Print a std::list"

    class _iterator(Iterator):
        def __init__(self, nodetype, head):
            self.nodetype = nodetype
            self.base = head['_M_next']
            self.head = head.address
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self.base == self.head:
                raise StopIteration
            elt = self.base.cast(self.nodetype).dereference()
            self.base = elt['_M_next']
            count = self.count
            self.count = self.count + 1
            val = get_value_from_list_node(elt)
            return ('[%d]' % count, val)

    def __init__(self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val

    def children(self):
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        return self._iterator(nodetype, self.val['_M_impl']['_M_node'])

    def to_string(self):
        if self.val['_M_impl']['_M_node'].address == self.val['_M_impl']['_M_node']['_M_next']:
            return 'empty %s' % (self.typename)
        return '%s' % (self.typename)

class NodeIteratorPrinter:
    def __init__(self, typename, val, contname):
        self.val = val
        self.typename = typename
        self.contname = contname

    def to_string(self):
        if not self.val['_M_node']:
            return 'non-dereferenceable iterator for std::%s' % (self.contname)
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        node = self.val['_M_node'].cast(nodetype).dereference()
        return str(get_value_from_list_node(node))

class StdListIteratorPrinter(NodeIteratorPrinter):
    "Print std::list::iterator"

    def __init__(self, typename, val):
        NodeIteratorPrinter.__init__(self, typename, val, 'list')

class StdFwdListIteratorPrinter(NodeIteratorPrinter):
    "Print std::forward_list::iterator"

    def __init__(self, typename, val):
        NodeIteratorPrinter.__init__(self, typename, val, 'forward_list')

class StdSlistPrinter:
    "Print a __gnu_cxx::slist"

    class _iterator(Iterator):
        def __init__(self, nodetype, head):
            self.nodetype = nodetype
            self.base = head['_M_head']['_M_next']
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self.base == 0:
                raise StopIteration
            elt = self.base.cast(self.nodetype).dereference()
            self.base = elt['_M_next']
            count = self.count
            self.count = self.count + 1
            return ('[%d]' % count, elt['_M_data'])

    def __init__(self, typename, val):
        self.val = val

    def children(self):
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        return self._iterator(nodetype, self.val)

    def to_string(self):
        if self.val['_M_head']['_M_next'] == 0:
            return 'empty __gnu_cxx::slist'
        return '__gnu_cxx::slist'

class StdSlistIteratorPrinter:
    "Print __gnu_cxx::slist::iterator"

    def __init__(self, typename, val):
        self.val = val

    def to_string(self):
        if not self.val['_M_node']:
            return 'non-dereferenceable iterator for __gnu_cxx::slist'
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        return str(self.val['_M_node'].cast(nodetype).dereference()['_M_data'])

class StdVectorPrinter:
    "Print a std::vector"

    class _iterator(Iterator):
        def __init__ (self, start, finish, bitvec):
            self.bitvec = bitvec
            if bitvec:
                self.item   = start['_M_p']
                self.so     = start['_M_offset']
                self.finish = finish['_M_p']
                self.fo     = finish['_M_offset']
                itype = self.item.dereference().type
                self.isize = 8 * itype.sizeof
            else:
                self.item = start
                self.finish = finish
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            count = self.count
            self.count = self.count + 1
            if self.bitvec:
                if self.item == self.finish and self.so >= self.fo:
                    raise StopIteration
                elt = self.item.dereference()
                if elt & (1 << self.so):
                    obit = 1
                else:
                    obit = 0
                self.so = self.so + 1
                if self.so >= self.isize:
                    self.item = self.item + 1
                    self.so = 0
                return ('[%d]' % count, obit)
            else:
                if self.item == self.finish:
                    raise StopIteration
                elt = self.item.dereference()
                self.item = self.item + 1
                return ('[%d]' % count, elt)

    def __init__(self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val
        self.is_bool = val.type.template_argument(0).code  == gdb.TYPE_CODE_BOOL

    def children(self):
        return self._iterator(self.val['_M_impl']['_M_start'],
                              self.val['_M_impl']['_M_finish'],
                              self.is_bool)

    def to_string(self):
        start = self.val['_M_impl']['_M_start']
        finish = self.val['_M_impl']['_M_finish']
        end = self.val['_M_impl']['_M_end_of_storage']
        if self.is_bool:
            start = self.val['_M_impl']['_M_start']['_M_p']
            so    = self.val['_M_impl']['_M_start']['_M_offset']
            finish = self.val['_M_impl']['_M_finish']['_M_p']
            fo     = self.val['_M_impl']['_M_finish']['_M_offset']
            itype = start.dereference().type
            bl = 8 * itype.sizeof
            length   = (bl - so) + bl * ((finish - start) - 1) + fo
            capacity = bl * (end - start)
            return ('%s<bool> of length %d, capacity %d'
                    % (self.typename, int (length), int (capacity)))
        else:
            return ('%s of length %d, capacity %d'
                    % (self.typename, int (finish - start), int (end - start)))

    def display_hint(self):
        return 'array'

class StdVectorIteratorPrinter:
    "Print std::vector::iterator"

    def __init__(self, typename, val):
        self.val = val

    def to_string(self):
        if not self.val['_M_current']:
            return 'non-dereferenceable iterator for std::vector'
        return str(self.val['_M_current'].dereference())

class StdTuplePrinter:
    "Print a std::tuple"

    class _iterator(Iterator):
        def __init__ (self, head):
            self.head = head

            # Set the base class as the initial head of the
            # tuple.
            nodes = self.head.type.fields ()
            if len (nodes) == 1:
                # Set the actual head to the first pair.
                self.head  = self.head.cast (nodes[0].type)
            elif len (nodes) != 0:
                raise ValueError("Top of tuple tree does not consist of a single node.")
            self.count = 0

        def __iter__ (self):
            return self

        def __next__ (self):
            # Check for further recursions in the inheritance tree.
            # For a GCC 5+ tuple self.head is None after visiting all nodes:
            if not self.head:
                raise StopIteration
            nodes = self.head.type.fields ()
            # For a GCC 4.x tuple there is a final node with no fields:
            if len (nodes) == 0:
                raise StopIteration
            # Check that this iteration has an expected structure.
            if len (nodes) > 2:
                raise ValueError("Cannot parse more than 2 nodes in a tuple tree.")

            if len (nodes) == 1:
                # This is the last node of a GCC 5+ std::tuple.
                impl = self.head.cast (nodes[0].type)
                self.head = None
            else:
                # Either a node before the last node, or the last node of
                # a GCC 4.x tuple (which has an empty parent).

                # - Left node is the next recursion parent.
                # - Right node is the actual class contained in the tuple.

                # Process right node.
                impl = self.head.cast (nodes[1].type)

                # Process left node and set it as head.
                self.head  = self.head.cast (nodes[0].type)

            self.count = self.count + 1

            # Finally, check the implementation.  If it is
            # wrapped in _M_head_impl return that, otherwise return
            # the value "as is".
            fields = impl.type.fields ()
            if len (fields) < 1 or fields[0].name != "_M_head_impl":
                return ('[%d]' % self.count, impl)
            else:
                return ('[%d]' % self.count, impl['_M_head_impl'])

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val;

    def children (self):
        return self._iterator (self.val)

    def to_string (self):
        if len (self.val.type.fields ()) == 0:
            return 'empty %s' % (self.typename)
        return '%s containing' % (self.typename)

class StdStackOrQueuePrinter:
    "Print a std::stack or std::queue"

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.visualizer = gdb.default_visualizer(val['c'])

    def children (self):
        return self.visualizer.children()

    def to_string (self):
        return '%s wrapping: %s' % (self.typename,
                                    self.visualizer.to_string())

    def display_hint (self):
        if hasattr (self.visualizer, 'display_hint'):
            return self.visualizer.display_hint ()
        return None

class RbtreeIterator(Iterator):
    """
    Turn an RB-tree-based container (std::map, std::set etc.) into
    a Python iterable object.
    """

    def __init__(self, rbtree):
        self.size = rbtree['_M_t']['_M_impl']['_M_node_count']
        self.node = rbtree['_M_t']['_M_impl']['_M_header']['_M_left']
        self.count = 0

    def __iter__(self):
        return self

    def __len__(self):
        return int (self.size)

    def __next__(self):
        if self.count == self.size:
            raise StopIteration
        result = self.node
        self.count = self.count + 1
        if self.count < self.size:
            # Compute the next node.
            node = self.node
            if node.dereference()['_M_right']:
                node = node.dereference()['_M_right']
                while node.dereference()['_M_left']:
                    node = node.dereference()['_M_left']
            else:
                parent = node.dereference()['_M_parent']
                while node == parent.dereference()['_M_right']:
                    node = parent
                    parent = parent.dereference()['_M_parent']
                if node.dereference()['_M_right'] != parent:
                    node = parent
            self.node = node
        return result

def get_value_from_Rb_tree_node(node):
    """Returns the value held in an _Rb_tree_node<_Val>"""
    try:
        member = node.type.fields()[1].name
        if member == '_M_value_field':
            # C++03 implementation, node contains the value as a member
            return node['_M_value_field']
        elif member == '_M_storage':
            # C++11 implementation, node stores value in __aligned_membuf
            valtype = node.type.template_argument(0)
            return get_value_from_aligned_membuf(node['_M_storage'], valtype)
    except:
        pass
    raise ValueError("Unsupported implementation for %s" % str(node.type))

# This is a pretty printer for std::_Rb_tree_iterator (which is
# std::map::iterator), and has nothing to do with the RbtreeIterator
# class above.
class StdRbtreeIteratorPrinter:
    "Print std::map::iterator, std::set::iterator, etc."

    def __init__ (self, typename, val):
        self.val = val
        valtype = self.val.type.template_argument(0).strip_typedefs()
        nodetype = '_Rb_tree_node<' + str(valtype) + '>'
        if _versioned_namespace and typename.startswith('std::' + _versioned_namespace):
            nodetype = _versioned_namespace + nodetype
        nodetype = gdb.lookup_type('std::' + nodetype)
        self.link_type = nodetype.strip_typedefs().pointer()

    def to_string (self):
        if not self.val['_M_node']:
            return 'non-dereferenceable iterator for associative container'
        node = self.val['_M_node'].cast(self.link_type).dereference()
        return str(get_value_from_Rb_tree_node(node))

class StdDebugIteratorPrinter:
    "Print a debug enabled version of an iterator"

    def __init__ (self, typename, val):
        self.val = val

    # Just strip away the encapsulating __gnu_debug::_Safe_iterator
    # and return the wrapped iterator value.
    def to_string (self):
        base_type = gdb.lookup_type('__gnu_debug::_Safe_iterator_base')
        itype = self.val.type.template_argument(0)
        safe_seq = self.val.cast(base_type)['_M_sequence']
        if not safe_seq:
            return str(self.val.cast(itype))
        if self.val['_M_version'] != safe_seq['_M_version']:
            return "invalid iterator"
        return str(self.val.cast(itype))

def num_elements(num):
    """Return either "1 element" or "N elements" depending on the argument."""
    return '1 element' if num == 1 else '%d elements' % num

class StdMapPrinter:
    "Print a std::map or std::multimap"

    # Turn an RbtreeIterator into a pretty-print iterator.
    class _iter(Iterator):
        def __init__(self, rbiter, type):
            self.rbiter = rbiter
            self.count = 0
            self.type = type

        def __iter__(self):
            return self

        def __next__(self):
            if self.count % 2 == 0:
                n = next(self.rbiter)
                n = n.cast(self.type).dereference()
                n = get_value_from_Rb_tree_node(n)
                self.pair = n
                item = n['first']
            else:
                item = self.pair['second']
            result = ('[%d]' % self.count, item)
            self.count = self.count + 1
            return result

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val

    def to_string (self):
        return '%s with %s' % (self.typename,
                               num_elements(len(RbtreeIterator (self.val))))

    def children (self):
        rep_type = find_type(self.val.type, '_Rep_type')
        node = find_type(rep_type, '_Link_type')
        node = node.strip_typedefs()
        return self._iter (RbtreeIterator (self.val), node)

    def display_hint (self):
        return 'map'

class StdSetPrinter:
    "Print a std::set or std::multiset"

    # Turn an RbtreeIterator into a pretty-print iterator.
    class _iter(Iterator):
        def __init__(self, rbiter, type):
            self.rbiter = rbiter
            self.count = 0
            self.type = type

        def __iter__(self):
            return self

        def __next__(self):
            item = next(self.rbiter)
            item = item.cast(self.type).dereference()
            item = get_value_from_Rb_tree_node(item)
            # FIXME: this is weird ... what to do?
            # Maybe a 'set' display hint?
            result = ('[%d]' % self.count, item)
            self.count = self.count + 1
            return result

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val

    def to_string (self):
        return '%s with %s' % (self.typename,
                               num_elements(len(RbtreeIterator (self.val))))

    def children (self):
        rep_type = find_type(self.val.type, '_Rep_type')
        node = find_type(rep_type, '_Link_type')
        node = node.strip_typedefs()
        return self._iter (RbtreeIterator (self.val), node)

class StdBitsetPrinter:
    "Print a std::bitset"

    def __init__(self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val

    def to_string (self):
        # If template_argument handled values, we could print the
        # size.  Or we could use a regexp on the type.
        return '%s' % (self.typename)

    def children (self):
        words = self.val['_M_w']
        wtype = words.type

        # The _M_w member can be either an unsigned long, or an
        # array.  This depends on the template specialization used.
        # If it is a single long, convert to a single element list.
        if wtype.code == gdb.TYPE_CODE_ARRAY:
            tsize = wtype.target ().sizeof
        else:
            words = [words]
            tsize = wtype.sizeof 

        nwords = wtype.sizeof / tsize
        result = []
        byte = 0
        while byte < nwords:
            w = words[byte]
            bit = 0
            while w != 0:
                if (w & 1) != 0:
                    # Another spot where we could use 'set'?
                    result.append(('[%d]' % (byte * tsize * 8 + bit), 1))
                bit = bit + 1
                w = w >> 1
            byte = byte + 1
        return result

class StdDequePrinter:
    "Print a std::deque"

    class _iter(Iterator):
        def __init__(self, node, start, end, last, buffer_size):
            self.node = node
            self.p = start
            self.end = end
            self.last = last
            self.buffer_size = buffer_size
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self.p == self.last:
                raise StopIteration

            result = ('[%d]' % self.count, self.p.dereference())
            self.count = self.count + 1

            # Advance the 'cur' pointer.
            self.p = self.p + 1
            if self.p == self.end:
                # If we got to the end of this bucket, move to the
                # next bucket.
                self.node = self.node + 1
                self.p = self.node[0]
                self.end = self.p + self.buffer_size

            return result

    def __init__(self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val
        self.elttype = val.type.template_argument(0)
        size = self.elttype.sizeof
        if size < 512:
            self.buffer_size = int (512 / size)
        else:
            self.buffer_size = 1

    def to_string(self):
        start = self.val['_M_impl']['_M_start']
        end = self.val['_M_impl']['_M_finish']

        delta_n = end['_M_node'] - start['_M_node'] - 1
        delta_s = start['_M_last'] - start['_M_cur']
        delta_e = end['_M_cur'] - end['_M_first']

        size = self.buffer_size * delta_n + delta_s + delta_e

        return '%s with %s' % (self.typename, num_elements(long(size)))

    def children(self):
        start = self.val['_M_impl']['_M_start']
        end = self.val['_M_impl']['_M_finish']
        return self._iter(start['_M_node'], start['_M_cur'], start['_M_last'],
                          end['_M_cur'], self.buffer_size)

    def display_hint (self):
        return 'array'

class StdDequeIteratorPrinter:
    "Print std::deque::iterator"

    def __init__(self, typename, val):
        self.val = val

    def to_string(self):
        if not self.val['_M_cur']:
            return 'non-dereferenceable iterator for std::deque'
        return str(self.val['_M_cur'].dereference())

class StdStringPrinter:
    "Print a std::basic_string of some kind"

    def __init__(self, typename, val):
        self.val = val
        self.new_string = typename.find("::__cxx11::basic_string") != -1

    def to_string(self):
        # Make sure &string works, too.
        type = self.val.type
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target ()

        # Calculate the length of the string so that to_string returns
        # the string according to length, not according to first null
        # encountered.
        ptr = self.val ['_M_dataplus']['_M_p']
        if self.new_string:
            length = self.val['_M_string_length']
            # https://sourceware.org/bugzilla/show_bug.cgi?id=17728
            ptr = ptr.cast(ptr.type.strip_typedefs())
        else:
            realtype = type.unqualified ().strip_typedefs ()
            reptype = gdb.lookup_type (str (realtype) + '::_Rep').pointer ()
            header = ptr.cast(reptype) - 1
            length = header.dereference ()['_M_length']
        if hasattr(ptr, "lazy_string"):
            return ptr.lazy_string (length = length)
        return ptr.string (length = length)

    def display_hint (self):
        return 'string'

class Tr1HashtableIterator(Iterator):
    def __init__ (self, hash):
        self.buckets = hash['_M_buckets']
        self.bucket = 0
        self.bucket_count = hash['_M_bucket_count']
        self.node_type = find_type(hash.type, '_Node').pointer()
        self.node = 0
        while self.bucket != self.bucket_count:
            self.node = self.buckets[self.bucket]
            if self.node:
                break
            self.bucket = self.bucket + 1        

    def __iter__ (self):
        return self

    def __next__ (self):
        if self.node == 0:
            raise StopIteration
        node = self.node.cast(self.node_type)
        result = node.dereference()['_M_v']
        self.node = node.dereference()['_M_next'];
        if self.node == 0:
            self.bucket = self.bucket + 1
            while self.bucket != self.bucket_count:
                self.node = self.buckets[self.bucket]
                if self.node:
                    break
                self.bucket = self.bucket + 1
        return result

class StdHashtableIterator(Iterator):
    def __init__(self, hash):
        self.node = hash['_M_before_begin']['_M_nxt']
        self.node_type = find_type(hash.type, '__node_type').pointer()

    def __iter__(self):
        return self

    def __next__(self):
        if self.node == 0:
            raise StopIteration
        elt = self.node.cast(self.node_type).dereference()
        self.node = elt['_M_nxt']
        valptr = elt['_M_storage'].address
        valptr = valptr.cast(elt.type.template_argument(0).pointer())
        return valptr.dereference()

class Tr1UnorderedSetPrinter:
    "Print a tr1::unordered_set"

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val

    def hashtable (self):
        if self.typename.startswith('std::tr1'):
            return self.val
        return self.val['_M_h']

    def to_string (self):
        count = self.hashtable()['_M_element_count']
        return '%s with %s' % (self.typename, num_elements(count))

    @staticmethod
    def format_count (i):
        return '[%d]' % i

    def children (self):
        counter = imap (self.format_count, itertools.count())
        if self.typename.startswith('std::tr1'):
            return izip (counter, Tr1HashtableIterator (self.hashtable()))
        return izip (counter, StdHashtableIterator (self.hashtable()))

class Tr1UnorderedMapPrinter:
    "Print a tr1::unordered_map"

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.val = val

    def hashtable (self):
        if self.typename.startswith('std::tr1'):
            return self.val
        return self.val['_M_h']

    def to_string (self):
        count = self.hashtable()['_M_element_count']
        return '%s with %s' % (self.typename, num_elements(count))

    @staticmethod
    def flatten (list):
        for elt in list:
            for i in elt:
                yield i

    @staticmethod
    def format_one (elt):
        return (elt['first'], elt['second'])

    @staticmethod
    def format_count (i):
        return '[%d]' % i

    def children (self):
        counter = imap (self.format_count, itertools.count())
        # Map over the hash table and flatten the result.
        if self.typename.startswith('std::tr1'):
            data = self.flatten (imap (self.format_one, Tr1HashtableIterator (self.hashtable())))
            # Zip the two iterators together.
            return izip (counter, data)
        data = self.flatten (imap (self.format_one, StdHashtableIterator (self.hashtable())))
        # Zip the two iterators together.
        return izip (counter, data)
        

    def display_hint (self):
        return 'map'

class StdForwardListPrinter:
    "Print a std::forward_list"

    class _iterator(Iterator):
        def __init__(self, nodetype, head):
            self.nodetype = nodetype
            self.base = head['_M_next']
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self.base == 0:
                raise StopIteration
            elt = self.base.cast(self.nodetype).dereference()
            self.base = elt['_M_next']
            count = self.count
            self.count = self.count + 1
            valptr = elt['_M_storage'].address
            valptr = valptr.cast(elt.type.template_argument(0).pointer())
            return ('[%d]' % count, valptr.dereference())

    def __init__(self, typename, val):
        self.val = val
        self.typename = strip_versioned_namespace(typename)

    def children(self):
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        return self._iterator(nodetype, self.val['_M_impl']['_M_head'])

    def to_string(self):
        if self.val['_M_impl']['_M_head']['_M_next'] == 0:
            return 'empty %s' % self.typename
        return '%s' % self.typename

class SingleObjContainerPrinter(object):
    "Base class for printers of containers of single objects"

    def __init__ (self, val, viz, hint = None):
        self.contained_value = val
        self.visualizer = viz
        self.hint = hint

    def _recognize(self, type):
        """Return TYPE as a string after applying type printers"""
        global _use_type_printing
        if not _use_type_printing:
            return str(type)
        return gdb.types.apply_type_recognizers(gdb.types.get_type_recognizers(),
                                                type) or str(type)

    class _contained(Iterator):
        def __init__ (self, val):
            self.val = val

        def __iter__ (self):
            return self

        def __next__(self):
            if self.val is None:
                raise StopIteration
            retval = self.val
            self.val = None
            return ('[contained value]', retval)

    def children (self):
        if self.contained_value is None:
            return self._contained (None)
        if hasattr (self.visualizer, 'children'):
            return self.visualizer.children ()
        return self._contained (self.contained_value)

    def display_hint (self):
        # if contained value is a map we want to display in the same way
        if hasattr (self.visualizer, 'children') and hasattr (self.visualizer, 'display_hint'):
            return self.visualizer.display_hint ()
        return self.hint

class StdExpAnyPrinter(SingleObjContainerPrinter):
    "Print a std::any or std::experimental::any"

    def __init__ (self, typename, val):
        self.typename = strip_versioned_namespace(typename)
        self.typename = re.sub('^std::experimental::fundamentals_v\d::', 'std::experimental::', self.typename, 1)
        self.val = val
        self.contained_type = None
        contained_value = None
        visualizer = None
        mgr = self.val['_M_manager']
        if mgr != 0:
            func = gdb.block_for_pc(int(mgr.cast(gdb.lookup_type('intptr_t'))))
            if not func:
                raise ValueError("Invalid function pointer in %s" % self.typename)
            rx = r"""({0}::_Manager_\w+<.*>)::_S_manage\({0}::_Op, {0} const\*, {0}::_Arg\*\)""".format(typename)
            m = re.match(rx, func.function.name)
            if not m:
                raise ValueError("Unknown manager function in %s" % self.typename)

            mgrname = m.group(1)
            # FIXME need to expand 'std::string' so that gdb.lookup_type works
            if 'std::string' in mgrname:
                mgrname = re.sub("std::string(?!\w)", str(gdb.lookup_type('std::string').strip_typedefs()), m.group(1))

            mgrtype = gdb.lookup_type(mgrname)
            self.contained_type = mgrtype.template_argument(0)
            valptr = None
            if '::_Manager_internal' in mgrname:
                valptr = self.val['_M_storage']['_M_buffer'].address
            elif '::_Manager_external' in mgrname:
                valptr = self.val['_M_storage']['_M_ptr']
            else:
                raise ValueError("Unknown manager function in %s" % self.typename)
            contained_value = valptr.cast(self.contained_type.pointer()).dereference()
            visualizer = gdb.default_visualizer(contained_value)
        super(StdExpAnyPrinter, self).__init__ (contained_value, visualizer)

    def to_string (self):
        if self.contained_type is None:
            return '%s [no contained value]' % self.typename
        desc = "%s containing " % self.typename
        if hasattr (self.visualizer, 'children'):
            return desc + self.visualizer.to_string ()
        valtype = self._recognize (self.contained_type)
        return desc + strip_versioned_namespace(str(valtype))

class StdExpOptionalPrinter(SingleObjContainerPrinter):
    "Print a std::optional or std::experimental::optional"

    def __init__ (self, typename, val):
        valtype = self._recognize (val.type.template_argument(0))
        self.typename = strip_versioned_namespace(typename)
        self.typename = re.sub('^std::(experimental::|)(fundamentals_v\d::|)(.*)', r'std::\1\3<%s>' % valtype, self.typename, 1)
        if not self.typename.startswith('std::experimental'):
            val = val['_M_payload']
        self.val = val
        contained_value = val['_M_payload'] if self.val['_M_engaged'] else None
        visualizer = gdb.default_visualizer (val['_M_payload'])
        super (StdExpOptionalPrinter, self).__init__ (contained_value, visualizer)

    def to_string (self):
        if self.contained_value is None:
            return "%s [no contained value]" % self.typename
        if hasattr (self.visualizer, 'children'):
            return "%s containing %s" % (self.typename,
                                         self.visualizer.to_string())
        return self.typename

class StdVariantPrinter(SingleObjContainerPrinter):
    "Print a std::variant"

    def __init__(self, typename, val):
        alternatives = get_template_arg_list(val.type)
        self.typename = strip_versioned_namespace(typename)
        self.typename = "%s<%s>" % (self.typename, ', '.join([self._recognize(alt) for alt in alternatives]))
        self.index = val['_M_index']
        if self.index >= len(alternatives):
            self.contained_type = None
            contained_value = None
            visualizer = None
        else:
            self.contained_type = alternatives[int(self.index)]
            addr = val['_M_u']['_M_first']['_M_storage'].address
            contained_value = addr.cast(self.contained_type.pointer()).dereference()
            visualizer = gdb.default_visualizer(contained_value)
        super (StdVariantPrinter, self).__init__(contained_value, visualizer, 'array')

    def to_string(self):
        if self.contained_value is None:
            return "%s [no contained value]" % self.typename
        if hasattr(self.visualizer, 'children'):
            return "%s [index %d] containing %s" % (self.typename, self.index,
                                                    self.visualizer.to_string())
        return "%s [index %d]" % (self.typename, self.index)

class StdNodeHandlePrinter(SingleObjContainerPrinter):
    "Print a container node handle"

    def __init__(self, typename, val):
        self.value_type = val.type.template_argument(1)
        nodetype = val.type.template_argument(2).template_argument(0)
        self.is_rb_tree_node = is_specialization_of(nodetype.name, '_Rb_tree_node')
        self.is_map_node = val.type.template_argument(0) != self.value_type
        nodeptr = val['_M_ptr']
        if nodeptr:
            if self.is_rb_tree_node:
                contained_value = get_value_from_Rb_tree_node(nodeptr.dereference())
            else:
                contained_value = get_value_from_aligned_membuf(nodeptr['_M_storage'],
                                                                self.value_type)
            visualizer = gdb.default_visualizer(contained_value)
        else:
            contained_value = None
            visualizer = None
        optalloc = val['_M_alloc']
        self.alloc = optalloc['_M_payload'] if optalloc['_M_engaged'] else None
        super(StdNodeHandlePrinter, self).__init__(contained_value, visualizer,
                                                   'array')

    def to_string(self):
        desc = 'node handle for '
        if not self.is_rb_tree_node:
            desc += 'unordered '
        if self.is_map_node:
            desc += 'map';
        else:
            desc += 'set';

        if self.contained_value:
            desc += ' with element'
            if hasattr(self.visualizer, 'children'):
                return "%s = %s" % (desc, self.visualizer.to_string())
            return desc
        else:
            return 'empty %s' % desc

class StdExpStringViewPrinter:
    "Print a std::basic_string_view or std::experimental::basic_string_view"

    def __init__ (self, typename, val):
        self.val = val

    def to_string (self):
        ptr = self.val['_M_str']
        len = self.val['_M_len']
        if hasattr (ptr, "lazy_string"):
            return ptr.lazy_string (length = len)
        return ptr.string (length = len)

    def display_hint (self):
        return 'string'

class StdExpPathPrinter:
    "Print a std::experimental::filesystem::path"

    def __init__ (self, typename, val):
        self.val = val
        start = self.val['_M_cmpts']['_M_impl']['_M_start']
        finish = self.val['_M_cmpts']['_M_impl']['_M_finish']
        self.num_cmpts = int (finish - start)

    def _path_type(self):
        t = str(self.val['_M_type'])
        if t[-9:] == '_Root_dir':
            return "root-directory"
        if t[-10:] == '_Root_name':
            return "root-name"
        return None

    def to_string (self):
        path = "%s" % self.val ['_M_pathname']
        if self.num_cmpts == 0:
            t = self._path_type()
            if t:
                path = '%s [%s]' % (path, t)
        return "filesystem::path %s" % path

    class _iterator(Iterator):
        def __init__(self, cmpts):
            self.item = cmpts['_M_impl']['_M_start']
            self.finish = cmpts['_M_impl']['_M_finish']
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self.item == self.finish:
                raise StopIteration
            item = self.item.dereference()
            count = self.count
            self.count = self.count + 1
            self.item = self.item + 1
            path = item['_M_pathname']
            t = StdExpPathPrinter(item.type.name, item)._path_type()
            if not t:
                t = count
            return ('[%s]' % t, path)

    def children(self):
        return self._iterator(self.val['_M_cmpts'])


# A "regular expression" printer which conforms to the
# "SubPrettyPrinter" protocol from gdb.printing.
class RxPrinter(object):
    def __init__(self, name, function):
        super(RxPrinter, self).__init__()
        self.name = name
        self.function = function
        self.enabled = True

    def invoke(self, value):
        if not self.enabled:
            return None

        if value.type.code == gdb.TYPE_CODE_REF:
            if hasattr(gdb.Value,"referenced_value"):
                value = value.referenced_value()

        return self.function(self.name, value)

# A pretty-printer that conforms to the "PrettyPrinter" protocol from
# gdb.printing.  It can also be used directly as an old-style printer.
class Printer(object):
    def __init__(self, name):
        super(Printer, self).__init__()
        self.name = name
        self.subprinters = []
        self.lookup = {}
        self.enabled = True
        self.compiled_rx = re.compile('^([a-zA-Z0-9_:]+)(<.*>)?$')

    def add(self, name, function):
        # A small sanity check.
        # FIXME
        if not self.compiled_rx.match(name):
            raise ValueError('libstdc++ programming error: "%s" does not match' % name)
        printer = RxPrinter(name, function)
        self.subprinters.append(printer)
        self.lookup[name] = printer

    # Add a name using _GLIBCXX_BEGIN_NAMESPACE_VERSION.
    def add_version(self, base, name, function):
        self.add(base + name, function)
        if _versioned_namespace:
            vbase = re.sub('^(std|__gnu_cxx)::', r'\g<0>%s' % _versioned_namespace, base)
            self.add(vbase + name, function)

    # Add a name using _GLIBCXX_BEGIN_NAMESPACE_CONTAINER.
    def add_container(self, base, name, function):
        self.add_version(base, name, function)
        self.add_version(base + '__cxx1998::', name, function)

    @staticmethod
    def get_basic_type(type):
        # If it points to a reference, get the reference.
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target ()

        # Get the unqualified type, stripped of typedefs.
        type = type.unqualified ().strip_typedefs ()

        return type.tag

    def __call__(self, val):
        typename = self.get_basic_type(val.type)
        if not typename:
            return None

        # All the types we match are template types, so we can use a
        # dictionary.
        match = self.compiled_rx.match(typename)
        if not match:
            return None

        basename = match.group(1)

        if val.type.code == gdb.TYPE_CODE_REF:
            if hasattr(gdb.Value,"referenced_value"):
                val = val.referenced_value()

        if basename in self.lookup:
            return self.lookup[basename].invoke(val)

        # Cannot find a pretty printer.  Return None.
        return None

libstdcxx_printer = None

class TemplateTypePrinter(object):
    r"""
    A type printer for class templates with default template arguments.

    Recognizes specializations of class templates and prints them without
    any template arguments that use a default template argument.
    Type printers are recursively applied to the template arguments.

    e.g. replace "std::vector<T, std::allocator<T> >" with "std::vector<T>".
    """

    def __init__(self, name, defargs):
        self.name = name
        self.defargs = defargs
        self.enabled = True

    class _recognizer(object):
        "The recognizer class for TemplateTypePrinter."

        def __init__(self, name, defargs):
            self.name = name
            self.defargs = defargs
            # self.type_obj = None

        def recognize(self, type_obj):
            """
            If type_obj is a specialization of self.name that uses all the
            default template arguments for the class template, then return
            a string representation of the type without default arguments.
            Otherwise, return None.
            """

            if type_obj.tag is None:
                return None

            if not type_obj.tag.startswith(self.name):
                return None

            template_args = get_template_arg_list(type_obj)
            displayed_args = []
            require_defaulted = False
            for n in range(len(template_args)):
                # The actual template argument in the type:
                targ = template_args[n]
                # The default template argument for the class template:
                defarg = self.defargs.get(n)
                if defarg is not None:
                    # Substitute other template arguments into the default:
                    defarg = defarg.format(*template_args)
                    # Fail to recognize the type (by returning None)
                    # unless the actual argument is the same as the default.
                    try:
                        if targ != gdb.lookup_type(defarg):
                            return None
                    except gdb.error:
                        # Type lookup failed, just use string comparison:
                        if targ.tag != defarg:
                            return None
                    # All subsequent args must have defaults:
                    require_defaulted = True
                elif require_defaulted:
                    return None
                else:
                    # Recursively apply recognizers to the template argument
                    # and add it to the arguments that will be displayed:
                    displayed_args.append(self._recognize_subtype(targ))

            # This assumes no class templates in the nested-name-specifier:
            template_name = type_obj.tag[0:type_obj.tag.find('<')]
            template_name = strip_inline_namespaces(template_name)

            return template_name + '<' + ', '.join(displayed_args) + '>'

        def _recognize_subtype(self, type_obj):
            """Convert a gdb.Type to a string by applying recognizers,
            or if that fails then simply converting to a string."""

            if type_obj.code == gdb.TYPE_CODE_PTR:
                return self._recognize_subtype(type_obj.target()) + '*'
            if type_obj.code == gdb.TYPE_CODE_ARRAY:
                type_str = self._recognize_subtype(type_obj.target())
                if str(type_obj.strip_typedefs()).endswith('[]'):
                    return type_str + '[]' # array of unknown bound
                return "%s[%d]" % (type_str, type_obj.range()[1] + 1)
            if type_obj.code == gdb.TYPE_CODE_REF:
                return self._recognize_subtype(type_obj.target()) + '&'
            if hasattr(gdb, 'TYPE_CODE_RVALUE_REF'):
                if type_obj.code == gdb.TYPE_CODE_RVALUE_REF:
                    return self._recognize_subtype(type_obj.target()) + '&&'

            type_str = gdb.types.apply_type_recognizers(
                    gdb.types.get_type_recognizers(), type_obj)
            if type_str:
                return type_str
            return str(type_obj)

    def instantiate(self):
        "Return a recognizer object for this type printer."
        return self._recognizer(self.name, self.defargs)

def add_one_template_type_printer(obj, name, defargs):
    r"""
    Add a type printer for a class template with default template arguments.

    Args:
        name (str): The template-name of the class template.
        defargs (dict int:string) The default template arguments.

    Types in defargs can refer to the Nth template-argument using {N}
    (with zero-based indices).

    e.g. 'unordered_map' has these defargs:
    { 2: 'std::hash<{0}>',
      3: 'std::equal_to<{0}>',
      4: 'std::allocator<std::pair<const {0}, {1}> >' }

    """
    printer = TemplateTypePrinter('std::'+name, defargs)
    gdb.types.register_type_printer(obj, printer)
    if _versioned_namespace:
        # Add second type printer for same type in versioned namespace:
        ns = 'std::' + _versioned_namespace
        # PR 86112 Cannot use dict comprehension here:
        defargs = dict((n, d.replace('std::', ns)) for (n,d) in defargs.items())
        printer = TemplateTypePrinter(ns+name, defargs)
        gdb.types.register_type_printer(obj, printer)

class FilteringTypePrinter(object):
    r"""
    A type printer that uses typedef names for common template specializations.

    Args:
        match (str): The class template to recognize.
        name (str): The typedef-name that will be used instead.

    Checks if a specialization of the class template 'match' is the same type
    as the typedef 'name', and prints it as 'name' instead.

    e.g. if an instantiation of std::basic_istream<C, T> is the same type as
    std::istream then print it as std::istream.
    """

    def __init__(self, match, name):
        self.match = match
        self.name = name
        self.enabled = True

    class _recognizer(object):
        "The recognizer class for TemplateTypePrinter."

        def __init__(self, match, name):
            self.match = match
            self.name = name
            self.type_obj = None

        def recognize(self, type_obj):
            """
            If type_obj starts with self.match and is the same type as
            self.name then return self.name, otherwise None.
            """
            if type_obj.tag is None:
                return None

            if self.type_obj is None:
                if not type_obj.tag.startswith(self.match):
                    # Filter didn't match.
                    return None
                try:
                    self.type_obj = gdb.lookup_type(self.name).strip_typedefs()
                except:
                    pass
            if self.type_obj == type_obj:
                return strip_inline_namespaces(self.name)
            return None

    def instantiate(self):
        "Return a recognizer object for this type printer."
        return self._recognizer(self.match, self.name)

def add_one_type_printer(obj, match, name):
    printer = FilteringTypePrinter('std::' + match, 'std::' + name)
    gdb.types.register_type_printer(obj, printer)
    if _versioned_namespace:
        ns = 'std::' + _versioned_namespace
        printer = FilteringTypePrinter(ns + match, ns + name)
        gdb.types.register_type_printer(obj, printer)

def register_type_printers(obj):
    global _use_type_printing

    if not _use_type_printing:
        return

    # Add type printers for typedefs std::string, std::wstring etc.
    for ch in ('', 'w', 'u16', 'u32'):
        add_one_type_printer(obj, 'basic_string', ch + 'string')
        add_one_type_printer(obj, '__cxx11::basic_string',
                             '__cxx11::' + ch + 'string')
        add_one_type_printer(obj, 'basic_string_view', ch + 'string_view')

    # Add type printers for typedefs std::istream, std::wistream etc.
    for ch in ('', 'w'):
        for x in ('ios', 'streambuf', 'istream', 'ostream', 'iostream',
                  'filebuf', 'ifstream', 'ofstream', 'fstream'):
            add_one_type_printer(obj, 'basic_' + x, ch + x)
        for x in ('stringbuf', 'istringstream', 'ostringstream',
                  'stringstream'):
            add_one_type_printer(obj, 'basic_' + x, ch + x)
            # <sstream> types are in __cxx11 namespace, but typedefs aren'x:
            add_one_type_printer(obj, '__cxx11::basic_' + x, ch + x)

    # Add type printers for typedefs regex, wregex, cmatch, wcmatch etc.
    for abi in ('', '__cxx11::'):
        for ch in ('', 'w'):
            add_one_type_printer(obj, abi + 'basic_regex', abi + ch + 'regex')
        for ch in ('c', 's', 'wc', 'ws'):
            add_one_type_printer(obj, abi + 'match_results', abi + ch + 'match')
            for x in ('sub_match', 'regex_iterator', 'regex_token_iterator'):
                add_one_type_printer(obj, abi + x, abi + ch + x)

    # Note that we can't have a printer for std::wstreampos, because
    # it is the same type as std::streampos.
    add_one_type_printer(obj, 'fpos', 'streampos')

    # Add type printers for <chrono> typedefs.
    for dur in ('nanoseconds', 'microseconds', 'milliseconds',
                'seconds', 'minutes', 'hours'):
        add_one_type_printer(obj, 'duration', dur)

    # Add type printers for <random> typedefs.
    add_one_type_printer(obj, 'linear_congruential_engine', 'minstd_rand0')
    add_one_type_printer(obj, 'linear_congruential_engine', 'minstd_rand')
    add_one_type_printer(obj, 'mersenne_twister_engine', 'mt19937')
    add_one_type_printer(obj, 'mersenne_twister_engine', 'mt19937_64')
    add_one_type_printer(obj, 'subtract_with_carry_engine', 'ranlux24_base')
    add_one_type_printer(obj, 'subtract_with_carry_engine', 'ranlux48_base')
    add_one_type_printer(obj, 'discard_block_engine', 'ranlux24')
    add_one_type_printer(obj, 'discard_block_engine', 'ranlux48')
    add_one_type_printer(obj, 'shuffle_order_engine', 'knuth_b')

    # Add type printers for experimental::basic_string_view typedefs.
    ns = 'experimental::fundamentals_v1::'
    for ch in ('', 'w', 'u16', 'u32'):
        add_one_type_printer(obj, ns + 'basic_string_view',
                             ns + ch + 'string_view')

    # Do not show defaulted template arguments in class templates.
    add_one_template_type_printer(obj, 'unique_ptr',
            { 1: 'std::default_delete<{0}>' })
    add_one_template_type_printer(obj, 'deque', { 1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'forward_list', { 1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'list', { 1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, '__cxx11::list', { 1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'vector', { 1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'map',
            { 2: 'std::less<{0}>',
              3: 'std::allocator<std::pair<{0} const, {1}>>' })
    add_one_template_type_printer(obj, 'multimap',
            { 2: 'std::less<{0}>',
              3: 'std::allocator<std::pair<{0} const, {1}>>' })
    add_one_template_type_printer(obj, 'set',
            { 1: 'std::less<{0}>', 2: 'std::allocator<{0}>' })
    add_one_template_type_printer(obj, 'multiset',
            { 1: 'std::less<{0}>', 2: 'std::allocator<{0}>' })
    add_one_template_type_printer(obj, 'unordered_map',
            { 2: 'std::hash<{0}>',
              3: 'std::equal_to<{0}>',
              4: 'std::allocator<std::pair<{0} const, {1}>>'})
    add_one_template_type_printer(obj, 'unordered_multimap',
            { 2: 'std::hash<{0}>',
              3: 'std::equal_to<{0}>',
              4: 'std::allocator<std::pair<{0} const, {1}>>'})
    add_one_template_type_printer(obj, 'unordered_set',
            { 1: 'std::hash<{0}>',
              2: 'std::equal_to<{0}>',
              3: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'unordered_multiset',
            { 1: 'std::hash<{0}>',
              2: 'std::equal_to<{0}>',
              3: 'std::allocator<{0}>'})

def register_libstdcxx_printers (obj):
    "Register libstdc++ pretty-printers with objfile Obj."

    global _use_gdb_pp
    global libstdcxx_printer

    if _use_gdb_pp:
        gdb.printing.register_pretty_printer(obj, libstdcxx_printer)
    else:
        if obj is None:
            obj = gdb
        obj.pretty_printers.append(libstdcxx_printer)

    register_type_printers(obj)

def build_libstdcxx_dictionary ():
    global libstdcxx_printer

    libstdcxx_printer = Printer("libstdc++-v6")

    # libstdc++ objects requiring pretty-printing.
    # In order from:
    # http://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/a01847.html
    libstdcxx_printer.add_version('std::', 'basic_string', StdStringPrinter)
    libstdcxx_printer.add_version('std::__cxx11::', 'basic_string', StdStringPrinter)
    libstdcxx_printer.add_container('std::', 'bitset', StdBitsetPrinter)
    libstdcxx_printer.add_container('std::', 'deque', StdDequePrinter)
    libstdcxx_printer.add_container('std::', 'list', StdListPrinter)
    libstdcxx_printer.add_container('std::__cxx11::', 'list', StdListPrinter)
    libstdcxx_printer.add_container('std::', 'map', StdMapPrinter)
    libstdcxx_printer.add_container('std::', 'multimap', StdMapPrinter)
    libstdcxx_printer.add_container('std::', 'multiset', StdSetPrinter)
    libstdcxx_printer.add_version('std::', 'priority_queue',
                                  StdStackOrQueuePrinter)
    libstdcxx_printer.add_version('std::', 'queue', StdStackOrQueuePrinter)
    libstdcxx_printer.add_version('std::', 'tuple', StdTuplePrinter)
    libstdcxx_printer.add_container('std::', 'set', StdSetPrinter)
    libstdcxx_printer.add_version('std::', 'stack', StdStackOrQueuePrinter)
    libstdcxx_printer.add_version('std::', 'unique_ptr', UniquePointerPrinter)
    libstdcxx_printer.add_container('std::', 'vector', StdVectorPrinter)
    # vector<bool>

    # Printer registrations for classes compiled with -D_GLIBCXX_DEBUG.
    libstdcxx_printer.add('std::__debug::bitset', StdBitsetPrinter)
    libstdcxx_printer.add('std::__debug::deque', StdDequePrinter)
    libstdcxx_printer.add('std::__debug::list', StdListPrinter)
    libstdcxx_printer.add('std::__debug::map', StdMapPrinter)
    libstdcxx_printer.add('std::__debug::multimap', StdMapPrinter)
    libstdcxx_printer.add('std::__debug::multiset', StdSetPrinter)
    libstdcxx_printer.add('std::__debug::priority_queue',
                          StdStackOrQueuePrinter)
    libstdcxx_printer.add('std::__debug::queue', StdStackOrQueuePrinter)
    libstdcxx_printer.add('std::__debug::set', StdSetPrinter)
    libstdcxx_printer.add('std::__debug::stack', StdStackOrQueuePrinter)
    libstdcxx_printer.add('std::__debug::unique_ptr', UniquePointerPrinter)
    libstdcxx_printer.add('std::__debug::vector', StdVectorPrinter)

    # These are the TR1 and C++11 printers.
    # For array - the default GDB pretty-printer seems reasonable.
    libstdcxx_printer.add_version('std::', 'shared_ptr', SharedPointerPrinter)
    libstdcxx_printer.add_version('std::', 'weak_ptr', SharedPointerPrinter)
    libstdcxx_printer.add_container('std::', 'unordered_map',
                                    Tr1UnorderedMapPrinter)
    libstdcxx_printer.add_container('std::', 'unordered_set',
                                    Tr1UnorderedSetPrinter)
    libstdcxx_printer.add_container('std::', 'unordered_multimap',
                                    Tr1UnorderedMapPrinter)
    libstdcxx_printer.add_container('std::', 'unordered_multiset',
                                    Tr1UnorderedSetPrinter)
    libstdcxx_printer.add_container('std::', 'forward_list',
                                    StdForwardListPrinter)

    libstdcxx_printer.add_version('std::tr1::', 'shared_ptr', SharedPointerPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'weak_ptr', SharedPointerPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_map',
                                  Tr1UnorderedMapPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_set',
                                  Tr1UnorderedSetPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_multimap',
                                  Tr1UnorderedMapPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_multiset',
                                  Tr1UnorderedSetPrinter)

    # These are the C++11 printer registrations for -D_GLIBCXX_DEBUG cases.
    # The tr1 namespace containers do not have any debug equivalents,
    # so do not register printers for them.
    libstdcxx_printer.add('std::__debug::unordered_map',
                          Tr1UnorderedMapPrinter)
    libstdcxx_printer.add('std::__debug::unordered_set',
                          Tr1UnorderedSetPrinter)
    libstdcxx_printer.add('std::__debug::unordered_multimap',
                          Tr1UnorderedMapPrinter)
    libstdcxx_printer.add('std::__debug::unordered_multiset',
                          Tr1UnorderedSetPrinter)
    libstdcxx_printer.add('std::__debug::forward_list',
                          StdForwardListPrinter)

    # Library Fundamentals TS components
    libstdcxx_printer.add_version('std::experimental::fundamentals_v1::',
                                  'any', StdExpAnyPrinter)
    libstdcxx_printer.add_version('std::experimental::fundamentals_v1::',
                                  'optional', StdExpOptionalPrinter)
    libstdcxx_printer.add_version('std::experimental::fundamentals_v1::',
                                  'basic_string_view', StdExpStringViewPrinter)
    # Filesystem TS components
    libstdcxx_printer.add_version('std::experimental::filesystem::v1::',
                                  'path', StdExpPathPrinter)
    libstdcxx_printer.add_version('std::experimental::filesystem::v1::__cxx11::',
                                  'path', StdExpPathPrinter)
    libstdcxx_printer.add_version('std::filesystem::',
                                  'path', StdExpPathPrinter)
    libstdcxx_printer.add_version('std::filesystem::__cxx11::',
                                  'path', StdExpPathPrinter)

    # C++17 components
    libstdcxx_printer.add_version('std::',
                                  'any', StdExpAnyPrinter)
    libstdcxx_printer.add_version('std::',
                                  'optional', StdExpOptionalPrinter)
    libstdcxx_printer.add_version('std::',
                                  'basic_string_view', StdExpStringViewPrinter)
    libstdcxx_printer.add_version('std::',
                                  'variant', StdVariantPrinter)
    libstdcxx_printer.add_version('std::',
                                  '_Node_handle', StdNodeHandlePrinter)

    # Extensions.
    libstdcxx_printer.add_version('__gnu_cxx::', 'slist', StdSlistPrinter)

    if True:
        # These shouldn't be necessary, if GDB "print *i" worked.
        # But it often doesn't, so here they are.
        libstdcxx_printer.add_container('std::', '_List_iterator',
                                        StdListIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_List_const_iterator',
                                        StdListIteratorPrinter)
        libstdcxx_printer.add_version('std::', '_Rb_tree_iterator',
                                      StdRbtreeIteratorPrinter)
        libstdcxx_printer.add_version('std::', '_Rb_tree_const_iterator',
                                      StdRbtreeIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_Deque_iterator',
                                        StdDequeIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_Deque_const_iterator',
                                        StdDequeIteratorPrinter)
        libstdcxx_printer.add_version('__gnu_cxx::', '__normal_iterator',
                                      StdVectorIteratorPrinter)
        libstdcxx_printer.add_version('__gnu_cxx::', '_Slist_iterator',
                                      StdSlistIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_Fwd_list_iterator',
                                        StdFwdListIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_Fwd_list_const_iterator',
                                        StdFwdListIteratorPrinter)

        # Debug (compiled with -D_GLIBCXX_DEBUG) printer
        # registrations.
        libstdcxx_printer.add('__gnu_debug::_Safe_iterator',
                              StdDebugIteratorPrinter)

build_libstdcxx_dictionary ()
