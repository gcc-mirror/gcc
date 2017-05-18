# Pretty-printers for libstdc++.

# Copyright (C) 2008-2016 Free Software Foundation, Inc.

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

class SharedPointerPrinter:
    "Print a shared_ptr or weak_ptr"

    def __init__ (self, typename, val):
        self.typename = typename
        self.val = val

    def to_string (self):
        state = 'empty'
        refcounts = self.val['_M_refcount']['_M_pi']
        if refcounts != 0:
            usecount = refcounts['_M_use_count']
            weakcount = refcounts['_M_weak_count']
            if usecount == 0:
                state = 'expired, weak %d' % weakcount
            else:
                state = 'count %d, weak %d' % (usecount, weakcount - 1)
        return '%s (%s) %s' % (self.typename, state, self.val['_M_ptr'])

class UniquePointerPrinter:
    "Print a unique_ptr"

    def __init__ (self, typename, val):
        self.val = val

    def to_string (self):
        v = self.val['_M_t']['_M_head_impl']
        return 'std::unique_ptr<%s> containing %s' % (str(v.type.target()),
                                                      str(v))

def get_value_from_list_node(node):
    """Returns the value held in an _List_node<_Val>"""
    try:
        member = node.type.fields()[1].name
        if member == '_M_data':
            # C++03 implementation, node contains the value as a member
            return node['_M_data']
        elif member == '_M_storage':
            # C++11 implementation, node stores value in __aligned_membuf
            p = node['_M_storage']['_M_storage'].address
            p = p.cast(node.type.template_argument(0).pointer())
            return p.dereference()
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
        self.typename = typename
        self.val = val

    def children(self):
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        return self._iterator(nodetype, self.val['_M_impl']['_M_node'])

    def to_string(self):
        if self.val['_M_impl']['_M_node'].address == self.val['_M_impl']['_M_node']['_M_next']:
            return 'empty %s' % (self.typename)
        return '%s' % (self.typename)

class StdListIteratorPrinter:
    "Print std::list::iterator"

    def __init__(self, typename, val):
        self.val = val
        self.typename = typename

    def to_string(self):
        if not self.val['_M_node']:
            return 'non-dereferenceable iterator for std::list'
        nodetype = find_type(self.val.type, '_Node')
        nodetype = nodetype.strip_typedefs().pointer()
        node = self.val['_M_node'].cast(nodetype).dereference()
        return str(get_value_from_list_node(node))

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
        self.typename = typename
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
        self.typename = typename
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
        self.typename = typename
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
            p = node['_M_storage']['_M_storage'].address
            p = p.cast(node.type.template_argument(0).pointer())
            return p.dereference()
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
        nodetype = gdb.lookup_type('std::_Rb_tree_node<' + str(valtype) + '>')
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
        itype = self.val.type.template_argument(0)
        return str(self.val.cast(itype))

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
        self.typename = typename
        self.val = val

    def to_string (self):
        return '%s with %d elements' % (self.typename,
                                        len (RbtreeIterator (self.val)))

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
        self.typename = typename
        self.val = val

    def to_string (self):
        return '%s with %d elements' % (self.typename,
                                        len (RbtreeIterator (self.val)))

    def children (self):
        rep_type = find_type(self.val.type, '_Rep_type')
        node = find_type(rep_type, '_Link_type')
        node = node.strip_typedefs()
        return self._iter (RbtreeIterator (self.val), node)

class StdBitsetPrinter:
    "Print a std::bitset"

    def __init__(self, typename, val):
        self.typename = typename
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
        self.typename = typename
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

        return '%s with %d elements' % (self.typename, long (size))

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
        self.typename = typename
        self.val = val

    def hashtable (self):
        if self.typename.startswith('std::tr1'):
            return self.val
        return self.val['_M_h']

    def to_string (self):
        return '%s with %d elements' % (self.typename, self.hashtable()['_M_element_count'])

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
        self.typename = typename
        self.val = val

    def hashtable (self):
        if self.typename.startswith('std::tr1'):
            return self.val
        return self.val['_M_h']

    def to_string (self):
        return '%s with %d elements' % (self.typename, self.hashtable()['_M_element_count'])

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
        self.typename = typename

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

    def __init__ (self, val, viz):
        self.contained_value = val
        self.visualizer = viz

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
        return None


class StdExpAnyPrinter(SingleObjContainerPrinter):
    "Print a std::experimental::any"

    def __init__ (self, typename, val):
        self.typename = 'std::experimental::any'
        self.val = val
        self.contained_type = None
        contained_value = None
        visualizer = None
        mgr = self.val['_M_manager']
        if mgr != 0:
            func = gdb.block_for_pc(int(mgr.cast(gdb.lookup_type('intptr_t'))))
            if not func:
                raise ValueError("Invalid function pointer in std::experimental::any")
            rx = r"""({0}::_Manager_\w+<.*>)::_S_manage\({0}::_Op, {0} const\*, {0}::_Arg\*\)""".format(typename)
            m = re.match(rx, func.function.name)
            if not m:
                raise ValueError("Unknown manager function in std::experimental::any")

            # FIXME need to expand 'std::string' so that gdb.lookup_type works
            mgrname = re.sub("std::string(?!\w)", str(gdb.lookup_type('std::string').strip_typedefs()), m.group(1))
            mgrtype = gdb.lookup_type(mgrname)
            self.contained_type = mgrtype.template_argument(0)
            valptr = None
            if '::_Manager_internal' in mgrname:
                valptr = self.val['_M_storage']['_M_buffer'].address
            elif '::_Manager_external' in mgrname:
                valptr = self.val['_M_storage']['_M_ptr']
            else:
                raise ValueError("Unknown manager function in std::experimental::any")
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
        return desc + valtype

class StdExpOptionalPrinter(SingleObjContainerPrinter):
    "Print a std::experimental::optional"

    def __init__ (self, typename, val):
        valtype = self._recognize (val.type.template_argument(0))
        self.typename = "std::experimental::optional<%s>" % valtype
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

class StdExpStringViewPrinter:
    "Print a std::experimental::basic_string_view"

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
        self.add(base + '__7::' + name, function)

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
    A type printer for class templates.

    Recognizes type names that match a regular expression.
    Replaces them with a formatted string which can use replacement field
    {N} to refer to the \N subgroup of the regex match.
    Type printers are recusively applied to the subgroups.

    This allows recognizing e.g. "std::vector<(.*), std::allocator<\\1> >"
    and replacing it with "std::vector<{1}>", omitting the template argument
    that uses the default type.
    """

    def __init__(self, name, pattern, subst):
        self.name = name
        self.pattern = re.compile(pattern)
        self.subst = subst
        self.enabled = True

    class _recognizer(object):
        def __init__(self, pattern, subst):
            self.pattern = pattern
            self.subst = subst
            self.type_obj = None

        def recognize(self, type_obj):
            if type_obj.tag is None:
                return None

            m = self.pattern.match(type_obj.tag)
            if m:
                subs = list(m.groups())
                for i, sub in enumerate(subs):
                    if ('{%d}' % (i+1)) in self.subst:
                        # apply recognizers to subgroup
                        rep = gdb.types.apply_type_recognizers(
                                gdb.types.get_type_recognizers(),
                                gdb.lookup_type(sub))
                        if rep:
                            subs[i] = rep
                subs = [None] + subs
                return self.subst.format(*subs)
            return None

    def instantiate(self):
        return self._recognizer(self.pattern, self.subst)

def add_one_template_type_printer(obj, name, match, subst):
    printer = TemplateTypePrinter(name, '^std::' + match + '$', 'std::' + subst)
    gdb.types.register_type_printer(obj, printer)

class FilteringTypePrinter(object):
    def __init__(self, match, name):
        self.match = match
        self.name = name
        self.enabled = True

    class _recognizer(object):
        def __init__(self, match, name):
            self.match = match
            self.name = name
            self.type_obj = None

        def recognize(self, type_obj):
            if type_obj.tag is None:
                return None

            if self.type_obj is None:
                if not self.match in type_obj.tag:
                    # Filter didn't match.
                    return None
                try:
                    self.type_obj = gdb.lookup_type(self.name).strip_typedefs()
                except:
                    pass
            if self.type_obj == type_obj:
                return self.name
            return None

    def instantiate(self):
        return self._recognizer(self.match, self.name)

def add_one_type_printer(obj, match, name):
    printer = FilteringTypePrinter(match, 'std::' + name)
    gdb.types.register_type_printer(obj, printer)

def register_type_printers(obj):
    global _use_type_printing

    if not _use_type_printing:
        return

    for pfx in ('', 'w'):
        add_one_type_printer(obj, 'basic_string', pfx + 'string')
        add_one_type_printer(obj, 'basic_ios', pfx + 'ios')
        add_one_type_printer(obj, 'basic_streambuf', pfx + 'streambuf')
        add_one_type_printer(obj, 'basic_istream', pfx + 'istream')
        add_one_type_printer(obj, 'basic_ostream', pfx + 'ostream')
        add_one_type_printer(obj, 'basic_iostream', pfx + 'iostream')
        add_one_type_printer(obj, 'basic_stringbuf', pfx + 'stringbuf')
        add_one_type_printer(obj, 'basic_istringstream',
                                 pfx + 'istringstream')
        add_one_type_printer(obj, 'basic_ostringstream',
                                 pfx + 'ostringstream')
        add_one_type_printer(obj, 'basic_stringstream',
                                 pfx + 'stringstream')
        add_one_type_printer(obj, 'basic_filebuf', pfx + 'filebuf')
        add_one_type_printer(obj, 'basic_ifstream', pfx + 'ifstream')
        add_one_type_printer(obj, 'basic_ofstream', pfx + 'ofstream')
        add_one_type_printer(obj, 'basic_fstream', pfx + 'fstream')
        add_one_type_printer(obj, 'basic_regex', pfx + 'regex')
        add_one_type_printer(obj, 'sub_match', pfx + 'csub_match')
        add_one_type_printer(obj, 'sub_match', pfx + 'ssub_match')
        add_one_type_printer(obj, 'match_results', pfx + 'cmatch')
        add_one_type_printer(obj, 'match_results', pfx + 'smatch')
        add_one_type_printer(obj, 'regex_iterator', pfx + 'cregex_iterator')
        add_one_type_printer(obj, 'regex_iterator', pfx + 'sregex_iterator')
        add_one_type_printer(obj, 'regex_token_iterator',
                                 pfx + 'cregex_token_iterator')
        add_one_type_printer(obj, 'regex_token_iterator',
                                 pfx + 'sregex_token_iterator')

    # Note that we can't have a printer for std::wstreampos, because
    # it shares the same underlying type as std::streampos.
    add_one_type_printer(obj, 'fpos', 'streampos')
    add_one_type_printer(obj, 'basic_string', 'u16string')
    add_one_type_printer(obj, 'basic_string', 'u32string')

    for dur in ('nanoseconds', 'microseconds', 'milliseconds',
                'seconds', 'minutes', 'hours'):
        add_one_type_printer(obj, 'duration', dur)

    add_one_type_printer(obj, 'linear_congruential_engine', 'minstd_rand0')
    add_one_type_printer(obj, 'linear_congruential_engine', 'minstd_rand')
    add_one_type_printer(obj, 'mersenne_twister_engine', 'mt19937')
    add_one_type_printer(obj, 'mersenne_twister_engine', 'mt19937_64')
    add_one_type_printer(obj, 'subtract_with_carry_engine', 'ranlux24_base')
    add_one_type_printer(obj, 'subtract_with_carry_engine', 'ranlux48_base')
    add_one_type_printer(obj, 'discard_block_engine', 'ranlux24')
    add_one_type_printer(obj, 'discard_block_engine', 'ranlux48')
    add_one_type_printer(obj, 'shuffle_order_engine', 'knuth_b')

    # Do not show defaulted template arguments in class templates
    add_one_template_type_printer(obj, 'unique_ptr<T>',
            'unique_ptr<(.*), std::default_delete<\\1 ?> >',
            'unique_ptr<{1}>')

    add_one_template_type_printer(obj, 'deque<T>',
            'deque<(.*), std::allocator<\\1 ?> >',
            'deque<{1}>')
    add_one_template_type_printer(obj, 'forward_list<T>',
            'forward_list<(.*), std::allocator<\\1 ?> >',
            'forward_list<{1}>')
    add_one_template_type_printer(obj, 'list<T>',
            'list<(.*), std::allocator<\\1 ?> >',
            'list<{1}>')
    add_one_template_type_printer(obj, 'vector<T>',
            'vector<(.*), std::allocator<\\1 ?> >',
            'vector<{1}>')
    add_one_template_type_printer(obj, 'map<Key, T>',
            'map<(.*), (.*), std::less<\\1 ?>, std::allocator<std::pair<\\1 const, \\2 ?> > >',
            'map<{1}, {2}>')
    add_one_template_type_printer(obj, 'multimap<Key, T>',
            'multimap<(.*), (.*), std::less<\\1 ?>, std::allocator<std::pair<\\1 const, \\2 ?> > >',
            'multimap<{1}, {2}>')
    add_one_template_type_printer(obj, 'set<T>',
            'set<(.*), std::less<\\1 ?>, std::allocator<\\1 ?> >',
            'set<{1}>')
    add_one_template_type_printer(obj, 'multiset<T>',
            'multiset<(.*), std::less<\\1 ?>, std::allocator<\\1 ?> >',
            'multiset<{1}>')
    add_one_template_type_printer(obj, 'unordered_map<Key, T>',
            'unordered_map<(.*), (.*), std::hash<\\1 ?>, std::equal_to<\\1 ?>, std::allocator<std::pair<\\1 const, \\2 ?> > >',
            'unordered_map<{1}, {2}>')
    add_one_template_type_printer(obj, 'unordered_multimap<Key, T>',
            'unordered_multimap<(.*), (.*), std::hash<\\1 ?>, std::equal_to<\\1 ?>, std::allocator<std::pair<\\1 const, \\2 ?> > >',
            'unordered_multimap<{1}, {2}>')
    add_one_template_type_printer(obj, 'unordered_set<T>',
            'unordered_set<(.*), std::hash<\\1 ?>, std::equal_to<\\1 ?>, std::allocator<\\1 ?> >',
            'unordered_set<{1}>')
    add_one_template_type_printer(obj, 'unordered_multiset<T>',
            'unordered_multiset<(.*), std::hash<\\1 ?>, std::equal_to<\\1 ?>, std::allocator<\\1 ?> >',
            'unordered_multiset<{1}>')

    # strip the "fundamentals_v1" inline namespace from these types
    add_one_template_type_printer(obj, 'optional<T>',
            'experimental::fundamentals_v1::optional<(.*)>',
            'experimental::optional<\\1>')
    add_one_template_type_printer(obj, 'basic_string_view<C>',
            'experimental::fundamentals_v1::basic_string_view<(.*), std::char_traits<\\1> >',
            'experimental::basic_string_view<\\1>')

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

    # For _GLIBCXX_BEGIN_NAMESPACE_VERSION.
    vers = '(__7::)?'
    # For _GLIBCXX_BEGIN_NAMESPACE_CONTAINER.
    container = '(__cxx1998::' + vers + ')?'

    # libstdc++ objects requiring pretty-printing.
    # In order from:
    # http://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/a01847.html
    libstdcxx_printer.add_version('std::', 'basic_string', StdStringPrinter)
    libstdcxx_printer.add_version('std::', '__cxx11::basic_string', StdStringPrinter)
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

    # These are the TR1 and C++0x printers.
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

    # These are the C++0x printer registrations for -D_GLIBCXX_DEBUG cases.
    # The tr1 namespace printers do not seem to have any debug
    # equivalents, so do no register them.
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

        # Debug (compiled with -D_GLIBCXX_DEBUG) printer
        # registrations.  The Rb_tree debug iterator when unwrapped
        # from the encapsulating __gnu_debug::_Safe_iterator does not
        # have the __norm namespace. Just use the existing printer
        # registration for that.
        libstdcxx_printer.add('__gnu_debug::_Safe_iterator',
                              StdDebugIteratorPrinter)
        libstdcxx_printer.add('std::__norm::_List_iterator',
                              StdListIteratorPrinter)
        libstdcxx_printer.add('std::__norm::_List_const_iterator',
                              StdListIteratorPrinter)
        libstdcxx_printer.add('std::__norm::_Deque_const_iterator',
                              StdDequeIteratorPrinter)
        libstdcxx_printer.add('std::__norm::_Deque_iterator',
                              StdDequeIteratorPrinter)

build_libstdcxx_dictionary ()
