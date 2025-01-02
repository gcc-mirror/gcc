# Pretty-printers for libstdc++.

# Copyright (C) 2008-2024 Free Software Foundation, Inc.

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
import errno
import datetime

# Python 2 + Python 3 compatibility code

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
    # Python 3 stuff
    Iterator = object
    # Python 3 folds these into the normal functions.
    imap = map
    izip = zip
    # Also, int subsumes long
    long = int
    _utc_timezone = datetime.timezone.utc
else:
    # Python 2 stuff
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

    # Python 2 does not provide the datetime.UTC singleton.
    class UTC(datetime.tzinfo):
        """Concrete tzinfo class representing the UTC time zone."""

        def utcoffset(self, dt):
            return datetime.timedelta(0)

        def tzname(self, dt):
            return "UTC"

        def dst(self, dt):
            return datetime.timedelta(0)
    _utc_timezone = UTC()

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

# Use the base class if available.
if hasattr(gdb, 'ValuePrinter'):
    printer_base = gdb.ValuePrinter
else:
    printer_base = object

# Starting with the type ORIG, search for the member type NAME.  This
# handles searching upward through superclasses.  This is needed to
# work around http://sourceware.org/bugzilla/show_bug.cgi?id=13615.


def find_type(orig, name):
    typ = orig.strip_typedefs()
    while True:
        # Use Type.tag to ignore cv-qualifiers.  PR 67440.
        search = '%s::%s' % (typ.tag, name)
        try:
            return gdb.lookup_type(search)
        except RuntimeError:
            pass
        # The type was not found, so try the superclass.  We only need
        # to check the first superclass, so we don't bother with
        # anything fancier here.
        fields = typ.fields()
        if len(fields) and fields[0].is_base_class:
            typ = fields[0].type
        else:
            raise ValueError("Cannot find type %s::%s" % (str(orig), name))


_versioned_namespace = '__8::'


def lookup_templ_spec(templ, *args):
    """
    Lookup template specialization templ<args...>.
    """
    t = '{}<{}>'.format(templ, ', '.join([str(a) for a in args]))
    try:
        return gdb.lookup_type(t)
    except gdb.error as e:
        # Type not found, try again in versioned namespace.
        global _versioned_namespace
        if _versioned_namespace not in templ:
            t = t.replace('::', '::' + _versioned_namespace, 1)
            try:
                return gdb.lookup_type(t)
            except gdb.error:
                # If that also fails, rethrow the original exception
                pass
        raise e

# Use this to find container node types instead of find_type,
# see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=91997 for details.
def lookup_node_type(nodename, containertype):
    """
    Lookup specialization of template nodename corresponding to containertype.

    nodename - The name of a class template, as a String
    containertype - The container, as a gdb.Type

    Return a gdb.Type for the corresponding specialization of nodename,
    or None if the type cannot be found.

    e.g. lookup_node_type('_List_node', gdb.lookup_type('std::list<int>'))
    will return a gdb.Type for the type std::_List_node<int>.
    """
    # If nodename is unqualified, assume it's in namespace std.
    if '::' not in nodename:
        nodename = 'std::' + nodename
    # Use either containertype's value_type or its first template argument.
    try:
        valtype = find_type(containertype, 'value_type')
    except:
        valtype = containertype.template_argument(0)
    valtype = valtype.strip_typedefs()
    try:
        return lookup_templ_spec(nodename, valtype)
    except gdb.error:
        # For debug mode containers the node is in std::__cxx1998.
        if is_member_of_namespace(nodename, 'std'):
            if is_member_of_namespace(containertype, 'std::__cxx1998',
                                      'std::__debug', '__gnu_debug'):
                nodename = nodename.replace('::', '::__cxx1998::', 1)
                try:
                    return lookup_templ_spec(nodename, valtype)
                except gdb.error:
                    pass
        return None


def is_member_of_namespace(typ, *namespaces):
    """
    Test whether a type is a member of one of the specified namespaces.
    The type can be specified as a string or a gdb.Type object.
    """
    if isinstance(typ, gdb.Type):
        typ = str(typ)
    typ = strip_versioned_namespace(typ)
    for namespace in namespaces:
        if typ.startswith(namespace + '::'):
            return True
    return False


def is_specialization_of(x, template_name):
    """
    Test whether a type is a specialization of the named class template.
    The type can be specified as a string or a gdb.Type object.
    The template should be the name of a class template as a string,
    without any 'std' qualification.
    """
    global _versioned_namespace
    if isinstance(x, gdb.Type):
        x = x.tag
    template_name = '(%s)?%s' % (_versioned_namespace, template_name)
    return re.match('^std::%s<.*>$' % template_name, x) is not None


def strip_versioned_namespace(typename):
    global _versioned_namespace
    return typename.replace(_versioned_namespace, '')


def strip_fundts_namespace(typ):
    """Remove "fundamentals_vN" inline namespace from qualified type name."""
    pattern = r'^std::experimental::fundamentals_v\d::'
    repl = 'std::experimental::'
    if sys.version_info[0] == 2:
        return re.sub(pattern, repl, typ, 1)
    else: # Technically this needs Python 3.1 but nobody should be using 3.0
        return re.sub(pattern, repl, typ, count=1)


def strip_inline_namespaces(type_str):
    """Remove known inline namespaces from the canonical name of a type."""
    type_str = strip_versioned_namespace(type_str)
    type_str = type_str.replace('std::__cxx11::', 'std::')
    expt_ns = 'std::experimental::'
    for lfts_ns in ('fundamentals_v1', 'fundamentals_v2'):
        type_str = type_str.replace(expt_ns + lfts_ns + '::', expt_ns)
    fs_ns = expt_ns + 'filesystem::'
    type_str = type_str.replace(fs_ns + 'v1::', fs_ns)
    return type_str


def get_template_arg_list(type_obj):
    """Return a type's template arguments as a list."""
    n = 0
    template_args = []
    while True:
        try:
            template_args.append(type_obj.template_argument(n))
        except:
            return template_args
        n += 1


class SmartPtrIterator(Iterator):
    """An iterator for smart pointer types with a single 'child' value."""

    def __init__(self, val):
        self._val = val

    def __iter__(self):
        return self

    def __next__(self):
        if self._val is None:
            raise StopIteration
        self._val, val = None, self._val
        return ('get()', val)


class SharedPointerPrinter(printer_base):
    """
    Print a shared_ptr, weak_ptr, atomic<shared_ptr>, or atomic<weak_ptr>.
    """

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val
        self._pointer = val['_M_ptr']

    def children(self):
        return SmartPtrIterator(self._pointer)

    # Return the _Sp_counted_base<>* that holds the refcounts.
    def _get_refcounts(self):
        if self._typename == 'std::atomic':
            # A tagged pointer is stored as uintptr_t.
            ptr_val = self._val['_M_refcount']['_M_val']['_M_i']
            ptr_val = ptr_val - (ptr_val % 2)  # clear lock bit
            ptr_type = find_type(self._val['_M_refcount'].type, 'pointer')
            return ptr_val.cast(ptr_type)
        return self._val['_M_refcount']['_M_pi']

    def to_string(self):
        state = 'empty'
        refcounts = self._get_refcounts()
        targ = self._val.type.template_argument(0)
        targ = strip_versioned_namespace(str(targ))

        if refcounts != 0:
            usecount = refcounts['_M_use_count']
            weakcount = refcounts['_M_weak_count']
            if usecount == 0:
                state = 'expired, weak count %d' % weakcount
            else:
                state = 'use count %d, weak count %d' % (
                    usecount, weakcount - 1)
        return '%s<%s> (%s)' % (self._typename, targ, state)


def _tuple_impl_get(val):
    """Return the tuple element stored in a _Tuple_impl<N, T> base class."""
    bases = val.type.fields()
    if not bases[-1].is_base_class:
        raise ValueError(
            "Unsupported implementation for std::tuple: %s" % str(val.type))
    # Get the _Head_base<N, T> base class:
    head_base = val.cast(bases[-1].type)
    fields = head_base.type.fields()
    if len(fields) == 0:
        raise ValueError(
            "Unsupported implementation for std::tuple: %s" % str(val.type))
    if fields[0].name == '_M_head_impl':
        # The tuple element is the _Head_base::_M_head_impl data member.
        return head_base['_M_head_impl']
    elif fields[0].is_base_class:
        # The tuple element is an empty base class of _Head_base.
        # Cast to that empty base class.
        return head_base.cast(fields[0].type)
    else:
        raise ValueError(
            "Unsupported implementation for std::tuple: %s" % str(val.type))


def tuple_get(n, val):
    """Return the result of std::get<n>(val) on a std::tuple."""
    tuple_size = len(get_template_arg_list(val.type))
    if n > tuple_size:
        raise ValueError("Out of range index for std::get<N> on std::tuple")
    # Get the first _Tuple_impl<0, T...> base class:
    node = val.cast(val.type.fields()[0].type)
    while n > 0:
        # Descend through the base classes until the Nth one.
        node = node.cast(node.type.fields()[0].type)
        n -= 1
    return _tuple_impl_get(node)


def unique_ptr_get(val):
    """Return the result of val.get() on a std::unique_ptr."""
    # std::unique_ptr<T, D> contains a std::tuple<D::pointer, D>,
    # either as a direct data member _M_t (the old implementation)
    # or within a data member of type __uniq_ptr_data.
    impl_type = val.type.fields()[0].type.strip_typedefs()
    # Check for new implementations first:
    if is_specialization_of(impl_type, '__uniq_ptr_data') \
            or is_specialization_of(impl_type, '__uniq_ptr_impl'):
        tuple_member = val['_M_t']['_M_t']
    elif is_specialization_of(impl_type, 'tuple'):
        tuple_member = val['_M_t']
    else:
        raise ValueError(
            "Unsupported implementation for unique_ptr: %s" % str(impl_type))
    return tuple_get(0, tuple_member)


class UniquePointerPrinter(printer_base):
    """Print a unique_ptr."""

    def __init__(self, typename, val):
        self._val = val

    def children(self):
        return SmartPtrIterator(unique_ptr_get(self._val))

    def to_string(self):
        t = self._val.type.template_argument(0)
        return 'std::unique_ptr<{}>'.format(str(t))


def get_value_from_aligned_membuf(buf, valtype):
    """Return the value held in a __gnu_cxx::__aligned_membuf."""
    return buf['_M_storage'].address.cast(valtype.pointer()).dereference()


def get_value_from_list_node(node):
    """Return the value held in an _List_node<_Val>."""
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


class StdListPrinter(printer_base):
    """Print a std::list."""

    class _iterator(Iterator):
        def __init__(self, nodetype, head):
            self._nodetype = nodetype
            self._base = head['_M_next']
            self._head = head.address
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self._base == self._head:
                raise StopIteration
            elt = self._base.cast(self._nodetype).dereference()
            self._base = elt['_M_next']
            count = self._count
            self._count = self._count + 1
            val = get_value_from_list_node(elt)
            return ('[%d]' % count, val)

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def children(self):
        nodetype = lookup_node_type('_List_node', self._val.type).pointer()
        return self._iterator(nodetype, self._val['_M_impl']['_M_node'])

    def to_string(self):
        headnode = self._val['_M_impl']['_M_node']
        if headnode['_M_next'] == headnode.address:
            return 'empty %s' % (self._typename)
        return '%s' % (self._typename)


class NodeIteratorPrinter(printer_base):
    def __init__(self, typename, val, contname, nodename):
        self._val = val
        self._typename = typename
        self._contname = contname
        self._nodetype = lookup_node_type(nodename, val.type)

    def to_string(self):
        if not self._val['_M_node']:
            return 'non-dereferenceable iterator for std::%s' % (self._contname)
        node = self._val['_M_node'].cast(
            self._nodetype.pointer()).dereference()
        return str(get_value_from_list_node(node))


class StdListIteratorPrinter(NodeIteratorPrinter):
    """Print std::list::iterator."""

    def __init__(self, typename, val):
        NodeIteratorPrinter.__init__(self, typename, val, 'list', '_List_node')


class StdFwdListIteratorPrinter(NodeIteratorPrinter):
    """Print std::forward_list::iterator."""

    def __init__(self, typename, val):
        NodeIteratorPrinter.__init__(self, typename, val, 'forward_list',
                                     '_Fwd_list_node')


class StdSlistPrinter(printer_base):
    """Print a __gnu_cxx::slist."""

    class _iterator(Iterator):
        def __init__(self, nodetype, head):
            self._nodetype = nodetype
            self._base = head['_M_head']['_M_next']
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self._base == 0:
                raise StopIteration
            elt = self._base.cast(self._nodetype).dereference()
            self._base = elt['_M_next']
            count = self._count
            self._count = self._count + 1
            return ('[%d]' % count, elt['_M_data'])

    def __init__(self, typename, val):
        self._val = val

    def children(self):
        nodetype = lookup_node_type('__gnu_cxx::_Slist_node', self._val.type)
        return self._iterator(nodetype.pointer(), self._val)

    def to_string(self):
        if self._val['_M_head']['_M_next'] == 0:
            return 'empty __gnu_cxx::slist'
        return '__gnu_cxx::slist'


class StdSlistIteratorPrinter(printer_base):
    """Print __gnu_cxx::slist::iterator."""

    def __init__(self, typename, val):
        self._val = val

    def to_string(self):
        if not self._val['_M_node']:
            return 'non-dereferenceable iterator for __gnu_cxx::slist'
        nodetype = lookup_node_type(
            '__gnu_cxx::_Slist_node', self._val.type).pointer()
        return str(self._val['_M_node'].cast(nodetype).dereference()['_M_data'])


class StdVectorPrinter(printer_base):
    """Print a std::vector."""

    class _iterator(Iterator):
        def __init__(self, start, finish, bitvec):
            self._bitvec = bitvec
            if bitvec:
                self._item = start['_M_p']
                self._so = 0
                self._finish = finish['_M_p']
                self._fo = finish['_M_offset']
                itype = self._item.dereference().type
                self._isize = 8 * itype.sizeof
            else:
                self._item = start
                self._finish = finish
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            count = self._count
            self._count = self._count + 1
            if self._bitvec:
                if self._item == self._finish and self._so >= self._fo:
                    raise StopIteration
                elt = bool(self._item.dereference() & (1 << self._so))
                self._so = self._so + 1
                if self._so >= self._isize:
                    self._item = self._item + 1
                    self._so = 0
                return ('[%d]' % count, elt)
            else:
                if self._item == self._finish:
                    raise StopIteration
                elt = self._item.dereference()
                self._item = self._item + 1
                return ('[%d]' % count, elt)

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val
        self._is_bool = val.type.template_argument(
            0).code == gdb.TYPE_CODE_BOOL

    def children(self):
        return self._iterator(self._val['_M_impl']['_M_start'],
                              self._val['_M_impl']['_M_finish'],
                              self._is_bool)

    def to_string(self):
        start = self._val['_M_impl']['_M_start']
        finish = self._val['_M_impl']['_M_finish']
        end = self._val['_M_impl']['_M_end_of_storage']
        if self._is_bool:
            start = self._val['_M_impl']['_M_start']['_M_p']
            finish = self._val['_M_impl']['_M_finish']['_M_p']
            fo = self._val['_M_impl']['_M_finish']['_M_offset']
            itype = start.dereference().type
            bl = 8 * itype.sizeof
            length = bl * (finish - start) + fo
            capacity = bl * (end - start)
            return ('%s<bool> of length %d, capacity %d'
                    % (self._typename, int(length), int(capacity)))
        else:
            return ('%s of length %d, capacity %d'
                    % (self._typename, int(finish - start), int(end - start)))

    def display_hint(self):
        return 'array'


class StdVectorIteratorPrinter(printer_base):
    """Print std::vector::iterator."""

    def __init__(self, typename, val):
        self._val = val

    def to_string(self):
        if not self._val['_M_current']:
            return 'non-dereferenceable iterator for std::vector'
        return str(self._val['_M_current'].dereference())


class StdBitIteratorPrinter(printer_base):
    """Print std::vector<bool>'s _Bit_iterator and _Bit_const_iterator."""

    def __init__(self, typename, val):
        self._val = val

    def to_string(self):
        if not self._val['_M_p']:
            return 'non-dereferenceable iterator for std::vector<bool>'
        return bool(self._val['_M_p'].dereference()
                    & (1 << self._val['_M_offset']))


class StdBitReferencePrinter(printer_base):
    """Print std::vector<bool>::reference."""

    def __init__(self, typename, val):
        self._val = val

    def to_string(self):
        if not self._val['_M_p']:
            return 'invalid std::vector<bool>::reference'
        return bool(self._val['_M_p'].dereference() & (self._val['_M_mask']))


class StdTuplePrinter(printer_base):
    """Print a std::tuple."""

    class _iterator(Iterator):
        @staticmethod
        def _is_nonempty_tuple(nodes):
            if len(nodes) == 2:
                if is_specialization_of(nodes[1].type, '__tuple_base'):
                    return True
            elif len(nodes) == 1:
                return True
            elif len(nodes) == 0:
                return False
            raise ValueError(
                "Top of tuple tree does not consist of a single node.")

        def __init__(self, head):
            self._head = head

            # Set the base class as the initial head of the
            # tuple.
            nodes = self._head.type.fields()
            if self._is_nonempty_tuple(nodes):
                # Set the actual head to the first pair.
                self._head = self._head.cast(nodes[0].type)
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            # Check for further recursions in the inheritance tree.
            # For a GCC 5+ tuple self._head is None after visiting all nodes:
            if not self._head:
                raise StopIteration
            nodes = self._head.type.fields()
            # For a GCC 4.x tuple there is a final node with no fields:
            if len(nodes) == 0:
                raise StopIteration
            # Check that this iteration has an expected structure.
            if len(nodes) > 2:
                raise ValueError(
                    "Cannot parse more than 2 nodes in a tuple tree.")

            if len(nodes) == 1:
                # This is the last node of a GCC 5+ std::tuple.
                impl = self._head.cast(nodes[0].type)
                self._head = None
            else:
                # Either a node before the last node, or the last node of
                # a GCC 4.x tuple (which has an empty parent).

                # - Left node is the next recursion parent.
                # - Right node is the actual class contained in the tuple.

                # Process right node.
                impl = self._head.cast(nodes[1].type)

                # Process left node and set it as head.
                self._head = self._head.cast(nodes[0].type)

            self._count = self._count + 1

            # Finally, check the implementation.  If it is
            # wrapped in _M_head_impl return that, otherwise return
            # the value "as is".
            fields = impl.type.fields()
            if len(fields) < 1 or fields[0].name != "_M_head_impl":
                return ('[%d]' % (self._count - 1), impl)
            else:
                return ('[%d]' % (self._count - 1), impl['_M_head_impl'])

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def children(self):
        return self._iterator(self._val)

    def to_string(self):
        if len(self._val.type.fields()) == 0:
            return 'empty %s' % (self._typename)
        return '%s containing' % (self._typename)


class StdStackOrQueuePrinter(printer_base):
    """Print a std::stack or std::queue."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._visualizer = gdb.default_visualizer(val['c'])

    def children(self):
        return self._visualizer.children()

    def to_string(self):
        return '%s wrapping: %s' % (self._typename,
                                    self._visualizer.to_string())

    def display_hint(self):
        if hasattr(self._visualizer, 'display_hint'):
            return self._visualizer.display_hint()
        return None


class RbtreeIterator(Iterator):
    """
    Turn an RB-tree-based container (std::map, std::set etc.) into
    a Python iterable object.
    """

    def __init__(self, rbtree):
        self._size = rbtree['_M_t']['_M_impl']['_M_node_count']
        self._node = rbtree['_M_t']['_M_impl']['_M_header']['_M_left']
        self._count = 0

    def __iter__(self):
        return self

    def __len__(self):
        return int(self._size)

    def __next__(self):
        if self._count == self._size:
            raise StopIteration
        result = self._node
        self._count = self._count + 1
        if self._count < self._size:
            # Compute the next node.
            node = self._node
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
            self._node = node
        return result


def get_value_from_Rb_tree_node(node):
    """Return the value held in an _Rb_tree_node<_Val>."""
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


class StdRbtreeIteratorPrinter(printer_base):
    """Print std::map::iterator, std::set::iterator, etc."""

    def __init__(self, typename, val):
        self._val = val
        nodetype = lookup_node_type('_Rb_tree_node', self._val.type)
        self._link_type = nodetype.pointer()

    def to_string(self):
        if not self._val['_M_node']:
            return 'non-dereferenceable iterator for associative container'
        node = self._val['_M_node'].cast(self._link_type).dereference()
        return str(get_value_from_Rb_tree_node(node))


class StdDebugIteratorPrinter(printer_base):
    """Print a debug enabled version of an iterator."""

    def __init__(self, typename, val):
        self._val = val

    # Just strip away the encapsulating __gnu_debug::_Safe_iterator
    # and return the wrapped iterator value.
    def to_string(self):
        base_type = gdb.lookup_type('__gnu_debug::_Safe_iterator_base')
        itype = self._val.type.template_argument(0)
        safe_seq = self._val.cast(base_type)['_M_sequence']
        if not safe_seq:
            return str(self._val.cast(itype))
        if self._val['_M_version'] != safe_seq['_M_version']:
            return "invalid iterator"
        return str(self._val.cast(itype))


def num_elements(num):
    """Return either "1 element" or "N elements" depending on the argument."""
    return '1 element' if num == 1 else '%d elements' % num


class StdMapPrinter(printer_base):
    """Print a std::map or std::multimap."""

    # Turn an RbtreeIterator into a pretty-print iterator.
    class _iter(Iterator):
        def __init__(self, rbiter, type):
            self._rbiter = rbiter
            self._count = 0
            self._type = type

        def __iter__(self):
            return self

        def __next__(self):
            if self._count % 2 == 0:
                n = next(self._rbiter)
                n = n.cast(self._type).dereference()
                n = get_value_from_Rb_tree_node(n)
                self._pair = n
                item = n['first']
            else:
                item = self._pair['second']
            result = ('[%d]' % self._count, item)
            self._count = self._count + 1
            return result

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        return '%s with %s' % (self._typename,
                               num_elements(len(RbtreeIterator(self._val))))

    def children(self):
        node = lookup_node_type('_Rb_tree_node', self._val.type).pointer()
        return self._iter(RbtreeIterator(self._val), node)

    def display_hint(self):
        return 'map'


class StdSetPrinter(printer_base):
    """Print a std::set or std::multiset."""

    # Turn an RbtreeIterator into a pretty-print iterator.
    class _iter(Iterator):
        def __init__(self, rbiter, type):
            self._rbiter = rbiter
            self._count = 0
            self._type = type

        def __iter__(self):
            return self

        def __next__(self):
            item = next(self._rbiter)
            item = item.cast(self._type).dereference()
            item = get_value_from_Rb_tree_node(item)
            # FIXME: this is weird ... what to do?
            # Maybe a 'set' display hint?
            result = ('[%d]' % self._count, item)
            self._count = self._count + 1
            return result

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        return '%s with %s' % (self._typename,
                               num_elements(len(RbtreeIterator(self._val))))

    def children(self):
        node = lookup_node_type('_Rb_tree_node', self._val.type).pointer()
        return self._iter(RbtreeIterator(self._val), node)


class StdBitsetPrinter(printer_base):
    """Print a std::bitset."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        # If template_argument handled values, we could print the
        # size.  Or we could use a regexp on the type.
        return '%s' % (self._typename)

    def children(self):
        try:
            # An empty bitset may not have any members which will
            # result in an exception being thrown.
            words = self._val['_M_w']
        except:
            return []

        wtype = words.type

        # The _M_w member can be either an unsigned long, or an
        # array.  This depends on the template specialization used.
        # If it is a single long, convert to a single element list.
        if wtype.code == gdb.TYPE_CODE_ARRAY:
            tsize = wtype.target().sizeof
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


class StdDequePrinter(printer_base):
    """Print a std::deque."""

    class _iter(Iterator):
        def __init__(self, node, start, end, last, buffer_size):
            self._node = node
            self._p = start
            self._end = end
            self._last = last
            self._buffer_size = buffer_size
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self._p == self._last:
                raise StopIteration

            result = ('[%d]' % self._count, self._p.dereference())
            self._count = self._count + 1

            # Advance the 'cur' pointer.
            self._p = self._p + 1
            if self._p == self._end:
                # If we got to the end of this bucket, move to the
                # next bucket.
                self._node = self._node + 1
                self._p = self._node[0]
                self._end = self._p + self._buffer_size

            return result

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val
        self._elttype = val.type.template_argument(0)
        size = self._elttype.sizeof
        if size < 512:
            self._buffer_size = int(512 / size)
        else:
            self._buffer_size = 1

    def to_string(self):
        start = self._val['_M_impl']['_M_start']
        end = self._val['_M_impl']['_M_finish']

        delta_n = end['_M_node'] - start['_M_node'] - 1
        delta_s = start['_M_last'] - start['_M_cur']
        delta_e = end['_M_cur'] - end['_M_first']

        size = self._buffer_size * delta_n + delta_s + delta_e

        return '%s with %s' % (self._typename, num_elements(long(size)))

    def children(self):
        start = self._val['_M_impl']['_M_start']
        end = self._val['_M_impl']['_M_finish']
        return self._iter(start['_M_node'], start['_M_cur'], start['_M_last'],
                          end['_M_cur'], self._buffer_size)

    def display_hint(self):
        return 'array'


class StdDequeIteratorPrinter(printer_base):
    """Print std::deque::iterator."""

    def __init__(self, typename, val):
        self._val = val

    def to_string(self):
        if not self._val['_M_cur']:
            return 'non-dereferenceable iterator for std::deque'
        return str(self._val['_M_cur'].dereference())


class StdStringPrinter(printer_base):
    """Print a std::basic_string of some kind."""

    def __init__(self, typename, val):
        self._val = val
        self._new_string = typename.find("::__cxx11::basic_string") != -1

    def to_string(self):
        # Make sure &string works, too.
        type = self._val.type
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target()

        # Calculate the length of the string so that to_string returns
        # the string according to length, not according to first null
        # encountered.
        ptr = self._val['_M_dataplus']['_M_p']
        if self._new_string:
            length = self._val['_M_string_length']
            # https://sourceware.org/bugzilla/show_bug.cgi?id=17728
            ptr = ptr.cast(ptr.type.strip_typedefs())
        else:
            realtype = type.unqualified().strip_typedefs()
            reptype = gdb.lookup_type(str(realtype) + '::_Rep').pointer()
            header = ptr.cast(reptype) - 1
            length = header.dereference()['_M_length']
        if hasattr(ptr, "lazy_string"):
            return ptr.lazy_string(length=length)
        return ptr.string(length=length)

    def display_hint(self):
        return 'string'


def access_streambuf_ptrs(streambuf):
    """Access the streambuf put area pointers."""
    pbase = streambuf['_M_out_beg']
    pptr = streambuf['_M_out_cur']
    egptr = streambuf['_M_in_end']
    return pbase, pptr, egptr


class StdStringBufPrinter(printer_base):
    """Print a std::basic_stringbuf."""

    def __init__(self, _, val):
        self._val = val

    def to_string(self):
        (pbase, pptr, egptr) = access_streambuf_ptrs(self._val)
        # Logic from basic_stringbuf::_M_high_mark()
        if pptr:
            if not egptr or pptr > egptr:
                return pbase.string(length=pptr - pbase)
            else:
                return pbase.string(length=egptr - pbase)
        return self._val['_M_string']

    def display_hint(self):
        return 'string'


class StdStringStreamPrinter(printer_base):
    """Print a std::basic_stringstream."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename

        # Check if the stream was redirected. This is essentially:
        # val['_M_streambuf'] != val['_M_stringbuf'].address
        # However, GDB can't resolve the virtual inheritance, so we do that
        # manually.
        basetype = [f.type for f in val.type.fields() if f.is_base_class][0]
        gdb.set_convenience_variable('__stream', val.cast(basetype).address)
        self._streambuf = gdb.parse_and_eval('$__stream->rdbuf()')
        self._was_redirected = self._streambuf != val['_M_stringbuf'].address

    def to_string(self):
        if self._was_redirected:
            return "%s redirected to %s" % (
                self._typename, self._streambuf.dereference())
        return self._val['_M_stringbuf']

    def display_hint(self):
        if self._was_redirected:
            return None
        return 'string'


class Tr1HashtableIterator(Iterator):
    def __init__(self, hashtable):
        self._buckets = hashtable['_M_buckets']
        self._bucket = 0
        self._bucket_count = hashtable['_M_bucket_count']
        self._node_type = find_type(hashtable.type, '_Node').pointer()
        self._node = 0
        while self._bucket != self._bucket_count:
            self._node = self._buckets[self._bucket]
            if self._node:
                break
            self._bucket = self._bucket + 1

    def __iter__(self):
        return self

    def __next__(self):
        if self._node == 0:
            raise StopIteration
        node = self._node.cast(self._node_type)
        result = node.dereference()['_M_v']
        self._node = node.dereference()['_M_next']
        if self._node == 0:
            self._bucket = self._bucket + 1
            while self._bucket != self._bucket_count:
                self._node = self._buckets[self._bucket]
                if self._node:
                    break
                self._bucket = self._bucket + 1
        return result


class StdHashtableIterator(Iterator):
    def __init__(self, hashtable):
        self._node = hashtable['_M_before_begin']['_M_nxt']
        valtype = hashtable.type.template_argument(1)
        cached = hashtable.type.template_argument(9).template_argument(0)
        node_type = lookup_templ_spec('std::__detail::_Hash_node', str(valtype),
                                      'true' if cached else 'false')
        self._node_type = node_type.pointer()

    def __iter__(self):
        return self

    def __next__(self):
        if self._node == 0:
            raise StopIteration
        elt = self._node.cast(self._node_type).dereference()
        self._node = elt['_M_nxt']
        valptr = elt['_M_storage'].address
        valptr = valptr.cast(elt.type.template_argument(0).pointer())
        return valptr.dereference()


class Tr1UnorderedSetPrinter(printer_base):
    """Print a std::unordered_set or tr1::unordered_set."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def _hashtable(self):
        if self._typename.startswith('std::tr1'):
            return self._val
        return self._val['_M_h']

    def to_string(self):
        count = self._hashtable()['_M_element_count']
        return '%s with %s' % (self._typename, num_elements(count))

    @staticmethod
    def _format_count(i):
        return '[%d]' % i

    def children(self):
        counter = imap(self._format_count, itertools.count())
        if self._typename.startswith('std::tr1'):
            return izip(counter, Tr1HashtableIterator(self._hashtable()))
        return izip(counter, StdHashtableIterator(self._hashtable()))


class Tr1UnorderedMapPrinter(printer_base):
    """Print a std::unordered_map or tr1::unordered_map."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def _hashtable(self):
        if self._typename.startswith('std::tr1'):
            return self._val
        return self._val['_M_h']

    def to_string(self):
        count = self._hashtable()['_M_element_count']
        return '%s with %s' % (self._typename, num_elements(count))

    @staticmethod
    def _flatten(list):
        for elt in list:
            for i in elt:
                yield i

    @staticmethod
    def _format_one(elt):
        return (elt['first'], elt['second'])

    @staticmethod
    def _format_count(i):
        return '[%d]' % i

    def children(self):
        counter = imap(self._format_count, itertools.count())
        # Map over the hash table and flatten the result.
        if self._typename.startswith('std::tr1'):
            data = self._flatten(
                imap(self._format_one, Tr1HashtableIterator(self._hashtable())))
            # Zip the two iterators together.
            return izip(counter, data)
        data = self._flatten(
            imap(self._format_one, StdHashtableIterator(self._hashtable())))
        # Zip the two iterators together.
        return izip(counter, data)

    def display_hint(self):
        return 'map'


class StdForwardListPrinter(printer_base):
    """Print a std::forward_list."""

    class _iterator(Iterator):
        def __init__(self, nodetype, head):
            self._nodetype = nodetype
            self._base = head['_M_next']
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self._base == 0:
                raise StopIteration
            elt = self._base.cast(self._nodetype).dereference()
            self._base = elt['_M_next']
            count = self._count
            self._count = self._count + 1
            valptr = elt['_M_storage'].address
            valptr = valptr.cast(elt.type.template_argument(0).pointer())
            return ('[%d]' % count, valptr.dereference())

    def __init__(self, typename, val):
        self._val = val
        self._typename = strip_versioned_namespace(typename)

    def children(self):
        nodetype = lookup_node_type('_Fwd_list_node', self._val.type).pointer()
        return self._iterator(nodetype, self._val['_M_impl']['_M_head'])

    def to_string(self):
        if self._val['_M_impl']['_M_head']['_M_next'] == 0:
            return 'empty %s' % self._typename
        return '%s' % self._typename


class SingleObjContainerPrinter(printer_base):
    """Base class for printers of containers of single objects."""

    def __init__(self, val, viz, hint=None):
        self._contained_value = val
        self._visualizer = viz
        self._hint = hint

    def _recognize(self, type):
        """Return type as a string after applying type printers."""
        global _use_type_printing
        if not _use_type_printing:
            return str(type)
        return gdb.types.apply_type_recognizers(gdb.types.get_type_recognizers(),
                                                type) or str(type)

    class _contained(Iterator):
        def __init__(self, val):
            self._val = val

        def __iter__(self):
            return self

        def __next__(self):
            if self._val is None:
                raise StopIteration
            retval = self._val
            self._val = None
            return ('[contained value]', retval)

    def children(self):
        if self._contained_value is None:
            return self._contained(None)
        if hasattr(self._visualizer, 'children'):
            return self._visualizer.children()
        return self._contained(self._contained_value)

    def display_hint(self):
        if (hasattr(self._visualizer, 'children')
                and hasattr(self._visualizer, 'display_hint')):
            # If contained value is a map we want to display in the same way.
            return self._visualizer.display_hint()
        return self._hint


def function_pointer_to_name(f):
    """Find the name of the function referred to by the gdb.Value f,
    which should contain a function pointer from the program."""

    # Turn the function pointer into an actual address.
    # This is needed to unpack ppc64 function descriptors.
    f = f.dereference().address

    if sys.version_info[0] == 2:
        # Older versions of GDB need to use long for Python 2,
        # because int(f) on 64-bit big-endian values raises a
        # gdb.error saying "Cannot convert value to int."
        f = long(f)
    else:
        f = int(f)

    try:
        # If the function can't be found older versions of GDB raise a
        # RuntimeError saying "Cannot locate object file for block."
        return gdb.block_for_pc(f).function.name
    except:
        return None


class StdExpAnyPrinter(SingleObjContainerPrinter):
    """Print a std::any or std::experimental::any."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._typename = strip_fundts_namespace(self._typename)
        self._val = val
        self._contained_type = None
        contained_value = None
        visualizer = None
        mgr = self._val['_M_manager']
        if mgr != 0:
            func = function_pointer_to_name(mgr)
            if not func:
                raise ValueError(
                    "Invalid function pointer in %s" % (self._typename))
            # We want to use this regular expression:
            # T::_Manager_xxx<.*>::_S_manage\(T::_Op, const T\*, T::_Arg\*\)
            # where T is std::any or std::experimental::any.
            # But we need to account for variances in demangled names
            # between GDB versions, e.g. 'enum T::_Op' instead of 'T::_Op'.
            rx = (
                r"({0}::_Manager_\w+<.*>)::_S_manage\("
                r"(enum )?{0}::_Op, (const {0}|{0} const) ?\*, "
                r"(union )?{0}::_Arg ?\*\)"
            ).format(typename)
            m = re.match(rx, func)
            if not m:
                raise ValueError(
                    "Unknown manager function in %s" % self._typename)

            mgrname = m.group(1)
            # FIXME need to expand 'std::string' so that gdb.lookup_type works
            if 'std::string' in mgrname:
                mgrtypes = []
                for s in StdExpAnyPrinter._string_types():
                    try:
                        x = re.sub(r"std::string(?!\w)", s, m.group(1))
                        # The following lookup might raise gdb.error if the
                        # manager function was never instantiated for 's' in
                        # the program, because there will be no such type.
                        mgrtypes.append(gdb.lookup_type(x))
                    except gdb.error:
                        pass
                if len(mgrtypes) != 1:
                    # FIXME: this is unlikely in practice, but possible for
                    # programs that use both old and new string types with
                    # std::any in a single program. Can we do better?
                    # Maybe find the address of each type's _S_manage and
                    # compare to the address stored in _M_manager?
                    raise ValueError(
                        'Cannot uniquely determine std::string type '
                        'used in std::any'
                    )
                mgrtype = mgrtypes[0]
            else:
                mgrtype = gdb.lookup_type(mgrname)
            self._contained_type = mgrtype.template_argument(0)
            valptr = None
            if '::_Manager_internal' in mgrname:
                valptr = self._val['_M_storage']['_M_buffer'].address
            elif '::_Manager_external' in mgrname:
                valptr = self._val['_M_storage']['_M_ptr']
            else:
                raise ValueError(
                    "Unknown manager function in %s" % self._typename)
            contained_value = valptr.cast(
                self._contained_type.pointer()).dereference()
            visualizer = gdb.default_visualizer(contained_value)
        super(StdExpAnyPrinter, self).__init__(contained_value, visualizer)

    def to_string(self):
        if self._contained_type is None:
            return '%s [no contained value]' % self._typename
        desc = "%s containing " % self._typename
        if hasattr(self._visualizer, 'children'):
            return desc + self._visualizer.to_string()
        valtype = self._recognize(self._contained_type)
        return desc + strip_versioned_namespace(str(valtype))

    @staticmethod
    def _string_types():
        # This lookup for std::string might return the __cxx11 version,
        # but that's not necessarily the one used by the std::any
        # manager function we're trying to find.
        strings = {str(gdb.lookup_type('std::string').strip_typedefs())}
        # So also consider all the other possible std::string types!
        s = 'basic_string<char, std::char_traits<char>, std::allocator<char> >'
        quals = ['std::', 'std::__cxx11::',
                 'std::' + _versioned_namespace]
        strings |= {q + s for q in quals}  # set of unique strings
        return strings


class StdExpOptionalPrinter(SingleObjContainerPrinter):
    """Print a std::optional or std::experimental::optional."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._typename = strip_fundts_namespace(self._typename)
        payload = val['_M_payload']
        if self._typename.startswith('std::experimental'):
            engaged = val['_M_engaged']
            contained_value = payload
        else:
            engaged = payload['_M_engaged']
            contained_value = payload['_M_payload']
            try:
                # Since GCC 9
                contained_value = contained_value['_M_value']
            except:
                pass
        visualizer = gdb.default_visualizer(contained_value)
        if not engaged:
            contained_value = None
        super(StdExpOptionalPrinter, self).__init__(
            contained_value, visualizer)

    def to_string(self):
        if self._contained_value is None:
            return "%s [no contained value]" % self._typename
        if hasattr(self._visualizer, 'children'):
            return "%s containing %s" % (self._typename,
                                         self._visualizer.to_string())
        return self._typename


class StdVariantPrinter(SingleObjContainerPrinter):
    """Print a std::variant."""

    def __init__(self, typename, val):
        alternatives = get_template_arg_list(val.type)
        self._typename = strip_versioned_namespace(typename)
        self._index = val['_M_index']
        if self._index >= len(alternatives):
            self._contained_type = None
            contained_value = None
            visualizer = None
        else:
            self._contained_type = alternatives[int(self._index)]
            addr = val['_M_u']['_M_first']['_M_storage'].address
            contained_value = addr.cast(
                self._contained_type.pointer()).dereference()
            visualizer = gdb.default_visualizer(contained_value)
        super(StdVariantPrinter, self).__init__(
            contained_value, visualizer, 'array')

    def to_string(self):
        if self._contained_value is None:
            return "%s [no contained value]" % self._typename
        if hasattr(self._visualizer, 'children'):
            return "%s [index %d] containing %s" % (self._typename, self._index,
                                                    self._visualizer.to_string())
        return "%s [index %d]" % (self._typename, self._index)


class StdNodeHandlePrinter(SingleObjContainerPrinter):
    """Print a container node handle."""

    def __init__(self, typename, val):
        self._value_type = val.type.template_argument(1)
        nodetype = val.type.template_argument(2).template_argument(0)
        self._is_rb_tree_node = is_specialization_of(
            nodetype.name, '_Rb_tree_node')
        self._is_map_node = val.type.template_argument(0) != self._value_type
        nodeptr = val['_M_ptr']
        if nodeptr:
            if self._is_rb_tree_node:
                contained_value = get_value_from_Rb_tree_node(
                    nodeptr.dereference())
            else:
                contained_value = get_value_from_aligned_membuf(nodeptr['_M_storage'],
                                                                self._value_type)
            visualizer = gdb.default_visualizer(contained_value)
        else:
            contained_value = None
            visualizer = None
        optalloc = val['_M_alloc']
        self._alloc = optalloc['_M_payload'] if optalloc['_M_engaged'] else None
        super(StdNodeHandlePrinter, self).__init__(contained_value, visualizer,
                                                   'array')

    def to_string(self):
        desc = 'node handle for '
        if not self._is_rb_tree_node:
            desc += 'unordered '
        if self._is_map_node:
            desc += 'map'
        else:
            desc += 'set'

        if self._contained_value:
            desc += ' with element'
            if hasattr(self._visualizer, 'children'):
                return "%s = %s" % (desc, self._visualizer.to_string())
            return desc
        else:
            return 'empty %s' % desc


class StdExpStringViewPrinter(printer_base):
    """
    Print a std::basic_string_view or std::experimental::basic_string_view
    """

    def __init__(self, typename, val):
        self._val = val

    def to_string(self):
        ptr = self._val['_M_str']
        len = self._val['_M_len']
        if hasattr(ptr, "lazy_string"):
            return ptr.lazy_string(length=len)
        return ptr.string(length=len)

    def display_hint(self):
        return 'string'


class StdExpPathPrinter(printer_base):
    """Print a std::experimental::filesystem::path."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename
        start = self._val['_M_cmpts']['_M_impl']['_M_start']
        finish = self._val['_M_cmpts']['_M_impl']['_M_finish']
        self._num_cmpts = int(finish - start)

    def _path_type(self):
        t = str(self._val['_M_type'])
        if t[-9:] == '_Root_dir':
            return "root-directory"
        if t[-10:] == '_Root_name':
            return "root-name"
        return None

    def to_string(self):
        path = "%s" % self._val['_M_pathname']
        if self._num_cmpts == 0:
            t = self._path_type()
            if t:
                path = '%s [%s]' % (path, t)
        return "experimental::filesystem::path %s" % path

    class _iterator(Iterator):
        def __init__(self, cmpts, pathtype):
            self._pathtype = pathtype
            self._item = cmpts['_M_impl']['_M_start']
            self._finish = cmpts['_M_impl']['_M_finish']
            self._count = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self._item == self._finish:
                raise StopIteration
            item = self._item.dereference()
            count = self._count
            self._count = self._count + 1
            self._item = self._item + 1
            path = item['_M_pathname']
            t = StdExpPathPrinter(self._pathtype, item)._path_type()
            if not t:
                t = count
            return ('[%s]' % t, path)

    def children(self):
        return self._iterator(self._val['_M_cmpts'], self._typename)


class StdPathPrinter(printer_base):
    """Print a std::filesystem::path."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename
        impl = unique_ptr_get(self._val['_M_cmpts']['_M_impl'])
        self._type = impl.cast(gdb.lookup_type('uintptr_t')) & 3
        if self._type == 0:
            self._impl = impl
        else:
            self._impl = None

    def _path_type(self):
        t = str(self._type.cast(gdb.lookup_type(self._typename + '::_Type')))
        if t[-9:] == '_Root_dir':
            return "root-directory"
        if t[-10:] == '_Root_name':
            return "root-name"
        return None

    def to_string(self):
        path = "%s" % self._val['_M_pathname']
        if self._type != 0:
            t = self._path_type()
            if t:
                path = '%s [%s]' % (path, t)
        return "filesystem::path %s" % path

    class _iterator(Iterator):
        def __init__(self, impl, pathtype):
            self._pathtype = pathtype
            if impl:
                # We can't access _Impl::_M_size because _Impl is incomplete
                # so cast to int* to access the _M_size member at offset zero,
                int_type = gdb.lookup_type('int')
                cmpt_type = gdb.lookup_type(pathtype + '::_Cmpt')
                char_type = gdb.lookup_type('char')
                impl = impl.cast(int_type.pointer())
                size = impl.dereference()
                #self._capacity = (impl + 1).dereference()
                if hasattr(gdb.Type, 'alignof'):
                    sizeof_Impl = max(2 * int_type.sizeof, cmpt_type.alignof)
                else:
                    sizeof_Impl = 2 * int_type.sizeof
                begin = impl.cast(char_type.pointer()) + sizeof_Impl
                self._item = begin.cast(cmpt_type.pointer())
                self._finish = self._item + size
                self._count = 0
            else:
                self._item = None
                self._finish = None

        def __iter__(self):
            return self

        def __next__(self):
            if self._item == self._finish:
                raise StopIteration
            item = self._item.dereference()
            count = self._count
            self._count = self._count + 1
            self._item = self._item + 1
            path = item['_M_pathname']
            t = StdPathPrinter(self._pathtype, item)._path_type()
            if not t:
                t = count
            return ('[%s]' % t, path)

    def children(self):
        return self._iterator(self._impl, self._typename)


class StdPairPrinter(printer_base):
    """Print a std::pair object, with 'first' and 'second' as children."""

    def __init__(self, typename, val):
        self._val = val

    class _iter(Iterator):
        """An iterator for std::pair types. Returns 'first' then 'second'."""

        def __init__(self, val):
            self._val = val
            self._which = 'first'

        def __iter__(self):
            return self

        def __next__(self):
            if self._which is None:
                raise StopIteration
            which = self._which
            if which == 'first':
                self._which = 'second'
            else:
                self._which = None
            return (which, self._val[which])

    def children(self):
        return self._iter(self._val)

    def to_string(self):
        return None


class StdCmpCatPrinter(printer_base):
    """Print a comparison category object."""

    def __init__(self, typename, val):
        self._typename = typename[typename.rfind(':') + 1:]
        self._val = val['_M_value']

    def to_string(self):
        if self._typename == 'strong_ordering' and self._val == 0:
            name = 'equal'
        else:
            names = {2: 'unordered', -1: 'less', 0: 'equivalent', 1: 'greater'}
            name = names[int(self._val)]
        return 'std::{}::{}'.format(self._typename, name)


class StdErrorCodePrinter(printer_base):
    """Print a std::error_code or std::error_condition."""

    _system_is_posix = None  # Whether std::system_category() use errno values.

    def __init__(self, typename, val):
        self._val = val
        self._typename = strip_versioned_namespace(typename)
        # Do this only once ...
        if StdErrorCodePrinter._system_is_posix is None:
            try:
                import posix
                StdErrorCodePrinter._system_is_posix = True
            except ImportError:
                StdErrorCodePrinter._system_is_posix = False

    @staticmethod
    def _find_errc_enum(name):
        typ = gdb.lookup_type(name)
        if typ is not None and typ.code == gdb.TYPE_CODE_ENUM:
            return typ
        return None

    @classmethod
    def _find_standard_errc_enum(cls, name):
        for ns in ['', _versioned_namespace]:
            try:
                qname = 'std::{}{}'.format(ns, name)
                return cls._find_errc_enum(qname)
            except RuntimeError:
                pass

    @classmethod
    def _match_net_ts_category(cls, cat):
        net_cats = ['stream', 'socket', 'ip::resolver']
        for c in net_cats:
            func = c + '_category()'
            for ns in ['', _versioned_namespace]:
                ns = 'std::{}experimental::net::v1'.format(ns)
                sym = gdb.lookup_symbol('{}::{}::__c'.format(ns, func))[0]
                if sym is not None:
                    if cat == sym.value().address:
                        name = 'net::' + func
                        enum = cls._find_errc_enum('{}::{}_errc'.format(ns, c))
                        return (name, enum)
        return (None, None)

    @classmethod
    def _category_info(cls, cat):
        """Return details of a std::error_category."""

        name = None
        enum = None
        is_errno = False

        # Try these first, or we get "warning: RTTI symbol not found" when
        # using cat.dynamic_type on the local class types for Net TS
        # categories.
        func, enum = cls._match_net_ts_category(cat)
        if func is not None:
            return (None, func, enum, is_errno)

        # This might give a warning for a program-defined category defined as
        # a local class, but there doesn't seem to be any way to avoid that.
        typ = cat.dynamic_type.target()
        # Shortcuts for the known categories defined by libstdc++.
        if typ.tag.endswith('::generic_error_category'):
            name = 'generic'
            is_errno = True
        if typ.tag.endswith('::system_error_category'):
            name = 'system'
            is_errno = cls._system_is_posix
        if typ.tag.endswith('::future_error_category'):
            name = 'future'
            enum = cls._find_standard_errc_enum('future_errc')
        if typ.tag.endswith('::io_error_category'):
            name = 'io'
            enum = cls._find_standard_errc_enum('io_errc')

        if name is None:
            try:
                # Want to call std::error_category::name() override, but it's
                # unsafe: https://sourceware.org/bugzilla/show_bug.cgi?id=28856
                # gdb.set_convenience_variable('__cat', cat)
                # return '"%s"' % gdb.parse_and_eval('$__cat->name()').string()
                pass
            except:
                pass
        return (name, typ.tag, enum, is_errno)

    @staticmethod
    def _unqualified_name(name):
        """
        Strip any nested-name-specifier from name to give an unqualified name.
        """
        return name.split('::')[-1]

    def to_string(self):
        value = self._val['_M_value']
        cat = self._val['_M_cat']
        name, alt_name, enum, is_errno = self._category_info(cat)
        if value == 0:
            default_cats = {'error_code': 'system',
                            'error_condition': 'generic'}
            if name == default_cats[self._unqualified_name(self._typename)]:
                return self._typename + ' = { }'  # default-constructed value

        strval = str(value)
        if is_errno and value != 0:
            try:
                strval = errno.errorcode[int(value)]
            except:
                pass
        elif enum is not None:
            strval = self._unqualified_name(str(value.cast(enum)))

        if name is not None:
            name = '"%s"' % name
        else:
            name = alt_name
        return '%s = {%s: %s}' % (self._typename, name, strval)


class StdRegexStatePrinter(printer_base):
    """Print a state node in the NFA for a std::regex."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename

    def to_string(self):
        opcode = str(self._val['_M_opcode'])
        if opcode:
            opcode = opcode[25:]
        next_id = self._val['_M_next']

        variants = {'repeat': 'alt', 'alternative': 'alt',
                    'subexpr_begin': 'subexpr', 'subexpr_end': 'subexpr',
                    'line_begin_assertion': None, 'line_end_assertion': None,
                    'word_boundary': 'neg', 'subexpr_lookahead': 'neg',
                    'backref': 'backref_index',
                    'match': None, 'accept': None,
                    'dummy': None, 'unknown': None
                    }
        v = variants[opcode]

        s = "opcode={}, next={}".format(opcode, next_id)
        if v is not None and self._val['_M_' + v] is not None:
            s = "{}, {}={}".format(s, v, self._val['_M_' + v])
        return "{%s}" % (s)


class StdSpanPrinter(printer_base):
    """Print a std::span."""

    class _iterator(Iterator):
        def __init__(self, begin, size):
            self._count = 0
            self._begin = begin
            self._size = size

        def __iter__(self):
            return self

        def __next__(self):
            if self._count == self._size:
                raise StopIteration

            count = self._count
            self._count = self._count + 1
            return '[%d]' % count, (self._begin + count).dereference()

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val
        size_max = gdb.parse_and_eval('static_cast<std::size_t>(-1)')
        if val.type.template_argument(1) == size_max:
            self._size = val['_M_extent']['_M_extent_value']
        else:
            self._size = val.type.template_argument(1)

    def to_string(self):
        return '%s of length %d' % (self._typename, self._size)

    def children(self):
        return self._iterator(self._val['_M_ptr'], self._size)

    def display_hint(self):
        return 'array'


class StdInitializerListPrinter(printer_base):
    """Print a std::initializer_list."""

    def __init__(self, typename, val):
        self._typename = typename
        self._val = val
        self._size = val['_M_len']

    def to_string(self):
        return '%s of length %d' % (self._typename, self._size)

    def children(self):
        return StdSpanPrinter._iterator(self._val['_M_array'], self._size)

    def display_hint(self):
        return 'array'


class StdAtomicPrinter(printer_base):
    """Print a std:atomic."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val
        self._shptr_printer = None
        self._value_type = self._val.type.template_argument(0)
        if self._value_type.tag is not None:
            typ = strip_versioned_namespace(self._value_type.tag)
            if (typ.startswith('std::shared_ptr<')
                    or typ.startswith('std::weak_ptr<')):
                impl = val['_M_impl']
                self._shptr_printer = SharedPointerPrinter(typename, impl)
                self.children = self._shptr_children

    def _shptr_children(self):
        return SmartPtrIterator(self._shptr_printer._pointer)

    def to_string(self):
        if self._shptr_printer is not None:
            return self._shptr_printer.to_string()

        if self._value_type.code == gdb.TYPE_CODE_INT:
            val = self._val['_M_i']
        elif self._value_type.code == gdb.TYPE_CODE_FLT:
            val = self._val['_M_fp']
        elif self._value_type.code == gdb.TYPE_CODE_PTR:
            val = self._val['_M_b']['_M_p']
        elif self._value_type.code == gdb.TYPE_CODE_BOOL:
            val = self._val['_M_base']['_M_i']
        else:
            val = self._val['_M_i']
        return '%s<%s> = { %s }' % (self._typename, str(self._value_type), val)


class StdFormatArgsPrinter(printer_base):
    """Print a std::basic_format_args."""
    # TODO: add printer for basic_format_arg<Context> and print out children.
    # TODO: add printer for __format::_ArgStore<Context, Args...>.

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        targs = get_template_arg_list(self._val.type)
        char_type = get_template_arg_list(targs[0])[1]
        if char_type == gdb.lookup_type('char'):
            typ = 'std::format_args'
        elif char_type == gdb.lookup_type('wchar_t'):
            typ = 'std::wformat_args'
        else:
            typ = 'std::basic_format_args'

        size = self._val['_M_packed_size']
        if size == 1:
            return "%s with 1 argument" % (typ)
        if size == 0:
            size = self._val['_M_unpacked_size']
        return "%s with %d arguments" % (typ, size)


class StdChronoDurationPrinter(printer_base):
    """Print a std::chrono::duration."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def _ratio(self):
        # TODO use reduced period i.e. duration::period
        period = self._val.type.template_argument(1)
        num = period.template_argument(0)
        den = period.template_argument(1)
        return (num, den)

    def _suffix(self):
        num, den = self._ratio()
        if num == 1:
            if den == 1:
                return 's'
            if den == 1000:
                return 'ms'
            if den == 1000000:
                return 'us'
            if den == 1000000000:
                return 'ns'
        elif den == 1:
            if num == 60:
                return 'min'
            if num == 3600:
                return 'h'
            if num == 86400:
                return 'd'
            return '[{}]s'.format(num)
        return "[{}/{}]s".format(num, den)

    def to_string(self):
        r = self._val['__r']
        if r.type.strip_typedefs().code == gdb.TYPE_CODE_FLT:
            r = "%g" % r
        return "std::chrono::duration = {{ {}{} }}".format(r, self._suffix())


class StdChronoTimePointPrinter(printer_base):
    """Print a std::chrono::time_point."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def _clock(self):
        clock = self._val.type.template_argument(0)
        name = strip_versioned_namespace(clock.name)
        if name == 'std::chrono::_V2::system_clock' \
                or name == 'std::chrono::system_clock':
            return ('std::chrono::sys_time', 0)
        # XXX need to remove leap seconds from utc, gps, and tai
        if name == 'std::chrono::utc_clock':
            return ('std::chrono::utc_time', None)  # XXX
        if name == 'std::chrono::gps_clock':
            return ('std::chrono::gps_time', None)  # XXX 315964809
        if name == 'std::chrono::tai_clock':
            return ('std::chrono::tai_time', None)  # XXX -378691210
        if name == 'std::filesystem::__file_clock':
            return ('std::chrono::file_time', 6437664000)
        if name == 'std::chrono::local_t':
            return ('std::chrono::local_time', 0)
        return ('{} time_point'.format(name), None)

    def to_string(self, abbrev=False):
        clock, offset = self._clock()
        d = self._val['__d']
        r = d['__r']
        printer = StdChronoDurationPrinter(d.type.name, d)
        suffix = printer._suffix()
        time = ''
        if offset is not None:
            num, den = printer._ratio()
            secs = (r * num / den) + offset
            try:
                dt = datetime.datetime.fromtimestamp(secs, _utc_timezone)
                time = ' [{:%Y-%m-%d %H:%M:%S}]'.format(dt)
            except:
                pass
        s = '%d%s%s' % (r, suffix, time)
        if abbrev:
            return s
        return '%s = { %s }' % (clock, s)


class StdChronoZonedTimePrinter(printer_base):
    """Print a std::chrono::zoned_time."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        zone = self._val['_M_zone'].dereference()['_M_name']
        time = self._val['_M_tp']
        printer = StdChronoTimePointPrinter(time.type.name, time)
        time = printer.to_string(True)
        return 'std::chrono::zoned_time = {{ {} {} }}'.format(zone, time)


months = [None, 'January', 'February', 'March', 'April', 'May', 'June',
          'July', 'August', 'September', 'October', 'November', 'December']

weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
            'Saturday', 'Sunday']


class StdChronoCalendarPrinter(printer_base):
    """Print a std::chrono::day, std::chrono::month, std::chrono::year etc."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        val = self._val
        typ = self._typename
        if 'month' in typ and typ != 'std::chrono::year_month_day_last':
            m = val['_M_m']
        if typ.startswith('std::chrono::year'):
            y = val['_M_y']

        if typ == 'std::chrono::day':
            return '{}'.format(int(val['_M_d']))
        if typ == 'std::chrono::month':
            if m < 1 or m >= len(months):
                return "%d is not a valid month" % m
            return months[m]
        if typ == 'std::chrono::year':
            return '{}y'.format(y)
        if typ == 'std::chrono::weekday':
            wd = val['_M_wd']
            if wd < 0 or wd >= len(weekdays):
                return "%d is not a valid weekday" % wd
            return '{}'.format(weekdays[wd])
        if typ == 'std::chrono::weekday_indexed':
            return '{}[{}]'.format(val['_M_wd'], int(val['_M_index']))
        if typ == 'std::chrono::weekday_last':
            return '{}[last]'.format(val['_M_wd'])
        if typ == 'std::chrono::month_day':
            return '{}/{}'.format(m, val['_M_d'])
        if typ == 'std::chrono::month_day_last':
            return '{}/last'.format(m)
        if typ == 'std::chrono::month_weekday':
            return '{}/{}'.format(m, val['_M_wdi'])
        if typ == 'std::chrono::month_weekday_last':
            return '{}/{}'.format(m, val['_M_wdl'])
        if typ == 'std::chrono::year_month':
            return '{}/{}'.format(y, m)
        if typ == 'std::chrono::year_month_day':
            return '{}/{}/{}'.format(y, m, val['_M_d'])
        if typ == 'std::chrono::year_month_day_last':
            return '{}/{}'.format(y, val['_M_mdl'])
        if typ == 'std::chrono::year_month_weekday':
            return '{}/{}/{}'.format(y, m, val['_M_wdi'])
        if typ == 'std::chrono::year_month_weekday_last':
            return '{}/{}/{}'.format(y, m, val['_M_wdl'])
        if typ.startswith('std::chrono::hh_mm_ss'):
            fract = ''
            if val['fractional_width'] != 0:
                fract = '.{:0{}d}'.format(int(val['_M_ss']['_M_r']),
                                          int(val['fractional_width']))
            h = int(val['_M_h']['__r'])
            m = int(val['_M_m']['__r'])
            s = int(val['_M_s']['__r'])
            if val['_M_is_neg']:
                h = -h
            return '{:02}:{:02}:{:02}{}'.format(h, m, s, fract)


class StdChronoTimeZonePrinter(printer_base):
    """Print a chrono::time_zone or chrono::time_zone_link."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        str = '%s = %s' % (self._typename, self._val['_M_name'])
        if self._typename.endswith("_link"):
            str += ' -> %s' % (self._val['_M_target'])
        return str


class StdChronoLeapSecondPrinter(printer_base):
    """Print a chrono::leap_second."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        date = self._val['_M_s']['__r']
        neg = '+-'[date < 0]
        return '%s %d (%c)' % (self._typename, abs(date), neg)


class StdChronoTzdbPrinter(printer_base):
    """Print a chrono::tzdb."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        return '%s %s' % (self._typename, self._val['version'])


class StdChronoTimeZoneRulePrinter(printer_base):
    """Print a chrono::time_zone rule."""

    def __init__(self, typename, val):
        self._typename = strip_versioned_namespace(typename)
        self._val = val

    def to_string(self):
        on = self._val['on']
        kind = on['kind']
        month = months[on['month']]
        suffixes = {1: 'st', 2: 'nd', 3: 'rd',
                    21: 'st', 22: 'nd', 23: 'rd', 31: 'st'}
        day = on['day_of_month']
        ordinal_day = '{}{}'.format(day, suffixes.get(day, 'th'))
        if kind == 0:  # DayOfMonth
            start = '{} {}'.format(month, ordinal_day)
        else:
            weekday = weekdays[on['day_of_week']]
            if kind == 1:  # LastWeekDay
                start = 'last {} in {}'.format(weekday, month)
            else:
                if kind == 2:  # LessEq
                    direction = ('last', '<=')
                else:
                    direction = ('first', '>=')
                day = on['day_of_month']
                start = '{} {} {} {} {}'.format(direction[0], weekday,
                                                direction[1], month,
                                                ordinal_day)
        return 'time_zone rule {} from {} to {} starting on {}'.format(
            self._val['name'], self._val['from'], self._val['to'], start)


class StdLocalePrinter(printer_base):
    """Print a std::locale."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename

    def to_string(self):
        names = self._val['_M_impl']['_M_names']
        mod = ''
        if names[0] == 0:
            name = '*'
        else:
            cats = gdb.parse_and_eval(self._typename + '::_S_categories')
            ncat = gdb.parse_and_eval(self._typename + '::_S_categories_size')
            n = names[0].string()
            cat = cats[0].string()
            name = '{}={}'.format(cat, n)
            cat_names = {cat: n}
            i = 1
            while i < ncat and names[i] != 0:
                n = names[i].string()
                cat = cats[i].string()
                name = '{};{}={}'.format(name, cat, n)
                cat_names[cat] = n
                i = i + 1
            uniq_names = set(cat_names.values())
            if len(uniq_names) == 1:
                name = n
            elif len(uniq_names) == 2:
                n1, n2 = (uniq_names)
                name_list = list(cat_names.values())
                other = None
                if name_list.count(n1) == 1:
                    name = n2
                    other = n1
                elif name_list.count(n2) == 1:
                    name = n1
                    other = n2
                if other is not None:
                    cat = next(c for c, n in cat_names.items() if n == other)
                    mod = ' with "{}={}"'.format(cat, other)
        return 'std::locale = "{}"{}'.format(name, mod)

class StdIntegralConstantPrinter(printer_base):
    """Print a std::true_type or std::false_type."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename

    def to_string(self):
        value_type = self._val.type.template_argument(0)
        value = self._val.type.template_argument(1)
        if value_type.code == gdb.TYPE_CODE_BOOL:
            if value:
                return "std::true_type"
            else:
                return "std::false_type"
        typename = strip_versioned_namespace(self._typename)
        return "{}<{}, {}>".format(typename, value_type, value)

class StdTextEncodingPrinter(printer_base):
    """Print a std::text_encoding."""

    def __init__(self, typename, val):
        self._val = val
        self._typename = typename

    def to_string(self):
        rep = self._val['_M_rep'].dereference()
        if rep['_M_id'] == 1:
            return self._val['_M_name']
        if rep['_M_id'] == 2:
            return 'unknown'
        return rep['_M_name']

# A "regular expression" printer which conforms to the
# "SubPrettyPrinter" protocol from gdb.printing.
class RxPrinter(object):
    def __init__(self, name, function):
        super(RxPrinter, self).__init__()
        self.name = name
        self._function = function
        self.enabled = True

    def invoke(self, value):
        if not self.enabled:
            return None

        if value.type.code == gdb.TYPE_CODE_REF:
            if hasattr(gdb.Value, "referenced_value"):
                value = value.referenced_value()

        return self._function(self.name, value)

# A pretty-printer that conforms to the "PrettyPrinter" protocol from
# gdb.printing.  It can also be used directly as an old-style printer.


class Printer(object):
    def __init__(self, name):
        super(Printer, self).__init__()
        self.name = name
        self._subprinters = []
        self._lookup = {}
        self.enabled = True
        self._compiled_rx = re.compile('^([a-zA-Z0-9_:]+)(<.*>)?$')

    def add(self, name, function):
        # A small sanity check.
        # FIXME
        if not self._compiled_rx.match(name):
            raise ValueError(
                'libstdc++ programming error: "%s" does not match' % name)
        printer = RxPrinter(name, function)
        self._subprinters.append(printer)
        self._lookup[name] = printer

    # Add a name using _GLIBCXX_BEGIN_NAMESPACE_VERSION.
    def add_version(self, base, name, function):
        self.add(base + name, function)
        if '__cxx11' not in base:
            vbase = re.sub('^(std|__gnu_cxx)::', r'\g<0>%s' %
                           _versioned_namespace, base)
            self.add(vbase + name, function)

    # Add a name using _GLIBCXX_BEGIN_NAMESPACE_CONTAINER.
    def add_container(self, base, name, function):
        self.add_version(base, name, function)
        self.add_version(base + '__cxx1998::', name, function)

    @staticmethod
    def get_basic_type(type):
        # If it points to a reference, get the reference.
        if type.code == gdb.TYPE_CODE_REF:
            type = type.target()

        # Get the unqualified type, stripped of typedefs.
        type = type.unqualified().strip_typedefs()

        return type.tag

    def __call__(self, val):
        typename = self.get_basic_type(val.type)
        if not typename:
            return None

        # All the types we match are template types, so we can use a
        # dictionary.
        match = self._compiled_rx.match(typename)
        if not match:
            return None

        basename = match.group(1)

        if val.type.code == gdb.TYPE_CODE_REF:
            if hasattr(gdb.Value, "referenced_value"):
                val = val.referenced_value()

        if basename in self._lookup:
            return self._lookup[basename].invoke(val)

        # Cannot find a pretty printer.  Return None.
        return None


libstdcxx_printer = None


class TemplateTypePrinter(object):
    """
    A type printer for class templates with default template arguments.

    Recognizes specializations of class templates and prints them without
    any template arguments that use a default template argument.
    Type printers are recursively applied to the template arguments.

    e.g. replace 'std::vector<T, std::allocator<T> >' with 'std::vector<T>'.
    """

    def __init__(self, name, defargs):
        self.name = name
        self._defargs = defargs
        self.enabled = True

    class _recognizer(object):
        """The recognizer class for TemplateTypePrinter."""

        def __init__(self, name, defargs):
            self.name = name
            self._defargs = defargs
            # self._type_obj = None

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
                defarg = self._defargs.get(n)
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
                    return type_str + '[]'  # array of unknown bound
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
        """Return a recognizer object for this type printer."""
        return self._recognizer(self.name, self._defargs)


def add_one_template_type_printer(obj, name, defargs):
    """
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
    printer = TemplateTypePrinter('std::' + name, defargs)
    gdb.types.register_type_printer(obj, printer)

    # Add type printer for same type in debug namespace:
    printer = TemplateTypePrinter('std::__debug::' + name, defargs)
    gdb.types.register_type_printer(obj, printer)

    if '__cxx11' not in name:
        # Add second type printer for same type in versioned namespace:
        ns = 'std::' + _versioned_namespace
        # PR 86112 Cannot use dict comprehension here:
        defargs = dict((n, d.replace('std::', ns))
                       for (n, d) in defargs.items())
        printer = TemplateTypePrinter(ns + name, defargs)
        gdb.types.register_type_printer(obj, printer)

        # Add type printer for same type in debug namespace:
        printer = TemplateTypePrinter('std::__debug::' + name, defargs)
        gdb.types.register_type_printer(obj, printer)


class FilteringTypePrinter(object):
    """
    A type printer that uses typedef names for common template specializations.

    Args:
        template (str): The class template to recognize.
        name (str): The typedef-name that will be used instead.
        targ1 (str, optional): The first template argument. Defaults to None.

    Checks if a specialization of the class template 'template' is the same type
    as the typedef 'name', and prints it as 'name' instead.

    e.g. if an instantiation of std::basic_istream<C, T> is the same type as
    std::istream then print it as std::istream.

    If targ1 is provided (not None), match only template specializations with
    this type as the first template argument, e.g. if template='basic_string'
    and targ1='char' then only match 'basic_string<char,...>' and not
    'basic_string<wchar_t,...>'. This rejects non-matching specializations
    more quickly, without needing to do GDB type lookups.
    """

    def __init__(self, template, name, targ1=None):
        self._template = template
        self.name = name
        self._targ1 = targ1
        self.enabled = True

    class _recognizer(object):
        """The recognizer class for FilteringTypePrinter."""

        def __init__(self, template, name, targ1):
            self._template = template
            self.name = name
            self._targ1 = targ1
            self._type_obj = None

        def recognize(self, type_obj):
            """
            If type_obj starts with self._template and is the same type as
            self.name then return self.name, otherwise None.
            """
            if type_obj.tag is None:
                return None

            if self._type_obj is None:
                if self._targ1 is not None:
                    s = '{}<{}'.format(self._template, self._targ1)
                    if not type_obj.tag.startswith(s):
                        # Filter didn't match.
                        return None
                elif not type_obj.tag.startswith(self._template):
                    # Filter didn't match.
                    return None

                try:
                    self._type_obj = gdb.lookup_type(
                        self.name).strip_typedefs()
                except:
                    pass

            if self._type_obj is None:
                return None

            t1 = gdb.types.get_basic_type(self._type_obj)
            t2 = gdb.types.get_basic_type(type_obj)
            if t1 == t2:
                return strip_inline_namespaces(self.name)

            # Workaround ambiguous typedefs matching both std:: and
            # std::__cxx11:: symbols.
            if self._template.split('::')[-1] == 'basic_string':
                s1 = self._type_obj.tag.replace('__cxx11::', '')
                s2 = type_obj.tag.replace('__cxx11::', '')
                if s1 == s2:
                    return strip_inline_namespaces(self.name)

            return None

    def instantiate(self):
        """Return a recognizer object for this type printer."""
        return self._recognizer(self._template, self.name, self._targ1)


def add_one_type_printer(obj, template, name, targ1=None):
    printer = FilteringTypePrinter('std::' + template, 'std::' + name, targ1)
    gdb.types.register_type_printer(obj, printer)
    if '__cxx11' not in template:
        ns = 'std::' + _versioned_namespace
        printer = FilteringTypePrinter(ns + template, ns + name, targ1)
        gdb.types.register_type_printer(obj, printer)


def register_type_printers(obj):
    global _use_type_printing

    if not _use_type_printing:
        return

    # Add type printers for typedefs std::string, std::wstring etc.
    for ch in (('', 'char'),
               ('w', 'wchar_t'),
               ('u8', 'char8_t'),
               ('u16', 'char16_t'),
               ('u32', 'char32_t')):
        add_one_type_printer(obj, 'basic_string', ch[0] + 'string', ch[1])
        add_one_type_printer(obj, '__cxx11::basic_string',
                             ch[0] + 'string', ch[1])
        # Typedefs for __cxx11::basic_string used to be in namespace __cxx11:
        add_one_type_printer(obj, '__cxx11::basic_string',
                             '__cxx11::' + ch[0] + 'string', ch[1])
        add_one_type_printer(obj, 'basic_string_view',
                             ch[0] + 'string_view', ch[1])

    # Add type printers for typedefs std::istream, std::wistream etc.
    for ch in (('', 'char'), ('w', 'wchar_t')):
        for x in ('ios', 'streambuf', 'istream', 'ostream', 'iostream',
                  'filebuf', 'ifstream', 'ofstream', 'fstream'):
            add_one_type_printer(obj, 'basic_' + x, ch[0] + x, ch[1])
        for x in ('stringbuf', 'istringstream', 'ostringstream',
                  'stringstream'):
            add_one_type_printer(obj, 'basic_' + x, ch[0] + x, ch[1])
            # <sstream> types are in __cxx11 namespace, but typedefs aren't:
            add_one_type_printer(obj, '__cxx11::basic_' + x, ch[0] + x, ch[1])

    # Add type printers for typedefs regex, wregex, cmatch, wcmatch etc.
    for abi in ('', '__cxx11::'):
        for ch in (('', 'char'), ('w', 'wchar_t')):
            add_one_type_printer(obj, abi + 'basic_regex',
                                 abi + ch[0] + 'regex', ch[1])
        for ch in ('c', 's', 'wc', 'ws'):
            add_one_type_printer(
                obj, abi + 'match_results', abi + ch + 'match')
            for x in ('sub_match', 'regex_iterator', 'regex_token_iterator'):
                add_one_type_printer(obj, abi + x, abi + ch + x)

    # Note that we can't have a printer for std::wstreampos, because
    # it is the same type as std::streampos.
    add_one_type_printer(obj, 'fpos', 'streampos')

    # Add type printers for <chrono> typedefs.
    for dur in ('nanoseconds', 'microseconds', 'milliseconds', 'seconds',
                'minutes', 'hours', 'days', 'weeks', 'years', 'months'):
        add_one_type_printer(obj, 'chrono::duration', 'chrono::' + dur)

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
    for ch in (('', 'char'),
               ('w', 'wchar_t'),
               ('u8', 'char8_t'),
               ('u16', 'char16_t'),
               ('u32', 'char32_t')):
        add_one_type_printer(obj, ns + 'basic_string_view',
                             ns + ch[0] + 'string_view', ch[1])

    # Do not show defaulted template arguments in class templates.
    add_one_template_type_printer(obj, 'unique_ptr',
                                  {1: 'std::default_delete<{0}>'})
    add_one_template_type_printer(obj, 'deque', {1: 'std::allocator<{0}>'})
    add_one_template_type_printer(
        obj, 'forward_list', {1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'list', {1: 'std::allocator<{0}>'})
    add_one_template_type_printer(
        obj, '__cxx11::list', {1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'vector', {1: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'map',
                                  {2: 'std::less<{0}>',
                                   3: 'std::allocator<std::pair<{0} const, {1}>>'})
    add_one_template_type_printer(obj, 'multimap',
                                  {2: 'std::less<{0}>',
                                   3: 'std::allocator<std::pair<{0} const, {1}>>'})
    add_one_template_type_printer(obj, 'set',
                                  {1: 'std::less<{0}>', 2: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'multiset',
                                  {1: 'std::less<{0}>', 2: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'unordered_map',
                                  {2: 'std::hash<{0}>',
                                   3: 'std::equal_to<{0}>',
                                   4: 'std::allocator<std::pair<{0} const, {1}>>'})
    add_one_template_type_printer(obj, 'unordered_multimap',
                                  {2: 'std::hash<{0}>',
                                   3: 'std::equal_to<{0}>',
                                   4: 'std::allocator<std::pair<{0} const, {1}>>'})
    add_one_template_type_printer(obj, 'unordered_set',
                                  {1: 'std::hash<{0}>',
                                   2: 'std::equal_to<{0}>',
                                   3: 'std::allocator<{0}>'})
    add_one_template_type_printer(obj, 'unordered_multiset',
                                  {1: 'std::hash<{0}>',
                                   2: 'std::equal_to<{0}>',
                                   3: 'std::allocator<{0}>'})


def register_libstdcxx_printers(obj):
    """Register libstdc++ pretty-printers with objfile Obj."""

    global _use_gdb_pp
    global libstdcxx_printer

    if _use_gdb_pp:
        gdb.printing.register_pretty_printer(obj, libstdcxx_printer)
    else:
        if obj is None:
            obj = gdb
        obj.pretty_printers.append(libstdcxx_printer)

    register_type_printers(obj)


def build_libstdcxx_dictionary():
    global libstdcxx_printer

    libstdcxx_printer = Printer("libstdc++-v6")

    # libstdc++ objects requiring pretty-printing.
    # In order from:
    # http://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/a01847.html
    libstdcxx_printer.add_version('std::', 'basic_string', StdStringPrinter)
    libstdcxx_printer.add_version(
        'std::__cxx11::', 'basic_string', StdStringPrinter)
    libstdcxx_printer.add_container('std::', 'bitset', StdBitsetPrinter)
    libstdcxx_printer.add_container('std::', 'deque', StdDequePrinter)
    libstdcxx_printer.add_container('std::', 'list', StdListPrinter)
    libstdcxx_printer.add_container('std::__cxx11::', 'list', StdListPrinter)
    libstdcxx_printer.add_container('std::', 'map', StdMapPrinter)
    libstdcxx_printer.add_container('std::', 'multimap', StdMapPrinter)
    libstdcxx_printer.add_container('std::', 'multiset', StdSetPrinter)
    libstdcxx_printer.add_version('std::', 'pair', StdPairPrinter)
    libstdcxx_printer.add_version('std::', 'priority_queue',
                                  StdStackOrQueuePrinter)
    libstdcxx_printer.add_version('std::', 'queue', StdStackOrQueuePrinter)
    libstdcxx_printer.add_version('std::', 'tuple', StdTuplePrinter)
    libstdcxx_printer.add_container('std::', 'set', StdSetPrinter)
    libstdcxx_printer.add_version('std::', 'stack', StdStackOrQueuePrinter)
    libstdcxx_printer.add_version('std::', 'unique_ptr', UniquePointerPrinter)
    libstdcxx_printer.add_container('std::', 'vector', StdVectorPrinter)
    # vector<bool>
    libstdcxx_printer.add_version('std::', 'locale', StdLocalePrinter)

    libstdcxx_printer.add_version('std::', 'integral_constant',
                                  StdIntegralConstantPrinter)
    libstdcxx_printer.add_version('std::', 'text_encoding',
                                  StdTextEncodingPrinter)

    if hasattr(gdb.Value, 'dynamic_type'):
        libstdcxx_printer.add_version('std::', 'error_code',
                                      StdErrorCodePrinter)
        libstdcxx_printer.add_version('std::', 'error_condition',
                                      StdErrorCodePrinter)

    # Printer registrations for classes compiled with -D_GLIBCXX_DEBUG.
    libstdcxx_printer.add('std::__debug::bitset', StdBitsetPrinter)
    libstdcxx_printer.add('std::__debug::deque', StdDequePrinter)
    libstdcxx_printer.add('std::__debug::list', StdListPrinter)
    libstdcxx_printer.add('std::__debug::map', StdMapPrinter)
    libstdcxx_printer.add('std::__debug::multimap', StdMapPrinter)
    libstdcxx_printer.add('std::__debug::multiset', StdSetPrinter)
    libstdcxx_printer.add('std::__debug::set', StdSetPrinter)
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

    libstdcxx_printer.add_version(
        'std::tr1::', 'shared_ptr', SharedPointerPrinter)
    libstdcxx_printer.add_version(
        'std::tr1::', 'weak_ptr', SharedPointerPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_map',
                                  Tr1UnorderedMapPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_set',
                                  Tr1UnorderedSetPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_multimap',
                                  Tr1UnorderedMapPrinter)
    libstdcxx_printer.add_version('std::tr1::', 'unordered_multiset',
                                  Tr1UnorderedSetPrinter)

    libstdcxx_printer.add_version('std::', 'initializer_list',
                                  StdInitializerListPrinter)
    libstdcxx_printer.add_version('std::', 'atomic', StdAtomicPrinter)
    libstdcxx_printer.add_version(
        'std::', 'basic_stringbuf', StdStringBufPrinter)
    libstdcxx_printer.add_version(
        'std::__cxx11::', 'basic_stringbuf', StdStringBufPrinter)
    for sstream in ('istringstream', 'ostringstream', 'stringstream'):
        libstdcxx_printer.add_version(
            'std::', 'basic_' + sstream, StdStringStreamPrinter)
        libstdcxx_printer.add_version(
            'std::__cxx11::', 'basic_' + sstream, StdStringStreamPrinter)

    libstdcxx_printer.add_version('std::chrono::', 'duration',
                                  StdChronoDurationPrinter)
    libstdcxx_printer.add_version('std::chrono::', 'time_point',
                                  StdChronoTimePointPrinter)

    # std::regex components
    libstdcxx_printer.add_version('std::__detail::', '_State',
                                  StdRegexStatePrinter)

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
                                  'path', StdPathPrinter)
    libstdcxx_printer.add_version('std::filesystem::__cxx11::',
                                  'path', StdPathPrinter)

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

    # C++20 components
    libstdcxx_printer.add_version(
        'std::', 'partial_ordering', StdCmpCatPrinter)
    libstdcxx_printer.add_version('std::', 'weak_ordering', StdCmpCatPrinter)
    libstdcxx_printer.add_version('std::', 'strong_ordering', StdCmpCatPrinter)
    libstdcxx_printer.add_version('std::', 'span', StdSpanPrinter)
    libstdcxx_printer.add_version('std::', 'basic_format_args',
                                  StdFormatArgsPrinter)
    for c in ['day', 'month', 'year', 'weekday', 'weekday_indexed', 'weekday_last',
              'month_day', 'month_day_last', 'month_weekday', 'month_weekday_last',
              'year_month', 'year_month_day', 'year_month_day_last',
              'year_month_weekday', 'year_month_weekday_last', 'hh_mm_ss']:
        libstdcxx_printer.add_version('std::chrono::', c,
                                      StdChronoCalendarPrinter)
    libstdcxx_printer.add_version('std::chrono::', 'time_zone',
                                  StdChronoTimeZonePrinter)
    libstdcxx_printer.add_version('std::chrono::', 'time_zone_link',
                                  StdChronoTimeZonePrinter)
    libstdcxx_printer.add_version('std::chrono::', 'zoned_time',
                                  StdChronoZonedTimePrinter)
    libstdcxx_printer.add_version('std::chrono::', 'leap_second',
                                  StdChronoLeapSecondPrinter)
    libstdcxx_printer.add_version(
        'std::chrono::', 'tzdb', StdChronoTzdbPrinter)
    # libstdcxx_printer.add_version('std::chrono::(anonymous namespace)', 'Rule',
    #                              StdChronoTimeZoneRulePrinter)

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
        libstdcxx_printer.add_container('std::', '_Bit_iterator',
                                        StdBitIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_Bit_const_iterator',
                                        StdBitIteratorPrinter)
        libstdcxx_printer.add_container('std::', '_Bit_reference',
                                        StdBitReferencePrinter)
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


build_libstdcxx_dictionary()
