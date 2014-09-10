# Xmethods for libstc++.

# Copyright (C) 2014 Free Software Foundation, Inc.

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
import gdb.xmethod
import re

matcher_name_prefix = 'libstdc++::'

# Xmethods for std::vector

class VectorSizeWorker(gdb.xmethod.XMethodWorker):
    def __init__(self):
        self.name = 'size'
        self.enabled = True

    def get_arg_types(self):
        return None

    def __call__(self, obj):
        return obj['_M_impl']['_M_finish'] - obj['_M_impl']['_M_start']

class VectorSubscriptWorker(gdb.xmethod.XMethodWorker):
    def __init__(self):
        self.name = 'operator[]'
        self.enabled = True

    def get_arg_types(self):
        return gdb.lookup_type('std::size_t')

    def __call__(self, obj, subscript):
        return obj['_M_impl']['_M_start'][subscript]

class VectorMethodsMatcher(gdb.xmethod.XMethodMatcher):
    def __init__(self):
        gdb.xmethod.XMethodMatcher.__init__(self,
                                            matcher_name_prefix + 'vector')
        self._subscript_worker = VectorSubscriptWorker()
        self._size_worker = VectorSizeWorker()
        self.methods = [self._subscript_worker, self._size_worker]

    def match(self, class_type, method_name):
        if not re.match('^std::vector<.*>$', class_type.tag):
            return None
        if method_name == 'operator[]' and self._subscript_worker.enabled:
            return self._subscript_worker
        elif method_name == 'size' and self._size_worker.enabled:
            return self._size_worker

# Xmethods for std::unique_ptr

class UniquePtrGetWorker(gdb.xmethod.XMethodWorker):
    def __init__(self):
        self.name = 'get'
        self.enabled = True

    def get_arg_types(self):
        return None

    def __call__(self, obj):
        return obj['_M_t']['_M_head_impl']

class UniquePtrDerefWorker(UniquePtrGetWorker):
    def __init__(self):
        UniquePtrGetWorker.__init__(self)
        self.name = 'operator*'

    def __call__(self, obj):
        return UniquePtrGetWorker.__call__(self, obj).dereference()

class UniquePtrMethodsMatcher(gdb.xmethod.XMethodMatcher):
    def __init__(self):
        gdb.xmethod.XMethodMatcher.__init__(self,
                                            matcher_name_prefix + 'unique_ptr')
        self._get_worker = UniquePtrGetWorker()
        self._deref_worker = UniquePtrDerefWorker()
        self.methods = [self._get_worker, self._deref_worker]

    def match(self, class_type, method_name):
        if not re.match('^std::unique_ptr<.*>$', class_type.tag):
            return None
        if method_name == 'operator*' and self._deref_worker.enabled:
            return self._deref_worker
        elif method_name == 'get' and self._get_worker.enabled:
            return self._get_worker

def register_libstdcxx_xmethods(locus):
    gdb.xmethod.register_xmethod_matcher(locus, VectorMethodsMatcher())
    gdb.xmethod.register_xmethod_matcher(locus, UniquePtrMethodsMatcher())
