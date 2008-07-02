## Copyright (C) 2004, 2005 Free Software Foundation
## Written by Gary Benson <gbenson@redhat.com>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

"""Read Java(TM) class files."""

import cStringIO as StringIO
import struct

class Class:
    def __init__(self, arg):
        if hasattr(arg, "read"):
            self.fp = arg
        elif type(arg) == type(""):
            if arg.startswith("\xca\xfe\xba\xbe"):
                self.fp = StringIO.StringIO(arg)
            else:
                self.fp = open(arg, "r")
        else:
            raise TypeError, type(arg)

        magic = self._read_int()
        assert magic == 0xcafebabeL
        minor, major = self._read(">HH")        
        self.version = (major, minor)

        self.pool_integrity_checks = None
        try:
            assert False
        except AssertionError:
            self.pool_integrity_checks = []

        self._read_constants_pool()

        self.access_flags = self._read_short()
        self.name = self._read_reference_Class()
        self.super = self._read_reference_Class()

        self.interfaces = self._read_interfaces()
        self.fields = self._read_fieldsormethods()
        self.methods = self._read_fieldsormethods()
        self.attributes = self._read_attributes()

        if self.pool_integrity_checks is not None:
            for index, tag in self.pool_integrity_checks:
                assert self.constants[index][0] == tag

        del self.fp, self.pool_integrity_checks

    def __repr__(self):
        result = []
        attrs = [attr for attr in dir(self)
                 if not attr.startswith("_") and attr != "Member"]
        attrs.sort()
        for attr in attrs:
            result.append("%-13s %s" % (
                attr + ":", attr == "constants" and
                "<ELIDED>" or repr(getattr(self, attr))))
        return "\n".join(result)

    def _read_constants_pool(self):
        self.constants = {}
        skip = False
        for i in xrange(1, self._read_short()):
            if skip:
                skip = False
                continue
            tag = {
                1: "Utf8", 3: "Integer", 4: "Float", 5: "Long",
                6: "Double", 7: "Class", 8: "String", 9: "Fieldref",
                10: "Methodref", 11: "InterfaceMethodref",
                12: "NameAndType"}[self._read_byte()]
            skip = tag in ("Long", "Double") # crack crack crack!
            self.constants[i] = (tag, getattr(self, "_read_constant_" + tag)())

    def _read_interfaces(self):
        result = []
        for i in xrange(self._read_short()):
            result.append(self._read_reference_Class())
        return result

    def _read_fieldsormethods(self):
        result = []
        for i in xrange(self._read_short()):
            result.append(self.Member(self))
        return result

    class Member:
        def __init__(self, source):
            self.access_flags = source._read_short()
            self.name = source._read_reference_Utf8()
            self.descriptor = source._read_reference_Utf8()
            self.attributes = source._read_attributes()

        def __repr__(self):
            result = []
            attrs = [attr for attr in dir(self) if not attr.startswith("_")]
            attrs.sort()
            for attr in attrs:
                value = getattr(self, attr)
                if attr == "attributes" and value.has_key("Code"):
                    value = value.copy()
                    value.update({"Code": "<ELIDED>"})
                result.append("%-13s %s" % (
                    attr + ":", repr(value).replace(
                        "'Code': '<ELIDED>'", "'Code': <ELIDED>")))
            return ("\n%s" % (15 * " ")).join(result)

    def _read_attributes(self):
        result = {}
        for i in xrange(self._read_short()):
            name = self._read_reference_Utf8()
            data = self.fp.read(self._read_int())
            assert not result.has_key(name)
            result[name] = data
        return result

    # Constants pool reference reader convenience functions

    def _read_reference_Utf8(self):
        return self._read_references("Utf8")[0]

    def _read_reference_Class(self):
        return self._read_references("Class")[0]

    def _read_reference_Class_NameAndType(self):
        return self._read_references("Class", "NameAndType")

    def _read_references(self, *args):
        result = []
        for arg in args:
            index = self._read_short()
            if self.pool_integrity_checks is not None:
                self.pool_integrity_checks.append((index, arg))
            result.append(index)
        return result

    # Constants pool constant reader functions

    def _read_constant_Utf8(self):
        constant = self.fp.read(self._read_short())
        try:
            constant = constant.decode("utf-8")
        except UnicodeError:
            constant = _bork_utf8_decode(constant)
        try:
            constant = constant.encode("us-ascii")
        except UnicodeError:
            pass
        return constant

    def _read_constant_Integer(self):
        return self._read_int()

    def _read_constant_Float(self):
        return self._read(">f")[0]

    def _read_constant_Long(self):
        return self._read(">q")[0]

    def _read_constant_Double(self):
        return self._read(">d")[0]

    _read_constant_Class = _read_reference_Utf8
    _read_constant_String = _read_reference_Utf8
    _read_constant_Fieldref = _read_reference_Class_NameAndType
    _read_constant_Methodref = _read_reference_Class_NameAndType
    _read_constant_InterfaceMethodref = _read_reference_Class_NameAndType

    def _read_constant_NameAndType(self):
        return self._read_reference_Utf8(), self._read_reference_Utf8()

    # Generic reader functions

    def _read_int(self):
        # XXX how else to read 32 bits on a 64-bit box?
        h, l = map(long, self._read(">HH"))
        return (h << 16) + l

    def _read_short(self):
        return self._read(">H")[0]

    def _read_byte(self):
        return self._read("B")[0]

    def _read(self, fmt):
        return struct.unpack(fmt, self.fp.read(struct.calcsize(fmt)))

def _bork_utf8_decode(data):
    # more crack!
    bytes, unicode = map(ord, data), ""
    while bytes:
        b1 = bytes.pop(0)
        if b1 & 0x80:
            assert b1 & 0x40
            b2 = bytes.pop(0)
            assert b2 & 0xC0 == 0x80
            if b1 & 0x20:
                assert not b1 & 0x10
                b3 = bytes.pop(0)
                assert b3 & 0xC0 == 0x80
                unicode += unichr(
                    ((b1 & 0x0f) << 12) + ((b2 & 0x3f) << 6) + (b3 & 0x3f))
            else:
                unicode += unichr(((b1 & 0x1f) << 6) + (b2 & 0x3f))
        else:
            unicode += unichr(b1)
    return unicode

if __name__ == "__main__":
    print Class("/usr/share/katana/build/ListDependentClasses.class")

