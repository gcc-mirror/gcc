/* DER.java -- Basic constants in DER sequences.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.security.der;

/**
 * The set of tags for DER types.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public interface DER
{
  int UNIVERSAL   = 0x00;
  int APPLICATION = 0x40;
  int CONTEXT     = 0x80;
  int PRIVATE     = 0xC0;

  int CONSTRUCTED = 0x20;

  int ANY               = 0x00;
  int BOOLEAN           = 0x01;
  int INTEGER           = 0x02;
  int BIT_STRING        = 0x03;
  int OCTET_STRING      = 0x04;
  int NULL              = 0x05;
  int OBJECT_IDENTIFIER = 0x06;
  int REAL              = 0x09;
  int ENUMERATED        = 0x0a;
  int RELATIVE_OID      = 0x0d;

  int SEQUENCE = 0x10;
  int SET      = 0x11;

  Object CONSTRUCTED_VALUE = new Object();

  int NUMERIC_STRING   = 0x12;
  int PRINTABLE_STRING = 0x13;
  int T61_STRING       = 0x14;
  int VIDEOTEX_STRING  = 0x15;
  int IA5_STRING       = 0x16;
  int GRAPHIC_STRING   = 0x19;
  int ISO646_STRING    = 0x1A;
  int GENERAL_STRING   = 0x1B;

  int UTF8_STRING      = 0x0C;
  int UNIVERSAL_STRING = 0x1C;
  int BMP_STRING       = 0x1E;

  int UTC_TIME         = 0x17;
  int GENERALIZED_TIME = 0x18;
}
