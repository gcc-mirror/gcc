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
 * @author Casey Marshall (rsdio@metastatic.org)
 */
public interface DER
{

  // Constants.
  // ------------------------------------------------------------------------

  public static final int UNIVERSAL   = 0x00;
  public static final int APPLICATION = 0x40;
  public static final int CONTEXT     = 0x80;
  public static final int PRIVATE     = 0xC0;

  public static final int CONSTRUCTED = 0x20;

  public static final int ANY               = 0x00;
  public static final int BOOLEAN           = 0x01;
  public static final int INTEGER           = 0x02;
  public static final int BIT_STRING        = 0x03;
  public static final int OCTET_STRING      = 0x04;
  public static final int NULL              = 0x05;
  public static final int OBJECT_IDENTIFIER = 0x06;
  public static final int REAL              = 0x09;
  public static final int ENUMERATED        = 0x0a;
  public static final int RELATIVE_OID      = 0x0d;

  public static final int SEQUENCE = 0x10;
  public static final int SET      = 0x11;

  public static final Object CONSTRUCTED_VALUE = new Object();

  public static final int NUMERIC_STRING   = 0x12;
  public static final int PRINTABLE_STRING = 0x13;
  public static final int T61_STRING       = 0x14;
  public static final int VIDEOTEX_STRING  = 0x15;
  public static final int IA5_STRING       = 0x16;
  public static final int GRAPHIC_STRING   = 0x19;
  public static final int ISO646_STRING    = 0x1A;
  public static final int GENERAL_STRING   = 0x1B;

  public static final int UTF8_STRING      = 0x0C;
  public static final int UNIVERSAL_STRING = 0x1C;
  public static final int BMP_STRING       = 0x1E;

  public static final int UTC_TIME         = 0x17;
  public static final int GENERALIZED_TIME = 0x18;
}
