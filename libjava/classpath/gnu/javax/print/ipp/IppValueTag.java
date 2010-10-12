/* IppValueTag.java --
   Copyright (C) 2006 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package gnu.javax.print.ipp;

/**
 * IPP Value Tags as described in RFC 2910 section 3.5.2.
 * <p>
 * Attributes are always of a special type syntax (e.g. boolean or
 * interger attribute). These value types are specified by the tag
 * constants provided in this class. Beside the syntax types some
 * out of band values for reporting requested attributes as
 * unsupported, unknown etc. back to the client.
 * </p>
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class IppValueTag
{

  /** Out of band value for unsupported attributes. */
  public static final byte UNSUPPORTED = 0x10;

  // 0x11 reserved for 'default' for definition in a future
  //      IETF standards track document

  /** Out of band value for unknown attributes. */
  public static final byte UNKNOWN = 0x12;

  /** Out of band value for attribute without a value. */
  public static final byte NO_VALUE = 0x13;

  // 0x14-0x1F reserved for "out-of-band" values in future IETF
  //           standards track documents.

  // 0x20 reserved for definition in a future IETF
  //      standards track document

  /** Indicates a value of syntax type integer. */
  public static final byte INTEGER = 0x21;

  /** Indicates a value of syntax type boolean. */
  public static final byte BOOLEAN = 0x22;

  /** Indicates a value of syntax type enum (enumeration). */
  public static final byte ENUM = 0x23;

  // 0x24-0x2F reserved for integer types for definition in
  //           future IETF standards track documents

  /** Indicates a value of syntax type octect string. */
  public static final byte OCTECTSTRING_UNSPECIFIED = 0x30;

  /** Indicates a value of syntax type datetime. */
  public static final byte DATETIME = 0x31;

  /** Indicates a value of syntax type resolution. */
  public static final byte RESOLUTION = 0x32;

  /** Indicates a value of syntax type range of integers. */
  public static final byte RANGEOFINTEGER = 0x33;

  // 0x34 reserved for definition in a future IETF
  //      standards track document

  /** Indicates a value of syntax type text with language. */
  public static final byte TEXT_WITH_LANGUAGE = 0x35;

  /** Indicates a value of syntax type name with language. */
  public static final byte NAME_WITH_LANGUAGE = 0x36;

  // 0x37-0x3F reserved for octetString type definitions in
  //            future IETF standards track documents

  // 0x40 reserved for definition in a future IETF
  //      standards track document

  /** Indicates a value of syntax type text without language. */
  public static final byte TEXT_WITHOUT_LANGUAGE = 0x41;

  /** Indicates a value of syntax type name without language. */
  public static final byte NAME_WITHOUT_LANGUAGE = 0x42;

  // 0x43 reserved for definition in a future IETF
  //      standards track document

  /** Indicates a value of syntax type keyword. */
  public static final byte KEYWORD = 0x44;

  /** Indicates a value of syntax type URI. */
  public static final byte URI = 0x45;

  /** Indicates a value of syntax type URI scheme. */
  public static final byte URI_SCHEME = 0x46;

  /** Indicates a value of syntax type charset. */
  public static final byte CHARSET = 0x47;

  /** Indicates a value of syntax type language. */
  public static final byte NATURAL_LANGUAGE =0x48;

  /** Indicates a value of syntax type mime media. */
  public static final byte MIME_MEDIA_TYPE = 0x49;

  // 0x4A-0x5F reserved for character string type definitions
  //           in future IETF standards track documents


  private IppValueTag()
  {
    // not to be instantiated;
  }

  /**
   * Tests if given value corresponds to a
   * value tag value.
   *
   * @param value the value to test for
   * @return <code>true</code> if, <code>false</code> otherwise.
   */
  public static boolean isValueTag(byte value)
  {
    if(value == 0x10 || value == 0x12 || value == 0x13
        || value == 0x21 || value == 0x22 || value == 0x23
        || value == 0x30 || value == 0x31 || value == 0x32
        || value == 0x33 || value == 0x35 || value == 0x36
        || value == 0x41 || value == 0x42 || value == 0x44
        || value == 0x45 || value == 0x46 || value == 0x47
        || value == 0x48 || value == 0x49 )
      return true;

    return false;
  }

}
