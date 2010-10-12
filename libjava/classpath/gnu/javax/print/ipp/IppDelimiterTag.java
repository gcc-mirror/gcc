/* IppDelimiterTag.java --
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
 * IPP Delimiter Tags as described in RFC 2910 section 3.5.1.
 * <p>
 * Every delimiter tag value can occur in the protocol field
 * begin-attribute-group-tag and indicates that the following
 * attributes will be part of the named group.<br>
 * The end-of-attributes-tag signals the end of the attributes
 * section in the IPP request/response and therefore the beginning
 * of the data section (if any).
 * </p>
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class IppDelimiterTag
{
  /** Start of the operation attributes group section. */
  public static final byte OPERATION_ATTRIBUTES_TAG = 0x01;

  /** Start of the job attributes group section. */
  public static final byte JOB_ATTRIBUTES_TAG = 0x02;

  /** End of the attributes section and begin of data section. */
  public static final byte END_OF_ATTRIBUTES_TAG = 0x03;

  /** Start of the printer attributes group section. */
  public static final byte PRINTER_ATTRIBUTES_TAG = 0x04;

  /** Start of the unsupported attributes group section. */
  public static final byte UNSUPPORTED_ATTRIBUTES_TAG = 0x05;


  // 0x00 reserved for definition in a future IETF
  // standards track document

  // 0x06-0x0f reserved for future delimiters in IETF
  // standards track documents

  private IppDelimiterTag()
  {
    // not to be instantiated
  }

  /**
   * Tests if given value corresponds to a
   * delimiter tag value.
   *
   * @param value the value to test for
   * @return <code>true</code> if, <code>false</code> otherwise.
   */
  public static boolean isDelimiterTag(byte value)
  {
    if (value >= 0x01 && value <= 0x05)
      return true;

    return false;
  }

}
