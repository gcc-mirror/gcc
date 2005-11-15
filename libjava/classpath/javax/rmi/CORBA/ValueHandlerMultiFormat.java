/* ValueHandlerMultiFormat.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.rmi.CORBA;

import org.omg.CORBA.portable.OutputStream;

import java.io.Serializable;

/**
 * This interface extends the previous ValueHandler, supporting various stream
 * format versions. The {@link ValueHandler} can be casted into this interface
 * to access additional features.
 * 
 * @since 1.5
 * 
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface ValueHandlerMultiFormat
  extends ValueHandler
{
  /**
   * Get the maximal supported version for the value types, supported by
   * this value handler. The versions are integer numbers, the currently valid
   * values being 1 and 2.
   * 
   * These two versions differ in how the additional data, stored by the
   * writeObject method, are encoded.
   * <ul>
   * <li> For version 1 (GNU Classpath default), that data (if present) are
   * written "as is". </li>
   * <li>For version 2, this data fragment is enclosed within a CDR custom
   * valuetype with no codebase and repository Id "RMI:org.omg.custom.<class>"
   * where <class> is the fully-qualified name of the class whose writeObject
   * method is being invoked. If the object does not write any data via
   * writeObject method, the null valuetype (0x0) must be written.</li>
   * </ul>
   * As the version number is part of the value type record, there is no need
   * to the format control during the reading.
   * 
   * @return the maximal supported version.
   */
  byte getMaximumStreamFormatVersion();

  /**
   * Write the value type to the output stream using the given format version.
   * The older method {@link ValueHandler#writeValue} always uses the version 1.
   * 
   * @param output the stream, where the value should be written, must implement
   * {@link ValueOutputStream}.
   * @param value the value that should be written.
   * @param version the version of the format that must be used to write the
   * value.
   * 
   * @throws BAD_PARAM if the version number is less than 1 or greater than the
   * maximal supported version.
   */
  void writeValue(OutputStream output, Serializable value, byte version);
}
