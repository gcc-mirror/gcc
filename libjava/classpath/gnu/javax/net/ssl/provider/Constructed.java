/* Constructed.java -- Constructed type.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

/**
 * The base interface to SSL constructed types.
 *
 * <p><b>Contract for ByteBuffer-based constructed types:</b>
 *
 * <p>Most implementations of this interface supported by this library
 * take a "view" of an underlying ByteBuffer. The general contract of
 * such classes is that they <em>will not</em> modify the position or
 * limit of the buffer when doing read operations. That is, the position
 * of the underlying buffer <em>should</em> remain at 0 throughout the
 * lifetime of the object, and the limit should be either set to the
 * capacity of the buffer, or to the size of the object (in most cases,
 * the length of the protocol object is determined by the contents of
 * the object, so the limit isn't useful in such cases. Of course, if the
 * limit is set to something other than the object's length, it must be
 * larger than the object length).
 *
 * <p>Setter methods (usually in a class that implements the {@link Builder}
 * interface) may modify the limit, but the general contract remains that
 * the position remain at zero, and that the limit be at least as large as
 * the object length.
 *
 * <p>Thus, very often the code will use <em>absolute</em> getters and setters
 * for primitive types, or it will use the {@link java.nio.ByteBuffer#duplicate()}
 * method, and sometimes the {@link java.nio.ByteBuffer#slice()} method, and
 * will change the position or limit of the duplicate buffer.
 */
public interface Constructed
{
  /**
   * Returns the total length, in bytes, of this structure.
   *
   * @return The length of this structure.
   */
  int length();

  /**
   * Returns a printable representation of this structure, with the
   * given prefix prepended to each line.
   *
   * @param prefix The prefix to prepend to each line of the
   * output. This value may be <code>null</code>.
   * @return A printable representation of this structure.
   */
  String toString(String prefix);
}
