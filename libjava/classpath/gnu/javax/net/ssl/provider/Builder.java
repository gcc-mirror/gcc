/* Builder.java -- builder interface for protocol objects.
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
exception statement from your version. */


package gnu.javax.net.ssl.provider;

import java.nio.ByteBuffer;

/**
 * The base interface for classes that build SSL protocol objects. The
 * general contract for Builder implementations is that they maintain a
 * buffer that grows to fit the object being built; the allocated size of
 * this buffer may be larger than the built object needs, but the general
 * effort will be not to allocate too large a buffer.
 * 
 * <p>Once the object is built, through various <em>setters</em> for
 * the object's attributes, the final buffer may be retrieved with the
 * {@link #buffer()} method.
 * 
 * @author Casey Marshall (csm@gnu.org)
 */
public interface Builder extends Constructed
{
  /**
   * Returns the final buffer, possibly containing the built object. The
   * returned buffer will be "trimmed" to size: its position will be zero,
   * and its limit and capacity set to the length of the built object.
   * 
   * @return The underlying buffer.
   */
  ByteBuffer buffer();
}
