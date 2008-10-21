/* FileObject.java --
   Copyright (C) 2008  Free Software Foundation, Inc.

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

package javax.tools;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URI;

/**
 * Abstraction for all kinds of file objects used by tools, e.g. regular files,
 * memory cache, or database data.
 *
 * @author Roman Kennke (roman@kennke.org)
 *
 * @since 1.6
 */
public interface FileObject
{

  /**
   * Returns a URI that represents this file object.
   *
   * @return a URI that represents this file object
   */
  URI toUri();

  /**
   * Returns a name for this file object. The exact name is implementation
   * dependent.
   *
   * @return a name for this file object
   */
  String getName();

  /**
   * Opens this file for reading and returns an input stream.
   *
   * @return an input stream to read this file object
   *
   * @throws IOException if an I/O error occured
   * @throws IllegalStateException if this file was opened for writing and
   *         does not support reading
   * @throws UnsupportedOperationException if this kind of file does not allow
   *         byte reading
   */
  InputStream openInputStream() throws IOException;

  /**
   * Opens this file for writing and returns an output stream.
   *
   * @return an output stream for writing this file object
   *
   * @throws IOException if an I/O error occurs
   * @throws IllegalStateException if this file was opened for reading and
   *         does not support writing
   * @throws UnsupportedOperationException if this kind of file does not allow
   *         byte writing
   */
  OutputStream openOutputStream() throws IOException;

  /**
   * Opens this file for reading and returns a reader.
   *
   * @param ignoreEncodingErrors <code>true</code> when encoding errors should be ignored
   *                             <code>false</code> otherwise
   * @return a reader for reading this file object
   *
   * @throws IOException if an I/O error occurs
   * @throws IllegalStateException if this file was opened for writing and
   *         does not support reading
   * @throws UnsupportedOperationException if this kind of file does not allow
   *         character reading
   */
  Reader openReader(boolean ignoreEncodingErrors) throws IOException;

  /**
   * Returns the character content of the file, if available. Any byte
   * that cannot be decoded will be replaced by the default replacement
   * character. A diagnostic may be reported, unless
   * <code>ignoreEncodingErrors</code> is <code>true</code>.
   * 
   * @param ignoreEncodingErrors <code>true</code> when encoding errors should be ignored
   *                             <code>false</code> otherwise
   * @return the character content, or <code>null</code> if not available
   *
   * @throws IOException if an I/O error occurs
   */
  CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException;

  /**
   * Opens this file for writer and returns a writer.
   *
   * @return a writer for writing this file object
   *
   * @throws IOException if an I/O error occurs
   * @throws IllegalStateException if this file was opened for reading and
   *         does not support writing
   * @throws UnsupportedOperationException if this kind of file does not allow
   *         character writing
   */
  Writer openWriter() throws IOException;

  /**
   * Returns the time when the file was last modified. The time is measured
   * like in <code>System.currentTimeMillis()</code>.
   *
   * @return the time when the file was last modified
   */
  long getLastModified();

  /**
   * Deletes this file object. In case of errors this returns
   * <code>false</code>.
   *
   * @return <code>true</code> when the file deletion was successful,
   *         <code>false</code> otherwise
   */
  boolean delete();
}
