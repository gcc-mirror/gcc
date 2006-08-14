/* JarOutputStream.java - OutputStream for writing jar files
   Copyright (C) 2000, 2004 Free Software Foundation, Inc.

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

package java.util.jar;

import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * OutputStream for writing jar files.
 * A special ZipOutputStream that can take JarEntries and can have a optional
 * Manifest as first entry.
 *
 * @author Mark Wielaard (mark@klomp.org)
 */

public class JarOutputStream extends ZipOutputStream
{
  // Constructors

  /**
   * Creates a new JarOutputStream without a manifest entry.
   *
   * @param out the stream to create the new jar on
   * @exception IOException if something unexpected happend
   */
  public JarOutputStream(OutputStream out) throws IOException
  {
    this(out, null);
  }

  /**
   * Creates a new JarOutputStream with a manifest entry.
   * The manifest will be the first entry in the jar.
   *
   * @param out the stream to create the new jar on
   * @param man the manifest that should be put in the jar file or null
   * for no manifest entry
   * @exception IOException if something unexpected happend
   */
  public JarOutputStream(OutputStream out, Manifest man) throws IOException
  {
    super(out);
    if (man != null)
      writeManifest(man);
  }

  // Methods

  /**
   * Writes the manifest to a new JarEntry in this JarOutputStream with as
   * name JarFile.MANIFEST_NAME.
   *
   * @param manifest the non null manifest to be written
   * @exception IOException if something unexpected happend
   */
  private void writeManifest(Manifest manifest) throws IOException
  {
    // Create a new Jar Entry for the Manifest
    JarEntry entry = new JarEntry(JarFile.MANIFEST_NAME);
    putNextEntry(entry);
    manifest.write(this);
    closeEntry();
  }

  /**
   * Prepares the JarOutputStream for writing the next entry. 
   * This implementation just calls <code>super.putNextEntry()</code>.
   *
   * @param entry The information for the next entry
   * @exception IOException when some unexpected I/O exception occurred
   */
  public void putNextEntry(ZipEntry entry) throws IOException
  {
    super.putNextEntry(entry);	// XXX
  }
}
