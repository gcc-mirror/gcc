/* JarOutputStream.java - OutputStream for writing jar files
   Copyright (C) 2000 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.util.jar;

import java.io.OutputStream;
import java.io.IOException;
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
   * This implementation just calls <code>super.putNextEntre()</code>.
   *
   * @param entry The information for the next entry
   * @exception IOException when some unexpected I/O exception occurred
   */
  public void putNextEntry(ZipEntry entry) throws IOException
  {
    super.putNextEntry(entry);	// XXX
  }
}
