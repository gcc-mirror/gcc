/* JarInputStream.java - InputStream for reading jar files
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

import java.io.InputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * InputStream for reading jar files.
 * XXX - verification of the signatures in the Manifest file is not yet
 * implemented.
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */

public class JarInputStream extends ZipInputStream
{
  // Fields

  /** The manifest for this file or null when there was no manifest. */
  private Manifest manifest;

  /** The first real JarEntry for this file. Used by readManifest() to store
     an entry that isn't the manifest but that should be returned by
     getNextEntry next time it is called. Null when no firstEntry was read
     while searching for the manifest entry, or when it has already been
     returned by getNextEntry(). */
  private JarEntry firstEntry;

  // Constructors

  /**
   * Creates a new JarInputStream and tries to read the manifest.
   * If such a manifest is present the JarInputStream tries to verify all
   * the entry signatures while reading.
   *
   * @param in InputStream to read the jar from
   * @exception IOException when an error occurs when opening or reading
   */
  public JarInputStream(InputStream in) throws IOException
  {
    this(in, true);
  }

  /**
   * Creates a new JarInputStream and tries to read the manifest.
   * If such a manifest is present and verify is true, the JarInputStream
   * tries to verify all the entry signatures while reading.
   *
   * @param in InputStream to read the jar from
   * @param verify whether or not to verify the manifest entries
   * @exception IOException when an error occurs when opening or reading
   */
  public JarInputStream(InputStream in, boolean verify) throws IOException
  {
    super(in);
    readManifest(verify);
  }

  // Methods

  /**
   * Set the manifest if found. Skips all entries that start with "META-INF/"
   *
   * @param verify when true (and a Manifest is found) checks the Manifest,
   * when false no check is performed
   * @exception IOException if an error occurs while reading
   */
  private void readManifest(boolean verify) throws IOException
  {
    firstEntry = (JarEntry) super.getNextEntry();
    while ((firstEntry != null) &&
	   firstEntry.getName().startsWith("META-INF/"))
      {
	if (firstEntry.getName().equals(JarFile.MANIFEST_NAME))
	  {
	    manifest = new Manifest(this);
	  }
	firstEntry = (JarEntry) super.getNextEntry();
      }
    closeEntry();

    if (verify)
      {
	// XXX
      }
  }

  /**
   * Creates a JarEntry for a particular name and consults the manifest
   * for the Attributes of the entry.
   * Used by <code>ZipEntry.getNextEntry()</code>
   *
   * @param name the name of the new entry
   */
  protected ZipEntry createZipEntry(String name)
  {
    ZipEntry zipEntry = super.createZipEntry(name);
    JarEntry jarEntry = new JarEntry(zipEntry);
    if (manifest != null)
      {
	jarEntry.attr = manifest.getAttributes(name);
      }
    return jarEntry;
  }

  /**
   * Returns the Manifest for the jar file or null if there was no Manifest.
   */
  public Manifest getManifest()
  {
    return manifest;
  }

  /**
   * Returns the next entry or null when there are no more entries.
   * Does actually return a JarEntry, if you don't want to cast it yourself
   * use <code>getNextJarEntry()</code>. Does not return any entries found
   * at the beginning of the ZipFile that are special
   * (those that start with "META-INF/").
   *
   * @exception IOException if an IO error occurs when reading the entry
   */
  public ZipEntry getNextEntry() throws IOException
  {
    ZipEntry entry;
    if (firstEntry != null)
      {
	entry = firstEntry;
	firstEntry = null;
      }
    else
      {
	entry = super.getNextEntry();
      }
    return entry;
  }

  /**
   * Returns the next jar entry or null when there are no more entries.
   *
   * @exception IOException if an IO error occurs when reading the entry
   */
  public JarEntry getNextJarEntry() throws IOException
  {
    return (JarEntry) getNextEntry();
  }

  /**
   * XXX
   *
   * @param buf XXX
   * @param off XXX
   * @param len XXX
   * @return XXX
   * @exception IOException XXX
   */
  public int read(byte[]buf, int off, int len) throws IOException
  {
    // XXX if (verify) {}
    return super.read(buf, off, len);
  }
}
