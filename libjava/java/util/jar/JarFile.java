/* JarFile.java - Representation of a jar file
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

import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.util.Enumeration;

/**
 * Representation of a jar file.
 * <p>
 * Note that this class is not a subclass of java.io.File but a subclass of
 * java.util.zip.ZipFile and you can only read JarFiles with it (although
 * there are constructors that take a File object).
 * <p>
 * XXX - verification of Manifest signatures is not yet implemented.
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */
public class JarFile extends ZipFile
{
  // Fields

  /** The name of the manifest entry: META-INF/MANIFEST.MF */
  public static final String MANIFEST_NAME = "META-INF/MANIFEST.MF";

  /**
   * The manifest of this file, if any, otherwise null.
   * Read by the constructor.
   */
  private final Manifest manifest;

  /** Wether to verify the manifest and all entries */
  private boolean verify;

  // Constructors

  /**
   * Creates a new JarFile, tries to read the manifest and if the manifest
   * exists verifies it.
   *
   * @param fileName the name of the file to open
   * @exception FileNotFoundException if the fileName cannot be found
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(String fileName) throws FileNotFoundException, IOException
  {
    this(fileName, true);
  }

  /**
   * Creates a new JarFile, tries to read the manifest and if the manifest
   * exists and verify is true verfies it.
   *
   * @param fileName the name of the file to open
   * @param verify checks manifest and entries when true and a manifest
   * exists, when false no checks are made
   * @exception FileNotFoundException if the fileName cannot be found
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(String fileName, boolean verify) throws
    FileNotFoundException, IOException
  {
    super(fileName);
    manifest = readManifest();
    if (verify)
      verify();
  }

  /**
   * Creates a new JarFile, tries to read the manifest and if the manifest
   * exists verifies it.
   *
   * @param file the file to open as a jar file
   * @exception FileNotFoundException if the file does not exits
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(File file) throws FileNotFoundException, IOException
  {
    this(file, true);
  }

  /**
   * Creates a new JarFile, tries to read the manifest and if the manifest
   * exists and verify is true verfies it.
   *
   * @param file the file to open to open as a jar file
   * @param verify checks manifest and entries when true and a manifest
   * exists, when false no checks are made
   * @exception FileNotFoundException if file does not exist
   * @exception IOException if another IO exception occurs while reading
   */
  public JarFile(File file, boolean verify) throws FileNotFoundException,
    IOException
  {
    super(file);
    manifest = readManifest();
    if (verify)
      verify();
  }

  /**
   * Creates a new JarFile with the indicated mode, tries to read the
   * manifest and if the manifest exists and verify is true verfies it.
   *
   * @param file the file to open to open as a jar file
   * @param verify checks manifest and entries when true and a manifest
   * exists, when false no checks are made
   * @param mode either ZipFile.OPEN_READ or
   *             (ZipFile.OPEN_READ | ZipFile.OPEN_DELETE)
   * @exception FileNotFoundException if the file does not exist
   * @exception IOException if another IO exception occurs while reading
   * @exception IllegalArgumentException when given an illegal mode
   * 
   * @since 1.3
   */
  public JarFile(File file, boolean verify, int mode) throws
    FileNotFoundException, IOException, IllegalArgumentException
  {
    super(file, mode);
    manifest = readManifest();
    if (verify)
      verify();
  }

  // Methods

  /**
   * XXX - should verify the manifest file
   */
  private void verify()
  {
    // only check if manifest is not null
    if (manifest == null)
      {
	verify = false;
	return;
      }

    verify = true;
    // XXX - verify manifest
  }

  /**
   * Parses and returns the manifest if it exists, otherwise returns null.
   */
  private Manifest readManifest()
  {
    try
      {
	ZipEntry manEntry = super.getEntry(MANIFEST_NAME);
	if (manEntry != null)
	  {
	    InputStream in = super.getInputStream(manEntry);
	    return new Manifest(in);
	  }
	else
	  {
	    return null;
	  }
      }
    catch (IOException ioe)
      {
	return null;
      }
  }

  /**
   * Returns a enumeration of all the entries in the JarFile.
   * Note that also the Jar META-INF entries are returned.
   *
   * @exception IllegalStateException when the JarFile is already closed
   */
  public Enumeration entries() throws IllegalStateException
  {
    return new JarEnumeration(super.entries());
  }

  /**
   * Wraps a given Zip Entries Enumeration. For every zip entry a
   * JarEntry is created and the corresponding Attributes are looked up.
   * XXX - Should also look up the certificates.
   */
  private class JarEnumeration implements Enumeration
  {

    private final Enumeration entries;

    JarEnumeration(Enumeration e)
    {
      entries = e;
    }

    public boolean hasMoreElements()
    {
      return entries.hasMoreElements();
    }

    public Object nextElement()
    {
      ZipEntry zip = (ZipEntry) entries.nextElement();
      JarEntry jar = new JarEntry(zip);
      if (manifest != null)
	{
	  jar.attr = manifest.getAttributes(jar.getName());
	}
      // XXX jar.certs
      return jar;
    }
  }

  /**
   * XXX
   * It actually returns a JarEntry not a zipEntry
   * @param name XXX
   */
  public ZipEntry getEntry(String name)
  {
    ZipEntry entry = super.getEntry(name);
    if (entry != null)
      {
	JarEntry jarEntry = new JarEntry(entry);
	if (manifest != null)
	  {
	    jarEntry.attr = manifest.getAttributes(name);
	    // XXX jarEntry.certs
	  }
	return jarEntry;
      }
    return null;
  }

  /**
   * XXX should verify the inputstream
   * @param entry XXX
   * @exception ZipException XXX
   * @exception IOException XXX
   */
  public synchronized InputStream getInputStream(ZipEntry entry) throws
    ZipException, IOException
  {
    return super.getInputStream(entry);	// XXX verify
  }

  /**
   * Returns the JarEntry that belongs to the name if such an entry
   * exists in the JarFile. Returns null otherwise
   * Convenience method that just casts the result from <code>getEntry</code>
   * to a JarEntry.
   *
   * @param name the jar entry name to look up
   * @return the JarEntry if it exists, null otherwise
   */
  public JarEntry getJarEntry(String name)
  {
    return (JarEntry) getEntry(name);
  }

  /**
   * Returns the manifest for this JarFile or null when the JarFile does not
   * contain a manifest file.
   */
  public Manifest getManifest()
  {
    return manifest;
  }
}
