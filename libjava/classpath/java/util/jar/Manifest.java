/* Manifest.java -- Reads, writes and manipulates jar manifest files
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

import gnu.java.util.jar.JarUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.Map;

/**
 * Reads, writes and manipulaties jar manifest files.
 * XXX
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */
public class Manifest implements Cloneable
{
  // Fields

  /** The main attributes of the manifest (jar file). */
  private final Attributes mainAttr;

  /** A map of atrributes for all entries described in this Manifest. */
  private final Map<String, Attributes> entries;

  // Constructors

  /**
   * Creates a new empty Manifest.
   */
  public Manifest()
  {
    mainAttr = new Attributes();
    entries = new Hashtable<String, Attributes>();
  }

  /**
   * Creates a Manifest from the supplied input stream.
   *
   * @see #read(InputStream)
   * @see #write(OutputStream)
   *
   * @param in the input stream to read the manifest from
   * @exception IOException when an i/o exception occurs or the input stream
   * does not describe a valid manifest
   */
  public Manifest(InputStream in) throws IOException
  {
    this();
    read(in);
  }

  /**
   * Creates a Manifest from another Manifest.
   * Makes a deep copy of the main attributes, but a shallow copy of
   * the other entries. This means that you can freely add, change or remove
   * the main attributes or the entries of the new manifest without effecting
   * the original manifest, but adding, changing or removing attributes from
   * a particular entry also changes the attributes of that entry in the
   * original manifest.
   *
   * @see #clone()
   * @param man the Manifest to copy from
   */
  public Manifest(Manifest man)
  {
    mainAttr = new Attributes(man.getMainAttributes());
    entries = new Hashtable<String, Attributes>(man.getEntries());
  }

  // Methods

  /**
   * Gets the main attributes of this Manifest.
   */
  public Attributes getMainAttributes()
  {
    return mainAttr;
  }

  /**
   * Gets a map of entry Strings to Attributes for all the entries described
   * in this manifest. Adding, changing or removing from this entries map
   * changes the entries of this manifest.
   */
  public Map<String, Attributes> getEntries()
  {
    return entries;
  }

  /**
   * Returns the Attributes associated with the Entry.
   * <p>
   * Implemented as:
   * <code>return (Attributes)getEntries().get(entryName)</code>
   *
   * @param entryName the name of the entry to look up
   * @return the attributes associated with the entry or null when none
   */
  public Attributes getAttributes(String entryName)
  {
    return getEntries().get(entryName);
  }

  /**
   * Clears the main attributes and removes all the entries from the
   * manifest.
   */
  public void clear()
  {
    mainAttr.clear();
    entries.clear();
  }

  /**
   * Read and merge a <code>Manifest</code> from the designated input stream.
   *
   * @param in the input stream to read from.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  public void read(InputStream in) throws IOException
  {
    JarUtils.readMFManifest(getMainAttributes(), getEntries(), in);
  }

  /**
   * Writes the contents of this <code>Manifest</code> to the designated
   * output stream. Line-endings are platform-independent and consist of the
   * 2-codepoint sequence <code>0x0D</code> and <code>0x0A</code>.
   *
   * @param out the output stream to write this <code>Manifest</code> to.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  public void write(OutputStream out) throws IOException
  {
    JarUtils.writeMFManifest(getMainAttributes(), getEntries(), out);
  }

  /**
   * Makes a deep copy of the main attributes, but a shallow copy of
   * the other entries. This means that you can freely add, change or remove
   * the main attributes or the entries of the new manifest without effecting
   * the original manifest, but adding, changing or removing attributes from
   * a particular entry also changes the attributes of that entry in the
   * original manifest. Calls <CODE>new Manifest(this)</CODE>.
   */
  public Object clone()
  {
    return new Manifest(this);
  }

  /**
   * Checks if another object is equal to this Manifest object.
   * Another Object is equal to this Manifest object if it is an instance of
   * Manifest and the main attributes and the entries of the other manifest
   * are equal to this one.
   */
  public boolean equals(Object o)
  {
    return (o instanceof Manifest) &&
      (mainAttr.equals(((Manifest) o).mainAttr)) &&
      (entries.equals(((Manifest) o).entries));
  }

  /**
   * Calculates the hash code of the manifest. Implemented by a xor of the
   * hash code of the main attributes with the hash code of the entries map.
   */
  public int hashCode()
  {
    return mainAttr.hashCode() ^ entries.hashCode();
  }

}
