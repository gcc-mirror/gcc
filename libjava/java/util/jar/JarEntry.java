/* JarEntry.java - Represents an entry in a jar file
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

import java.io.IOException;
import java.security.cert.Certificate;
import java.util.zip.ZipEntry;

/**
 * Extension to a ZipEntry that contains manifest attributes and certificates.
 * Both the Atrributes and the Certificates can be null when not set.
 * Note that the <code>getCertificates()</code> method only returns a
 * valid value after all of the data of the entry has been read.
 * <p>
 * There are no public methods to set the attributes or certificate of an
 * Entru. Only JarEntries created by the classes in <code>java.util.jar</code>
 * will have these properties set.
 *
 * @since 1.2
 * @author Mark Wielaard (mark@klomp.org)
 */

public class JarEntry extends ZipEntry
{
  // (Package local) fields

  Attributes attr;
  Certificate certs[];

  // Constructors

  /**
   * Creates a new JarEntry with the specified name and no attributes or
   * or certificates. Calls <code>super(name)</code> so all other (zip)entry
   * fields are null or -1.
   *
   * @param name the name of the new jar entry
   * @exception NullPointerException when the supplied name is null
   * @exception IllegalArgumentException when the supplied name is longer
   * than 65535 bytes
   */
  public JarEntry(String name) throws NullPointerException,
    IllegalArgumentException
  {
    super(name);
    attr = null;
    certs = null;
  }

  /**
   * Creates a new JarEntry with the specified ZipEntry as template for
   * all properties of the entry. Both attributes and certificates will be
   * null.
   *
   * @param entry the ZipEntry whose fields should be copied
   */
  public JarEntry(ZipEntry entry)
  {
    super(entry);
    attr = null;
    certs = null;
  }

  /**
   * Creates a new JarEntry with the specified JarEntry as template for
   * all properties of the entry.
   *
   * @param entry the jarEntry whose fields should be copied
   */
  public JarEntry(JarEntry entry)
  {
    super(entry);
    try
      {
	attr = entry.getAttributes();
      }
    catch (IOException _)
      {
      }
    certs = entry.getCertificates();
  }

  // Methods

  /**
   * Returns a copy of the Attributes set for this entry.
   * When no Attributes are set in the manifest null is returned.
   *
   * @return a copy of the Attributes set for this entry
   * @exception IOException This will never be thrown. It is here for
   * binary compatibility.
   */
  public Attributes getAttributes() throws IOException
  {
    if (attr != null)
      {
	return (Attributes) attr.clone();
      }
    else
      {
	return null;
      }
  }

  /**
   * Returns a copy of the certificates set for this entry.
   * When no certificates are set or when not all data of this entry has
   * been read null is returned.
   * <p>
   * To make sure that this call returns a valid value you must read all
   * data from the JarInputStream for this entry.
   * When you don't need the data for an entry but want to know the
   * certificates that are set for the entry then you can skip all data by
   * calling <code>skip(entry.getSize())</code> on the JarInputStream for
   * the entry.
   *
   * @return a copy of the certificates set for this entry
   */
  public Certificate[] getCertificates()
  {
    if (certs != null)
      {
	return (Certificate[])certs.clone();
      }
    else
      {
	return null;
      }
  }
}
