/* JarURLConnection.java -- Class for manipulating remote jar files
   Copyright (C) 1998, 2002, 2003 Free Software Foundation, Inc.

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

package java.net;

import java.io.IOException;
import java.security.cert.Certificate;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;


/**
 * This abstract class represents a common superclass for implementations
 * of jar URL's.  A jar URL is a special type of URL that allows JAR
 * files on remote systems to be accessed.  It has the form:
 * <p>
 * jar:&lt;standard URL pointing to jar filei&gt;!/file/within/jarfile
 * <p> for example:
 * <p>
 * jar:http://www.urbanophile.com/java/foo.jar!/com/urbanophile/bar.class
 * <p>
 * That example URL points to the file /com/urbanophile/bar.class in the
 * remote JAR file http://www.urbanophile.com/java/foo.jar.  The HTTP
 * protocol is used only as an example.  Any supported remote protocol
 * can be used.
 * <p>
 * This class currently works by retrieving the entire jar file into a
 * local cache file, then performing standard jar operations on it.
 * (At least this is true for the default protocol implementation).
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Kresten Krab Thorup (krab@gnu.org)
 * @date Aug 10, 1999.
 *
 * @since 1.2
 */
public abstract class JarURLConnection extends URLConnection
{
  /**
   * This is the actual URL that points the remote jar file.  This is parsed
   * out of the jar URL by the constructor.
   */
  private final URL jarFileURL;

  /**
   * The connection to the jar file itself. A JarURLConnection
   * can represent an entry in a jar file or an entire jar file.  In
   * either case this describes just the jar file itself.
   */
  protected URLConnection jarFileURLConnection;

  /**
   * This is the jar file "entry name" or portion after the "!/" in the
   * URL which represents the pathname inside the actual jar file.
   */
  private final String entryName;

  /**
   * Creates a JarURLConnection from an URL object
   *
   * @param url The URL object for this connection.
   *
   * @exception MalformedURLException If url is invalid
   *
   * @specnote This constructor is protected since JDK 1.4
   */
  protected JarURLConnection(URL url) throws MalformedURLException
  {
    super(url);

    if (! url.getProtocol().equals("jar"))
      throw new MalformedURLException(url + ": Not jar protocol.");

    String spec = url.getFile();
    int bang = spec.indexOf("!/");
    if (bang == -1)
      throw new MalformedURLException(url + ": No `!/' in spec.");

    // Extract the url for the jar itself.
    jarFileURL = new URL(spec.substring(0, bang));

    // Get the name of the entry, if any.
    entryName = spec.length() == (bang + 2) ? null : spec.substring(bang + 2);
  }

  /**
   * This method returns the "real" URL where the JarFile is located.
   * //****Is this right?*****
   *
   * @return The remote URL
   */
  public URL getJarFileURL()
  {
    return jarFileURL;
  }

  /**
   * Returns the "entry name" portion of the jar URL.  This is the portion
   * after the "!/" in the jar URL that represents the pathname inside the
   * actual jar file.
   *
   * @return The entry name.
   */
  public String getEntryName()
  {
    return entryName;
  }

  /**
   * Returns the entry in this jar file specified by the URL.
   *
   * @return The jar entry
   *
   * @exception IOException If an error occurs
   */
  public JarEntry getJarEntry() throws IOException
  {
    JarFile jarfile = null;

    if (entryName == null)
      return null;

    if (! doInput)
      throw new ProtocolException("Can't open JarEntry if doInput is false");

    try
      {
	jarfile = getJarFile ();
      }
    catch (IOException x)
      {
	/* ignore */
      }
    
    if (jarfile == null)
      {
	JarInputStream zis = new JarInputStream(
			jarFileURLConnection.getInputStream ());

	// This is hideous, we're doing a linear search for the thing...
	for (ZipEntry ent = zis.getNextEntry (); 
	     ent != null; 
	     ent = zis.getNextEntry ())
	  {
	    if (entryName.equals (ent.getName()))
	      {
		return new JarEntry (ent);
	      }
	  }
      }

    else
      {
	return jarfile.getJarEntry (entryName);
      }

    return null;
  }

  /**
   * Returns a read-only JarFile object for the remote jar file
   *
   * @return The JarFile object
   *
   * @exception IOException If an error occurs
   */
  public abstract JarFile getJarFile() throws IOException;

  /**
   * Returns an array of Certificate objects for the jar file entry specified
   * by this URL or null if there are none
   *
   * @return A Certificate array
   *
   * @exception IOException If an error occurs
   */
  public Certificate[] getCertificates() throws IOException
  {
    JarEntry entry = getJarEntry();

    return entry != null ? entry.getCertificates() : null;
  }

  /**
   * Returns the main Attributes for the jar file specified in the URL or
   * null if there are none
   *
   * @return The main Attributes for the JAR file for this connection
   *
   * @exception IOException If an error occurs
   */
  public Attributes getMainAttributes() throws IOException
  {
    Manifest manifest = getManifest();

    return manifest != null ? manifest.getMainAttributes() : null;
  }

  /**
   * Returns the Attributes for the Jar entry specified by the URL or null
   * if none
   *
   * @return The Attributes object for this connection if the URL for it points
   * to a JAR file entry, null otherwise
   *
   * @exception IOException If an error occurs
   */
  public Attributes getAttributes() throws IOException
  {
    JarEntry entry = getJarEntry();

    return entry != null ? entry.getAttributes() : null;
  }

  /**
   * Returns a Manifest object for this jar file, or null if there is no
   * manifest.
   *
   * @return The Manifest for this connection, or null if none
   *
   * @exception IOException If an error occurs
   */
  public Manifest getManifest() throws IOException
  {
    JarFile file = getJarFile();

    return file != null ? file.getManifest() : null;
  }
}
