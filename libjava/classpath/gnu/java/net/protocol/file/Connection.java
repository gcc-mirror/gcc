/* FileURLConnection.java -- URLConnection class for "file" protocol
   Copyright (C) 1998, 1999, 2003 Free Software Foundation, Inc.

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

package gnu.java.net.protocol.file;

import gnu.classpath.SystemProperties;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilePermission;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLConnection;
import java.security.Permission;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.net.MalformedURLException;

/**
 * This subclass of java.net.URLConnection models a URLConnection via
 * the "file" protocol.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Nic Ferrier (nferrier@tapsellferrier.co.uk)
 * @author Warren Levy (warrenl@cygnus.com)
 */
public class Connection extends URLConnection
{
  /**
   * Default permission for a file
   */
  private static final String DEFAULT_PERMISSION = "read";

  private static class StaticData
  {
    /**
     * HTTP-style DateFormat, used to format the last-modified header.
     */
    static SimpleDateFormat dateFormat
      = new SimpleDateFormat("EEE, dd MMM yyyy hh:mm:ss 'GMT'",
                             new Locale ("En", "Us", "Unix"));

    static String lineSeparator =
      SystemProperties.getProperty("line.separator");
  }

  
  /**
   * This is a File object for this connection
   */
  private File file;

  /**
   * If a directory, contains a list of files in the directory.
   */
  private byte[] directoryListing;

  /**
   * InputStream if we are reading from the file
   */
  private InputStream inputStream;

  /**
   * OutputStream if we are writing to the file
   */
  private OutputStream outputStream;
  
  /**
   * FilePermission to read the file
   */
  private FilePermission permission;

  /**
   * Calls superclass constructor to initialize.
   */
  public Connection(URL url)
  {
    super (url);

    permission = new FilePermission(getURL().getFile(), DEFAULT_PERMISSION);
  }
  
  /**
   * Unquote "%" + hex quotes characters
   *
   * @param str The string to unquote or null.
   *
   * @return The unquoted string or null if str was null.
   *
   * @exception MalformedURLException If the given string contains invalid
   * escape sequences.
   *
   */
  public static String unquote(String str) throws MalformedURLException
  {
    if (str == null)
      return null;

    final int MAX_BYTES_PER_UTF_8_CHAR = 3;
    byte[] buf = new byte[str.length()*MAX_BYTES_PER_UTF_8_CHAR];
    int pos = 0;
    for (int i = 0; i < str.length(); i++)
      {
	char c = str.charAt(i);
	if (c == '%')
	  {
	    if (i + 2 >= str.length())
	      throw new MalformedURLException(str + " : Invalid quoted character");
	    int hi = Character.digit(str.charAt(++i), 16);
	    int lo = Character.digit(str.charAt(++i), 16);
	    if (lo < 0 || hi < 0)
	      throw new MalformedURLException(str + " : Invalid quoted character");
	    buf[pos++] = (byte) (hi * 16 + lo);
	  }
 	else if (c > 127) {
	    try {
		byte [] c_as_bytes = Character.toString(c).getBytes("utf-8");
		final int c_length = c_as_bytes.length;
		System.arraycopy(c_as_bytes, 0, buf, pos, c_length);
		pos += c_length;
	    }
	    catch (java.io.UnsupportedEncodingException x2) {
		throw (Error) new InternalError().initCause(x2);
	    }
	}    
	else
	  buf[pos++] = (byte) c;
      }
    try
      {
	return new String(buf, 0, pos, "utf-8");
      }
    catch (java.io.UnsupportedEncodingException x2)
      {
	throw (Error) new InternalError().initCause(x2);
      }
  }

  /**
   * "Connects" to the file by opening it.
   */
  public void connect() throws IOException
  {
    // Call is ignored if already connected.
    if (connected)
      return;
    
    // If not connected, then file needs to be openned.
    file = new File (unquote(getURL().getFile()));

    if (! file.isDirectory())
      {
	if (doInput)
	  inputStream = new BufferedInputStream(new FileInputStream(file));
    
	if (doOutput)
	  outputStream = new BufferedOutputStream(new FileOutputStream(file));
      }
    else
      {
	if (doInput)
	  {
            inputStream = new ByteArrayInputStream(getDirectoryListing());
	  }

	if (doOutput)
	  throw new ProtocolException
	    ("file: protocol does not support output on directories");
      }
    
    connected = true;
  }

  /**
   * Populates the <code>directoryListing</code> field with a byte array
   * containing a representation of the directory listing.
   */
  byte[] getDirectoryListing()
    throws IOException
  {
    if (directoryListing == null)
      {
        ByteArrayOutputStream sink = new ByteArrayOutputStream();
        // NB uses default character encoding for this system
        Writer writer = new OutputStreamWriter(sink);
    
        String[] files = file.list();
    
        for (int i = 0; i < files.length; i++)
          {
            writer.write(files[i]);
            writer.write(StaticData.lineSeparator);
          }

        directoryListing = sink.toByteArray();
      }
    return directoryListing;  
  }
  
  /**
   * Opens the file for reading and returns a stream for it.
   *
   * @return An InputStream for this connection.
   *
   * @exception IOException If an error occurs
   */
  public InputStream getInputStream()
    throws IOException
  {
    if (!doInput)
      throw new ProtocolException("Can't open InputStream if doInput is false");
    
    if (!connected)
      connect();
    
    return inputStream;
  }

  /**
   * Opens the file for writing and returns a stream for it.
   *
   * @return An OutputStream for this connection.
   *
   * @exception IOException If an error occurs.
   */
  public OutputStream getOutputStream()
    throws IOException
  {
    if (!doOutput)
      throw new
	ProtocolException("Can't open OutputStream if doOutput is false");

    if (!connected)
      connect();
    
    return outputStream;
  }

  /**
   * Get the last modified time of the resource.
   *
   * @return the time since epoch that the resource was modified.
   */
  public long getLastModified()
  {
    try
      {
	if (!connected)
	  connect();

	return file.lastModified();
      }
    catch (IOException e)
      {
	return -1;
      }
  }
  
  /**
   *  Get an http-style header field. Just handle a few common ones. 
   */
  public String getHeaderField(String field)
  {
    try
      {
	if (!connected)
	  connect();

	if (field.equals("content-type"))
          return guessContentTypeFromName(file.getName());
	else if (field.equals("content-length"))
          {
            if (file.isDirectory())
              {
                return Integer.toString(getContentLength());
              }
            return Long.toString(file.length());
          }
	else if (field.equals("last-modified"))
	  {
	    synchronized (StaticData.dateFormat)
	      {
        	return StaticData.dateFormat.format(
                        new Date(file.lastModified()));
	      }
	  }
      }
    catch (IOException e)
      {
        // Fall through.
      }
    return null;
  }

  /**
   * Get the length of content.
   *
   * @return the length of the content.
   */
  public int getContentLength()
  {
    try
      {
	if (!connected)
	  connect();
        
        if (file.isDirectory())
          {
            return getDirectoryListing().length;
          }
	return (int) file.length();
      }
    catch (IOException e)
      {
	return -1;
      }
  }
  
  /**
   * This method returns a <code>Permission</code> object representing the
   * permissions required to access this URL.  This method returns a
   * <code>java.io.FilePermission</code> for the file's path with a read
   * permission.
   *
   * @return A Permission object
   */
  public Permission getPermission() throws IOException
  {
    return permission;
  }
}
