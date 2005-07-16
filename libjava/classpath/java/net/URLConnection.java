/* URLConnection.java -- Abstract superclass for reading from URL's
   Copyright (C) 1998, 2002, 2003, 2004 Free Software Foundation, Inc.

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
import java.io.InputStream;
import java.io.OutputStream;
import java.security.AllPermission;
import java.security.Permission;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  One guessContentTypeFrom... methods not implemented.
 *    getContent method assumes content type from response; see comment there.
 */
/**
 * This class models a connection that retrieves the information pointed
 * to by a URL object.  This is typically a connection to a remote node
 * on the network, but could be a simple disk read.
 * <p>
 * A URLConnection object is normally created by calling the openConnection()
 * method of a URL object.  This method is somewhat misnamed because it does
 * not actually open the connection.  Instead, it return an unconnected
 * instance of this object.  The caller then has the opportunity to set
 * various connection options prior to calling the actual connect() method.
 * <p>
 * After the connection has been opened, there are a number of methods in
 * this class that access various attributes of the data, typically
 * represented by headers sent in advance of the actual data itself.
 * <p>
 * Also of note are the getInputStream and getContent() methods which allow
 * the caller to retrieve the actual data from the connection.  Note that
 * for some types of connections, writing is also allowed.  The setDoOutput()
 * method must be called prior to connecing in order to enable this, then
 * the getOutputStream method called after the connection in order to
 * obtain a stream to write the output to.
 * <p>
 * The getContent() method is of particular note.  This method returns an
 * Object that encapsulates the data returned.  There is no way do determine
 * the type of object that will be returned in advance.  This is determined
 * by the actual content handlers as described in the description of that
 * method.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 */
public abstract class URLConnection
{
  /**
   * This is an object that maps filenames to MIME types.  The interface
   * to do this is implemented by this class, so just create an empty
   * instance and store it here.
   */
  private static FileNameMap fileNameMap;

  /**
   * This is the ContentHandlerFactory set by the caller, if any
   */
  private static ContentHandlerFactory factory;

  /**
   * This is the default value that will be used to determine whether or
   * not user interaction should be allowed.
   */
  private static boolean defaultAllowUserInteraction;

  /**
   * This is the default flag indicating whether or not to use caches to
   * store the data returned from a server
   */
  private static boolean defaultUseCaches = true;

  /**
   * This variable determines whether or not interaction is allowed with
   * the user.  For example, to prompt for a username and password.
   */
  protected boolean allowUserInteraction;

  /**
   * Indicates whether or not a connection has been established to the
   * destination specified in the URL
   */
  protected boolean connected;

  /**
   * Indicates whether or not input can be read from this URL
   */
  protected boolean doInput = true;

  /**
   * Indicates whether or not output can be sent to this URL
   */
  protected boolean doOutput;

  /**
   * If this flag is set, the protocol is allowed to cache data whenever
   * it can (caching is not guaranteed). If it is not set, the protocol
   * must a get a fresh copy of the data.
   * <p>
   * This field is set by the setUseCaches method and returned by the
   * getUseCaches method.
   *
   * Its default value is that determined by the last invocation of
   * setDefaultUseCaches
   */
  protected boolean useCaches;

  /**
   * If this value is non-zero, then the connection will only attempt to
   * fetch the document pointed to by the URL if the document has been
   * modified more recently than the date set in this variable.  That date
   * should be specified as the number of seconds since 1/1/1970 GMT.
   */
  protected long ifModifiedSince;

  /**
   * This is the URL associated with this connection
   */
  protected URL url;
  private static SimpleDateFormat[] dateFormats;
  private static boolean dateformats_initialized;

  /* Cached ParsePosition, used when parsing dates. */
  private ParsePosition position;

  /**
   * Creates a URL connection to a given URL. A real connection is not made.
   * Use #connect to do this.
   *
   * @param url The Object to create the URL connection to
   *
   * @see URLConnection#connect()
   */
  protected URLConnection(URL url)
  {
    // Set up all our instance variables
    this.url = url;
    allowUserInteraction = defaultAllowUserInteraction;
    useCaches = defaultUseCaches;
  }

  /**
   * Establishes the actual connection to the URL associated with this
   * connection object
   *
   * @exception IOException if an error occurs
   */
  public abstract void connect() throws IOException;

  /**
   * Returns the URL object associated with this connection
   *
   * @return The URL for this connection.
   */
  public URL getURL()
  {
    return url;
  }

  /**
   * Returns the value of the content-length header field or -1 if the value
   * is not known or not present.
   *
   * @return The content-length field
   */
  public int getContentLength()
  {
    return getHeaderFieldInt("content-length", -1);
  }

  /**
   * Returns the the content-type of the data pointed to by the URL.  This
   * method first tries looking for a content-type header.  If that is not
   * present, it attempts to use the file name to determine the content's
   * MIME type.  If that is unsuccessful, the method returns null.  The caller
   * may then still attempt to determine the MIME type by a call to
   * guessContentTypeFromStream()
   *
   * @return The content MIME type
   */
  public String getContentType()
  {
    return getHeaderField("content-type");
  }

  /**
   * Returns the value of the content-encoding field or null if it is not
   * known or not present.
   *
   * @return The content-encoding field
   */
  public String getContentEncoding()
  {
    return getHeaderField("content-encoding");
  }

  /**
   * Returns the value of the expires header or 0 if not known or present.
   * If populated, the return value is number of seconds since midnight
   * on 1/1/1970 GMT.
   *
   * @return The expiration time.
   */
  public long getExpiration()
  {
    return getHeaderFieldDate("expires", 0L);
  }

  /**
   * Returns the date of the document pointed to by the URL as reported in
   * the date field of the header or 0 if the value is not present or not
   * known. If populated, the return value is number of seconds since
   * midnight on 1/1/1970 GMT.
   *
   * @return The document date
   */
  public long getDate()
  {
    return getHeaderFieldDate("date", 0L);
  }

  /**
   * Returns the value of the last-modified header field or 0 if not known known
   * or not present.  If populated, the return value is the number of seconds
   * since midnight on 1/1/1970.
   *
   * @return The last modified time
   */
  public long getLastModified()
  {
    return getHeaderFieldDate("last-modified", 0L);
  }

  /**
   * Return a String representing the header value at the specified index.
   * This allows the caller to walk the list of header fields.  The analogous
   * getHeaderFieldKey(int) method allows access to the corresponding key
   * for this header field
   *
   * @param index The index into the header field list to retrieve the value for
   *
   * @return The header value or null if index is past the end of the headers
   */
  public String getHeaderField(int index)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * Returns a String representing the value of the header field having
   * the named key.  Returns null if the header field does not exist.
   *
   * @param name The key of the header field
   *
   * @return The value of the header field as a String
   */
  public String getHeaderField(String name)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * Returns a map of all sent header fields
   *
   * @return all header fields
   *
   * @since 1.4
   */
  public Map getHeaderFields()
  {
    // Subclasses for specific protocols override this.
    return Collections.EMPTY_MAP;
  }

  /**
   * Returns the value of the named header field as an int.  If the field
   * is not present or cannot be parsed as an integer, the default value
   * will be returned.
   *
   * @param name The header field key to lookup
   * @param defaultValue The defaule value if the header field is not found
   * or can't be parsed.
   *
   * @return The value of the header field or the default value if the field
   * is missing or malformed
   */
  public int getHeaderFieldInt(String name, int defaultValue)
  {
    String value = getHeaderField(name);

    if (value == null)
      return defaultValue;

    try
      {
	return Integer.parseInt(value);
      }
    catch (NumberFormatException e)
      {
	return defaultValue;
      }
  }

  /**
   * Returns the value of the named header field as a date.  This date will
   * be the number of seconds since midnight 1/1/1970 GMT or the default
   * value if the field is not present or cannot be converted to a date.
   *
   * @param name The name of the header field
   * @param defaultValue The default date if the header field is not found
   * or can't be converted.
   *
   * @return Returns the date value of the header filed or the default value
   * if the field is missing or malformed
   */
  public long getHeaderFieldDate(String name, long defaultValue)
  {
    if (! dateformats_initialized)
      initializeDateFormats();

    if (position == null)
      position = new ParsePosition(0);

    long result = defaultValue;
    String str = getHeaderField(name);

    if (str != null)
      {
	for (int i = 0; i < dateFormats.length; i++)
	  {
	    SimpleDateFormat df = dateFormats[i];
	    position.setIndex(0);
	    position.setErrorIndex(0);
	    Date date = df.parse(str, position);
	    if (date != null)
	      return date.getTime();
	  }
      }

    return result;
  }

  /**
   * Returns a String representing the header key at the specified index.
   * This allows the caller to walk the list of header fields.  The analogous
   * getHeaderField(int) method allows access to the corresponding value for
   * this tag.
   *
   * @param index The index into the header field list to retrieve the key for.
   *
   * @return The header field key or null if index is past the end
   * of the headers.
   */
  public String getHeaderFieldKey(int index)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * This method returns the content of the document pointed to by the
   * URL as an Object.  The type of object depends on the MIME type of
   * the object and particular content hander loaded.  Most text type
   * content handlers will return a subclass of
   * <code>InputStream</code>.  Images usually return a class that
   * implements <code>ImageProducer</code>.  There is not guarantee
   * what type of object will be returned, however.
   *
   * <p>This class first determines the MIME type of the content, then
   * creates a ContentHandler object to process the input.  If the
   * <code>ContentHandlerFactory</code> is set, then that object is
   * called to load a content handler, otherwise a class called
   * gnu.java.net.content.&lt;content_type&gt; is tried.  If this
   * handler does not exist, the method will simple return the
   * <code>InputStream</code> returned by
   * <code>getInputStream()</code>.  Note that the default
   * implementation of <code>getInputStream()</code> throws a
   * <code>UnknownServiceException</code> so subclasses are encouraged
   * to override this method.</p>
   *
   * @return the content
   *
   * @exception IOException If an error with the connection occurs.
   * @exception UnknownServiceException If the protocol does not support the
   * content type at all.
   */
  public Object getContent() throws IOException
  {
    if (!connected)
      connect();

    // FIXME: Doc indicates that other criteria should be applied as
    // heuristics to determine the true content type, e.g. see 
    // guessContentTypeFromName() and guessContentTypeFromStream methods
    // as well as FileNameMap class & fileNameMap field & get/set methods.
    String type = getContentType();
    ContentHandler ch = getContentHandler(type);

    if (ch != null)
      return ch.getContent(this);

    return getInputStream();
  }

  /**
   * Retrieves the content of this URLConnection
   *
   * @param classes The allowed classes for the content
   *
   * @return the content
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support the
   * content type
   */
  public Object getContent(Class[] classes) throws IOException
  {
    // FIXME: implement this
    return getContent();
  }

  /**
   * This method returns a <code>Permission</code> object representing the
   * permissions required to access this URL.  This method returns
   * <code>java.security.AllPermission</code> by default.  Subclasses should
   * override it to return a more specific permission.  For example, an
   * HTTP URL should return an instance of <code>SocketPermission</code>
   * for the appropriate host and port.
   * <p>
   * Note that because of items such as HTTP redirects, the permission
   * object returned might be different before and after connecting.
   *
   * @return A Permission object
   *
   * @exception IOException If the computation of the permission requires
   * network or file I/O and an exception occurs while computing it
   */
  public Permission getPermission() throws IOException
  {
    // Subclasses may override this.
    return new AllPermission();
  }

  /**
   * Returns an InputStream for this connection.  As this default
   * implementation returns null, subclasses should override this method
   *
   * @return An InputStream for this connection
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support input
   */
  public InputStream getInputStream() throws IOException
  {
    // Subclasses for specific protocols override this.
    throw new UnknownServiceException("Protocol " + url.getProtocol()
                                      + " does not support input.");
  }

  /**
   * Returns an OutputStream for this connection.  As this default
   * implementation returns null, subclasses should override this method
   *
   * @return An OutputStream for this connection
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support output
   */
  public OutputStream getOutputStream() throws IOException
  {
    // Subclasses for specific protocols override this.
    throw new UnknownServiceException("Protocol " + url.getProtocol()
                                      + " does not support output.");
  }

  /**
   * The methods prints the value of this object as a String by calling the
   * toString() method of its associated URL.  Overrides Object.toString()
   *
   * @return A String representation of this object
   */
  public String toString()
  {
    return this.getClass().getName() + ":" + url.toString();
  }

  /**
   * Returns the value of a flag indicating whether or not input is going
   * to be done for this connection.  This default to true unless the
   * doOutput flag is set to false, in which case this defaults to false.
   *
   * @param input <code>true</code> if input is to be done,
   * <code>false</code> otherwise
   *
   * @exception IllegalStateException If already connected
   */
  public void setDoInput(boolean input)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    doInput = input;
  }

  /**
   * Returns the value of a flag indicating whether or not input is going
   * to be done for this connection.  This default to true unless the
   * doOutput flag is set to false, in which case this defaults to false.
   *
   * @return true if input is to be done, false otherwise
   */
  public boolean getDoInput()
  {
    return doInput;
  }

  /**
   * Returns a boolean flag indicating whether or not output will be done
   * on this connection.  The default value is false, so this method can
   * be used to override the default
   *
   * @param output ture if output is to be done, false otherwise
   *
   * @exception IllegalStateException If already connected
   */
  public void setDoOutput(boolean output)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    doOutput = output;
  }

  /**
   * Returns a boolean flag indicating whether or not output will be done
   * on this connection.  This defaults to false.
   *
   * @return true if output is to be done, false otherwise
   */
  public boolean getDoOutput()
  {
    return doOutput;
  }

  /**
   * Sets a boolean flag indicating whether or not user interaction is
   * allowed for this connection.  (For example, in order to prompt for
   * username and password info.
   *
   * @param allow true if user interaction should be allowed, false otherwise.
   *
   * @exception IllegalStateException If already connected
   */
  public void setAllowUserInteraction(boolean allow)
  {
    allowUserInteraction = allow;
  }

  /**
   * Returns a boolean flag indicating whether or not user interaction is
   * allowed for this connection.  (For example, in order to prompt for
   * username and password info.
   *
   * @return true if user interaction is allowed, false otherwise
   */
  public boolean getAllowUserInteraction()
  {
    return allowUserInteraction;
  }

  /**
   * Sets the default flag for whether or not interaction with a user
   * is allowed.  This will be used for all connections unless overridden
   *
   * @param allow true to allow user interaction, false otherwise
   */
  public static void setDefaultAllowUserInteraction(boolean allow)
  {
    defaultAllowUserInteraction = allow;
  }

  /**
   * Returns the default flag for whether or not interaction with a user
   * is allowed.  This will be used for all connections unless overridden
   *
   * @return true if user interaction is allowed, false otherwise
   */
  public static boolean getDefaultAllowUserInteraction()
  {
    return defaultAllowUserInteraction;
  }

  /**
   * Sets a boolean flag indicating whether or not caching will be used
   * (if possible) to store data downloaded via the connection.
   *
   * @param usecaches The new value
   *
   * @exception IllegalStateException If already connected
   */
  public void setUseCaches(boolean usecaches)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    useCaches = usecaches;
  }

  /**
   * Returns a boolean flag indicating whether or not caching will be used
   * (if possible) to store data downloaded via the connection.
   *
   * @return true if caching should be used if possible, false otherwise
   */
  public boolean getUseCaches()
  {
    return useCaches;
  }

  /**
   * Sets the ifModified since instance variable.  If this value is non
   * zero and the underlying protocol supports it, the actual document will
   * not be fetched unless it has been modified since this time.  The value
   * passed should  be 0 if this feature is to be disabled or the time expressed
   * as the number of seconds since midnight 1/1/1970 GMT otherwise.
   *
   * @param ifmodifiedsince The new value in milliseconds
   * since January 1, 1970 GMT
   *
   * @exception IllegalStateException If already connected
   */
  public void setIfModifiedSince(long ifmodifiedsince)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    ifModifiedSince = ifmodifiedsince;
  }

  /**
   * Returns the ifModified since instance variable.  If this value is non
   * zero and the underlying protocol supports it, the actual document will
   * not be fetched unless it has been modified since this time.  The value
   * returned will be 0 if this feature is disabled or the time expressed
   * as the number of seconds since midnight 1/1/1970 GMT otherwise
   *
   * @return The ifModifiedSince value
   */
  public long getIfModifiedSince()
  {
    return ifModifiedSince;
  }

  /**
   * Returns the default value used to determine whether or not caching
   * of documents will be done when possible.
   *
   * @return true if caches will be used, false otherwise
   */
  public boolean getDefaultUseCaches()
  {
    return defaultUseCaches;
  }

  /**
   * Sets the default value used to determine whether or not caching
   * of documents will be done when possible.
   *
   * @param use true to use caches if possible by default, false otherwise
   */
  public void setDefaultUseCaches(boolean use)
  {
    defaultUseCaches = use;
  }

  /**
   * Sets the value of the named request property
   *
   * @param key The name of the property
   * @param value The value of the property
   *
   * @exception IllegalStateException If already connected
   * @exception NullPointerException If key is null
   *
   * @see URLConnection#getRequestProperty(String key)
   * @see URLConnection#addRequestProperty(String key, String value)
   *
   * @since 1.4
   */
  public void setRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    if (key == null)
      throw new NullPointerException("key is null");

    // Do nothing unless overridden by subclasses that support setting
    // header fields in the request.
  }

  /**
   * Adds a new request property by a key/value pair.
   * This method does not overwrite existing properties with the same key.
   *
   * @param key Key of the property to add
   * @param value Value of the Property to add
   *
   * @exception IllegalStateException If already connected
   * @exception NullPointerException If key is null
   *
   * @see URLConnection#getRequestProperty(String key)
   * @see URLConnection#setRequestProperty(String key, String value)
   *
   * @since 1.4
   */
  public void addRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    if (key == null)
      throw new NullPointerException("key is null");

    // Do nothing unless overridden by subclasses that support adding
    // header fields in the request.
  }

  /**
   * Returns the value of the named request property.
   *
   * @param key The name of the property
   *
   * @return Value of the property
   *
   * @exception IllegalStateException If already connected
   *
   * @see URLConnection#setRequestProperty(String key, String value)
   * @see URLConnection#addRequestProperty(String key, String value)
   */
  public String getRequestProperty(String key)
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    // Overridden by subclasses that support reading header fields from the
    // request.
    return null;
  }

  /**
   * Returns an unmodifiable Map containing the request properties.
   *
   * @return The map of properties
   *
   * @exception IllegalStateException If already connected
   *
   * @since 1.4
   */
  public Map getRequestProperties()
  {
    if (connected)
      throw new IllegalStateException("Already connected");

    // Overridden by subclasses that support reading header fields from the
    // request.
    return Collections.EMPTY_MAP;
  }

  /**
   * Sets the default value of a request property.  This will be used
   * for all connections unless the value of the property is manually
   * overridden.
   *
   * @param key The request property name the default is being set for
   * @param value The value to set the default to
   *
   * @deprecated 1.3 The method setRequestProperty should be used instead.
   * This method does nothing now.
   *
   * @see URLConnection#setRequestProperty(String key, String value)
   */
  public static void setDefaultRequestProperty(String key, String value)
  {
    // This method does nothing since JDK 1.3.
  }

  /**
   * Returns the default value of a request property.  This will be used
   * for all connections unless the value of the property is manually
   * overridden.
   *
   * @param key The request property to return the default value of
   *
   * @return The value of the default property or null if not available
   *
   * @deprecated 1.3 The method getRequestProperty should be used instead.
   * This method does nothing now.
   *
   * @see URLConnection#getRequestProperty(String key)
   */
  public static String getDefaultRequestProperty(String key)
  {
    // This method does nothing since JDK 1.3.
    return null;
  }

  /**
   * Set's the ContentHandlerFactory for an application.  This can be called
   * once and only once.  If it is called again, then an Error is thrown.
   * Unlike for other set factory methods, this one does not do a security
   * check prior to setting the factory.
   *
   * @param factory The ContentHandlerFactory for this application
   *
   * @exception Error If the factory has already been defined
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   */
  public static synchronized void setContentHandlerFactory(ContentHandlerFactory factory)
  {
    if (URLConnection.factory != null)
      throw new Error("ContentHandlerFactory already set");

    // Throw an exception if an extant security mgr precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();

    URLConnection.factory = factory;
  }

  /**
   * Returns the MIME type of a file based on the name of the file.  This
   * works by searching for the file's extension in a list of file extensions
   * and returning the MIME type associated with it.  If no type is found,
   * then a MIME type of "application/octet-stream" will be returned.
   *
   * @param filename The filename to determine the MIME type for
   *
   * @return The MIME type String
   *
   * @specnote public since JDK 1.4
   */
  public static String guessContentTypeFromName(String filename)
  {
    return getFileNameMap().getContentTypeFor(filename.toLowerCase());
  }

  /**
   * Returns the MIME type of a stream based on the first few characters
   * at the beginning of the stream.  This routine can be used to determine
   * the MIME type if a server is believed to be returning an incorrect
   * MIME type.  This method returns "application/octet-stream" if it
   * cannot determine the MIME type.
   * <p>
   * NOTE: Overriding MIME types sent from the server can be obnoxious
   * to user's.  See Internet Exploder 4 if you don't believe me.
   *
   * @param is The InputStream to determine the MIME type from
   *
   * @return The MIME type
   *
   * @exception IOException If an error occurs
   */
  public static String guessContentTypeFromStream(InputStream is)
    throws IOException
  {
    return "application/octet-stream";
  }

  /**
   * This method returns the <code>FileNameMap</code> object being used
   * to decode MIME types by file extension.
   *
   * @return The <code>FileNameMap</code>.
   *
   * @since 1.2
   */
  public static synchronized FileNameMap getFileNameMap()
  {
    // Delayed initialization.
    if (fileNameMap == null)
      fileNameMap = new MimeTypeMapper();

    return fileNameMap;
  }

  /**
   * This method set the <code>FileNameMap</code> object being used
   * to decode MIME types by file extension.
   *
   * @param map The <code>FileNameMap</code>.
   *
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   *
   * @since 1.2
   */
  public static synchronized void setFileNameMap(FileNameMap map)
  {
    // Throw an exception if an extant security manager precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();

    fileNameMap = map;
  }

  private ContentHandler getContentHandler(String contentType)
  {
    // No content type so just handle it as the default.
    if (contentType == null || contentType.equals(""))
      return null;

    ContentHandler handler = null;

    // If a non-default factory has been set, use it.
    if (factory != null)
      handler = factory.createContentHandler(contentType);

    // Then try our default class.
    try
      {
	String typeClass = contentType.replace('/', '.');

	// Deal with "Content-Type: text/html; charset=ISO-8859-1".
	int parameterBegin = typeClass.indexOf(';');
	if (parameterBegin >= 1)
	  typeClass = typeClass.substring(0, parameterBegin);

	Class cls = Class.forName("gnu.java.net.content." + typeClass);
	Object obj = cls.newInstance();

	if (obj instanceof ContentHandler)
	  {
	    handler = (ContentHandler) obj;
	    return handler;
	  }
      }
    catch (ClassNotFoundException e)
      {
	// Ignore.
      }
    catch (InstantiationException e)
      {
	// Ignore.
      }
    catch (IllegalAccessException e)
      {
	// Ignore.
      }

    return handler;
  }
  
  // We don't put these in a static initializer, because it creates problems
  // with initializer co-dependency: SimpleDateFormat's constructors eventually 
  // depend on URLConnection (via the java.text.*Symbols classes).
  private static synchronized void initializeDateFormats()
  {
    if (dateformats_initialized)
      return;

    Locale locale = new Locale("En", "Us", "Unix");
    dateFormats = new SimpleDateFormat[3];
    dateFormats[0] =
      new SimpleDateFormat("EEE, dd MMM yyyy hh:mm:ss 'GMT'", locale);
    dateFormats[1] =
      new SimpleDateFormat("EEEE, dd-MMM-yy hh:mm:ss 'GMT'", locale);
    dateFormats[2] = new SimpleDateFormat("EEE MMM d hh:mm:ss yyyy", locale);
    dateformats_initialized = true;
  }
}
