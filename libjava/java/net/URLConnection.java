// URLConnection.java - Superclass of all communications links between
//			an application and a URL.

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.*;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.Hashtable;
import java.util.Map;
import java.util.StringTokenizer;
import java.security.Permission;
import java.security.AllPermission;
import gnu.gcj.io.MimeTypes;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 5, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  One guessContentTypeFrom... methods not implemented.
 *    getContent method assumes content type from response; see comment there.
 */

public abstract class URLConnection
{
  protected URL url;
  protected boolean doInput = true;
  protected boolean doOutput = false;
  protected boolean allowUserInteraction;
  protected boolean useCaches;
  protected long ifModifiedSince = 0L;
  protected boolean connected = false;
  private static boolean defaultAllowUserInteraction = false;
  private static boolean defaultUseCaches = true;
  private static FileNameMap fileNameMap;  // Set by the URLConnection subclass.
  private static ContentHandlerFactory factory;
  private static ContentHandler contentHandler;
  private static Hashtable handlers = new Hashtable();
  private static Locale locale; 
  private static SimpleDateFormat dateFormat1, dateFormat2, dateFormat3;
  private static boolean dateformats_initialized = false;

  /**
   * Creates a URL connection to a given URL. A real connection is not made.
   * Use #connect to do this.
   *
   * @param url The Object to create the URL connection to
   *
   * @see URLConnection:connect
   */
  protected URLConnection(URL url)
  {
    this.url = url;
    allowUserInteraction = defaultAllowUserInteraction;
    useCaches = defaultUseCaches;
  }

  /**
   * Creates a real connection to the object references by the URL given
   * to the constructor
   *
   * @exception IOException If an error occurs
   */
  public abstract void connect() throws IOException;

  /**
   * Returns ths URL to the object.
   */
  public URL getURL()
  {
    return url;
  }

  /**
   * Returns the value of the content-length header field
   */
  public int getContentLength()
  {
    return getHeaderFieldInt("content-length", -1);
  }

  /**
   * Returns the value of the content-type header field
   */
  public String getContentType()
  {
    return getHeaderField("content-type");
  }

  /**
   * Returns the value of the content-encoding header field
   */
  public String getContentEncoding()
  {
    return getHeaderField("content-encoding");
  }

  /**
   * Returns the value of the expires header field
   */
  public long getExpiration()
  {
    return getHeaderFieldDate("expiration", 0L);
  }

  /**
   * Returns the value of the date header field
   */
  public long getDate()
  {
    return getHeaderFieldDate("date", 0L);
  }

  /**
   * Returns the value of the last-modified header field
   */
  public long getLastModified()
  {
    return getHeaderFieldDate("last-modified", 0L);
  }

  /**
   * Returns the value of the n-th header field
   *
   * @param num The number of the header field
   */
  public String getHeaderField(int num)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * Returns the value of the header filed specified by name
   *
   * @param name The name of the header field
   */
  public String getHeaderField(String name)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * Returns a map of all sent header fields
   * 
   * @since 1.4
   */
  public Map getHeaderFields()
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * Returns the value of the header filed name as int.
   *
   * @param name The name of the header field
   * @param val The default value
   *
   * @return Returns the value of the header filed or the default value
   * if the field is missing or malformed
   */
  public int getHeaderFieldInt(String name, int val)
  {
    String str = getHeaderField(name);
    try
      {
	if (str != null)
	  val = Integer.parseInt(str);
      }
    catch (NumberFormatException e)
      {
	; // Do nothing; val is the default.
      }
    return val;
  }

  /**
   * Returns the value of a header field parsed as date. The result is then
   * number of milliseconds since January 1st, 1970 GMT.
   *
   * @param name The name of the header field
   * @param val The dafault date
   *
   * @return Returns the date value of the header filed or the default value
   * if the field is missing or malformed
   */
  public long getHeaderFieldDate(String name, long val)
  {
    if (! dateformats_initialized)
      initializeDateFormats();
    String str = getHeaderField(name);
    if (str != null)
      {
        Date date;
	if ((date = dateFormat1.parse(str, new ParsePosition(0))) != null)
	  val = date.getTime();
	else if ((date = dateFormat2.parse(str, new ParsePosition(0))) != null)
	  val = date.getTime();
	else if ((date = dateFormat3.parse(str, new ParsePosition(0))) != null)
	  val = date.getTime();
      }
    return val;
  }

  /**
   * Returns the key of the n-th header field
   *
   * @param num The number of the header field
   */
  public String getHeaderFieldKey(int num)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  /**
   * Retrieves the content of this URLConnection
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support the
   * content type
   */
  public Object getContent() throws IOException
  {
    // FIXME: Doc indicates that other criteria should be applied as
    // heuristics to determine the true content type, e.g. see 
    // guessContentTypeFromName() and guessContentTypeFromStream methods
    // as well as FileNameMap class & fileNameMap field & get/set methods.
    String cType = getContentType();
    contentHandler = setContentHandler(cType);
    if (contentHandler == null)
      return getInputStream();

    return contentHandler.getContent(this);
  }

  /**
   * Retrieves the content of this URLConnection
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support the
   * content type
   */
  public Object getContent(Class[] classes) throws IOException
  {
    // FIXME: implement this
    return getContent ();
  }

  /**
   * Returns a permission object representing the permission necessary to make
   * the connection represented by this object. This method returns null if no
   * permission is required to make the connection.
   *
   * @exception IOException If the computation of the permission requires
   * network or file I/O and an exception occurs while computing it
   */
  public Permission getPermission() throws IOException
  {
    // Subclasses may override this.
    return new java.security.AllPermission();
  }

  /**
   * Returns the input stream of the URL connection
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support input
   */
  public InputStream getInputStream() throws IOException
  {
    // Subclasses for specific protocols override this.
    throw new UnknownServiceException("Protocol " + url.getProtocol() +
			" does not support input.");
  }

  /**
   * Returns the output stream of the URL connection
   *
   * @exception IOException If an error occurs
   * @exception UnknownServiceException If the protocol does not support output
   */
  public OutputStream getOutputStream() throws IOException
  {
    // Subclasses for specific protocols override this.
    throw new UnknownServiceException("Protocol " + url.getProtocol() +
			" does not support output.");
  }

  /**
   * Returns a string representation of the URL connection object
   */
  public String toString()
  {
    return this.getClass().getName() + ":" + url.toString();
  }

  /**
   * Sets tha value of the doInput field.
   *
   * @param doinput The new value of the doInput field
   *
   * @exception IllegalStateException If already connected
   */
  public void setDoInput(boolean doinput)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    doInput = doinput;
  }

  /**
   * Returns the current value of the doInput field
   */
  public boolean getDoInput()
  {
    return doInput;
  }

  /**
   * Sets the value of the doOutput field
   *
   * @param dooutput The new value of the doOutput field
   *
   * @exception IllegalStateException If already connected
   */
  public void setDoOutput(boolean dooutput)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    doOutput = dooutput;
  }

  /**
   * Returns the current value of the doOutput field
   */
  public boolean getDoOutput()
  {
    return doOutput;
  }

  /**
   * Sets a new value to the allowUserInteraction field
   *
   * @param allowed The new value
   *
   * @exception IllegalStateException If already connected
   */
  public void setAllowUserInteraction(boolean allowed)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    allowUserInteraction = allowed;
  }

  /**
   * Returns the current value of the allowUserInteraction field
   */
  public boolean getAllowUserInteraction()
  {
    return allowUserInteraction;
  }

  /**
   * Sets the default value if the allowUserInteraction field
   *
   * @param allowed The new default value
   */
  public static void setDefaultAllowUserInteraction(boolean allowed)
  {
    defaultAllowUserInteraction = allowed;
  }

  /**
   * Returns the default value of the allowUserInteraction field
   */
  public static boolean getDefaultAllowUserInteraction()
  {
    return defaultAllowUserInteraction;
  }

  /**
   * Sets a new value to the useCaches field
   *
   * @param usecaches The new value
   *
   * @exception IllegalStateException If already connected
   */
  public void setUseCaches(boolean usecaches)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    useCaches = usecaches;
  }

  /**
   * The current value of the useCaches field
   */
  public boolean getUseCaches()
  {
    return useCaches;
  }

  /**
   * Sets the value of the ifModifiedSince field
   *
   * @param ifmodifiedsince The new value in milliseconds
   * since January 1, 1970 GMT
   *
   * @exception IllegalStateException If already connected
   */
  public void setIfModifiedSince(long ifmodifiedsince)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    ifModifiedSince = ifmodifiedsince;
  }

  /**
   * Returns the current value of the ifModifiedSince field
   */
  public long getIfModifiedSince()
  {
    return ifModifiedSince;
  }

  /**
   * Returns the default value of the useCaches field
   */
  public boolean getDefaultUseCaches()
  {
    return defaultUseCaches;
  }

  /**
   * Sets the default value of the useCaches field
   *
   * @param defaultusecaches The new default value
   */
  public void setDefaultUseCaches(boolean defaultusecaches)
  {
    defaultUseCaches = defaultusecaches;
  }

  /**
   * Sets a property specified by key to value.
   * 
   * @param key Key of the property to set
   * @param value Value of the Property to set
   *
   * @exception IllegalStateException If already connected
   * @exception NullPointerException If key is null
   *
   * @see URLConnection:getRequestProperty(String key)
   * @see URLConnection:addRequestProperty(String key, String value)
   */
  public void setRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    // Do nothing unless overridden by subclasses that support setting
    // header fields in the request.
  }

  /**
   * Sets a property specified by key to value. If the property key already
   * is assigned to a value it does nothing.
   * 
   * @param key Key of the property to add
   * @param value Value of the Property to add
   *
   * @exception IllegalStateException If already connected
   * @exception NullPointerException If key is null
   * 
   * @see URLConnection:getRequestProperty(String key)
   * @see URLConnection:setRequestProperty(String key, String value)
   * 
   * @since 1.4
   */
  public void addRequestProperty(String key, String value)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    if (getRequestProperty (key) == null)
      {
        setRequestProperty (key, value);
      }
  }

  /**
   * Returns a property value specified by key.
   *
   * @param key Key of the property to return
   *
   * @exception IllegalStateException If already connected
   *
   * @see URLConnection:setRequestProperty(String key, String value)
   * @see URLConnection:addRequestProperty(String key, String value)
   * 
   * @return Value of the property.
   */
  public String getRequestProperty(String key)
  {
    if (connected)
      throw new IllegalStateException ("Already connected");

    // Overridden by subclasses that support reading header fields from the
    // request.
    return null;
  }

  /**
   * Returns a map that contains all properties of the request
   *
   * @exception IllegalStateException If already connected
   *
   * @return The map of properties
   */
  public Map getRequestProperties()
  {
    // Overridden by subclasses that support reading header fields from the
    // request.
    return null;
  }

  /**
   * Defines a default request property
   *
   * @param key The key of the property
   * @param value The value of the property
   *
   * @deprecated 1.3 The method setRequestProperty should be used instead
   *
   * @see URLConnection:setRequestProperty
   */
  public static void setDefaultRequestProperty(String key, String value)
  {
    // Do nothing unless overridden by subclasses that support setting
    // default request properties.
  }

  /**
   * Returns the value of a default request property
   *
   * @param key The key of the default property
   *
   * @return The value of the default property or null if not available
   * 
   * @deprecated 1.3 The method getRequestProperty should be used instead
   *
   * @see URLConnection:getRequestProperty
   */
  public static String getDefaultRequestProperty(String key)
  {
    // Overridden by subclasses that support default request properties.
    return null;
  }

  /**
   * Sets a ContentHandlerFactory
   *
   * @param fac The ContentHandlerFactory
   *
   * @exception Error If the factory has already been defined
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   */
  public static void setContentHandlerFactory(ContentHandlerFactory fac)
  {
    if (factory != null)
      throw new Error("ContentHandlerFactory already set");

    // Throw an exception if an extant security mgr precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();
    factory = fac;
  }

  /**
   * Tries to determine the content type of an object, based on the
   * specified file name
   *
   * @param fname The filename to guess the content type from
   *
   * @specnote public since JDK 1.4
   */
  public static String guessContentTypeFromName(String fname)
  {
    int dot = fname.lastIndexOf (".");
    
    if (dot != -1)
      {
	if (dot == fname.length())
	  return ("application/octet-stream");
	else
	  fname = fname.substring (dot + 1);
      }
    
    String type = MimeTypes.getMimeTypeFromExtension (fname);
    
    if (type == null)
      return("application/octet-stream");

    return(type);
  }

  /**
   * Tries to guess the content type of an object, based on the characters
   * at the beginning of then input stream
   *
   * @param is The input stream to guess from
   *
   * @exception IOException If an error occurs
   */
  public static String guessContentTypeFromStream(InputStream is)
    throws IOException
  {
    is.mark(1024);
    // FIXME: Implement this. Use system mimetype informations (like "file").
    is.reset();
    return null;
  }

  /**
   * Returns a filename map (a mimetable)
   *
   * @since 1.2
   */
  public static FileNameMap getFileNameMap()
  {
    return fileNameMap;
  }

  /**
   * Sets a FileNameMap
   *
   * @param map The new FileNameMap
   *
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   * 
   * @since 1.2
   */
  public static void setFileNameMap(FileNameMap map)
  {
    // Throw an exception if an extant security mgr precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();

    fileNameMap = map;
  }

  private ContentHandler setContentHandler(String contentType)
  {
    ContentHandler handler;

    // No content type so just handle it as the default.
    if (contentType == null || contentType == "")
      return null;

    // See if a handler has been cached for this content type.
    // For efficiency, if a content type has been searched for but not
    // found, it will be in the hash table but as the contentType String
    // instead of a ContentHandler.
    if ((handler = (ContentHandler) handlers.get(contentType)) != null)
      if (handler instanceof ContentHandler)
	return handler;
      else
	return null;

    // If a non-default factory has been set, use it to find the content type.
    if (factory != null)
      handler = factory.createContentHandler(contentType);

    // Non-default factory may have returned null or a factory wasn't set.
    // Use the default search algorithm to find a handler for this content type.
    if (handler == null)
      {
	// Get the list of packages to check and append our default handler
	// to it, along with the JDK specified default as a last resort.
	// Except in very unusual environments the JDK specified one shouldn't
	// ever be needed (or available).
	String propVal = System.getProperty("java.content.handler.pkgs");
	propVal = (propVal == null) ? "" : (propVal + "|");
	propVal = propVal + "gnu.gcj.content|sun.net.www.content";

	// Replace the '/' character in the content type with '.' and
	// all other non-alphabetic, non-numeric characters with '_'.
	StringTokenizer pkgPrefix = new StringTokenizer(propVal, "|");
	char[] cArray = contentType.toCharArray();
	for (int i = 0; i < cArray.length; i++)
	  {
	    if (cArray[i] == '/')
	      cArray[i] = '.';
	    else if (! ((cArray[i] >= 'A' && cArray[i] <= 'Z') || 
			(cArray[i] >= 'a' && cArray[i] <= 'z') ||
			(cArray[i] >= '0' && cArray[i] <= '9')))
	      cArray[i] = '_';
	  }
	String contentClass = new String(cArray);

	// See if a class of this content type exists in any of the packages.
	do
	  {
	    String facName = pkgPrefix.nextToken() + "." + contentClass;
	    try
	      {
		handler =
		  (ContentHandler) Class.forName(facName).newInstance();
	      }
	    catch (Exception e)
	      {
		// Can't instantiate; handler still null, go on to next element.
	      }
	  } while ((handler == null ||
		    ! (handler instanceof ContentHandler)) &&
		   pkgPrefix.hasMoreTokens());
      }

    // Update the hashtable with the new content handler.
    if (handler != null && handler instanceof ContentHandler)
      {
	handlers.put(contentType, handler);
	return handler;
      }

    // For efficiency on subsequent searches, put a dummy entry in the hash
    // table for content types that don't have a non-default ContentHandler.
    handlers.put(contentType, contentType);
    return null;
  }
  
  // We don't put these in a static initializer, because it creates problems
  // with initializer co-dependency: SimpleDateFormat's constructors eventually 
  // depend on URLConnection (via the java.text.*Symbols classes).
  private synchronized void initializeDateFormats()
  {
    if (dateformats_initialized)
      return;
    locale = new Locale("En", "Us", "Unix");
    dateFormat1 = new SimpleDateFormat("EEE, dd MMM yyyy hh:mm:ss 'GMT'", 
                                       locale);
    dateFormat2 = new SimpleDateFormat("EEEE, dd-MMM-yy hh:mm:ss 'GMT'", 
                                       locale);
    dateFormat3 = new SimpleDateFormat("EEE MMM d hh:mm:ss yyyy", locale);
    dateformats_initialized = true;
  }
}
