// URLConnection.java - Superclass of all communications links between
//			an application and a URL.

/* Copyright (C) 1999, 2000  Red Hat, Inc.

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
import java.util.StringTokenizer;
import gnu.gcj.io.MimeTypes;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 5, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  One guessContentTypeFrom... methods not implemented.
 *	getContent method assumes content type from response; see comment there.
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

  protected URLConnection(URL url)
  {
    this.url = url;
    allowUserInteraction = defaultAllowUserInteraction;
    useCaches = defaultUseCaches;
  }

  public abstract void connect() throws IOException;

  public URL getURL()
  {
    return url;
  }

  public int getContentLength()
  {
    return getHeaderFieldInt("content-length", -1);
  }

  public String getContentType()
  {
    return getHeaderField("content-type");
  }

  public String getContentEncoding()
  {
    return getHeaderField("content-encoding");
  }

  public long getExpiration()
  {
    return getHeaderFieldDate("expiration", 0L);
  }

  public long getDate()
  {
    return getHeaderFieldDate("date", 0L);
  }

  public long getLastModified()
  {
    return getHeaderFieldDate("last-modified", 0L);
  }

  public String getHeaderField(int n)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

  public String getHeaderField(String name)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

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

  public String getHeaderFieldKey(int n)
  {
    // Subclasses for specific protocols override this.
    return null;
  }

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

// TODO12:  public Permission getPermission() throws IOException
//   {
//     // Subclasses may override this.
//     return java.security.AllPermission;
//   }

  public InputStream getInputStream() throws IOException
  {
    // Subclasses for specific protocols override this.
    throw new UnknownServiceException("Protocol " + url.getProtocol() +
			" does not support input.");
  }

  public OutputStream getOutputStream() throws IOException
  {
    // Subclasses for specific protocols override this.
    throw new UnknownServiceException("Protocol " + url.getProtocol() +
			" does not support output.");
  }

  public String toString()
  {
    return this.getClass().getName() + ":" + url.toString();
  }

  public void setDoInput(boolean doinput)
  {
    if (connected)
      throw new IllegalAccessError("Already connected");

    doInput = doinput;
  }

  public boolean getDoInput()
  {
    return doInput;
  }

  public void setDoOutput(boolean dooutput)
  {
    if (connected)
      throw new IllegalAccessError("Already connected");

    doOutput = dooutput;
    if (doOutput)
      doInput = false;
  }

  public boolean getDoOutput()
  {
    return doOutput;
  }

  public void setAllowUserInteraction(boolean allowuserinteraction)
  {
    if (connected)
      throw new IllegalAccessError("Already connected");

    allowUserInteraction = allowuserinteraction;
  }

  public boolean getAllowUserInteraction()
  {
    return allowUserInteraction;
  }

  public static void
    setDefaultAllowUserInteraction(boolean defaultallowuserinteraction)
  {
    defaultAllowUserInteraction = defaultallowuserinteraction;
  }

  public static boolean getDefaultAllowUserInteraction()
  {
    return defaultAllowUserInteraction;
  }

  public void setUseCaches(boolean usecaches)
  {
    if (connected)
      throw new IllegalAccessError("Already connected");

    useCaches = usecaches;
  }

  public boolean getUseCaches()
  {
    return useCaches;
  }

  public void setIfModifiedSince(long ifmodifiedsince)
  {
    if (connected)
      throw new IllegalAccessError("Already connected");

    ifModifiedSince = ifmodifiedsince;
  }

  public long getIfModifiedSince()
  {
    return ifModifiedSince;
  }

  public boolean getDefaultUseCaches()
  {
    return defaultUseCaches;
  }

  public void setDefaultUseCaches(boolean defaultusecaches)
  {
    defaultUseCaches = defaultusecaches;
  }

  public void setRequestProperty(String key, String value)
  {
    // Do nothing unless overridden by subclasses that support setting
    // header fields in the request.
  }

  public String getRequestProperty(String key)
  {
    // Overridden by subclasses that support reading header fields from the
    // request.
    return null;
  }

  public static void setDefaultRequestProperty(String key, String value)
  {
    // Do nothing unless overridden by subclasses that support setting
    // default request properties.
  }

  public static String getDefaultRequestProperty(String key)
  {
    // Overridden by subclasses that support default request properties.
    return null;
  }

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

  protected static String guessContentTypeFromName(String fname)
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

// TODO:  public static String guessContentTypeFromStream(InputStream is)
//          throws IOException
//   {
//   }

// TODO12:  protected void parseURL(URL u, String spec, int start, int limit)

  // JDK1.2
  public static FileNameMap getFileNameMap()
  {
    return fileNameMap;
  }

  // JDK1.2
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
