// URL.java - A Uniform Resource Locator.

/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.*;
import java.util.Hashtable;
import java.util.StringTokenizer;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 4, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public final class URL implements Serializable
{
  private String protocol;
  private String authority;
  private String userInfo;
  private String host;
  private int port = -1;	// Initialize for constructor using context.
  private String file;
  private String query;
  private String ref;
  private int hashCode = 0;
  transient private URLStreamHandler handler;
  private static Hashtable handlers = new Hashtable();
  private static URLStreamHandlerFactory factory;

  private static final long serialVersionUID = -7627629688361524110L;

  /**
   * Creates an URL object from the given arguments
   *
   * @param protocol The protocol of the URL
   * @param host The host of the URL
   * @param port The port of the URL
   * @param file The file of the URL
   *
   * @exception MalformedURLException If an error occurs
   */
  public URL(String protocol, String host, int port, String file)
    throws MalformedURLException
  {
    this(protocol, host, port, file, null);
  }

  /**
   * Creates an URL object from the given arguments
   *
   * @param protocol The protocol of the URL
   * @param host The host of the URL
   * @param file The file of the URL
   *
   * @exception MalformedURLException If an error occurs
   */
  public URL(String protocol, String host, String file)
    throws MalformedURLException
  {
    this(protocol, host, -1, file, null);
  }

  /**
   * Creates an URL object from the given arguments
   *
   * @param protocol The protocol of the URL
   * @param host The host of the URL
   * @param port The port of the URL
   * @param file The file of the URL
   * @param handler The stream handler for the URL
   *
   * @exception MalformedURLException If an error occurs
   * @exception SecurityException If  a security manager exists and its
   * checkPermission method doesn't allow specifying a stream handler
   * explicitly
   *
   * @since 1.2
   */
  public URL(String protocol, String host, int port, String file,
    URLStreamHandler handler) throws MalformedURLException
  {
    if (protocol == null)
      throw new MalformedURLException("null protocol");
    this.protocol = protocol;

    if (handler != null)
      {
	SecurityManager s = System.getSecurityManager ();
	if (s != null)
	  s.checkPermission (new NetPermission ("specifyStreamHandler"));

        this.handler = handler;
      }
    else
      this.handler = setURLStreamHandler(protocol);

    if (this.handler == null)
      throw new MalformedURLException (
		      "Protocol handler not found: " + protocol);

    this.host = host;
    this.port = port;
    this.userInfo = null;
    this.authority = null;
    this.query = null;

    int hashAt = file.indexOf('#');
    if (hashAt < 0)
      {
	this.file = file;
	this.ref = null;
      }
    else
      {
	this.file = file.substring(0, hashAt);
	this.ref = file.substring(hashAt + 1);
      }
    hashCode = hashCode();			// Used for serialization.
  }

  /**
   * Creates an URL object from the given arguments
   * 
   * @param spec The string to parse an URL
   *
   * @exception MalformedURLException If an error occurs
   */
  public URL(String spec) throws MalformedURLException
  {
    this((URL) null, spec, (URLStreamHandler) null);
  }

  /**
   * Creates an URL object from the given arguments
   * 
   * @param context The context on which to parse the specification
   * @param spec The string to parse an URL
   *
   * @exception MalformedURLException If an error occurs
   */
  public URL(URL context, String spec) throws MalformedURLException
  {
    this(context, spec, (URLStreamHandler) null);
  }

  /**
   * Creates an URL from given arguments
   *
   * @param context The context in which to parse the specification
   * @param spec The string to parse as an URL
   * @param handler The stream handler for the URL
   *
   * @exception MalformedURLException If an error occurs
   * @exception SecurityException If  a security manager exists and its
   * checkPermission method doesn't allow specifying a stream handler
   * explicitly
   * 
   * @since 1.2
   */
  public URL(URL context, String spec, URLStreamHandler handler)
    throws MalformedURLException
  {
    /* A protocol is defined by the doc as the substring before a ':'
     * as long as the ':' occurs before any '/'.
     *
     * If context is null, then spec must be an absolute URL.
     *
     * The relative URL need not specify all the components of a URL.
     * If the protocol, host name, or port number is missing, the value
     * is inherited from the context.  A bare file component is appended
     * to the context's file.  The optional anchor is not inherited. 
     */

    // If this is an absolute URL, then ignore context completely.
    // An absolute URL must have chars prior to "://" but cannot have a colon
    // right after the "://".  The second colon is for an optional port value
    // and implies that the host from the context is used if available.
    int colon;
    if ((colon = spec.indexOf("://", 1)) > 0 &&
	! spec.regionMatches(colon, "://:", 0, 4))
      context = null;

    int slash;
    if ((colon = spec.indexOf(':')) > 0 &&
	(colon < (slash = spec.indexOf('/')) || slash < 0))
      {
	// Protocol specified in spec string.
	protocol = spec.substring(0, colon);
	if (context != null && context.protocol.equals(protocol))
	  {
	    // The 1.2 doc specifically says these are copied to the new URL.
	    host = context.host;
	    port = context.port;
	    file = context.file;
	    userInfo = context.userInfo;
	    authority = context.authority;
	    query = context.query;
	  }
      }
    else if (context != null)
      {
	// Protocol NOT specified in spec string.
	// Use context fields (except ref) as a foundation for relative URLs.
	colon = -1;
	protocol = context.protocol;
	host = context.host;
	port = context.port;
	file = context.file;
        userInfo = context.userInfo;
        authority = context.authority;
        query = context.query;
      }
    else	// Protocol NOT specified in spec. and no context available.
      throw new
	  MalformedURLException("Absolute URL required with null context");

    if (handler != null)
      {
	SecurityManager s = System.getSecurityManager ();
	if (s != null)
	  s.checkPermission (new NetPermission ("specifyStreamHandler"));

        this.handler = handler;
      }
    else
      this.handler = setURLStreamHandler(protocol);

    if (this.handler == null)
      throw new MalformedURLException("Protocol handler not found: "
				      + protocol);

    // JDK 1.2 doc for parseURL specifically states that any '#' ref
    // is to be excluded by passing the 'limit' as the indexOf the '#'
    // if one exists, otherwise pass the end of the string.
    int hashAt = spec.indexOf('#', colon + 1);
    this.handler.parseURL(this, spec, colon + 1,
			  hashAt < 0 ? spec.length() : hashAt);
    if (hashAt >= 0)
      ref = spec.substring(hashAt + 1);

    hashCode = hashCode();			// Used for serialization.
  }

  public boolean equals(Object obj)
  {
    if (obj == null || ! (obj instanceof URL))
      return false;

    URL uObj = (URL) obj;

    return handler.equals (this, uObj);
  }

  /**
   * Gets the contents of this URL
   *
   * @exception IOException If an error occurs
   *
   * @since 1.3
   */
  public final Object getContent() throws IOException
  {
    return openConnection().getContent();
  }

  /**
   * Gets the contents of this URL
   *
   * @exception IOException If an error occurs
   */
  public final Object getContent (Class[] classes) throws IOException
  {
    // FIXME: implement this
    return getContent();
  }

  public String getFile()
  {
    return file;
  }

  /**
   * Returns the path of the URL
   *
   * @since 1.3
   */
  public String getPath()
  {
    int quest = file.indexOf('?');
    return quest < 0 ? file : file.substring(0, quest);
  }

  /**
   * Returns the authority of the URL
   * 
   * @since 1.3
   */
  public String getAuthority()
  {
    return authority;
  }

  /**
   * Returns the host of the URL
   */
  public String getHost()
  {
    return host;
  }

  /**
   * Returns of port of the URL
   */
  public int getPort()
  {
    return port;
  }

  /**
   * Returns the default port of the URL
   */
  public int getDefaultPort()
  {
    return 0;
  }

  /**
   * Returns the protocol of the URL
   */
  public String getProtocol()
  {
    return protocol;
  }

  public String getRef()
  {
    return ref;
  }

  /**
   * Returns the user information of the URL
   */
  public String getUserInfo ()
  {
    int at = host.indexOf('@');
    return at < 0 ? null : host.substring(0, at);
  }

  /**
   * Returns the query of the URL
   */
  public String getQuery ()
  {
    return query;
  }

  public int hashCode()
  {
    // JCL book says this is computed using (only) the hashcodes of the 
    // protocol, host and file fields.  Empirical evidence indicates this
    // is probably XOR in JDK 1.1.  In JDK 1.2 it seems to be a sum including
    // the port.
    //
    // JDK 1.2 online doc infers that host could be null because it
    // explicitly states that file cannot be null but is silent on host.
    // A simple example with protocol "http" (hashcode 3213448), host null,
    // file "/" (hashcode 47) produced a hashcode (3213494) which appeared
    // to be the sum of the two hashcodes plus the port.  Another example
    // using "/index.html" for file bore this out; as well as "#" for file
    // (which was reduced to "" with a hashcode of zero).  A "" host also
    // causes the port number and the two hashcodes to be summed.

    if (hashCode != 0)
      return hashCode;		// Use cached value if available.
    else
      return handler.hashCode (this);
  }

  /**
   * Returns a URLConnection object that represents a connection to the remote
   * object referred to by the URL
   *
   * @exception IOException If an error occurs
   */
  public URLConnection openConnection() throws IOException
  {
    return handler.openConnection(this);
  }

  /**
   * Opens a connection to this URL and returns an InputStream for reading
   * from that connection
   *
   * @exception IOException If an error occurs
   */
  public final InputStream openStream() throws IOException
  {
    return openConnection().getInputStream();
  }

  public boolean sameFile(URL other)
  {
    return handler.sameFile(this, other);
  }

  /**
   * Sets the specified fields of the URL. This is not a public method so
   * that only URLStreamHandlers can modify URL fields. URLs are otherwise
   * constant
   */
  protected void set(String protocol, String host, int port, String file,
		     String ref)
  {
    // TBD: Theoretically, a poorly written StreamHandler could pass an
    // invalid protocol.  It will cause the handler to be set to null
    // thus overriding a valid handler.  Callers of this method should
    // be aware of this.
    this.handler = setURLStreamHandler(protocol);
    this.protocol = protocol;
    this.authority = null;
    this.userInfo = null;
    this.port = port;
    this.host = host;
    this.file = file;
    this.query = null;
    this.ref = ref;
    hashCode = hashCode();			// Used for serialization.
  }

  /**
   * Sets the specified fields of the URL. This is not a public method so
   * that only URLStreamHandlers can modify URL fields. URLs are otherwise
   * constant
   *
   * @since 1.3
   */
  protected void set(String protocol, String host, int port,
		     String authority, String userInfo,
		     String path, String query, String ref)
  {
    // TBD: Theoretically, a poorly written StreamHandler could pass an
    // invalid protocol.  It will cause the handler to be set to null
    // thus overriding a valid handler.  Callers of this method should
    // be aware of this.
    this.handler = setURLStreamHandler(protocol);
    this.protocol = protocol;
    if (userInfo == null)
      this.host = host;
    else
      this.host = userInfo + "@" + host;
    this.port = port;
    if (query == null)
      this.file = path;
    else
      this.file = path + "?" + query;
    this.ref = ref;
    hashCode = hashCode();			// Used for serialization.
  }

  /**
   * Sets an application's URLStreamHandlerFactory
   *
   * @exception Error If the application has already set a factory
   * @exception SecurityException If a security manager exists and its
   * checkSetFactory method doesn't allow the operation
   */
  public static synchronized void
	setURLStreamHandlerFactory(URLStreamHandlerFactory fac)
  {
    if (factory != null)
      throw new Error("URLStreamHandlerFactory already set");

    // Throw an exception if an extant security mgr precludes
    // setting the factory.
    SecurityManager s = System.getSecurityManager();
    if (s != null)
      s.checkSetFactory();
    factory = fac;
  }

  public String toExternalForm()
  {
    // Identical to toString().
    return handler.toExternalForm(this);
  }

  public String toString()
  {
    // Identical to toExternalForm().
    return handler.toExternalForm(this);
  }

  private URLStreamHandler setURLStreamHandler(String protocol)
  {
    URLStreamHandler handler;

    // See if a handler has been cached for this protocol.
    if ((handler = (URLStreamHandler) handlers.get(protocol)) != null)
      return handler;

    // If a non-default factory has been set, use it to find the protocol.
    if (factory != null)
      handler = factory.createURLStreamHandler(protocol);
    else if (protocol.equals ("core"))
      {
 	handler = new gnu.gcj.protocol.core.Handler ();
      }
    else if (protocol.equals ("file"))
      {
	// This is an interesting case.  It's tempting to think that we
	// could call Class.forName ("gnu.gcj.protocol.file.Handler") to
	// get the appropriate class.  Unfortunately, if we do that the
	// program will never terminate, because setURLStreamHandler is
	// eventually called by Class.forName.
	//
	// Treating "file" as a special case is the minimum that will
	// fix this problem.  If other protocols are required in a
	// statically linked application they will need to be handled in
	// the same way as "file".
	handler = new gnu.gcj.protocol.file.Handler ();
      }

    // Non-default factory may have returned null or a factory wasn't set.
    // Use the default search algorithm to find a handler for this protocol.
    if (handler == null)
      {
	// Get the list of packages to check and append our default handler
	// to it, along with the JDK specified default as a last resort.
	// Except in very unusual environments the JDK specified one shouldn't
	// ever be needed (or available).
	String propVal = System.getProperty("java.protocol.handler.pkgs");
	propVal = (propVal == null) ? "" : (propVal + "|");
	propVal = propVal + "gnu.gcj.protocol|sun.net.www.protocol";

	StringTokenizer pkgPrefix = new StringTokenizer(propVal, "|");
	do
	  {
	    String facName = pkgPrefix.nextToken() + "." + protocol +
				".Handler";
	    try
	      {
		handler =
		  (URLStreamHandler) Class.forName(facName).newInstance();
	      }
	    catch (Exception e)
	      {
		// Can't instantiate; handler still null, go on to next element.
	      }
	  } while ((handler == null ||
		    ! (handler instanceof URLStreamHandler)) &&
		   pkgPrefix.hasMoreTokens());
      }

    // Update the hashtable with the new protocol handler.
    if (handler != null)
      if (handler instanceof URLStreamHandler)
	handlers.put(protocol, handler);
      else
	handler = null;

    return handler;
  }

  private void readObject(ObjectInputStream ois)
    throws IOException, ClassNotFoundException
  {
    ois.defaultReadObject();
    this.handler = setURLStreamHandler(protocol);
    if (this.handler == null)
      throw new IOException("Handler for protocol " + protocol + " not found");
  }

  private void writeObject(ObjectOutputStream oos) throws IOException
  {
    oos.defaultWriteObject();
  }
}
