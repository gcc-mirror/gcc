// URLStreamHandler.java - Superclass of all stream protocol handlers.

/* Copyright (C) 1999, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.IOException;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date March 4, 1999.
 */

/**
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

public abstract class URLStreamHandler
{
  /**
   * Creates a URLStreamHander
   */
  public URLStreamHandler ()
  {
  }

  /**
   * Opens a connection to the object referenced by the URL argument.
   * This method should be overridden by a subclass.
   *
   * @exception IOException If an error occurs
   */
  protected abstract URLConnection openConnection(URL u)
    throws IOException;

  /**
   * Pasrses the given URL
   *
   * @param u The URL to parse
   * @param spec The specification to use
   * @param start The character index at which to begin parsing. This is just
   * past the ':' (if there is one) that specifies the determination of the
   * protocol name
   * @param limit The character position to stop parsing at. This is the end
   * of the string or the position of the "#" character, if present. All
   * information after the sharp sign indicates an anchor
   */
  protected void parseURL(URL u, String spec, int start, int limit)
  {
    String host = u.getHost();
    int port = u.getPort();
    String file = u.getFile();
    
    /* TBD: The JDK 1.2 doc specifically says that limit is the position
     * to stop parsing at and that it will be either the end of the string
     * or the position of '#'; thus the doc infers that this method does
     * not set the ref.
     */
    if (spec.regionMatches (start, "//", 0, 2))
      {
	int hostEnd;
	int colon;

	start += 2;
	int slash = spec.indexOf('/', start);
	if (slash >= 0) 
	  hostEnd = slash;
        else
	  hostEnd = limit;

	host = spec.substring (start, hostEnd);
	
	// Look for optional port number.  It is valid for the non-port
	// part of the host name to be null (e.g. a URL "http://:80").
	// TBD: JDK 1.2 in this case sets host to null rather than "";
	// this is undocumented and likely an unintended side effect in 1.2
	// so we'll be simple here and stick with "". Note that
	// "http://" or "http:///" produce a "" host in JDK 1.2.
	if ((colon = host.indexOf(':')) >= 0)
	  {
	    try
	      {
		port = Integer.parseInt(host.substring(colon + 1));
	      }
	    catch (NumberFormatException e)
	      {
		; // Ignore invalid port values; port is already set to u's
		  // port.
	      }
	    host = host.substring(0, colon);
	  }
	file = null;
	start = hostEnd;
      } 
    else if (host == null) 
      host = "";

    if (start < limit && spec.charAt(start) == '/') 
      {
	// This is an absolute path name; ignore any file context.
	file = spec.substring(start, limit);
      } 
    else if (file == null || file.length() <= 0)
      {
	// No file context available; just spec for file.
	file = spec.substring(start, limit);
      }
    else if (start < limit)
      {
	// Context is available, but only override it if there is a new file.
	file = file.substring(0, file.lastIndexOf('/'))
		+ '/' + spec.substring(start, limit);
      }

    u.set(u.getProtocol(), host, port, file, u.getRef());
  }
  
  private static String canonicalizeFilename(String file)
  {
    int index;

    // Replace "/./" with "/".  This probably isn't very efficient in
    // the general case, but it's probably not bad most of the time.
    while ((index = file.indexOf("/./")) >= 0)
      file = file.substring(0, index) + file.substring(index + 2);

    // Process "/../" correctly.  This probably isn't very efficient in
    // the general case, but it's probably not bad most of the time.
    while ((index = file.indexOf("/../")) >= 0)
      {
	// Strip of the previous directory - if it exists.
	int previous = file.lastIndexOf('/', index - 1);
	if (previous >= 0)
	  file = file.substring(0, previous) + file.substring(index + 3);
	else
	  break;
      }
    return file; 
  }

  /**
   * Compares two URLs, excluding the fragment component
   *
   * @param url1 The first url
   * @param url2 The second url to compare with the first
   * 
   * @specnote Now protected
   */
  protected boolean sameFile(URL url1, URL url2)
  {
    if (url1 == url2)
      return true;
    // This comparison is very conservative.  It assumes that any
    // field can be null.
    if (url1 == null || url2 == null || url1.getPort() != url2.getPort())
      return false;
    String s1, s2;
    s1 = url1.getProtocol();
    s2 = url2.getProtocol();
    if (s1 != s2 && (s1 == null || ! s1.equals(s2)))
      return false;
    s1 = url1.getHost();
    s2 = url2.getHost();
    if (s1 != s2 && (s1 == null || ! s1.equals(s2)))
      return false;
    s1 = canonicalizeFilename(url1.getFile());
    s2 = canonicalizeFilename(url2.getFile());
    if (s1 != s2 && (s1 == null || ! s1.equals(s2)))
      return false;
    return true;
  }

  /**
   * Sets the fields of the URL argument to the indicated values
   *
   * @param u The URL to modify
   * @param protocol The protocol to set
   * @param host The host name to et
   * @param port The port number to set
   * @param file The filename to set
   * @param ref The reference
   *
   * @exception SecurityException If the protocol handler of the URL is
   * different from this one
   *
   * @deprecated 1.2 Please use
   * #setURL(URL,String,String,int,String,String,String,String);
   */
  protected void setURL(URL u, String protocol, String host, int port,
			String file, String ref)
  {
    u.set(protocol, host, port, file, ref);
  }

  /**
   * Sets the fields of the URL argument to the indicated values
   *
   * @param u The URL to modify
   * @param protocol The protocol to set
   * @param host The host name to set
   * @param port The port number to set
   * @param authority The authority to set
   * @param userInfo The user information to set
   * @param path The path/filename to set
   * @param query The query part to set
   * @param ref The reference
   *
   * @exception SecurityException If the protocol handler of the URL is
   * different from this one
   */
  protected void setURL(URL u, String protocol, String host, int port,
			String authority, String userInfo, String path,
			String query, String ref)
  {
    u.set(protocol, host, port, authority, userInfo, path, query, ref);
  }

  /**
   * Provides the default equals calculation. May be overidden by handlers for
   * other protocols that have different requirements for equals(). This method
   * requires that none of its arguments is null. This is guaranteed by the
   * fact that it is only called by java.net.URL class.
   *
   * @param url1 An URL object
   * @param url2 An URL object
   */
  protected boolean equals (URL url1, URL url2)
  {
    // FIXME: implement this
    return false;
  }

  /**
   * Compares the host components of two URLs.
   *
   * @exception UnknownHostException If an unknown host is found
   */
  protected boolean hostsEqual (URL url1, URL url2)
  {
    // FIXME: implement this
    return false;
  }

  /**
   * Get the IP address of our host. An empty host field or a DNS failure will
   * result in a null return.
   */
  protected InetAddress getHostAddress (URL url)
  {
    String hostname = url.getHost ();

    if (hostname == "")
      return null;
    
    try
      {
        return InetAddress.getByName (hostname);
      }
    catch (UnknownHostException e)
      {
	return null;
      }
  }

  /**
   * Returns the default port for a URL parsed by this handler. This method is
   * meant to be overidden by handlers with default port numbers.
   */
  protected int getDefaultPort ()
  {
    return -1;
  }

  /**
   * Provides the default hash calculation. May be overidden by handlers for
   * other protocols that have different requirements for hashCode calculation.
   */
  protected int hashCode (URL url)
  {
    // FIXME: implement this
    return 0;
  }

  /**
   * Converts an URL of a specific protocol to a string
   *
   * @param u The URL to convert
   */
  protected String toExternalForm(URL u)
  {
    String resStr, host, file, ref;
    int port;

    resStr = u.getProtocol() + ":";
    host = u.getHost();
    port = u.getPort();
    file = u.getFile();
    ref = u.getRef();

    // JDK 1.2 online doc infers that host could be null because it
    // explicitly states that file cannot be null, but is silent on host.
    //
    // Note that this produces different results from JDK 1.2 as JDK 1.2
    // ignores a non-default port if host is null or "".  That is inconsistent
    // with the spec since the result of this method is spec'ed so it can be
    // used to construct a new URL that is equivalent to the original.
    if (host == null)
      host = "";
    if (port >= 0 || ! (host.length() == 0))
      resStr = resStr + "//" + host + (port < 0 ? "" : ":" + port);

    resStr = resStr + file;

    if (ref != null)
      resStr = resStr + "#" + ref;

    return resStr;
  }
}
