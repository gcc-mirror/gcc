/* URLStreamHandler.java -- Abstract superclass for all protocol handlers
   Copyright (C) 1998, 1999, 2002, 2003 Free Software Foundation, Inc.

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


package java.net;

import java.io.IOException;
import java.io.File;

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * Status:  Believed complete and correct.
 */

/**
 * This class is the superclass of all URL protocol handlers.  The URL
 * class loads the appropriate protocol handler to establish a connection
 * to a (possibly) remote service (eg, "http", "ftp") and to do protocol
 * specific parsing of URL's.  Refer to the URL class documentation for
 * details on how that class locates and loads protocol handlers.
 * <p>
 * A protocol handler implementation should override the openConnection()
 * method, and optionally override the parseURL() and toExternalForm()
 * methods if necessary. (The default implementations will parse/write all
 * URL's in the same form as http URL's).  A protocol  specific subclass 
 * of URLConnection will most likely need to be created as well.
 * <p>
 * Note that the instance methods in this class are called as if they
 * were static methods.  That is, a URL object to act on is passed with
 * every call rather than the caller assuming the URL is stored in an
 * instance variable of the "this" object.
 * <p>
 * The methods in this class are protected and accessible only to subclasses.
 * URLStreamConnection objects are intended for use by the URL class only,
 * not by other classes (unless those classes are implementing protocols).
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Warren Levy (warrenl@cygnus.com)
 * 
 * @see URL
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
   * Returns a URLConnection for the passed in URL.  Note that this should
   * not actually create the connection to the (possibly) remote host, but
   * rather simply return a URLConnection object.  The connect() method of
   * URL connection is used to establish the actual connection, possibly
   * after the caller sets up various connection options.
   *
   * @param url The URL to get a connection object for
   *
   * @return A URLConnection object for the given URL
   *
   * @exception IOException If an error occurs
   */
  protected abstract URLConnection openConnection(URL u)
    throws IOException;

  /**
   * This method parses the string passed in as a URL and set's the
   * instance data fields in the URL object passed in to the various values
   * parsed out of the string.  The start parameter is the position to start
   * scanning the string.  This is usually the position after the ":" which
   * terminates the protocol name.  The end parameter is the position to
   * stop scanning.  This will be either the end of the String, or the
   * position of the "#" character, which separates the "file" portion of
   * the URL from the "anchor" portion.
   * <p>
   * This method assumes URL's are formatted like http protocol URL's, so 
   * subclasses that implement protocols with URL's the follow a different 
   * syntax should override this method.  The lone exception is that if
   * the protocol name set in the URL is "file", this method will accept
   * an empty hostname (i.e., "file:///"), which is legal for that protocol
   *
   * @param url The URL object in which to store the results
   * @param spec The String-ized URL to parse
   * @param start The position in the string to start scanning from
   * @param end The position in the string to stop scanning
   */
  protected void parseURL(URL url, String spec, int start, int end)
  {
    String host = url.getHost();
    int port = url.getPort();
    String file = url.getFile();
    String ref = url.getRef();
    
    if (spec.regionMatches (start, "//", 0, 2))
      {
	String genuineHost;
	int hostEnd;
	int colon, at_host;

	start += 2;
	int slash = spec.indexOf ('/', start);
	if (slash >= 0) 
	  hostEnd = slash;
        else
	  hostEnd = end;

	host = spec.substring (start, hostEnd);
	
	// We first need a genuine host name (with userinfo).
	// So we check for '@': if it's present check the port in the
	// section after '@' in the other case check it in the full string.
	// P.S.: We don't care having '@' at the beginning of the string.
	if ((at_host = host.indexOf ('@')) >= 0)
	  genuineHost = host.substring (at_host);
	else
	  genuineHost = host;

	// Look for optional port number.  It is valid for the non-port
	// part of the host name to be null (e.g. a URL "http://:80").
	// TBD: JDK 1.2 in this case sets host to null rather than "";
	// this is undocumented and likely an unintended side effect in 1.2
	// so we'll be simple here and stick with "". Note that
	// "http://" or "http:///" produce a "" host in JDK 1.2.
	if ((colon = genuineHost.indexOf (':')) >= 0)
	  {
	    try
	      {
		port = Integer.parseInt (genuineHost.substring (colon + 1));
	      }
	    catch (NumberFormatException e)
	      {
		; // Ignore invalid port values; port is already set to u's
		  // port.
	      }
	    // Now we must cut the port number in the original string.
	    if (at_host >= 0)
	      host = host.substring (0, at_host + colon);
	    else
	      host = host.substring (0, colon);
	  }
	file = null;
	start = hostEnd;
      } 
    else if (host == null) 
      host = "";

    if (file == null || file.length() == 0
	|| (start < end && spec.charAt(start) == '/')) 
      {
	// No file context available; just spec for file.
	// Or this is an absolute path name; ignore any file context.
	file = spec.substring(start, end);
	ref = null;
      } 
    else if (start < end)
      {
        // Context is available, but only override it if there is a new file.
        char sepChar = '/';
        int lastSlash = file.lastIndexOf (sepChar);
        if (lastSlash < 0 && File.separatorChar != sepChar
            && url.getProtocol ().equals ("file"))
          {
            // On Windows, even '\' is allowed in a "file" URL.
            sepChar = File.separatorChar;
            lastSlash = file.lastIndexOf (sepChar);
          }
        
        file = file.substring(0, lastSlash)
                + sepChar + spec.substring (start, end);

        if (url.getProtocol ().equals ("file"))
          {
            // For "file" URLs constructed relative to a context, we
            // need to canonicalise the file path.
            try
              {
		boolean endsWithSlash = file.charAt(file.length() - 1) == '/';
                file = new File (file).getCanonicalPath ();
		if (endsWithSlash
		    && file.charAt(file.length() - 1) != '/')
		  file += '/';
              }
            catch (IOException e)
              {
		// Do nothing.
              }
          }

	ref = null;
      }

    if (ref == null)
      {
	// Normally there should be no '#' in the file part,
	// but we are nice.
	int hash = file.indexOf('#');
	if (hash != -1)
	  {
	    ref = file.substring(hash + 1, file.length());
	    file = file.substring(0, hash);
	  }
      }

    // XXX - Classpath used to call PlatformHelper.toCanonicalForm() on
    // the file part. It seems like overhead, but supposedly there is some
    // benefit in windows based systems (it also lowercased the string).

    setURL(url, url.getProtocol(), host, port, file, ref);
  }
  
  /*
   * Canonicalize a filename.
   */
  private static String canonicalizeFilename(String file)
  {
    // XXX - GNU Classpath has an implementation that might be more appropriate
    // for Windows based systems (gnu.java.io.PlatformHelper.toCanonicalForm)

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
   * @return True if both URLs point to the same file, false otherwise.
   *
   * @specnote Now protected
   */
  protected boolean sameFile(URL url1, URL url2)
  {
    if (url1 == url2)
      return true;
    // This comparison is very conservative.  It assumes that any
    // field can be null.
    if (url1 == null || url2 == null)
      return false;
    int p1 = url1.getPort ();
    if (p1 == -1)
      p1 = url1.ph.getDefaultPort ();
    int p2 = url2.getPort ();
    if (p2 == -1)
      p2 = url2.ph.getDefaultPort ();
    if (p1 != p2)
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
   * This methods sets the instance variables representing the various fields
   * of the URL to the values passed in.
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
   *
   * @return True if both given URLs are equal, false otherwise.
   */
  protected boolean equals (URL url1, URL url2)
  {
    // This comparison is very conservative.  It assumes that any
    // field can be null.
    return (url1.getPort () == url2.getPort ()
	    && ((url1.getProtocol () == null && url2.getProtocol () == null)
		|| (url1.getProtocol () != null
			&& url1.getProtocol ().equals (url2.getProtocol ())))
	    && ((url1.getUserInfo () == null && url2.getUserInfo () == null)
                || (url1.getUserInfo () != null
			&& url1.getUserInfo ().equals(url2.getUserInfo ())))
	    && ((url1.getAuthority () == null && url2.getAuthority () == null)
                || (url1.getAuthority () != null
			&& url1.getAuthority ().equals(url2.getAuthority ())))
	    && ((url1.getHost () == null && url2.getHost () == null)
		|| (url1.getHost () != null
			&& url1.getHost ().equals(url2.getHost ())))
	    && ((url1.getPath () == null && url2.getPath () == null)
		|| (url1.getPath () != null
			&& url1.getPath ().equals (url2.getPath ())))
	    && ((url1.getQuery () == null && url2.getQuery () == null)
                || (url1.getQuery () != null
			&& url1.getQuery ().equals(url2.getQuery ())))
	    && ((url1.getRef () == null && url2.getRef () == null)
		|| (url1.getRef () != null
			&& url1.getRef ().equals(url2.getRef ()))));
  }

  /**
   * Compares the host components of two URLs.
   *
   * @param url1 The first URL.
   * @param url2 The second URL.
   *
   * @return True if both URLs contain the same host.
   *
   * @exception UnknownHostException If an unknown host is found
   */
  protected boolean hostsEqual (URL url1, URL url2)
  {
    InetAddress addr1 = getHostAddress (url1);
    InetAddress addr2 = getHostAddress (url2);

    if (addr1 != null || addr2 != null)
      return addr1.equals (addr2);

    String host1 = url1.getHost();
    String host2 = url2.getHost();
    
    if (host1 != null && host2 != null)
      return host1.equalsIgnoreCase (host2);

    return host1 == null && host2 == null;
  }

  /**
   * Get the IP address of our host. An empty host field or a DNS failure will
   * result in a null return.
   *
   * @param url The URL to return the host address for.
   *
   * @return The address of the hostname in url.
   */
  protected InetAddress getHostAddress (URL url)
  {
    String hostname = url.getHost ();

    if (hostname.equals(""))
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
   *
   * @return The default port number.
   */
  protected int getDefaultPort ()
  {
    return -1;
  }

  /**
   * Provides the default hash calculation. May be overidden by handlers for
   * other protocols that have different requirements for hashCode calculation.
   *
   * @param url The URL to calc the hashcode for.
   * 
   * @return The hashcode for the given URL.
   */
  protected int hashCode (URL url)
  {
    return url.getProtocol ().hashCode () +
           ((url.getHost () == null) ? 0 : url.getHost ().hashCode ()) +
	   url.getFile ().hashCode() +
	   url.getPort ();
  }

  /**
   * This method converts a URL object into a String.  This method creates
   * Strings in the mold of http URL's, so protocol handlers which use URL's
   * that have a different syntax should override this method
   *
   * @param url The URL object to convert
   *
   * @return A string representation of the url
   */
  protected String toExternalForm(URL u)
  {
    String protocol, host, file, ref, user;
    int port;

    protocol = u.getProtocol();

    // JDK 1.2 online doc infers that host could be null because it
    // explicitly states that file cannot be null, but is silent on host.
    host = u.getHost();
    if (host == null)
      host = "";

    port = u.getPort();
    file = u.getFile();
    ref = u.getRef();
    user = u.getUserInfo();

    // Guess a reasonable size for the string buffer so we have to resize
    // at most once.
    int size = protocol.length() + host.length() + file.length() + 24;
    StringBuffer sb = new StringBuffer(size);

    if (protocol != null && protocol.length() > 0)
      {
	sb.append(protocol);
	sb.append(":");
      }

    if (host.length() != 0)
      {
	sb.append("//");
	if (user != null && !"".equals(user))
	  sb.append(user).append('@');
	sb.append(host);

        // Append port if port was in URL spec.
        if (port >= 0)
          sb.append(':').append(port);
      }

    sb.append(file);

    if (ref != null)
      sb.append('#').append(ref);

    return sb.toString();
  }
}
