// URLStreamHandler.java - Superclass of all stream protocol handlers.

/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

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
  protected abstract URLConnection openConnection(URL u)
    throws java.io.IOException;

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
	file = "/" + spec.substring(start, limit);
      }
    else if (start < limit)
      {
	// Context is available, but only override it if there is a new file.
        // FIXME: unsure to what extent `/` and File.separatorChar
	//        can mix in URLs.  Ignore File.separatorChar for now.
	file = file.substring(0, file.lastIndexOf('/'))
		+ "/" + spec.substring(start, limit);
      }

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
	  file = file.substring(index + 3);
      }
    
    u.set(u.getProtocol(), host, port, file, u.getRef());
  }
  
  protected void setURL(URL u, String protocol, String host, int port,
			String file, String ref)
  {
    u.set(protocol, host, port, file, ref);
  }

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
