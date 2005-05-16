/* URI.java -- An URI class
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>
 * A URI instance represents that defined by 
 * <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC2396</a>,
 * with some deviations.
 * </p>
 * <p>
 * At its highest level, a URI consists of:
 * </p>
 * <code>[<em>scheme</em><strong>:</strong>]<em>scheme-specific-part</em>
 * [<strong>#</strong><em>fragment</em>]</code>
 * </p>
 * <p>
 * where <strong>#</strong> and <strong>:</strong> are literal characters,
 * and those parts enclosed in square brackets are optional.
 * </p>
 * <p>
 * There are two main types of URI.  An <em>opaque</em> URI is one
 * which just consists of the above three parts, and is not further
 * defined.  An example of such a URI would be <em>mailto:</em> URI.
 * In contrast, <em>hierarchical</em> URIs give further definition
 * to the scheme-specific part, so as represent some part of a hierarchical
 * structure.
 * </p>
 * <p>
 * <code>[<strong>//</strong><em>authority</em>][<em>path</em>]
 * [<strong>?</strong><em>query</em>]</code>
 * </p>
 * <p>
 * with <strong>/</strong> and <strong>?</strong> being literal characters.
 * When server-based, the authority section is further subdivided into:
 * </p>
 * <p>
 * <code>[<em>user-info</em><strong>@</strong>]<em>host</em>
 * [<strong>:</strong><em>port</em>]</code>
 * </p>
 * <p>
 * with <strong>@</strong> and <strong>:</strong> as literal characters.
 * Authority sections that are not server-based are said to be registry-based.
 * </p>
 * <p>
 * Hierarchical URIs can be either relative or absolute.  Absolute URIs
 * always start with a `<strong>/</strong>', while relative URIs don't
 * specify a scheme.  Opaque URIs are always absolute.
 * </p>
 * <p>
 * Each part of the URI may have one of three states: undefined, empty
 * or containing some content.  The former two of these are represented
 * by <code>null</code> and the empty string in Java, respectively.
 * The scheme-specific part may never be undefined.  It also follows from
 * this that the path sub-part may also not be undefined, so as to ensure
 * the former.
 * </p>
 * 
 * @author Ito Kazumitsu (ito.kazumitsu@hitachi-cable.co.jp)
 * @author Dalibor Topic (robilad@kaffe.org)
 * @author Michael Koch (konqueror@gmx.de)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.4
 */
public final class URI 
  implements Comparable, Serializable
{
  static final long serialVersionUID = -6052424284110960213L;

  /**
   * Regular expression for parsing URIs.
   *
   * Taken from RFC 2396, Appendix B.
   * This expression doesn't parse IPv6 addresses.
   */
  private static final String URI_REGEXP =
    "^(([^:/?#]+):)?((//([^/?#]*))?([^?#]*)(\\?([^#]*))?)?(#(.*))?";

  private static final String AUTHORITY_REGEXP =
    "(([^?#]*)@)?([^?#:]*)(:([^?#]*))?";

  /**
   * Valid characters (taken from rfc2396)
   */
  private static final String RFC2396_DIGIT = "0123456789";
  private static final String RFC2396_LOWALPHA = "abcdefghijklmnopqrstuvwxyz";
  private static final String RFC2396_UPALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  private static final String RFC2396_ALPHA =
    RFC2396_LOWALPHA + RFC2396_UPALPHA;
  private static final String RFC2396_ALPHANUM = RFC2396_DIGIT + RFC2396_ALPHA;
  private static final String RFC2396_MARK = "-_.!~*'()";
  private static final String RFC2396_UNRESERVED =
    RFC2396_ALPHANUM + RFC2396_MARK;
  private static final String RFC2396_REG_NAME =
    RFC2396_UNRESERVED + "$,;:@&=+";
  private static final String RFC2396_PCHAR = RFC2396_UNRESERVED + ":@&=+$,";
  private static final String RFC2396_SEGMENT = RFC2396_PCHAR + ";";
  private static final String RFC2396_PATH_SEGMENTS = RFC2396_SEGMENT + "/";

  /**
   * Index of scheme component in parsed URI.
   */
  private static final int SCHEME_GROUP = 2;

  /**
   * Index of scheme-specific-part in parsed URI.
   */
  private static final int SCHEME_SPEC_PART_GROUP = 3;

  /**
   * Index of authority component in parsed URI.
   */
  private static final int AUTHORITY_GROUP = 5;

  /**
   * Index of path component in parsed URI.
   */
  private static final int PATH_GROUP = 6;

  /**
   * Index of query component in parsed URI.
   */
  private static final int QUERY_GROUP = 8;

  /**
   * Index of fragment component in parsed URI.
   */
  private static final int FRAGMENT_GROUP = 10;
  
  private static final int AUTHORITY_USERINFO_GROUP = 2;
  private static final int AUTHORITY_HOST_GROUP = 3;
  private static final int AUTHORITY_PORT_GROUP = 5;
  
  private transient String scheme;
  private transient String rawSchemeSpecificPart;
  private transient String schemeSpecificPart;
  private transient String rawAuthority;
  private transient String authority;
  private transient String rawUserInfo;
  private transient String userInfo;
  private transient String rawHost;
  private transient String host;
  private transient int port = -1;
  private transient String rawPath;
  private transient String path;
  private transient String rawQuery;
  private transient String query;
  private transient String rawFragment;
  private transient String fragment;
  private String string;

  private void readObject(ObjectInputStream is)
    throws ClassNotFoundException, IOException
  {
    this.string = (String) is.readObject();
    try
      {
	parseURI(this.string);
      }
    catch (URISyntaxException x)
      {
	// Should not happen.
	throw new RuntimeException(x);
      }
  }

  private void writeObject(ObjectOutputStream os) throws IOException
  {
    if (string == null)
      string = toString(); 
    os.writeObject(string);
  }

  private static String getURIGroup(Matcher match, int group)
  {
    String matched = match.group(group);
    return matched.length() == 0 ? null : matched;
  }

  /**
   * Sets fields of this URI by parsing the given string.
   *
   * @param str The string to parse
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  private void parseURI(String str) throws URISyntaxException
  {
    Pattern pattern = Pattern.compile(URI_REGEXP);
    Matcher matcher = pattern.matcher(str);
    
    if (matcher.matches())
      {
	scheme = getURIGroup(matcher, SCHEME_GROUP);
	rawSchemeSpecificPart = matcher.group(SCHEME_SPEC_PART_GROUP);
	schemeSpecificPart = unquote(rawSchemeSpecificPart);
	if (!isOpaque())
	  {
	    rawAuthority = getURIGroup(matcher, AUTHORITY_GROUP);
	    rawPath = matcher.group(PATH_GROUP);
	    rawQuery = getURIGroup(matcher, QUERY_GROUP);
	  }
	rawFragment = getURIGroup(matcher, FRAGMENT_GROUP);
      }
    else
      throw new URISyntaxException(str, "doesn't match URI regular expression");

    if (rawAuthority != null)
      {
	pattern = Pattern.compile(AUTHORITY_REGEXP);
	matcher = pattern.matcher(rawAuthority);

	if (matcher.matches())
	  {
	    rawUserInfo = getURIGroup(matcher, AUTHORITY_USERINFO_GROUP);
	    rawHost = getURIGroup(matcher, AUTHORITY_HOST_GROUP);

	    String portStr = getURIGroup(matcher, AUTHORITY_PORT_GROUP);

	    if (portStr != null)
	      try
		{
		  port = Integer.parseInt(portStr);
		}
	      catch (NumberFormatException e)
		{
		  URISyntaxException use =
		    new URISyntaxException
		      (str, "doesn't match URI regular expression");
		  use.initCause(e);
		  throw use;
		}
	  }
	else
	  throw new URISyntaxException(str, "doesn't match URI regular expression");
      }

    // We must eagerly unquote the parts, because this is the only time
    // we may throw an exception.
    authority = unquote(rawAuthority);
    userInfo = unquote(rawUserInfo);
    host = unquote(rawHost);
    path = unquote(rawPath);
    query = unquote(rawQuery);
    fragment = unquote(rawFragment);
  }

  /**
   * Unquote "%" + hex quotes characters
   *
   * @param str The string to unquote or null.
   *
   * @return The unquoted string or null if str was null.
   *
   * @exception URISyntaxException If the given string contains invalid
   * escape sequences.
   */
  private static String unquote(String str) throws URISyntaxException
  {
    if (str == null)
      return null;
    byte[] buf = new byte[str.length()];
    int pos = 0;
    for (int i = 0; i < str.length(); i++)
      {
	char c = str.charAt(i);
	if (c > 127)
	  throw new URISyntaxException(str, "Invalid character");
	if (c == '%')
	  {
	    if (i + 2 >= str.length())
	      throw new URISyntaxException(str, "Invalid quoted character");
	    int hi = Character.digit(str.charAt(++i), 16);
	    int lo = Character.digit(str.charAt(++i), 16);
	    if (lo < 0 || hi < 0)
	      throw new URISyntaxException(str, "Invalid quoted character");
	    buf[pos++] = (byte) (hi * 16 + lo);
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
   * Quote characters illegal in URIs in given string.
   *
   * Replace illegal characters by encoding their UTF-8
   * representation as "%" + hex code for each resulting
   * UTF-8 character.
   *
   * @param str The string to quote
   *
   * @return The quoted string.
   */
  private static String quote(String str)
  {
    // FIXME: unimplemented.
    return str;
  }

  /**
   * Quote characters illegal in URI authorities in given string.
   *
   * Replace illegal characters by encoding their UTF-8
   * representation as "%" + hex code for each resulting
   * UTF-8 character.
   *
   * @param str The string to quote
   *
   * @return The quoted string.
   */
  private static String quoteAuthority(String str)
  {
    // Technically, we should be using RFC2396_AUTHORITY, but
    // it contains no additional characters.
    return quote(str, RFC2396_REG_NAME);
  }

  /**
   * Quote characters in str that are not part of legalCharacters.
   *
   * Replace illegal characters by encoding their UTF-8
   * representation as "%" + hex code for each resulting
   * UTF-8 character.
   *
   * @param str The string to quote
   * @param legalCharacters The set of legal characters
   *
   * @return The quoted string.
   */
  private static String quote(String str, String legalCharacters)
  {
    StringBuffer sb = new StringBuffer(str.length());
    for (int i = 0; i < str.length(); i++)
      {
	char c = str.charAt(i);
	if (legalCharacters.indexOf(c) == -1)
	  {
	    String hex = "0123456789ABCDEF";
	    if (c <= 127)
	      sb.append('%').append(hex.charAt(c / 16)).append(hex.charAt(c % 16));
	    else
	      {
		try
		  {
		    // this is far from optimal, but it works
		    byte[] utf8 = str.substring(i, i + 1).getBytes("utf-8");
		    for (int j = 0; j < utf8.length; j++)
		      sb.append('%').append(hex.charAt((utf8[j] & 0xff) / 16))
		        .append(hex.charAt((utf8[j] & 0xff) % 16));
		  }
		catch (java.io.UnsupportedEncodingException x)
		  {
		    throw (Error) new InternalError().initCause(x);
		  }
	      }
	  }
	else
	  sb.append(c);
      }
    return sb.toString();
  }

  /**
   * Quote characters illegal in URI hosts in given string.
   *
   * Replace illegal characters by encoding their UTF-8
   * representation as "%" + hex code for each resulting
   * UTF-8 character.
   *
   * @param str The string to quote
   *
   * @return The quoted string.
   */
  private static String quoteHost(String str)
  {
    // FIXME: unimplemented.
    return str;
  }

  /**
   * Quote characters illegal in URI paths in given string.
   *
   * Replace illegal characters by encoding their UTF-8
   * representation as "%" + hex code for each resulting
   * UTF-8 character.
   *
   * @param str The string to quote
   *
   * @return The quoted string.
   */
  private static String quotePath(String str)
  {
    // Technically, we should be using RFC2396_PATH, but
    // it contains no additional characters.
    return quote(str, RFC2396_PATH_SEGMENTS);
  }

  /**
   * Quote characters illegal in URI user infos in given string.
   *
   * Replace illegal characters by encoding their UTF-8
   * representation as "%" + hex code for each resulting
   * UTF-8 character.
   *
   * @param str The string to quote
   *
   * @return The quoted string.
   */
  private static String quoteUserInfo(String str)
  {
    // FIXME: unimplemented.
    return str;
  }

  /**
   * Creates an URI from the given string
   *
   * @param str The string to create the URI from
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   * @exception NullPointerException If str is null
   */
  public URI(String str) throws URISyntaxException
  {
    this.string = str;
    parseURI(str);
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param userInfo The username and authorization info
   * @param host The hostname
   * @param port The port number
   * @param path The path
   * @param query The query
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI(String scheme, String userInfo, String host, int port,
             String path, String query, String fragment)
    throws URISyntaxException
  {
    this((scheme == null ? "" : scheme + ":")
         + (userInfo == null && host == null && port == -1 ? "" : "//")
         + (userInfo == null ? "" : quoteUserInfo(userInfo) + "@")
         + (host == null ? "" : quoteHost(host))
         + (port == -1 ? "" : ":" + String.valueOf(port))
         + (path == null ? "" : quotePath(path))
         + (query == null ? "" : "?" + quote(query))
         + (fragment == null ? "" : "#" + quote(fragment)));

    parseServerAuthority();
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param authority The authority
   * @param path The apth
   * @param query The query
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI(String scheme, String authority, String path, String query,
             String fragment) throws URISyntaxException
  {
    this((scheme == null ? "" : scheme + ":")
         + (authority == null ? "" : "//" + quoteAuthority(authority))
         + (path == null ? "" : quotePath(path))
         + (query == null ? "" : "?" + quote(query))
         + (fragment == null ? "" : "#" + quote(fragment)));
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param host The hostname
   * @param path The path
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI(String scheme, String host, String path, String fragment)
    throws URISyntaxException
  {
    this(scheme, null, host, -1, path, null, fragment);
  }

  /**
   * Create an URI from the given components
   *
   * @param scheme The scheme name
   * @param ssp The scheme specific part
   * @param fragment The fragment
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI(String scheme, String ssp, String fragment)
    throws URISyntaxException
  {
    this((scheme == null ? "" : scheme + ":")
         + (ssp == null ? "" : quote(ssp))
         + (fragment == null ? "" : "#" + quote(fragment)));
  }

  /**
   * Create an URI from the given string
   *
   * @param str The string to create the URI from
   *
   * @exception IllegalArgumentException If the given string violates RFC 2396
   * @exception NullPointerException If str is null
   */
  public static URI create(String str)
  {
    try
      {
	return new URI(str);
      }
    catch (URISyntaxException e)
      {
	throw (IllegalArgumentException) new IllegalArgumentException()
	      .initCause(e);
      }
  }

  /**
   * Attempts to parse this URI's authority component, if defined,
   * into user-information, host, and port components
   *
   * @exception URISyntaxException If the given string violates RFC 2396
   */
  public URI parseServerAuthority() throws URISyntaxException
  {
    return null;
  }

  /**
   * Returns a normalizes versions of the URI
   */
  public URI normalize()
  {
    return null;
  }

  /**
   * Resolves the given URI against this URI
   *
   * @param uri The URI to resolve against this URI
   *
   * @return The resulting URI, or null when it couldn't be resolved
   * for some reason.
   *
   * @exception NullPointerException If uri is null
   */
  public URI resolve(URI uri)
  {
    if (uri.isAbsolute())
      return uri;
    if (uri.isOpaque())
      return uri;

    String scheme = uri.getScheme();
    String schemeSpecificPart = uri.getSchemeSpecificPart();
    String authority = uri.getAuthority();
    String path = uri.getPath();
    String query = uri.getQuery();
    String fragment = uri.getFragment();

    try
      {
	if (fragment != null && path != null && path.equals("")
	    && scheme == null && authority == null && query == null)
	  return new URI(this.scheme, this.schemeSpecificPart, fragment);

	if (authority == null)
	  {
	    authority = this.authority;
	    if (path == null)
	      path = "";
	    if (! (path.startsWith("/")))
	      {
		StringBuffer basepath = new StringBuffer(this.path);
		int i = this.path.lastIndexOf('/');

		if (i >= 0)
		  basepath.delete(i + 1, basepath.length());

		basepath.append(path);
		path = basepath.toString();
		//  FIXME We must normalize the path here.
		//  Normalization process omitted.
	      }
	  }
	return new URI(this.scheme, authority, path, query, fragment);
      }
    catch (URISyntaxException e)
      {
	return null;
      }
  }

  /**
   * Resolves the given URI string against this URI
   *
   * @param str The URI as string to resolve against this URI
   *
   * @return The resulting URI
   *
   * @exception IllegalArgumentException If the given URI string
   * violates RFC 2396
   * @exception NullPointerException If uri is null
   */
  public URI resolve(String str) throws IllegalArgumentException
  {
    return resolve(create(str));
  }

  /**
   * Relativizes the given URI against this URI
   *
   * @param uri The URI to relativize this URI
   *
   * @return The resulting URI
   *
   * @exception NullPointerException If uri is null
   */
  public URI relativize(URI uri)
  {
    return null;
  }

  /**
   * Creates an URL from an URI
   *
   * @exception MalformedURLException If a protocol handler for the URL could
   * not be found, or if some other error occurred while constructing the URL
   * @exception IllegalArgumentException If the URI is not absolute
   */
  public URL toURL() throws IllegalArgumentException, MalformedURLException
  {
    if (isAbsolute())
      return new URL(this.toString());

    throw new IllegalArgumentException("not absolute");
  }

  /**
   * Returns the scheme of the URI
   */
  public String getScheme()
  {
    return scheme;
  }

  /**
   * Tells whether this URI is absolute or not
   */
  public boolean isAbsolute()
  {
    return scheme != null;
  }

  /**
   * Tell whether this URI is opaque or not
   */
  public boolean isOpaque()
  {
    return ((scheme != null) && ! (schemeSpecificPart.startsWith("/")));
  }

  /**
   * Returns the raw scheme specific part of this URI.
   * The scheme-specific part is never undefined, though it may be empty
   */
  public String getRawSchemeSpecificPart()
  {
    return rawSchemeSpecificPart;
  }

  /**
   * Returns the decoded scheme specific part of this URI.
   */
  public String getSchemeSpecificPart()
  {
    return schemeSpecificPart;
  }

  /**
   * Returns the rae authority part of this URI
   */
  public String getRawAuthority()
  {
    return rawAuthority;
  }

  /**
   * Returns the decoded authority part of this URI
   */
  public String getAuthority()
  {
    return authority;
  }

  /**
   * Returns the raw user info part of this URI
   */
  public String getRawUserInfo()
  {
    return rawUserInfo;
  }

  /**
   * Returns the decoded user info part of this URI
   */
  public String getUserInfo()
  {
    return userInfo;
  }

  /**
   * Returns the hostname of the URI
   */
  public String getHost()
  {
    return host;
  }

  /**
   * Returns the port number of the URI
   */
  public int getPort()
  {
    return port;
  }

  /**
   * Returns the raw path part of this URI
   */
  public String getRawPath()
  {
    return rawPath;
  }

  /**
   * Returns the path of the URI
   */
  public String getPath()
  {
    return path;
  }

  /**
   * Returns the raw query part of this URI
   */
  public String getRawQuery()
  {
    return rawQuery;
  }

  /**
   * Returns the query of the URI
   */
  public String getQuery()
  {
    return query;
  }

  /**
   * Return the raw fragment part of this URI
   */
  public String getRawFragment()
  {
    return rawFragment;
  }

  /**
   * Returns the fragment of the URI
   */
  public String getFragment()
  {
    return fragment;
  }

  /**
   * Compares the URI with a given object
   *
   * @param obj The obj to compare the URI with
   */
  public boolean equals(Object obj)
  {
    return false;
  }

  /**
   * Computes the hascode of the URI
   */
  public int hashCode()
  {
    return 0;
  }

  /**
   * Compare the URI with another object that must be an URI too
   *
   * @param obj This object to compare this URI with
   *
   * @exception ClassCastException If given object ist not an URI
   */
  public int compareTo(Object obj) throws ClassCastException
  {
    return 0;
  }

  /**
   * Returns the URI as a String.  If the URI was created using a constructor,
   * then this will be the same as the original input string.
   *
   * @return a string representation of the URI.
   */
  public String toString()
  {
    return (getScheme() == null ? "" : getScheme() + ":")
           + getRawSchemeSpecificPart()
           + (getRawFragment() == null ? "" : "#" + getRawFragment());
  }

  /**
   * Returns the URI as US-ASCII string
   */
  public String toASCIIString()
  {
    return "";
  }
}
