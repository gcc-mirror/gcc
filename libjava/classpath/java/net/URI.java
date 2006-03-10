/* URI.java -- An URI class
   Copyright (C) 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>
 * A URI instance represents that defined by 
 * <a href="http://www.ietf.org/rfc/rfc3986.txt">RFC3986</a>,
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
 * <h2>Character Escaping and Quoting</h2>
 * <p>
 * The characters that can be used within a valid URI are restricted.
 * There are two main classes of characters which can't be used as is
 * within the URI:
 * </p>
 * <ol>
 * <li><strong>Characters outside the US-ASCII character set</strong>.
 * These have to be <strong>escaped</strong> in order to create
 * an RFC-compliant URI; this means replacing the character with the
 * appropriate hexadecimal value, preceded by a `%'.</li>
 * <li><strong>Illegal characters</strong> (e.g. space characters,
 * control characters) are quoted, which results in them being encoded
 * in the same way as non-US-ASCII characters.</li>
 * </ol>
 * <p>
 * The set of valid characters differs depending on the section of the URI:
 * </p>
 * <ul>
 * <li><strong>Scheme</strong>: Must be an alphanumeric, `-', `.' or '+'.</li>
 * <li><strong>Authority</strong>:Composed of the username, host, port, `@'
 * and `:'.</li>
 * <li><strong>Username</strong>: Allows unreserved or percent-encoded
 * characters, sub-delimiters and `:'.</li>
 * <li><strong>Host</strong>: Allows unreserved or percent-encoded
 * characters, sub-delimiters and square brackets (`[' and `]') for IPv6
 * addresses.</li>
 * <li><strong>Port</strong>: Digits only.</li>
 * <li><strong>Path</strong>: Allows the path characters and `/'.
 * <li><strong>Query</strong>: Allows the path characters, `?' and '/'.
 * <li><strong>Fragment</strong>: Allows the path characters, `?' and '/'.
 * </ul>
 * <p>
 * These definitions reference the following sets of characters:
 * </p>
 * <ul>
 * <li><strong>Unreserved characters</strong>: The alphanumerics plus
 * `-', `.', `_', and `~'.</li>
 * <li><strong>Sub-delimiters</strong>: `!', `$', `&', `(', `)', `*',
 * `+', `,', `;', `=' and the single-quote itself.</li>
 * <li><strong>Path characters</strong>: Unreserved and percent-encoded
 * characters and the sub-delimiters along with `@' and `:'.</li>
 * </ul>
 * <p>
 * The constructors and accessor methods allow the use and retrieval of
 * URI components which contain non-US-ASCII characters directly.
 * They are only escaped when the <code>toASCIIString()</code> method
 * is used.  In contrast, illegal characters are always quoted, with the
 * exception of the return values of the non-raw accessors.
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
  /**
   * For serialization compatability.
   */
  static final long serialVersionUID = -6052424284110960213L;

  /**
   * Regular expression for parsing URIs.
   *
   * Taken from RFC 2396, Appendix B.
   * This expression doesn't parse IPv6 addresses.
   */
  private static final String URI_REGEXP =
    "^(([^:/?#]+):)?((//([^/?#]*))?([^?#]*)(\\?([^#]*))?)?(#(.*))?";

  /**
   * Regular expression for parsing the authority segment.
   */
  private static final String AUTHORITY_REGEXP =
    "(([^?#]*)@)?([^?#:]*)(:([0-9]*))?";

  /**
   * Valid characters (taken from rfc2396/3986)
   */
  private static final String RFC2396_DIGIT = "0123456789";
  private static final String RFC2396_LOWALPHA = "abcdefghijklmnopqrstuvwxyz";
  private static final String RFC2396_UPALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  private static final String RFC2396_ALPHA =
    RFC2396_LOWALPHA + RFC2396_UPALPHA;
  private static final String RFC2396_ALPHANUM = RFC2396_DIGIT + RFC2396_ALPHA;
  private static final String RFC3986_UNRESERVED = RFC2396_ALPHANUM + "-._~";
  private static final String RFC3986_SUBDELIMS = "!$&'()*+,;=";
  private static final String RFC3986_REG_NAME =
    RFC3986_UNRESERVED + RFC3986_SUBDELIMS + "%";
  private static final String RFC3986_PCHAR = RFC3986_UNRESERVED + 
    RFC3986_SUBDELIMS + ":@%";
  private static final String RFC3986_SEGMENT = RFC3986_PCHAR;
  private static final String RFC3986_PATH_SEGMENTS = RFC3986_SEGMENT + "/";
  private static final String RFC3986_SSP = RFC3986_PCHAR + "?/";
  private static final String RFC3986_HOST = RFC3986_REG_NAME + "[]";
  private static final String RFC3986_USERINFO = RFC3986_REG_NAME + ":";

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
  
  /**
   * Index of userinfo component in parsed authority section.
   */
  private static final int AUTHORITY_USERINFO_GROUP = 2;

  /**
   * Index of host component in parsed authority section.
   */
  private static final int AUTHORITY_HOST_GROUP = 3;

  /**
   * Index of port component in parsed authority section.
   */
  private static final int AUTHORITY_PORT_GROUP = 5;

  /**
   * The compiled version of the URI regular expression.
   */
  private static final Pattern URI_PATTERN;

  /**
   * The compiled version of the authority regular expression.
   */
  private static final Pattern AUTHORITY_PATTERN;

  /**
   * The set of valid hexadecimal characters.
   */
  private static final String HEX = "0123456789ABCDEF";

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

  /**
   * Static initializer to pre-compile the regular expressions.
   */
  static
  {
    URI_PATTERN = Pattern.compile(URI_REGEXP);
    AUTHORITY_PATTERN = Pattern.compile(AUTHORITY_REGEXP);
  }

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

  /**
   * <p>
   * Returns the string content of the specified group of the supplied
   * matcher.  The returned value is modified according to the following:
   * </p>
   * <ul>
   * <li>If the resulting string has a length greater than 0, then
   * that string is returned.</li>
   * <li>If a string of zero length, is matched, then the content
   * of the preceding group is considered.  If this is also an empty
   * string, then <code>null</code> is returned to indicate an undefined
   * value.  Otherwise, the value is truly the empty string and this is
   * the returned value.</li>
   * </ul>
   * <p>
   * This method is used for matching against all parts of the URI
   * that may be either undefined or empty (i.e. all those but the
   * scheme-specific part and the path).  In each case, the preceding
   * group is the content of the original group, along with some
   * additional distinguishing feature.  For example, the preceding
   * group for the query includes the preceding question mark,
   * while that of the fragment includes the hash symbol.  The presence
   * of these features enables disambiguation between the two cases
   * of a completely unspecified value and a simple non-existant value.
   * The scheme differs in that it will never return an empty string;
   * the delimiter follows the scheme rather than preceding it, so
   * it becomes part of the following section.  The same is true
   * of the user information.
   * </p>
   *
   * @param match the matcher, which contains the results of the URI
   *              matched against the URI regular expression.
   * @return either the matched content, <code>null</code> for undefined
   *         values, or an empty string for a URI part with empty content.
   */
  private static String getURIGroup(Matcher match, int group)
  {
    String matched = match.group(group);
    if (matched == null || matched.length() == 0)
      {
	String prevMatched = match.group(group -1);
	if (prevMatched == null || prevMatched.length() == 0)
	  return null;
	else
	  return "";
      }
    return matched;
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
    Matcher matcher = URI_PATTERN.matcher(str);
    
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
      throw new URISyntaxException(str,
				   "doesn't match URI regular expression");
    parseServerAuthority();

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
    return quote(str, RFC3986_SSP);
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
    return quote(str, RFC3986_REG_NAME);
  }

  /**
   * Quotes the characters in the supplied string that are not part of
   * the specified set of legal characters.
   *
   * @param str the string to quote
   * @param legalCharacters the set of legal characters
   *
   * @return the quoted string.
   */
  private static String quote(String str, String legalCharacters)
  {
    StringBuffer sb = new StringBuffer(str.length());
    for (int i = 0; i < str.length(); i++)
      {
	char c = str.charAt(i);
	if (legalCharacters.indexOf(c) == -1)
	  {
	    if (c <= 127)
	      {
		sb.append('%');
		sb.append(HEX.charAt(c / 16));
		sb.append(HEX.charAt(c % 16));
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
    return quote(str, RFC3986_HOST);
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
    return quote(str, RFC3986_PATH_SEGMENTS);
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
    return quote(str, RFC3986_USERINFO);
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
   * into user-information, host, and port components.  The purpose
   * of this method was to disambiguate between some authority sections,
   * which form invalid server-based authories, but valid registry
   * based authorities.  In the updated RFC 3986, the authority section
   * is defined differently, with registry-based authorities part of
   * the host section.  Thus, this method is now simply an explicit
   * way of parsing any authority section.
   *
   * @return the URI, with the authority section parsed into user
   *         information, host and port components.
   * @throws URISyntaxException if the given string violates RFC 2396
   */
  public URI parseServerAuthority() throws URISyntaxException
  {
    if (rawAuthority != null)
      {
	Matcher matcher = AUTHORITY_PATTERN.matcher(rawAuthority);

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
		      (string, "doesn't match URI regular expression");
		  use.initCause(e);
		  throw use;
		}
	  }
	else
	  throw new URISyntaxException(string,
				       "doesn't match URI regular expression");
      }
    return this;
  }

  /**
   * <p>
   * Returns a normalized version of the URI.  If the URI is opaque,
   * or its path is already in normal form, then this URI is simply
   * returned.  Otherwise, the following transformation of the path
   * element takes place:
   * </p>
   * <ol>
   * <li>All `.' segments are removed.</li>
   * <li>Each `..' segment which can be paired with a prior non-`..' segment
   * is removed along with the preceding segment.</li>
   * <li>A `.' segment is added to the front if the first segment contains
   * a colon (`:').  This is a deviation from the RFC, which prevents
   * confusion between the path and the scheme.</li>
   * </ol>
   * <p>
   * The resulting URI will be free of `.' and `..' segments, barring those
   * that were prepended or which couldn't be paired, respectively.
   * </p>
   *
   * @return the normalized URI.
   */
  public URI normalize()
  {
    if (isOpaque() || path.indexOf("/./") == -1 && path.indexOf("/../") == -1)
      return this;
    try
      {
	return new URI(scheme, authority, normalizePath(path), query,
		       fragment);
      }
    catch (URISyntaxException e)
      {
	throw (Error) new InternalError("Normalized URI variant could not "+
					"be constructed").initCause(e);
      }
  }

  /**
   * <p>
   * Normalize the given path.  The following transformation takes place:
   * </p>
   * <ol>
   * <li>All `.' segments are removed.</li>
   * <li>Each `..' segment which can be paired with a prior non-`..' segment
   * is removed along with the preceding segment.</li>
   * <li>A `.' segment is added to the front if the first segment contains
   * a colon (`:').  This is a deviation from the RFC, which prevents
   * confusion between the path and the scheme.</li>
   * </ol>
   * <p>
   * The resulting URI will be free of `.' and `..' segments, barring those
   * that were prepended or which couldn't be paired, respectively.
   * </p>
   * 
   * @param relativePath the relative path to be normalized.
   * @return the normalized path.
   */
  private String normalizePath(String relativePath)
  {
    /* 
       This follows the algorithm in section 5.2.4. of RFC3986,
       but doesn't modify the input buffer.
    */
    StringBuffer input = new StringBuffer(relativePath);
    StringBuffer output = new StringBuffer();
    int start = 0;
    while (start < input.length())
      {
	/* A */
	if (input.indexOf("../",start) == start)
	  {
	    start += 3;
	    continue;
	  }
	if (input.indexOf("./",start) == start)
	  {
	    start += 2;
	    continue;
	  }
	/* B */
	if (input.indexOf("/./",start) == start)
	  {
	    start += 2;
	    continue;
	  }
	if (input.indexOf("/.",start) == start
	    && input.charAt(start + 2) != '.')
	  {
	    start += 1;
	    input.setCharAt(start,'/');
	    continue;
	  }
	/* C */
	if (input.indexOf("/../",start) == start)
	  {
	    start += 3;
	    removeLastSegment(output);
	    continue;
	  }
	if (input.indexOf("/..",start) == start)
	  {
	    start += 2;
	    input.setCharAt(start,'/');
	    removeLastSegment(output);
	    continue;
	  }
	/* D */
	if (start == input.length() - 1 && input.indexOf(".",start) == start)
	  {
	    input.delete(0,1);
	    continue;
	  }
	if (start == input.length() - 2 && input.indexOf("..",start) == start)
	  {
	    input.delete(0,2);
	    continue;
	  }
	/* E */
	int indexOfSlash = input.indexOf("/",start);
	while (indexOfSlash == start)
	  {
	    output.append("/");
	    ++start;
	    indexOfSlash = input.indexOf("/",start);
	  }
	if (indexOfSlash == -1)
	  indexOfSlash = input.length();
	output.append(input.substring(start, indexOfSlash));
        start = indexOfSlash;
      }
    return output.toString();
  }

  /**
   * Removes the last segment of the path from the specified buffer.
   *
   * @param buffer the buffer containing the path.
   */
  private void removeLastSegment(StringBuffer buffer)
  {
    int lastSlash = buffer.lastIndexOf("/");
    if (lastSlash == -1)
      buffer.setLength(0);
    else
      buffer.setLength(lastSlash);
  }

  /**
   * Resolves the given URI against this URI
   *
   * @param uri The URI to resolve against this URI
   *
   * @return The resulting URI, or null when it couldn't be resolved
   * for some reason.
   *
   * @throws NullPointerException if uri is null
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
		path = normalizePath(basepath.toString());
	      }
	  }
	return new URI(this.scheme, authority, path, query, fragment);
      }
    catch (URISyntaxException e)
      {
	throw (Error) new InternalError("Resolved URI variant could not "+
					"be constructed").initCause(e);
      }
  }

  /**
   * Resolves the given URI string against this URI
   *
   * @param str The URI as string to resolve against this URI
   *
   * @return The resulting URI
   *
   * @throws IllegalArgumentException If the given URI string
   * violates RFC 2396
   * @throws NullPointerException If uri is null
   */
  public URI resolve(String str) throws IllegalArgumentException
  {
    return resolve(create(str));
  }

  /**
   * <p>
   * Relativizes the given URI against this URI.  The following
   * algorithm is used:
   * </p>
   * <ul>
   * <li>If either URI is opaque, the given URI is returned.</li>
   * <li>If the schemes of the URIs differ, the given URI is returned.</li>
   * <li>If the authority components of the URIs differ, then the given
   * URI is returned.</li>
   * <li>If the path of this URI is not a prefix of the supplied URI,
   * then the given URI is returned.</li>
   * <li>If all the above conditions hold, a new URI is created using the
   * query and fragment components of the given URI, along with a path
   * computed by removing the path of this URI from the start of the path
   * of the supplied URI.</li>
   * </ul>
   *
   * @param uri the URI to relativize agsint this URI
   * @return the resulting URI
   * @throws NullPointerException if the uri is null
   */
  public URI relativize(URI uri)
  {
    if (isOpaque() || uri.isOpaque())
      return uri;
    if (scheme == null && uri.getScheme() != null)
      return uri;
    if (scheme != null && !(scheme.equals(uri.getScheme())))
      return uri;
    if (rawAuthority == null && uri.getRawAuthority() != null)
      return uri;
    if (rawAuthority != null && !(rawAuthority.equals(uri.getRawAuthority())))
      return uri;
    if (!(uri.getRawPath().startsWith(rawPath)))
      return uri;
    try
      {
	return new URI(null, null, 
		       uri.getRawPath().substring(rawPath.length()),
		       uri.getRawQuery(), uri.getRawFragment());
      }
    catch (URISyntaxException e)
      {
	throw (Error) new InternalError("Relativized URI variant could not "+
					"be constructed").initCause(e);       
      }
  }

  /**
   * Creates an URL from an URI
   *
   * @throws MalformedURLException If a protocol handler for the URL could
   * not be found, or if some other error occurred while constructing the URL
   * @throws IllegalArgumentException If the URI is not absolute
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
   * Returns the raw authority part of this URI
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
   * <p> 
   * Compares the URI with the given object for equality.  If the
   * object is not a <code>URI</code>, then the method returns false.
   * Otherwise, the following criteria are observed:
   * </p>
   * <ul>
   * <li>The scheme of the URIs must either be null (undefined) in both cases,
   * or equal, ignorant of case.</li>
   * <li>The raw fragment of the URIs must either be null (undefined) in both
   * cases, or equal, ignorant of case.</li>
   * <li>Both URIs must be of the same type (opaque or hierarchial)</li>
   * <li><strong>For opaque URIs:</strong></li>
   * <ul>
   * <li>The raw scheme-specific parts must be equal.</li>
   * </ul>
   * <li>For hierarchical URIs:</li>
   * <ul>
   * <li>The raw paths must be equal, ignorant of case.</li>
   * <li>The raw queries are either both undefined or both equal, ignorant
   * of case.</li>
   * <li>The raw authority sections are either both undefined or:</li>
   * <li><strong>For registry-based authorities:</strong></li>
   * <ul><li>they are equal.</li></ul>
   * <li><strong>For server-based authorities:</strong></li>
   * <ul>
   * <li>the hosts are equal, ignoring case</li>
   * <li>the ports are equal</li>
   * <li>the user information components are equal</li>
   * </ul>
   * </ul>
   * </ul>
   *
   * @param obj the obj to compare the URI with.
   * @return <code>true</code> if the objects are equal, according to
   *         the specification above.
   */
  public boolean equals(Object obj)
  {
    if (!(obj instanceof URI))
      return false;
    URI uriObj = (URI) obj;
    if (scheme == null)
      {
	if (uriObj.getScheme() != null)
	  return false;
      }
    else
      if (!(scheme.equalsIgnoreCase(uriObj.getScheme())))
	return false;
    if (rawFragment == null)
      {
	if (uriObj.getRawFragment() != null)
	  return false;
      }
    else
      if (!(rawFragment.equalsIgnoreCase(uriObj.getRawFragment())))
	return false;
    boolean opaqueThis = isOpaque();
    boolean opaqueObj = uriObj.isOpaque();
    if (opaqueThis && opaqueObj)
      return rawSchemeSpecificPart.equals(uriObj.getRawSchemeSpecificPart());
    else if (!opaqueThis && !opaqueObj)
      {
	boolean common = rawPath.equalsIgnoreCase(uriObj.getRawPath())
	  && ((rawQuery == null && uriObj.getRawQuery() == null)
	      || rawQuery.equalsIgnoreCase(uriObj.getRawQuery()));
	if (rawAuthority == null && uriObj.getRawAuthority() == null)
	  return common;
	if (host == null)
	  return common 
	    && rawAuthority.equalsIgnoreCase(uriObj.getRawAuthority());
	return common 
	  && host.equalsIgnoreCase(uriObj.getHost())
	  && port == uriObj.getPort()
	  && (rawUserInfo == null ?
	      uriObj.getRawUserInfo() == null :
	      rawUserInfo.equalsIgnoreCase(uriObj.getRawUserInfo()));
      }
    else
      return false;
  }

  /**
   * Computes the hashcode of the URI
   */
  public int hashCode()
  {
    return (getScheme() == null ? 0 : 13 * getScheme().hashCode())
      + 17 * getRawSchemeSpecificPart().hashCode()
      + (getRawFragment() == null ? 0 : 21 + getRawFragment().hashCode());
  }

  /**
   * Compare the URI with another object that must also be a URI.
   * Undefined components are taken to be less than any other component.
   * The following criteria are observed:
   * </p>
   * <ul>
   * <li>Two URIs with different schemes are compared according to their
   * scheme, regardless of case.</li>
   * <li>A hierarchical URI is less than an opaque URI with the same
   * scheme.</li>
   * <li><strong>For opaque URIs:</strong></li>
   * <ul>
   * <li>URIs with differing scheme-specific parts are ordered according
   * to the ordering of the scheme-specific part.</li>
   * <li>URIs with the same scheme-specific part are ordered by the
   * raw fragment.</li>
   * </ul>
   * <li>For hierarchical URIs:</li>
   * <ul>
   * <li>URIs are ordered according to their raw authority sections,
   * if they are unequal.</li>
   * <li><strong>For registry-based authorities:</strong></li>
   * <ul><li>they are ordered according to the ordering of the authority
   * component.</li></ul>
   * <li><strong>For server-based authorities:</strong></li>
   * <ul>
   * <li>URIs are ordered according to the raw user information.</li>
   * <li>URIs with the same user information are ordered by the host,
   * ignoring case.</li>
   * <lI>URIs with the same host are ordered by the port.</li>
   * </ul>
   * <li>URIs with the same authority section are ordered by the raw path.</li>
   * <li>URIs with the same path are ordered by their raw query.</li>
   * <li>URIs with the same query are ordered by their raw fragments.</li>
   * </ul>
   * </ul>
   *
   * @param obj This object to compare this URI with
   * @return a negative integer, zero or a positive integer depending
   *         on whether this URI is less than, equal to or greater
   *         than that supplied, respectively.
   * @throws ClassCastException if the given object is not a URI
   */
  public int compareTo(Object obj) 
    throws ClassCastException
  {
    URI uri = (URI) obj;
    if (scheme == null && uri.getScheme() != null)
      return -1;
    if (scheme != null)
      {
	int sCompare = scheme.compareToIgnoreCase(uri.getScheme()); 
	if (sCompare != 0)
	  return sCompare;
      }
    boolean opaqueThis = isOpaque();
    boolean opaqueObj = uri.isOpaque();
    if (opaqueThis && !opaqueObj)
      return 1;
    if (!opaqueThis && opaqueObj)
      return -1;
    if (opaqueThis)
      {
	int ssCompare = 
	  rawSchemeSpecificPart.compareTo(uri.getRawSchemeSpecificPart());
	if (ssCompare == 0)
	  return compareFragments(uri);
	else
	  return ssCompare;
      }
    if (rawAuthority == null && uri.getRawAuthority() != null)
      return -1;
    if (rawAuthority != null)
      {
	int aCompare = rawAuthority.compareTo(uri.getRawAuthority());
	if (aCompare != 0)
	  {
	    if (host == null)
	      return aCompare;
	    if (rawUserInfo == null && uri.getRawUserInfo() != null)
	      return -1;
	    int uCompare = rawUserInfo.compareTo(uri.getRawUserInfo());
	    if (uCompare != 0)
	      return uCompare;
	    if (host == null && uri.getHost() != null)
	      return -1;
	    int hCompare = host.compareTo(uri.getHost());
	    if (hCompare != 0)
	      return hCompare;
	    return new Integer(port).compareTo(new Integer(uri.getPort()));
	  }
      }
    if (rawPath == null && uri.getRawPath() != null)
      return -1;
    if (rawPath != null)
      {
	int pCompare = rawPath.compareTo(uri.getRawPath()); 
	if (pCompare != 0)
	  return pCompare;
      }
    if (rawQuery == null && uri.getRawQuery() != null)
      return -1;
    if (rawQuery != null)
      {
	int qCompare = rawQuery.compareTo(uri.getRawQuery());
	if (qCompare != 0)
	  return qCompare;
      }
    return compareFragments(uri);
  }

  /**
   * Compares the fragment of this URI with that of the supplied URI.
   *
   * @param uri the URI to compare with this one.
   * @return a negative integer, zero or a positive integer depending
   *         on whether this uri's fragment is less than, equal to
   *         or greater than the fragment of the uri supplied, respectively.
   */
  private int compareFragments(URI uri)
  {
    if (rawFragment == null && uri.getRawFragment() != null)
      return -1;
    else if (rawFragment == null)
      return 0;
    else
      return rawFragment.compareTo(uri.getRawFragment());
  }

  /**
   * Returns the URI as a String.  If the URI was created using a constructor,
   * then this will be the same as the original input string.
   *
   * @return a string representation of the URI.
   */
  public String toString()
  {
    return (scheme == null ? "" : scheme + ":")
      + rawSchemeSpecificPart
      + (rawFragment == null ? "" : "#" + rawFragment);
  }

  /**
   * Returns the URI as US-ASCII string.  This is the same as the result
   * from <code>toString()</code> for URIs that don't contain any non-US-ASCII
   * characters.  Otherwise, the non-US-ASCII characters are replaced
   * by their percent-encoded representations.
   *
   * @return a string representation of the URI, containing only US-ASCII
   *         characters.
   */
  public String toASCIIString()
  {
    String strRep = toString();
    boolean inNonAsciiBlock = false;
    StringBuffer buffer = new StringBuffer();
    StringBuffer encBuffer = null;
    for (int i = 0; i < strRep.length(); i++)
      {
	char c = strRep.charAt(i);
	if (c <= 127)
	  {
	    if (inNonAsciiBlock)
	      {
		buffer.append(escapeCharacters(encBuffer.toString()));
		inNonAsciiBlock = false;
	      }
	    buffer.append(c);
	  }
	else
	  {
	    if (!inNonAsciiBlock)
	      {
		encBuffer = new StringBuffer();
		inNonAsciiBlock = true;
	      }
	    encBuffer.append(c);
	  }
      }
    return buffer.toString();
  }

  /**
   * Converts the non-ASCII characters in the supplied string
   * to their equivalent percent-encoded representations.
   * That is, they are replaced by "%" followed by their hexadecimal value.
   *
   * @param str a string including non-ASCII characters.
   * @return the string with the non-ASCII characters converted to their
   *         percent-encoded representations.
   */
  private static String escapeCharacters(String str)
  {
    try
      {
	StringBuffer sb = new StringBuffer(); 
	// this is far from optimal, but it works
	byte[] utf8 = str.getBytes("utf-8");
	for (int j = 0; j < utf8.length; j++)
	  {
	    sb.append('%');
	    sb.append(HEX.charAt((utf8[j] & 0xff) / 16));
	    sb.append(HEX.charAt((utf8[j] & 0xff) % 16));
	  }
	return sb.toString();
      }
    catch (java.io.UnsupportedEncodingException x)
      {
	throw (Error) new InternalError("Escaping error").initCause(x);
      }
  }

}
