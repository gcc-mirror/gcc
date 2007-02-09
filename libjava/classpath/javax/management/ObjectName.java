/* ObjectName.java -- Represent the name of a bean, or a pattern for a name.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import java.io.Serializable;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

/**
 * <p>
 * An {@link ObjectName} instance represents the name of a management
 * bean, or a pattern which may match the name of one or more
 * management beans.  Patterns are distinguished from names by the
 * presence of the '?' and '*' characters (which match a single
 * character and a series of zero or more characters, respectively).
 * </p>
 * <p>
 * Each name begins with a domain element, which is terminated by
 * a ':' character.  The domain may be empty.  If so, it will be
 * replaced by the default domain of the bean server in certain
 * contexts.  The domain is a pattern, if it contains either '?'
 * or '*'.  To avoid collisions, it is usual to use reverse
 * DNS names for the domain, as in Java package and property names.
 * </p>
 * <p>
 * Following the ':' character is a series of properties.  The list
 * is separated by commas, and largely consists of unordered key-value
 * pairs, separated by an equals sign ('=').  At most one element may
 * be an asterisk ('*'), which turns the {@link ObjectName} instance
 * into a <emph>property pattern</emph>.  In this situation, the pattern
 * matches a name if the name contains at least those key-value pairs
 * given and has the same domain.
 * </p>
 * <p>
 * A <emph>key</emph> is a string of characters which doesn't include
 * any of those used as delimiters or in patterns (':', '=', ',', '?'
 * and '*').  Keys must be unique.
 * </p>
 * <p>
 * A value may be <emph>quoted</emph> or <emph>unquoted</emph>.  Unquoted
 * values obey the same rules as given for keys above.  Quoted values are
 * surrounded by quotation marks ("), and use a backslash ('\') character
 * to include quotes ('\"'), backslashes ('\\'), newlines ('\n'), and
 * the pattern characters ('\?' and '\*').  The quotes and backslashes
 * (after expansion) are considered part of the value.
 * </p>
 * <p>
 * Spaces are maintained within the different parts of the name.  Thus,
 * '<code>domain: key1 = value1 </code>' has a key ' key1 ' with value
 * ' value1 '.  Newlines are disallowed, except where escaped in quoted
 * values.
 * </p> 
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class ObjectName
  implements Serializable, QueryExp
{

  /**
   * The domain of the name.
   */
  private String domain;

  /**
   * The properties, as key-value pairs.
   */
  private TreeMap properties;

  /**
   * The properties as a string (stored for ordering).
   */
  private String propertyListString;

  /**
   * True if this object name is a property pattern.
   */
  private boolean propertyPattern;

  /**
   * The management server associated with this object name.
   */
  private MBeanServer server;

  /**
   * Constructs an {@link ObjectName} instance from the given string,
   * which should be of the form
   * &lt;domain&gt;:&lt;properties&gt;&lt;wild&gt;.  &lt;domain&gt;
   * represents the domain section of the name.  &lt;properties&gt;
   * represents the key-value pairs, as returned by {@link
   * #getKeyPropertyListString()}.  &lt;wild&gt; is the optional
   * asterisk present in the property list.  If the string doesn't
   * represent a property pattern, it will be empty.  If it does,
   * it will be either ',*' or '*', depending on whether other
   * properties are present or not, respectively.
   *
   * @param name the string to use to construct this instance.
   * @throws MalformedObjectNameException if the string is of the
   *                                      wrong format.
   * @throws NullPointerException if <code>name</code> is
   *                              <code>null</code>.
   */
  public ObjectName(String name)
    throws MalformedObjectNameException
  {
    int domainSep = name.indexOf(':');
    if (domainSep == -1)
      throw new MalformedObjectNameException("No domain separator was found.");
    domain = name.substring(0, domainSep);
    String rest = name.substring(domainSep + 1);
    if (rest.equals("*"))
      propertyPattern = true;
    else
      {
	if (rest.endsWith(",*"))
	  {
	    propertyPattern = true;
	    propertyListString = rest.substring(0, rest.length() - 2);
	  }
	else
	  propertyListString = rest;
	String[] pairs = propertyListString.split(",");
	if (pairs.length == 0 && !isPattern())
	  throw new MalformedObjectNameException("A name that is not a " +
						 "pattern must contain at " +
						 "least one key-value pair.");
	properties = new TreeMap();
	for (int a = 0; a < pairs.length; ++a)
	  {
	    int sep = pairs[a].indexOf('=');
	    String key = pairs[a].substring(0, sep);
	    if (properties.containsKey(key))
	      throw new MalformedObjectNameException("The same key occurs " +
						     "more than once.");
	    properties.put(key, pairs[a].substring(sep + 1));
	  }	
      }
    checkComponents();
  }

  /**
   * Constructs an {@link ObjectName} instance using the given
   * domain and the one specified property.
   *
   * @param domain the domain part of the object name.
   * @param key the key of the property.
   * @param value the value of the property.
   * @throws MalformedObjectNameException the domain, key or value
   *                                      contains an illegal
   *                                      character or the value
   *                                      does not follow the quoting
   *                                      specifications.
   * @throws NullPointerException if one of the parameters is
   *                              <code>null</code>.
   */
  public ObjectName(String domain, String key, String value)
    throws MalformedObjectNameException
  {
    this.domain = domain;
    properties = new TreeMap();
    properties.put(key, value);
    checkComponents();
  }

  /**
   * Constructs an {@link ObjectName} instance using the given
   * domain and properties.
   *
   * @param domain the domain part of the object name.
   * @param properties the key-value property pairs.
   * @throws MalformedObjectNameException the domain, a key or a value
   *                                      contains an illegal
   *                                      character or a value
   *                                      does not follow the quoting
   *                                      specifications.
   * @throws NullPointerException if one of the parameters is
   *                              <code>null</code>.
   */
  public ObjectName(String domain, Hashtable properties)
    throws MalformedObjectNameException
  {
    this.domain = domain;
    this.properties.putAll(properties);
    checkComponents();
  }

  /**
   * Checks the legality of the domain and the properties.
   *
   * @throws MalformedObjectNameException the domain, a key or a value
   *                                      contains an illegal
   *                                      character or a value
   *                                      does not follow the quoting
   *                                      specifications.
   */
  private void checkComponents()
    throws MalformedObjectNameException
  {
    if (domain.indexOf(':') != -1)
      throw new MalformedObjectNameException("The domain includes a ':' " +
					     "character.");
    if (domain.indexOf('\n') != -1)
      throw new MalformedObjectNameException("The domain includes a newline " +
					     "character.");
    char[] chars = new char[] { ':', ',', '*', '?', '=' };
    Iterator i = properties.entrySet().iterator();
    while (i.hasNext())
      {
	Map.Entry entry = (Map.Entry) i.next();
	String key = (String) entry.getKey();
	for (int a = 0; a < chars.length; ++a)
	  if (key.indexOf(chars[a]) != -1)
	    throw new MalformedObjectNameException("A key contains a '" +
						   chars[a] + "' " +
						   "character.");
	String value = (String) entry.getValue();
	int quote = value.indexOf('"');
	if (quote == 0)
	  {
	    try
	      {
		unquote(value);
	      }
	    catch (IllegalArgumentException e)
	      {
		throw new MalformedObjectNameException("The quoted value is " +
						       "invalid.");
	      }
	  }
	else if (quote != -1)
	  throw new MalformedObjectNameException("A value contains " +
						 "a '\"' character.");
	else
	  {
	    for (int a = 0; a < chars.length; ++a)
	      if (value.indexOf(chars[a]) != -1)
		throw new MalformedObjectNameException("A value contains " +
						       "a '" + chars[a] + "' " +
						       "character.");
	  }
      }
  }

  /**
   * <p>
   * Attempts to find a match between this name and the one supplied.
   * The following criteria are used:
   * </p>
   * <ul>
   * <li>If the supplied name is a pattern, <code>false</code> is
   * returned.</li>
   * <li>If this name is a pattern, this method returns <code>true</code>
   * if the supplied name matches the pattern.</li>
   * <li>If this name is not a pattern, the result of
   * <code>equals(name)</code> is returned.
   * </ul>
   *
   * @param name the name to find a match with.
   * @return true if the name either matches this pattern or is
   *         equivalent to this name under the criteria of
   *         {@link #equals(java.lang.Object)}
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public boolean apply(ObjectName name)
  {
    if (name.isPattern())
      return false;
    if (isPattern())
      {
	boolean domainMatch, propMatch;
	if (isDomainPattern())
	  {
	    String oDomain = name.getDomain();
	    int oLength = oDomain.length();
	    for (int a = 0; a < domain.length(); ++a)
	      {
		char n = domain.charAt(a);
		if (oLength == a && n != '*')
		  return false;
		if (n == '?')
		  continue;
		if (n == '*')
		  if ((a + 1) < domain.length())
		    {
		      if (oLength == a)
			return false;
		      char next;
		      do
			{
			  next = domain.charAt(a + 1);
			} while (next == '*');
		      if (next == '?')
			continue;
		      int pos = a;
		      while (oDomain.charAt(pos) != next)
			{
			  ++pos;
			  if (pos == oLength)
			    return false;
			}
		    }
		if (n != oDomain.charAt(a))
		  return false;
	      }
	    domainMatch = true;
	  }
	else
	  domainMatch = domain.equals(name.getDomain());
	if (isPropertyPattern())
	  {
	    Hashtable oProps = name.getKeyPropertyList();
	    Iterator i = properties.entrySet().iterator();
	    while (i.hasNext())
	      {
		Map.Entry entry = (Map.Entry) i.next();
		String key = (String) entry.getKey();
		if (!(oProps.containsKey(key)))
		  return false;
		String val = (String) entry.getValue();
		if (!(val.equals(oProps.get(key))))
		  return false;
	      }
	    propMatch = true;
	  }
	else
	  propMatch =
	    getCanonicalKeyPropertyListString().equals
	    (name.getCanonicalKeyPropertyListString());
	return domainMatch && propMatch;
      }
    return equals(name);
  }

  /**
   * Compares the specified object with this one.  The two
   * are judged to be equivalent if the given object is an
   * instance of {@link ObjectName} and has an equal canonical
   * form (as returned by {@link #getCanonicalName()}).
   *
   * @param obj the object to compare with this.
   * @return true if the object is also an {@link ObjectName}
   *         with an equivalent canonical form.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof ObjectName)
      {
	ObjectName o = (ObjectName) obj;
	return getCanonicalName().equals(o.getCanonicalName());
      }
    return false;
  }

  /**
   * Returns the property list in canonical form.  The keys
   * are ordered using the lexicographic ordering used by
   * {@link java.lang.String#compareTo(java.lang.Object)}.
   * 
   * @return the property list, with the keys in lexicographic
   *         order.
   */
  public String getCanonicalKeyPropertyListString()
  {
    StringBuilder builder = new StringBuilder();
    Iterator i = properties.entrySet().iterator();
    while (i.hasNext())
      {
	Map.Entry entry = (Map.Entry) i.next();
	builder.append(entry.getKey() + "=" + entry.getValue());
	if (i.hasNext())
	  builder.append(",");
      }
    return builder.toString();
  }

  /**
   * <p>
   * Returns the name as a string in canonical form.  More precisely,
   * this returns a string of the format 
   * &lt;domain&gt;:&lt;properties&gt;&lt;wild&gt;.  &lt;properties&gt;
   * is the same value as returned by
   * {@link #getCanonicalKeyPropertyListString()}.  &lt;wild&gt;
   * is:
   * </p>
   * <ul>
   * <li>an empty string, if the object name is not a property pattern.</li>
   * <li>'*' if &lt;properties&gt; is empty.</li>
   * <li>',*' if there is at least one key-value pair.</li>
   * </ul>
   * 
   * @return the canonical string form of the object name, as specified
   *         above.
   */
  public String getCanonicalName()
  {
    return domain + ":" +
      getCanonicalKeyPropertyListString() +
      (isPropertyPattern() ? (properties.isEmpty() ? "*" : ",*") : "");
  }

  /**
   * Returns the domain part of the object name.
   *
   * @return the domain.
   */
  public String getDomain()
  {
    return domain;
  }

  /**
   * Returns an {@link ObjectName} instance that is substitutable for the
   * one given.  The instance returned may be a subclass of {@link ObjectName},
   * but is not guaranteed to be of the same type as the given name, if that
   * should also turn out to be a subclass.  The returned instance may or may
   * not be equivalent to the one given.  The purpose of this method is to provide
   * an instance of {@link ObjectName} with a well-defined semantics, such as may
   * be used in cases where the given name is not trustworthy.
   *
   * @param name the {@link ObjectName} to provide a substitute for.
   * @return a substitute for the given name, which may or may not be a subclass
   *         of {@link ObjectName}.  In either case, the returned object is
   *         guaranteed to have the semantics defined here.
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public static ObjectName getInstance(ObjectName name)
  {
    try
      {
	return new ObjectName(name.getCanonicalName());
      }
    catch (MalformedObjectNameException e)
      {
	throw (InternalError)
	  (new InternalError("The canonical name of " +
			     "the given name is invalid.").initCause(e));
      }
  }

  /**
   * Returns an {@link ObjectName} instance for the specified name, represented
   * as a {@link java.lang.String}.  The instance returned may be a subclass of
   * {@link ObjectName} and may or may not be equivalent to earlier instances
   * returned by this method for the same string.
   *
   * @param name the {@link ObjectName} to provide an instance of.
   * @return a instance for the given name, which may or may not be a subclass
   *         of {@link ObjectName}.  
   * @throws MalformedObjectNameException the domain, a key or a value
   *                                      contains an illegal
   *                                      character or a value
   *                                      does not follow the quoting
   *                                      specifications.
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public static ObjectName getInstance(String name)
    throws MalformedObjectNameException
  {
    return new ObjectName(name);
  }

  /**
   * Returns an {@link ObjectName} instance for the specified name, represented
   * as a series of {@link java.lang.String} objects for the domain and a single
   * property, as a key-value pair.  The instance returned may be a subclass of
   * {@link ObjectName} and may or may not be equivalent to earlier instances
   * returned by this method for the same parameters.
   *
   * @param domain the domain part of the object name.
   * @param key the key of the property.
   * @param value the value of the property.
   * @return a instance for the given name, which may or may not be a subclass
   *         of {@link ObjectName}.  
   * @throws MalformedObjectNameException the domain, a key or a value
   *                                      contains an illegal
   *                                      character or a value
   *                                      does not follow the quoting
   *                                      specifications.
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public static ObjectName getInstance(String domain, String key, String value)
    throws MalformedObjectNameException
  {
    return new ObjectName(domain, key, value);
  }

  /**
   * Returns an {@link ObjectName} instance for the specified name, represented
   * as a domain {@link java.lang.String} and a table of properties.  The
   * instance returned may be a subclass of {@link ObjectName} and may or may
   * not be equivalent to earlier instances returned by this method for the
   * same string.
   *
   * @param domain the domain part of the object name.
   * @param properties the key-value property pairs.
   * @return a instance for the given name, which may or may not be a subclass
   *         of {@link ObjectName}.  
   * @throws MalformedObjectNameException the domain, a key or a value
   *                                      contains an illegal
   *                                      character or a value
   *                                      does not follow the quoting
   *                                      specifications.
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public static ObjectName getInstance(String domain, Hashtable properties)
    throws MalformedObjectNameException
  {
    return new ObjectName(domain, properties);
  }

  /**
   * Returns the property value corresponding to the given key.
   *
   * @param key the key of the property to be obtained.
   * @return the value of the specified property.
   * @throws NullPointerException if <code>key</code> is <code>null</code>.
   */
  public String getKeyProperty(String key)
  {
    if (key == null)
      throw new NullPointerException("Null key given in request for a value.");
    return (String) properties.get(key);
  }

  /**
   * Returns the properties in a {@link java.util.Hashtable}.  The table
   * contains each of the properties as keys mapped to their value.  The
   * returned table may be unmodifiable.  If the case that the table is
   * modifiable, changes made to it will not be reflected in the object
   * name.
   *
   * @return a {@link java.util.Hashtable}, containing each of the object
   *         name's properties.
   */
  public Hashtable getKeyPropertyList()
  {
    return (Hashtable) Collections.unmodifiableMap(new Hashtable(properties));
  }

  /**
   * Returns a {@link java.lang.String} representation of the property
   * list.  If the object name was created using {@link
   * ObjectName(String)}, then this string will contain the properties
   * in the same order they were given in at creation.
   * 
   * @return the property list.
   */
  public String getKeyPropertyListString()
  {
    if (propertyListString != null)
      return propertyListString;
    return getCanonicalKeyPropertyListString();
  }

  /**
   * Returns a hash code for this object name.  This is calculated as the
   * summation of the hash codes of the domain and the properties.
   *
   * @return a hash code for this object name.
   */
  public int hashCode()
  {
    return domain.hashCode() + properties.hashCode();
  }

  /**
   * Returns true if the domain of this object name is a pattern.
   * This is the case if it contains one or more wildcard characters
   * ('*' or '?').
   *
   * @return true if the domain is a pattern.
   */
  public boolean isDomainPattern()
  {
    return domain.contains("?") || domain.contains("*");
  }

  /**
   * Returns true if this is an object name pattern.  An object
   * name pattern has a domain containing a wildcard character
   * ('*' or '?') and/or a '*' in the list of properties.
   * This method will return true if either {@link #isDomainPattern()}
   * or {@link #isPropertyPattern()} does.
   *
   * @return true if this is an object name pattern.
   */
  public boolean isPattern()
  {
    return isDomainPattern() || isPropertyPattern();
  }

  /**
   * Returns true if this object name is a property pattern.  This is
   * the case if the list of properties contains an '*'.
   *
   * @return true if this is a property pattern.
   */
  public boolean isPropertyPattern()
  {
    return propertyPattern;
  }

  /**
   * <p>
   * Returns a quoted version of the supplied string.  The string may
   * contain any character.  The resulting quoted version is guaranteed
   * to be usable as the value of a property, so this method provides
   * a good way of ensuring that a value is legal.
   * </p>
   * <p>
   * The string is transformed as follows:
   * </p>
   * <ul>
   * <li>The string is prefixed with an opening quote character, '"'.
   * <li>For each character, s:
   * <ul>
   * <li>If s is a quote ('"'), it is replaced by a backslash
   * followed by a quote.</li>
   * <li>If s is a star ('*'), it is replaced by a backslash followed
   * by a star.</li>
   * <li>If s is a question mark ('?'), it is replaced by a backslash
   * followed by a question mark.</li>
   * <li>If s is a backslash ('\'), it is replaced by two backslashes.</li>
   * <li>If s is a newline character, it is replaced by a backslash followed by
   * a '\n'.</li>
   * <li>Otherwise, s is used verbatim.
   * </ul></li>
   * <li>The string is terminated with a closing quote character, '"'.</li>
   * </ul> 
   * 
   * @param string the string to quote.
   * @return a quoted version of the supplied string.
   * @throws NullPointerException if <code>string</code> is <code>null</code>.
   */
  public static String quote(String string)
  {
    StringBuilder builder = new StringBuilder();
    builder.append('"');
    for (int a = 0; a < string.length(); ++a)
      {
	char s = string.charAt(a);
	switch (s)
	  {
	  case '"':
	    builder.append("\\\"");
	    break;
	  case '*':
	    builder.append("\\*");
	    break;
	  case '?':
	    builder.append("\\?");
	    break;
	  case '\\':
	    builder.append("\\\\");
	    break;
	  case '\n':
	    builder.append("\\\n");
	    break;
	  default:
	    builder.append(s);
	  }
      }
    builder.append('"');
    return builder.toString();
  }

  /**
   * Changes the {@link MBeanServer} on which this query is performed.
   *
   * @param server the new server to use.
   */
  public void setMBeanServer(MBeanServer server)
  {
    this.server = server;
  }

  /**
   * Returns a textual representation of the object name.
   * The format is unspecified, but it should be expected that
   * two equivalent object names will return the same string
   * from this method.
   *
   * @return a textual representation of the object name.
   */
  public String toString()
  {
    return getClass().toString() +
      "[domain = " + domain +
      ",properties = " + properties +
      ",propertyPattern = " + propertyPattern +
      "]";
  }

  /**
   * Unquotes the supplied string.  The quotation marks are removed as
   * are the backslashes preceding the escaped characters ('"', '?',
   * '*', '\n', '\\').  A one-to-one mapping exists between quoted and
   * unquoted values.  As a result, a string <code>s</code> should be
   * equal to <code>unquote(quote(s))</code>.
   *
   * @param q the quoted string to unquote.
   * @return the unquoted string.
   * @throws NullPointerException if <code>q</code> is <code>null</code>.
   * @throws IllegalArgumentException if the string is not a valid
   *                                  quoted string i.e. it is not 
   *                                  surrounded by quotation marks
   *                                  and/or characters are not properly
   *                                  escaped.
   */
  public static String unquote(String q)
  {
    if (q.charAt(0) != '"')
      throw new IllegalArgumentException("The string does " +
					 "not start with a quote.");
    if (q.charAt(q.length() - 1) != '"')
      throw new IllegalArgumentException("The string does " +
					 "not end with a quote.");
    StringBuilder builder = new StringBuilder();
    for (int a = 1; a < (q.length() - 1); ++a)
      {
	char n = q.charAt(a);
	if (n == '\\')
	  {
	    n = q.charAt(++a);
	    if (n != '"' && n != '?' && n != '*' &&
		n != '\n' && n != '\\')
	      throw new IllegalArgumentException("Illegal escaped character: "
						 + n);
	  }
	builder.append(n);
      }

    return builder.toString();
  }

}
