/* Attributes.java -- Represents attribute name/value pairs from a Manifest
   Copyright (C) 2000, 2002, 2005 Free Software Foundation, Inc.

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

package java.util.jar;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;

/**
 * Represents attribute name/value pairs from a Manifest as a Map.
 * The names of an attribute are represented by the
 * <code>Attributes.Name</code> class and should confirm to the restrictions
 * described in that class. Note that the Map interface that Attributes
 * implements allows you to put names and values into the attribute that don't
 * follow these restriction (and are not really Atrribute.Names, but if you do
 * that it might cause undefined behaviour later).
 * <p>
 * If you use the constants defined in the inner class Name then you can be
 * sure that you always access the right attribute names. This makes
 * manipulating the Attributes more or less type safe.
 * <p>
 * Most of the methods are wrappers to implement the Map interface. The really
 * useful and often used methods are <code>getValue(Name)</code> and
 * <code>getValue(String)</code>. If you actually want to set attributes you
 * may want to use the <code>putValue(String, String)</code> method
 * (sorry there is no public type safe <code>putValue(Name, String)</code>
 * method).
 *
 * @see java.util.jar.Attributes.Name
 * @author Mark Wielaard (mark@klomp.org)
 */
public class Attributes implements Cloneable, Map
{

  // Fields

  /**
   * The map that holds all the attribute name/value pairs. In this
   * implementation it is actually a Hashtable, but that can be different in
   * other implementations.
   */
  protected Map map;

  // Inner class

  /**
   * Represents a name of a Manifest Attribute. Defines a couple of well
   * know names for the general main attributes, stand alone application
   * attributes, applet attributes, extension identification attributes,
   * package versioning and sealing attributes, file contents attributes,
   * bean objects attribute and signing attributes. See the 
   * 
   * <p>The characters of a Name must obey the following restrictions:</p>
   * 
   * <ul>
   * <li>Must contain at least one character</li>
   * <li>The first character must be alphanumeric (a-z, A-Z, 0-9)</li>
   * <li>All other characters must be alphanumeric, a '-' or a '_'</li>
   * </ul>
   * 
   * <p>When comparing Names (with <code>equals</code>) all characters are
   * converted to lowercase. But you can get the original case sensitive
   * string with the <code>toString()</code> method.</p>
   *
   * <p>Most important attributes have a constant defined in this
   * class. Some other attributes used in Manifest files are:
   * <ul>
   * <li> "Created-By" - General main attribute, tool and version
   * that created this Manifest file.</li>
   * <li> "Java-Bean" - Bean objects attribute, whether the entry is a Bean.
   * Value is either "true" or "false".</li>
   * <li> "Magic" - Signing attribute, application specific signing attribute.
   * Must be understood by the manifest parser when present to validate the
   * jar (entry).</li>
   * </ul>
   *
   * @since 1.2
   * @author Mark Wielaard (mark@klomp.org)
   */
  public static class Name
  {
    // General Main Attributes

    /**
     * General main attribute -
     * the version of this Manifest file.
     */
    public static final Name MANIFEST_VERSION = new Name("Manifest-Version");
    
    /**
     * General main attribute -
     * the version of the jar file signature.
     */
    public static final Name SIGNATURE_VERSION
      = new Name("Signature-Version");
    
    /**
     * General main attribute -
     * (relative) file paths of the libraries/classpaths that the Classes in
     * this jar file depend on. Paths are separated by spaces.
     */
    public static final Name CLASS_PATH = new Name("Class-Path");

    /**
     * Stand alone application attribute -
     * the entry (without the .class ending) that is the main
     * class of this jar file.
     */
    public static final Name MAIN_CLASS = new Name("Main-Class");

    /**
     * Applet attribute -
     * a list of extension libraries that the applet in this
     * jar file depends on.
     * For every named extension there should be some Attributes in the
     * Manifest manifest file with the following Names:
     * <ul>
     * <li> &lt;extension&gt;-Extension-Name:
     * unique name of the extension</li>
     * <li> &lt;extension&gt;-Specification-Version:
     * minimum specification version</li>
     * <li> &lt;extension&gt;-Implementation-Version:
     * minimum implementation version</li>
     * <li> &lt;extension&gt;-Implementation-Vendor-Id:
     * unique id of implementation vendor</li>
     * <li> &lt;extension&gt;-Implementation-URL:
     * where the latest version of the extension library can be found</li>
     * </ul>
     */
    public static final Name EXTENSION_LIST = new Name("Extension-List");

    /**
     * Extension identification attribute -
     * the name if the extension library contained in the jar.
     */
    public static final Name EXTENSION_NAME = new Name("Extension-Name");
    
    /**
     * Extension identification attribute -
     * synonym for <code>EXTENSTION_NAME</code>.
     */
    public static final Name EXTENSION_INSTALLATION = EXTENSION_NAME;

    // Package versioning and sealing attributes
    
    /**
     * Package versioning -
     * name of extension library contained in this jar.
     */
    public static final Name IMPLEMENTATION_TITLE
      = new Name("Implementation-Title");
    
    /**
     * Package versioning -
     * version of the extension library contained in this jar.
     */
    public static final Name IMPLEMENTATION_VERSION
      = new Name("Implementation-Version");
    
    /**
     * Package versioning -
     * name of extension library creator contained in this jar.
     */
    public static final Name IMPLEMENTATION_VENDOR
      = new Name("Implementation-Vendor");
    
    /**
     * Package versioning -
     * unique id of extension library creator.
     */
    public static final Name IMPLEMENTATION_VENDOR_ID
      = new Name("Implementation-Vendor-Id");
    
    /**
     * Package versioning -
     * location where this implementation can be downloaded.
     */
    public static final Name IMPLEMENTATION_URL
      = new Name("Implementation-URL");
    
    /**
     * Package versioning -
     * title of the specification contained in this jar.
     */
    public static final Name SPECIFICATION_TITLE
      = new Name("Specification-Title");

    /**
     * Package versioning -
     * version of the specification contained in this jar.
     */
    public static final Name SPECIFICATION_VERSION
      = new Name("Specification-Version");

    /**
     * Package versioning -
     * organisation that maintains the specification contains in this
     * jar.
     */
    public static final Name SPECIFICATION_VENDOR
      = new Name("Specification-Vendor");

    /**
     * Package sealing -
     * whether (all) package(s) is(/are) sealed. Value is either "true"
     * or "false".
     */
    public static final Name SEALED = new Name("Sealed");

    /**
     * File contents attribute -
     * Mime type and subtype for the jar entry.
     */
    public static final Name CONTENT_TYPE = new Name("Content-Type");

    /** The (lowercase) String representation of this Name */
    private final String name;

    /** The original String given to the constructor */
    private final String origName;

    // Constructor

    /**
     * Creates a new Name from the given String.
     * Throws an IllegalArgumentException if the given String is empty or
     * contains any illegal Name characters.
     * 
     * @param name the name of the new Name
     * @exception IllegalArgumentException if name isn't a valid String
     * representation of a Name
     * @exception NullPointerException if name is null
     */
    public Name(String name) throws IllegalArgumentException,
      NullPointerException
    {
      // name must not be null
      // this will throw a NullPointerException if it is
      char chars[] = name.toCharArray();

      // there must be at least one character
      if (chars.length == 0)
	throw new
	  IllegalArgumentException
	  ("There must be at least one character in a name");

      // first character must be alphanum
      char c = chars[0];
      if (!((c >= 'a' && c <= 'z') ||
	    (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')))
	throw new
	  IllegalArgumentException("First character must be alphanum");

      // all other characters must be alphanums, '-' or '_'
      for (int i = 1; i < chars.length; i++)
	{
	  c = chars[i];
	  if (!((c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		(c >= '0' && c <= '9') || (c == '-') || (c == '_')))
	    throw new
	      IllegalArgumentException
	      ("Characters must be alphanums, '-' or '_'");
	}

      // Still here? Then convert to lower case and be done.
      // Store the original name for toString();
      this.origName = name;
      this.name = name.toLowerCase();
    }

    /**
     * Returns the hash code of the (lowercase) String representation of
     * this Name.
     */
    public int hashCode()
    {
      return name.hashCode();
    }

    /**
     * Checks if another object is equal to this Name object.
     * Another object is equal to this Name object if it is an instance of
     * Name and the (lowercase) string representation of the name is equal.
     */
    public boolean equals(Object o)
    {
      // Quick and dirty check
      if (name == o)
	return true;

      try
	{
	  // Note that the constructor already converts the strings to
	  // lowercase.
	  String otherName = ((Name) o).name;
	  return name.equals(otherName);
	}
      catch (ClassCastException cce)
	{
	  return false;
	}
      catch (NullPointerException npe)
	{
	  return false;
	}
    }

    /**
     * Returns the string representation of this Name as given to the
     * constructor (not neccesarily the lower case representation).
     */
    public String toString()
    {
      return origName;
    }
  }

  // Constructors

  /**
   * Creates an empty Attributes map.
   */
  public Attributes()
  {
    map = new Hashtable();
  }

  /**
   * Creates an empty Attributes map with the given initial size.
   * @param size the initial size of the underlying map
   */
  public Attributes(int size)
  {
    map = new Hashtable(size);
  }

  /**
   * Creates an Attributes map with the initial values taken from another
   * Attributes map.
   * @param attr Attributes map to take the initial values from
   */
  public Attributes(Attributes attr)
  {
    map = new Hashtable(attr.map);
  }

  // Methods

  /**
   * Gets the value of an attribute name given as a String.
   *
   * @param name a String describing the Name to look for
   * @return the value gotten from the map of null when not found
   */
  public String getValue(String name)
  {
    return (String) get(new Name(name));
  }

  /**
   * Gets the value of the given attribute name.
   *
   * @param name the Name to look for
   * @return the value gotten from the map of null when not found
   */
  public String getValue(Name name)
  {
    return (String) get(name);
  }

  /**
   * Stores an attribute name (represented by a String) and value in this
   * Attributes map.
   * When the (case insensitive string) name already exists the value is
   * replaced and the old value is returned.
   *
   * @param name a (case insensitive) String representation of the attribite
   * name to add/replace
   * @param value the (new) value of the attribute name
   * @returns the old value of the attribute name or null if it didn't exist
   * yet
   */
  public String putValue(String name, String value)
  {
    return putValue(new Name(name), value);
  }

  /**
   * Stores an attribute name (represented by a String) and value in this
   * Attributes map.
   * When the name already exists the value is replaced and the old value
   * is returned.
   * <p>
   * I don't know why there is no public method with this signature. I think
   * there should be one.
   *
   * @param name the attribite name to add/replace
   * @param value the (new) value of the attribute name
   * @returns the old value of the attribute name or null if it didn't exist
   * yet
   */
  String putValue(Name name, String value)
  {
    return (String) put(name, value);
  }

  // Methods from Cloneable interface

  /**
   * Return a clone of this attribute map.
   */
  public Object clone()
  {
    return new Attributes(this);
  }

  // Methods from Map interface

  /**
   * Removes all attributes.
   */
  public void clear()
  {
    map.clear();
  }

  /**
   * Checks to see if there is an attribute with the specified name.
   * XXX - what if the object is a String?
   *
   * @param attrName the name of the attribute to check
   * @return true if there is an attribute with the specified name, false
   * otherwise
   */
  public boolean containsKey(Object attrName)
  {
    return map.containsKey(attrName);
  }

  /**
   * Checks to see if there is an attribute name with the specified value.
   *
   * @param attrValue the value of a attribute to check
   * @return true if there is an attribute name with the specified value,
   * false otherwise
   */
  public boolean containsValue(Object attrValue)
  {
    return map.containsValue(attrValue);
  }

  /**
   * Gives a Set of attribute name and values pairs as MapEntries.
   * @see java.util.Map.Entry
   * @see java.util.Map#entrySet()
   *
   * @return a set of attribute name value pairs
   */
  public Set entrySet()
  {
    return map.entrySet();
  }

  /**
   * Checks to see if two Attributes are equal. The supplied object must be
   * a real instance of Attributes and contain the same attribute name/value
   * pairs.
   *
   * @param o another Attribute object which should be checked for equality
   * @return true if the object is an instance of Attributes and contains the
   * same name/value pairs, false otherwise
   */
  public boolean equals(Object o)
  {
    // quick and dirty check
    if (this == o)
      return true;

    try
      {
	return map.equals(((Attributes) o).map);
      }
    catch (ClassCastException cce)
      {
	return false;
      }
    catch (NullPointerException npe)
      {
	return false;
      }
  }

  /**
   * Gets the value of a specified attribute name.
   * XXX - what if the object is a String?
   *
   * @param attrName the name of the attribute we want the value of
   * @return the value of the specified attribute name or null when there is
   * no such attribute name
   */
  public Object get(Object attrName)
  {
    return map.get(attrName);
  }

  /**
   * Returns the hashcode of the attribute name/value map.
   */
  public int hashCode()
  {
    return map.hashCode();
  }

  /**
   * Returns true if there are no attributes set, false otherwise.
   */
  public boolean isEmpty()
  {
    return map.isEmpty();
  }

  /**
   * Gives a Set of all the values of defined attribute names.
   */
  public Set keySet()
  {
    return map.keySet();
  }

  /**
   * Adds or replaces a attribute name/value pair.
   * XXX - What if the name is a string? What if the name is neither a Name
   * nor a String? What if the value is not a string?
   *
   * @param name the name of the attribute
   * @param value the (new) value of the attribute
   * @return the old value of the attribute or null when there was no old
   * attribute with this name
   */
  public Object put(Object name, Object value)
  {
    return map.put(name, value);
  }

  /**
   * Adds or replaces all attribute name/value pairs from another
   * Attributes object to this one. The supplied Map must be an instance of
   * Attributes.
   *
   * @param attr the Attributes object to merge with this one
   * @exception ClassCastException if the supplied map is not an instance of
   * Attributes
   */
  public void putAll(Map attr)
  {
    if (!(attr instanceof Attributes))
      {
	throw new
	  ClassCastException("Supplied Map is not an instance of Attributes");
      }
    map.putAll(attr);
  }

  /**
   * Remove a attribute name/value pair.
   * XXX - What if the name is a String?
   *
   * @param name the name of the attribute name/value pair to remove
   * @return the old value of the attribute or null if the attribute didn't
   * exist
   */
  public Object remove(Object name)
  {
    return map.remove(name);
  }

  /**
   * Returns the number of defined attribute name/value pairs.
   */
  public int size()
  {
    return map.size();
  }

  /**
   * Returns all the values of the defined attribute name/value pairs as a
   * Collection.
   */
  public Collection values()
  {
    return map.values();
  }
}
