/* AttributedCharacterIterator.java -- Iterate over attributes
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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


package java.text;

import java.io.Serializable;
import java.io.InvalidObjectException;
import java.util.Set;
import java.util.Map;

/**
 * This interface extends the <code>CharacterIterator</code> interface
 * in order to support iteration over character attributes as well as
 * over the characters themselves.
 * <p>
 * In addition to attributes of specific characters, this interface
 * supports the concept of the "attribute run", which is an attribute
 * that is defined for a particular value across an entire range of
 * characters or which is undefined over a range of characters.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public interface AttributedCharacterIterator extends CharacterIterator
{
  /**
   * This class defines attribute keys that are used as text attributes.
   */
  public static class Attribute implements Serializable
  {
    private static final long serialVersionUID = -9142742483513960612L;

    /**
     * This is the attribute for the language of the text.  The value of
     * attributes of this key type are instances of <code>Locale</code>.
     */
    public static final Attribute LANGUAGE = new Attribute ("LANGUAGE");

    /**
     * This is the attribute for the reading form of text.  This is used
     * for storing pronunciation along with the written text for languages
     * which need it.  The value of attributes of this key type are
     * instances of <code>Annotation</code> which wrappers a <code>String</code>.
     */
    public static final Attribute READING = new Attribute ("READING");

    /**
     * This is the attribute for input method segments.  The value of attributes
     * of this key type are instances of <code>Annotation</code> which wrapper
     * a <code>String</code>.
     */
    public static final Attribute INPUT_METHOD_SEGMENT =
      new Attribute ("INPUT_METHOD_SEGMENT");

    /**
     * This is the name of the attribute key
     * @serial
     */
    private String name;

    /**
     * This method initializes a new instance of this class with the specified
     * name.
     *
     * @param name The name of this attribute key.
     */
    protected Attribute (String name)
    {
      this.name = name;
    }

    /**
     * This method returns the name of this attribute.
     *
     * @return The attribute name
     */
    protected String getName()
    {
      return name;
    }

    /**
     * This method resolves an instance of <code>AttributedCharacterIterator.Attribute</code>
     * that is being deserialized to one of the three pre-defined attribute
     * constants.  It does this by comparing the names of the attributes.  The
     * constant that the deserialized object resolves to is returned.
     *
     * @return The resolved contant value
     *
     * @exception InvalidObjectException If the object being deserialized cannot be resolved.
     */
    protected Object readResolve() throws InvalidObjectException
    {
      if (this.equals (READING))
        return READING;

      if (this.equals (LANGUAGE))
        return LANGUAGE;

      if (this.equals (INPUT_METHOD_SEGMENT))
        return INPUT_METHOD_SEGMENT;

      throw new InvalidObjectException ("Can't resolve Attribute: " + getName());
    }
    
    /**
     * This method tests this object for equality against the specified object.
     * The two objects will be considered equal if and only if:
     * <ul>
     * <li>The specified object is not <code>null</code>.
     * <li>The specified object is an instance of <code>AttributedCharacterIterator.Attribute</code>.
     * <li>The specified object has the same attribute name as this object.
     * </ul>
     *
     * @param The <code>Object</code> to test for equality against this object.
     *
     * @return <code>true</code> if the specified object is equal to this one, <code>false</code> otherwise.
     */
    public final boolean equals (Object obj)
    {
      if (obj == this)
        return true;
      else 
        return false;
    }

    /**
     * This method returns a hash value for this object.
     *
     * @return A hash value for this object.
     */
    public final int hashCode()
    {
      return super.hashCode();
    }

    /**
     * This method returns a <code>String</code> representation of this object.
     *
     * @return A <code>String</code> representation of this object.
     */
    public String toString()
    {
      return getClass().getName() + "(" + getName() + ")";
    }

  } // Inner class Attribute

  /**
   * This method returns a list of all keys that are defined for the 
   * text range.  This can be an empty list if no attributes are defined.
   *
   * @return A list of keys 
   */
  Set getAllAttributeKeys();

  /**
   * This method returns a <code>Map</code> of the attributed defined for
   * the current character.
   *
   * @return A <code>Map</code> of the attributes for the current character.
   */
  Map getAttributes();

  /**
   * This method returns the value of the specified attribute for the
   * current character.  If the attribute is not defined for the current
   * character, <code>null</code> is returned.
   *
   * @param attrib The attribute to retrieve the value of.
   *
   * @return The value of the specified attribute
   */
  Object getAttribute (AttributedCharacterIterator.Attribute attrib);

  /**
   * This method returns the index of the first character in the run that
   * contains all attributes defined for the current character.
   *
   * @return The start index of the run
   */
  int getRunStart();

  /**
   * This method returns the index of the first character in the run that
   * contains all attributes in the specified <code>Set</code> defined for
   * the current character.
   *
   * @param attribs The <code>Set</code> of attributes.
   *
   * @return The start index of the run.
   */
  int getRunStart (Set attribs);
  
  /**
   * This method returns the index of the first character in the run that
   * contains the specified attribute defined for the current character.
   *
   * @param attrib The attribute.
   *
   * @return The start index of the run.
   */
  int getRunStart (AttributedCharacterIterator.Attribute attrib);
  
  /**
   * This method returns the index of the character after the end of the run
   * that contains all attributed defined for the current character.
   *
   * @return The end index of the run.
   */
  int getRunLimit();
  
  /**
   * This method returns the index of the character after the end of the run
   * that contains all attributes in the specified <code>Set</code> defined
   * for the current character.
   *
   * @param attribs The <code>Set</code> of attributes.
   *
   * @return The end index of the run.
   */
  int getRunLimit (Set attribs);
  
  /**
   * This methods returns the index of the character after the end of the run
   * that contains the specified attribute defined for the current character.
   *
   * @param attrib The attribute.
   * 
   * @return The end index of the run.
   */
  int getRunLimit (AttributedCharacterIterator.Attribute attrib);

} // interface AttributedCharacterIterator
