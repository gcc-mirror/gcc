/* Element.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package javax.swing.text.html.parser;

import gnu.javax.swing.text.html.parser.support.gnuStringIntMapper;

import java.io.Serializable;

import java.util.BitSet;

/**
 * <p>
 * Stores the element information, obtained by parsing SGML DTD
 * tag <code>&lt;!ELEMENT .. &gt;</code>. This class has no public
 * constructor and can only be instantiated using the
 * {@link javax.swing.text.html.parser.DTD } methods</p>
 *
 * <p>SGML defines elements that represent structures or
 * behavior. An element typically consists of a start tag, content, and an
 * end tag. Hence the elements are not tags. The HTML 4.0 definition specifies
 * that some elements are not required to have the end tags. Also, some
 * HTML elements (like <code>&lt;hr&gt;</code>) have no content. Element names
 * are case sensitive.</p>
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class Element
  implements DTDConstants, Serializable
{
  /**
   * Package level mapper between type names and they string values.
   */
  static final gnuStringIntMapper mapper =
    new gnuStringIntMapper()
    {
      protected void create()
      {
        add("CDATA", DTDConstants.CDATA);
        add("RCDATA", DTDConstants.RCDATA);
        add("EMPTY", DTDConstants.EMPTY);
        add("ANY", DTDConstants.ANY);
      }
    };

  /** Use serialVersionUID for interoperability. */
  private static final long serialVersionUID = -6717939384601675586L;

  /**
   * The element attributes.
   */
  public AttributeList atts;

  /**
   * Contains refernces to elements that must NOT occur inside this element,
   * at any level of hierarchy.
   */
  public BitSet exclusions;

  /**
   * Contains refernces to elements that must CAN occur inside this element,
   * at any level of hierarchy.
   */
  public BitSet inclusions;

  /**
   * The content model, defining elements, entities and DTD text
   * that may/may not occur inside this element.
   */
  public ContentModel content;

  /**
   * A field to store additional user data for this Element.
   */
  public Object data;

  /**
   * The element name.
   */
  public String name;

  /**
   * True is this element need not to have the closing tag, false
   * otherwise. The HTML 4.0 definition specifies
   * that some elements (like <code>&lt;hr&gt;</code>are
   * not required to have the end tags.
   */
  public boolean oEnd;

  /**
   * True is this element need not to have the starting tag, false
   * otherwise. The HTML 4.0 definition specifies
   * that some elements (like <code>&lt;head&gt;</code> or
   * <code>&lt;body&gt;</code>) are
   * not required to have the start tags.

   */
  public boolean oStart;

  /**
   * This field contains the unique integer identifier of this Element,
   * used to refer the element (more exactly, the element flag)
   * in <code>inclusions</code> and <code>exclusions</code> bit set.
   */
  public int index;

  /**
   * The element type, containing value, defined in DTDConstants.
   * In this implementation, the element type can be
   * CDATA, RCDATA, EMPTY or ANY.
   */
  public int type;

  /**
   * The default constructor must have package level access in this
   * class. Use DTD.defineElement(..) to create an element when required.
   * @todo MAKE THIS PACKAGE in the final version. Now the Parser needs it!
   */
  Element()
  {
  }

  /**
   * Converts the string representation of the element type
   * into its unique integer identifier, defined in DTDConstants.
   * @param a_type A name of the type
   * @return DTDConstants.CDATA, DTDConstants.RCDATA, DTDConstants.EMPTY,
   * DTDConstants.ANY or null if the type name is not
   * "CDATA", "RCDATA", "EMPTY" or "ANY". This function is case sensitive.
   * @throws NullPointerException if <code>a_type</code> is null.
   */
  public static int name2type(String a_type)
  {
    return mapper.get(a_type);
  }

  /**
   * Get the element attribute by name.
   * @param attribute the attribute name, case insensitive.
   * @return the correspoding attribute of this element. The class,
   * for storing as attribute list, as a single attribute, is used to
   * store a single attribute in this case.
   * @throws NullPointerException if the attribute name is null.
   */
  public AttributeList getAttribute(String attribute)
  {
    AttributeList a = atts;

    while (a != null && !attribute.equalsIgnoreCase(a.name))
      a = a.next;

    return a;
  }

  /**
   * Get the element attribute by its value.
   * @param a_value the attribute value, case insensitive.
   * @return the correspoding attribute of this element. The class,
   * for storing as attribute list, as a single attribute, is used to
   * store a single attribute in this case. If there are several
   * attributes with the same value, there is no garranty, which one
   * is returned.
   */
  public AttributeList getAttributeByValue(String a_value)
  {
    AttributeList a = atts;

    if (a_value == null)
      {
        while (a != null)
          {
            if (a.value == null)
              return a;

            a = a.next;
          }
      }
    else
      {
        while (a != null)
          {
            if (a.value != null && a_value.equalsIgnoreCase(a.value))
              return a;

            a = a.next;
          }
      }

    return null;
  }

  /**
   * Get all attributes of this document as an attribute list.
   * @return
   */
  public AttributeList getAttributes()
  {
    return atts;
  }

  /**
   * Get the content model, defining elements, entities and DTD text
   * that may/may not occur inside this element.
   */
  public ContentModel getContent()
  {
    return content;
  }

  /**
   * Returns true for the element with no content.
   * Empty elements are defined with the SGML DTD keyword "EMPTY".
   * @return true if content model field (content) method is equal to
   * null or its method empty() returns true.
   */
  public boolean isEmpty()
  {
    return content == null || content.empty();
  }

  /**
   * Get the unique integer identifier of this Element,
   * used to refer the element (more exactly, the element flag)
   * in <code>inclusions</code> and <code>exclusions</code> bit set.
   * WARNING: This value may not be the same between different
   * implementations.
   */
  public int getIndex()
  {
    return index;
  }

  /**
   * Get the element name.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Get the element type.
   * @return one of the values, defined DTDConstants.
   * In this implementation, the element type can be
   * CDATA, RCDATA, EMPTY or ANY.
   */
  public int getType()
  {
    return type;
  }

  /**
   * True is this element need not to have the starting tag, false
   * otherwise.s element need not to have the closing tag, false
   * otherwise. The HTML 4.0 definition specifies
   * that some elements (like <code>&lt;hr&gt;</code>are
   * not required to have the end tags.
   */
  public boolean omitEnd()
  {
    return oEnd;
  }

  /**
   * True is this element need not to have the closing tag, false
   * otherwise. The HTML 4.0 definition specifies
   * that some elements (like <code>&lt;head&gt;</code> or
   * <code>&lt;body&gt;</code>) are
   * not required to have the start tags.
   */
  public boolean omitStart()
  {
    return oStart;
  }

  /**
   * Returns the name of this element.
   */
  public String toString()
  {
    return name;
  }
}
