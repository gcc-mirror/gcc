/* gnuDTD.java --
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


package gnu.javax.swing.text.html.parser;

import java.io.PrintStream;
import java.io.Serializable;

import java.util.BitSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.ContentModel;
import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.Entity;

/**
 * <p>
 * The class is derived from {@link gnu.javax.swing.text.html.parser.DTD }
 * making structure creation methods public. This is required when
 * creating the DTD by SGML parser that must have access to the structure.
 *
 * SGML DTD representation. Provides basis for describing a syntax of the
 * HTML documents. The fields of this class are NOT initialized in
 * constructor. You need to do this separately before passing this data
 * structure to the parser constructor.</p>
 *
 * <p>This implementation also provides you the derived class
 * <code>gnu.javax.swing.text.html.parser.DTD.HTML_4_0_1</code>, where
 * all fields are initialized to the values, representing HTML 4.01
 * ("-//W3C//DTD HTML 4.01 Frameset//EN") DTD. You can use it if you do not care
 * about the portability between different implementations of the core
 * class libraries. </p>
 * <p>Use {@link javax.swing.HTML.HTMLEditorKit.Parser#parse }
 * for parsing in accordance with "-//W3C//DTD HTML 4.01 Frameset//EN"
 * without specifying DTD separately.</p>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuDTD
  extends javax.swing.text.html.parser.DTD
  implements javax.swing.text.html.parser.DTDConstants, Serializable
{
  /* The undocumented element types, used to specify types, not defined
  in DTDConstants. */

  /**
   * The URI element type (not defined in DTDConstants).
   */
  public static final int URI = 512;

  /**
   * The Length element type
   */
  public static final int Length = 513;

  /**
   * The Char element type
   */
  public static final int Char = 514;

  /**
   * The Color element type
   */
  public static final int Color = 515;

  /**
   * Creates a new instance of gnuDTD.
   * @param name the name of the DTD.
   */
  public gnuDTD(String name)
  {
    super(name);
  }

  /**
   * Creates and returns new attribute (not an attribute list).
   * @param name the name of this attribute
   * @param type the type of this attribute (FIXED, IMPLIED or
   * REQUIRED from <code>DTDConstants</code>).
   * @param modifier the modifier of this attribute
   * @param default_value the default value of this attribute or null if
   * it is not specified.
   * @param allowed_values the allowed values of this attribute. The multiple
   * possible values in this parameter are supposed to be separated by
   * '|', same as in SGML DTD <code>&lt;!ATTLIST </code>tag. This parameter
   * can be null if no list of allowed values is specified.
   * @param atts the previous attribute of this element. This is
   * placed to the field
   * {@link javax.swing.text.html.parser.AttributeList#next },
   * creating a linked list.
   * @return
   */
  public AttributeList defAttributeList(String name, int type, int modifier,
                                        String default_value,
                                        String allowed_values,
                                        AttributeList atts
                                       )
  {
    return super.defAttributeList(name, type, modifier, default_value,
                                  allowed_values, atts
                                 );
  }

  /**
   * Define the attributes for the element with the given name.
   * If the element is not exist, it is created. This method is
   * needed if the element attributes are defined befor the
   * element itself.
   * @param forElement
   * @param attributes
   */
  public void defAttrsFor(String forElement, AttributeList attributes)
  {
    super.defineAttributes(forElement, attributes);
  }

  /**
   * Creates a new content model.
   * @param type specifies the BNF operation for this content model.
   * The valid operations are documented in the
   * {@link javax.swing.text.html.parser.ContentModel#type }.
   * @param content the content of this content model
   * @param next if the content model is specified by BNF-like
   * expression, contains the rest of this expression.
   * @return The newly created content model.
   */
  public ContentModel defContentModel(int type, Object content,
                                      ContentModel next
                                     )
  {
    return super.defContentModel(type, content, next);
  }

  /**
   * Defines a new element and adds it to the element table.
   * If the element alredy exists,
   * overrides it settings with the specified values.
   * @param name the name of the new element
   * @param type the type of the element
   * @param headless true if the element needs no starting tag
   * @param tailless true if the element needs no closing tag
   * @param content the element content.
   * @param exclusions the elements that must be excluded from the
   * content of this element, in all levels of the hierarchy.
   * @param inclusions the elements that can be included as the
   * content of this element.
   * @param attributes the element attributes.
   * @return the created or updated element.
   */
  public Element defElement(String name, int type, boolean headless,
                            boolean tailless, ContentModel content,
                            String[] exclusions, String[] inclusions,
                            AttributeList attributes
                           )
  {
    return super.defElement(name, type, headless, tailless, content,
                            exclusions, inclusions, attributes
                           );
  }

  /**
   * Defines a new element and adds it to the element table.
   * If the element alredy exists,
   * overrides it settings with the specified values.
   * @param name the name of the new element
   * @param type the type of the element
   * @param headless true if the element needs no starting tag
   * @param tailless true if the element needs no closing tag
   * @param content the element content.
   * @param exclusions the elements that must be excluded from the
   * content of this element, in all levels of the hierarchy.
   * @param inclusions the elements that can be included as the
   * content of this element.
   * @param attributes the element attributes.
   * @return the created or updated element.
   */
  public Element defElement(String name, int type, boolean headless,
                            boolean tailless, ContentModel content,
                            Collection exclusions, Collection inclusions,
                            AttributeList attributes
                           )
  {
    return super.defElement(name, type, headless, tailless, content,
                            toStringArray(exclusions),
                            toStringArray(inclusions), attributes
                           );
  }

  /**
   * Defines a new element and adds it to the element table.
   * If the element alredy exists,
   * overrides it settings with the specified values.
   * @param name the name of the new element
   * @param type the type of the element
   * @param headless true if the element needs no starting tag
   * @param tailless true if the element needs no closing tag
   * @param content the element content.
   * @param exclusions the elements that must be excluded from the
   * content of this element, in all levels of the hierarchy.
   * @param inclusions the elements that can be included as the
   * content of this element.
   * @param attributes the element attributes (an array and not a
   * linked list). The attributes are chained into the linked list
   * inside this method.
   * @return the created or updated element.
   */
  public Element defElement(String name, int type, boolean headless,
                            boolean tailless, ContentModel content,
                            String[] exclusions, String[] inclusions,
                            AttributeList[] attributes
                           )
  {
    AttributeList list;

    if (attributes == null || attributes.length == 0)
      list = null;
    else
      {
        if (attributes.length > 1)
          for (int i = 1; i < attributes.length; i++)
            {
              attributes [ i - 1 ].next = attributes [ i ];
            }
        list = attributes [ 0 ];
      }

    Element e =
      super.defElement(name, type, headless, tailless, content, exclusions,
                       inclusions, list
                      );
    return e;
  }

  /**
   * Creates, adds into the internal table and returns the
   * character entity like <code>&amp;lt;</code>
   *  (means '<code>&lt;</code>' );
   * This method inactivates the recursive refenrences to the same
   * entity.
   * @param name The entity name (without heading &amp; and closing ;)
   * @param type The entity type
   * @param character The entity value (single character)
   * @return The created entity
   */
  public Entity defEntity(String name, int type, String data)
  {
    int r;
    String eref = "%" + name + ";";
    do
      {
        r = data.indexOf(eref);
        if (r > 0)
          {
            data = data.substring(0, r) + data.substring(r + 1);
          }
      }
    while (r > 0);

    return super.defEntity(name, type, data);
  }

  /**
   * Summarises the document content into the given PrintStream.
   */
  public void dump(PrintStream p)
  {
    Iterator iter = entityHash.entrySet().iterator();
    while (iter.hasNext())
      {
        Map.Entry item = (Map.Entry) iter.next();
        Entity e = (Entity) item.getValue();
        if (e.isGeneral())
          p.println("Entity " + e.getName() + ": " + e.getString());
      }

    iter = elementHash.entrySet().iterator();
    while (iter.hasNext())
      {
        Map.Entry item = (Map.Entry) iter.next();
        Element e = (Element) item.getValue();
        p.println("Element " + e.getName());

        System.out.println(" includes:");
        dump(e.inclusions);
        System.out.println(" excludes:");
        dump(e.exclusions);
        System.out.println(" attributes:");

        AttributeList atts = e.atts;
        while (atts != null)
          {
            p.print("    " + atts.name + " = " + atts.value);
            if (atts.values == null || atts.values.size() == 0)
              p.println();
            else
              {
                Iterator viter = atts.values.iterator();
                System.out.print(" ( ");
                while (viter.hasNext())
                  {
                    System.out.print(viter.next());
                    if (viter.hasNext())
                      System.out.print(" | ");
                  }
                System.out.println(" ) ");
              }
            atts = atts.next;
          }
      }
  }

  /**
   * Prints the content of the given attribute set to the System.out.
   * @param b
   */
  public void dump(BitSet b)
  {
    if (b != null)
      {
        for (int i = 0; i < b.size(); i++)
          {
            if (b.get(i))
              System.out.println(" " + elements.get(i));
          }
      }
    else
      System.out.println(" NULL set");
  }

  /**
   * Creates the attribute.
   * @param name The attribute name.
   * @param type The attribute type.
   * @param modifier The attribute modifier.
   * @param defaultValue Default value (or null)
   * @param allowed_values Allowed values (or null)
   * @return The newly created AttributeList. The <code>next</code>
   * field is initialized to null.
   */
  protected AttributeList attr(String name, String default_value,
                               String[] allowed_values, int type, int modifier
                              )
  {
    Vector allowed = null;

    if (allowed_values != null)
      {
        allowed = new Vector(allowed_values.length);
        for (int i = 0; i < allowed_values.length; i++)
          {
            allowed.add(allowed_values [ i ]);
          }
      }

    AttributeList attr =
      new AttributeList(name, type, modifier, default_value, allowed, null);

    return attr;
  }

  /**
   * Define the general entity, holding a single character.
   * @param name The entity name (for example, 'amp').
   * The defined entity <b>is</b> stored into the entity table.
   * @param character The entity character (for example, '&').
   */
  protected void defineEntity(String name, int character)
  {
    super.defEntity(name, GENERAL, character);
  }

  private String[] toStringArray(Collection c)
  {
    String[] s = new String[ c.size() ];
    Iterator iter = c.iterator();
    for (int i = 0; i < s.length; i++)
      {
        s [ i ] = iter.next().toString();
      }
    return s;
  }
}
