/* DTD.java --
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


package javax.swing.text.html.parser;

import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.BitSet;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * <p>Representation or the SGML DTD document.
 * Provides basis for describing a syntax of the
 * HTML documents. The fields of this class are NOT initialized in
 * constructor. You need to do this separately before passing this data
 * structure to the HTML parser. The subclasses with the fields, pre-
 * initialized, for example, for HTML 4.01, can be available only between
 * the implementation specific classes
 * ( for example, {@link gnu.javax.swing.text.html.parser.HTML_401F }
 * in this implementation).</p>
 * <p>
 * If you need more information about SGML DTD documents,
 * the author suggests to read SGML tutorial on
 * <a href="http://www.w3.org/TR/WD-html40-970708/intro/sgmltut.html"
 * >http://www.w3.org/TR/WD-html40-970708/intro/sgmltut.html</a>.
 * We also recommend Goldfarb C.F (1991) <i>The SGML Handbook</i>,
 * Oxford University Press, 688 p, ISBN: 0198537379.
 * </p>
 * <p>
 * Warning: the html, head and other tag fields will only be automatically
 * assigned if the VM has the correctly implemented reflection mechanism.
 * As these fields are not used anywhere in the implementation, not
 * exception will be thrown in the opposite case.
 * </p>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class DTD
  implements DTDConstants
{
  /**
   * The version of the persistent data format.
   * @specnote This was made <code>final</code> in 1.5.
   */
  public static final int FILE_VERSION = 1;

  /**
   * The table of existing available DTDs.
   */
  static Hashtable dtdHash = new Hashtable();

  /**
   * The applet element for this DTD.
   */
  public Element applet;

  /**
   * The base element for this DTD.
   */
  public Element base;

  /**
   * The body element for this DTD.
   */
  public Element body;

  /**
   * The head element for this DTD.
   */
  public Element head;

  /**
   * The html element for this DTD.
   */
  public Element html;

  /**
   * The isindex element of for this DTD.
   */
  public Element isindex;

  /**
   * The meta element for this DTD.
   */
  public Element meta;

  /**
   * The p element for this DTD.
   */
  public Element p;

  /**
   * The param element for this DTD.
   */
  public Element param;

  /**
   * The pcdata for this DTD.
   */
  public Element pcdata;

  /**
   * The title element for this DTD.
   */
  public Element title;

  /**
   * The element for accessing all DTD elements by name.
   */
  public Hashtable elementHash = new Hashtable();

  /**
   * The entity table for accessing all DTD entities by name.
   */
  public Hashtable entityHash = new Hashtable();

  /**
   *  The name of this DTD.
   */
  public String name;

  /**
   * Contains all elements in this DTD. The
   * javax.swing.text.html.parser.Element#index field of all elements
   * in this vector is set to the element position in this vector.
   */
  public Vector elements = new Vector();

  /** Create a new DTD with the specified name. */
  protected DTD(String a_name)
  {
    name = a_name;
  }

  /** Get this DTD by name. The current implementation
   * only looks in the internal table of DTD documents. If no corresponding
   * entry is found, the new entry is created, placed into
   * the table and returned. */
  public static DTD getDTD(String name)
                    throws IOException
  {
    DTD d = (DTD) dtdHash.get(name);

    if (d == null)
      {
        d = new DTD(name);
        dtdHash.put(d.name, d);
      }

    return d;
  }

  /**
   * Get the element by the element name. If the element is not yet
   * defined, it is newly created and placed into the element table.
   * If the element name matches (ingoring case) a public non static
   * element field in this class, this field is assigned to the value
   * of the newly created element.
   */
  public Element getElement(String element_name)
  {
    return newElement(element_name);
  }

  /**
   * Get the element by the value of its
   * {@link javax.swing.text.html.parser.Element#index} field.
   */
  public Element getElement(int index)
  {
    return (Element) elements.get(index);
  }

  /**
   * Get the entity with the given identifier.
   * @param id that can be returned by
   * {@link javax.swing.text.html.parser.Entity#name2type(String an_entity)}
   * @return The entity from this DTD or null if there is no entity with
   * such id or such entity is not present in the table of this instance.
   */
  public Entity getEntity(int id)
  {
    String name = Entity.mapper.get(id);

    if (name != null)
      return (Entity) entityHash.get(name);
    else
      return null;
  }

  /**
   * Get the named entity by its name.
   */
  public Entity getEntity(String entity_name)
  {
    return (Entity) entityHash.get(entity_name);
  }

  /**
   * Get the name of this instance of DTD
   */
  public String getName()
  {
    return name;
  }

  /**
   * Creates, adds into the entity table and returns the
   * character entity like <code>&amp;lt;</code>
   *  (means '<code>&lt;</code>' );
   * @param name The entity name (without heading &amp; and closing ;)
   * @param type The entity type
   * @param character The entity value (single character)
   * @return The created entity
   */
  public Entity defEntity(String name, int type, int character)
  {
    Entity e = newEntity(name, type);
    e.data = new char[] { (char) character };
    return e;
  }

  /**
   * Define the attributes for the element with the given name.
   * If the element is not exist, it is created.
   * @param forElement
   * @param attributes
   */
  public void defineAttributes(String forElement, AttributeList attributes)
  {
    Element e = (Element) elementHash.get(forElement.toLowerCase());

    if (e == null)
      e = newElement(forElement);

    e.atts = attributes;
  }

  /**
   * Defines the element and adds it to the element table. Sets the
   * <code>Element.index</code> field to the value, unique for this
   * instance of DTD. If the element with the given name already exists,
   * replaces all other its settings by the method argument values.
   * @param name the name of the element
   * @param type the type of the element
   * @param headless true if the element needs no starting tag
   * (should not occur in HTML).
   * @param tailless true if the element needs no ending tag (like
   * <code>&lt;hr&gt;</code>
   * @param content the element content
   * @param exclusions the set of elements that must not occur inside
   * this element. The <code>Element.index</code> value defines which
   * bit in this bitset corresponds to that element.
   * @param inclusions the set of elements that can occur inside this
   * element. the <code>Element.index</code> value defines which
   * bit in this bitset corresponds to that element.
   * @param attributes the element attributes.
   * @return the newly defined element.
   */
  public Element defineElement(String name, int type, boolean headless,
                               boolean tailless, ContentModel content,
                               BitSet exclusions, BitSet inclusions,
                               AttributeList attributes
                              )
  {
    Element e = newElement(name);
    e.type = type;
    e.oStart = headless;
    e.oEnd = tailless;
    e.content = content;
    e.exclusions = exclusions;
    e.inclusions = inclusions;
    e.atts = attributes;

    return e;
  }

  /**
   * Creates, intializes and adds to the entity table the new
   * entity.
   * @param name the name of the entity
   * @param type the type of the entity
   * @param data the data section of the entity
   * @return the created entity
   */
  public Entity defineEntity(String name, int type, char[] data)
  {
    Entity e = newEntity(name, type);
    e.data = data;

    return e;
  }

  /** Place this DTD into the DTD table. */
  public static void putDTDHash(String name, DTD dtd)
  {
    dtdHash.put(name, dtd);
  }

  /**
   * <p>Reads DTD from an archived format. This format is not standardized
   * and differs between implementations.</p><p> This implementation
   * reads and defines all entities and elements using
   * ObjectInputStream. The elements and entities can be written into the
   * stream in any order. The objects other than elements and entities
   * are ignored.</p>
   * @param stream A data stream to read from.
   * @throws java.io.IOException If one is thrown by the input stream
   */
  public void read(DataInputStream stream)
            throws java.io.IOException
  {
    ObjectInputStream oi = new ObjectInputStream(stream);
    Object def;
    try
      {
        while (true)
          {
            def = oi.readObject();
            if (def instanceof Element)
              {
                Element e = (Element) def;
                elementHash.put(e.name.toLowerCase(), e);
                assignField(e);
              }
            else if (def instanceof Entity)
              {
                Entity e = (Entity) def;
                entityHash.put(e.name, e);
              }
          }
      }
    catch (ClassNotFoundException ex)
      {
        throw new IOException(ex.getMessage());
      }
    catch (EOFException ex)
      {
        // ok EOF
      }
  }

  /**
   * Returns the name of this instance of DTD.
   */
  public String toString()
  {
    return name;
  }

  /**
   * Creates and returns new attribute (not an attribute list).
   * @param name the name of this attribute
   * @param type the type of this attribute (FIXED, IMPLIED or
   * REQUIRED from <code>DTDConstants</code>).
   * @param modifier the modifier of this attribute
   * @param default_value the default value of this attribute
   * @param allowed_values the allowed values of this attribute. The multiple
   * possible values in this parameter are supposed to be separated by
   * '|', same as in SGML DTD <code>&lt;!ATTLIST </code>tag. This parameter
   * can be null if no list of allowed values is specified.
   * @param atts the previous attribute of this element. This is
   * placed to the field
   * {@link javax.swing.text.html.parser.AttributeList#next },
   * creating a linked list.
   * @return The attributes.
   */
  protected AttributeList defAttributeList(String name, int type, int modifier,
                                           String default_value,
                                           String allowed_values,
                                           AttributeList atts
                                          )
  {
    AttributeList al = new AttributeList(name);
    al.modifier = modifier;
    al.value = default_value;
    al.next = atts;

    if (allowed_values != null)
      {
        StringTokenizer st = new StringTokenizer(allowed_values, " \t|");
        Vector v = new Vector(st.countTokens());

        while (st.hasMoreTokens())
          v.add(st.nextToken());

        al.values = v;
      }

    return al;
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
  protected ContentModel defContentModel(int type, Object content,
                                         ContentModel next
                                        )
  {
    ContentModel model = new ContentModel();
    model.type = type;
    model.next = next;
    model.content = content;

    return model;
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
  protected Element defElement(String name, int type, boolean headless,
                               boolean tailless, ContentModel content,
                               String[] exclusions, String[] inclusions,
                               AttributeList attributes
                              )
  {
    // compute the bit sets
    BitSet exclude = bitSet(exclusions);
    BitSet include = bitSet(inclusions);

    Element e =
      defineElement(name, type, headless, tailless, content, exclude, include,
                    attributes
                   );

    return e;
  }

  /**
   * Creates, intializes and adds to the entity table the new
   * entity.
   * @param name the name of the entity
   * @param type the type of the entity
   * @param data the data section of the entity
   * @return the created entity
   */
  protected Entity defEntity(String name, int type, String data)
  {
    Entity e = newEntity(name, type);
    e.data = data.toCharArray();

    return e;
  }

  private void assignField(Element e)
  {
    String element_name = e.name;
    try
      {
        // Assign the field via reflection.
        Field f = getClass().getField(element_name.toLowerCase());
        if ((f.getModifiers() & Modifier.PUBLIC) != 0)
          if ((f.getModifiers() & Modifier.STATIC) == 0)
            if (f.getType().isAssignableFrom(e.getClass()))
              f.set(this, e);
      }
    catch (IllegalAccessException ex)
      {
        unexpected(ex);
      }
    catch (NoSuchFieldException ex)
      {
        // This is ok.
      }

    // Some virtual machines may still lack the proper
    // implementation of reflection. As the tag fields
    // are not used anywhere in this implementation,
    // (and this class is also rarely used by the end user),
    // it may be better not to crash everything by throwing an error
    // for each case when the HTML parsing is required.
    catch (Throwable t)
      {
        // This VM has no reflection mechanism implemented!
        if (t instanceof OutOfMemoryError)
          throw (Error) t;
      }
  }

  /**
   * Create the bit set for this array of elements.
   * The unknown elements are automatically defined and added
   * to the element table.
   * @param elements
   * @return The bit set.
   */
  private BitSet bitSet(String[] elements)
  {
    BitSet b = new BitSet();

    for (int i = 0; i < elements.length; i++)
      {
        Element e = getElement(elements [ i ]);

        if (e == null)
          e = newElement(elements [ i ]);

        b.set(e.index);
      }

    return b;
  }

  /**
   * Find the element with the given name in the element table.
   * If not find, create a new element with this name and add to the
   * table.
   * @param name the name of the element
   * @return the found or created element.
   */
  private Element newElement(String name)
  {
    Element e = (Element) elementHash.get(name.toLowerCase());

    if (e == null)
      {
        e = new Element();
        e.name = name;
        e.index = elements.size();
        elements.add(e);
        elementHash.put(e.name.toLowerCase(), e);
        assignField(e);
      }
    return e;
  }

  /**
   * Creates and adds to the element table the entity with an
   * unitialized data section. Used internally.
   * @param name the name of the entity
   * @param type the type of the entity, a bitwise combination
   * of GENERAL, PARAMETER, SYSTEM and PUBLIC.
   *
   * @return the created entity
   */
  private Entity newEntity(String name, int type)
  {
    Entity e = new Entity(name, type, null);
    entityHash.put(e.name, e);
    return e;
  }

  private void unexpected(Exception ex)
  {
    throw new Error("This should never happen, report a bug", ex);
  }
}
