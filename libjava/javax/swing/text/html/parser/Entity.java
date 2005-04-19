/* Entity.java -- Stores information, obtained by parsing SGML DTL
 * &lt;!ENTITY % .. &gt; tag
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

/**
 * <p>Stores information, obtained by parsing SGML DTL
 * &lt;!ENTITY % .. &gt; tag.</p>
 * <p>
 * The entity defines some kind of macro that can be used elsewhere in
 * the document.
 * When the macro is referred to by the name in the DTD, it is expanded into
 * a string
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class Entity
  implements DTDConstants, Serializable
{
  /**
   * Package level mapper between type names and they string values.
   */
  final static gnuStringIntMapper mapper =
    new gnuStringIntMapper()
    {
      protected void create()
      {
        add("ANY", DTDConstants.ANY);
        add("CDATA", DTDConstants.CDATA);
        add("PUBLIC", DTDConstants.PUBLIC);
        add("SDATA", DTDConstants.SDATA);
        add("PI", DTDConstants.PI);
        add("STARTTAG", DTDConstants.STARTTAG);
        add("ENDTAG", DTDConstants.ENDTAG);
        add("MS", DTDConstants.MS);
        add("MD", DTDConstants.MD);
        add("SYSTEM", DTDConstants.SYSTEM);
      }
    };

  /**
   * The entity name.
   */
  public String name;

  /**
   * The entity data
   */
  public char[] data;

  /**
   *  The entity type.
   */
  public int type;

  /**
   * String representation of the entity data.
   */
  private String sdata;

  /**
   * Create a new entity
   * @param a_name the entity name
   * @param a_type the entity type
   * @param a_data the data replacing the entity reference
   */
  public Entity(String a_name, int a_type, char[] a_data)
  {
    name = a_name;
    type = a_type;
    data = a_data;
  }

  /**
   * Converts a given string to the corresponding entity type.
   * @return a value, defined in DTDConstants (one of
   * PUBLIC, CDATA, SDATA, PI, STARTTAG, ENDTAG, MS, MD, SYSTEM)
   * or CDATA if the parameter is not a valid entity type.
   */
  public static int name2type(String an_entity)
  {
    int r = mapper.get(an_entity);
    return (r == 0) ? DTDConstants.CDATA : r;
  }

  /**
   * Get the entity data.
   */
  public char[] getData()
  {
    return data;
  }

  /**
   * Returns true for general entities. Each general entity can be
   * referenced as <code>&entity-name;</code>. Such entities are
   * defined by the SGML DTD tag
   * <code>&lt;!ENTITY <i>name</i>    "<i>value</i>"></code>. The general
   * entities can be used anywhere in the document.
   */
  public boolean isGeneral()
  {
    return (type & DTDConstants.GENERAL) != 0;
  }

  /**
   * Get the entity name.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Returns true for parameter entities. Each parameter entity can be
   * referenced as <code>&entity-name;</code>. Such entities are
   * defined by the SGML DTD tag
   * <code>&lt;!ENTITY % <i>name</i>    "<i>value</i>"></code>. The parameter
   * entities can be used only in SGML context.
   */
  public boolean isParameter()
  {
    return (type & DTDConstants.PARAMETER) != 0;
  }

  /**
   * Returns a data as String
   */
  public String getString()
  {
    if (sdata == null)
      sdata = new String(data);

    return sdata;
  }
  
  /**
   * Get the entity type.
   * @return the value of the {@link #type}.
   */
  public int getType() 
  {
    return type;
  }  
          
}
