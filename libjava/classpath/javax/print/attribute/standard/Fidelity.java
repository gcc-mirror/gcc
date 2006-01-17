/* Fidelity.java --
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
exception statement from your version.  */


package javax.print.attribute.standard;

import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.PrintRequestAttribute;


/**
 * The <code>Fidelity</code> attribute specifies how a print job is handled
 * if the supplied attributes are not fully supported.
 * <p>
 * There may be conflicts between the client requested attributes and the
 * attributes supported by the printer object. Such situations are controlled
 * through the client by providing this attribute to indicate the wanted
 * conflict handling mechanism:
 * <ul>
 * <li>{@link #FIDELITY_TRUE}: Reject the job since the job can not be 
 * processed exactly as specified by the attributes of the client.</li>
 * <li>{@link #FIDELITY_FALSE}: The Printer may make any changes necessary 
 * to proceed with processing the Job as good as possible.</li>
 * </ul>
 * </p> 
 * <p>
 * <b>IPP Compatibility:</b> Fidelity is an IPP 1.1 attribute. The IPP name
 * is "ipp-attribute-fidelity". The IPP specification treats Fidelity as a 
 * boolean type which is not available in the Java Print Service API. The IPP
 * boolean value "true" corresponds to <code>FIDELITY_TRUE</code> and "false" 
 * to <code>FIDELITY_FALSE</code>.
 * </p>
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class Fidelity extends EnumSyntax
  implements PrintJobAttribute, PrintRequestAttribute
{
  private static final long serialVersionUID = 6320827847329172308L;

  /** 
   * Requests that the job is printed exactly as specified, 
   * or rejected otherwise.
   */
  public static final Fidelity FIDELITY_TRUE = new Fidelity(0);
  
  /** 
   * Requests that the job is printed as exactly as reasonable. This means
   * that the print service may choose to substitute the default value 
   * associated with that attribute, or use some other supported value that 
   * is similar to the unsupported requested value. 
   */
  public static final Fidelity FIDELITY_FALSE = new Fidelity(1);
  
  private static final String[] stringTable = { "true", "false" };
  private static final Fidelity[] enumValueTable = { FIDELITY_TRUE,
                                                     FIDELITY_FALSE };

  /**
   * Constructs a <code>Fidelity</code> object.
   * 
   * @param value the value
   */
  protected Fidelity(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>Fidelity</code> itself.
   */
  public Class getCategory()
  {
    return Fidelity.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "ipp-attribute-fidelity".
   */
  public String getName()
  {
    return "ipp-attribute-fidelity";
  }
  
  /**
   * Returns a table with the enumeration values represented as strings
   * for this object.
   *
   * @return The enumeration values as strings.
   */
  protected String[] getStringTable()
  {
    return stringTable;
  }

  /**
   * Returns a table with the enumeration values for this object.
   *
   * @return The enumeration values.
   */
  protected EnumSyntax[] getEnumValueTable()
  {
    return enumValueTable;
  }
}
