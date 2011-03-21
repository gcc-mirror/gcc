/* JobOriginatingUserName.java --
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
exception statement from your version. */


package javax.print.attribute.standard;

import java.util.Locale;

import javax.print.attribute.Attribute;
import javax.print.attribute.PrintJobAttribute;
import javax.print.attribute.TextSyntax;

/**
 * The <code>JobOriginatingUserName</code> attribute specifies the name of
 * the user which submitted the print job.
 * <p>
 * <b>IPP Compatibility:</b> JobOriginatingUserName is an IPP 1.1 attribute.
 * </p>
 *
 * @author Michael Koch (konqueror@gmx.de)
 */
public final class JobOriginatingUserName extends TextSyntax
  implements PrintJobAttribute
{
  private static final long serialVersionUID = -8052537926362933477L;

  /**
   * Creates a <code>JobOriginatingUserName</code> object.
   *
   * @param userName the user name
   * @param locale the locale to use, if <code>null</code> the default
   * locale is used.
   *
   * @exception NullPointerException if userName is <code>null</code>.
   */
  public JobOriginatingUserName(String userName, Locale locale)
  {
    super(userName, locale);
  }

  /**
   * Tests if the given object is equal to this object.
   *
   * @param obj the object to test
   *
   * @return <code>true</code> if both objects are equal,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if(! (obj instanceof JobOriginatingUserName))
      return false;

    return super.equals(obj);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobOriginatingUserName</code> itself.
   */
  public Class< ? extends Attribute> getCategory()
  {
    return JobOriginatingUserName.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-originating-user-name".
   */
  public String getName()
  {
    return "job-originating-user-name";
  }
}
