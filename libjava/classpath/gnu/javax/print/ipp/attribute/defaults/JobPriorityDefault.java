/* JobPriorityDefault.java -- 
   Copyright (C) 2006  Free Software Foundation, Inc.

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

package gnu.javax.print.ipp.attribute.defaults;

import gnu.javax.print.ipp.attribute.DefaultValueAttribute;

import javax.print.attribute.Attribute;
import javax.print.attribute.IntegerSyntax;
import javax.print.attribute.standard.JobPriority;


/**
 * JobPriorityDefault attribute provides the default value of
 * the printer object for the job-priority attribute.
 * 
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class JobPriorityDefault extends IntegerSyntax
  implements DefaultValueAttribute
{
  
  /**
   * Creates a <code>JobPriorityDefault</code> object.
   *
   * @param value the priority
   *
   * @exception IllegalArgumentException if value &lt; 1 or value &gt; 100
   */
  public JobPriorityDefault(int value)
  {
    super(value);

    if (value < 1 || value > 100)
      throw new IllegalArgumentException("value out of range");
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
    if(! (obj instanceof JobPriorityDefault))
      return false;

    return super.equals(obj);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobPriorityDefault</code> itself.
   */
  public Class getCategory()
  {
    return JobPriorityDefault.class;
  }

  /**
   * Returns name of this class.
   *
   * @return The anme "job-priority-default".
   */
  public String getName()
  {
    return "job-priority-default";
  }
  
  /**
   * Returns the equally enum of the standard attribute class
   * of this DefaultValuesAttribute enum.
   * 
   * @return The enum of the standard attribute class.
   */
  public Attribute getAssociatedAttribute() 
  {
    return new JobPriority(getValue());
  }
}
