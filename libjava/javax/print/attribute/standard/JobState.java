/* JobState.java --
   Copyright (C) 2004 Free Software Foundation, Inc.

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

import javax.print.attribute.EnumSyntax;
import javax.print.attribute.PrintJobAttribute;


/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public class JobState extends EnumSyntax
  implements PrintJobAttribute
{
  private static final long serialVersionUID = 400465010094018920L;

  public static final JobState UNKNOWN = new JobState(0);
  public static final JobState PENDING = new JobState(1);
  public static final JobState PENDING_HELD = new JobState(2);
  public static final JobState PROCESSING = new JobState(3);
  public static final JobState PROCESSING_STOPPED = new JobState(4);
  public static final JobState CANCELED = new JobState(5);
  public static final JobState ABORTED = new JobState(6);
  public static final JobState COMPLETED = new JobState(7);

  /**
   * Constructs a <code>JobState</code> object.
   */
  protected JobState(int value)
  {
    super(value);
  }

  /**
   * Returns category of this class.
   *
   * @return the class <code>JobState</code> itself
   */
  public Class getCategory()
  {
    return JobState.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return the name
   */
  public String getName()
  {
    return "job-state";
  }
}
