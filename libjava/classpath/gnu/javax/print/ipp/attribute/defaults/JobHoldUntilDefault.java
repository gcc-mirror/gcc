/* JobHoldUntilDefault.java --
   Copyright (C) 2006 Free Software Foundation, Inc.

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

import java.util.Date;
import java.util.Locale;

import javax.print.attribute.Attribute;
import javax.print.attribute.TextSyntax;
import javax.print.attribute.standard.JobHoldUntil;

/**
 * JobHoldUntilDefault attribute provides the default value
 * for the attribute type job-hold-until.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class JobHoldUntilDefault extends TextSyntax
  implements DefaultValueAttribute
{

  // a keyword/name based attribute in IPP
  // can be extended by administrators
  // standard values are predefined

  /** Job should be printed immediately. */
  public static final JobHoldUntilDefault NO_HOLD =
    new JobHoldUntilDefault("no-hold", null);

  /** Job should be hold indefinitely. */
  public static final JobHoldUntilDefault INDEFINITE =
    new JobHoldUntilDefault("indefinite", null);

  /**  Job should be processed during the day. */
  public static final JobHoldUntilDefault DAY_TIME =
    new JobHoldUntilDefault("day-time", null);

  /**  Job should be processed in the evening. */
  public static final JobHoldUntilDefault EVENING =
    new JobHoldUntilDefault("evening", null);

  /**  Job should be processed during night. */
  public static final JobHoldUntilDefault NIGHT =
    new JobHoldUntilDefault("night", null);

  /**  Job should be processed during the weekend. */
  public static final JobHoldUntilDefault WEEKEND =
    new JobHoldUntilDefault("weekend", null);

  /**
   * Job should be processed as second-shift
   * (after close of business).
   */
  public static final JobHoldUntilDefault SECOND_SHIFT =
    new JobHoldUntilDefault("second-shift", null);

  /**
   * Job should be processed as third-shift
   * (after midnight).
   */
  public static final JobHoldUntilDefault THIRD_SHIFT =
    new JobHoldUntilDefault("third-shift", null);

  /**
   * Creates a <code>JobHoldUntilDefault</code> object with the
   * given value and locale.
   *
   * @param value the value for this syntax
   * @param locale the locale to use, if <code>null</code> the default
   * locale is used.
   *
   * @throws NullPointerException if value is null
   */
  public JobHoldUntilDefault(String value, Locale locale)
  {
    super(value, locale);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobHoldUntilDefault</code> itself.
   */
  public Class<? extends Attribute> getCategory()
  {
    return JobHoldUntilDefault.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-hold-until-default".
   */
  public String getName()
  {
    return "job-hold-until-default";
  }

  /**
   * Returns the equally enum of the standard attribute class
   * of this DefaultValuesAttribute enum.
   *
   * @return The enum of the standard attribute class.
   */
  public Attribute getAssociatedAttribute()
  {
    // FIXME Same Mapping problem as in IppPrintService
    return new JobHoldUntil(new Date());
  }

}
