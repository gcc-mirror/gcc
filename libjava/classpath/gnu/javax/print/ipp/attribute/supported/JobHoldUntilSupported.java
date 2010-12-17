/* JobHoldUntilSupported.java --
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


package gnu.javax.print.ipp.attribute.supported;

import java.util.Locale;

import javax.print.attribute.Attribute;
import javax.print.attribute.SupportedValuesAttribute;
import javax.print.attribute.TextSyntax;

/**
 * JobHoldUntilSupported attribute provides the supported
 * values for the attribute type job-hold-until.
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public final class JobHoldUntilSupported extends TextSyntax
  implements SupportedValuesAttribute
{

  // a keyword/name based attribute in IPP
  // can be extended by administrators
  // standard values are predefined

  /** Job should be printed immediately. */
  public static final JobHoldUntilSupported NO_HOLD =
    new JobHoldUntilSupported("no-hold", null);

  /** Job should be hold indefinitely. */
  public static final JobHoldUntilSupported INDEFINITE =
    new JobHoldUntilSupported("indefinite", null);

  /**  Job should be processed during the day. */
  public static final JobHoldUntilSupported DAY_TIME =
    new JobHoldUntilSupported("day-time", null);

  /**  Job should be processed in the evening. */
  public static final JobHoldUntilSupported EVENING =
    new JobHoldUntilSupported("evening", null);

  /**  Job should be processed during night. */
  public static final JobHoldUntilSupported NIGHT =
    new JobHoldUntilSupported("night", null);

  /**  Job should be processed during the weekend. */
  public static final JobHoldUntilSupported WEEKEND =
    new JobHoldUntilSupported("weekend", null);

  /**
   * Job should be processed as second-shift
   * (after close of business).
   */
  public static final JobHoldUntilSupported SECOND_SHIFT =
    new JobHoldUntilSupported("second-shift", null);

  /**
   * Job should be processed as third-shift
   * (after midnight).
   */
  public static final JobHoldUntilSupported THIRD_SHIFT =
    new JobHoldUntilSupported("third-shift", null);

  /**
   * Creates a <code>JobHoldUntilSupported</code> object with the
   * given value and locale.
   *
   * @param value the value for this syntax
   * @param locale the locale to use, if <code>null</code> the default
   * locale is used.
   *
   * @throws NullPointerException if value is null
   */
  public JobHoldUntilSupported(String value, Locale locale)
  {
    super(value, locale);
  }

  /**
   * Returns category of this class.
   *
   * @return The class <code>JobHoldUntilSupported</code> itself.
   */
  public Class<? extends Attribute> getCategory()
  {
    return JobHoldUntilSupported.class;
  }

  /**
   * Returns the name of this attribute.
   *
   * @return The name "job-hold-until-supported".
   */
  public String getName()
  {
    return "job-hold-until-supported";
  }

}
