/* SpinnerDateModel.java --
   Copyright (C) 2002, 2004, 2006 Free Software Foundation, Inc.

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


package javax.swing;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;

import javax.swing.event.ChangeEvent;

/**
 * A date model used by the {@link JSpinner} component.  This implements a
 * spinner model for dates, rotating a calendar field such as  month, year,
 * day, week, hour, minute.
 *
 * @author Sven de Marothy
 * @since 1.4
 */
public class SpinnerDateModel extends AbstractSpinnerModel
  implements Serializable
{
  /** The current date. */
  private Calendar date;

  /**
   * A constraint on the start or earliest permitted date (<code>null</code>
   * for no minimum).
   */
  private Comparable start;

  /**
   * A constraint on the end or latest permitted date (<code>null</code> for no
   * maximum).
   */
  private Comparable end;

  /**
   * The calendar field used to calculate the previous or next date.
   */
  private int calendarField;

  /**
   * For compatability with Sun's JDK
   */
  private static final long serialVersionUID = -4802518107105940612L;

  /**
   * Constructs a <code>SpinnerDateModel</code> using the current date,
   * no start or end limit, and {@link Calendar#DAY_OF_MONTH} as the calendar
   * field.
   */
  public SpinnerDateModel()
  {
    this(new Date(), null, null, Calendar.DAY_OF_MONTH);
  }

  /**
   * Constructs a <code>SpinnerDateModel</code> with the specified value, lower
   * and upper bounds, and which spins the specified calendar field.
   * <p>
   * The <code>start</code> and <code>end</code> limits must have a
   * <code>compareTo</code> method that supports instances of {@link Date}, but
   * do not themselves need to be instances of {@link Date} (although typically
   * they are).
   *
   * @param value  the initial value/date (<code>null</code> not permitted).
   * @param start  a constraint that specifies the earliest permitted date
   *     value, or <code>null</code> for no lower limit.
   * @param end  a constraint that specifies the latest permitted date value,
   *     or <code>null</code> for no upper limit.
   * @param calendarField  the <code>Calendar</code> field to spin,
   *     (Calendar.ZONE_OFFSET and Calendar.DST_OFFSET are invalid)
   */
  public SpinnerDateModel(Date value, Comparable start, Comparable end,
                          int calendarField)
  {
    if (value == null)
      throw new IllegalArgumentException("Null 'value' argument.");
    if (start != null && start.compareTo(value) > 0)
      throw new IllegalArgumentException("Require value on or after start.");
    if (end != null && end.compareTo(value) < 0)
      throw new IllegalArgumentException("Require value on or before end.");
    date = Calendar.getInstance();
    date.setTime(value);
    this.start = start;
    this.end = end;
    setCalendarField(calendarField);
  }

  /**
   * Returns the {@link Calendar} field used to calculate the previous and
   * next dates in the sequence.
   *
   * @return The date field code.
   */
  public int getCalendarField()
  {
    return calendarField;
  }

  /**
   * Returns the current date/time.
   *
   * @return The current date/time (never <code>null</code>).
   *
   * @see #getValue()
   */
  public Date getDate()
  {
    return date.getTime();
  }

  /**
   * Returns the lower limit on the date/time value, or <code>null</code> if
   * there is no minimum date/time.
   *
   * @return The lower limit.
   *
   * @see #setStart(Comparable)
   */
  public Comparable getStart()
  {
    return start;
  }

  /**
   * Returns the upper limit on the date/time value, or <code>null</code> if
   * there is no maximum date/time.
   *
   * @return The upper limit.
   *
   * @see #setEnd(Comparable)
   */
  public Comparable getEnd()
  {
    return end;
  }

  /**
   * Returns the current date in the sequence (this method returns the same as
   * {@link #getDate()}).
   *
   * @return The current date (never <code>null</code>).
   */
  public Object getValue()
  {
    return date.getTime();
  }

  /**
   * Returns the next date in the sequence, or <code>null</code> if the
   * next date is past the upper limit (if one is specified).  The current date
   * is not changed.
   *
   * @return The next date, or <code>null</code> if the current value is
   *         the latest date represented by the model.
   *
   * @see #getEnd()
   */
  public Object getNextValue()
  {
    Calendar nextCal = Calendar.getInstance();
    nextCal.setTime(date.getTime());
    nextCal.roll(calendarField, true);
    Date nextDate = nextCal.getTime();
    if (end != null)
      if (end.compareTo(nextDate) < 0)
        return null;
    return nextDate;
  }

  /**
   * Returns the previous date in the sequence, or <code>null</code> if the
   * previous date is prior to the lower limit (if one is specified).  The
   * current date is not changed.
   *
   * @return The previous date, or <code>null</code> if the current value is
   *         the earliest date represented by the model.
   *
   * @see #getStart()
   */
  public Object getPreviousValue()
  {
    Calendar prevCal = Calendar.getInstance();
    prevCal.setTime(date.getTime());
    prevCal.roll(calendarField, false);
    Date prevDate = prevCal.getTime();
    if (start != null)
      if (start.compareTo(prevDate) > 0)
        return null;
    return prevDate;
  }

  /**
   * Sets the date field to change when calculating the next and previous
   * values. It must be a valid {@link Calendar} field, excluding
   * {@link Calendar#ZONE_OFFSET} and {@link Calendar#DST_OFFSET}.
   *
   * @param calendarField  the calendar field to set.
   *
   * @throws IllegalArgumentException if <code>calendarField</code> is not
   *         a valid code.
   */
  public void setCalendarField(int calendarField)
  {
    if (calendarField < 0 || calendarField >= Calendar.FIELD_COUNT
        || calendarField == Calendar.ZONE_OFFSET
        || calendarField == Calendar.DST_OFFSET)
      throw new IllegalArgumentException("Illegal calendarField");

    if (this.calendarField != calendarField)
      {
        this.calendarField = calendarField;
        fireStateChanged();
      }
  }

  /**
   * Sets the lower limit for the date/time value and, if the new limit is
   * different to the old limit, sends a {@link ChangeEvent} to all registered
   * listeners.  A <code>null</code> value is interpreted as "no lower limit".
   * No check is made to ensure that the current date/time is on or after the
   * new lower limit - the caller is responsible for ensuring that this
   * relationship holds.  In addition, the caller should ensure that
   * <code>start</code> is {@link Serializable}.
   *
   * @param start  the new lower limit for the date/time value
   *     (<code>null</code> permitted).
   */
  public void setStart(Comparable start)
  {
    if (this.start != start)
      {
        this.start = start;
        fireStateChanged();
      }
  }

  /**
   * Sets the upper limit for the date/time value and, if the new limit is
   * different to the old limit, sends a {@link ChangeEvent} to all registered
   * listeners.  A <code>null</code> value is interpreted as "no upper limit".
   * No check is made to ensure that the current date/time is on or before the
   * new upper limit - the caller is responsible for ensuring that this
   * relationship holds.  In addition, the caller should ensure that
   * <code>end</code> is {@link Serializable}.
   *
   * @param end  the new upper limit for the date/time value (<code>null</code>
   *     permitted).
   */
  public void setEnd(Comparable end)
  {
    if (this.end != end)
      {
        this.end = end;
        fireStateChanged();
      }
  }

  /**
   * Sets the current date and, if the new value is different to the old
   * value, sends a {@link ChangeEvent} to all registered listeners.
   *
   * @param value  the new date (<code>null</code> not permitted, must be an
   *               instance of <code>Date</code>).
   *
   * @throws IllegalArgumentException if <code>value</code> is not an instance
   *         of <code>Date</code>.
   */
  public void setValue(Object value)
  {
    if (! (value instanceof Date) || value == null)
      throw new IllegalArgumentException("Value not a date.");

    if (!date.getTime().equals(value))
      {
        date.setTime((Date) value);
        fireStateChanged();
      }
  }
}
