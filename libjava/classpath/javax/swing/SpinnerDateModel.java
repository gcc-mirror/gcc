/* SpinnerDateModel.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

/**
 * SpinnerDateModel
 *
 * Implements a SpinnerModel for dates, rotating a calendar field such as 
 * month, year, day, week, hour, minute.
 *
 * @author Sven de Marothy
 * @version 0.1 (first implementation)
 */
public class SpinnerDateModel extends AbstractSpinnerModel
  implements Serializable
{
  private Calendar date;
  private Comparable start;
  private Comparable end;
  private int calendarField;

  /**
   * For compatability with Sun's JDK
   * FIXME: Which fields should be serialized?
   */
  private static final long serialVersionUID = -4802518107105940612L;

  /**
   * Constructs a SpinnerDateModel using the current date,
   * no start or end limit, and Calendar.DAY_OF_MONTH as the calendar field.
   */
  public SpinnerDateModel()
  {
    this(new Date(), null, null, Calendar.DAY_OF_MONTH);
  }

  /**
   * Constructs a SpinnerDateModel which spins a given calendar field,
   * using a given date and start and end date limits.
   * @param value - the initial Date value
   * @param start - start limit, as a Date object, or <code>null</code>
   * for no lower limit.
   * @param end - end limit, or <code>null</code> for no upper limit.
   * @param calendarField - the <code>Calendar</code> field to spin,
   * (Calendar.ZONE_OFFSET and Calendar.DST_OFFSET are invalid)
   */
  public SpinnerDateModel(Date value, Comparable start, Comparable end,
                          int calendarField)
  {
    date = Calendar.getInstance();
    date.setTime(value);
    this.start = start;
    this.end = end;
    setCalendarField(calendarField);
  }

  /**
   * Returns the value of the Calendar field to spin.
   */
  public int getCalendarField()
  {
    return calendarField;
  }

  /**
   * Returns the current date in the sequence.
   * @return a <code>Date</code> object.
   */
  public Date getDate()
  {
    return date.getTime();
  }

  /**
   * Returns the starting limit of the SpinnerModel.
   * @return a Date object, or <code>null</code> if there is no limit.
   */
  public Comparable getStart()
  {
    return start;
  }

  /**
   * Returns the end limit of the SpinnerModel.
   * @return a Date object, or <code>null</code> if there is no limit.
   */
  public Comparable getEnd()
  {
    return end;
  }

  /**
   * Returns the current date in the sequence,
   * this method returns the same as <code>getDate()</code>.
   * @return a <code>Date</code> object.
   */
  public Object getValue()
  {
    return date.getTime();
  }

  /**
   * Returns the next date in the sequence, or <code>null</code> if the
   * next date is equal to or past the end limit.
   * @return a Date object, or <code>null</code>.
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
   * next date is equal to or past the end limit.
   * @return a Date object, or <code>null</code>.
   */
  public Object getPreviousValue()
  {
    Calendar prevCal = Calendar.getInstance();
    prevCal.setTime(date.getTime());
    prevCal.roll(calendarField, false);
    Date prevDate = prevCal.getTime();
    if (end != null)
      if (end.compareTo(prevDate) > 0)
	return null;
    return prevDate;
  }

  /**
   * Sets the date field to change. It must be a valid Calendar field,
   * excluding Calendar.ZONE_OFFSET and Calendar.DST_OFFSET.
   * @param calendarField - the calendar field to set.
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
   * Sets the starting date limit for the sequence.
   *
   * @param start - a Date object of the limit date,
   * or <code>null</code> for no limit.
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
   * Sets the end date limit for the sequence.
   *
   * @param end - a Date object of the limit date,
   * or <code>null</code> for no limit.
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
   * Sets the current date in the sequence.
   *
   * @param value - a Date object.
   */
  public void setValue(Object value)
  {
    if (! (value instanceof Date) || value == null)
      throw new IllegalArgumentException("Value not a date.");
    date.setTime((Date) value);
    fireStateChanged();
  }
}
