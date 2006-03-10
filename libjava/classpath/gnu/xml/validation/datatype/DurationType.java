/* DurationType.java -- 
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

package gnu.xml.validation.datatype;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.ValidationContext;

/**
 * The XML Schema duration type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
final class DurationType
  extends AtomicSimpleType
{

  static class Duration
    implements Comparable
  {
    int years;
    int months;
    int days;
    int minutes;
    float seconds;

    public int hashCode()
    {
      int hc = years;
      hc = hc * 31 + months;
      hc = hc * 31 + days;
      hc = hc * 31 + minutes;
      hc = hc * 31 + new Float(seconds).hashCode();
      return hc;
    }

    public boolean equals(Object other)
    {
      if (other instanceof Duration)
        {
          Duration duration = (Duration) other;
          return duration.years ==years &&
            duration.months == months &&
            duration.days == days &&
            duration.minutes == minutes &&
            duration.seconds == seconds;
        }
      return false;
    }

    public int compareTo(Object other)
    {
      if (other instanceof Duration)
        {
          Duration duration = (Duration) other;
          if (duration.years != years)
            return years - duration.years;
          if (duration.months != months)
            return months - duration.months;
          if (duration.days != days)
            return days - duration.days;
          if (duration.minutes != minutes)
            return minutes = duration.minutes;
          if (duration.seconds == seconds)
            return 0;
          return (seconds < duration.seconds) ? -1 : 1;
        }
      return 0;
    }
    
  }

  static final int[] CONSTRAINING_FACETS = {
    Facet.PATTERN,
    Facet.ENUMERATION,
    Facet.WHITESPACE,
    Facet.MAX_INCLUSIVE,
    Facet.MAX_EXCLUSIVE,
    Facet.MIN_INCLUSIVE,
    Facet.MIN_EXCLUSIVE
  };

  DurationType()
  {
    super(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "duration"),
          TypeLibrary.ANY_SIMPLE_TYPE);
  }

  public int[] getConstrainingFacets()
  {
    return CONSTRAINING_FACETS;
  }

  public void checkValid(String value, ValidationContext context)
    throws DatatypeException
  {
    super.checkValid(value, context);
    int len = value.length();
    char expect = 'P';
    boolean seenT = false;
    for (int i = 0; i < len; i++)
      {
        char c = value.charAt(i);
        if (c == '-' && expect == 'P')
          continue;
        if (c == expect)
          {
            if (c == 'P')
              expect = 'Y';
            else if (c == 'Y')
              expect = 'M';
            else if (c == 'M' && !seenT)
              expect = 'D';
            else if (c == 'D')
              expect = 'T';
            else if (c == 'T')
              {
                expect = 'H';
                seenT = true;
              }
            else if (c == 'H')
              expect = 'M';
            else if (c == 'M' && seenT)
              expect = 'S';
            else if (c == 'S')
              {
                if (i + 1 != len)
                  throw new DatatypeException(i, "illegal duration value");
              }
            continue;
          }
        if (c >= 0x30 && c <= 0x39 && expect != 'P' && expect != 'T')
          continue;
        throw new DatatypeException(i, "illegal duration value");
      }
  }
  
  public Object createValue(String value, ValidationContext context) {
    boolean negative = false;
    int days = 0, months = 0, years = 0;
    int minutes = 0;
    float seconds = 0.0f;
    int len = value.length();
    char expect = 'P';
    boolean seenT = false;
    int start = 0;
    for (int i = 0; i < len; i++)
      {
        char c = value.charAt(i);
        if (c == '-' && expect == 'P')
          {
            negative = true;
            continue;
          }
        if (c == expect)
          {
            if (c == 'P')
              expect = 'Y';
            else if (c == 'Y')
              {
                expect = 'M';
                years = Integer.parseInt(value.substring(start, i));
              }
            else if (c == 'M' && !seenT)
              expect = 'D';
            else if (c == 'D')
              expect = 'T';
            else if (c == 'T')
              {
                expect = 'H';
                seenT = true;
              }
            else if (c == 'H')
              expect = 'M';
            else if (c == 'M' && seenT)
              expect = 'S';
            else if (c == 'S')
              {
                if (i + 1 != len)
                  return null;
              }
            start = i + 1;
            continue;
          }
        if (c >= 0x30 && c <= 0x39 && expect != 'P' && expect != 'T')
          continue;
        return null;
      }
    if (negative)
      {
        days = days * -1;
        minutes = minutes * -1;
        seconds = seconds * -1.0f;
      }
    Duration duration = new Duration();
    duration.days = days;
    duration.minutes = minutes;
    duration.seconds = seconds;
    return duration;
  }

}

