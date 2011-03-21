/* gnu.java.util.ZoneInfo
   Copyright (C) 2007 Free Software Foundation, Inc.

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


package gnu.java.util;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * This class represents more advanced variant of java.util.SimpleTimeZone.
 * It can handle zic(8) compiled transition dates plus uses a SimpleTimeZone
 * for years beyond last precomputed transition.  Before first precomputed
 * transition it assumes no daylight saving was in effect.
 * Timezones that never used daylight saving time should use just
 * SimpleTimeZone instead of this class.
 *
 * This object is tightly bound to the Gregorian calendar.  It assumes
 * a regular seven days week, and the month lengths are that of the
 * Gregorian Calendar.
 *
 * @see Calendar
 * @see GregorianCalendar
 * @see SimpleTimeZone
 * @author Jakub Jelinek
 */
public class ZoneInfo extends TimeZone
{
  private static final int SECS_SHIFT = 22;
  private static final long OFFSET_MASK = (1 << 21) - 1;
  private static final int OFFSET_SHIFT = 64 - 21;
  private static final long IS_DST = 1 << 21;

  /**
   * The raw time zone offset in milliseconds to GMT, ignoring
   * daylight savings.
   * @serial
   */
  private int rawOffset;

  /**
   * Cached DST savings for the last transition rule.
   */
  private int dstSavings;

  /**
   * Cached flag whether last transition rule uses DST saving.
   */
  private boolean useDaylight;

  /**
   * Array of encoded transitions.
   * Transition time in UTC seconds since epoch is in the most
   * significant 64 - SECS_SHIFT bits, then one bit flag
   * whether DST is active and the least significant bits
   * containing offset relative to rawOffset.  Both the DST
   * flag and relative offset apply to time before the transition
   * and after or equal to previous transition if any.
   * The array must be sorted.
   */
  private long[] transitions;

  /**
   * SimpleTimeZone rule which applies on or after the latest
   * transition.  If the DST changes are not expresible as a
   * SimpleTimeZone rule, then the rule should just contain
   * the standard time and no DST time.
   */
  private SimpleTimeZone lastRule;

  /**
   * Cached GMT SimpleTimeZone object for internal use in
   * getOffset method.
   */
  private static SimpleTimeZone gmtZone = null;

  static final long serialVersionUID = -3740626706860383657L;

  /**
   * Create a <code>ZoneInfo</code> with the given time offset
   * from GMT and with daylight savings.
   *
   * @param rawOffset The time offset from GMT in milliseconds.
   * @param id  The identifier of this time zone.
   * @param transitions  Array of transition times in UTC seconds since
   * Epoch in topmost 42 bits, below that 1 boolean bit whether the time
   * before that transition used daylight saving and in bottommost 21
   * bits relative daylight saving offset against rawOffset in seconds
   * that applies before this transition.
   * @param endRule SimpleTimeZone class describing the daylight saving
   * rules after the last transition.
   */
  public ZoneInfo(int rawOffset, String id, long[] transitions,
                  SimpleTimeZone lastRule)
  {
    if (transitions == null || transitions.length < 1)
      throw new IllegalArgumentException("transitions must not be null");
    if (lastRule == null)
      throw new IllegalArgumentException("lastRule must not be null");
    this.rawOffset = rawOffset;
    this.transitions = transitions;
    this.lastRule = lastRule;
    setID(id);
    computeDSTSavings();
  }

  /**
   * Gets the time zone offset, for current date, modified in case of
   * daylight savings.  This is the offset to add to UTC to get the local
   * time.
   *
   * The day must be a positive number and dayOfWeek must be a positive value
   * from Calendar.  dayOfWeek is redundant, but must match the other values
   * or an inaccurate result may be returned.
   *
   * @param era the era of the given date
   * @param year the year of the given date
   * @param month the month of the given date, 0 for January.
   * @param day the day of month
   * @param dayOfWeek the day of week; this must match the other fields.
   * @param millis the millis in the day (in local standard time)
   * @return the time zone offset in milliseconds.
   * @throws IllegalArgumentException if arguments are incorrect.
   */
  public int getOffset(int era, int year, int month, int day, int dayOfWeek,
                       int millis)
  {
    if (gmtZone == null)
      gmtZone = new SimpleTimeZone(0, "GMT");

    if (dayOfWeek < Calendar.SUNDAY || dayOfWeek > Calendar.SATURDAY)
      throw new IllegalArgumentException("dayOfWeek out of range");
    if (month < Calendar.JANUARY || month > Calendar.DECEMBER)
      throw new IllegalArgumentException("month out of range:" + month);

    if (era != GregorianCalendar.AD)
      return (int) (((transitions[0] << OFFSET_SHIFT) >> OFFSET_SHIFT) * 1000);

    GregorianCalendar cal = new GregorianCalendar((TimeZone) gmtZone);
    cal.set(year, month, day, 0, 0, 0);
    if (cal.get(Calendar.DAY_OF_MONTH) != day)
      throw new IllegalArgumentException("day out of range");

    return getOffset(cal.getTimeInMillis() - rawOffset + millis);
  }

  private long findTransition(long secs)
  {
    if (secs < (transitions[0] >> SECS_SHIFT))
      return transitions[0];

    if (secs >= (transitions[transitions.length-1] >> SECS_SHIFT))
      return Long.MAX_VALUE;

    long val = (secs + 1) << SECS_SHIFT;
    int lo = 1;
    int hi = transitions.length;
    int mid = 1;
    while (lo < hi)
      {
        mid = (lo + hi) / 2;
        // secs < (transitions[mid-1] >> SECS_SHIFT)
        if (val <= transitions[mid-1])
          hi = mid;
        // secs >= (transitions[mid] >> SECS_SHIFT)
        else if (val > transitions[mid])
          lo = mid + 1;
        else
          break;
      }
    return transitions[mid];
  }

  /**
   * Get the time zone offset for the specified date, modified in case of
   * daylight savings.  This is the offset to add to UTC to get the local
   * time.
   * @param date the date represented in millisecends
   * since January 1, 1970 00:00:00 GMT.
   */
  public int getOffset(long date)
  {
    long d = (date >= 0 ? date / 1000 : (date + 1) / 1000 - 1);
    long transition = findTransition(d);

    // For times on or after last transition use lastRule.
    if (transition == Long.MAX_VALUE)
      return lastRule.getOffset(date);

    return (int) (((transition << OFFSET_SHIFT) >> OFFSET_SHIFT) * 1000);
  }

  /**
   * Returns the time zone offset to GMT in milliseconds, ignoring
   * day light savings.
   * @return the time zone offset.
   */
  public int getRawOffset()
  {
    return rawOffset;
  }

  /**
   * Sets the standard time zone offset to GMT.
   * @param rawOffset The time offset from GMT in milliseconds.
   */
  public void setRawOffset(int rawOffset)
  {
    this.rawOffset = rawOffset;
    lastRule.setRawOffset(rawOffset);
  }

  private void computeDSTSavings()
  {
    if (lastRule.useDaylightTime())
      {
        dstSavings = lastRule.getDSTSavings();
        useDaylight = true;
      }
    else
      {
        dstSavings = 0;
        useDaylight = false;
        // lastRule might say no DST is in effect simply because
        // the DST rules are too complex for SimpleTimeZone, say
        // for Asia/Jerusalem.
        // Look at the last DST offset if it is newer than current time.
        long currentSecs = System.currentTimeMillis() / 1000;
        int i;
        for (i = transitions.length - 1;
             i >= 0 && currentSecs < (transitions[i] >> SECS_SHIFT);
             i--)
          if ((transitions[i] & IS_DST) != 0)
            {
              dstSavings = (int) (((transitions[i] << OFFSET_SHIFT)
                                   >> OFFSET_SHIFT) * 1000)
                           - rawOffset;
              useDaylight = true;
              break;
            }
      }
  }

  /**
   * Gets the daylight savings offset.  This is a positive offset in
   * milliseconds with respect to standard time.  Typically this
   * is one hour, but for some time zones this may be half an our.
   * @return the daylight savings offset in milliseconds.
   */
  public int getDSTSavings()
  {
    return dstSavings;
  }

  /**
   * Returns if this time zone uses daylight savings time.
   * @return true, if we use daylight savings time, false otherwise.
   */
  public boolean useDaylightTime()
  {
    return useDaylight;
  }

  /**
   * Determines if the given date is in daylight savings time.
   * @return true, if it is in daylight savings time, false otherwise.
   */
  public boolean inDaylightTime(Date date)
  {
    long d = date.getTime();
    d = (d >= 0 ? d / 1000 : (d + 1) / 1000 - 1);
    long transition = findTransition(d);

    // For times on or after last transition use lastRule.
    if (transition == Long.MAX_VALUE)
      return lastRule.inDaylightTime(date);

    return (transition & IS_DST) != 0;
  }

  /**
   * Generates the hashCode for the SimpleDateFormat object.  It is
   * the rawOffset, possibly, if useDaylightSavings is true, xored
   * with startYear, startMonth, startDayOfWeekInMonth, ..., endTime.
   */
  public synchronized int hashCode()
  {
    int hash = lastRule.hashCode();
    // FIXME - hash transitions?
    return hash;
  }

  public synchronized boolean equals(Object o)
  {
    if (! hasSameRules((TimeZone) o))
      return false;

    ZoneInfo zone = (ZoneInfo) o;
    return getID().equals(zone.getID());
  }

  /**
   * Test if the other time zone uses the same rule and only
   * possibly differs in ID.  This implementation for this particular
   * class will return true if the other object is a ZoneInfo,
   * the raw offsets and useDaylight are identical and if useDaylight
   * is true, also the start and end datas are identical.
   * @return true if this zone uses the same rule.
   */
  public boolean hasSameRules(TimeZone o)
  {
    if (this == o)
      return true;
    if (! (o instanceof ZoneInfo))
      return false;
    ZoneInfo zone = (ZoneInfo) o;
    if (zone.hashCode() != hashCode() || rawOffset != zone.rawOffset)
      return false;
    if (! lastRule.equals(zone.lastRule))
      return false;
    // FIXME - compare transitions?
    return true;
  }

  /**
   * Returns a string representation of this ZoneInfo object.
   * @return a string representation of this ZoneInfo object.
   */
  public String toString()
  {
    return getClass().getName() + "[" + "id=" + getID() + ",offset="
           + rawOffset + ",transitions=" + transitions.length
           + ",useDaylight=" + useDaylight
           + (useDaylight ? (",dstSavings=" + dstSavings) : "")
           + ",lastRule=" + lastRule.toString() + "]";
  }

  /**
   * Reads zic(8) compiled timezone data file from file
   * and returns a TimeZone class describing it, either
   * SimpleTimeZone or ZoneInfo depending on whether
   * it can be described by SimpleTimeZone rule or not.
   */
  public static TimeZone readTZFile(String id, String file)
  {
    DataInputStream dis = null;
    try
      {
        FileInputStream fis = new FileInputStream(file);
        BufferedInputStream bis = new BufferedInputStream(fis);
        dis = new DataInputStream(bis);

        // Make sure we are reading a tzfile.
        byte[] tzif = new byte[5];
        dis.readFully(tzif);
        int tzif2 = 4;
        if (tzif[0] == 'T' && tzif[1] == 'Z'
            && tzif[2] == 'i' && tzif[3] == 'f')
          {
            if (tzif[4] >= '2')
              tzif2 = 8;
            // Reserved bytes
            skipFully(dis, 16 - 1);
          }
        else
          // Darwin has tzdata files that don't start with the TZif marker
          skipFully(dis, 16 - 5);

        int ttisgmtcnt = dis.readInt();
        int ttisstdcnt = dis.readInt();
        int leapcnt = dis.readInt();
        int timecnt = dis.readInt();
        int typecnt = dis.readInt();
        int charcnt = dis.readInt();
        if (tzif2 == 8)
          {
            skipFully(dis, timecnt * (4 + 1) + typecnt * (4 + 1 + 1) + charcnt
                           + leapcnt * (4 + 4) + ttisgmtcnt + ttisstdcnt);

            dis.readFully(tzif);
            if (tzif[0] != 'T' || tzif[1] != 'Z' || tzif[2] != 'i'
                || tzif[3] != 'f' || tzif[4] < '2')
              return null;

            // Reserved bytes
            skipFully(dis, 16 - 1);
            ttisgmtcnt = dis.readInt();
            ttisstdcnt = dis.readInt();
            leapcnt = dis.readInt();
            timecnt = dis.readInt();
            typecnt = dis.readInt();
            charcnt = dis.readInt();
          }

        // Sanity checks
        if (typecnt <= 0 || timecnt < 0 || charcnt < 0
            || leapcnt < 0 || ttisgmtcnt < 0 || ttisstdcnt < 0
            || ttisgmtcnt > typecnt || ttisstdcnt > typecnt)
          return null;

        // Transition times
        long[] times = new long[timecnt];
        for (int i = 0; i < timecnt; i++)
          if (tzif2 == 8)
            times[i] = dis.readLong();
          else
            times[i] = (long) dis.readInt();

        // Transition types
        int[] types = new int[timecnt];
        for (int i = 0; i < timecnt; i++)
          {
            types[i] = dis.readByte();
            if (types[i] < 0)
              types[i] += 256;
            if (types[i] >= typecnt)
              return null;
          }

        // Types
        int[] offsets = new int[typecnt];
        int[] typeflags = new int[typecnt];
        for (int i = 0; i < typecnt; i++)
          {
            offsets[i] = dis.readInt();
            if (offsets[i] >= IS_DST / 2 || offsets[i] <= -IS_DST / 2)
              return null;
            int dst = dis.readByte();
            int abbrind = dis.readByte();
            if (abbrind < 0)
              abbrind += 256;
            if (abbrind >= charcnt)
              return null;
            typeflags[i] = (dst != 0 ? (1 << 8) : 0) + abbrind;
          }

        // Abbrev names
        byte[] names = new byte[charcnt];
        dis.readFully(names);

        // Leap transitions, for now ignore
        skipFully(dis, leapcnt * (tzif2 + 4) + ttisstdcnt + ttisgmtcnt);

        // tzIf2 format has optional POSIX TZ env string
        String tzstr = null;
        if (tzif2 == 8 && dis.readByte() == '\n')
          {
            tzstr = dis.readLine();
            if (tzstr.length() <= 0)
              tzstr = null;
          }

        // Get std/dst_offset and dst/non-dst time zone names.
        int std_ind = -1;
        int dst_ind = -1;
        if (timecnt == 0)
          std_ind = 0;
        else
          for (int i = timecnt - 1; i >= 0; i--)
            {
              if (std_ind == -1 && (typeflags[types[i]] & (1 << 8)) == 0)
                std_ind = types[i];
              else if (dst_ind == -1 && (typeflags[types[i]] & (1 << 8)) != 0)
                dst_ind = types[i];
              if (dst_ind != -1 && std_ind != -1)
                break;
            }

        if (std_ind == -1)
          return null;

        int j = typeflags[std_ind] & 255;
        while (j < charcnt && names[j] != 0)
          j++;
        String std_zonename = new String(names, typeflags[std_ind] & 255,
                                         j - (typeflags[std_ind] & 255),
                                         "ASCII");

        String dst_zonename = "";
        if (dst_ind != -1)
          {
            j = typeflags[dst_ind] & 255;
            while (j < charcnt && names[j] != 0)
              j++;
            dst_zonename = new String(names, typeflags[dst_ind] & 255,
                                      j - (typeflags[dst_ind] & 255), "ASCII");
          }

        // Only use gmt offset when necessary.
        // Also special case GMT+/- timezones.
        String std_offset_string = "";
        String dst_offset_string = "";
        if (tzstr == null
            && (dst_ind != -1
                || (offsets[std_ind] != 0
                    && !std_zonename.startsWith("GMT+")
                    && !std_zonename.startsWith("GMT-"))))
          {
            std_offset_string = Integer.toString(-offsets[std_ind] / 3600);
            int seconds = -offsets[std_ind] % 3600;
            if (seconds != 0)
              {
                if (seconds < 0)
                  seconds *= -1;
                if (seconds < 600)
                  std_offset_string += ":0" + Integer.toString(seconds / 60);
                else
                  std_offset_string += ":" + Integer.toString(seconds / 60);
                seconds = seconds % 60;
                if (seconds >= 10)
                  std_offset_string += ":" + Integer.toString(seconds);
                else if (seconds > 0)
                  std_offset_string += ":0" + Integer.toString(seconds);
              }

            if (dst_ind != -1 && offsets[dst_ind] != offsets[std_ind] + 3600)
              {
                dst_offset_string = Integer.toString(-offsets[dst_ind] / 3600);
                seconds = -offsets[dst_ind] % 3600;
                if (seconds != 0)
                  {
                    if (seconds < 0)
                      seconds *= -1;
                    if (seconds < 600)
                      dst_offset_string
                        += ":0" + Integer.toString(seconds / 60);
                    else
                      dst_offset_string
                        += ":" + Integer.toString(seconds / 60);
                    seconds = seconds % 60;
                    if (seconds >= 10)
                      dst_offset_string += ":" + Integer.toString(seconds);
                    else if (seconds > 0)
                      dst_offset_string += ":0" + Integer.toString(seconds);
                  }
              }
          }

        /*
         * If no tzIf2 POSIX TZ string is available and the timezone
         * uses DST, try to guess the last rule by trying to make
         * sense from transitions at 5 years in the future and onwards.
         * tzdata actually uses only 3 forms of rules:
         * fixed date within a month, e.g. change on April, 5th
         * 1st weekday on or after Nth: change on Sun>=15 in April
         * last weekday in a month: change on lastSun in April
         */
        String[] change_spec = { null, null };
        if (tzstr == null && dst_ind != -1 && timecnt > 10)
          {
            long nowPlus5y = System.currentTimeMillis() / 1000
                             + 5 * 365 * 86400;
            int first;

            for (first = timecnt - 1; first >= 0; first--)
              if (times[first] < nowPlus5y
                  || (types[first] != std_ind && types[first] != dst_ind)
                  || types[first] != types[timecnt - 2 + ((first ^ timecnt) & 1)])
                break;
            first++;

            if (timecnt - first >= 10 && types[timecnt - 1] != types[timecnt - 2])
              {
                GregorianCalendar cal
                  = new GregorianCalendar(new SimpleTimeZone(0, "GMT"));

                int[] values = new int[2 * 11];
                int i;
                for (i = timecnt - 1; i >= first; i--)
                  {
                    int base = (i % 2) * 11;
                    int offset = offsets[types[i > first ? i - 1 : i + 1]];
                    cal.setTimeInMillis((times[i] + offset) * 1000);
                    if (i >= timecnt - 2)
                      {
                        values[base + 0] = cal.get(Calendar.YEAR);
                        values[base + 1] = cal.get(Calendar.MONTH);
                        values[base + 2] = cal.get(Calendar.DAY_OF_MONTH);
                        values[base + 3]
                          = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
                        values[base + 4] = cal.get(Calendar.DAY_OF_WEEK);
                        values[base + 5] = cal.get(Calendar.HOUR_OF_DAY);
                        values[base + 6] = cal.get(Calendar.MINUTE);
                        values[base + 7] = cal.get(Calendar.SECOND);
                        values[base + 8] = values[base + 2]; // Range start
                        values[base + 9] = values[base + 2]; // Range end
                        values[base + 10] = 0; // Determined type
                      }
                    else
                      {
                        int year = cal.get(Calendar.YEAR);
                        int month = cal.get(Calendar.MONTH);
                        int day_of_month = cal.get(Calendar.DAY_OF_MONTH);
                        int month_days
                          = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
                        int day_of_week = cal.get(Calendar.DAY_OF_WEEK);
                        int hour = cal.get(Calendar.HOUR_OF_DAY);
                        int minute = cal.get(Calendar.MINUTE);
                        int second = cal.get(Calendar.SECOND);
                        if (year != values[base + 0] - 1
                            || month != values[base + 1]
                            || hour != values[base + 5]
                            || minute != values[base + 6]
                            || second != values[base + 7])
                          break;
                        if (day_of_week == values[base + 4])
                          {
                            // Either a Sun>=8 or lastSun rule.
                            if (day_of_month < values[base + 8])
                              values[base + 8] = day_of_month;
                            if (day_of_month > values[base + 9])
                              values[base + 9] = day_of_month;
                            if (values[base + 10] < 0)
                              break;
                            if (values[base + 10] == 0)
                              {
                                values[base + 10] = 1;
                                // If day of month > 28, this is
                                // certainly lastSun rule.
                                if (values[base + 2] > 28)
                                  values[base + 2] = 3;
                                // If day of month isn't in the last
                                // week, it can't be lastSun rule.
                                else if (values[base + 2]
                                         <= values[base + 3] - 7)
                                  values[base + 3] = 2;
                              }
                            if (values[base + 10] == 1)
                              {
                                // If day of month is > 28, this is
                                // certainly lastSun rule.
                                if (day_of_month > 28)
                                  values[base + 10] = 3;
                                // If day of month isn't in the last
                                // week, it can't be lastSun rule.
                                else if (day_of_month <= month_days - 7)
                                  values[base + 10] = 2;
                              }
                            else if ((values[base + 10] == 2
                                      && day_of_month > 28)
                                     || (values[base + 10] == 3
                                         && day_of_month <= month_days - 7))
                              break;
                          }
                        else
                          {
                            // Must be fixed day in month rule.
                            if (day_of_month != values[base + 2]
                                || values[base + 10] > 0)
                              break;
                            values[base + 4] = day_of_week;
                            values[base + 10] = -1;
                          }
                        values[base + 0] -= 1;
                      }
                  }

                if (i < first)
                  {
                    for (i = 0; i < 2; i++)
                      {
                        int base = 11 * i;
                        if (values[base + 10] == 0)
                          continue;
                        if (values[base + 10] == -1)
                          {
                            int[] dayCount
                              = { 0, 31, 59, 90, 120, 151,
                                  181, 212, 243, 273, 304, 334 };
                            int d = dayCount[values[base + 1]
                                             - Calendar.JANUARY];
                            d += values[base + 2];
                            change_spec[i] = ",J" + Integer.toString(d);
                          }
                        else if (values[base + 10] == 2)
                          {
                            // If we haven't seen all days of the week,
                            // we can't be sure what the rule really is.
                            if (values[base + 8] + 6 != values[base + 9])
                              continue;

                            int d;
                            d = values[base + 1] - Calendar.JANUARY + 1;
                            // E.g. Sun >= 5 is not representable in POSIX
                            // TZ env string, use ",Am.n.d" extension
                            // where m is month 1 .. 12, n is the date on
                            // or after which it happens and d is day
                            // of the week, 0 .. 6.  So Sun >= 5 in April
                            // is ",A4.5.0".
                            if ((values[base + 8] % 7) == 1)
                              {
                                change_spec[i] = ",M" + Integer.toString(d);
                                d = (values[base + 8] + 6) / 7;
                              }
                            else
                              {
                                change_spec[i] = ",A" + Integer.toString(d);
                                d = values[base + 8];
                              }
                            change_spec[i] += "." + Integer.toString(d);
                            d = values[base + 4] - Calendar.SUNDAY;
                            change_spec[i] += "." + Integer.toString(d);
                          }
                        else
                          {
                            // If we don't know whether this is lastSun or
                            // Sun >= 22 rule.  That can be either because
                            // there was insufficient number of
                            // transitions, or February, where it is quite
                            // probable we haven't seen any 29th dates.
                            // For February, assume lastSun rule, otherwise
                            // punt.
                            if (values[base + 10] == 1
                                && values[base + 1] != Calendar.FEBRUARY)
                              continue;

                            int d;
                            d = values[base + 1] - Calendar.JANUARY + 1;
                            change_spec[i] = ",M" + Integer.toString(d);
                            d = values[base + 4] - Calendar.SUNDAY;
                            change_spec[i] += ".5." + Integer.toString(d);
                          }

                        // Don't add time specification if time is
                        // 02:00:00.
                        if (values[base + 5] != 2
                            || values[base + 6] != 0
                            || values[base + 7] != 0)
                          {
                            int d = values[base + 5];
                            change_spec[i] += "/" + Integer.toString(d);
                            if (values[base + 6] != 0 || values[base + 7] != 0)
                              {
                                d = values[base + 6];
                                if (d < 10)
                                  change_spec[i]
                                    += ":0" + Integer.toString(d);
                                else
                                  change_spec[i] += ":" + Integer.toString(d);
                                d = values[base + 7];
                                if (d >= 10)
                                   change_spec[i]
                                     += ":" + Integer.toString(d);
                                else if (d > 0)
                                  change_spec[i]
                                    += ":0" + Integer.toString(d);
                              }
                          }
                      }
                    if (types[(timecnt - 1) & -2] == std_ind)
                      {
                        String tmp = change_spec[0];
                        change_spec[0] = change_spec[1];
                        change_spec[1] = tmp;
                      }
                  }
              }
          }

        if (tzstr == null)
          {
            tzstr = std_zonename + std_offset_string;
            if (change_spec[0] != null && change_spec[1] != null)
              tzstr += dst_zonename + dst_offset_string
                       + change_spec[0] + change_spec[1];
          }

        if (timecnt == 0)
          return new SimpleTimeZone(offsets[std_ind] * 1000,
                                    id != null ? id : tzstr);

        SimpleTimeZone endRule = createLastRule(tzstr);
        if (endRule == null)
          return null;

        /* Finally adjust the times array into the form the constructor
         * expects.  times[0] is special, the offset and DST flag there
         * are for all times before that transition.  Use the first non-DST
         * type.  For all other transitions, the data file has the type
         * (<offset, isdst, zonename>) for the time interval starting
         */
        for (int i = 0; i < typecnt; i++)
          if ((typeflags[i] & (1 << 8)) == 0)
            {
              times[0] = (times[0] << SECS_SHIFT) | (offsets[i] & OFFSET_MASK);
              break;
            }

        for (int i = 1; i < timecnt; i++)
          times[i] = (times[i] << SECS_SHIFT)
                     | (offsets[types[i - 1]] & OFFSET_MASK)
                     | ((typeflags[types[i - 1]] & (1 << 8)) != 0 ? IS_DST : 0);

        return new ZoneInfo(offsets[std_ind] * 1000, id != null ? id : tzstr,
                            times, endRule);
      }
    catch (IOException ioe)
      {
        // Parse error, not a proper tzfile.
        return null;
      }
    finally
      {
        try
          {
            if (dis != null)
              dis.close();
          }
        catch(IOException ioe)
          {
            // Error while close, nothing we can do.
          }
      }
  }

  /**
   * Skips the requested number of bytes in the given InputStream.
   * Throws EOFException if not enough bytes could be skipped.
   * Negative numbers of bytes to skip are ignored.
   */
  private static void skipFully(InputStream is, long l) throws IOException
  {
    while (l > 0)
      {
        long k = is.skip(l);
        if (k <= 0)
          throw new EOFException();
        l -= k;
      }
  }

  /**
   * Create a SimpleTimeZone from a POSIX TZ environment string,
   * see http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html
   * for details.
   * It supports also an extension, where Am.n.d rule (m 1 .. 12, n 1 .. 25, d
   * 0 .. 6) describes day of week d on or after nth day of month m.
   * Say A4.5.0 is Sun>=5 in April.
   */
  private static SimpleTimeZone createLastRule(String tzstr)
  {
    String stdName = null;
    int stdOffs;
    int dstOffs;
    try
      {
        int idLength = tzstr.length();

        int index = 0;
        int prevIndex;
        char c;

        // get std
        do
          c = tzstr.charAt(index);
        while (c != '+' && c != '-' && c != ',' && c != ':'
               && ! Character.isDigit(c) && c != '\0' && ++index < idLength);

        if (index >= idLength)
          return new SimpleTimeZone(0, tzstr);

        stdName = tzstr.substring(0, index);
        prevIndex = index;

        // get the std offset
        do
          c = tzstr.charAt(index++);
        while ((c == '-' || c == '+' || c == ':' || Character.isDigit(c))
               && index < idLength);
        if (index < idLength)
          index--;

        { // convert the dst string to a millis number
            String offset = tzstr.substring(prevIndex, index);
            prevIndex = index;

            if (offset.charAt(0) == '+' || offset.charAt(0) == '-')
              stdOffs = parseTime(offset.substring(1));
            else
              stdOffs = parseTime(offset);

            if (offset.charAt(0) == '-')
              stdOffs = -stdOffs;

            // TZ timezone offsets are positive when WEST of the meridian.
            stdOffs = -stdOffs;
        }

        // Done yet? (Format: std offset)
        if (index >= idLength)
          return new SimpleTimeZone(stdOffs, stdName);

        // get dst
        do
          c = tzstr.charAt(index);
        while (c != '+' && c != '-' && c != ',' && c != ':'
               && ! Character.isDigit(c) && c != '\0' && ++index < idLength);

        // Done yet? (Format: std offset dst)
        if (index >= idLength)
          return new SimpleTimeZone(stdOffs, stdName);

        // get the dst offset
        prevIndex = index;
        do
          c = tzstr.charAt(index++);
        while ((c == '-' || c == '+' || c == ':' || Character.isDigit(c))
               && index < idLength);
        if (index < idLength)
          index--;

        if (index == prevIndex && (c == ',' || c == ';'))
          {
            // Missing dst offset defaults to one hour ahead of standard
            // time.
            dstOffs = stdOffs + 60 * 60 * 1000;
          }
        else
          { // convert the dst string to a millis number
            String offset = tzstr.substring(prevIndex, index);
            prevIndex = index;

            if (offset.charAt(0) == '+' || offset.charAt(0) == '-')
              dstOffs = parseTime(offset.substring(1));
            else
              dstOffs = parseTime(offset);

            if (offset.charAt(0) == '-')
              dstOffs = -dstOffs;

            // TZ timezone offsets are positive when WEST of the meridian.
            dstOffs = -dstOffs;
          }

        // Done yet? (Format: std offset dst offset)
        if (index >= idLength)
          return new SimpleTimeZone(stdOffs, stdName);

        // get the DST rule
        if (tzstr.charAt(index) == ','
            || tzstr.charAt(index) == ';')
          {
            index++;
            int offs = index;
            while (tzstr.charAt(index) != ','
                   && tzstr.charAt(index) != ';')
              index++;
            String startTime = tzstr.substring(offs, index);
            index++;
            String endTime = tzstr.substring(index);

            index = startTime.indexOf('/');
            int startMillis;
            int endMillis;
            String startDate;
            String endDate;
            if (index != -1)
              {
                startDate = startTime.substring(0, index);
                startMillis = parseTime(startTime.substring(index + 1));
              }
            else
              {
                startDate = startTime;
                // if time isn't given, default to 2:00:00 AM.
                startMillis = 2 * 60 * 60 * 1000;
              }
            index = endTime.indexOf('/');
            if (index != -1)
              {
                endDate = endTime.substring(0, index);
                endMillis = parseTime(endTime.substring(index + 1));
              }
            else
              {
                endDate = endTime;
                // if time isn't given, default to 2:00:00 AM.
                endMillis = 2 * 60 * 60 * 1000;
              }

            int[] start = getDateParams(startDate);
            int[] end = getDateParams(endDate);
            return new SimpleTimeZone(stdOffs, tzstr, start[0], start[1],
                                      start[2], startMillis, end[0], end[1],
                                      end[2], endMillis, (dstOffs - stdOffs));
          }
      }

    catch (IndexOutOfBoundsException _)
      {
      }
    catch (NumberFormatException _)
      {
      }

    return null;
  }

  /**
   * Parses and returns the params for a POSIX TZ date field,
   * in the format int[]{ month, day, dayOfWeek }, following the
   * SimpleTimeZone constructor rules.
   */
  private static int[] getDateParams(String date)
  {
    int[] dayCount = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
    int month;
    int type = 0;

    if (date.charAt(0) == 'M' || date.charAt(0) == 'm')
      type = 1;
    else if (date.charAt(0) == 'A' || date.charAt(0) == 'a')
      type = 2;

    if (type > 0)
      {
        int day;

        // Month, week of month, day of week
        // "Mm.w.d".  d is between 0 (Sunday) and 6.  Week w is
        // between 1 and 5; Week 1 is the first week in which day d
        // occurs and Week 5 specifies the last d day in the month.
        // Month m is between 1 and 12.

        // Month, day of month, day of week
        // ZoneInfo extension, not in POSIX
        // "Am.n.d".  d is between 0 (Sunday) and 6.  Day of month n is
        // between 1 and 25.  Month m is between 1 and 12.

        month = Integer.parseInt(date.substring(1, date.indexOf('.')));
        int week = Integer.parseInt(date.substring(date.indexOf('.') + 1,
                                                   date.lastIndexOf('.')));
        int dayOfWeek = Integer.parseInt(date.substring(date.lastIndexOf('.')
                                                        + 1));
        dayOfWeek++; // Java day of week is one-based, Sunday is first day.

        if (type == 2)
          {
            day = week;
            dayOfWeek = -dayOfWeek;
          }
        else if (week == 5)
          day = -1; // last day of month is -1 in java, 5 in TZ
        else
          {
            // First day of week starting on or after.  For example,
            // to specify the second Sunday of April, set month to
            // APRIL, day-of-month to 8, and day-of-week to -SUNDAY.
            day = (week - 1) * 7 + 1;
            dayOfWeek = -dayOfWeek;
          }

        month--; // Java month is zero-based.
        return new int[] { month, day, dayOfWeek };
      }

    // julian day, either zero-based 0<=n<=365 (incl feb 29)
    // or one-based 1<=n<=365 (no feb 29)
    int julianDay; // Julian day

    if (date.charAt(0) != 'J' || date.charAt(0) != 'j')
      {
        julianDay = Integer.parseInt(date.substring(1));
        julianDay++; // make 1-based
        // Adjust day count to include feb 29.
        dayCount = new int[]
                   {
                     0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
                   };
      }
    else
      // 1-based julian day
      julianDay = Integer.parseInt(date);

    int i = 11;
    while (i > 0)
      if (dayCount[i] < julianDay)
        break;
      else
        i--;
    julianDay -= dayCount[i];
    month = i;
    return new int[] { month, julianDay, 0 };
  }

  /**
   * Parses a time field hh[:mm[:ss]], returning the result
   * in milliseconds. No leading sign.
   */
  private static int parseTime(String time)
  {
    int millis = 0;
    int i = 0;

    while (i < time.length())
      if (time.charAt(i) == ':')
        break;
      else
        i++;
    millis = 60 * 60 * 1000 * Integer.parseInt(time.substring(0, i));
    if (i >= time.length())
      return millis;

    int iprev = ++i;
    while (i < time.length())
      if (time.charAt(i) == ':')
        break;
      else
        i++;
    millis += 60 * 1000 * Integer.parseInt(time.substring(iprev, i));
    if (i >= time.length())
      return millis;

    millis += 1000 * Integer.parseInt(time.substring(++i));
    return millis;
  }
}
