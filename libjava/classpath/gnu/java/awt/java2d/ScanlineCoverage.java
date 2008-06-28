/* ScanlineCoverage.java -- Manages coverage information for a scanline
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

package gnu.java.awt.java2d;

/**
 * Stores and handles the pixel converage for a scanline. The pixel coverage
 * is stored as sorted list of {@linke Covergage} entries, each of which holds
 * information about the coverage for the X and Y axis. This is utilized to
 * compute the actual coverage for each pixel on the scanline and finding
 * chunks of pixels with equal coverage quickly.
 */
public final class ScanlineCoverage
{

  /**
   * Iterates over the coverage list and calculates the actual coverage
   * ranges on a scanline.
   */
  public final class Iterator
  {
    /**
     * This instance is reused in the iteration.
     */
    private Range range;

    /**
     * The pointer to the current item in the iteration.
     */
    private Coverage currentItem;

    /**
     * The current coverage value.
     */
    private int currentCoverage;

    /**
     * True when the current pixel coverage has already been handled, false
     * otherwise.
     */
    private boolean handledPixelCoverage;

    /**
     * Creates a new CoverageIterator.
     */
    Iterator()
    {
      range = new Range();
    }

    /**
     * Returns the next coverage range on the scanline. The returned object
     * will always be the same object, but with different values. Keep that
     * in mind when dealing with this object.
     *
     * @return the next coverage range on the scanline
     */
    public Range next()
    {
      // TODO: Lump together the single-pixel coverage and the
      // between-pixel coverage when the pixel coverage delta is 0.
      if (handledPixelCoverage == false)
        {
          // Handle single pixel coverage.
          range.setXPos(currentItem.xPos);
          range.setLength(1);
          range.setCoverage(currentCoverage + currentItem.pixelCoverage);
          handledPixelCoverage = true;
        }
      else
        {
          // Handle pixel span coverage.
          currentCoverage += currentItem.covDelta;
          range.setCoverage(currentCoverage);
          range.setXPos(currentItem.xPos + 1);
          currentItem = currentItem.next;
          range.setLength(currentItem.xPos - range.xPos);
          handledPixelCoverage = false;
        }
      return range;
    }

    /**
     * Returns {@ true} when there are more coverage ranges to iterate,
     * {@ false} otherwise.
     *
     * @return {@ true} when there are more coverage ranges to iterate,
     *         {@ false} otherwise
     */
    public boolean hasNext()
    {
      boolean hasNext;
      if (currentItem != null && handledPixelCoverage == false)
        {
          // We have at least one more coverage item when there's a pixel
          // coverage piece left.
          hasNext = true;
        }
      else if (currentItem == null || currentItem.next == null
          || currentItem.next == last)
        {
          hasNext = false;
        }
      else
        {
          hasNext = true;
        }
      return hasNext;
    }

    /**
     * Resets this iterator to the start of the list.
     */
    void reset()
    {
      currentItem = head;
      currentCoverage = 0;
      handledPixelCoverage = false;
    }
  }

  /**
   * A data object that carries information about pixel coverage on a scanline.
   * The data consists of a starting X position on the scanline, the
   * length of the range in pixels and the actual coverage value.
   **/
  public static final class Range
  {
    /**
     * The X position on the scanline, in pixels.
     */
    private int xPos;

    /**
     * The length of the range, in pixels.
     */
    private int length;

    /**
     * The actual coverage. The relation depends on
     * {@link ScanlineCoverage#maxCoverage}.
     */
    private int coverage;

    /**
     * Creates a new CoverageRange object.
     */
    Range()
    {
      // Nothing to do. The values get initialized in the corresponding
      // setters.
    }

    /**
     * Sets the X start position (left) on the scanline. This value is
     * considered to be in pixels and device space.
     *
     * @param x the x position
     */
    void setXPos(int x)
    {
      xPos = x;
    }

    /**
     * Returns the X start position (left) on the scanline. This value
     * is considered to be in pixels and device space.
     * 
     * @return the X position on the scanline
     */
    public int getXPos()
    {
      return xPos;
    }

    /**
     * Sets the length of the pixel range. This is in pixel units.
     *
     * @param l the length of the range
     */
    void setLength(int l)
    {
      length = l;
    }

    /**
     * Returns the length of the range in pixel units.
     *
     * @return the length of the range in pixel units
     */
    public int getLength()
    {
      return length;
    }

    /**
     * Returns the first X position after the range.
     *
     * @return the first X position after the range
     */
    public int getXPosEnd()
    {
      return xPos + length;
    }

    /**
     * Sets the coverage of the pixel range. The relation of that value
     * depends on {@link ScanlineCoverage#maxCoverage}.
     *
     * @param cov the coverage value for the pixel range
     */
    void setCoverage(int cov)
    {
      coverage = cov;
    }

    /**
     * Returns the coverage of the pixel range. The relation of this value
     * depends on {@link ScanlineCoverage#getMaxCoverage()}.
     *
     * @return the coverage of the pixel range
     */
    public int getCoverage()
    {
      return coverage;
    }

    /**
     * Returns a string representation.
     */
    public String toString()
    {
      return "Coverage range: xPos=" + xPos + ", length=" + length
             + ", coverage: " + coverage;
    }
  }

  /**
   * One bucket in the list.
   */
  private static final class Coverage
  {
    /**
     * The X coordinate on the scanline to which this bucket belongs.
     */
    int xPos;

    /**
     * The coverage delta from the pixel at xPos to xPos + 1.
     */
    int covDelta;

    /**
     * The delta for the pixel at xPos. This is added to the pixel at xPos,
     * but not to the following pixel.
     */
    int pixelCoverage;

    /**
     * Implements a linked list. This points to the next element of the list.
     */
    Coverage next;

    /**
     * Returns the X coordinate for this entry.
     *
     * @return the X coordinate for this entry
     */
    public int getXPos()
    {
      return xPos;
    }

    /**
     * Returns the coverage delta for this entry.
     *
     * @return the coverage delta for this entry
     */
    public int getCoverageDelta()
    {
      return covDelta;
    }

    /**
     * Returns a string representation.
     *
     * @return a string representation
     */
    public String toString()
    {
      return "Coverage: xPos: " + xPos + ", covDelta: " + covDelta;
    }

    /**
     * Returns a string representation of this entry and all the following
     * in the linked list.
     *
     * @return a string representation of this entry and all the following
     *         in the linked list
     */
    public String list()
    {
      String str = toString();
      if (next != null)
        str = str + " --> " + next.list();
      return str;
    }
  }

  /**
   * The head of the sorted list of buckets.
   */
  private Coverage head;

  /**
   * The current bucket. We make use of the fact that the scanline converter
   * always scans the scanline (and thus this list) from left to right to
   * quickly find buckets or insertion points.
   */
  private Coverage current;

  /**
   * The item that is before current in the list.
   */
  private Coverage currentPrev;

  /**
   * The bucket after the last valid bucket. Unused buckets are not thrown
   * away and garbage collected. Instead, we keep them at the tail of the list
   * and reuse them when necessary.
   */
  private Coverage last;

  /**
   * The last valid entry.
   */
  private Coverage lastPrev;

  /**
   * The minimum X coordinate of this scanline.
   */
  private int minX;

  /**
   * The maximum X coordinate of this scanline.
   */
  private int maxX;

  /**
   * The maximum coverage value.
   */
  private int maxCoverage;

  /**
   * The iterator over the ranges of this scanline.
   */
  private Iterator iterator;

  /**
   * Creates a new ScanlineCoverage instance.
   */
  public ScanlineCoverage()
  {
    iterator = new Iterator();
  }

  /**
   * Indicates the the next scan of the scanline begins and that the next
   * request will be at the beginning of this list. This makes searching and
   * sorting of this list very quick.
   */
  public void rewind()
  {
    current = head;
    currentPrev = null;
  }

  /**
   * Clears the list. This does not throw away the old buckets but only
   * resets the end-pointer of the list to the first element. All buckets are
   * then unused and are reused when the list is filled again.
   */
  public void clear()
  {
    last = head;
    lastPrev = null;
    current = head;
    currentPrev = null;
    minX = Integer.MAX_VALUE;
    maxX = Integer.MIN_VALUE;
  }

  /**
   * This adds the specified coverage to the pixel at the specified
   * X position.
   *
   * @param x the X position
   * @param xc the x coverage
   * @param yc the y coverage
   */
  public void add(int x, int xc, int yc)
  {
    Coverage bucket = findOrInsert(x);
    bucket.covDelta += xc;
    bucket.pixelCoverage += yc;
    minX = Math.min(minX, x);
    maxX = Math.max(maxX, x);
  }

  /**
   * Returns the maximum coverage value for the scanline.
   *
   * @return the maximum coverage value for the scanline
   */  
  public int getMaxCoverage()
  {
    return maxCoverage;
  }

  /**
   * Sets the maximum coverage value for the scanline.
   *
   * @param maxCov the maximum coverage value for the scanline
   */
  void setMaxCoverage(int maxCov)
  {
    maxCoverage = maxCov;
  }

  /**
   * Returns the maximum X coordinate of the current scanline.
   *
   * @return the maximum X coordinate of the current scanline
   */
  public int getMaxX()
  {
    return maxX;
  }

  /**
   * Returns the minimum X coordinate of the current scanline.
   *
   * @return the minimum X coordinate of the current scanline
   */
  public int getMinX()
  {
    return minX;
  }

  /**
   * Finds the bucket in the list with the specified X coordinate.
   * If no such bucket is found, then a new one is fetched (either a cached
   * bucket from the end of the list or a newly allocated one) inserted at the
   * correct position and returned.
   *
   * @param x the X coordinate
   *
   * @return a bucket to hold the coverage data
   */
  private Coverage findOrInsert(int x)
  {
    // First search for a matching bucket.
    if (head == null)
      {
        // Special case: the list is still empty.
        // Testpoint 1.
        head = new Coverage();
        head.xPos = x;
        current = head;
        currentPrev = null;
        return head;
      }

    // This performs a linear search, starting from the current bucket.
    // This is reasonably efficient because access to this list is always done
    // in a linear fashion and we are usually not more then 1 or 2 buckets away
    // from the one we're looking for.
    Coverage match = current;
    Coverage prev = currentPrev;
    while (match != last && match.xPos < x)
      {
        prev = match;
        match = match.next;
      }

    // At this point we have either found an entry with xPos >= x, or reached
    // the end of the list (match == last || match == null).
    if (match == null)
      {
        // End of the list. No cached items to reuse.
        // Testpoint 2.
        match = new Coverage();
        match.xPos = x;
        if (prev != null)
          prev.next = match;
        current = match;
        currentPrev = prev;
        return match;
      }
    else if (match == last)
      {
        // End of the list. Reuse this item. Expand list.
        // Testpoint 3.
        last = match.next;
        lastPrev = match;
        match.xPos = x;
        match.covDelta = 0;
        match.pixelCoverage = 0;
        // Keep link to last element or null, indicating the end of the list.
        current = match;
        currentPrev = prev;
        return match;
      }

    if (x == match.xPos)
      {
        // Special case: We have another coverage entry at the same location
        // as an already existing entry. Return this.
        // Testpoint 4.
        current = match;
        currentPrev = prev;
        return match;
      }
    else // x <= match.xPos
      {
        assert (x <= match.xPos);
        assert (prev == null ||x > prev.xPos);

        // Create new entry, or reuse existing one.
        Coverage cov;
        if (last != null)
          {
            // Testpoint 5.
            cov = last;
            last = cov.next;
            lastPrev.next = last;
          }
        else
          {
            // Testpoint 6.
            cov = new Coverage();
          }
        
        cov.xPos = x;
        cov.covDelta = 0;
        cov.pixelCoverage = 0;

        // Insert this item in the list.
        if (prev != null)
          {
            // Testpoint 5 & 6.
            prev.next = cov;
            cov.next = match;
            current = cov;
            currentPrev = prev;
          }
        else
          {
            // Testpoint 7.
            assert (match == head);
            // Insert at head.
            head = cov;
            head.next = match;
            current = head;
            currentPrev = null;
          }
        return cov;
      }
  }

  /**
   * (Re-)Starts iterating the coverage values for the scanline.
   * Use the returned iterator to get the consecutive coverage ranges.
   *
   * @return the iterator
   */
  public Iterator iterate()
  {
    iterator.reset();
    return iterator;
  }

  /**
   * Returns {@ true} if this object has no entries for the current scanline,
   * {@ false} otherwise.
   *
   * @return {@ true} if this object has no entries for the current scanline,
   *         {@ false} otherwise
   */
  public boolean isEmpty()
  {
    return head == null || head == last
           || head.next == null || head.next == last;
  }

}
