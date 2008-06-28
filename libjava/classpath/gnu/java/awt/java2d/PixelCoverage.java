package gnu.java.awt.java2d;

/**
 * Stores and handles the pixel converage for a scanline. The pixel coverage
 * is stored as sorted list of buckets, each of which holds information about
 * the coverage for the X and Y axis. This is utilized to compute the actual
 * coverage for each pixel on the scanline and finding chunks of pixels with
 * equal coverage.
 */
final class PixelCoverage
{

  /**
   * One bucket in the list.
   */
  private static final class Bucket
  {
    /**
     * The X coordinate on the scanline to which this bucket belongs.
     */
    int xPos;

    /**
     * The X coverage.
     */
    int xCov;

    /**
     * The Y coverage.
     */
    int yCov;

    /**
     * Implements a linked list. This points to the next element of the list.
     */
    Bucket next;

    /**
     * Implements a linked list. This points to the previous element of the
     * list.
     */
    Bucket prev;
  }

  /**
   * The head of the sorted list of buckets.
   */
  private Bucket head;

  /**
   * The current bucket. We make use of the fact that the scanline converter
   * always scans the scanline (and thus this list) from left to right to
   * quickly find buckets or insertion points.
   */
  private Bucket current;

  /**
   * The bucket after the last valid bucket. Unused buckets are not thrown
   * away and garbage collected. Instead, we keep them at the tail of the list
   * and reuse them when necessary.
   */
  private Bucket last;

  /**
   * Indicates the the next scan of the scanline begins and that the next
   * request will be at the beginning of this list. This makes searching and
   * sorting of this list very quick.
   */
  void rewind()
  {
    current = head;
  }

  /**
   * Clears the list. This does not throw away the old buckets but only
   * resets the end-pointer of the list to the first element. All buckets are
   * then unused and are reused when the list is filled again.
   */
  void clear()
  {
    last = head;
  }

  /**
   * This adds the specified x and y coverage to the pixel at the specified
   * X position.
   *
   * @param x the X position
   * @param xc the x coverage
   * @param yc the y coverage
   */
  void add(int x, int xc, int yc)
  {
    Bucket bucket = findOrInsert(x);
    bucket.xCov += xc;
    bucket.yCov += yc;
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
  private Bucket findOrInsert(int x)
  {
    // First search for a matching bucket.
    if (head == null)
      {
        // Special case: the list is still empty.
        head = new Bucket();
        current = head;
        return head;
      }

    // This performs a linear search, starting from the current bucket.
    // This is reasonably efficient because access to this list is always done
    // in a linear fashion and we are not more then 1 or 2 buckets away from
    // the one we're looking for.
    Bucket match = current;
    while (match != null && match.xPos != x)
      {
        
      }

    return match;
  }
}
