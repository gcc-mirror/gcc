/* UUID.java -- Class that represents a UUID object.
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


package java.util;

import java.io.Serializable;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * This class represents a 128-bit UUID value.
 * 
 * There are several types of UUID, and while this class can be used to store
 * them, only the Leach-Salz (variant 2) UUID specified in RFC-4122 will 
 * give meaningful results from the method calls.
 * See: http://tools.ietf.org/html/4122 for the details
 *
 * The format of a Leach-Salz (variant 2) time-based (version 1) UUID 
 * is as follows:
 * time_low - upper 32 bits of the most significant 64 bits,
 *            this is the least-significant part of the timestamp.
 *
 * time_mid - bits 16-31 of the most significant 64 bits,
 *            this is the middle portion of the timestamp. 
 *
 * version  - bits 8-15 of the most significant 64 bits. 
 *
 * time_hi  - bits 0-7 of the most significant 64 bits,
 *            the most significant portion of the timestamp.
 *
 * clock_and_reserved  - bits 48-63 of the least significant 64 bits.
 *                       a variable number of bits hold the variant 
 *                       (see the spec)
 * 
 * node identifier     - bits 0-47 of the least signficant 64 bits.
 *
 * These fields are valid only for version 1, in the remaining versions,
 * only the version and variant fields are set, all others are used for data.
 *
 * @since 1.5
 * @author Sven de Marothy
 */
public final class UUID 
  extends Object 
  implements Serializable, Comparable<UUID>
{
  private static final long serialVersionUID = -4856846361193249489L;

  /**
   * Serialized field - most significant 64 bits.
   */
  private long mostSigBits;

  /**
   * Serialized field - least significant 64 bits.
   */
  private long leastSigBits;

  /**
   * Random-number generator.
   */
  private static transient Random r = new Random();

  /**
   * Constructs a new UUID.
   *
   * @since 1.5
   */
  public UUID(long mostSigBits, long leastSigBits)
  {
    this.mostSigBits = mostSigBits;
    this.leastSigBits = leastSigBits;
  }
 
  /**
   * Returns the clock-sequence value of this UUID.
   * This field only exists in a time-based (version 1) UUID.
   *
   * @throws UnsupportedOperationException if the UUID type is not 1.
   * @returns an int containing the clock-sequence value.
   */
  public int clockSequence()
  {
    if( version() != 1 )
      throw new UnsupportedOperationException("Not a type 1 UUID");
    return (int)((leastSigBits & 0x3FFF000000000000L) >> 48);
  }

  /**
   * Compare this UUID to another.
   * The comparison is performed as between two 128-bit integers.
   *
   * @return -1 if this < val, 0 if they are equal, 1 if this > val.
   */
  public int compareTo(UUID o)
  {
    if( mostSigBits < o.mostSigBits )
      return -1;
    if( mostSigBits > o.mostSigBits )
      return 1;
    if( leastSigBits < o.leastSigBits )
      return -1;
    if( leastSigBits > o.mostSigBits )
      return 1;
    return 0;
  }

  /**
   * Compare a (UUID) object to this one
   */
  public boolean equals(Object obj)
  {
    if( !(obj instanceof UUID ) )
      return false;
    return ( ((UUID)obj).mostSigBits == mostSigBits && 
	     ((UUID)obj).leastSigBits == leastSigBits );
  }

  /**
   * Creates a UUID object from a Sting representation.
   *
   * For the format of the string,
   * @see #toString()
   *
   * @return a new UUID object.
   */
  public static UUID fromString(String name)
  {
    StringTokenizer st = new StringTokenizer( name.trim(), "-" );
    if( st.countTokens() < 5 )
      throw new IllegalArgumentException( "Incorrect UUID string"+
					  " representation:"+name );

    long msb = (Long.parseLong(st.nextToken(), 16) << 32); // time low
    msb |= (Long.parseLong(st.nextToken(), 16) << 16); // time mid
    msb |= Long.parseLong(st.nextToken(), 16); // time high

    long lsb = (Long.parseLong(st.nextToken(), 16) << 48); // clock
    lsb |= Long.parseLong(st.nextToken(), 16); // node

    return new UUID(msb, lsb);
  }

  /**
   * Returns a String representation of the UUID.
   *
   * The format of the standard string representation (given in RFC4122) is:
   *
   * time-low "-" time-mid "-"
   * time-high-and-version "-"
   * clock-seq-and-reserved
   * clock-seq-low "-" node
   *
   * Where each field is represented as a hex string.
   *
   * @return the String representation.
   */
  public String toString()
  {
    return // time-low first
      padHex( (( mostSigBits & 0xFFFFFFFF00000000L) >> 32) & 0xFFFFFFFFL, 8)
      + "-" + // then time-mid
      padHex( (( mostSigBits & 0xFFFF0000L ) >> 16), 4 ) 
      + "-" + // time-high
      padHex( ( mostSigBits & 0x0000000000000000FFFFL ), 4 ) 
      + "-" + // clock (note - no reason to separate high and low here)
      padHex( (((leastSigBits & 0xFFFF000000000000L) >> 48) & 0xFFFF), 4 ) 
      + "-" + // finally the node value.
      padHex(leastSigBits & 0xFFFFFFFFFFFFL, 12); 
  }

  /**
   * Returns the least significant 64 bits of the UUID as a <code>long</code>.
   */ 
  public long getLeastSignificantBits()
  {
    return leastSigBits;
  }

  /**
   * Returns the most significant 64 bits of the UUID as a <code>long</code>.
   */ 
  public long getMostSignificantBits()
  {
    return mostSigBits;
  }

  /**
   * Returns a hash of this UUID.
   */
  public int hashCode()
  {
    int l1 = (int)(leastSigBits & 0xFFFFFFFFL);
    int l2 = (int)((leastSigBits & 0xFFFFFFFF00000000L) >> 32);
    int m1 = (int)(mostSigBits & 0xFFFFFFFFL);
    int m2 = (int)((mostSigBits & 0xFFFFFFFF00000000L) >> 32);

    return (l1 ^ l2) ^ (m1 ^ m2);
  }

  /**
   * Creates a UUID version 3 object (name based with MD5 hashing)
   * from a series of bytes representing a name.
   */
  public static UUID nameUUIDFromBytes(byte[] name)
  {    
    long msb, lsb;
    byte[] hash;

    try
      {
	MessageDigest md5 = MessageDigest.getInstance("MD5");
	hash = md5.digest( name );
      } 
    catch (NoSuchAlgorithmException e) 
      {
	throw new UnsupportedOperationException("No MD5 algorithm available.");
      }
	
    msb = ((hash[0] & 0xFFL) << 56) | ((hash[1] & 0xFFL) << 48) |
      ((hash[2] & 0xFFL) << 40) | ((hash[3] & 0xFFL) << 32) |
      ((hash[4] & 0xFFL) << 24) | ((hash[5] & 0xFFL) << 16) |
      ((hash[6] & 0xFFL) << 8) | (hash[7] & 0xFFL);

    lsb = ((hash[8] & 0xFFL) << 56) | ((hash[9] & 0xFFL) << 48) |
      ((hash[10] & 0xFFL) << 40) | ((hash[11] & 0xFFL) << 32) |
      ((hash[12] & 0xFFL) << 24) | ((hash[13] & 0xFFL) << 16) |
      ((hash[14] & 0xFFL) << 8) | (hash[15] & 0xFFL);

    lsb &= 0x3FFFFFFFFFFFFFFFL; 
    lsb |= 0x8000000000000000L; // set top two bits to variant 2

    msb &= 0xFFFFFFFFFFFF0FFFL; 
    msb |= 0x3000; // Version 3; 

    return new UUID(msb, lsb);
  }

  /**
   * Returns the 48-bit node value in a long. 
   * This field only exists in a time-based (version 1) UUID.
   *
   * @throws UnsupportedOperationException if the UUID type is not 1.
   * @returns a long with the node value in the lower 48 bits.
   */
  public long node() 
  {
    if( version() != 1 )
      throw new UnsupportedOperationException("Not a type 1 UUID");
    return (leastSigBits & 0xFFFFFFFFFFFFL);
  }

  /**
   * Returns the 60-bit timestamp value of the UUID in a long. 
   * This field only exists in a time-based (version 1) UUID.
   *
   * @throws UnsupportedOperationException if the UUID type is not 1.
   * @returns a long with the timestamp value.
   */
  public long timestamp()
  {
    if( version() != 1 )
      throw new UnsupportedOperationException("Not a type 1 UUID");
    long time = (( mostSigBits & 0xFFFFFFFF00000000L) >> 32);
    time |= (( mostSigBits & 0xFFFF0000L ) << 16);
    long time_hi = ( mostSigBits & 0xFFFL );
    time |= (time_hi << 48);
    return time;
  }

  /**
   * Generate a Leach-Salz (Variant 2) randomly generated (version 4)
   * UUID.
   *
   */
  public static UUID randomUUID()
  {  
    long lsb = r.nextLong(); 
    long msb = r.nextLong();

    lsb &= 0x3FFFFFFFFFFFFFFFL; 
    lsb |= 0x8000000000000000L; // set top two bits to variant 2

    msb &= 0xFFFFFFFFFFFF0FFFL; 
    msb |= 0x4000; // Version 4; 

    return new UUID( msb, lsb );
  }

  /**
   * Returns a hex String from l, padded to n spaces.
   */
  private String padHex( long l, int n )
  {
    String s = Long.toHexString( l );
    while( s.length() < n )
      s = "0" + s;
    return s;
  }

  /**
   * Returns the variant of the UUID
   *
   * This may be:
   * 0 = Reserved for NCS backwards-compatibility
   * 2 = Leach-Salz (supports the other methods in this class)
   * 6 = Reserved for Microsoft backwards-compatibility
   * 7 = (reserved for future use)
   */
  public int variant()
  {
    // Get the top 3 bits (not all may be part of the variant)
    int v = (int)((leastSigBits & 0xE000000000000000L) >> 61);
    if( (v & 0x04) == 0 ) // msb of the variant is 0
      return 0;
    if( (v & 0x02) == 0 ) // variant is 0 1 (Leach-Salz)
      return 2;
    return v; // 6 or 7 
  }

  /**
   * Returns the version # of the UUID.
   *
   * Valid version numbers for a variant 2 UUID are:
   * 1 = Time based UUID
   * 2 = DCE security UUID
   * 3 = Name-based UUID using MD5 hashing
   * 4 = Randomly generated UUID
   * 5 = Name-based UUID using SHA-1 hashing
   *
   * @return the version number
   */
  public int version()
  {
    return (int)((mostSigBits & 0xF000L) >> 12);
  }
}
