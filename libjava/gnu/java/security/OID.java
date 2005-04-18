/* OID.java -- numeric representation of an object identifier
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package gnu.java.security;

import gnu.java.security.der.DEREncodingException;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.StringTokenizer;

/**
 * This immutable class represents an object identifier, or OID.
 *
 * <p>OIDs are represented as a series of hierarcical tokens, each of
 * which is usually represented as a single, unsigned integer. The
 * hierarchy works so that later tokens are considered within the group
 * of earlier tokens. Thus, the OID for the Serpent block cipher,
 * 1.3.6.1.4.1.11591.13.2, is maintained by the GNU project, whose OID
 * is 1.3.6.1.4.1.11591 (which is, in turn, part of bigger, more general
 * bodies; the topmost, 1, stands for the OIDs assigned by the
 * International Standards Organization, ISO).
 *
 * <p>OIDs can be represented in a variety of ways, including the
 * dotted-decimal form we use here.
 *
 * <p>OIDs may be relative, in which case the first two elements of the
 * OID are omitted.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class OID implements Cloneable, Comparable, java.io.Serializable
{

  // Fields.
  // ------------------------------------------------------------------------

  /**
   * The numeric ID structure.
   */
  private int[] components;

  /**
   * The string representation of this OID, in dotted-decimal format.
   */
  private transient String strRep;

  /**
   * The DER encoding of this OID.
   */
  private transient byte[] der;

  /**
   * Whether or not this OID is relative.
   */
  private boolean relative;

  // Constructors.
  // ------------------------------------------------------------------------

  /**
   * Create a new OID from the given byte array. The argument (which can
   * neither be null nor zero-length) is copied to prevent subsequent
   * modification.
   *
   * @param components The numeric IDs.
   * @throws IllegalArgumentException If <i>components</i> is null or empty.
   */
  public OID(int[] components)
  {
    this(components, false);
  }

  /**
   * Create a new OID from the given byte array. The argument (which can
   * neither be null nor zero-length) is copied to prevent subsequent
   * modification.
   *
   * @param components The numeric IDs.
   * @param relative The relative flag.
   * @throws IllegalArgumentException If <i>components</i> is null or empty.
   */
  public OID(int[] components, boolean relative)
  {
    if (components == null || components.length == 0)
      throw new IllegalArgumentException();
    this.components = (int[]) components.clone();
    this.relative = relative;
  }

  /**
   * Create a new OID from the given dotted-decimal representation.
   *
   * @param strRep The string representation of the OID.
   * @throws IllegalArgumentException If the string does not contain at
   * least one integer.
   * @throws NumberFormatException If the string does not contain only
   * numbers and periods ('.').
   */
  public OID(String strRep)
  {
    this(strRep, false);
  }

  /**
   * Create a new OID from the given dotted-decimal representation.
   *
   * @param strRep The string representation of the OID.
   * @param relative The relative flag.
   * @throws IllegalArgumentException If the string does not contain at
   * least one integer.
   * @throws NumberFormatException If the string does not contain only
   * numbers and periods ('.').
   */
  public OID(String strRep, boolean relative)
  {
    this.relative = relative;
    this.strRep = strRep;
    components = fromString(strRep);
  }

  /**
   * Construct a new OID from the DER bytes in an input stream. This method
   * does not read the tag or the length field from the input stream, so
   * the caller must supply the number of octets in this OID's encoded
   * form.
   *
   * @param derIn The DER input stream.
   * @param len   The number of bytes in the encoded form.
   * @throws IOException If an error occurs reading the OID.
   */
  public OID(InputStream derIn, int len) throws IOException
  {
    this(derIn, len, false);
  }

  /**
   * Construct a new OID from the DER bytes in an input stream. This method
   * does not read the tag or the length field from the input stream, so
   * the caller must supply the number of octets in this OID's encoded
   * form.
   *
   * @param derIn The DER input stream.
   * @param len   The number of bytes in the encoded form.
   * @param relative The relative flag.
   * @throws IOException If an error occurs reading the OID.
   */
  public OID(InputStream derIn, int len, boolean relative) throws IOException
  {
    der = new byte[len];
    derIn.read(der);
    this.relative = relative;
    try
      {
        components = fromDER(der, relative);
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
        aioobe.printStackTrace();
        throw aioobe;
      }
  }

  /**
   * Construct a new OID from the given DER bytes.
   *
   * @param encoded The DER encoded OID.
   * @throws IOException If an error occurs reading the OID.
   */
  public OID(byte[] encoded) throws IOException
  {
    this(encoded, false);
  }

  /**
   * Construct a new OID from the given DER bytes.
   *
   * @param root The root OID.
   * @param encoded The encoded relative OID.
   * @param relative The relative flag.
   */
  public OID(byte[] encoded, boolean relative) throws IOException
  {
    der = (byte[]) encoded.clone();
    this.relative = relative;
    try
      {
        components = fromDER(der, relative);
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
        aioobe.printStackTrace();
        throw aioobe;
      }
  }

  /**
   * Our private constructor.
   */
  private OID()
  {
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Return the numeric IDs of this OID. The value returned is copied to
   * prevent modification.
   *
   * @return The IDs in a new integer array.
   */
  public int[] getIDs()
  {
    return (int[]) components.clone();
  }

  /**
   * Get the DER encoding of this OID, minus the tag and length fields.
   *
   * @return The DER bytes.
   */
  public byte[] getDER()
  {
    if (der == null)
      {
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        int i = 0;
        if (!relative)
          {
            int b = components[i++] * 40 + (components.length > 1
              ? components[i++] : 0);
            encodeSubID(bout, b);
          }
        for ( ; i < components.length; i++)
          encodeSubID(bout, components[i]);
        der = bout.toByteArray();
      }
    return (byte[]) der.clone();
  }

  /**
   * Get the parent OID of this OID. That is, if this OID is "1.2.3.4",
   * then the parent OID will be "1.2.3". If this OID is a top-level
   * OID, this method returns null.
   *
   * @return The parent OID, or null.
   */
  public OID getParent()
  {
    if (components.length == 1)
      return null;
    int[] parent = new int[components.length - 1];
    System.arraycopy(components, 0, parent, 0, parent.length);
    return new OID(parent);
  }

  public OID getChild(int id)
  {
    int[] child = new int[components.length + 1];
    System.arraycopy(components, 0, child, 0, components.length);
    child[child.length - 1] = id;
    return new OID(child);
  }

  /**
   * Get the root OID of this OID. That is, the first two components.
   *
   * @return The root OID.
   */
  public OID getRoot()
  {
    if (components.length <= 2)
      return this;
    int[] root = new int[2];
    root[0] = components[0];
    root[1] = components[1];
    return new OID(root);
  }

  public boolean isRelative()
  {
    return relative;
  }

  /**
   * Returns a copy of this OID.
   *
   * @return The copy.
   */
  public Object clone()
  {
    OID oid = new OID();
    oid.components = this.components;
    oid.strRep = this.strRep;
    return oid;
  }

  /* Nice idea, but possibly too expensive for whatever benefit it
   * provides.

  public String getShortName()
  {
    return OIDTable.getShortName(this);
  }

  public String getLongName()
  {
    return OIDTable.getLongName(this);
  }

  */

  /**
   * Returns the value of this OID in dotted-decimal format.
   *
   * @return The string representation.
   */
  public String toString()
  {
    if (strRep != null)
      return strRep;
    else
      {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < components.length; i++)
          {
            buf.append((long) components[i] & 0xFFFFFFFFL);
            if (i < components.length - 1)
              buf.append('.');
          }
        return (strRep = buf.toString());
      }
  }

  /**
   * Computes a hash code for this OID.
   *
   * @return The hash code.
   */
  public int hashCode()
  {
    int ret = 0;
    for (int i = 0; i < components.length; i++)
      ret += components[i] << (i & 31);
    return ret;
  }

  /**
   * Tests whether or not this OID equals another.
   *
   * @return Whether or not this OID equals the other.
   */
  public boolean equals(Object o)
  {
    if (!(o instanceof OID))
      return false;
    return java.util.Arrays.equals(components, ((OID) o).components);
  }

  /**
   * Compares this OID to another. The comparison is essentially
   * lexicographic, where the two OIDs are compared until their
   * first difference, then that difference is returned. If one OID is
   * shorter, but all elements equal between the two for the shorter
   * length, then the shorter OID is lesser than the longer.
   *
   * @param o The object to compare.
   * @return An integer less than, equal to, or greater than zero if
   *         this object is less than, equal to, or greater than the
   *         argument.
   * @throws ClassCastException If <i>o</i> is not an OID.
   */
  public int compareTo(Object o)
  {
    if (equals(o))
      return 0;
    int[] components2 = ((OID) o).components;
    int len = Math.min(components.length, components2.length);
    for (int i = 0; i < len; i++)
      {
        if (components[i] != components2[i])
          return (components[i] < components2[i]) ? -1 : 1;
      }
    if (components.length == components2.length)
      return 0;
    return (components.length < components2.length) ? -1 : 1;
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private static int[] fromDER(byte[] der, boolean relative)
    throws DEREncodingException
  {
    // cannot be longer than this.
    int[] components = new int[der.length + 1];
    int count = 0;
    int i = 0;
    if (!relative && i < der.length)
      {
        // Non-relative OIDs have the first two arcs coded as:
        //
        //   i = first_arc * 40 + second_arc;
        //
        int j = (der[i] & 0xFF);
        components[count++] = j / 40;
        components[count++] = j % 40;
        i++;
      }
    while (i < der.length)
      {
        int j = 0;
        do
          {
            j = der[i++] & 0xFF;
            components[count] <<= 7;
            components[count]  |= j & 0x7F;
            if (i >= der.length && (j & 0x80) != 0)
              throw new DEREncodingException("malformed OID");
          }
        while ((j & 0x80) != 0);
        count++;
      }
    if (count == components.length)
      return components;
    int[] ret = new int[count];
    System.arraycopy(components, 0, ret, 0, count);
    return ret;
  }

  private static int[] fromString(String strRep) throws NumberFormatException
  {
    if (strRep.startsWith("OID.") || strRep.startsWith("oid."))
      strRep = strRep.substring(4);
    StringTokenizer tok = new StringTokenizer(strRep, ".");
    if (tok.countTokens() == 0)
      throw new IllegalArgumentException();
    int[] components = new int[tok.countTokens()];
    int i = 0;
    while (tok.hasMoreTokens())
      {
        components[i++] = Integer.parseInt(tok.nextToken());
      }
    return components;
  }

  private static void encodeSubID(ByteArrayOutputStream out, int id)
  {
    if (id < 128)
      {
        out.write(id);
      }
    else if (id < 16384)
      {
        out.write((id >>> 7) | 0x80);
        out.write(id & 0x7F);
      }
    else if (id < 2097152)
      {
        out.write((id >>> 14) | 0x80);
        out.write(((id >>> 7) | 0x80) & 0xFF);
        out.write(id & 0x7F);
      }
    else if (id < 268435456)
      {
        out.write( (id >>> 21) | 0x80);
        out.write(((id >>> 14) | 0x80) & 0xFF);
        out.write(((id >>>  7) | 0x80) & 0xFF);
        out.write(id & 0x7F);
      }
  }
}
