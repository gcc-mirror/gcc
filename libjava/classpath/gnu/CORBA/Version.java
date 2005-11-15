/* Version.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.CORBA;

import java.io.IOException;
import java.io.Serializable;

import org.omg.CORBA.MARSHAL;

/**
 * A version number, represented by the major version number
 * and the minor version number.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Version
  implements Serializable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  /**
   * Major number (0..256, so the byte cannot be used).
   */
  public final int major;

  /**
   * Minor number.
   */
  public final int minor;

  /**
   * Create the version with the given version numbers.
   *
   * @param _major major number (0..255)
   * @param _minor minor number (0..255)
   */
  public Version(int _major, int _minor)
  {
    major = (byte) _major;
    minor = (byte) _minor;
  }

  /**
   * Returns true if the versions are equal.
   * @param other the other version to compare.
   *
   * @return true if the versions are equal
   */
  public boolean equals(java.lang.Object other)
  {
    if (other == this)
      {
        return true;
      }
    if (!(other instanceof Version))
      {
        return false;
      }

    Version that = (Version) other;
    return same(that);
  }
  
  /**
   * Get the hashcode, higher 8 bits being the major version and lower 8 bits
   * the minor version.
   */
  public int hashCode()
  {
    return major << 8 | minor;
  }    

  /**
   * Read from the input stream, major number first.
   * @param in a stream to read from.
   */
  public static Version read_version(java.io.InputStream in)
  {
    try
      {
        int major = in.read() & 0xFF;
        int minor = in.read() & 0xFF;
        return new Version(major, minor);
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL("IOException while reading message header");
        m.initCause(ex);
        m.minor = Minor.Header;
        throw m;
      }
  }

  /**
   * Returns true if the versions are the same.
   *
   * @param that the other version to compare.
   *
   * @return true if the versions are the same.
   */
  public boolean same(Version that)
  {
    return major == that.major && minor == that.minor;
  }

  /**
   * Returns true if the given version is higher than
   * or equals to the version, supplied as parameter
   * in the form of two integers.
   *
   * @param a_major major number of the version to compare.
   * @param a_minor minor number of the version to compare.
   *
   * @return true if this version is higher than or equals to
   * the version v.
   */
  public boolean since_inclusive(int a_major, int a_minor)
  {
    if (major > a_major)
      return true;
    else if (major < a_major)
      return false;
    else

      // Major numbers are equal.
      return minor >= a_minor;
  }

  /**
   * Return the string representation, in the form
   * major.minor.
   */
  public String toString()
  {
    return major + "." + minor;
  }

  /**
   * Returs true if the given version is lower or equal to the
   * version, specified by the provided minor and major version
   * number. This means, the version, specified by these two numbers,
   * should be supported by the current version.
   *
   * @param a_major a major version number.
   * @param a_minor a minor version number.
   *
   * @return true if the current version should be supported by the
   * version, specified by the two passed numbers.
   */
  public boolean until_inclusive(int a_major, int a_minor)
  {
    if (major < a_major)
      return true;
    else if (major > a_major)
      return false;
    else

      // Major numbers are equal.
      return minor <= a_minor;
  }

  /**
   * Write into the output stream, major number first.
   *
   * @param out a stream to write into.
   */
  public void write(java.io.OutputStream out)
  {
    try
      {
        out.write(major & 0xFF);
        out.write(minor & 0xFF);
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL("IOException while writing message header");
        m.minor = Minor.Header;
        m.initCause(ex);
        throw m;
      }
  }
 
}
