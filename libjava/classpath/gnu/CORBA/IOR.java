/* IOR.java --
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

import gnu.CORBA.CDR.cdrBufInput;
import gnu.CORBA.CDR.cdrBufOutput;
import gnu.CORBA.CDR.cdrInput;
import gnu.CORBA.CDR.cdrOutput;
import gnu.CORBA.GIOP.CharSets_OSF;
import gnu.CORBA.GIOP.cxCodeSet;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.ULongSeqHelper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * The implementaton of the Interoperable Object Reference (IOR).
 * IOR can be compared with the Internet address for a web page,
 * it provides means to locate the CORBA service on the web.
 * IOR contains the host address, port number, the object identifier
 * (key) inside the server, the communication protocol version,
 * supported charsets and so on.
 *
 * Ths class provides method for encoding and
 * decoding the IOR information from/to the stringified references,
 * usually returned by {@link org.omg.CORBA.ORB#String object_to_string()}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 *
 * @see org.mog.CORBA.Object.object_to_string(Object forObject)
 * @see string_to_object(String IOR)
 */
public class IOR
{
  /**
   * The code sets profile.
   */
  public static class CodeSets_profile
  {
    /**
     * The code set component.
     */
    public static class CodeSet_component
    {
      /**
       * The conversion code sets.
       */
      public int[] conversion;

      /**
       * The native code set.
       */
      public int native_set;

      /**
       * Read from the CDR stream.
       */
      public void read(org.omg.CORBA.portable.InputStream in)
      {
        native_set = in.read_ulong();
        conversion = ULongSeqHelper.read(in);
      }

      /**
       * Get a string representation.
       */
      public String toString()
      {
        StringBuffer b = new StringBuffer();
        b.append("native " + name(native_set));
        if (conversion != null && conversion.length > 0)
          {
            b.append(" conversion ");
            for (int i = 0; i < conversion.length; i++)
              {
                b.append(name(conversion [ i ]));
                b.append(' ');
              }
          }
        b.append(' ');
        return b.toString();
      }

      /**
       * Write into CDR stream.
       */
      public void write(org.omg.CORBA.portable.OutputStream out)
      {
        out.write_long(native_set);
        ULongSeqHelper.write(out, conversion);
      }

      private String name(int set)
      {
        return "0x" + Integer.toHexString(set) + " (" +
               CharSets_OSF.getName(set) + ") ";
      }
    }

    /**
     * The agreed tag for the Codesets profile.
     */
    public static final int TAG_CODE_SETS = 1;

    /**
     * Information about narrow character encoding (TCS-C).
     */
    public CodeSet_component narrow = new CodeSet_component();

    /**
     * About wide character encoding (TCS-W).
     */
    public CodeSet_component wide = new CodeSet_component();

    /**
     * The negotiated coding result for this IOR. Saves time, requred for
     * negotiation computations.
     */
    public cxCodeSet negotiated;

    /**
     * Read the code set profile information from the given input stream.
     *
     * @param profile a stream to read from.
     */
    public void read(cdrInput profile)
    {
      cdrBufInput encapsulation = profile.read_encapsulation();
      narrow.read(encapsulation);
      wide.read(encapsulation);
    }

    /**
     * Returns a string representation.
     */
    public String toString()
    {
      return "Narrow char: " + narrow + ", Wide char: " + wide;
    }

    /**
     * Write the code set profile information into the given input stream.
     *
     * @param profile a stream to write into.
     */
    public void write(cdrOutput profile)
    {
      cdrOutput encapsulation = profile.createEncapsulation();
      narrow.write(encapsulation);
      wide.write(encapsulation);
      try
        {
          encapsulation.close();
        }
      catch (IOException ex)
        {
          throw new InternalError();
        }
    }
  }

  /**
   * The internet profile.
   */
  public static class Internet_profile
  {
    /**
     * The agreed tag for the Internet profile.
     */
    public static final int TAG_INTERNET_IOP = 0;

    /**
     * The host.
     */
    public String host;

    /**
     * The IIOP version (initialised to 1.2 by default).
     */
    public Version version = new Version(1, 2);

    /**
     * The port.
     */
    public int port;

    /**
     * Return the human readable representation.
     */
    public String toString()
    {
      StringBuffer b = new StringBuffer();
      b.append(host);
      b.append(":");
      b.append(port);
      b.append(" (v");
      b.append(version);
      b.append(")");
      return b.toString();
    }
  }

  /**
   * The standard minor code, indicating that the string to object
   * converstio has failed due non specific reasons.
   */
  public static final int FAILED = 10;

  /**
   * The code sets profile of this IOR.
   */
  public CodeSets_profile CodeSets = new CodeSets_profile();

  /**
   * The internet profile of this IOR.
   */
  public Internet_profile Internet = new Internet_profile();

  /**
   * The object repository Id.
   */
  public String Id;

  /**
   * The additional tagged components, encapsulated in
   * the byte arrays. They are only supported by the
   * later versions, than currently implemented.
   */
  public byte[][] extra;

  /**
   * The object key.
   */
  public byte[] key;

  /**
   * True if the profile was encoded using the Big Endian or
   * the encoding is not known.
   *
   * false if it was encoded using the Little Endian.
   */
  public boolean Big_Endian = true;

  /**
   * Create an empty instance, initialising the code sets to default
   * values.
   */
  public IOR()
  {
    int[] supported = CharSets_OSF.getSupportedCharSets();

    CodeSets.narrow.native_set = CharSets_OSF.NATIVE_CHARACTER;
    CodeSets.narrow.conversion = supported;

    CodeSets.wide.native_set = CharSets_OSF.NATIVE_WIDE_CHARACTER;
    CodeSets.wide.conversion = supported;
  }

  /**
   * Parse the provided stringifed reference.
   *
   * @param stringified_reference, in the form of
   * IOR:nnnnnn.....
   *
   * @return the parsed IOR
   *
   * @throws BAD_PARAM, minor code 10, if the IOR cannot be parsed.
   *
   * TODO corballoc and other alternative formats.
   */
  public static IOR parse(String stringified_reference)
                   throws BAD_PARAM
  {
    try
      {
        if (!stringified_reference.startsWith("IOR:"))
          throw new BAD_PARAM("The string refernce must start with IOR:",
                              FAILED, CompletionStatus.COMPLETED_NO
                             );

        IOR r = new IOR();

        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        String x = stringified_reference;
        x = x.substring(x.indexOf(":") + 1);

        char cx;

        for (int i = 0; i < x.length(); i = i + 2)
          {
            cx = (char) Integer.parseInt(x.substring(i, i + 2), 16);
            buf.write(cx);
          }

        cdrBufInput cdr = new cdrBufInput(buf.toByteArray());

        r._read(cdr);
        return r;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        throw new BAD_PARAM(ex + " while parsing " + stringified_reference,
                            FAILED, CompletionStatus.COMPLETED_NO
                           );
      }
  }

  /**
   * Read the IOR from the provided input stream.
   *
   * @param c a stream to read from.
   * @throws IOException if the stream throws it.
   */
  public void _read(cdrInput c)
             throws IOException, BAD_PARAM
  {
    int endian;

    endian = c.read_long();
    if (endian != 0)
      {
        Big_Endian = false;
        c.setBigEndian(false);
      }
    _read_no_endian(c);
  }

  /**
   * Read the IOR from the provided input stream, not reading
   * the endian data at the beginning of the stream. The IOR is
   * thansferred in this form in
   * {@link write_Object(org.omg.CORBA.Object)}.
   *
   * If the stream contains a null value, the Id and Internet fields become
   * equal to null. Otherwise Id contains some string (possibly
   * empty).
   *
   * Id is checked for null in cdrInput that then returns
   * null instead of object.
   *
   * @param c a stream to read from.
   * @throws IOException if the stream throws it.
   */
  public void _read_no_endian(cdrInput c)
                       throws IOException, BAD_PARAM
  {
    Id = c.read_string();

    int n_profiles = c.read_long();

    if (n_profiles == 0)
      {
        Id = null;
        Internet = null;
        return;
      }

    for (int i = 0; i < n_profiles; i++)
      {
        int tag = c.read_long();
        cdrBufInput profile = c.read_encapsulation();

        if (tag == Internet_profile.TAG_INTERNET_IOP)
          {
            Internet = new Internet_profile();
            Internet.version = Version.read_version(profile);
            Internet.host = profile.read_string();
            Internet.port = profile.gnu_read_ushort();

            int lk = profile.read_long();
            key = new byte[ lk ];
            profile.read(key);

            // Read tagged components.
            int n_components = 0;

            try
              {
                if (Internet.version.since_inclusive(1, 1))
                  n_components = profile.read_long();

                for (int t = 0; t < n_components; t++)
                  {
                    int ctag = profile.read_long();

                    if (ctag == CodeSets_profile.TAG_CODE_SETS)
                      {
                        CodeSets.read(profile);
                      }
                  }
              }
            catch (Unexpected ex)
              {
                ex.printStackTrace();
              }
          }
      }
  }

  /**
   * Write this IOR record to the provided CDR stream.
   * This procedure writes the zero (Big Endian) marker first.
   */
  public void _write(cdrOutput out)
  {
    // Always use Big Endian.
    out.write(0);
    _write_no_endian(out);
  }

  /**
   * Write a null value to the CDR output stream.
   *
   * The null value is written as defined in OMG specification
   * (zero length string, followed by an empty set of profiles).
   */
  public static void write_null(cdrOutput out)
  {
    // Empty Id string.
    out.write_string("");

    // Empty set of profiles.
    out.write_long(0);
  }

  /**
   * Write this IOR record to the provided CDR stream. The procedure
   * writed data in Big Endian, but does NOT add any endian marker
   * to the beginning.
   */
  public void _write_no_endian(cdrOutput out)
  {
    try
      {
        // Write repository id.
        out.write_string(Id);

        // Always one profile.
        out.write_long(1);

        // It is the Internet profile.
        out.write_long(Internet_profile.TAG_INTERNET_IOP);

        // Need to write the Internet profile into the separate
        // stream as we must know the size in advance.
        cdrOutput b = out.createEncapsulation();

        Internet.version.write(b);
        b.write_string(Internet.host);

        b.write_ushort((short) (Internet.port & 0xFFFF));

        // Write the object key.
        b.write_long(key.length);
        b.write(key);

        // One tagged component.
        b.write_long(1);

        b.write_long(CodeSets_profile.TAG_CODE_SETS);
        CodeSets.write(b);

        b.close();
      }
    catch (IOException ex)
      {
        Unexpected.error(ex);
      }
  }

  /**
   * Returns a human readable string representation of this IOR object.
   */
  public String toString()
  {
    StringBuffer b = new StringBuffer();
    b.append(Id);
    b.append(" at ");
    b.append(Internet);

    if (!Big_Endian)
      b.append(" (Little endian) ");

    b.append(" Key ");

    for (int i = 0; i < key.length; i++)
      {
        b.append(Integer.toHexString(key [ i ] & 0xFF));
      }

    b.append(" ");
    b.append(CodeSets);

    return b.toString();
  }

  /**
   * Returs a stringified reference.
   *
   * @return a newly constructed stringified reference.
   */
  public String toStringifiedReference()
  {
    cdrBufOutput out = new cdrBufOutput();

    _write(out);

    StringBuffer b = new StringBuffer("IOR:");

    byte[] binary = out.buffer.toByteArray();
    String s;

    for (int i = 0; i < binary.length; i++)
      {
        s = Integer.toHexString(binary [ i ] & 0xFF);
        if (s.length() == 1)
          b.append('0');
        b.append(s);
      }

    return b.toString();
  }
}