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

import gnu.CORBA.CDR.BufferredCdrInput;
import gnu.CORBA.CDR.BufferedCdrOutput;
import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.CDR.AbstractCdrOutput;
import gnu.CORBA.GIOP.CharSets_OSF;
import gnu.CORBA.GIOP.CodeSetServiceContext;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ULongSeqHelper;
import org.omg.IOP.TAG_INTERNET_IOP;
import org.omg.IOP.TAG_MULTIPLE_COMPONENTS;
import org.omg.IOP.TaggedComponent;
import org.omg.IOP.TaggedComponentHelper;
import org.omg.IOP.TaggedProfile;
import org.omg.IOP.TaggedProfileHelper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.zip.Adler32;

/**
 * The implementaton of the Interoperable Object Reference (IOR). IOR can be
 * compared with the Internet address for a web page, it provides means to
 * locate the CORBA service on the web. IOR contains the host address, port
 * number, the object identifier (key) inside the server, the communication
 * protocol version, supported charsets and so on.
 *
 * Ths class provides method for encoding and decoding the IOR information
 * from/to the stringified references, usually returned by
 * {@link org.omg.CORBA.ORB#String object_to_string()}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 *
 * @see org.mog.CORBA.Object.object_to_string(Object forObject)
 * @see string_to_object(String IOR)
 */
public class IOR
{
  /**
   * The code sets tagged component, normally part of the Internet profile. This
   * compone consists of the two componenets itself.
   */
  public static class CodeSets_profile
  {
    public CodeSets_profile()
    {
      int[] supported = CharSets_OSF.getSupportedCharSets();

      narrow.native_set = CharSets_OSF.NATIVE_CHARACTER;
      narrow.conversion = supported;

      wide.native_set = CharSets_OSF.NATIVE_WIDE_CHARACTER;
      wide.conversion = supported;
    }

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
                b.append(name(conversion[i]));
                b.append(' ');
              }
          }
        b.append(' ');
        return b.toString();
      }
      
      /**
       * Get a better formatted multiline string representation.
       */
      public String toStringFormatted()
      {
        StringBuffer b = new StringBuffer();
        b.append("\n  Native set " + name(native_set));
        if (conversion != null && conversion.length > 0)
          {
            b.append("\n  Other supported sets:\n    ");
            for (int i = 0; i < conversion.length; i++)
              {
                b.append(name(conversion[i]));
                b.append(' ');
              }
          }
        b.append("\n");
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
        return "0x" + Integer.toHexString(set) + " ("
               + CharSets_OSF.getName(set) + ") ";
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
    public CodeSetServiceContext negotiated;

    /**
     * Read the code set profile information from the given input stream.
     *
     * @param profile a stream to read from.
     */
    public void read(AbstractCdrInput profile)
    {
      BufferredCdrInput encapsulation = profile.read_encapsulation();
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
    public void write(AbstractCdrOutput profile)
    {
      AbstractCdrOutput encapsulation = profile.createEncapsulation();
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
  public class Internet_profile
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
     * The code sets component in the internet profile of this IOR. This is not
     * a separate profile.
     */
    public CodeSets_profile CodeSets = new CodeSets_profile();

    /**
     * Reserved for all components of this profile, this array holds the
     * components other than code set components.
     */
    ArrayList components = new ArrayList();

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
      if (components.size() > 0)
        b.append(" " + components.size() + " extra components.");
      return b.toString();
    }

    /**
     * Write the internet profile (except the heading tag.
     */
    public void write(AbstractCdrOutput out)
    {
      try
        {
          // Need to write the Internet profile into the separate
          // stream as we must know the size in advance.
          AbstractCdrOutput b = out.createEncapsulation();

          version.write(b);
          b.write_string(host);

          b.write_ushort((short) (port & 0xFFFF));

          // Write the object key.
          b.write_long(key.length);
          b.write(key);

          // Number of the tagged components.
          b.write_long(1 + components.size());

          b.write_long(CodeSets_profile.TAG_CODE_SETS);
          CodeSets.write(b);

          TaggedComponent t;

          for (int i = 0; i < components.size(); i++)
            {
              t = (TaggedComponent) components.get(i);
              TaggedComponentHelper.write(b, t);
            }

          b.close();
        }
      catch (Exception e)
        {
          MARSHAL m = new MARSHAL("Unable to write Internet profile.");
          m.minor = Minor.IOR;
          m.initCause(e);
          throw m;
        }
    }
  }

  /**
   * The standard minor code, indicating that the string to object converstio
   * has failed due non specific reasons.
   */
  public static final int FAILED = 10;

  /**
   * The internet profile of this IOR.
   */
  public Internet_profile Internet = new Internet_profile();

  /**
   * The object repository Id.
   */
  public String Id;

  /**
   * The object key.
   */
  public byte[] key;

  /**
   * All tagged profiles of this IOR, except the separately defined Internet
   * profile.
   */
  ArrayList profiles = new ArrayList();

  /**
   * True if the profile was encoded using the Big Endian or the encoding is not
   * known.
   *
   * false if it was encoded using the Little Endian.
   */
  public boolean Big_Endian = true;

  /**
   * Create an empty instance, initialising the code sets to default values.
   */
  public IOR()
  {
  }

  /**
   * Parse the provided stringifed reference.
   *
   * @param stringified_reference, in the form of IOR:nnnnnn.....
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
                              FAILED, CompletionStatus.COMPLETED_NO);

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

        BufferredCdrInput cdr = new BufferredCdrInput(buf.toByteArray());

        r._read(cdr);
        return r;
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        throw new BAD_PARAM(ex + " while parsing " + stringified_reference,
                            FAILED, CompletionStatus.COMPLETED_NO);
      }
  }

  /**
   * Read the IOR from the provided input stream.
   *
   * @param c a stream to read from.
   * @throws IOException if the stream throws it.
   */
  public void _read(AbstractCdrInput c)
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
   * Read the IOR from the provided input stream, not reading the endian data at
   * the beginning of the stream. The IOR is thansferred in this form in
   * {@link write_Object(org.omg.CORBA.Object)}.
   *
   * If the stream contains a null value, the Id and Internet fields become
   * equal to null. Otherwise Id contains some string (possibly empty).
   *
   * Id is checked for null in AbstractCdrInput that then returns null instead of
   * object.
   *
   * @param c a stream to read from.
   * @throws IOException if the stream throws it.
   */
  public void _read_no_endian(AbstractCdrInput c)
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
        BufferredCdrInput profile = c.read_encapsulation();

        if (tag == Internet_profile.TAG_INTERNET_IOP)
          {
            Internet = new Internet_profile();
            Internet.version = Version.read_version(profile);
            Internet.host = profile.read_string();
            Internet.port = profile.gnu_read_ushort();

            key = profile.read_sequence();

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
                        Internet.CodeSets.read(profile);
                      }
                    else
                      {
                        // Construct a generic component for codesets
                        // profile.
                        TaggedComponent pc = new TaggedComponent();
                        pc.tag = ctag;
                        pc.component_data = profile.read_sequence();
                        Internet.components.add(pc);
                      }
                  }
              }
            catch (Unexpected ex)
              {
                ex.printStackTrace();
              }
          }
        else
          {
            // Construct a generic profile.
            TaggedProfile p = new TaggedProfile();
            p.tag = tag;
            p.profile_data = profile.buffer.getBuffer();

            profiles.add(p);
          }
      }
  }

  /**
   * Write this IOR record to the provided CDR stream. This procedure writes the
   * zero (Big Endian) marker first.
   */
  public void _write(AbstractCdrOutput out)
  {
    // Always use Big Endian.
    out.write(0);
    _write_no_endian(out);
  }

  /**
   * Write a null value to the CDR output stream.
   *
   * The null value is written as defined in OMG specification (zero length
   * string, followed by an empty set of profiles).
   */
  public static void write_null(AbstractCdrOutput out)
  {
    // Empty Id string.
    out.write_string("");

    // Empty set of profiles.
    out.write_long(0);
  }

  /**
   * Write this IOR record to the provided CDR stream. The procedure writed data
   * in Big Endian, but does NOT add any endian marker to the beginning.
   */
  public void _write_no_endian(AbstractCdrOutput out)
  {
    // Write repository id.
    out.write_string(Id);

    out.write_long(1 + profiles.size());

    // Write the Internet profile.
    out.write_long(Internet_profile.TAG_INTERNET_IOP);
    Internet.write(out);

    // Write other profiles.
    TaggedProfile tp;

    for (int i = 0; i < profiles.size(); i++)
      {
        tp = (TaggedProfile) profiles.get(i);
        TaggedProfileHelper.write(out, tp);
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
        b.append(Integer.toHexString(key[i] & 0xFF));
      }

    b.append(" ");
    b.append(Internet.CodeSets);

    return b.toString();
  }
  
  /**
   * Returns a multiline formatted human readable string representation of 
   * this IOR object.
   */
  public String toStringFormatted()
  {
    StringBuffer b = new StringBuffer();
    b.append("\nObject Id:\n  ");
    b.append(Id);
    b.append("\nObject is accessible at:\n  ");
    b.append(Internet);

    if (Big_Endian)
      b.append("\n  Big endian encoding");
    else
      b.append("\n  Little endian encoding.");      

    b.append("\nObject Key\n  ");

    for (int i = 0; i < key.length; i++)
      {
        b.append(Integer.toHexString(key[i] & 0xFF));
      }

    b.append("\nSupported code sets:");
    b.append("\n Wide:");
    b.append(Internet.CodeSets.wide.toStringFormatted());
    b.append(" Narrow:");
    b.append(Internet.CodeSets.wide.toStringFormatted());

    return b.toString();
  }  

  /**
   * Returs a stringified reference.
   *
   * @return a newly constructed stringified reference.
   */
  public String toStringifiedReference()
  {
    BufferedCdrOutput out = new BufferedCdrOutput();

    _write(out);

    StringBuffer b = new StringBuffer("IOR:");

    byte[] binary = out.buffer.toByteArray();
    String s;

    for (int i = 0; i < binary.length; i++)
      {
        s = Integer.toHexString(binary[i] & 0xFF);
        if (s.length() == 1)
          b.append('0');
        b.append(s);
      }

    return b.toString();
  }

  /**
   * Adds a service-specific component to the IOR profile. The specified
   * component will be included in all profiles, present in the IOR.
   *
   * @param tagged_component a tagged component being added.
   */
  public void add_ior_component(TaggedComponent tagged_component)
  {
    // Add to the Internet profile.
    Internet.components.add(tagged_component);

    // Add to others.
    for (int i = 0; i < profiles.size(); i++)
      {
        TaggedProfile profile = (TaggedProfile) profiles.get(i);
        addComponentTo(profile, tagged_component);
      }
  }

  /**
   * Adds a service-specific component to the IOR profile.
   *
   * @param tagged_component a tagged component being added.
   *
   * @param profile_id the IOR profile to that the component must be added. The
   * 0 value ({@link org.omg.IOP.TAG_INTERNET_IOP#value}) adds to the Internet
   * profile where host and port are stored by default.
   */
  public void add_ior_component_to_profile(TaggedComponent tagged_component,
                                           int profile_id)
  {
    if (profile_id == TAG_INTERNET_IOP.value)
      // Add to the Internet profile
      Internet.components.add(tagged_component);
    else
      {
        // Add to others.
        for (int i = 0; i < profiles.size(); i++)
          {
            TaggedProfile profile = (TaggedProfile) profiles.get(i);
            if (profile.tag == profile_id)
              addComponentTo(profile, tagged_component);
          }
      }
  }

  /**
   * Add given component to the given profile that is NOT an Internet profile.
   *
   * @param profile the profile, where the component should be added.
   * @param component the component to add.
   */
  private static void addComponentTo(TaggedProfile profile,
                                     TaggedComponent component)
  {
    if (profile.tag == TAG_MULTIPLE_COMPONENTS.value)
      {
        TaggedComponent[] present;
        if (profile.profile_data.length > 0)
          {
            BufferredCdrInput in = new BufferredCdrInput(profile.profile_data);

            present = new TaggedComponent[in.read_long()];

            for (int i = 0; i < present.length; i++)
              {
                present[i] = TaggedComponentHelper.read(in);
              }
          }
        else
          present = new TaggedComponent[0];

        BufferedCdrOutput out = new BufferedCdrOutput(profile.profile_data.length
                                            + component.component_data.length
                                            + 8);

        // Write new amount of components.
        out.write_long(present.length + 1);

        // Write other components.
        for (int i = 0; i < present.length; i++)
          TaggedComponentHelper.write(out, present[i]);

        // Write the passed component.
        TaggedComponentHelper.write(out, component);

        try
          {
            out.close();
          }
        catch (IOException e)
          {
            throw new Unexpected(e);
          }
        profile.profile_data = out.buffer.toByteArray();
      }
    else
      // The future supported tagged profiles should be added here.
      throw new BAD_PARAM("Unsupported profile type " + profile.tag);
  }
  
  /**
   * Checks for equality.
   */
  public boolean equals(Object x)
  {
    if (x instanceof IOR)
      {
        boolean keys;
        boolean hosts = true;

        IOR other = (IOR) x;
        
        if (Internet==null || other.Internet==null)
          return Internet == other.Internet;
        
        if (key != null && other.key != null)
          keys = Arrays.equals(key, other.key);
        else
          keys = key == other.key;

        if (Internet != null && Internet.host != null)
          if (other.Internet != null && other.Internet.host != null)
            hosts = other.Internet.host.equals(Internet.host);

        return keys & hosts && Internet.port==other.Internet.port;
      }
    else
      return false;
  }
  
  /**
   * Get the hashcode of this IOR.
   */
  public int hashCode()
  {
    Adler32 adler = new Adler32();
    if (key != null)
      adler.update(key);
    if (Internet != null)
      {
        if (Internet.host != null)
          adler.update(Internet.host.getBytes());
        adler.update(Internet.port);
      }
    return (int) adler.getValue();
  }
}