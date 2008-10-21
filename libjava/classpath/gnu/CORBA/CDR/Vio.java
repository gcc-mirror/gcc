/* Vio.java -- Value type IO operations.
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


package gnu.CORBA.CDR;

import gnu.CORBA.Minor;
import gnu.CORBA.ObjectCreator;

import gnu.java.lang.CPStringBuilder;

import org.omg.CORBA.CustomMarshal;
import org.omg.CORBA.DataInputStream;
import org.omg.CORBA.DataOutputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.StringValueHelper;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.WStringValueHelper;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;
import org.omg.CORBA.portable.ValueFactory;

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.StringTokenizer;

import javax.rmi.CORBA.Util;
import javax.rmi.CORBA.ValueHandler;

/**
 * A specialised class for reading and writing the value types.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class Vio
{
  /**
   * If true, wrap value type data into chunks. This decrease the performance,
   * and is not required for interoperability with jdk 1.5, but is left in the
   * implementation as the optional mode for solving possible interoperability
   * problems with non-Sun CORBA implementations.
   * 
   * The current implementation would accept both single chunk or multiple
   * chunks, but will always send a single chunk (if true) or unchunked data (if
   * false).
   */
  public static boolean USE_CHUNKING = false;

  /**
   * The first field in the value record. The last octet may contain additional
   * flags (vf_CODEBASE, vf_ID and vf_MULTIPLE_IDS). The tag value is different
   * for the indirections (vt_INDIRECTION) and nulls (vt_NULL).
   */
  public static final int vt_VALUE_TAG = 0x7fffff00;

  /**
   * The value tag flag, indicating that the codebase URL is present in the
   * value tag record.
   */
  public static final int vf_CODEBASE = 0x1;

  /**
   * The value tag flag, indicating that a single repository id is present in
   * the value tag record.
   */
  public static final int vf_ID = 0x2;

  /**
   * The value tag flag, indicating, that there are multiple repository ids
   * present in the record. If this flag is set, the flag vf_ID must also be
   * set, resulting the value of the least significant byte 0x6.
   */
  public static final int vf_MULTIPLE_IDS = 0x4;

  /**
   * The value tag flag, indicating the presence of chunking. Each chunk is
   * preceeded by a positive int, indicating the number of bytes in the chunk. A
   * sequence of chunks is terminated by a non positive int.
   */
  public static final int vf_CHUNKING = 0x8;

  /**
   * The indirection tag value. Such tag must be followed by the CORBA long,
   * indicating the offset in the CORBA message, where the indirected
   * information is present. This offset is assumed zero at the position where
   * the mentioned CORBA long starts and can refer both forward (positive
   * values) and backward (negative values).
   */
  public static final int vt_INDIRECTION = 0xffffffff;

  /**
   * This tag value means that the value object being transferred is equal to
   * null.
   */
  public static final int vt_NULL = 0x0;

  /**
   * The size of CORBA long (java int).
   */
  static final int INT_SIZE = 4;

  /**
   * The String value helper (one instance is sufficient).
   */
  public static final WStringValueHelper m_StringValueHelper = new WStringValueHelper();

  /**
   * An instance of the value handler.
   */
  static ValueHandler handler = Util.createValueHandler();

  /**
   * Read the value base from the given input stream. Determines the required
   * class from the repository id. This includes operations that are not
   * required when an unitialised instance or at least class of the value type
   * is known. Hence it may be faster to use the alternative methods,
   * read(InputStream, Class) or read(InputStream, Serializable).
   * 
   * @param input a stream to read from.
   * 
   * @return the loaded value.
   * 
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Serializable read(InputStream input)
  {
    return read(input, (String) null);
  }

  /**
   * Read the value base from the given input stream. Determines the required
   * class from the repository id. This includes operations that are not
   * required when an unitialised instance or at least class of the value type
   * is known. Hence it may be faster to use the alternative methods,
   * read(InputStream, Class) or read(InputStream, Serializable).
   * 
   * @param input a stream to read from.
   * @param repository_id a repository id of the object being read, may be null.
   * 
   * @return the loaded value.
   * 
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Serializable read(InputStream input, String repository_id)
  {
    try
      {
        final int position = getCurrentPosition(input);
        // We may need to jump back if the value is read via value factory.
        input.mark(512);

        int value_tag = input.read_long();
        checkTag(value_tag);

        String codebase = null;
        String[] ids = null;
        String id = repository_id;

        // Check for the agreed null value.
        if (value_tag == vt_NULL)
          return null;
        else if (value_tag == vt_INDIRECTION)
          return readIndirection(input);
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                // The codebase is present. The codebase is a space
                // separated list of URLs from where the implementing
                // code can be downloaded.
                codebase = read_string(input);
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present.
                ids = read_string_array(input);
              }
            else if ((value_tag & vf_ID) != 0)
              {
                // Single supported repository id is present.
                id = read_string(input);
              }
          }

        BoxedValueHelper helper = getHelper(null, id);
        // The existing implementing object.
        java.lang.Object ox = null;

        if (helper != null)
          ox = null; // Helper will care about the instantiating.
        else if (id.equals(WStringValueHelper.id()))
          helper = m_StringValueHelper;
        else
          ox = createInstance(id, ids, codebase);
        return (Serializable) read_instance(input, position, ox, value_tag,
          helper, id, ids, codebase);
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL();
        m.minor = Minor.Value;        
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Read the value base from the given input stream when the value base class
   * is available. Hence there is no need to guess it from the repository id.
   * 
   * @param input a stream to read from.
   * @param value_class the class of the value being read.
   * 
   * @return the loaded value.
   * 
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Serializable read(InputStream input, Class value_class)
  {
    final int position = getCurrentPosition(input);

    String id = null;
    String[] ids = null;
    String codebase = null;

    try
      {
        int value_tag = input.read_long();
        checkTag(value_tag);

        // Check for the agreed null value.
        if (value_tag == vt_NULL)
          return null;
        else if (value_tag == vt_INDIRECTION)
          return readIndirection(input);
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                // The codebase is present.
                codebase = read_string(input);
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present.
                ids = read_string_array(input);
              }
            else if ((value_tag & vf_ID) != 0)
              {
                // Single supported repository id is present.
                id = read_string(input);
              }
          }

        BoxedValueHelper vHelper = id != null ? getHelper(value_class, id)
          : getHelper(value_class, ids);

        java.lang.Object ox;

        if (vHelper == null)
          {
            try
              {
                ox = createInstance(id, ids, codebase);
              }
            catch (Exception e)
              {
                ox = null;
              }

            if (ox != null)
              {
                if (value_class != null
                  && !value_class.isAssignableFrom(ox.getClass()))
                  {
                    MARSHAL m = new MARSHAL(ox.getClass() + " is not a "
                    + value_class.getName());
                    m.minor = Minor.ClassCast;
                    throw m;
                  }
              }
          }
        else
          ox = null;

        ox = read_instance(input, position, ox, value_tag, vHelper, id, ids,
          codebase);
        return (Serializable) ox;
      }
    catch (MARSHAL m)
      {
        throw m;
      }
    catch (SystemException sysEx)
      {
        // OK.
        throw sysEx;
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL("Cant read " + value_class);
        m.minor = Minor.Value;
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Read the value base from the given input stream when the unitialised
   * instance is available. Hence there is no need to guess the class from the
   * repository id and then to instantiate an instance.
   * 
   * @param input a stream to read from.
   * 
   * @param value_instance an pre-created instance of the value. If the helper
   * is not null, this parameter is ignored an should be null.
   * 
   * @param helper a helper to create an instance and read the object- specific
   * part of the record. If the value_instance is used instead, this parameter
   * should be null.
   * 
   * @return the loaded value.
   * 
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Object read(InputStream input, Object value_instance,
    BoxedValueHelper helper)
  {
    final int position = getCurrentPosition(input);

    String id = null;
    String[] ids = null;
    String codebase = null;

    try
      {
        int value_tag = input.read_long();
        checkTag(value_tag);

        // Check for the agreed null value.
        if (value_tag == vt_NULL)
          return null;
        else if (value_tag == vt_INDIRECTION)
          return readIndirection(input);
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                // The codebase is present.
                codebase = read_string(input);
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present.
                ids = read_string_array(input);
              }
            else if ((value_tag & vf_ID) != 0)
              {
                // Single supported repository id is present.
                id = read_string(input);
              }
          }

        Class value_class = value_instance == null ? null
          : value_instance.getClass();

        if (helper == null)
          helper = id != null ? getHelper(value_class, id) : getHelper(
            value_class, ids);

        value_instance = read_instance(input, position, value_instance,
          value_tag, helper, id, ids, codebase);
        return value_instance;
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL();
        m.minor = Minor.Value;
        m.initCause(ex);
        throw m;
      }
  }

  /**
   * Read using provided boxed value helper. This method expects the full value
   * type header, followed by contents, that are delegated to the provided
   * helper. It handles null.
   * 
   * @param input the stream to read from.
   * @param helper the helper that reads the type-specific part of the content.
   * 
   * @return the value, created by the helper, or null if the header indicates
   * that null was previously written.
   */
  public static Serializable read(InputStream input, BoxedValueHelper helper)
  {
    return (Serializable) read(input, null, helper);
  }

  /**
   * Fill in the instance fields by the data from the input stream. The method
   * assumes that the value header, if any, is already behind. The information
   * from the stream is stored into the passed ox parameter.
   * 
   * @param input an input stream to read from.
   * 
   * @param value a pre-instantiated value type object, must be either
   * Streamable or CustomMarshal. If the helper is used, this parameter is
   * ignored and should be null.
   * 
   * @param value_tag the tag that must be read previously.
   * @param helper the helper for read object specific part; may be null to read
   * in using other methods.
   * 
   * @return the value that was read.
   */
  static Object read_instance(InputStream input, final int position,
    Object value, int value_tag, BoxedValueHelper helper, String id,
    String[] ids, String codebase)
  {
    if (helper != m_StringValueHelper && id != null)
      if (id.equals(StringValueHelper.id()))
        {
          value = null;
          helper = m_StringValueHelper;
        }

    try
      {
        if ((value_tag & vf_CHUNKING) != 0)
          {
            BufferedCdrOutput output = createBuffer(input, 1024);
            // Read the current (not a nested one) value in this spec case.
            readNestedValue(value_tag, input, output, -1);
            BufferredCdrInput ci = new BufferredCdrInput(output.buffer.getBuffer());
            ci.setRunTime(output.getRunTime());

            input = new HeadlessInput(ci, input);
          }
        else
          {
            if (input instanceof BufferredCdrInput)
              {
                // Highly probable case.
                input = new HeadlessInput((BufferredCdrInput) input, null);
              }
            else if (input instanceof HeadlessInput)
              {
                // There is no need to instantiate one more HeadlessInput
                // as we can just reset.
                ((HeadlessInput) input).subsequentCalls = false;
              }
            else
              {
                BufferedCdrOutput bout = new BufferedCdrOutput();
                int c;
                while ((c = input.read()) >= 0)
                  bout.write((byte) c);
                input = new HeadlessInput(
                  (BufferredCdrInput) bout.create_input_stream(), input);
              }
          }
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL("Unable to read chunks");
        m.minor = Minor.Value;
        m.initCause(ex);
        throw m;
      }

    return readValue(input, position, value, helper, id, ids, codebase);
  }

  /**
   * Create a buffer, inheriting critical settings from the passed input stream.
   */
  private static BufferedCdrOutput createBuffer(InputStream input, int proposed_size)
  {
    BufferedCdrOutput bout;
    bout = new BufferedCdrOutput(2 * proposed_size + 256);

    if (input instanceof BufferredCdrInput)
      {
        BufferredCdrInput in = (BufferredCdrInput) input;
        bout.setBigEndian(in.isBigEndian());
      }

    if (input instanceof gnuValueStream)
      bout.setRunTime(((gnuValueStream) input).getRunTime());
    else
      bout.setRunTime(new gnuRuntime(null, null));
    return bout;
  }

  /**
   * Read the chunked nested value from the given input stream, transferring the
   * contents to the given output stream.
   * 
   * @param value_tag the value tag of the value being read.
   * @param input the input stream from where the remainder of the nested value
   * must be read.
   * @param output the output stream where the unchunked nested value must be
   * copied.
   * 
   * @return the tag that ended the nested value.
   */
  public static int readNestedValue(int value_tag, InputStream input,
    BufferedCdrOutput output, int level)
    throws IOException
  {
    String id = null;
    if (level < -1)
      {
        // For the first level, this information is already behind.
        output.write_long(value_tag - vf_CHUNKING);

        // The nested value should be aways chunked.
        if ((value_tag & vf_CHUNKING) == 0)
          {
            MARSHAL m = new MARSHAL("readNestedValue: must be chunked");
            m.minor = Minor.Chunks;
            throw m;
          }
        else if (value_tag == vt_NULL)
          {
            MARSHAL m = new MARSHAL("readNestedValue: nul");
            m.minor = Minor.Chunks;
            throw m;
          }
        else if (value_tag == vt_INDIRECTION)
          {
            MARSHAL m = new MARSHAL("readNestedValue: indirection");
            m.minor = Minor.Chunks;
            throw m;
          }
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                String codebase = read_string(input);
                write_string(output, codebase);
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present.
                String[] ids = read_string_array(input);
                id = ids[0];
                write_string_array(output, ids);
              }
            else if ((value_tag & vf_ID) != 0)
              {
                id = read_string(input);
                write_string(output, id);
              }
          }
      }

    int n = -1;

    // Read all chunks.
    int chunk_size;

    byte[] r = null;

    while (true)
      {
        // Read the size of the next chunk or it may also be the
        // header of the nested value.
        chunk_size = input.read_long();

        // End of chunk terminator.
        if (chunk_size < 0 && chunk_size >= level)
          return chunk_size;
        else if (chunk_size >= 0x7FFFFF00)
          {
            int onInput = getCurrentPosition(input) - 4;
            int onOutput = output.getPosition();
            output.getRunTime().redirect(onInput, onOutput);
            // Value over 0x7FFFFF00 indicates that the nested value
            // starts here. Read the nested value, storing it into the output.
            // First parameter is actually the value tag.
            chunk_size = readNestedValue(chunk_size, input, output, level - 1);
            if (chunk_size < 0 && chunk_size >= level)
              return chunk_size;
          }
        else
          {
            // The chunk follows.
            if (r == null || r.length < chunk_size)
              r = new byte[chunk_size + 256];

            n = 0;
            while (n < chunk_size)
              n += input.read(r, n, chunk_size - n);
            output.write(r, 0, n);
          }
      }
  }

  /**
   * Read the value (the header must be behind).
   */
  public static Serializable readValue(InputStream input, final int position,
    Object value, BoxedValueHelper helper, String id, String[] ids,
    String codebase)
  {
    gnuRuntime g;
    gnuValueStream c = ((gnuValueStream) input);
    if (c.getRunTime() == null)
      {
        g = new gnuRuntime(codebase, value);
        c.setRunTime(g);
      }
    else
      {
        g = c.getRunTime();
        g.addCodeBase(codebase);
        g.target = (Serializable) value;
      }
    if (value != null)
      g.objectWritten(value, position);

    if (input instanceof HeadlessInput)
      ((HeadlessInput) input).subsequentCalls = false;

    boolean ok = true;

    // The user-defined io operations are implemented.
    if (value instanceof CustomMarshal)
      {
        CustomMarshal marsh = (CustomMarshal) value;
        marsh.unmarshal((DataInputStream) input);
      }
    else
    // The IDL-generated io operations are implemented.
    if (value instanceof Streamable)
      {
        ((Streamable) value)._read(input);
      }
    else if (helper != null)
      {
        // If helper is non-null the value should normally be null.
        value = helper.read_value(input);
        g.objectWritten(value, position);
      }
    else
      {
        ok = false;
        ValueFactory factory = null;
        org.omg.CORBA_2_3.ORB orb = (org.omg.CORBA_2_3.ORB) input.orb();

        if (id != null)
          factory = orb.lookup_value_factory(id);

        if (factory == null && ids != null)
          {
            for (int i = 0; i < ids.length && factory == null; i++)
              {
                factory = orb.lookup_value_factory(ids[i]);
              }
          }

        if (factory != null)
          {
            value = factory.read_value((org.omg.CORBA_2_3.portable.InputStream) input);
            ok = true;
          }
      }

    if (!ok && value instanceof Serializable)
    // Delegate to ValueHandler
      {
        if (ids != null && ids.length > 0)
          id = ids[0];

        value = handler.readValue(input, position, value.getClass(), id, g);
        ok = true;
      }

    if (!ok)
      {
        if (value != null)
          {
            MARSHAL m = new MARSHAL(value.getClass().getName()
            + " must be Streamable, CustomMarshal or Serializable");
            m.minor = Minor.UnsupportedValue;
            throw m;
          }
        else
          {
            MARSHAL m = new MARSHAL("Unable to instantiate " + id + ":" + list(ids)
            + " helper " + helper);
            m.minor = Minor.UnsupportedValue;
            throw m;
          }
      }
    else
      return (Serializable) value;
  }

  /**
   * Conveniency method to list ids in exception reports.
   */
  static String list(String[] s)
  {
    if (s == null)
      return "null";
    else
      {
        CPStringBuilder b = new CPStringBuilder("{");
        for (int i = 0; i < s.length; i++)
          {
            b.append(s[i]);
            b.append(" ");
          }
        b.append("}");
        return b.toString();
      }
  }

  /**
   * Write the value base into the given stream.
   * 
   * @param output a stream to write to.
   * 
   * @param value a value type object, must be either Streamable or
   * CustomMarshal.
   * 
   * @throws MARSHAL if the writing failed due any reason.
   */
  public static void write(OutputStream output, Serializable value)
  {
    // Write null if this is a null value.
    if (value == null)
      output.write_long(vt_NULL);
    else if (value instanceof String)
      write(output, value, m_StringValueHelper);
    else
      write(output, value, value.getClass());
  }

  /**
   * Write the value base into the given stream, stating that it is an instance
   * of the given class.
   * 
   * @param output a stream to write to.
   * 
   * @param value a value to write.
   * 
   * @throws MARSHAL if the writing failed due any reason.
   */
  public static void write(OutputStream output, Serializable value,
    Class substitute)
  {
    // Write null if this is a null value.
    if (value == null)
      output.write_long(vt_NULL);
    else if (value instanceof String || substitute == String.class)
      writeString(output, value);
    else
      {
        String vId = ObjectCreator.getRepositoryId(value.getClass());
        if (substitute == null || value.getClass().equals(substitute))
          write_instance(output, value, vId, getHelper(value.getClass(), vId));
        else
          {
            String vC = ObjectCreator.getRepositoryId(substitute);
            String[] ids = new String[] { vId, vC };
            BoxedValueHelper h = getHelper(substitute.getClass(), ids);
            // If the helper is available, it is also responsible for
            // providing the repository Id. Otherwise, write both
            // ids.
            if (h == null)
              write_instance(output, value, ids, null);
            else
              write_instance(output, value, h.get_id(), null);
          }
      }
  }

  /**
   * Write the value base into the given stream, supplementing it with an array
   * of the provided repository ids plus the repository id, derived from the
   * passed value.
   * 
   * @param output a stream to write to.
   * 
   * @param value a value to write.
   * 
   * @throws MARSHAL if the writing failed due any reason.
   */
  public static void write(OutputStream output, Serializable value,
    String[] multiple_ids)
  {
    // Write null if this is a null value.
    if (value == null)
      output.write_long(vt_NULL);
    else
      {
        String[] ids = new String[multiple_ids.length + 1];
        ids[0] = ObjectCreator.getRepositoryId(value.getClass());
        System.arraycopy(multiple_ids, 0, ids, 1, multiple_ids.length);
        BoxedValueHelper h = getHelper(value.getClass(), ids);
        write_instance(output, value, ids, h);
      }
  }

  /**
   * Write value when its repository Id is explicitly given. Only this Id is
   * written, the type of value is not taken into consideration.
   * 
   * @param output an output stream to write into.
   * @param value a value to write.
   * @param id a value repository id.
   */
  public static void write(OutputStream output, Serializable value, String id)
  {
    if (value == null)
      output.write_long(vt_NULL);
    else
      write_instance(output, value, id, getHelper(value.getClass(), id));
  }

  /**
   * Write standard value type header, followed by contents, produced by the
   * boxed value helper.
   * 
   * @param output the stream to write to.
   * @param value the value to write, can be null.
   * @param helper the helper that writes the value content if it is not null
   * (must be provided for this method).
   */
  public static void write(OutputStream output, Serializable value,
    BoxedValueHelper helper)
  {
    if (helper == null)
      throw new AssertionError("Helper must be provided");
    if (value == null)
      output.write_long(vt_NULL);
    else
      write_instance(output, value, helper.get_id(), helper);
  }

  /**
   * Write the parameter that is surely a string and not null.
   */
  private static void writeString(OutputStream output, Serializable string)
  {
    write_instance(output, string, m_StringValueHelper.get_id(),
      m_StringValueHelper);
  }

  /**
   * Write value when its repository Id is explicitly given. Does not handle
   * null.
   * 
   * @param output an output stream to write into.
   * @param value a value to write.
   * @param ids a value repository id (can be either single string or string
   * array).
   * @param helper a helper, writing object - specifical part. Can be null if
   * the value should be written using other methods.
   */
  static void write_instance(OutputStream output, Serializable value,
    Object ids, BoxedValueHelper helper)
  {
    gnuValueStream rout = null;
    gnuRuntime runtime = null;

    try
      {
        if (output instanceof gnuValueStream)
          {
            int position;
            rout = (gnuValueStream) output;
            runtime = rout.getRunTime();

            if (runtime == null)
              {
                runtime = new gnuRuntime(null, value);
                rout.setRunTime(runtime);
                rout.getRunTime().objectWritten(value,
                  position = rout.getPosition());
              }
            else if (runtime.target == value)
              {
                if (!writeSelf(output, value))
                  throw new InternalError("Recursive helper call for "
                    + value.getClass().getName());
                return;
              }
            else
              {
                position = runtime.isWrittenAt(value);
                if (position >= 0)
                  {
                    // The object was already written.
                    output.write_long(vt_INDIRECTION);
                    output.write_long(position - rout.getPosition());
                    // Replacing object write data by indirection reference.
                    return;
                  }
                else
                  {
                    runtime.objectWritten(value, position = rout.getPosition());
                  }
              }
          }

        int value_tag = vt_VALUE_TAG;

        if (ids instanceof String)
          value_tag |= vf_ID;
        else if (ids instanceof String[])
          // OMG standard requires to set both flags.
          value_tag |= vf_MULTIPLE_IDS | vf_ID;

        int chunkSizeLocation;

        OutputStream outObj;

        if (USE_CHUNKING)
          {
            // Wrap the value being written into one chunk (makes sense only for
            // compatibility reasons).
            outObj = output;
            value_tag |= vf_CHUNKING;
          }
        else
          outObj = output;

        output.write_long(value_tag);

        if ((value_tag & vf_MULTIPLE_IDS) != 0)
          write_string_array(output, (String[]) ids);
        else if ((value_tag & vf_ID) != 0)
          write_string(output, (String) ids);

        if (USE_CHUNKING)
          {
            // So far, write 0x55555555 instead of the chunk size (alignment may
            // take place).
            output.write_long(0x55555555);
            // If the chunking is involved, the chunk size must be written here.
            chunkSizeLocation = rout.getPosition() - INT_SIZE;
          }
        else
          // Not in use for this case.
          chunkSizeLocation = -1;

        writeValue(outObj, value, helper);

        if (USE_CHUNKING)
          {
            // Write the chunk size where the place for it was reserved.
            int chunkSize = rout.getPosition() - chunkSizeLocation - INT_SIZE;
            int current = rout.getPosition();
            rout.seek(chunkSizeLocation);
            output.write_long(chunkSize);
            rout.seek(current);

            // The end of record marker.
            output.write_long(-1);
          }
      }
    finally
      {
        if (runtime != null)
          runtime.target = null;
      }
  }

  /**
   * Write value (after header).
   */
  static void writeValue(OutputStream output, Serializable value,
    BoxedValueHelper helper)
  {
    ((gnuValueStream) output).getRunTime().target = value;
    if (helper != null)
      helper.write_value(output, value);
    else if (!writeSelf(output, value))
      {
        // Try to find helper via class loader.
        boolean ok = false;

        if (!ok)
          {
            if (output instanceof BufferedCdrOutput)
              {
                BufferedCdrOutput b = (BufferedCdrOutput) output;
                if (b.runtime == null)
                  b.runtime = new gnuRuntime(null, value);
              }

            handler.writeValue(output, value);
          }
      }
  }

  /**
   * Try to write value supposing that it implements self-streamable interfaces.
   * Return false if it does not or true on success.
   */
  static boolean writeSelf(OutputStream output, Serializable value)
  {
    // User defined write method is present.
    if (value instanceof CustomMarshal)
      {
        ((CustomMarshal) value).marshal((DataOutputStream) output);
        return true;
      }
    else if (value instanceof Streamable)
      {
        ((Streamable) value)._write(output);
        return true;
      }
    return false;
  }

  /**
   * Read the indirection data and return the object that was already written to
   * this stream.
   * 
   * @param an_input the input stream, must be BufferredCdrInput.
   */
  static Serializable readIndirection(InputStream an_input)
  {
    if (!(an_input instanceof gnuValueStream))
      throw new NO_IMPLEMENT(gnuValueStream.class.getName()
        + " expected as parameter");

    gnuValueStream in = (gnuValueStream) an_input;

    int current_pos = in.getPosition();

    int offset = an_input.read_long();
    if (offset > -INT_SIZE)
      {
        MARSHAL m = new MARSHAL("Indirection tag refers to " + offset
        + " (must be less than -" + INT_SIZE + ")");
        m.minor = Minor.Offset;
        throw m;
      }

    int stored_at = current_pos + offset;

    if (in.getRunTime() == null)
      {
        MARSHAL m = new MARSHAL(stored_at + " offset " + offset + ": not written");
        m.minor = Minor.Value;
        throw m;
      }

    return (Serializable) in.getRunTime().isObjectWrittenAt(stored_at, offset);
  }

  /**
   * Check the passed value tag for correctness.
   * 
   * @param value_tag a tag to check, must be between 0x7fffff00 and 0x7fffffff
   * 
   * @throws MARSHAL if the tag is outside this interval.
   */
  static void checkTag(int value_tag)
  {
    if ((value_tag < 0x7fffff00 || value_tag > 0x7fffffff)
      && value_tag != vt_NULL && value_tag != vt_INDIRECTION)
      {
        MARSHAL m = new MARSHAL("Invalid value record, unsupported header tag: "
        + value_tag + " (0x" + Integer.toHexString(value_tag) + ")");
        m.minor = Minor.ValueHeaderTag;
        throw m;
      }

    if ((value_tag & vf_MULTIPLE_IDS) != 0 && (value_tag & vf_ID) == 0)
      {
        MARSHAL m = new MARSHAL("Invalid value record header flag combination (0x"
        + Integer.toHexString(value_tag) + ")");
        m.minor = Minor.ValueHeaderFlags;
        throw m;
      }
  }

  /**
   * Throw MARSHAL.
   */
  static void throwIt(String msg, String id1, String id2, Throwable e)
    throws MARSHAL
  {
    MARSHAL m = new MARSHAL(msg + ":'" + id1 + "' versus '" + id2 + "'");
    if (e != null)
      m.initCause(e);
    m.minor = Minor.Value;
    throw m;
  }

  /**
   * Load class by name and create the instance.
   */
  static Object createInstance(String id, String[] ids, String codebase)
  {
    Object o = null;

    if (id != null)
      o = _createInstance(id, codebase);

    if (ids != null)
      for (int i = 0; i < ids.length && o == null; i++)
        o = _createInstance(ids[i], codebase);
    return o;
  }

  static Object _createInstance(String id, String codebase)
  {
    if (id == null)
      return null;
    if (id.equals(StringValueHelper.id()))
      return "";
    StringTokenizer st = new StringTokenizer(id, ":");

    String prefix = st.nextToken();
    if (prefix.equalsIgnoreCase("IDL"))
      return ObjectCreator.Idl2Object(id);
    else if (prefix.equalsIgnoreCase("RMI"))
      {
        String className = st.nextToken();
        String hashCode = st.nextToken();
        String sid = null;
        if (st.hasMoreElements())
          sid = st.nextToken();

        try
          {
            Class objectClass = Util.loadClass(className, codebase,
              Vio.class.getClassLoader());

            String rid = ObjectCreator.getRepositoryId(objectClass);

            if (!rid.equals(id))
              {
                // If direct string comparison fails, compare by meaning.
                StringTokenizer st2 = new StringTokenizer(rid, ":");
                if (!st2.nextToken().equals("RMI"))
                  throw new InternalError("RMI format expected: '" + rid + "'");
                if (!st2.nextToken().equals(className))
                  throwIt("Class name mismatch", id, rid, null);

                try
                  {
                    long h1 = Long.parseLong(hashCode, 16);
                    long h2 = Long.parseLong(st2.nextToken(), 16);
                    if (h1 != h2)
                      throwIt("Hashcode mismatch", id, rid, null);

                    if (sid != null && st2.hasMoreTokens())
                      {
                        long s1 = Long.parseLong(hashCode, 16);
                        long s2 = Long.parseLong(st2.nextToken(), 16);
                        if (s1 != s2)
                          throwIt("serialVersionUID mismatch", id, rid, null);
                      }
                  }
                catch (NumberFormatException e)
                  {
                    throwIt("Invalid hashcode or svuid format: ", id, rid, e);
                  }
              }

            // Low - level instantiation required here.
            return instantiateAnyWay(objectClass);
          }
        catch (Exception ex)
          {
            MARSHAL m = new MARSHAL("Unable to instantiate " + id);
            m.minor = Minor.Instantiation;
            m.initCause(ex);
            throw m;
          }
      }
    else
      throw new NO_IMPLEMENT("Unsupported prefix " + prefix + ":");
  }

  /**
   * Read string, expecting the probable indirection.
   */
  static String read_string(InputStream input)
  {
    gnuValueStream g = (gnuValueStream) input;
    int previous = g.getPosition();
    int l = input.read_long();
    if (l != vt_INDIRECTION)
      {
        g.seek(previous);
        String s = input.read_string();
        if (g.getRunTime() == null)
          g.setRunTime(new gnuRuntime(null, null));
        g.getRunTime().singleIdWritten(s, previous);
        return s;
      }
    else
      {
        gnuRuntime r = g.getRunTime();
        int base = g.getPosition();
        int delta = input.read_long();
        if (r == null)
          {
            previous = g.getPosition();
            g.seek(base + delta);
            String indir = input.read_string();
            g.seek(previous);
            return indir;
          }
        else
          {
            return (String) r.isObjectWrittenAt(base + delta, delta);
          }
      }
  }

  /**
   * Read string array, expecting the probable indirection.
   */
  static String[] read_string_array(InputStream input)
  {
    gnuValueStream g = (gnuValueStream) input;
    int previous = g.getPosition();
    int l = input.read_long();
    if (l != vt_INDIRECTION)
      {
        g.seek(previous);
        String[] s = StringSeqHelper.read(input);
        if (g.getRunTime() == null)
          g.setRunTime(new gnuRuntime(null, null));
        g.getRunTime().objectWritten(s, previous);
        return s;
      }
    else
      {
        gnuRuntime r = g.getRunTime();
        int base = g.getPosition();
        int delta = input.read_long();
        if (r == null)
          {
            previous = g.getPosition();
            g.seek(base + delta);
            String[] indir = StringSeqHelper.read(input);
            g.seek(previous);
            return indir;
          }
        else
          {
            return (String[]) r.isObjectWrittenAt(base + delta, delta);
          }
      }
  }

  /**
   * Write repository Id, probably shared.
   */
  static void write_string(OutputStream output, String id)
  {
    if (output instanceof gnuValueStream)
      {
        gnuValueStream b = (gnuValueStream) output;
        if (b != null)
          {
            int written = b.getRunTime().idWrittenAt(id);
            if (written >= 0)
              {
                // Reuse existing id record.
                output.write_long(vt_INDIRECTION);
                int p = b.getPosition();
                output.write_long(written - p);
              }
            else
              {
                b.getRunTime().singleIdWritten(id, b.getPosition());
                output.write_string(id);
              }
          }
      }
    else
      output.write_string(id);
  }

  /**
   * Write repository Id, probably shared.
   */
  static void write_string_array(OutputStream output, String[] ids)
  {
    if (output instanceof gnuValueStream)
      {
        gnuValueStream b = (gnuValueStream) output;
        if (b != null)
          {
            int written = b.getRunTime().idWrittenAt(ids);
            if (written >= 0)
              {
                // Reuse existing id record.
                output.write_long(vt_INDIRECTION);
                int p = b.getPosition();
                output.write_long(written - p);
              }
            else
              {
                b.getRunTime().multipleIdsWritten(ids, b.getPosition());
                StringSeqHelper.write(output, ids);
              }
          }
      }
    else
      StringSeqHelper.write(output, ids);
  }

  /**
   * Get the helper that could write the given object, or null if no pre-defined
   * helper available for this object.
   */
  public static BoxedValueHelper getHelper(Class x, Object ids)
  {
    if (x != null && x.equals(String.class))
      return m_StringValueHelper;
    else if (x != null && x.isArray())
      return new ArrayValueHelper(x);
    else if (ids instanceof String)
      return locateHelper((String) ids);
    else if (ids instanceof String[])
      {
        String[] ia = (String[]) ids;
        BoxedValueHelper h;
        for (int i = 0; i < ia.length; i++)
          {
            h = locateHelper(ia[i]);
            if (h != null)
              return h;
          }
        return null;
      }
    else
      return null;
  }

  /**
   * Get the helper that could write the given object, or null if no pre-defined
   * helper available for this object.
   */
  public static BoxedValueHelper getHelper(Class x, String id)
  {
    if (x != null && x.equals(String.class))
      return m_StringValueHelper;
    else if (x != null && x.isArray())
      return new ArrayValueHelper(x);
    else
      return locateHelper(id);
  }

  /**
   * Try to locate helper from the repository id.
   */
  static BoxedValueHelper locateHelper(String id)
  {
    if (id != null)
      {
        if (id.equals(m_StringValueHelper.get_id()))
          return m_StringValueHelper;
        else
        // Try to locate helper for IDL type.
        if (id.startsWith("IDL:"))
          {
            try
              {
                Class helperClass = ObjectCreator.findHelper(id);
                if (BoxedValueHelper.class.isAssignableFrom(helperClass))
                  return (BoxedValueHelper) helperClass.newInstance();
                else if (helperClass != null)
                  return new IDLTypeHelper(helperClass);
                else
                  return null;
              }
            catch (Exception ex)
              {
                return null;
              }
          }
      }
    return null;
  }

  /**
   * Get the current position.
   */
  static int getCurrentPosition(InputStream x)
  {
    if (x instanceof gnuValueStream)
      return ((gnuValueStream) x).getPosition();
    else
      return 0;
  }

  /**
   * Instantiate an instance of this class anyway; also in the case when it has
   * no parameterless or any other constructor. The fields will be assigned
   * while reading the class from the stream.
   * 
   * @param clazz a class for that the instance should be instantiated.
   */
  public static Object instantiateAnyWay(Class clazz)
    throws Exception
  {
    Class first_nonserial = clazz;

    while (Serializable.class.isAssignableFrom(first_nonserial)
      || Modifier.isAbstract(first_nonserial.getModifiers()))
      first_nonserial = first_nonserial.getSuperclass();

    final Class local_constructor_class = first_nonserial;

    Constructor constructor = local_constructor_class.getDeclaredConstructor(new Class[0]);

    return VMVio.allocateObject(clazz, constructor.getDeclaringClass(),
      constructor);
  }
}
