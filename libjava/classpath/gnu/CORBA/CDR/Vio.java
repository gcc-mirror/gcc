/* gnuValueBaseHelper.java --
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

import gnu.CORBA.ObjectCreator;

import org.omg.CORBA.CustomMarshal;
import org.omg.CORBA.DataInputStream;
import org.omg.CORBA.DataOutputStream;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;
import org.omg.CORBA.portable.ValueFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;

import java.lang.reflect.Method;

/**
 * A specialised class for reading and writing the value types.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class Vio
{
  /**
   * If true, wrap value type data into chunks. This decrease the
   * performance, but is required for the interoperability with
   * Sun's CORBA implementation. Chunking may increase the security,
   * as there is more control on the number of bytes being transferred.
   *
   * The current implementation would accept both single chunk or multiple
   * chunks, but will always send a single chunk.
   */
  public static boolean USE_CHUNKING = true;

  /**
   * The first field in the value record. The last octet may contain
   * additional flags (vf_CODEBASE, vf_ID and vf_MULTIPLE_IDS). The tag
   * value is different for the indirections (vt_INDIRECTION) and
   * nulls (vt_NULL).
   */
  public static final int vt_VALUE_TAG = 0x7fffff00;

  /**
   * The value tag flag, indicating that the codebase URL is present
   * in the value tag record.
   */
  public static final int vf_CODEBASE = 0x1;

  /**
   * The value tag flag, indicating that a single repository id is present
   * in the value tag record.
   */
  public static final int vf_ID = 0x2;

  /**
   * The value tag flag, indicating, that there are multiple repository
   * ids present in the record. If this flag is set, the flag vf_ID must
   * also be set, resulting the value of the least significant byte 0x6.
   */
  public static final int vf_MULTIPLE_IDS = 0x4;

  /**
   * The value tag flag, indicating the presence of chunking. Each chunk is
   * preceeded by a positive int, indicating the number of bytes in the chunk.
   * A sequence of chunks is terminated by a non positive int.
   */
  public static final int vf_CHUNKING = 0x8;

  /**
   * The indirection tag value. Such tag must be followed by the CORBA long,
   * indicating the offset in the CORBA message, where the indirected
   * information is present. This offset is assumed zero at the position
   * where the mentioned CORBA long starts and can refer both forward
   * (positive values) and backward (negative values).
   */
  public static final int vt_INDIRECTION = 0xffffffff;

  /**
   * This tag value means that the value object being transferred is equal
   * to null.
   */
  public static final int vt_NULL = 0x0;

  /**
   * Read the value base from the given input stream. Determines the
   * required class from the repository id. This includes operations
   * that are not required when an unitialised instance or at least
   * class of the value type is known. Hence it may be faster to use
   * the alternative methods, read(InputStream, Class) or
   * read(InputStream, Serializable).
   *
   * @param input a stream to read from.
   *
   * @return the loaded value.
   *
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Serializable read(InputStream input)
  {
    // Explicitly prevent the stream from closing as we may need
    // to read the subsequent bytes as well. Stream may be auto-closed
    // in its finalizer.
    try
      {
        // We may need to jump back if the value is read via value factory.
        input.mark(512);

        int value_tag = input.read_long();
        checkTag(value_tag);

        String codebase = null;
        String[] ids = null;
        String id = null;

        // The existing implementing object.
        java.lang.Object ox = null;

        // Check for the agreed null value.
        if (value_tag == vt_NULL)
          return null;
        else if (value_tag == vt_INDIRECTION)

          // TODO FIXME Implement support for indirections.
          throw new NO_IMPLEMENT("Indirections unsupported");
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                // The codebase is present. The codebase is a space
                // separated list of URLs from where the implementing
                // code can be downloaded.
                codebase = input.read_string();
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present.
                ids = StringSeqHelper.read(input);
                for (int i = 0; (i < ids.length) && (ox == null); i++)
                  {
                    ox = ObjectCreator.Idl2Object(ids [ i ]);

                    if (ox == null)
                      {
                        // Try to find the value factory.
                        ValueFactory f =
                          ((org.omg.CORBA_2_3.ORB) input.orb()).lookup_value_factory(ids [ i ]);

                        if (f != null)
                          {
                            // Reset, as the value factory reads from beginning.
                            input.reset();
                            return f.read_value((org.omg.CORBA_2_3.portable.InputStream) input);
                          }
                      }
                  }
              }
            else if ((value_tag & vf_ID) != 0)
              {
                // Single supported repository id is present.
                id = input.read_string();
                ox = ObjectCreator.Idl2Object(id);

                if (ox == null)
                  {
                    // Try to find the value factory.
                    ValueFactory f =
                      ((org.omg.CORBA_2_3.ORB) input.orb()).lookup_value_factory(id);

                    if (f != null)
                      {
                        input.reset();
                        return f.read_value((org.omg.CORBA_2_3.portable.InputStream) input);
                      }
                  }
              }
          }

        if (ox == null)
          throw new MARSHAL("Unable to instantiate the value type");
        else
          {
            read_instance(input, ox, value_tag, null);
            return (Serializable) ox;
          }
      }
    catch (Exception ex)
      {
        throw new MARSHAL(ex + ":" + ex.getMessage());
      }
  }

  /**
   * Read the value base from the given input stream when
   * the value base class is available. Hence there is no need
   * to guess it from the repository id.
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
    // Explicitly prevent the stream from closing as we may need
    // to read the subsequent bytes as well. Stream may be auto-closed
    // in its finalizer.
    try
      {
        int value_tag = input.read_long();
        checkTag(value_tag);

        // The existing implementing object.
        java.lang.Object ox = value_class.newInstance();

        // Check for the agreed null value.
        if (value_tag == vt_NULL)
          return null;
        else if (value_tag == vt_INDIRECTION)

          // TODO FIXME Implement support for indirections.
          throw new NO_IMPLEMENT("Indirections unsupported");
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                // The codebase is present, but skip it.
                input.read_string();
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present, but skip them.
                StringSeqHelper.read(input);
              }
            else if ((value_tag & vf_ID) != 0)
              {
                // Single supported repository id is present, but skip it.
                input.read_string();
              }
          }

        read_instance(input, ox, value_tag, null);
        return (Serializable) ox;
      }
    catch (Exception ex)
      {
        throw new MARSHAL(ex + ":" + ex.getMessage());
      }
  }

  /**
   * Read the value base from the given input stream when
   * the unitialised instance is available. Hence there is no need
   * to guess the class from the repository id and then to instantiate
   * an instance.
   *
   * @param input a stream to read from.
   *
   * @param value_instance an pre-created instance of the value. If the
   * helper is not null, this parameter is ignored an should be null.
   *
   * @param helper a helper to create an instance and read the object-
   * specific part of the record. If the value_instance is used instead,
   * this parameter should be null.
   *
   * @return the loaded value.
   *
   * @throws MARSHAL if the reading has failed due any reason.
   */
  public static Object read(InputStream input, Object value_instance,
    Object helper
  )
  {
    try
      {
        int value_tag = input.read_long();
        checkTag(value_tag);

        // Check for the agreed null value.
        if (value_tag == vt_NULL)
          return null;
        else if (value_tag == vt_INDIRECTION)

          // TODO FIXME Implement support for indirections.
          throw new NO_IMPLEMENT("Indirections unsupported");
        else
          {
            // Read the value.
            if ((value_tag & vf_CODEBASE) != 0)
              {
                // The codebase is present, but skip it.
                input.read_string();
              }

            if ((value_tag & vf_MULTIPLE_IDS) != 0)
              {
                // Multiple supported repository ids are present, but skip them.
                StringSeqHelper.read(input);
              }
            else if ((value_tag & vf_ID) != 0)
              {
                // Single supported repository id is present, but skip it.
                input.read_string();
              }
          }

        value_instance =
          read_instance(input, value_instance, value_tag, helper);
        return value_instance;
      }
    catch (Exception ex)
      {
        throw new MARSHAL(ex + ":" + ex.getMessage());
      }
  }

  /**
   * Read using provided boxed value helper. This method expects
   * the full value type header, followed by contents, that are
   * delegated to the provided helper. It handles null.
   *
   * @param input the stream to read from.
   * @param helper the helper that reads the type-specific part of
   * the content.
   *
   * @return the value, created by the helper, or null if the
   * header indicates that null was previously written.
   */
  public static Serializable read(InputStream input, Object helper)
  {
    return (Serializable) read(input, null, helper);
  }

  /**
   * Fill in the instance fields by the data from the input stream.
   * The method assumes that the value header, if any, is already
   * behind. The information from the stream is stored into the
   * passed ox parameter.
   *
   * @param input an input stream to read from.
   *
   * @param value a pre-instantiated value type object, must be either
   * Streamable or CustomMarshal. If the helper is used, this parameter
   * is ignored and should be null.
   *
   * @param value_tag the tag that must be read previously.
   * @param helper the helper for read object specific part; may be
   * null to read in using other methods.
   *
   * @return the value that was read.
   */
  private static Object read_instance(InputStream input, Object value,
    int value_tag, Object helper
  )
  {
    try
      {
        if ((value_tag & vf_CHUNKING) != 0)
          {
            ByteArrayOutputStream bout = null;
            int n = -1;

            // Read all chunks.
            int chunk_size = input.read_long();
            if (chunk_size < 0)
              throw new MARSHAL("Invalid first chunk size " + chunk_size);

            byte[] r = new byte[ chunk_size ];

            while (chunk_size > 0)
              {
                if (r.length < chunk_size)
                  r = new byte[ chunk_size + 256 ];

                n = 0;
                reading:
                while (n < chunk_size)
                  n += input.read(r, n, r.length - n);

                // Read the size of the next chunk.
                chunk_size = input.read_long();

                // If the value is non negative, there is more than one chunk.
                // Accumulate chunks in the buffer.
                // The last chunk (or the only chunk, if only one chunk is
                // present) is not written in the buffer. It is stored in the
                // array r, avoiding unnecessary buffer operations.
                if (chunk_size > 0)
                  {
                    bout = new ByteArrayOutputStream(2 * chunk_size);
                    bout.write(r, 0, chunk_size);
                  }
              }

            if (bout != null)
              {
                // More than one chunk was present.
                // Add the last chunk.
                bout.write(r, 0, n);
                input = new noHeaderInput(bout.toByteArray());
              }
            else
              {
                // Only one chunk was present.
                input = new noHeaderInput(r);
              }
          }
        else
          {
            if (input instanceof cdrBufInput)
              {
                // Highly probable case.
                input =
                  new noHeaderInput(((cdrBufInput) input).buffer.getBuffer());
              }
            else
              {
                cdrBufOutput bout = new cdrBufOutput();
                int c;
                while ((c = input.read()) >= 0)
                  bout.write((byte) c);
                input = new noHeaderInput(bout.buffer.toByteArray());
              }
          }
      }
    catch (IOException ex)
      {
        MARSHAL m = new MARSHAL("Unable to read chunks");
        m.initCause(ex);
        throw m;
      }

    // The user-defines io operations are implemented.
    if (value instanceof CustomMarshal)
      {
        CustomMarshal marsh = (CustomMarshal) value;
        try
          {
            marsh.unmarshal((DataInputStream) input);
          }
        catch (ClassCastException ex)
          {
            incorrect_plug_in(ex);
          }
      }
    else
    // The IDL-generated io operations are implemented.
    if (value instanceof Streamable)
      {
        ((Streamable) value)._read(input);
      }
    else if (helper instanceof BoxedValueHelper)
      value = ((BoxedValueHelper) helper).read_value(input);
    else if (helper instanceof ValueFactory)
      value =
        ((ValueFactory) helper).read_value((org.omg.CORBA_2_3.portable.InputStream) input);
    else

      // Stating the interfaces that the USER should use.
      throw new MARSHAL("The " + value.getClass().getName() +
        " must implement either StreamableValue or CustomValue."
      );

    // The negative end of state marker is expected from OMG standard.
    // If the chunking is used, this marker is already extracted.
    if ((value_tag & vf_CHUNKING) == 0)
      {
        int eor = input.read_long();
        if (eor >= 0)
          throw new MARSHAL("End of state marker has an invalid value " + eor);
      }

    return value;
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
    else
      write(output, value, ObjectCreator.toIDL(value.getClass().getName()));
  }

  /**
   * Write the value base into the given stream, stating that it is an
   * instance of the given class. The written record has no repository
   * id and requires to supply a class or initialised instance for reading
   * rather than an actual class it is.
   *
   * This results writing a different repository id.
   *
   * If the passed value implements the {@link CustomMarshal},
   * the helper uses {@link CustomMarshal#marshal}
   * to write the content in a user defined way. Otherwise,
   * this implementation initialises the {@link ObjectOutputStream}
   * and writes through it.
   *
   * @param output a stream to write to.
   *
   * @param value a value to write.
   *
   * @throws MARSHAL if the writing failed due any reason.
   */
  public static void write(OutputStream output, Serializable value,
    Class substitute
  )
  {
    // Write null if this is a null value.
    if (value == null)
      output.write_long(vt_NULL);

    else
      write(output, value, ObjectCreator.toIDL(substitute.getName()));
  }

  /**
   * Write value when its repository Id is explicitly given.
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
      write_instance(output, value, id, null);
  }

  /**
   * Write standard value type header, followed by contents, produced
   * by the boxed value helper.
   *
   * @param output the stream to write to.
   * @param value the value to write, can be null.
   * @param helper the helper that writes the value content if it is
   * not null.
   */
  public static void write(OutputStream output, Serializable value,
    Object helper
  )
  {
    if (value == null)
      output.write_long(vt_NULL);
    else
      {
        String id;

        if (helper instanceof BoxedValueHelper)
          id = ((BoxedValueHelper) helper).get_id();
        else
          id = "";

        write_instance(output, value, id, helper);
      }
  }

  /**
   * Write value when its repository Id is explicitly given.
   * Does not handle null.
   *
   * @param output an output stream to write into.
   * @param value a value to write.
   * @param id a value repository id.
   * @param helper a helper, writing object - specifica part. Can be null
   * if the value should be written unsing other methods.
   */
  private static void write_instance(OutputStream output, Serializable value,
    String id, Object helper
  )
  {
    // This implementation always writes a single repository id.
    // It never writes multiple repository ids and currently does not use
    // a codebase.
    int value_tag = vt_VALUE_TAG | vf_ID;

    OutputStream outObj;
    cdrBufOutput out = null;

    if (USE_CHUNKING)
      {
        out = new cdrBufOutput();
        out.setOrb(output.orb());
        outObj = out;
        value_tag |= vf_CHUNKING;
      }
    else
      outObj = output;

    output.write_long(value_tag);
    output.write_string(id);

    if (helper instanceof BoxedValueHelper)
      {
        ((BoxedValueHelper) helper).write_value(outObj, value);
      }
    else
    // User defince write method is present.
    if (value instanceof CustomMarshal)
      {
        try
          {
            ((CustomMarshal) value).marshal((DataOutputStream) outObj);
          }
        catch (ClassCastException ex)
          {
            incorrect_plug_in(ex);
          }
      }
    else if (value instanceof Streamable)
      {
        ((Streamable) value)._write(outObj);
      }
    else
      {
        // Try to find helper via class loader.
        boolean ok = false;
        try
          {
            Class helperClass = Class.forName(ObjectCreator.toHelperName(id));

            // It will be the helper for the encapsulated boxed value, not the
            // for the global boxed value type itself.
            Method write =
              helperClass.getMethod("write",
                new Class[]
                {
                  org.omg.CORBA.portable.OutputStream.class, value.getClass()
                }
              );
            write.invoke(null, new Object[] { outObj, value });
            ok = true;
          }
        catch (Exception ex)
          {
            ok = false;
          }

        // Stating the interfaces that the USER should use.
        if (!ok)
          throw new MARSHAL("The " + value.getClass().getName() +
            " must implement either StreamableValue" + " or CustomValue."
          );
      }

    if (USE_CHUNKING)
      {
        output.write_long(out.buffer.size());
        try
          {
            out.buffer.writeTo(output);
          }
        catch (IOException ex)
          {
            MARSHAL m = new MARSHAL();
            m.initCause(ex);
            throw m;
          }
      }

    // The end of record marker, required by OMG standard.
    output.write_long(-1);
  }

  /**
   * This can be called if the alternative CORBA implementation
   * is incorrectly plugged in.
   *
   * @throws NO_IMPLEMENT, always.
   */
  static void incorrect_plug_in(Throwable ex) throws NO_IMPLEMENT
  {
    NO_IMPLEMENT no = new NO_IMPLEMENT("Incorrect CORBA plug-in");
    no.initCause(ex);
    throw no;
  }

  /**
   * Check the passed value tag for correctness.
   *
   * @param value_tag a tag to check, must be between 0x7fffff00 and 0x7fffffff
   *
   * @throws MARSHAL if the tag is outside this interval.
   */
  private static final void checkTag(int value_tag)
  {
    if ((value_tag < 0x7fffff00 || value_tag > 0x7fffffff) &&
      value_tag != vt_NULL &&
      value_tag != vt_INDIRECTION
    )
      throw new MARSHAL("Invalid value record, unsupported header tag: " +
        value_tag
      );
  }
}