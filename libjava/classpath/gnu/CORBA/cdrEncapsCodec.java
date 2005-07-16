/* cdrEncapsCodec.java --
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
import gnu.CORBA.CDR.cdrOutput;

import org.omg.CORBA.Any;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.UserException;
import org.omg.IOP.Codec;
import org.omg.IOP.CodecPackage.FormatMismatch;
import org.omg.IOP.CodecPackage.InvalidTypeForEncoding;
import org.omg.IOP.CodecPackage.TypeMismatch;

/**
 * The local {@link Codec} implementation for ENCODING_CDR_ENCAPS
 * encoding. This is a local implementation; the remote side should
 * have its own Codec of this kind.
 *
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class cdrEncapsCodec
  extends LocalObject
  implements Codec
{
  /**
   * The default version of encoding, used in parameterless constructor.
   */
  private static final Version DEFAULT_VERSION = new Version(1, 2);

  /**
   * If set to true, no wide string or wide character is allowed (GIOP 1.0).
   */
  private final boolean noWide;

  /**
   * The version of this encoding.
   */
  private final Version version;

  /**
   * The associated ORB.
   */
  protected final ORB orb;

  /**
   * If true, this Codec writes the record length (as int) in the beginning
   * of the record. This indicator is part of the formal OMG standard, but it is
   * missing in Sun's implementation. Both Suns's and this Codec detects
   * the indicator, if present, but can also decode data where this information
   * is missing. If the length indicator is missing, the first four bytes in
   * Suns encoding are equal to 0 (Big Endian marker).
   */
  private boolean lengthIndicator = true;

  /**
   * Create an instance of this Codec, encoding following the given version.
   */
  public cdrEncapsCodec(ORB _orb, Version _version)
  {
    orb = _orb;
    version = _version;
    noWide = version.until_inclusive(1, 0);
  }

  /**
   * Return the array of repository ids for this object.
   *
   * @return { "IDL:gnu/CORBA/cdrEnapsCodec:1.0" }, always.
   */
  public String[] _ids()
  {
    return new String[] { "IDL:gnu/CORBA/cdrEnapsCodec:1.0" };
  }

  /**
   * Decode the contents of the byte array into Any.
   * The byte array may have the optional four byte length indicator
   * in the beginning. If these four bytes are zero, it is assumed,
   * that no length indicator is present.
   */
  public Any decode(byte[] them)
             throws FormatMismatch
  {
    cdrBufInput input = createInput(them);
    cdrBufInput encapsulation = createEncapsulation(them, input);

    TypeCode type = encapsulation.read_TypeCode();

    try
      {
        checkTypePossibility("", type);
      }
    catch (InvalidTypeForEncoding ex)
      {
        throw new FormatMismatch(ex.getMessage());
      }

    return readAny(type, encapsulation);
  }

  private cdrBufInput createEncapsulation(byte[] them, cdrBufInput input)
  {
    cdrBufInput encapsulation;

    if ((them [ 0 ] | them [ 1 ] | them [ 2 ] | them [ 3 ]) == 0)
      {
        // Skip that appears to be the always present Big Endian marker.
        encapsulation = input;
        input.read_short();
      }
    else
      encapsulation = input.read_encapsulation();
    return encapsulation;
  }

  /** {@inheritDoc} */
  public byte[] encode(Any that)
                throws InvalidTypeForEncoding
  {
    checkTypePossibility("", that.type());

    cdrBufOutput output = createOutput(that);

    // cdrBufOutput has internal support for this encoding.
    cdrOutput encapsulation = output.createEncapsulation();

    try
      {
        TypeCodeHelper.write(encapsulation, that.type());
        that.write_value(encapsulation);

        encapsulation.close();
        output.close();
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL();
        m.initCause(ex);
        throw m;
      }
    return output.buffer.toByteArray();
  }

  /**
   * Decode the value, stored in the byte array, into Any, assuming,
   * that the byte array holds the data structure, defined by the
   * given typecode.
   *
   * The byte array may have the optional four byte length indicator
   * in the beginning. If these four bytes are zero, it is assumed,
   * that no length indicator is present.
   */
  public Any decode_value(byte[] them, TypeCode type)
                   throws FormatMismatch, TypeMismatch
  {
    try
      {
        checkTypePossibility("", type);
      }
    catch (InvalidTypeForEncoding ex)
      {
        throw new TypeMismatch(ex.getMessage());
      }

    cdrBufInput input = createInput(them);
    cdrBufInput encapsulation = createEncapsulation(them, input);
    return readAny(type, encapsulation);
  }

  /**
   * Read an Any from the given stream.
   *
   * @param type a type of the Any to read.
   * @param input the encapsulation stream.
   */
  private Any readAny(TypeCode type, cdrBufInput encapsulation)
               throws MARSHAL
  {
    gnuAny a = new gnuAny();
    a.setOrb(orb);

    // cdrBufInput has internal support for this encoding.
    a.read_value(encapsulation, type);
    return a;
  }

  /** {@inheritDoc} */
  public byte[] encode_value(Any that)
                      throws InvalidTypeForEncoding
  {
    checkTypePossibility("", that.type());

    cdrBufOutput output = createOutput(that);

    cdrOutput encapsulation = output.createEncapsulation();

    try
      {
        that.write_value(encapsulation);

        encapsulation.close();
        output.close();
      }
    catch (Exception ex)
      {
        MARSHAL m = new MARSHAL();
        m.initCause(ex);
        throw m;
      }
    return output.buffer.toByteArray();
  }

  /**
   * Create the CDR output stream for writing the given Any.
   * The cdrBufOutput has internal support for encapsulation encodings.
   *
   * @param that the Any that will be written.
   *
   * @return the stream.
   *
   * @throws InvalidTypeForEncoding if that Any cannot be written under the
   * given version.
   */
  private cdrBufOutput createOutput(Any that)
                             throws InvalidTypeForEncoding
  {
    cdrBufOutput output = new cdrBufOutput();
    output.setOrb(orb);
    output.setVersion(version);
    return output;
  }

  /**
   * Checks if the given type can be encoded. Currently only checks for wide
   * strings and wide chars for GIOP 1.0.
   *
   * @param t a typecode to chek.
   *
   * @throws InvalidTypeForEncoding if the typecode is not valid for the given
   * version.
   */
  private void checkTypePossibility(String name, TypeCode t)
                             throws InvalidTypeForEncoding
  {
    if (noWide)
      {
        try
          {
            int kind = t.kind().value();

            if (kind == TCKind._tk_wchar || kind == TCKind._tk_wstring)
              throw new InvalidTypeForEncoding(name + " wide char in " +
                                               version
                                              );
            else if (kind == TCKind._tk_alias || kind == TCKind._tk_array ||
                     kind == TCKind._tk_sequence
                    )
              checkTypePossibility("Array member", t.content_type());

            else if (kind == TCKind._tk_struct || kind == TCKind._tk_union)
              {
                for (int i = 0; i < t.member_count(); i++)
                  {
                    checkTypePossibility(t.member_name(i), t.member_type(i));
                  }
              }
          }
        catch (UserException ex)
          {
            InternalError ierr = new InternalError();
            ierr.initCause(ex);
            throw ierr;
          }
      }
  }

  /**
   * Create the CDR input stream for reading the given byte array.
   *
   * @param them a byte array to read.
   *
   * @return the stream.
   */
  private cdrBufInput createInput(byte[] them)
  {
    cdrBufInput input = new cdrBufInput(them);
    input.setOrb(orb);
    input.setVersion(version);
    return input;
  }

  /**
   * Check if the Codec writes the length indicator.
   */
  public boolean hasLengthIndicator()
  {
    return lengthIndicator;
  }

  /**
   * Sets if the Codec must write the record length in the beginning of the
   * array. Encodings both with and without that indicator are understood
   * both by Suns and this codec, but the OMG specification seems requiring
   * it. The default behavior is to use the length indicator.
   *
   * @param use_lengthIndicator
   */
  public void setUseLengthIndicator(boolean use_lengthIndicator)
  {
    lengthIndicator = use_lengthIndicator;
  }
}