/* CodeSet_sctx.java --
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


package gnu.CORBA.GIOP;

import gnu.CORBA.CDR.AbstractCdrInput;
import gnu.CORBA.CDR.AbstractCdrOutput;
import gnu.CORBA.IOR;
import gnu.CORBA.IOR.CodeSets_profile;

import java.io.IOException;

/**
 * The code set service context. This context must be included in all
 * messages that use wide characters.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class CodeSetServiceContext
  extends ServiceContext
{
  /**
   * The context code sets id.
   */
  public static final int ID = 1;

  /**
   * The standard component to include in the messages.
   */
  public static final CodeSetServiceContext STANDARD = new CodeSetServiceContext();

  /**
   * The encoding, used to transfer the narrow (1 byte) character data.
   * The default value is taken from {@link CharSets_OSF#NATIVE_CHARACTER}.
   */
  public int char_data = CharSets_OSF.NATIVE_CHARACTER;

  /**
   * The encoding, used to transfer the wide character data.
   * The default value is taken from
   * {@link CharSets_OSF#NATIVE_WIDE_CHARACTER}.
   */
  public int wide_char_data = CharSets_OSF.NATIVE_WIDE_CHARACTER;

  /**
   * Find and return the code set service context in the give
   * contexts array. Returns {@link #STANDARD} if no code set
   * context is present.
   *
   * @param contexts the array of contexts, can be null.
   */
  public static CodeSetServiceContext find(ServiceContext[] contexts)
  {
    if (contexts != null)
      for (int i = 0; i < contexts.length; i++)
        {
          if (contexts [ i ] instanceof CodeSetServiceContext)
            return (CodeSetServiceContext) contexts [ i ];
        }
    return STANDARD;
  }

  /**
   * Select the suitable encoding that is defined in the provided profile.
   *
   * TODO character encoding. Now the encoding can be set, but it is ignored.
   * If you take this task, scan 'TODO character encoding' for
   * relevant places.
   */
  public static CodeSetServiceContext negotiate(IOR.CodeSets_profile profile)
  {
    if (profile.negotiated != null)
      return profile.negotiated;

    CodeSetServiceContext use = new CodeSetServiceContext();

    use.char_data =
      negotiate(profile.narrow, STANDARD.char_data, CharSets_OSF.ISO8859_1);

    use.wide_char_data =
      negotiate(profile.wide, STANDARD.wide_char_data, CharSets_OSF.UTF16);

    profile.negotiated = use;

    return use;
  }

  /**
   * Read the context from the given stream. Does not read the
   * code sets id.
   */
  public void readContext(AbstractCdrInput input)
  {
    AbstractCdrInput encap = input.read_encapsulation();

    char_data = encap.read_ulong();
    wide_char_data = encap.read_ulong();
  }

  /**
   * Return a string representation.
   */
  public String toString()
  {
    return " Encoding: narrow " + name(char_data) + ", wide " +
           name(wide_char_data) + ". ";
  }

  /**
   * Write the context to the given stream, including the code
   * sets id.
   */
  public void write(AbstractCdrOutput output)
  {
    output.write_ulong(ID);

    AbstractCdrOutput enout = output.createEncapsulation();

    enout.write_long(char_data);
    enout.write_ulong(wide_char_data);

    try
      {
        enout.close();
      }
    catch (IOException ex)
      {
        InternalError t = new InternalError();
        t.initCause(ex);
        throw t;
      }
  }

  /**
   * Negotiate about the character encoding. Prefer our native encoding,
   * if no, prefer IORs native encoding, if no, find any encoding,
   * supported by both sides, if no, return the specified final decission.
   *
   * @param profile the component profile in IOR.
   * @param our_native our native encoding
   * @param final_decission the encoding that must be returned if no
   * compromise is found.
   *
   * @return the resulted encoding.
   */
  protected static int negotiate(IOR.CodeSets_profile.CodeSet_component profile,
                                 int our_native, int final_decission
                                )
  {
    // If our and IORs native sets match, use the native set.
    if (profile.native_set == our_native)
      return our_native;

    // If the native sets do not match, but the IOR says it
    // supports our native set, use our native set.
    if (profile.conversion != null)
      for (int i = 0; i < profile.conversion.length; i++)
        {
          if (our_native == profile.conversion [ i ])
            return our_native;
        }

    // At this point, we suggest to use the IORs native set.
    int[] allSupported = CharSets_OSF.getSupportedCharSets();

    for (int s = 0; s < allSupported.length; s++)
      if (allSupported [ s ] == profile.native_set)
        return profile.native_set;

    // Any compromise left?
    if (profile.conversion != null)
      for (int s = 0; s < allSupported.length; s++)
        for (int i = 0; i < profile.conversion.length; i++)
          if (allSupported [ s ] == profile.conversion [ i ])
            return allSupported [ s ];

    // Return the CORBA default char encoding.
    return final_decission;
  }

  /**
   * Conveniency method, used in toString()
   */
  private String name(int set)
  {
    return "0x" + Integer.toHexString(set) + " (" + CharSets_OSF.getName(set) +
           ") ";
  }
}
