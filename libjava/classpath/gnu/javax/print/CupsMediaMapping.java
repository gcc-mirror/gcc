/* CupsMediaMapping.java --
   Copyright (C) 2006 Free Software Foundation, Inc.

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


package gnu.javax.print;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.print.attribute.standard.MediaSizeName;

/**
 * Provides the currently known mappings of the media attribute
 * values of the CUPS printing system to the IPP standard values.
 * <p>
 * The mapping is used to build up print service specific mappings
 * for use of media attribute translation between Java JPS API and
 * CUPS.
 * </p>
 *
 * @author Wolfgang Baer (WBaer@gmx.de)
 */
public class CupsMediaMapping
{
  // the mapping map
  private static final HashMap ippByCups = new HashMap();

  /**
   * Initialize currently known mappings.
   */
  static
    {
      ippByCups.put("Postcard", MediaSizeName.JAPANESE_POSTCARD);
      ippByCups.put("Statement", MediaSizeName.INVOICE);

      ippByCups.put("Letter", MediaSizeName.NA_LETTER);
      ippByCups.put("Executive", MediaSizeName.EXECUTIVE);
      ippByCups.put("Legal", MediaSizeName.NA_LEGAL);

      ippByCups.put("A0", MediaSizeName.ISO_A0);
      ippByCups.put("A1", MediaSizeName.ISO_A1);
      ippByCups.put("A2", MediaSizeName.ISO_A2);
      ippByCups.put("A3", MediaSizeName.ISO_A3);
      ippByCups.put("A4", MediaSizeName.ISO_A4);
      ippByCups.put("A5", MediaSizeName.ISO_A5);
      ippByCups.put("A6", MediaSizeName.ISO_A6);
      ippByCups.put("A7", MediaSizeName.ISO_A7);
      ippByCups.put("A8", MediaSizeName.ISO_A8);
      ippByCups.put("A9", MediaSizeName.ISO_A9);
      ippByCups.put("A10", MediaSizeName.ISO_A10);

      ippByCups.put("B0", MediaSizeName.JIS_B0);
      ippByCups.put("B1", MediaSizeName.JIS_B1);
      ippByCups.put("B2", MediaSizeName.JIS_B2);
      ippByCups.put("B3", MediaSizeName.JIS_B3);
      ippByCups.put("B4", MediaSizeName.JIS_B4);
      ippByCups.put("B5", MediaSizeName.JIS_B5);
      ippByCups.put("B6", MediaSizeName.JIS_B6);
      ippByCups.put("B7", MediaSizeName.JIS_B7);
      ippByCups.put("B8", MediaSizeName.JIS_B8);
      ippByCups.put("B9", MediaSizeName.JIS_B9);
      ippByCups.put("B10", MediaSizeName.JIS_B10);

      ippByCups.put("ISOB0", MediaSizeName.ISO_B0);
      ippByCups.put("ISOB1", MediaSizeName.ISO_B1);
      ippByCups.put("ISOB2", MediaSizeName.ISO_B2);
      ippByCups.put("ISOB3", MediaSizeName.ISO_B3);
      ippByCups.put("ISOB4", MediaSizeName.ISO_B4);
      ippByCups.put("ISOB5", MediaSizeName.ISO_B5);
      ippByCups.put("ISOB6", MediaSizeName.ISO_B6);
      ippByCups.put("ISOB7", MediaSizeName.ISO_B7);
      ippByCups.put("ISOB8", MediaSizeName.ISO_B8);
      ippByCups.put("ISOB9", MediaSizeName.ISO_B9);
      ippByCups.put("ISOB10", MediaSizeName.ISO_B10);
      ippByCups.put("EnvISOB0", MediaSizeName.ISO_B0);
      ippByCups.put("EnvISOB1", MediaSizeName.ISO_B1);
      ippByCups.put("EnvISOB2", MediaSizeName.ISO_B2);
      ippByCups.put("EnvISOB3", MediaSizeName.ISO_B3);
      ippByCups.put("EnvISOB4", MediaSizeName.ISO_B4);
      ippByCups.put("EnvISOB5", MediaSizeName.ISO_B5);
      ippByCups.put("EnvISOB6", MediaSizeName.ISO_B6);
      ippByCups.put("EnvISOB7", MediaSizeName.ISO_B7);
      ippByCups.put("EnvISOB8", MediaSizeName.ISO_B8);
      ippByCups.put("EnvISOB9", MediaSizeName.ISO_B9);
      ippByCups.put("EnvISOB10", MediaSizeName.ISO_B10);

      ippByCups.put("C0", MediaSizeName.ISO_C0);
      ippByCups.put("C1", MediaSizeName.ISO_C1);
      ippByCups.put("C2", MediaSizeName.ISO_C2);
      ippByCups.put("C3", MediaSizeName.ISO_C3);
      ippByCups.put("C4", MediaSizeName.ISO_C4);
      ippByCups.put("C5", MediaSizeName.ISO_C5);
      ippByCups.put("C6", MediaSizeName.ISO_C6);

      ippByCups.put("EnvPersonal", MediaSizeName.PERSONAL_ENVELOPE);
      ippByCups.put("EnvMonarch", MediaSizeName.MONARCH_ENVELOPE);
      ippByCups.put("Monarch", MediaSizeName.MONARCH_ENVELOPE);
      ippByCups.put("Env9", MediaSizeName.NA_NUMBER_9_ENVELOPE);
      ippByCups.put("Env10", MediaSizeName.NA_NUMBER_10_ENVELOPE);
      ippByCups.put("Env11", MediaSizeName.NA_NUMBER_11_ENVELOPE);
      ippByCups.put("Env12", MediaSizeName.NA_NUMBER_12_ENVELOPE);
      ippByCups.put("Env14", MediaSizeName.NA_NUMBER_14_ENVELOPE);
      ippByCups.put("c8x10", MediaSizeName.NA_8X10);

      ippByCups.put("EnvDL", MediaSizeName.ISO_DESIGNATED_LONG);
      ippByCups.put("DL", MediaSizeName.ISO_DESIGNATED_LONG);
      ippByCups.put("EnvC0", MediaSizeName.ISO_C0);
      ippByCups.put("EnvC1", MediaSizeName.ISO_C1);
      ippByCups.put("EnvC2", MediaSizeName.ISO_C2);
      ippByCups.put("EnvC3", MediaSizeName.ISO_C3);
      ippByCups.put("EnvC4", MediaSizeName.ISO_C4);
      ippByCups.put("EnvC5", MediaSizeName.ISO_C5);
      ippByCups.put("EnvC6", MediaSizeName.ISO_C6);
    }

  /**
   * Returns the IPP media name of the given cups name.
   *
   * @param cupsName the name in cups
   * @return The IPP name if a mapping is known, <code>null</code> otherwise.
   */
  public static final String getIppName(String cupsName)
  {
    return (String) ippByCups.get(cupsName);
  }

  /**
   * Returns the mapping map for iteration.
   *
   * @return The mapping map as unmodifiable map.
   */
  public static final Map getMappingMap()
  {
    return Collections.unmodifiableMap(ippByCups);
  }

  private CupsMediaMapping()
  {
    // not to be instantiated
  }
}
