/* TypeBuilder.java --
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

package gnu.xml.validation.datatype;

import java.util.LinkedHashSet;
import java.util.regex.Pattern;
import org.relaxng.datatype.Datatype;
import org.relaxng.datatype.DatatypeBuilder;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.ValidationContext;

/**
 * Datatype builder.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class TypeBuilder
  implements DatatypeBuilder
{

  final SimpleType type;

  TypeBuilder(SimpleType type)
  {
    this.type = type;
    // TODO fundamental facets
    type.facets = new LinkedHashSet();
  }

  public void addParameter(String name, String value, ValidationContext context)
    throws DatatypeException
  {
    // TODO fundamental facets
    if ("length".equals(name))
      type.facets.add(parseLengthFacet(value));
    else if ("minLength".equals(name))
      type.facets.add(parseMinLengthFacet(value));
    else if ("maxLength".equals(name))
      type.facets.add(parseMaxLengthFacet(value));
    else if ("pattern".equals(name))
      type.facets.add(parsePatternFacet(value));
    else if ("enumeration".equals(name))
      type.facets.add(parseEnumerationFacet(value));
    else if ("whiteSpace".equals(name))
      type.facets.add(parseWhiteSpaceFacet(value));
    else if ("maxInclusive".equals(name))
      type.facets.add(parseMaxInclusiveFacet(value, context));
    else if ("maxExclusive".equals(name))
      type.facets.add(parseMaxExclusiveFacet(value, context));
    else if ("minExclusive".equals(name))
      type.facets.add(parseMinExclusiveFacet(value, context));
    else if ("minInclusive".equals(name))
      type.facets.add(parseMinInclusiveFacet(value, context));
    else if ("totalDigits".equals(name))
      type.facets.add(parseTotalDigitsFacet(value));
    else if ("fractionDigits".equals(name))
      type.facets.add(parseFractionDigitsFacet(value));
  }

  LengthFacet parseLengthFacet(String value)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new LengthFacet(Integer.parseInt(value), fixed, null);
  }

  MinLengthFacet parseMinLengthFacet(String value)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new MinLengthFacet(Integer.parseInt(value), fixed, null);
  }

  MaxLengthFacet parseMaxLengthFacet(String value)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new MaxLengthFacet(Integer.parseInt(value), fixed, null);
  }

  PatternFacet parsePatternFacet(String value)
    throws DatatypeException
  {
    return new PatternFacet(Pattern.compile(value), null);
  }

  EnumerationFacet parseEnumerationFacet(String value)
    throws DatatypeException
  {
    return new EnumerationFacet(value, null);
  }

  WhiteSpaceFacet parseWhiteSpaceFacet(String value)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    if ("preserve".equals(value))
      return new WhiteSpaceFacet(WhiteSpaceFacet.PRESERVE, fixed, null);
    if ("replace".equals(value))
      return new WhiteSpaceFacet(WhiteSpaceFacet.REPLACE, fixed, null);
    if ("collapse".equals(value))
      return new WhiteSpaceFacet(WhiteSpaceFacet.COLLAPSE, fixed, null);
    throw new DatatypeException("argument must be preserve, replace, or collapse");
  }

  MaxInclusiveFacet parseMaxInclusiveFacet(String value,
                                           ValidationContext context)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new MaxInclusiveFacet(type.createValue(value, context), fixed, null);
  }

  MaxExclusiveFacet parseMaxExclusiveFacet(String value,
                                           ValidationContext context)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new MaxExclusiveFacet(type.createValue(value, context), fixed, null);
  }

  MinExclusiveFacet parseMinExclusiveFacet(String value,
                                           ValidationContext context)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new MinExclusiveFacet(type.createValue(value, context), fixed, null);
  }

  MinInclusiveFacet parseMinInclusiveFacet(String value,
                                           ValidationContext context)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    return new MinInclusiveFacet(type.createValue(value, context), fixed, null);
  }

  TotalDigitsFacet parseTotalDigitsFacet(String value)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    int val = Integer.parseInt(value);
    if (val < 0)
      throw new DatatypeException("value must be a positiveInteger");
    return new TotalDigitsFacet(val, fixed, null);
  }

  FractionDigitsFacet parseFractionDigitsFacet(String value)
    throws DatatypeException
  {
    int si = value.indexOf(' ');
    boolean fixed = false;
    if (si != -1)
      {
        if (!"FIXED".equalsIgnoreCase(value.substring(si + 1)))
          throw new DatatypeException("second argument must be FIXED if present");
        fixed = true;
        value = value.substring(0, si);
      }
    int val = Integer.parseInt(value);
    if (val < 0)
      throw new DatatypeException("value must be a positiveInteger");
    return new FractionDigitsFacet(val, fixed, null);
  }

  public Datatype createDatatype()
  {
    return type;
  }

}
