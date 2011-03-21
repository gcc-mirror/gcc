/* SimpleType.java --
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

import java.util.Iterator;
import java.util.Set;
import java.util.regex.Matcher;
import javax.xml.namespace.QName;
import org.relaxng.datatype.Datatype;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.DatatypeStreamingValidator;
import org.relaxng.datatype.ValidationContext;

/**
 * An XML Schema simple type.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class SimpleType
  extends Type
  implements Datatype
{

  /**
   * The variety of the <code>anySimpleType</code> datatype.
   */
  public static final int ANY = 0;

  /**
   * The atomic variety.
   */
  public static final int ATOMIC = 1;

  /**
   * The list variety.
   */
  public static final int LIST = 2;

  /**
   * The union variety.
   */
  public static final int UNION = 3;

  public static final int ID_TYPE_NULL = 0;
  public static final int ID_TYPE_ID = 1;
  public static final int ID_TYPE_IDREF = 2;
  public static final int ID_TYPE_IDREFS = 3;

  /**
   * The variety of this simple type.
   */
  public final int variety;

  /**
   * The facets of this simple type.
   */
  public Set facets;

  /**
   * The fundamental facets of this simple type.
   */
  public int fundamentalFacets;

  /**
   * If this datatype has been derived by restriction, then the component
   * from which it was derived.
   */
  public final SimpleType baseType;

  /**
   * Optional annotation.
   */
  public final Annotation annotation;

  public SimpleType(QName name, int variety, Set facets,
                    int fundamentalFacets, SimpleType baseType,
                    Annotation annotation)
  {
    super(name);
    this.variety = variety;
    this.facets = facets;
    this.fundamentalFacets = fundamentalFacets;
    this.baseType = baseType;
    this.annotation = annotation;
  }

  /**
   * Indicates whether this type permits the specified value.
   */
  public boolean isValid(String value, ValidationContext context)
  {
    try
      {
        checkValid(value, context);
        return true;
      }
    catch (DatatypeException e)
      {
        return false;
      }
  }

  public void checkValid(String value, ValidationContext context)
    throws DatatypeException
  {
    if (facets != null && !facets.isEmpty())
      {
        Object parsedValue = createValue(value, context);
        for (Iterator i = facets.iterator(); i.hasNext(); )
          {
            Facet facet = (Facet) i.next();
            switch (facet.type)
              {
              case Facet.LENGTH:
                LengthFacet lf = (LengthFacet) facet;
                if (value.length() != lf.value)
                  throw new DatatypeException("invalid length");
                break;
              case Facet.MIN_LENGTH:
                MinLengthFacet nlf = (MinLengthFacet) facet;
                if (value.length() < nlf.value)
                  throw new DatatypeException("invalid minimum length");
                break;
              case Facet.MAX_LENGTH:
                MaxLengthFacet xlf = (MaxLengthFacet) facet;
                if (value.length() > xlf.value)
                  throw new DatatypeException("invalid maximum length");
                break;
              case Facet.PATTERN:
                PatternFacet pf = (PatternFacet) facet;
                Matcher matcher = pf.value.matcher(value);
                if (!matcher.find())
                  throw new DatatypeException("invalid match for pattern");
                break;
              case Facet.ENUMERATION:
                // TODO
                break;
              case Facet.WHITESPACE:
                // TODO
                break;
              case Facet.MAX_INCLUSIVE:
                MaxInclusiveFacet xif = (MaxInclusiveFacet) facet;
                if (!xif.matches(parsedValue))
                  throw new DatatypeException("beyond upper bound");
                break;
              case Facet.MAX_EXCLUSIVE:
                MaxExclusiveFacet xef = (MaxExclusiveFacet) facet;
                if (!xef.matches(parsedValue))
                  throw new DatatypeException("beyond upper bound");
                break;
              case Facet.MIN_EXCLUSIVE:
                MinExclusiveFacet nef = (MinExclusiveFacet) facet;
                if (!nef.matches(parsedValue))
                  throw new DatatypeException("beyond lower bound");
                break;
              case Facet.MIN_INCLUSIVE:
                MinInclusiveFacet nif = (MinInclusiveFacet) facet;
                if (!nif.matches(parsedValue))
                  throw new DatatypeException("beyond lower bound");
                break;
              case Facet.TOTAL_DIGITS:
                TotalDigitsFacet tdf = (TotalDigitsFacet) facet;
                if (countDigits(value, true) > tdf.value)
                  throw new DatatypeException("too many digits");
                break;
              case Facet.FRACTION_DIGITS:
                FractionDigitsFacet fdf = (FractionDigitsFacet) facet;
                if (countDigits(value, false) > fdf.value)
                  throw new DatatypeException("too many fraction digits");
                break;
              }
          }
      }
  }

  private static int countDigits(String value, boolean any)
  {
    int count = 0;
    int len = value.length();
    boolean seenDecimal = false;
    for (int i = 0; i < len; i++)
      {
        char c = value.charAt(i);
        if (c == 0x2e)
          seenDecimal = true;
        else if (c >= 0x30 && c <= 0x39 && (any || seenDecimal))
          count++;
      }
    return count;
  }

  // TODO createStreamingValidator
  public DatatypeStreamingValidator createStreamingValidator(ValidationContext context)
  {
    throw new UnsupportedOperationException();
  }

  public Object createValue(String literal, ValidationContext context) {
    return literal;
  }

  public boolean sameValue(Object value1, Object value2) {
    return value1.equals(value2);
  }

  public int valueHashCode(Object value) {
    return value.hashCode();
  }

  public int getIdType()
  {
    return ID_TYPE_NULL;
  }

  public boolean isContextDependent()
  {
    return false;
  }

}
