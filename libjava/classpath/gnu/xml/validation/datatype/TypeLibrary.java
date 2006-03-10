/* TypeLibrary.java -- 
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

import java.util.HashMap;
import java.util.Map;
import org.relaxng.datatype.Datatype;
import org.relaxng.datatype.DatatypeBuilder;
import org.relaxng.datatype.DatatypeException;
import org.relaxng.datatype.DatatypeLibrary;

/**
 * Datatype library for XML Schema datatypes.
 *
 * @author <a href='mailto:dog@gnu.org'>Chris Burdess</a>
 */
public class TypeLibrary
  implements DatatypeLibrary
{
  
  public static final SimpleType ANY_SIMPLE_TYPE = new AnySimpleType();

  public static final SimpleType STRING = new StringType();
  public static final SimpleType BOOLEAN = new BooleanType();
  public static final SimpleType DECIMAL = new DecimalType();
  public static final SimpleType FLOAT = new FloatType();
  public static final SimpleType DOUBLE = new DoubleType();
  public static final SimpleType DURATION = new DurationType();
  public static final SimpleType DATE_TIME = new DateTimeType();
  public static final SimpleType TIME = new TimeType();
  public static final SimpleType DATE = new DateType();
  public static final SimpleType G_YEAR_MONTH = new GYearMonthType();
  public static final SimpleType G_YEAR = new GYearType();
  public static final SimpleType G_MONTH_DAY = new GMonthDayType();
  public static final SimpleType G_DAY = new GDayType();
  public static final SimpleType G_MONTH = new GMonthType();
  public static final SimpleType HEX_BINARY = new HexBinaryType();
  public static final SimpleType BASE64_BINARY = new Base64BinaryType();
  public static final SimpleType ANY_URI = new AnyURIType();
  public static final SimpleType QNAME = new QNameType();
  public static final SimpleType NOTATION = new NotationType();

  public static final SimpleType NORMALIZED_STRING = new NormalizedStringType();
  public static final SimpleType TOKEN = new TokenType();
  public static final SimpleType LANGUAGE = new LanguageType();
  public static final SimpleType NMTOKEN = new NMTokenType();
  public static final SimpleType NMTOKENS = new NMTokensType();
  public static final SimpleType NAME = new NameType();
  public static final SimpleType NCNAME = new NCNameType();
  public static final SimpleType ID = new IDType();
  public static final SimpleType IDREF = new IDRefType();
  public static final SimpleType IDREFS = new IDRefsType();
  public static final SimpleType ENTITY = new EntityType();
  public static final SimpleType ENTITIES = new EntitiesType();
  public static final SimpleType INTEGER = new IntegerType();
  public static final SimpleType NON_POSITIVE_INTEGER = new NonPositiveIntegerType();
  public static final SimpleType NEGATIVE_INTEGER = new NegativeIntegerType();
  public static final SimpleType LONG = new LongType();
  public static final SimpleType INT = new IntType();
  public static final SimpleType SHORT = new ShortType();
  public static final SimpleType BYTE = new ByteType();
  public static final SimpleType NON_NEGATIVE_INTEGER = new NonNegativeIntegerType();
  public static final SimpleType UNSIGNED_LONG = new UnsignedLongType();
  public static final SimpleType UNSIGNED_INT = new UnsignedIntType();
  public static final SimpleType UNSIGNED_SHORT = new UnsignedShortType();
  public static final SimpleType UNSIGNED_BYTE = new UnsignedByteType();
  public static final SimpleType POSITIVE_INTEGER = new PositiveIntegerType();

  private static Map byName;
  static
  {
    byName = new HashMap();
    byName.put("anySimpleType", ANY_SIMPLE_TYPE);
    byName.put("string", STRING);
    byName.put("boolean", BOOLEAN);
    byName.put("decimal", DECIMAL);
    byName.put("float", FLOAT);
    byName.put("double", DOUBLE);
    byName.put("duration", DURATION);
    byName.put("dateTime", DATE_TIME);
    byName.put("time", TIME);
    byName.put("date", DATE);
    byName.put("gYearMonth", G_YEAR_MONTH);
    byName.put("gYear", G_YEAR);
    byName.put("gMonthDay", G_MONTH_DAY);
    byName.put("gDay", G_DAY);
    byName.put("gMonth",G_MONTH);
    byName.put("hexBinary", HEX_BINARY);
    byName.put("base64Binary", BASE64_BINARY);
    byName.put("anyURI", ANY_URI);
    byName.put("QName", QNAME);
    byName.put("NOTATION", NOTATION);
    byName.put("normalizedString", NORMALIZED_STRING);
    byName.put("token", TOKEN);
    byName.put("language", LANGUAGE);
    byName.put("NMTOKEN", NMTOKEN);
    byName.put("NMTOKENS", NMTOKENS);
    byName.put("Name", NAME);
    byName.put("NCName", NCNAME);
    byName.put("ID", ID);
    byName.put("IDREF", IDREF);
    byName.put("IDREFS", IDREFS);
    byName.put("ENTITY", ENTITY);
    byName.put("ENTITIES", ENTITIES);
    byName.put("integer", INTEGER);
    byName.put("nonPositiveInteger", NON_POSITIVE_INTEGER);
    byName.put("negativeInteger", NEGATIVE_INTEGER);
    byName.put("long", LONG);
    byName.put("int", INT);
    byName.put("short", SHORT);
    byName.put("byte", BYTE);
    byName.put("nonNegativeInteger", NON_NEGATIVE_INTEGER);
    byName.put("unsignedLong", UNSIGNED_LONG);
    byName.put("unsignedInt", UNSIGNED_INT);
    byName.put("unsignedShort", UNSIGNED_SHORT);
    byName.put("unsignedByte", UNSIGNED_BYTE);
    byName.put("positiveInteger", POSITIVE_INTEGER);
  }

  public DatatypeBuilder createDatatypeBuilder(String baseTypeLocalName)
    throws DatatypeException
  {
    SimpleType type = (SimpleType) byName.get(baseTypeLocalName);
    if (type == null)
      throw new DatatypeException("Unknown type name: " + baseTypeLocalName);
    return new TypeBuilder(type);
  }

  public Datatype createDatatype(String typeLocalName)
    throws DatatypeException
  {
    SimpleType type = (SimpleType) byName.get(typeLocalName);
    if (type == null)
      throw new DatatypeException("Unknown type name: " + typeLocalName);
    return type;
  }
  
}
