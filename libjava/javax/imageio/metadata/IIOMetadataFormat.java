/* IIOMetadataFormat.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package javax.imageio.metadata;

import java.util.Locale;

import javax.imageio.ImageTypeSpecifier;

/**
 * @author Michael Koch (konqueror@gmx.de)
 */
public interface IIOMetadataFormat
{
  int CHILD_POLICY_ALL = 1;
  int CHILD_POLICY_CHOICE = 3;
  int CHILD_POLICY_EMPTY = 0;
  int CHILD_POLICY_MAX = 5;
  int CHILD_POLICY_REPEAT = 5;
  int CHILD_POLICY_SEQUENCE = 4;
  int CHILD_POLICY_SOME = 2;
  int DATATYPE_BOOLEAN = 1;
  int DATATYPE_DOUBLE = 4;
  int DATATYPE_FLOAT = 3;
  int DATATYPE_INTEGER = 2;
  int DATATYPE_STRING = 0;
  int VALUE_ARBITRARY = 1;
  int VALUE_ENUMERATION = 16;
  int VALUE_LIST = 32;
  int VALUE_NONE = 0;
  int VALUE_RANGE = 2;
  int VALUE_RANGE_MAX_INCLUSIVE = 10;
  int VALUE_RANGE_MAX_INCLUSIVE_MASK = 8;
  int VALUE_RANGE_MIN_INCLUSIVE = 6;
  int VALUE_RANGE_MIN_INCLUSIVE_MASK = 4;
  int VALUE_RANGE_MIN_MAX_INCLUSIVE = 14;

  boolean canNodeAppear (String elementName, ImageTypeSpecifier imageType);

  int getAttributeDataType (String elementName, String attrName);

  String getAttributeDefaultValue (String elementName, String attrName);

  String getAttributeDescription (String elementName, String attrName, Locale locale);

  String[] getAttributeEnumerations (String elementName, String attrName);

  int getAttributeListMaxLength (String elementName, String attrName);

  int getAttributeListMinLength (String elementName, String attrName);

  String getAttributeMaxValue (String elementName, String attrName);

  String getAttributeMinValue (String elementName, String attrName);

  String[] getAttributeNames (String elementName);

  int getAttributeValueType (String elementName, String attrName);

  String[] getChildNames (String elementName);

  int getChildPolicy (String elementName);

  String getElementDescription (String elementName, Locale locale);

  int getElementMaxChildren (String elementName);

  int getElementMinChildren (String elementName);

  int getObjectArrayMaxLength (String elementName);

  int getObjectArrayMinLength (String elementName);

  Class getObjectClass (String elementName);

  Object getObjectDefaultValue (String elementName);

  Object[] getObjectEnumerations (String elementName);

  Comparable getObjectMaxValue (String elementName);

  Comparable getObjectMinValue (String elementName);

  int getObjectValueType (String elementName);

  String getRootName();

  boolean isAttributeRequired (String elementName, String attrName);
}
