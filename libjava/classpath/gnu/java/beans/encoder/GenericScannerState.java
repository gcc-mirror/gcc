/* GenericScannerState.java
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


package gnu.java.beans.encoder;

import java.util.HashMap;

import gnu.java.beans.encoder.elements.ArrayInstantiation;
import gnu.java.beans.encoder.elements.Array_Get;
import gnu.java.beans.encoder.elements.Array_Set;
import gnu.java.beans.encoder.elements.ClassResolution;
import gnu.java.beans.encoder.elements.Element;
import gnu.java.beans.encoder.elements.List_Get;
import gnu.java.beans.encoder.elements.List_Set;
import gnu.java.beans.encoder.elements.MethodInvocation;
import gnu.java.beans.encoder.elements.NullObject;
import gnu.java.beans.encoder.elements.ObjectInstantiation;
import gnu.java.beans.encoder.elements.ObjectReference;
import gnu.java.beans.encoder.elements.PrimitiveInstantiation;
import gnu.java.beans.encoder.elements.StaticFieldAccess;
import gnu.java.beans.encoder.elements.StaticMethodInvocation;
import gnu.java.beans.encoder.elements.StringReference;

/**
 * This class is a {@link ScannerState} implementation that creates
 * suitable {@link gnu.java.beans.encoder.elements.Element} instances
 * for each transition variant.
 * 
 * <p>Furthermore it can optionally skip a certain number of child
 * elements. The algorithm can cope with the fact that one 
 * <code>GenericScannerState</code> instance may be called at
 * different levels of recursions.</p>
 * 
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
class GenericScannerState extends ScannerState
{
  private int skipElements, initialSkipElements;

  final Root root;

  HashMap skipValues;

  GenericScannerState(Root newRoot)
  {
    root = newRoot;
  }

  GenericScannerState(Root root, int skipElements)
  {
    this(root);
    this.skipElements = initialSkipElements = skipElements;

    if (skipElements > 0)
      skipValues = new HashMap();
  }
  
  protected void enterImpl(Context ctx)
  {
    if (skipValues != null)
      {
        Integer skip = (Integer) skipValues.get(ctx);
        
        if (skip == null)
          {
            skip = Integer.valueOf(initialSkipElements);
            skipValues.put(ctx, skip);
          }
        
        skipElements = skip.intValue();
      }
  }

  void methodInvocation(String methodName)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new MethodInvocation(methodName));
  }

  void staticMethodInvocation(String className, String methodName)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new StaticMethodInvocation(className, methodName));
  }

  void staticFieldAccess(String className, String fieldName)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new StaticFieldAccess(className, fieldName));
  }

  void classResolution(String className)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new ClassResolution(className));
  }

  void objectInstantiation(String className, ObjectId objectId)
  {
    if (skipValues != null && skipElements > 0)
      return;

    Element elem = new ObjectInstantiation(className);
    elem.initId(objectId);

    root.addChild(elem);
  }

  void primitiveInstantiation(String primitiveName, String valueAsString)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new PrimitiveInstantiation(primitiveName, valueAsString));
  }

  void objectArrayInstantiation(String arrayClassName, String lengthAsString,
                          ObjectId objectId)
  {
    if (skipValues != null && skipElements > 0)
      return;

    Element elem = new ArrayInstantiation(arrayClassName, lengthAsString);
    elem.initId(objectId);

    root.addChild(elem);
  }

  void primitiveArrayInstantiation(String arrayClassName, String lengthAsString,
                                ObjectId objectId)
  {
    objectArrayInstantiation(arrayClassName, lengthAsString, objectId);
  }

  void arraySet(String indexAsString)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new Array_Set(indexAsString));
  }

  void arrayGet(String indexAsString)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new Array_Get(indexAsString));
  }

  void listGet()
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new List_Get());
  }

  void listSet()
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new List_Set());
  }

  void nullObject()
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new NullObject());
  }

  void stringReference(String string)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new StringReference(string));
  }

  void objectReference(ObjectId id)
  {
    if (skipValues != null && skipElements > 0)
      return;

    root.addChild(new ObjectReference(id));
  }

  void end()
  {
    if (skipValues != null)
      {
        if (skipElements > 0)
          skipElements--;
        else
          {
            // Finishes the Element we are constructing.
            root.end();
          }
        skipValues.put(context(), Integer.valueOf(skipElements));
      }
    else
      root.end();

  }

  void enter()
  {

  }

}
