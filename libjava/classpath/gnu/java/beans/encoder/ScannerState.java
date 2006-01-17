/* ScannerState.java
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

/** <p>Provides the infrastructure for the state machine and the transition
 * mechanism.</p>
 * 
 * <p>Each states knows a set of successor. There can be one successor for
 * every transition variant. Furthermore a state knows about a default
 * successor which is taken when there is no special setup for a
 * transition.</p>
 * 
 * @author Robert Schuster (robertschuster@fsfe.org)
 *
 */
public abstract class ScannerState
{

  static final int TRANSITION_METHOD_INVOCATION = 0;

  static final int TRANSITION_STATIC_METHOD_INVOCATION = 1;

  static final int TRANSITION_STATIC_FIELD_ACCESS = 2;

  static final int TRANSITION_CLASS_RESOLUTION = 3;

  static final int TRANSITION_OBJECT_INSTANTIATION = 4;

  static final int TRANSITION_PRIMITIVE_INSTANTIATION = 5;

  static final int TRANSITION_OBJECT_ARRAY_INSTANTIATION = 6;

  static final int TRANSITION_PRIMITIVE_ARRAY_INSTANTIATION = 7;

  static final int TRANSITION_ARRAY_SET = 8;

  static final int TRANSITION_ARRAY_GET = 9;

  static final int TRANSITION_LIST_SET = 10;

  static final int TRANSITION_LIST_GET = 11;

  static final int TRANSITION_NULL_OBJECT = 12;

  static final int TRANSITION_STRING_REFERENCE = 13;

  static final int TRANSITION_OBJECT_REFERENCE = 14;

  static final int TRANSITION_FIRST = 0;

  static final int TRANSITION_LAST = 14;

  static final String DEFAULT_STATE_NAME = "default";

  String defaultSuccessor = DEFAULT_STATE_NAME;

  static String[] transitionNames = { "METHOD_INVOCATION", "STATIC_METHOD_INVOCATION",
                              "STATIC_FIELD_ACCESS", "CLASS_RESOLUTION",
                              "OBJECT_INSTANTIATION",
                              "PRIMITIVE_INSTANTIATION", "OBJECT_ARRAY_INSTANTIATION",
                              "PRIMITIVE_ARRAY_INSTANTIATION",
                              "ARRAY_SET", "ARRAY_GET", "LIST_SET", "LIST_GET",
                              "NULL_OBJECT", "STRING_REFERENCE", "OBJECT_REFERENCE" };

  /**
   * Stores the transition setup as the relation
   * transition->successor's state name.
   */
  HashMap transitions = new HashMap();
  
  int calls;
  
  Context context;
  
  String name;
  
  final void init(String newName)
  {
    assert (name == null);
    
    name = newName;
  }
  
  final String getName()
  {
    return name;
  }
  
  final void enter(Context ctx)
  {
    calls++;
    context = ctx;
    
    enterImpl(ctx);
  }
  
  protected void enterImpl(Context ctx)
  {
  }
  
  final Context context()
  {
    return context;
  }
  
  final int getCalls()
  {
    return calls;
  }

  /**
   * <p>Stores a successor's state name for a certain transition.</p>
   * 
   * <p>This method is only used at the configuration time of the state
   * machine.</p>
   * 
   * @param transition One of the transition constants.
   * @param stateName The state name of the successor.
   */
  final void putSuccessor(int transition, String stateName)
  {
    assert (transition >= TRANSITION_FIRST && transition <= TRANSITION_LAST) :
      "Transition identifier '" + transition + "' is unknown.";

    transitions.put(new Integer(transition), stateName);
  }

  /** <p>Retrieves a the state name of a successor for the given transition
   * constant.</p>
   * 
   * <p>Returns the default successor's state name if no special setup was
   * prepared.</p>
   * 
   * @param transition One of the transition constants.
   * @return The state name of the successor.
   */
  final String getSuccessor(int transition)
  {
    String state = (String) transitions.get(new Integer(transition));

    return (state == null) ? defaultSuccessor : state;
  }

  /**
   * Sets the name for the default successor state.
   * 
   * @param newDefaultSuccessor The default successor's state name.
   */
  final void setDefaultSuccessor(String newDefaultSuccessor)
  {
    defaultSuccessor = newDefaultSuccessor;
  }

  abstract void methodInvocation(String methodName);

  abstract void staticMethodInvocation(String className, String methodName);

  abstract void staticFieldAccess(String className, String fieldName);

  abstract void classResolution(String className);

  abstract void objectInstantiation(String className, ObjectId objectId);

  abstract void primitiveInstantiation(String primitiveName,
                                       String valueAsString);

  abstract void objectArrayInstantiation(String arrayClassName, String lengthAsString, ObjectId objectId);
  
  abstract void primitiveArrayInstantiation(String arrayClassName, String lengthAsString, ObjectId objectId);

  abstract void arraySet(String indexAsString);

  abstract void arrayGet(String indexAsString);

  abstract void listGet();

  abstract void listSet();

  abstract void nullObject();

  abstract void stringReference(String string);

  abstract void objectReference(ObjectId id);

  /**
   * <p>A special event that does not provoke a direct transition.</p>
   * 
   * <p>Instead the transition is done by the <code>ScanEngine</code>: It goes
   * back to the previous state and just uses this method to inform the state
   * about this happening.</p>
   */
  abstract void end();

  void enter()
  {
  }
  
}
