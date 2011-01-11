/* ScanEngine.java
 -- Scans the input and generates an object tree that can be written as XML.
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

import java.beans.Expression;
import java.beans.Statement;
import java.io.OutputStream;
import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Stack;

/** <p>The <code>ScanEngine</code> is the main class of the backend of the
 * XML persistence algorithm. It scans {@link java.beans.Expression} and
 * {@link java.beans.Statement} instances and some raw objects via the
 * {@link #writeObject} method and feeds it to a state machine. The
 * state machine then constructs and object tree which is finally
 * written as XML by a {@link Writer} implementation.</p>
 *
 * <p>How does it work?</p>
 * <p>The <code>ScanEngine</code> sits below the {@link java.beans.XMLEncoder}
 * class and is called by it exclusively. The <code>XMLEncoder</code> sends
 * interpretive data by invoking {@link #writeExpression}, {@link #writeStatement}
 * and {@link #writeObject}. The invocations of <code>writeExpression</code> and
 * <code>writeStatement</code> are usually nested into each other and provide
 * more information then necessary to generate the XML representation.
 * Furthermore the meaning of certain <code>Expressions</code> differs
 * depending on the enclosing elements or the inner elements have to be
 * simply discarded.</p>
 *
 * <p>To cope with this state dependant nature the <code>ScanEngine</code>
 * contains a state machine which is programmed statically (no adjustments are
 * needed, all <code>ScanEngine</code> engines use the same setup). The
 * <code>ScanEngine</code>'s job is to decode the <code>Expression</code>s,
 * <code>Statement</code>s and certain objects (namely <code>String</code>,
 * <code>null</code> objects and instances which are repeatedly provided to
 * the encoder) into 13 low-level (event) methods, which denote the meaning of the
 * argument. For example an <code>Expression</code> can be an array
 * instantiation which provokes a call to {@link arrayInstantiation} or
 * it can be a class resolution leading to a call to {@link #classResolution}.
 * For the state machione the 13 methods are the distinct way to transit
 * from one state to another. Whenever the <code>ScanEngine</code> calls
 * one of the event methods the current's state successor for that event
 * is fetched from the state machine configuration, the successpr becomes
 * the current state and then the event method is called in the new current
 * state. The last step allows the state instance to do something meaningful
 * to the object tree.</p>
 *
 * <p>The state machine knows the concept of returning to the previous
 * state. This is done using a stack of states which is popped every
 * time a call to <code>writeStatement</code>, <code>writeExpression</code>
 * in the <code>XMLEncoder</code> ends by calling the {@link #end} method.
 * Note that due to the inheritance relationship of <code>Encoder</code>
 * and <code>XMLEncoder</code> it is impossible for the
 * <code>ScanEngine</code> itself to decide when an expression or statement
 * ended. This can only be done in case of {@link #writeObject} calls because
 * they are not nested.</p>
 *
 * <p>When the XML persistence mechanism reaches an object twice (and more)
 * it should generate an XML element using the "idref" attribute and add
 * an "id" attribute to its first instantiation. This complicates things a bit
 * because the first instantiation will always be part of the object tree
 * as some {@link gnu.java.beans.encoder.elements.Element} subclass instance when the
 * second and further objects accesses are written. Therefore the {@link ObjectId}
 * class was introduced which is shared between all the object tree elements
 * and has the notion of an "unused" state meaning that no identification
 * is needed. The relationship between an object and its <code>ObjectId</code>
 * instance is stored in the <code>ScanEngine</code> and gets cleared whenever
 * the {@link #flush} method is called. This method also writes the currently
 * built object tree and generates the XML representation.</p>
 *
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class ScanEngine
{

  /** Change this to true to let the ScanEngine print state transition
   * information.
   */
  boolean DEBUG = false;

  /**
   * Stores the scanner engine states as values and their names as keys.
   */
  HashMap states = new HashMap();

  /**
   * Stores former scanner state and makes it possible to come back to them.
   */
  Stack parents = new Stack();

  /**
   * The currently active scanner state.
   */
  ScannerState current;

  /**
   * The root of an object tree that is later written to XML.
   */
  Root root;

  /**
   * The Writer used to generate the XML output.
   */
  Writer writer;

  /** Stores the relationship between objects and their {@link ObjectId} instance.
   */
  IdentityHashMap objects = new IdentityHashMap();

  public ScanEngine(OutputStream os)
  {
    // TODO: Provide another Writer implementation (e.g. one that does not use
    // the XML APIs at all).
    writer = new StAXWriter(os);
    root = new Root();

    final ScannerState start = current = new GenericScannerState(root);
    ScannerState conf;

    // Use the ReportingScannerState to debug serialization issues.
    register(ScannerState.DEFAULT_STATE_NAME, new IgnoringScannerState());

    register("start", start);

    // Special dead-end state where all transitions are ignored.
    register("ignoreAll", new IgnoringScannerState())
      .setDefaultSuccessor("ignoreAll");

    // Object reference, string reference, null object
    start.putSuccessor(ScannerState.TRANSITION_OBJECT_REFERENCE, "simple");
    start.putSuccessor(ScannerState.TRANSITION_STRING_REFERENCE, "simple");
    start.putSuccessor(ScannerState.TRANSITION_NULL_OBJECT, "simple");
    register("simple", new GenericScannerState(root))
      .setDefaultSuccessor("ignoreAll");

    // Class resolution.
    start.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "classRes0");
    register("classRes0",
             new GenericScannerState(root)).setDefaultSuccessor("ignoreAll");

    // Object instantiation.
    start.putSuccessor(ScannerState.TRANSITION_OBJECT_INSTANTIATION,
                       "newObj0");
    conf = register("newObj0", new GenericScannerState(root));
    conf.setDefaultSuccessor("ignoreAll");

    // Simply use the start state to encode method invocations inside of
    // objects.
    conf.putSuccessor(ScannerState.TRANSITION_METHOD_INVOCATION, "start");

    // Primitive instantiations.
    start.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                       "newPrimitive0");
    register("newPrimitive0",
             new GenericScannerState(root)).setDefaultSuccessor("ignoreAll");

    // Object arrays use the ARRAY_GET transition to create setting the
    // array values.
    start.putSuccessor(ScannerState.TRANSITION_OBJECT_ARRAY_INSTANTIATION,
                       "newObjectArray");
    conf = register("newObjectArray", new GenericScannerState(root));
    conf.putSuccessor(ScannerState.TRANSITION_ARRAY_GET, "newOArrayGet");
    conf.putSuccessor(ScannerState.TRANSITION_ARRAY_SET, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                      "ignoreAll");

    // Get here when a value is set in the array.
    register("newOArrayGet",
             conf = new GenericScannerState(root));

    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                      "newOArrayGet_ignoreFirstInteger");

    // "newArrayGet_ignoreFirstInteger" is set up mostly identical like the "start"
    // state. Otherwise things would not behave the same when done inside
    // arrays.
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_REFERENCE, "simple");
    conf.putSuccessor(ScannerState.TRANSITION_STRING_REFERENCE, "simple");
    conf.putSuccessor(ScannerState.TRANSITION_NULL_OBJECT, "simple");
    conf.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "classRes0");
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_INSTANTIATION, "newObj0");
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_ARRAY_INSTANTIATION,
                      "newPrimitiveArray");
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_ARRAY_INSTANTIATION,
                      "newObjectArray");

    conf = register("newOArrayGet_ignoreFirstInteger",
                    new GenericScannerState(root, 1));

    // In non-int primitive arrays class resolutions can happen
    // but they should be ignored.
    conf.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "ignoreAll");

    // Spurious object and string references occur when setting array
    // elements. This suppresses them.
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                      "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_REFERENCE, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_STRING_REFERENCE, "ignoreAll");

    conf.setDefaultSuccessor("start");

    // Primitive arrays use the ARRAY_SET transition to create setting the
    // array values. This turned out to be the only working solution.
    // When primitive arrays were handled by ARRAY_GET the values in boolean
    // arrays were always skipped.
    start.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_ARRAY_INSTANTIATION,
                       "newPrimitiveArray");
    conf = register("newPrimitiveArray", new GenericScannerState(root));
    conf.putSuccessor(ScannerState.TRANSITION_ARRAY_GET, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_ARRAY_SET, "newPArraySet");
    conf.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                      "ignoreAll");

    conf = register("newPArraySet", new GenericScannerState(root));
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                      "newPArraySet_ignoreFirstInteger");

    // Primitive arrays ignore all kinds of non-primitive object information.
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_REFERENCE,
                      "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_STRING_REFERENCE, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_NULL_OBJECT, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "ingoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_INSTANTIATION, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_ARRAY_INSTANTIATION,
                      "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_ARRAY_INSTANTIATION,
                      "ignoreAll");

    conf = register("newPArraySet_ignoreFirstInteger",
                    new GenericScannerState(root, 1));

    // In non-int primitive arrays class resolutions can happen
    // but they should be ignored.
    conf.putSuccessor(ScannerState.TRANSITION_CLASS_RESOLUTION, "ignoreAll");

    // Spurious object and string references occur when setting array
    // elements. This suppresses them.
    conf.putSuccessor(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION,
                      "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_OBJECT_REFERENCE, "ignoreAll");
    conf.putSuccessor(ScannerState.TRANSITION_STRING_REFERENCE, "ignoreAll");
    conf.setDefaultSuccessor("start");

  }

  /** Registers a <code>ScannerState</code> under a certain name.
   *
   * @param name Name of the state
   * @param state The <code>ScannerState</code> instance.
   * @return The second argument.
   */
  private ScannerState register(String name, ScannerState state)
  {
    state.init(name);

    states.put(name, state);

    return state;
  }

  /** Generates or returns an id for the given object which can be activated
   * later if the object is suitable.
   *
   * <p>Objects are unsuitable if they are an instance of a primitive wrapper
   * or String.</p>
   *
   * @param value The object to retrieve an id for.
   * @return The id for the object or <code>null</code>.
   */
  private ObjectId retrieveId(Object value)
  {
    Class valueClass = value.getClass();
    ObjectId id = null;

    // Although multiple accesses to Class objects are not handled
    // through ids we generate one for them, too. This allows us to detect
    // second time references to such objects in the writeObject method
    // and handle them in a special way.
    if (valueClass != String.class
        && valueClass.getSuperclass() != Number.class
        && valueClass != Boolean.class)
      {
        if ((id = (ObjectId) objects.get(value)) == null)
          {
            id = new ObjectId(valueClass);
            objects.put(value, id);
          }
      }

    return id;
  }

  /** Scans the argument and calls one of event methods. See
   * the introduction of this class for details.
   *
   * @param expr The expression to serialize.
   */
  public void writeExpression(Expression expr)
  {
    String methodName = expr.getMethodName();
    Object[] args = expr.getArguments();
    Object target = expr.getTarget();
    Object value = null;

    try
      {
        value = expr.getValue();
      }
    catch (Exception e)
      {
        throw (InternalError)
          new InternalError(
          "The Expression's value should be available at this point.")
          .initCause(e);
      }

    // TODO: What if the value is null?
    ObjectId id;
    Class valueClass = value.getClass();

    if (target == Array.class)
      {
        if (methodName.equals("newInstance"))
          {
            id = retrieveId(value);

            Class ct = (Class) args[0];

            if (ct.isPrimitive() || ct == Boolean.class || ct == Byte.class
                || ct == Short.class || ct == Integer.class || ct == Long.class
                || ct == Float.class || ct == Double.class)
              primitiveArrayInstantiation(ct.getName(),
                                          args[1].toString(),
                                          id);
            else
              objectArrayInstantiation(ct.getName(),
                                       args[1].toString(),
                                       id);

            return;
          }
        else if (methodName.equals("get"))
          {
            arrayGet(args[1].toString());

            // The encoder does not call the ScanEngine
            // when an object is serialized that we already know.
            // We test for this situation and insert the object reference
            // manually.
            // Since there is already a workaround for the Class class
            // in writeObject we have to except it from this behavior.
            id = (ObjectId) objects.get(value);
            if (id != null && valueClass != Class.class)
              {
                objectReference(id);
                end();
              }

            return;
          }
        else if (methodName.equals("set"))
          {
            arraySet(args[1].toString());
            return;
          }
      }

    id = retrieveId(value);

    if (target instanceof Class)
      {
        if (methodName.equals("new"))
          {
            Class targetClass = (Class) target;

            // All primitive types have short-hand forms for their
            // constructors.
            if (valueClass == Boolean.class)
              primitiveInstantiation("boolean", args[0].toString());
            else if (valueClass == Byte.class)
              primitiveInstantiation("byte", args[0].toString());
            else if (valueClass == Short.class)
              primitiveInstantiation("short", args[0].toString());
            else if (valueClass == Integer.class)
              primitiveInstantiation("int", args[0].toString());
            else if (valueClass == Long.class)
              primitiveInstantiation("long", args[0].toString());
            else if (valueClass == Float.class)
              primitiveInstantiation("float", args[0].toString());
            else if (valueClass == Double.class)
              primitiveInstantiation("double", args[0].toString());
            else
              objectInstantiation(targetClass.getName(), id);

            return;
          }
        else if (value instanceof Class)
          {
            String className = ((Class) value).getName();

            // At this point we know that some *static* method will be called.

            if (methodName.equals("forName"))
              {
                // However "Class.forName" represents class resolution and has a
                // special syntax.
                classResolution(className);
                return;
              }
            else if (methodName.equals("getField"))
              {
                // The same goes for "Class.getField".
                // Note: The name of the wanted field is given in
                // the argument array.
                staticFieldAccess(className, args[0].toString());
                return;
              }
            else
              {
                // If nothing fits it is just a static method
                // invocation which we decode as such.
                staticMethodInvocation(className, methodName);
                return;
              }
          }
      }
    else if (target instanceof List)
      {
        // Special behavior for indexed get and set method for list-style
        // classes.
        // The arguments are in the args array but we need them as subelements.
        if (methodName.equals("get"))
          {
            listGet();
            return;
          }
        else if (methodName.equals("set"))
          {
            listSet();
            return;
          }
      }

    // If nothing else could be used then this is a normal
    // method invocation.
    methodInvocation(methodName);
  }

  /**
   * Ends the current state and returns to the last one.
   */
  public void end()
  {
    current.end();

    if (DEBUG) System.err.print("back from " + current.getName());

    ScannerState oldCurrent = current;
    current = (ScannerState) parents.pop();

    if (DEBUG) System.err.println(" to " + current.getName());
  }

  /**
   * Returns to the last state and deletes the last element in the object tree.
   */
  public void revoke()
  {
    ScannerState oldCurrent = current;
    current = (ScannerState) parents.pop();

    root.deleteLast();
  }

  /** Scans the argument and calls one of event methods. See
   * the introduction of this class for details.
   *
   * @param stmt The statement to serialize.
   */
  public void writeStatement(Statement stmt)
  {
    // This is a simplified version of writeExpression. Everything
    // that would not create something that is embedded in a <void> tag
    // is left out (instantiation, getters, ...).
    // TODO: Is this the right thing to do?

    String methodName = stmt.getMethodName();
    Object target = stmt.getTarget();
    Object[] args = stmt.getArguments();

    if (target == Array.class && methodName.equals("set"))
      {
        arraySet(args[1].toString());
        return;
      }

    if (target instanceof List)
      {
        if (methodName.equals("set"))
          {
            listSet();
            return;
          }
      }

    // If nothing else could be used then this is a normal
    // method invocation.
    methodInvocation(methodName);
  }

  /** Scans the argument and calls one of event methods. See
   * the introduction of this class for details.
   *
   * @param o The object to serialize.
   */
  public boolean writeObject(Object o)
  {
    ObjectId id = null;

    if (o == null)
      {
        // Handle null objects which have a special syntax.
        nullObject();
        end();
      }
    else if (o.getClass() == String.class)
      {
        // Handle strings which are treated extremely special
        // in the encoder (they are never converted into a
        // Expression).
        stringReference((String) o);
        end();
      }
    else if ((id = (ObjectId) objects.get(o)) != null)
      {
        // Multiple references to a Class object do not generate
        // an object reference but we use the id to detect that
        // situation.
        if (o.getClass() == Class.class)
          {
            classResolution(((Class) o).getName());
            end();
            return false;
          }

        // If our object has a corresponding ObjectId instance
        // then generate an objectReference. This will
        // initialize the id (= brings it in the "used" state)
        // when this is the first referal.
        objectReference(id);
        end();
        return false;
      }

    return true;
  }

  /**
   * Writes the currently constructed object tree out as
   * XML and clears the object to {@link ObjectId} relations.
   */
  public void flush()
  {
    // Make all references unreachable. That means we have to generate
    // new object ids.
    objects.clear();

    root.traverse(writer);
  }

  /** Writes the final bits if the object tree and closes the stream
   * afterwards.
   */
  public void close()
  {
    flush();
    root.close(writer);
  }

  /**
   * Does a transition from one state to another using the given event.
   *
   * <p>This involves saving the current state, retrieving it's
   * successor and setting it as the current state.</p>
   *
   * @param transition One of {@link ScannerStates]'s transition constants.
   */
  private void transition(int transition)
  {
    parents.push(current);

    String stateName = current.getSuccessor(transition);

    if (DEBUG)
      {
        System.err.println("from state: " + current.getName() + "\n\troute: "
                           + ScannerState.transitionNames[transition]
                           + "\n\t\tto state: "
                           + stateName);
      }

    ScannerState newState = (ScannerState) states.get(stateName);

    newState.enter(new Context(current.getName(), current.getCalls()));

    assert (newState != null) : "State '" + stateName + "' was not defined.";

    current = newState;
  }

  /** Event method that denotes a (non-static) method invocation.
   *
   * <p>More details about this method can be found in this
   * class' introduction.</p>
   *
   * @param methodName The name of the method which is called.
   */
  void methodInvocation(String methodName)
  {
    transition(ScannerState.TRANSITION_METHOD_INVOCATION);

    current.methodInvocation(methodName);
  }

  /** Event method that denotes a static method invocation.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param methodName The name of the method which is called.
  * @param className The name of the class in which the method is called.
  */
  void staticMethodInvocation(String className, String methodName)
  {
    transition(ScannerState.TRANSITION_STATIC_METHOD_INVOCATION);

    current.staticMethodInvocation(className, methodName);
  }

  /** Event method that denotes the retrieval of a static field's value.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param fieldName The name of the field whose value is retrieved.
  * @param className The name of the class in which the method is called.
  */
  void staticFieldAccess(String className, String fieldName)
  {
    transition(ScannerState.TRANSITION_STATIC_FIELD_ACCESS);

    current.staticFieldAccess(className, fieldName);
  }

  /** Event method that denotes the resolution of a class.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param className The name of the class in which the method is called.
  */
  void classResolution(String className)
  {
    transition(ScannerState.TRANSITION_CLASS_RESOLUTION);

    current.classResolution(className);
  }

  /** Event method that denotes the instantiation of an object.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param className The name of the class in which the method is called.
  * @param objectId An ObjectId instance which can be activated later.
  */
  void objectInstantiation(String className, ObjectId objectId)
  {
    transition(ScannerState.TRANSITION_OBJECT_INSTANTIATION);

    current.objectInstantiation(className, objectId);
  }

  /** Event method that denotes the instantiation of a primitive.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param primitiveName One of "boolean, "byte", "short", "int", "long"
  * , "float" or "double"
  * @param valueAsString The value of the primitive as a String.
  */
  void primitiveInstantiation(String primitiveName, String valueAsString)
  {
    transition(ScannerState.TRANSITION_PRIMITIVE_INSTANTIATION);

    current.primitiveInstantiation(primitiveName, valueAsString);
  }

  /** Event method that denotes the instantiation of an object array.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param arrayClassName The array's class name.
  * @param objectId An ObjectId instance which can be activated later.
  * @param lengthAsString The array's length as String.
  */
  void objectArrayInstantiation(String arrayClassName, String lengthAsString,
                          ObjectId objectId)
  {
    transition(ScannerState.TRANSITION_OBJECT_ARRAY_INSTANTIATION);

    current.objectArrayInstantiation(arrayClassName, lengthAsString, objectId);
  }

  /** Event method that denotes the instantiation of a primitive array.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param arrayClassName The array's class name.
  * @param objectId An ObjectId instance which can be activated later.
  * @param lengthAsString The array's length as String.
  */
  void primitiveArrayInstantiation(String arrayClassName, String lengthAsString,
                                ObjectId objectId)
  {
    transition(ScannerState.TRANSITION_PRIMITIVE_ARRAY_INSTANTIATION);

    current.objectArrayInstantiation(arrayClassName, lengthAsString, objectId);
  }

  /** Event method that denotes the setting of a value in an array.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param indexAsString The index to as a String.
  */
  void arraySet(String indexAsString)
  {
    transition(ScannerState.TRANSITION_ARRAY_SET);

    current.arraySet(indexAsString);
  }

  /** Event method that denotes the retrieval of a value in an array.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  *
  * @param indexAsString The index to as a String.
  */
  void arrayGet(String indexAsString)
  {
    transition(ScannerState.TRANSITION_ARRAY_GET);

    current.arrayGet(indexAsString);
  }

  /** Event method that denotes the setting of a value in a list.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  */
  void listSet()
  {
    transition(ScannerState.TRANSITION_LIST_SET);

    current.listSet();
  }

  /** Event method that denotes the retrieval of a value in a list.
  *
  * <p>More details about this method can be found in this
  * class' introduction.</p>
  */
  void listGet()
  {
    transition(ScannerState.TRANSITION_LIST_GET);

    current.listGet();
  }

  /** Event method that denotes the null value.
  */
  void nullObject()
  {
    transition(ScannerState.TRANSITION_NULL_OBJECT);

    current.nullObject();
  }

  /** Event method that denotes a string.
   *
   * @param string The string that should be written.
   */
  void stringReference(String string)
  {
    transition(ScannerState.TRANSITION_STRING_REFERENCE);

    current.stringReference(string);
  }

  /** Event method that denotes a reference to an existing object.
   *
   * @param id The ObjectId to be used.
   */
  void objectReference(ObjectId id)
  {
    transition(ScannerState.TRANSITION_OBJECT_REFERENCE);

    current.objectReference(id);
  }

}
