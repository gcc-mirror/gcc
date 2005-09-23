/* JdwpConstants.java -- Constants defined by JDWP 1.4 specification
   Copyright (C) 2005 Free Software Foundation

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
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.classpath.jdwp;

/**
 * Constants defined by JDWP specification.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class JdwpConstants
{
  public static final class Version
  {
    public static final int MAJOR = 1;
    public static final int MINOR = 4;
  }
  
  ////////////////////////////////////////
  //           Commands/Sets            //
  ////////////////////////////////////////

  public static final class CommandSet
  {
    public static final class VirtualMachine
    {
      public static final byte CS_VALUE = 1;

      // commands
      public static final byte VERSION = 1;
      public static final byte CLASSES_BY_SIGNATURE = 2;
      public static final byte ALL_CLASSES = 3;
      public static final byte ALL_THREADS = 4;
      public static final byte TOP_LEVEL_THREAD_GROUPS = 5;
      public static final byte DISPOSE = 6;
      public static final byte IDSIZES = 7;
      public static final byte SUSPEND = 8;
      public static final byte RESUME = 9;
      public static final byte EXIT = 10;
      public static final byte CREATE_STRING = 11;
      public static final byte CAPABILITIES = 12;
      public static final byte CLASS_PATHS = 13;
      public static final byte DISPOSE_OBJECTS = 14;
      public static final byte HOLD_EVENTS = 15; 
      public static final byte RELEASE_EVENTS = 16;
      public static final byte CAPABILITIES_NEW = 17;
      public static final byte REDEFINE_CLASSES = 18;
      public static final byte SET_DEFAULT_STRATUM = 19;
      public static final byte ALL_CLASSES_WITH_GENERIC = 20;
    }

    public static final class ReferenceType
    {
      public static final byte CS_VALUE = 2;

      // commands
      public static final byte SIGNATURE= 1;
      public static final byte CLASS_LOADER= 2;
      public static final byte MODIFIERS = 3;
      public static final byte FIELDS = 4;
      public static final byte METHODS = 5; 
      public static final byte GET_VALUES = 6;
      public static final byte SOURCE_FILE = 7;
      public static final byte NESTED_TYPES = 8;
      public static final byte STATUS = 9;
      public static final byte INTERFACES= 10;
      public static final byte CLASS_OBJECT = 11;
      public static final byte SOURCE_DEBUG_EXTENSION = 12;
      public static final byte SIGNATURE_WITH_GENERIC = 13;
      public static final byte FIELDS_WITH_GENERIC = 14;
      public static final byte METHODS_WITH_GENERIC = 15;
    }

    public static final class ClassType
    {
      public static final byte CS_VALUE = 3;

      // commands
      public static final byte SUPERCLASS = 1;
      public static final byte SET_VALUES = 2;
      public static final byte INVOKE_METHOD = 3;
      public static final byte NEW_INSTANCE = 4;
    }

    public static final class ArrayType
    {
      public static final byte CS_VALUE = 4;

      // commands
      public static final byte NEW_INSTANCE = 1;
    }

    public static final class InterfaceType
    {
      public static final byte CS_VALUE = 5;

      // commands
    }

    public static final class Method
    {
      public static final byte CS_VALUE = 6;

      // commands
      public static final byte LINE_TABLE = 1;
      public static final byte VARIABLE_TABLE = 2;
      public static final byte BYTE_CODES = 3;
      public static final byte IS_OBSOLETE = 4;
      public static final byte VARIABLE_TABLE_WITH_GENERIC = 5;
    }

    public static final class Field
    {
      public static final byte CS_VALUE = 8;

      // commands
    }

    public static final class ObjectReference
    {
      public static final byte CS_VALUE = 9;

      // commands
      public static final byte REFERENCE_TYPE = 1;
      public static final byte GET_VALUES = 2;
      public static final byte SET_VALUES = 3;
      public static final byte MONITOR_INFO = 5;
      public static final byte INVOKE_METHOD = 6;
      public static final byte DISABLE_COLLECTION = 7;
      public static final byte ENABLE_COLLECTION = 8;
      public static final byte IS_COLLECTED = 9;
    }

    public static final class StringReference
    {
      public static final byte CS_VALUE = 10;

      // commands
      public static final byte VALUE = 1;
    }

    public static final class ThreadReference
    {
      public static final byte CS_VALUE = 11;

      // commands
      public static final byte NAME = 1;
      public static final byte SUSPEND = 2;
      public static final byte RESUME = 3;
      public static final byte STATUS = 4;
      public static final byte THREAD_GROUP = 5;
      public static final byte FRAMES = 6;
      public static final byte FRAME_COUNT = 7;
      public static final byte OWNED_MONITORS = 8;
      public static final byte CURRENT_CONTENDED_MONITOR = 9;
      public static final byte STOP = 10;
      public static final byte INTERRUPT = 11;
      public static final byte SUSPEND_COUNT = 12;
    }

    public static final class ThreadGroupReference
    {
      public static final byte CS_VALUE = 12;

      // commands
      public static final byte NAME = 1;
      public static final byte PARENT = 2;
      public static final byte CHILDREN = 3;
    }

    public static final class ArrayReference
    {
      public static final byte CS_VALUE = 13;

      // commands
      public static final byte LENGTH = 1;
      public static final byte GET_VALUES = 2;
      public static final byte SET_VALUES = 3;
    }

    public static final class ClassLoaderReference
    {
      public static final byte CS_VALUE = 14;

      // commands
      public static final byte VISIBLE_CLASSES = 1;
    }

    public static final class EventRequest
    {
      public static final byte CS_VALUE = 15;

      // commands
      public static final byte SET = 1;
      public static final byte CLEAR = 2;
      public static final byte CLEAR_ALL_BREAKPOINTS = 3;
    }

    public static final class StackFrame
    {
      public static final byte CS_VALUE = 16;

      // commands
      public static final byte GET_VALUES = 1;
      public static final byte SET_VALUES = 2;
      public static final byte THIS_OBJECT = 3;
      public static final byte POP_FRAMES = 4;
    }

    public static final class ClassObjectReference
    {
      public static final byte CS_VALUE = 17;

      // commands
      public static final byte REFLECTED_TYPE = 1;
    }

    public static final int MAXIMUM = ClassObjectReference.CS_VALUE;

    public static final class Event
    {
      public static final byte CS_VALUE = 64;

      // commands
      public static final byte COMPOSITE = 100;
    }
  }

  ////////////////////////////////////////
  //             Constants              //
  ////////////////////////////////////////

  /*
   * Error constants
   */
  public static final class Error
  {
    /**
     * No error has occurred
     */
    public static final short NONE = 0;

    /**
     * Passed thread is null, is not a valid thread or has exited
     */
    public static final short INVALID_THREAD = 10;

    /**
     * Thread group invalid
     */
    public static final short INVALID_THREAD_GROUP = 11;

    /**
     * Invalid priority
     */
    public static final short INVALID_PRIORITY = 12;

    /**
     * Specified thread has not been suspended by an event
     */
    public static final short THREAD_NOT_SUSPENDED = 13;

    /**
     * Thread already suspended
     */
    public static final short THREAD_SUSPENDED = 14;

    /**
     * Reference type has been unloaded and garbage collected
     */
    public static final short INVALID_OBJECT = 20;

    /**
     * Invalid class
     */
    public static final short INVALID_CLASS = 21;

    /**
     * Class has been loaded but not yet prepared
     */
    public static final short CLASS_NOT_PREPARED = 22;

    /**
     * Invalid method
     */
    public static final short INVALID_METHODID = 23;

    /**
     * Invalid location
     */
    public static final short INVALID_LOCATION = 24;

    /**
     * Invalid field
     */
    public static final short INVALID_FIELDID = 25;

    /**
     * Invaliid frame
     */
    public static final short INVALID_FRAMEID = 30;

    /**
     * There are no more Java or JNI frames on the call stack
     */
    public static final short NO_MORE_FRAMES = 31;

    /**
     * Information about the frame is not available
     */
    public static final short OPAQUE_FRAME = 32;

    /**
     * Operation can only be performed on current frame
     */
    public static final short NOT_CURRENT_FRAME = 33;

    /**
     * Variable is not an appropriate type for the function used
     */
    public static final short TYPE_MISMATCH = 34;

    /**
     * Invalid slot
     */
    public static final short INVALID_SLOT = 35;

    /**
     * Item already set
     */
    public static final short DUPLICATE = 40;

    /**
     * Desired element not found
     */
    public static final short NOT_FOUND = 41;

    /**
     * Invalid monitor
     */
    public static final short INVALID_MONITOR = 50;

    /**
     * Thread doesn't own the monitor
     */
    public static final short NOT_MONITOR_OWNER = 51;

    /**
     * Call has been interrupted before completion
     */
    public static final short INTERRUPT = 52;

    /**
     * Virtual machine attempted to read a class file and determined that
     * the file is malformed or otherwise cannot be interpreted as a class
     * file
     */
    public static final short INVALID_CLASS_FORMAT = 60;

    /**
     * Circularity has been detected while initializing a class
     */
    public static final short CIRCULAR_CLASS_DEFINITION = 61;

    /**
     * Verifier detected that a class file, though well formed, contained
     * some sort of internal inconsistency or security problem
     */
    public static final short FAILS_VERIFICATION = 62;

    /**
     * Adding methods has not been implemented
     */
    public static final short ADD_METHOD_NOT_IMPLEMENTED = 63;

    /**
     * Schema change has not been implemented
     */
    public static final short SCHEMA_CHANGE_NOT_IMPLEMENTED = 64;

    /**
     * State of the thread has been modified and is now inconsistent
     */
    public static final short INVALID_TYPESTATE = 65;

    /**
     * A direct superclass is different for the new class version, or the set
     * of directly implemented interfaces is different and
     * <code>canUnrestrictedlyRedefineClasses</code> is false
     */
    public static final short HIERARCHY_CHANGE_NOT_IMPLEMENTED = 66;

    /**
     * New class version does not declare a method declared in the old
     * class version and <code>canUnrestrictedlyRedefineClasses</code>
     * is false
     */
    public static final short DELETE_METHOD_NOT_IMPLEMENTED = 67;

    /**
     * Class file has a version number not supported by this VM
     */
    public static final short UNSUPPORTED_VERSION = 68;

    /**
     * Class name defined in the new class file is different from the name
     * in the old class object
     */
    public static final short NAMES_DONT_MATCH = 69;

    /**
     * New class version has different modifiers and
     * <code>canUnrestrictedlyRedefineClasses</code> is false
     */
    public static final short CLASS_MODIFIERS_CHANGE_NOT_IMPLEMENTED = 70;

    /**
     * A method in the new class version has different modifiers than its
     * counterpart in the old class version and
     * <code>canUnrestrictedlyRedefineClasses</code> is false.
     */
    public static final short METHOD_MODIFIERS_CHANGE_NOT_IMPLEMENTED = 71;

    /**
     * Functionality is not implemented in this virtual machine
     */
    public static final short NOT_IMPLEMENTED = 99;

    /**
     * Invalid pointer
     */
    public static final short NULL_POINTER = 100;

    /**
     * Desired information is not available
     */
    public static final short ABSENT_INFORMATION = 101;

    /**
     * Specified event type id is not recognized
     */
    public static final short INVALID_EVENT_TYPE = 102;

    /**
     * Illegal argument
     */
    public static final short ILLEGAL_ARGUMENT = 103;

    /**
     * The function needed to allocate memory and no more memory was
     * available for allocation
     */
    public static final short OUT_OF_MEMORY = 110;

    /**
     * Debugging has not been enabled in this virtual machine. JVMDI cannot
     * be used
     */
    public static final short ACCESS_DENIED = 111;

    /**
     * The virtual machine is not running
     */
    public static final short VM_DEAD = 112;

    /**
     * An unexpected internal error has occurred
     */
    public static final short INTERNAL = 113;

    /**
     * The thread being used to call this function is not attached to the
     * virtual machine. Calls must be made from attached threads.
     */
    public static final short UNATTACHED_THREAD = 115;

    /**
     * Invalid object type id or class tag
     */
    public static final short INVALID_TAG = 500;

    /**
     * Previous invoke not complete
     */
    public static final short ALREADY_INVOKING = 502;

    /**
     * Invalid index
     */
    public static final short INVALID_INDEX = 503;

    /**
     * Invalid length
     */
    public static final short INVALID_LENGTH = 504;

    /**
     * Invalid string
     */
    public static final short INVALID_STRING = 506;

    /**
     * Invalid class loader
     */
    public static final short INVALID_CLASS_LOADER = 507;

    /**
     * Invalid array
     */
    public static final short INVALID_ARRAY = 508;

    /**
     * Unable to load the transport
     */
    public static final short TRANSPORT_LOAD = 509;

    /**
     * Unablie to initialize the transport
     */
    public static final short TRANSPORT_INIT = 510;

    /**
     * Method is native
     */
    public static final short NATIVE_METHOD = 511;

    /**
     * Invalid count
     */
    public static final short INVALID_COUNT = 512;
  }

  /*
   * EventKind constants
   */
  public static final class EventKind
  {
    public static final byte SINGLE_STEP = 1;
    public static final byte BREAKPOINT = 2;
    public static final byte FRAME_POP = 3;
    public static final byte EXCEPTION = 4;
    public static final byte USER_DEFINED = 5;
    public static final byte THREAD_START = 6;
    public static final byte THREAD_END = 7;
    public static final byte CLASS_PREPARE = 8;
    public static final byte CLASS_UNLOAD = 9;
    public static final byte CLASS_LOAD = 10;
    public static final byte FIELD_ACCESS = 20;
    public static final byte FIELD_MODIFICATION = 21;
    public static final byte EXCEPTION_CATCH = 30;
    public static final byte METHOD_ENTRY = 40;
    public static final byte METHOD_EXIT = 41;
    public static final byte VM_INIT = 90;
    public static final byte VM_DEATH = 99;    
    public static final byte VM_DISCONNECTED = 100;

    public static final byte VM_START = VM_INIT;
    public static final byte THREAD_DEATH = THREAD_END;
  }

  /*
   * ModKind constants (event filters)
   */
  public static final class ModKind
  {
    /**
     * Limit the requested event to be reported at most once after a
     * given number of occurrences. May be used with any event.
     */
    public static final byte COUNT = 1;

    /**
     * Conditional on expression
     */
    public static final byte CONDITIONAL = 2;

    /**
     * Restricts reported events to those in the given thread.
     * May be used with any event except for class unload.
     */
    public static final byte THREAD_ONLY = 3;

    /**
     * For class prepare events, restricts generated events 
     * to be the preparation of the given reference type and any
     * subtypes.
     *
     * For other events, restricts the generated events to those where
     * location is in the given reference type or any of its subtypes.
     *
     * An event will be generated for any location in a reference type
     * that can be safely cast to the given reference type.
     *
     * May be used with any event except class unload, thread start,
     * and thread end.
     */
    public static final byte CLASS_ONLY = 4;

    /**
     * Restricts reported events to those for classes whose name matches
     * the given restricted regular expression.
     *
     * For class prepare events, the prepared class name is matched.
     * For class unload events, the unloaded class name is matched.
     * For other events, the class name of the event's location is matched.
     *
     * May be used with any event except thread start and thread end.
     */
    public static final byte CLASS_MATCH = 5;

    /**
     * Restricts reported events to those for classes whose name does not
     * match the given restricted regular expression.
     * 
     * For class prepare events, the prepared class name is matched.
     * For class unload events, the unloaded class name is matched.
     * For other events, the class name of the event's location is matched.
     *
     * May be used with any event except thread start and thread end.
     */
    public static final byte CLASS_EXCLUDE = 6;

    /**
     * Restricts reported events to those that occur at the given location.
     *
     * May be used with breakpoint, field access, field modification, step,
     * and exception event kinds.
     */
    public static final byte LOCATION_ONLY = 7;

    /**
     * Restricts reported exceptions by their class and whether they are
     * caught or uncaught.
     *
     * May be used with exception event kinds only.
     */
    public static final byte EXCEPTION_ONLY = 8;

    /**
     * Restricts reported events to those that occur for a given field.
     *
     * May be used with field access and field modification event kinds only.
     */
    public static final byte FIELD_ONLY = 9;

    /**
     * Restricts reported step events to those which satisfy depth and
     * size constraints.
     * 
     * May be used with step event kinds only.
     */
    public static final byte STEP = 10;

    /**
     * Restricts reported events to those whose active 'this' object is
     * the given object. Match value is the null object for static methods.
     *
     * May be used with any event except class prepare, class unload,
     * thread start, and thread end.
     */
    public static final byte INSTANCE_ONLY = 11;
  }

  /*
   * ThreadStatus constants
   */
  public static final class ThreadStatus
  {
    public static final int ZOMBIE = 0;
    public static final int RUNNING = 1;
    public static final int SLEEPING = 2;
    public static final int MONITOR = 3;
    public static final int WAIT = 4;
  }

  /*
   * SuspendStatus constants
   */
  public static final class SuspendStatus
  {
    public static final byte SUSPENDED = 1;
  }

  /*
   * ClassStatus constants
   */
  public static final class ClassStatus
  {
    public static final int VERIFIED = 1;
    public static final int PREPARED = 2;
    public static final int INITIALIZED = 4;
    public static final int ERROR = 8;
  }

  /*
   * TypeTag constants
   */
  public static final class TypeTag
  {
    public static final byte CLASS = 1;
    public static final byte INTERFACE = 2;
    public static final byte ARRAY = 3;
  }

  /*
   * Tag constants
   */
  public static final class Tag
  {
    /**
     * Array object (objectID size)
     */
    public static final byte ARRAY = '[';

    /**
     * Byte value (1 byte)
     */
    public static final byte BYTE = 'B';

    /**
     * Character value (2 bytes)
     */
    public static final byte CHAR = 'C';

    /**
     * Object (objectID size)
     */
    public static final byte OBJECT = 'L';

    /**
     * Float value (4 bytes)
     */
    public static final byte FLOAT = 'F';

    /**
     * Double value (8 bytes)
     */
    public static final byte DOUBLE = 'D';

    /**
     * Int value (4 bytes)
     */
    public static final byte INT = 'I';

    /**
     * Long value (8 bytes)
     */
    public static final byte LONG = 'J';

    /**
     * Short value (2 bytes)
     */
    public static final byte SHORT = 'S';

    /**
     * Void value (no bytes)
     */
    public static final byte  VOID = 'V';

    /**
     * Boolean value (1 byte)
     */
    public static final byte BOOLEAN = 'Z';

    /**
     * String object (objectID size)
     */
    public static final byte STRING = 's';

    /**
     * Thread object (objectID size)
     */
    public static final byte THREAD = 't';

    /**
     * ThreadGroup object (objectID size)
     */
    public static final byte THREAD_GROUP = 'g';

    /**
     * ClassLoader object (objectID size)
     */
    public static final byte CLASS_LOADER = 'l';

    /**
     * Class object object (objectID size)
     */
    public static final byte CLASS_OBJECT = 'c';
  }

  /*
   * StepDepth constants
   */
  public static final class StepDepth
  {
    /**
     * Step into any method calls that occur before the end of the step
     */
    public static final int INTO = 0;

    /**
     * Step over any method calls that occur before the end of the step
     */
    public static final int OVER = 1;

    /**
     * Step out of the current method
     */
    public static final int OUT = 2;
  }

  /*
   * StepSize constants
   */
  public static final class StepSize
  {
    /**
     * Step by the minimum possible amount (often a bytecode instruction)
     */
    public static final int MIN = 0;

    /**
     * Step to the next source line unless there is no line number information,
     * in which case MIN step is done instead
     */
    public static final int LINE = 1;
  }

  /*
   * SuspendPolicy constants
   */
  public static final class SuspendPolicy
  {
    /**
     * Suspend no threads when this event is encountered
     */
    public static final byte NONE = 0;

    /**
     * Suspend the event thread when this event is encountered
     */
    public static final byte EVENT_THREAD = 1;

    /**
     * Suspend all threads when this event is encountered
     */
    public static final byte ALL = 2;
  }

  /*
   * InvokeOptions flag constants
   */
  public static final class InvokeOptions
  {
    /**
     * otherwise, all threads started
     */
    public static final int INVOKE_SINGLE_THREADED = 0x1;

    /**
     * otherwise, normal virtual invoke (instance methods only)
     */
    public static final int INVOKE_NONVIRTUAL = 0x2;
  }
}
