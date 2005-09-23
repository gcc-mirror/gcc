/* VMIdManager.java -- A reference/example implementation of a manager for
   JDWP object/reference type IDs

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

import gnu.classpath.jdwp.exception.InvalidClassException;
import gnu.classpath.jdwp.exception.InvalidObjectException;
import gnu.classpath.jdwp.id.*;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;

/**
 * This class manages objects and referencetypes that are reported
 * to the debugger. All objects and referencetypes reported to the
 * debugger should go through this manager.
 *
 * A brief summary of what an <code>IdManager</code> must provide:
 *
 * <code>
 * public ObjectId getObjectId (Object theObject);
 * public ObjectId get (long id);
 * public ObjectId readObjectId (ByteBuffer bb);
 * public ReferenceTypeId getReferenceTypeId (Class clazz);
 * public ReferenceTypeId getReferenceType (long id);
 * public ReferenceTypeId readReferenceTypeId (ByteBuffer bb);
 * </code>
 *
 * See the javadoc on these methods later in this file for more
 * information on these functions.
 *
 * <b>NOTE:</b> All IDs handled by the ID manager (all object and reference
 * type IDs) are assumed to be of type <code>long</code>.
 *
 * @author Keith Seitz  (keiths@redhat.com)
 */
public class VMIdManager
{
  // This factory generates ids for objects and types that may
  // be sent to a debugger.
  private static class IdFactory
  {
    // ID of last object / referencetype
    private static Object _idLock = new Object ();
    private static Object _ridLock = new Object ();
    private static long _lastId = 0;
    private static long _lastRid = 0;

    // A list of all ID types
    private static HashMap _idList = new HashMap ();

    // Initialize the id list with known types
    static
    {
      // ObjectId and ArrayId are special cases. See newObjectId.
      _idList.put (ClassLoaderId.typeClass, ClassLoaderId.class);
      _idList.put (ClassObjectId.typeClass, ClassObjectId.class);
      //_idList.put (FieldId.typeClass, FieldId.class);
      //_idList.put (FrameId.typeClass, FrameId.class);
      //_idList.put (MethodId.typeClass, MethodId.class);
      _idList.put (StringId.typeClass, StringId.class);
      _idList.put (ThreadId.typeClass, ThreadId.class);
      _idList.put (ThreadGroupId.typeClass, ThreadGroupId.class);
    }

    /**
     * Returns a new id for the given object
     *
     * @param object  the object for which an id is desired
     * @returns a suitable object id
     */
    public static ObjectId newObjectId (SoftReference obj)
    {
      ObjectId id = null;
      Object object = obj.get ();

      // Special case: arrays
      if (object.getClass ().isArray ())
	id = new ArrayId ();
      else
	{
	  // Loop through all classes until we hit baseclass
	  Class myClass;
	  for (myClass = object.getClass (); myClass != null;
	       myClass = myClass.getSuperclass ())
	    {
	      Class clz = (Class) _idList.get (myClass);
	      if (clz != null)
		{
		  try
		    {
		      id = (ObjectId) clz.newInstance ();
		      synchronized (_idLock)
			{
			  id.setId (++_lastId);
			}
		      return id;
		    }
		  catch (InstantiationException ie)
		    {
		      // This really should not happen
		      throw new RuntimeException ("cannot create new ID", ie);
		    }
		  catch (IllegalAccessException iae)
		    {
		      // This really should not happen
		      throw new RuntimeException ("illegal access of ID", iae);
		    }
		}
	    }

	  /* getSuperclass returned null and no matching ID type found.
	     So it must derive from Object. */
	  id = new ObjectId ();
	}

      synchronized (_idLock)
	{
	  id.setId (++_lastId);
	}

      return id;
    }

    /**
     * Returns a new reference type id for the given class
     *
     * @param clazz  the <code>Class</code> for which an id is desired
     * @returns a suitable reference type id or null when the
     * reference is cleared.
     */
    public static ReferenceTypeId newReferenceTypeId (SoftReference ref)
    {
      ReferenceTypeId id;
      Class clazz = (Class) ref.get ();
      if (clazz == null)
	return null;

      if (clazz.isArray ())
	id = new ArrayReferenceTypeId ();
      else if (clazz.isInterface ())
	id = new InterfaceReferenceTypeId ();
      else
	id = new ClassReferenceTypeId ();
      synchronized (_ridLock)
	{
	  id.setId (++_lastRid);
	}
      return id;
    }
  }

  /**
   * This class is a SoftReferenceIdentity type that is used by
   * the ID manager.
   */
  class ReferenceKey extends SoftReference
  {
    // Hash code of referent
    private int _hash;

    /**
     * Constructs a new <code>ReferenceKey</code> object
     * with the given referent.
     *
     * <p>This constructor should only be used for object lookups
     * by the backend.
     *
     * @param referent  the object to reference
     */
    public ReferenceKey (Object referent)
    {
      super (referent);
      _hash = referent.hashCode ();
    }
    
    /**
     * Constructs a new <code>ReferenceKey</code> object
     * with the given referent and reference queue.
     *
     * <p>The JDWP back-end stores a <code>ReferenceKey</code>
     * with its corresponding <code>JdwpId</code>. This constructor
     * is used by the back-end when adding new IDs to be managed.
     *
     * @param referent  the object to reference
     * @param queue     the queue to which to report garbage collections
     */
    public ReferenceKey (Object referent, ReferenceQueue queue)
    {
      super (referent, queue);
      _hash = referent.hashCode ();
    }
    
    /**
     * Returns the hash code of the referent.
     * This seems hacky, but is required in order to use this class
     * as a hash table key.
     *
     * @returns the hash code of the referent
     */
    public int hashCode ()
    {
      return _hash;
    }

    /**
     * Comparator for keys
     *
     * This method can be used in two ways:
     *
     * <ol>
     *    <li>For table lookups, where we want to compare referents</li>
     *    <li>For clearing GCd objects, where we want to compare the actual
     *        key object (not the referent)</li>
     * </ol>
     */
    public boolean equals (Object obj)
    {
      if (obj instanceof ReferenceKey)
	{
	  ReferenceKey ref = (ReferenceKey) obj;
	  
	  /* First check if the two references are the same.
	     If they are, that means we must be clearing GCd objects. */
	  if (this == obj)
	    return true;
	  
	  return (ref.get () == get ());
	}
      
      return false;
    }
  }
  
  // instance of VMIdManager
  private static VMIdManager _idm = new VMIdManager ();

  // A reference queue for our objects
  private ReferenceQueue _refQueue;

  // Mapping of objects (ReferenceKey) to IDs (ObjectId)
  private Hashtable _oidTable;

  // Mapping of ID numbers (Long) to IDs (ObjectId)
  private Hashtable _idTable;

  /* Mapping of class (ReferenceKey) to IDs (ReferenceTypeId) for reference
     types. Unlike other types, reference id types are NEVER released. */
  private Hashtable _classTable;

  // Mapping of ID numbers (Long) to reference type IDs (ReferenceTypeId)
  private Hashtable _ridTable;

  /**
   * Gets the instance of VMIdManager, constructing a new one
   * if none currently exists.
   */
  public static VMIdManager getDefault ()
  {
    return _idm;
  }

  // Constructs a new <code>IdManager</code>
  private VMIdManager ()
  {
    _refQueue = new ReferenceQueue ();
    _oidTable = new Hashtable (50);
    _idTable = new Hashtable (50);
    _classTable = new Hashtable (20);
    _ridTable = new Hashtable (20);
  }

  // Updates the object ID table, removing IDs whose objects have
  // been garbage collected.
  private void _update ()
  {
    Reference ref;
    while ((ref = _refQueue.poll ()) != null)
      {
	ObjectId id = (ObjectId) _oidTable.get (ref);
	_oidTable.remove (ref);
	_idTable.remove (new Long (id.getId ()));
      }
  }

  /**
   * Returns an id for the given object, adding it
   * if it does not have an id.
   *
   * @param theObject  the object to get an ID/add
   * @returns  the ID of the object
   */
  public ObjectId getObjectId (Object theObject)
  {
    ReferenceKey ref = new ReferenceKey (theObject, _refQueue);
    ObjectId id = (ObjectId) _oidTable.get (ref);
    if (id == null)
      {
	// update the tables -- this is an arbitrary place to put this
	_update ();

	// Object not found. Make new id for it
	id = IdFactory.newObjectId (ref);
	_oidTable.put (ref, id);
	_idTable.put (new Long (id.getId ()), id);
      }

    return id;
  }

  /**
   * Returns the <code>JdwpId</code> for a given ID. Unlike
   * <code>getId</code>, it throws an exception if the ID is not
   * known.
   *
   * @param id  the numerical ID of the desired <code>JdwpId</code>
   * @throws InvalidObjectException if the ID is not found
   */
  public ObjectId get (long id)
    throws InvalidObjectException
  {
    ObjectId oid = (ObjectId) _idTable.get (new Long (id));
    if (oid == null)
      throw new InvalidObjectException (id);
 
    return oid;
  }

  public ObjectId readObjectId (ByteBuffer bb)
    throws InvalidObjectException
  {
    long id = bb.getLong ();
    return get (id);
  }

  /**
   * Gets the reference type id for the given class, creating
   * a new one if it does not already have an id
   *
   * @param clazz  the class for which to get an ID
   * @returns  the ID of the class
   */
  public ReferenceTypeId getReferenceTypeId (Class clazz)
  {
    ReferenceKey ref = new ReferenceKey (clazz);
    ReferenceTypeId id = (ReferenceTypeId)_classTable.get (ref);
    if (id == null)
      {
	// Object not found. Make new id for it
	id = IdFactory.newReferenceTypeId (ref);
	_classTable.put (ref, id);
	_ridTable.put (new Long (id.getId ()), id);
      }

    return id;
  }

  /**
   * Returns the <code>ReferenceTypeId</code> for a given ID. Unlike
   * <code>getReferenceTypeId</code>, it throws an exception if the ID is not
   * known.
   *
   * @param id  the numerical ID of the desired reference type
   * @throws InvalidClassException if the ID is not found
   */
  public ReferenceTypeId getReferenceType (long id)
    throws InvalidClassException
  {
    ReferenceTypeId rid = (ReferenceTypeId) _ridTable.get (new Long (id));
    if (rid == null)
      throw new InvalidClassException (id);
 
    return rid;
  }

  public ReferenceTypeId readReferenceTypeId (ByteBuffer bb)
    throws InvalidClassException
  {
    long id = bb.getLong ();
    return getReferenceType (id);
  }
}
