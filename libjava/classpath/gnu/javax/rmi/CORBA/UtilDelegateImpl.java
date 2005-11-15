/* UtilDelegateImpl.java --
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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


package gnu.javax.rmi.CORBA;

import gnu.CORBA.Minor;
import gnu.CORBA.ObjectCreator;
import gnu.CORBA.Poa.ORB_1_4;
import gnu.CORBA.Poa.AOM;
import gnu.CORBA.Poa.gnuPOA;
import gnu.CORBA.typecodes.GeneralTypeCode;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.COMM_FAILURE;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.INVALID_TRANSACTION;
import org.omg.CORBA.INV_OBJREF;
import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.NO_PERMISSION;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.OMGVMCID;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TRANSACTION_REQUIRED;
import org.omg.CORBA.TRANSACTION_ROLLEDBACK;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.UNKNOWN;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.rmi.AccessException;
import java.rmi.MarshalException;
import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.ServerError;
import java.rmi.ServerException;
import java.rmi.UnexpectedException;
import java.rmi.server.RMIClassLoader;
import java.util.Hashtable;

import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.Tie;
import javax.rmi.CORBA.Util;
import javax.rmi.CORBA.UtilDelegate;
import javax.rmi.CORBA.ValueHandler;
import javax.transaction.InvalidTransactionException;
import javax.transaction.TransactionRequiredException;
import javax.transaction.TransactionRolledbackException;

/**
 * The implementation of UtilDelegate.
 * 
 * @author Wu Gansha (gansha.wu@intel.com) (stub)
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org) (implementation)
 */
public class UtilDelegateImpl
  extends RmiUtilities
  implements UtilDelegate
{
  /**
   * The instance of the value handler, requested once.
   */
  static ValueHandler m_ValueHandler;

  /**
   * The global map of all ties to they records.
   */
  static Hashtable m_Ties = new Hashtable();

  /**
   * The global map of all targets to they records.
   */
  static Hashtable m_Targets = new Hashtable();

  /**
   * The standard package for that the exception names are omitted.
   */
  static final String m_StandardPackage = "org.omg.CORBA.";

  /**
   * Make a deep copy of the object.
   */
  public Object copyObject(Object obj, ORB orb)
    throws RemoteException
  {
    // Strings are immutable, can be shared.
    if (obj instanceof String)
      return obj;
    else if (obj == null)
      return null;
    else if (obj instanceof String[] || obj instanceof String[][]
      || obj instanceof String[][][])
      {
        // String arrays can be just cloned.
        return ((Object[]) obj).clone();
      }
    else if (obj instanceof Serializable)
      {
        try
          {
            ByteArrayOutputStream a = new ByteArrayOutputStream();
            ObjectOutputStream ou = new ObjectOutputStream(a);
            ou.writeObject(obj);
            ou.close();
            ObjectInputStream input = new ObjectInputStream(
              new ByteArrayInputStream(a.toByteArray()));
            return input.readObject();
          }
        catch (Exception ex)
          {
            RemoteException rex = new RemoteException("Cannot copy " + obj);
            throw rex;
          }
      }
    else
      return obj;
  }

  /**
   * Make a deep copy of the object array.
   */
  public Object[] copyObjects(Object[] obj, ORB orb)
    throws RemoteException
  {
    return (Object[]) copyObject(obj, orb);
  }

  public ValueHandler createValueHandler()
  {
    if (m_ValueHandler == null)
      m_ValueHandler = (ValueHandler) DelegateFactory.getInstance(DelegateFactory.VALUEHANDLER);
    return m_ValueHandler;
  }

  /**
   * Returns the codebase of the given class.
   */
  public String getCodebase(Class clz)
  {
    return RMIClassLoader.getClassAnnotation(clz);
  }

  /**
   * Get the Tie that handles invocations on the given target. If the target/Tie
   * pair has not been previously registered using {@link #registerTarget},
   * this method tries to locate a tie class by the name pattern. If this
   * succeeds, the tie-target pair is also registered.
   * 
   * @return the Tie.
   */
  public Tie getTie(Remote target)
  {
    synchronized (m_Targets)
      {
        Tie tie;
        TieTargetRecord r = ((TieTargetRecord) m_Targets.get(target));
        if (r == null)
          {
            if (target instanceof Stub)
              {
                tie = StubDelegateImpl.getTieFromStub(target);
                registerTarget(tie, target);
              }
            else
              {
                // Treat this as implementation.
                String tieClassName = getTieClassName(target.getClass().getName());
                try
                  {
                    Class tieClass = Util.loadClass(tieClassName, null,
                      target.getClass().getClassLoader());
                    tie = (Tie) tieClass.newInstance();
                  }
                catch (Exception e)
                  {
                    MARSHAL m = new MARSHAL("Unable to instantiate "
                      + tieClassName);
                    m.minor = Minor.TargetConversion;
                    m.initCause(e);
                    throw m;
                  }
                tie.setTarget(target);
                registerTarget(tie, target);
              }
          }
        else
          tie = r.tie;
        return tie;
      }
  }

  /**
   * Get the Stub class name for the name, representing the given interface.
   */
  private String getTieClassName(String interf)
  {
    String stubClassName;
    int p = interf.lastIndexOf('.');

    if (p < 0)
      // The interface is defined in the default package.
      stubClassName = "_" + interf + "_Tie";
    else
      stubClassName = interf.substring(0, p + 1) + "_"
        + interf.substring(p + 1) + "_Tie";
    return stubClassName;
  }

  /**
   * Register the Tie-target pair. As the Tie is a Servant, it can potentially
   * be connected to several objects and hence may be registered with several
   * targets.
   */
  public void registerTarget(Tie tie, Remote target)
  {
    synchronized (m_Ties)
      {
        synchronized (m_Targets)
          {
            TieTargetRecord r = (TieTargetRecord) m_Ties.get(tie);
            if (r == null)
              {
                // First registration for this Tie.
                r = new TieTargetRecord(tie);
                m_Ties.put(tie, r);
              }
            if (target != null)
              {
                r.add(target);
                m_Targets.put(target, r);
              }
          }
      }
  }

  /**
   * Deactivate the associated Tie, if it is found and is not connected to other
   * registered targets. Independing from the POA policies, the transparent
   * reactivation will not be possible.
   */
  public void unexportObject(Remote target)
    throws NoSuchObjectException
  {
    synchronized (m_Ties)
      {
        synchronized (m_Targets)
          {
            TieTargetRecord r = ((TieTargetRecord) m_Targets.get(target));
            if (r != null)
              {
                if (target instanceof org.omg.CORBA.Object)
                  r.tie.orb().disconnect((org.omg.CORBA.Object) target);

                if (r.unused())
                  {
                    m_Targets.remove(target);
                    m_Ties.remove(r.tie);
                    r.tie.deactivate();

                    if (r.tie.orb() instanceof ORB_1_4)
                      {
                        // Standard case, when more deep cleanup is possible.
                        // Independing from the POA policies, the object will
                        // not be activable transparently.
                        ORB_1_4 orb = (ORB_1_4) r.tie.orb();

                        if (target instanceof org.omg.CORBA.Object)
                          {
                            AOM.Obj record = orb.rootPOA.findObject((org.omg.CORBA.Object) target);

                            if (record != null && record.servant == r.tie
                              && record.poa instanceof gnuPOA)
                              {
                                ((gnuPOA) record.poa).aom.remove(record.key);
                                record.deactivated = true;
                                record.servant = null;
                              }
                          }
                      }
                  }
              }
          }
      }
  }

  /**
   * Checks if the given stub is local.
   * 
   * @param stub a stub to check.
   * @return true if the stub is local, false otherwise.
   */
  public boolean isLocal(Stub stub)
    throws RemoteException
  {
    try
      {
        return stub._is_local();
      }
    catch (SystemException e)
      {
        RemoteException rex = new RemoteException();
        rex.initCause(e);
        throw rex;
      }
  }

  /**
   * Load the class. The method uses class loaders from the call stact first. If
   * this fails, the further behaviour depends on the System Property
   * "java.rmi.server.useCodebaseOnly" with default value "false".
   * 
   * <ul>
   * <li>Try the current thread context class loader first.</li>
   * <li>If remoteCodebase is non-null and useCodebaseOnly is "false" then call
   * java.rmi.server.RMIClassLoader.loadClass (remoteCodebase, className)</li>
   * <li> If remoteCodebase is null or useCodebaseOnly is true then call
   * java.rmi.server.RMIClassLoader.loadClass(className)</li>
   * <li>If a class is still not successfully loaded and the loader != null
   * then try Class.forName(className, false, loader). </li>
   * </ul>
   * 
   * @param className the name of the class.
   * @param remoteCodebase the codebase.
   * @param loader the class loader.
   * @return the loaded class.
   * 
   * @throws ClassNotFoundException of the class cannot be loaded.
   */
  public Class loadClass(String className, String remoteCodebase,
    ClassLoader loader)
    throws ClassNotFoundException
  {
    if (loader == null)
      loader = Thread.currentThread().getContextClassLoader();

    String p_useCodebaseOnly = System.getProperty("java.rmi.server.useCodebaseOnly");

    boolean useCodebaseOnly = p_useCodebaseOnly != null
      && p_useCodebaseOnly.trim().equalsIgnoreCase("true");

    try
      {
        if (remoteCodebase != null && !useCodebaseOnly)
          return RMIClassLoader.loadClass(remoteCodebase, className);
      }
    catch (Exception e)
      {
        // This failed but try others.
      }

    try
      {
        if (remoteCodebase == null || useCodebaseOnly)
          return RMIClassLoader.loadClass(remoteCodebase, className);
      }
    catch (Exception e)
      {
        // This failed but try others.
      }

    if (loader != null)
      return Class.forName(className, true, loader);

    throw new ClassNotFoundException(className + " at " + remoteCodebase);
  }

  /**
   * Converts CORBA {@link SystemException} into RMI {@link RemoteException}.
   * The exception is converted as defined in the following table:
   * <p>
   * <table border = "1">
   * <tr>
   * <th>CORBA Exception</th>
   * <th>RMI Exception</th>
   * </tr>
   * <tr>
   * <td>{@link COMM_FAILURE}</td>
   * <td>{@link MarshalException}</td>
   * </tr>
   * <tr>
   * <td>{@link INV_OBJREF}</td>
   * <td>{@link  NoSuchObjectException}</td>
   * </tr>
   * <tr>
   * <td>{@link NO_PERMISSION}</td>
   * <td>{@link  AccessException}</td>
   * </tr>
   * <tr>
   * <td>{@link MARSHAL}</td>
   * <td>{@link  MarshalException}</td>
   * </tr>
   * <tr>
   * <td>{@link BAD_PARAM} (all other cases)</td>
   * <td>{@link  MarshalException}</td>
   * </tr>
   * <tr>
   * <td>{@link OBJECT_NOT_EXIST}</td>
   * <td>{@link  NoSuchObjectException}</td>
   * </tr>
   * <tr>
   * <td>{@link TRANSACTION_REQUIRED}</td>
   * <td>{@link  TransactionRequiredException}</td>
   * </tr>
   * <tr>
   * <td>{@link TRANSACTION_ROLLEDBACK}</td>
   * <td>{@link  TransactionRolledbackException}</td>
   * </tr>
   * <tr>
   * <td>{@link INVALID_TRANSACTION}</td>
   * <td>{@link  InvalidTransactionException}</td>
   * </tr>
   * <tr>
   * <td bgcolor="lightgray">Any other {@link SystemException}</td>
   * <td bgcolor="lightgray">{@link RemoteException}</td>
   * </tr>
   * </table>
   * </p>
   * <p>
   * The exception detailed message always consists of
   * <ol>
   * <li>the string "CORBA "</li>
   * <li>the CORBA name of the system exception</li>
   * <li>single space</li>
   * <li>the hexadecimal value of the system exception's minor code, preceeded
   * by 0x (higher bits contain {@link OMGVMCID}).</li>
   * <li>single space</li>
   * <li>the {@link CompletionStatus} of the exception: "Yes", "No" or "Maybe".</li>
   * </ol>
   * <p>
   * For instance, if the Internet connection was refused:
   * </p>
   * <p>
   * <pre>
   * <code>CORBA COMM_FAILURE 0x535500C9 No</code>
   * </p>
   * <p>
   * The original CORBA exception is set as the cause of the RemoteException
   * being created.
   * </p>
   */
  public RemoteException mapSystemException(SystemException ex)
  {
    RemoteException rex;

    String status;

    switch (ex.completed.value())
      {
        case CompletionStatus._COMPLETED_MAYBE:
          status = "Maybe";
          break;

        case CompletionStatus._COMPLETED_NO:
          status = "No";
          break;

        case CompletionStatus._COMPLETED_YES:
          status = "Yes";
          break;

        default:
          status = "Unexpected completion status " + ex.completed.value();
      }

    String name = ex.getClass().getName();

    if (name.startsWith(m_StandardPackage))
      name = name.substring(m_StandardPackage.length());

    String message = "CORBA " + name + " 0x" + Integer.toHexString(ex.minor)
      + " " + status;

    if (ex instanceof COMM_FAILURE)
      rex = new MarshalException(message, ex);
    else if (ex instanceof INV_OBJREF)
      {
        rex = new NoSuchObjectException(message);
        rex.detail = ex;
      }
    else if (ex instanceof NO_PERMISSION)
      rex = new AccessException(message, ex);
    else if (ex instanceof MARSHAL)
      rex = new MarshalException(message, ex);
    else if (ex instanceof BAD_PARAM)
      rex = new MarshalException(message, ex);
    else if (ex instanceof OBJECT_NOT_EXIST)
      {
        rex = new NoSuchObjectException(message);
        rex.detail = ex;
      }
    else if (ex instanceof TRANSACTION_REQUIRED)
      {
        rex = new TransactionRequiredException(message);
        rex.detail = ex;
      }
    else if (ex instanceof TRANSACTION_ROLLEDBACK)
      {
        rex = new TransactionRolledbackException(message);
        rex.detail = ex;
      }
    else if (ex instanceof INVALID_TRANSACTION)
      {
        rex = new InvalidTransactionException(message);
        rex.detail = ex;
      }
    else if (ex instanceof UNKNOWN)
      rex = wrapException(ex.getCause());
    else
      rex = new RemoteException(message, ex);

    return rex;
  }

  /**
   * Converts the exception that was thrown by the implementation method on a
   * server side into RemoteException that can be transferred and re-thrown on a
   * client side. The method converts exceptions as defined in the following
   * table: <table border = "1">
   * <tr>
   * <th>Exception to map (or subclass)</th>
   * <th>Maps into</th>
   * </tr>
   * <tr>
   * <td>{@link Error}</td>
   * <td>{@link ServerError}</td>
   * </tr>
   * <tr>
   * <td>{@link RemoteException}</td>
   * <td>{@link ServerException}</td>
   * </tr>
   * <tr>
   * <td>{@link SystemException}</td>
   * <td>wrapException({@link #mapSystemException})</td>
   * </tr>
   * <tr>
   * <td>{@link RuntimeException}</td>
   * <td><b>rethrows</b></td>
   * </tr>
   * <tr>
   * <td>Any other exception</td>
   * <td>{@link UnexpectedException}</td>
   * </tr>
   * </table>
   * 
   * @param ex an exception that was thrown on a server side implementation.
   * 
   * @return the corresponding RemoteException unless it is a RuntimeException.
   * 
   * @throws RuntimeException the passed exception if it is an instance of
   * RuntimeException.
   * 
   * @specnote It is the same behavior, as in Suns implementations 1.4.0-1.5.0.
   */
  public RemoteException wrapException(Throwable ex)
    throws RuntimeException
  {
    if (ex instanceof RuntimeException)
      throw (RuntimeException) ex;
    else if (ex instanceof Error)
      return new ServerError(ex.getMessage(), (Error) ex);
    else if (ex instanceof RemoteException)
      return new ServerException(ex.getMessage(), (Exception) ex);
    else if (ex instanceof SystemException)
      return wrapException(mapSystemException((SystemException) ex));
    else
      return new UnexpectedException("Unexpected", (Exception) ex);
  }

  /**
   * Write abstract interface to the CORBA output stream. The write format is
   * matching CORBA abstract interface. Remotes and CORBA objects are written as
   * objects, other classes are supposed to be value types and are written as
   * such. {@link Remote}s are processed as defined in
   * {@link #writeRemoteObject}. The written data contains discriminator,
   * defining, that was written. Another method that writes the same content is
   * {@link org.omg.CORBA_2_3.portable.OutputStream#write_abstract_interface(java.lang.Object)}.
   * 
   * @param output a stream to write to, must be
   * {@link org.omg.CORBA_2_3.portable.OutputStream}.
   * 
   * @param object an object to write, must be CORBA object, Remote
   */
  public void writeAbstractObject(OutputStream output, Object object)
  {
    ((org.omg.CORBA_2_3.portable.OutputStream) output).write_abstract_interface(object);
  }

  /**
   * Write the passed java object to the output stream in the form of the CORBA
   * {@link Any}. This includes creating an writing the object {@link TypeCode}
   * first. Such Any can be later read by a non-RMI-IIOP CORBA implementation
   * and manipulated, for instance, by means, provided in
   * {@link org.omg.DynamicAny.DynAny}. Depending from the passed value, this
   * method writes CORBA object, value type or value box. For value types Null
   * is written with the abstract interface, its typecode having repository id
   * "IDL:omg.org/CORBA/AbstractBase:1.0" and the empty string name.
   * 
   * @param output the object to write.
   * @param object the java object that must be written in the form of the CORBA
   * {@link Any}.
   */
  public void writeAny(OutputStream output, Object object)
  {
    Any any = output.orb().create_any();
    if (object == null)
      {
        GeneralTypeCode t = new GeneralTypeCode(TCKind.tk_abstract_interface);
        t.setId("IDL:omg.org/CORBA/AbstractBase:1.0");
        t.setName("");
        any.type(t);
        output.write_any(any);
        return;
      }
    else if (object instanceof org.omg.CORBA.Object
      && !(object instanceof Remote))
      {
        // Write as value type.
        boolean inserted = ObjectCreator.insertWithHelper(any, object);
        if (inserted)
          {
            output.write_any(any);
            return;
          }
      }

    if (object instanceof org.omg.CORBA.Object)
      writeAnyAsRemote(output, object);
    else if (object instanceof Serializable)
      {
        any.insert_Value((Serializable) object);
        output.write_any(any);
      }
    else
      {
        MARSHAL m = new MARSHAL(object.getClass().getName()
          + " must be CORBA Object, Remote or Serializable");
        m.minor = Minor.NonSerializable;
        throw m;
      }
  }

  /**
   * Write Any as for remote object.
   */
  void writeAnyAsRemote(OutputStream output, Object object)
  {
    GeneralTypeCode t = new GeneralTypeCode(TCKind.tk_objref);
    t.setId(m_ValueHandler.getRMIRepositoryID(object.getClass()));
    t.setName(object.getClass().getName());

    // Writing Any (typecode, followed by value).
    output.write_TypeCode(t);
    writeRemoteObject(output, object);
  }

  /**
   * Get the class name excluding the package name.
   */
  String getName(String n)
  {
    int p = n.lastIndexOf('.');
    if (p < 0)
      return n;
    else
      return n.substring(p + 1);
  }

  /**
   * Read Any from the input stream.
   */
  public Object readAny(InputStream input)
  {
    return input.read_any();
  }

  /**
   * Write the passed parameter to the output stream as CORBA object. If the
   * parameter is an instance of Remote and not an instance of Stub, the method
   * instantiates a suitable Tie, connects the parameter to this Tie and then
   * connects that Tie to the ORB that is requested from the output stream. Then
   * the object reference is written to the stream, making remote invocations
   * possible. This method is used in write_value(..) method group in
   * {@link org.omg.CORBA_2_3.portable.OutputStream} and also may be called
   * directly from generated Stubs and Ties.
   * 
   * @param output a stream to write to, must be
   * org.omg.CORBA_2_3.portable.OutputStream
   * @param object an object to write.
   */
  public void writeRemoteObject(OutputStream an_output, Object object)
  {
    org.omg.CORBA_2_3.portable.OutputStream output = (org.omg.CORBA_2_3.portable.OutputStream) an_output;
    if (object == null)
      an_output.write_Object(null);
    else if (isTieRequired(object))
      {
        // Find the interface that is implemented by the object and extends
        // Remote.
        Class fc = getExportedInterface(object);
        exportTie(output, object, fc);
      }
    else if (object instanceof org.omg.CORBA.Object)
      {
        ensureOrbRunning(output);
        an_output.write_Object((org.omg.CORBA.Object) object);
      }
    else if (object != null && object instanceof Serializable)
      writeFields(an_output, (Serializable) object);
  }

}