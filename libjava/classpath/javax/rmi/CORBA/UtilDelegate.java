/* UtilDelegate.java --
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


package javax.rmi.CORBA;

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
import org.omg.CORBA.TRANSACTION_REQUIRED;
import org.omg.CORBA.TRANSACTION_ROLLEDBACK;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.rmi.AccessException;
import java.rmi.MarshalException;
import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.ServerError;
import java.rmi.ServerException;
import java.rmi.UnexpectedException;

import javax.transaction.InvalidTransactionException;
import javax.transaction.TransactionRequiredException;
import javax.transaction.TransactionRolledbackException;

/**
 * A delegate, implementing the functionality, provided by the {@link Util}.
 * 
 * The default delegate can be altered by setting the system property
 * "javax.rmi.CORBA.UtilClass" to the name of the alternative class that must
 * implement this interface.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public interface UtilDelegate
{
  /**
   * Used by local stubs to create a copy of the object.
   */
  Object copyObject(Object obj, ORB orb)
    throws RemoteException;

  /**
   * Used by local stubs to create a multiple copies of the object, preserving
   * sharing accross the parameters if necessary.
   */
  Object[] copyObjects(Object[] obj, ORB orb)
    throws RemoteException;

  /**
   * Get the value handler that Serializes Java objects to and from CDR (GIOP)
   * streams.
   */
  ValueHandler createValueHandler();

  String getCodebase(Class clz);

  /**
   * Checks if the given stub is local.
   */
  boolean isLocal(Stub stub)
    throws RemoteException;

  Class loadClass(String className, String remoteCodebase, ClassLoader loader)
    throws ClassNotFoundException;

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
   * The subsequent content is not part of the official RMI-IIOP standart and is
   * added for compatibility with Sun's implementation:
   * <ol>
   * <li>the phrase "<code>; nested exception is: <i>(line feed)(tab)</i></code>"</li>
   * <li>the full name of the mapped SystemException, as returned by
   * Class.getName().</li>
   * <li>the ": ".
   * <li>the value, returned by .getMessage() of the passed parameter.</li>
   * </ol>
   * <p>
   * For instance, if the Internet connection was refused:
   * </p><p>
   * <code>CORBA COMM_FAILURE 0x535500C9 No</code>
   * </p><p>
   * The original CORBA exception is set as the cause of the RemoteException
   * being created.
   * </p>
   */
  RemoteException mapSystemException(SystemException ex);

  /**
   * Get the Tie that handles invocations on the given target. The target/Tie
   * pair must be previously registered using {@link #registerTarget}.
   * 
   * @return the Tie, or null if no such is known.
   */
  Tie getTie(Remote target);

  /**
   * Register the Tie-target pair.
   */
  void registerTarget(Tie tie, Remote target);

  /**
   * Deactivate the associated Tie, if it is found and is not connected to other
   * registered targets.
   */
  void unexportObject(Remote target)
    throws NoSuchObjectException;

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
  RemoteException wrapException(Throwable orig);

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
  void writeRemoteObject(OutputStream output, Object obj);

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
  void writeAbstractObject(OutputStream output, Object object);

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
  void writeAny(OutputStream output, Object object);

  /**
   * Read Any from the input stream. 
   */
  Object readAny(InputStream input);

}