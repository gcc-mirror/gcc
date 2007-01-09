package #package;

#imports
import java.rmi.Remote;
import javax.rmi.PortableRemoteObject;
import javax.rmi.CORBA.Tie;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.CORBA.portable.UnknownException;
import org.omg.PortableServer.Servant;

import org.omg.CORBA_2_3.portable.ObjectImpl;
import org.omg.CORBA_2_3.portable.InputStream;

// This Tie type is obsolete. Use the POA - based tie (key -poa).

/**
 * This class accepts remote calls to the served GIOP object and delegates them
 * to the enclosed implementing class. Being derived from the ObjectImpl,
 * it directly implements the GIOP Object.
 *
 * It is normally generated with grmic -impl
 */
public class _#nameImpl_Tie extends ObjectImpl implements Tie
{
  /**
   * All decoded remote calls are forwarded to this target.
   */
  #implName target;
  
  /**
   * The array of repository ids, supported by this GIOP Object
   */ 
  private static final String[] type_ids =
    { 
#idList
    };
      
  /**
   * Get an array of all interfaces (repository ids),
   * supported by this Object.
   *
   * @return the array of Ids.
   */
  public String[] _ids() 
  { 
    return type_ids;
  }  

  /**
   * Set the invocation target, where all received calls are finally
   * forwarded.
   *
   * @param a_target the forwarding target
   *
   * @throws ClassCastException if the target is not an instance of
   * #implName
   */ 
  public void setTarget(Remote a_target)
  {
    this.target = (#implName) a_target;
  }

  /**
   * Get the invocation target, where all received calls are finally
   * forwarded.
   *
   * @return the target, an instance of
   * #implName
   */ 
  public Remote getTarget()
  {
    return target;
  }
  
  /**
   * Return the actual GIOP object that would handle this request.
   * 
   * @return <code>this</code>, always.
   */
  public org.omg.CORBA.Object thisObject()
  {
    return this;
  }
  
  /**
   * Deactivates this object, disconnecting it from the orb.
   */
  public void deactivate()
  {
     _orb().disconnect(this);
     _set_delegate(null);
     target = null;
  }

  /**
   * Get the {@link ORB} where this {@link Servant} is connected.
   * 
   * @return the ORB
   */
  public ORB orb()
  {
    return _orb();
  }

  /**
   * Connect this servant to the given ORB. 
   */
  public void orb(ORB orb)
  {
    orb.connect(this);
  }

/**
 * This method is invoked by ORB in response to the remote call. It redirects
 * the call to one of the methods in the target.
 * 
 * @param method the name of the method to call.
 * @param parameter_stream the input stream, from where the parameters must be
 * read. 
 * @param reply the response hander, providing methods to return the result.
 * 
 * @return the output stream, created by the response handler
 * 
 * @throws SystemException if one occurs during method invocation.
 */  
  public OutputStream _invoke(String method, 
    org.omg.CORBA.portable.InputStream parameter_stream,
    ResponseHandler reply)
  {
    try
      {
        InputStream in =(InputStream) parameter_stream;
          
#tie_methods          
          
       throw new BAD_OPERATION("No such method: '"+method+"'");
      }
    catch (SystemException ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
        throw new UnknownException(ex);
      }
  }
}