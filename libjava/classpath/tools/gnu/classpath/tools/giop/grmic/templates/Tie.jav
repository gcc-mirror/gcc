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
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAPackage.WrongPolicy;
import org.omg.PortableServer.POAPackage.ObjectNotActive;
import org.omg.PortableServer.POAPackage.ServantNotActive;

import org.omg.CORBA_2_3.portable.InputStream;

/**
 * This class accepts remote calls to the served GIOP object and delegates them
 * to the enclosed implementing class. Being servant, it must be connected to
 * the ORB Poa. 
 * It is normally generated with grmic -poa
 */
public class _#nameImpl_Tie extends Servant implements Tie
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
   * Get an array of all interfaces, supported by this 
   * {@link Servant}.
   * 
   * @param poa unused
   * @param objectId unused
   * 
   * @return the array of Ids.
   */
  public String[] _all_interfaces(POA poa,
    byte[] objectId
  )
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
   * @return the GIOP object.
   */
  public org.omg.CORBA.Object thisObject()
  {
    return _this_object();
  }
  
  /**
   * Deactivate this {@link Servant}. The WrongPolicy, ObjectNotActive
   * and ServantNotActive exceptions, if thrown during deactivation, are
   * catched and silently ignored.
   */
  public void deactivate()
  {
    try
      {
        _poa().deactivate_object(_poa().servant_to_id(this));
      }
    catch (WrongPolicy exception)
      {
      }
    catch (ObjectNotActive exception)
      {
      }
    catch (ServantNotActive exception)
      {
      }
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
   * Connect this servant to the given ORB. It is recommended to connect
   * servant to the ORBs root or other POA rather than using this method.
   */
  public void orb(ORB orb)
  {
    try
      {
        ((org.omg.CORBA_2_3.ORB) orb).set_delegate(this);
      }
    catch (ClassCastException e)
      {
        throw new org.omg.CORBA.BAD_PARAM(
          "POA Servant requires an instance of org.omg.CORBA_2_3.ORB"
        );
      }
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
    ResponseHandler reply
  ) throws SystemException
  {
    try
      {
        InputStream in =(InputStream) parameter_stream;
        switch (method.charAt(#hashCharPos))
          {
#tie_methods
            default: break;
          }
          
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