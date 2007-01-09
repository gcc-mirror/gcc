package #package;

#imports
import java.rmi.UnexpectedException;

import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.Util;

import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.RemarshalException;

import org.omg.CORBA_2_3.portable.OutputStream;

/**
 * This class delegates its method calls to the remote GIOP object.
 * It is normally generated with grmic.
 */
public class _#name_Stub extends Stub 
  implements #interfaces
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;
  
  /**
   * The array of repository ids, supported by this GIOP Object
   */ 
  private static final String[] type_ids =
    { 
#idList
    };

  /**
   * Return the array of repository ids, supported by this GIOP Object.   
   *
   * @return the array of Ids.
   */ 
  public String[] _ids()
  {
    return type_ids;
  }
  
#stub_methods    
}