package #package;

#imports
import java.lang.reflect.Method;
import java.rmi.server.RemoteRef;
import java.rmi.server.RemoteStub;
import java.rmi.UnexpectedException;

/**
 * This class delegates its method calls to the remote RMI object, referenced
 * by {@link RemoteRef}. 
 *
 * It is normally generated with rmic.
 */
public final class #name_Stub 
    extends RemoteStub
    implements #interfaces
{
    /**
     * Use serialVersionUID for interoperability 
     */
    private static final long serialVersionUID = 2;
    
    /**
     * The explaining message for {@ling UnexpectedException}.
     */
    private static final String exception_message = 
      "undeclared checked exception";

     /* All remote methods, invoked by this stub: */
#stub_method_declarations
    #zeroSizeObjecArray
    static
      {
        #zeroSizeClassArray      
        try 
          {
#stub_method_initializations
          }
        catch (NoSuchMethodException nex)
          {
             NoSuchMethodError err = new NoSuchMethodError(
               "#name_Stub class initialization failed");
             err.initCause(nex);
             throw err;
          }  
      }
    
    /**
     * Create the instance for _#name_Stub that forwards method calls to the
     * remote object.
     *
     * @para the reference to the remote object.
     */
    public #name_Stub(RemoteRef reference) 
    {
       super(reference);
    }    
    
    /* Methods */    
#stub_methods    
}
