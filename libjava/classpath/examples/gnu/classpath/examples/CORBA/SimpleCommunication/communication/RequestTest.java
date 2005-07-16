

package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ByteHolder;
import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NVList;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.ShortHolder;
import org.omg.CORBA.StringHolder;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.UnknownUserException;

/**
 * This code uses CORBA to call various methods of the remote object,
 * passing data structures in both directions. It finds the server by
 * reading the IOR.txt file that must be present in the folder,
 * where the program has been started.
 *
 * The IOR.txt file is written by the server
 * {@link gnu.classpath.examples.CORBA.SimpleCommunication.comServer}.
 * The server should be reachable over Internet, unless blocked by
 * security tools.
 *
 * This code is tested for interoperability with Sun Microsystems
 * java implementation 1.4.2 (08.b03). Server, client of both can
 * be started either on Sun's or on Classpath CORBA implementation,
 * in any combinations.
 *
 * BE SURE TO START THE SERVER BEFORE STARTING THE CLIENT.
 *
 * Test invocations using org.omg.CORBA.Request. The methods are
 * called by "name", like in java.lang.reflect.
 * No need to have the local pre-compiled stub classes.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class RequestTest
{
  /*
  * The IOR.txt file, used to find the server and the object on the server. is written when starting the accompanying
  */
  public static final String IOR_FILE = "IOR.txt";

  /**
   * The Object Request Brocker, used for various CORBA operations.
   */
  ORB orb;

  /**
   * Our remote object - the invocation target.
   */
  org.omg.CORBA.Object object;

  /**
   * Prepare for work. Read the file IOR.txt in the current folder
   * and find the server using its information.
   */
  public static void main(String[] args)
  {
    RequestTest we = new RequestTest();

    we.orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);

    char[] c = null;
    try
      {
        File f = new File(IOR_FILE);
        c = new char[ (int) f.length() ];

        FileReader fr = new FileReader(f);
        fr.read(c);
        fr.close();
      }
    catch (IOException ex)
      {
        System.out.println("Unable to write the IOR.txt into the current folder");
        ex.printStackTrace();
      }

    String ior = new String(c);

    we.object = we.orb.string_to_object(ior);
    we.Demo();
    we.orb.shutdown(false);
  }

  /** Run all demos. */
  public void Demo()
  {
    testHello();
    try
      {
        testParameters();
      }
    catch (Exception ex)
      {
        // Not expected.
        throw new InternalError();
      }
    testSystemException();
    testUserException();
    testWideNarrowStrings();
  }

  /**
   * Send the hello message, one way.
   */
  public void testHello()
  {
    System.out.println("***** Test 'HELLO WORLD' (see the server console).");

    Request hello =
      object._create_request(null, "sayHello", orb.create_list(0), null);

    // No response expected.
    hello.send_oneway();
  }

  /**
   * Test passing various parameters in both directions.
   */
  public void testParameters()
                      throws Exception
  {
    System.out.println("***** Test passing multiple parameters:");

    Request r =
      object._create_request(null, "passSimple", orb.create_list(0), null);

    ByteHolder a_byte = new ByteHolder((byte) 0);
    ShortHolder a_short = new ShortHolder((short) 3);
    StringHolder a_string = new StringHolder("[string 4]");

    // This is an 'out' parameter; the value must not be passed to servant.
    DoubleHolder a_double = new DoubleHolder(56.789);

    r.add_inout_arg().insert_octet((byte) 0);
    r.add_in_arg().insert_long(2);
    r.add_inout_arg().insert_short((short) 3);
    r.add_inout_arg().insert_string("[string 4]");
    r.add_out_arg().type(orb.get_primitive_tc(TCKind.tk_double));

    NVList para = r.arguments();

    System.out.println(" --- Parameters before invocation: ");

    System.out.println("  octet " + para.item(0).value().extract_octet());
    System.out.println("  long (in parameter) " +
                       para.item(1).value().extract_long()
                      );
    System.out.println("  short " + para.item(2).value().extract_short());
    System.out.println("  string " + para.item(3).value().extract_string());

    // For the last parameter, the value is not set.
    r.set_return_type(orb.get_primitive_tc(TCKind.tk_long));

    r.invoke();

    para = r.arguments();

    System.out.println(" --- Parameters after invocation:");

    System.out.println("  octet " + para.item(0).value().extract_octet());
    System.out.println("  long (in parameter, must not be changed) " +
                       para.item(1).value().extract_long()
                      );
    System.out.println("  short " + para.item(2).value().extract_short());
    System.out.println("  string " + para.item(3).value().extract_string());
    System.out.println("  double " + para.item(4).value().extract_double());

    System.out.println("  Returned value " + r.result().value().extract_long());
  }

  /**
   * Test catching the system exception, thrown on the remote side.
   */
  public void testSystemException()
  {
    System.out.println("**** Test system exception:");
    try
      {
        ExceptionList exList = orb.create_exception_list();
        exList.add(ourUserExceptionHelper.type());

        Request rq =
          object._create_request(null, "throwException", orb.create_list(1),
                                 null, exList, null
                                );

        rq.add_in_arg().insert_long(-55);

        rq.invoke();

        throw new InternalError();
      }
    catch (BAD_OPERATION ex)
      {
        System.out.println("  The expected BAD_OPERATION, minor code " +
                           ex.minor + ", has been thrown on remote side."
                          );
      }
  }

  /**
   * Test catching the user exception, thrown on the remote side.
   */
  public void testUserException()
  {
    System.out.println("**** Test user exception:");

    ExceptionList exList = orb.create_exception_list();
    exList.add(ourUserExceptionHelper.type());

    Request rq =
      object._create_request(null, "throwException", orb.create_list(1), null,
                             exList, null
                            );

    rq.add_in_arg().insert_long(123);
    rq.invoke();

    UnknownUserException uku = (UnknownUserException) rq.env().exception();
    ourUserException our_exception = ourUserExceptionHelper.extract(uku.except);

    System.out.println("  Our user exception, field " + our_exception.ourField +
                       ", has been thrown on remote side."
                      );
  }

  /**
   * Passes wide (UTF-16) string and narrow (ISO8859_1) string.
   * @see gnu.CORBA.GIOP.CharSets_OSF for supported and default
   * encodings.
   */
  public void testWideNarrowStrings()
                             throws BAD_OPERATION
  {
    System.out.println("**** Test 8 bit and 16 bit char strings");

    Request rq =
      object._create_request(null, "passCharacters", orb.create_list(0), null);

    rq.add_in_arg().insert_wstring("wide string");
    rq.add_in_arg().insert_string("narrow string");

    rq.set_return_type(orb.get_primitive_tc(TCKind.tk_wstring));

    rq.invoke();

    System.out.println("  Returned ' " + rq.result().value().extract_wstring());
  }
}
