/* A class to test my java.net.InetAddress implementation */

import java.net.*;

public class TestNameLookups extends Object
{
public static void
main(String[] argv) throws UnknownHostException
{
  InetAddress addr;

  System.out.println("Started address lookup test");

  /* Test local host */
  try
    {
      addr = InetAddress.getLocalHost();
 
      System.out.println("The local hostname is " + addr.getHostName() +
                     " with an IP address of " + addr.getHostAddress());
    }
  catch(UnknownHostException e)
    {
      System.out.println("WARNING: Can't resolve local hostname");
    }

  /* Test simple lookup by IP */
  addr = InetAddress.getByName("18.159.0.42");
 
  System.out.println("Looked up IP addres 18.159.0.42 and got back a " +
                     "hostname of " + addr.getHostName());

  /* Test failed reverse lookup of IP */
  addr = InetAddress.getByName("194.72.246.154");

  System.out.println("Looked up IP addres 194.72.246.154 and got back a " +
                     "hostname of " + addr.getHostName());

  /* Test mangled/invalid IP's */
  try { addr = InetAddress.getByName("122.24.1."); }
  catch (UnknownHostException e) { 
    System.out.println("Passed bad IP test 1");
  }

  try { addr = InetAddress.getByName("122.24.52"); }
  catch (UnknownHostException e) { 
    System.out.println("Passed bad IP test 2");
  }

  try { addr = InetAddress.getByName("122.256.52.1"); }
  catch (UnknownHostException e) { 
    System.out.println("Passed bad IP test 3");
  }

  /* Test simple lookup by name with external info */
  addr = InetAddress.getByName("www.starnews.com"); 
  System.out.println("Looked up host www.starnews.com and got back an " +
                     "IP address of " + addr.getHostAddress());
  byte[] octets = addr.getAddress();
  System.out.println("Raw Address Bytes: octet1=" + (int)octets[0] +
    " octets2=" + (int)octets[1] + " octet3=" + (int)octets[2] +
    " octets4=" + (int)octets[3]);
  System.out.println("toString() returned: " + addr.toString());
  System.out.println("isMulticastAddress returned: " 
                     + addr.isMulticastAddress());

  /* Test complex lookup */
  System.out.println("Looking up all addresses for indiana.edu ...");
  InetAddress[] list = InetAddress.getAllByName("indiana.edu");
  for (int i = 0; i < list.length; i++)
    {
      addr = list[i];

      System.out.println("   Hostname: " + addr.getHostName() + 
                         " IP Address: " + addr.getHostAddress());
    }

  /* Test equality */
  InetAddress addr1 = InetAddress.getByName("www.urbanophile.com");    
  InetAddress addr2 = InetAddress.getByName("www.urbanophile.com");    

  if (addr1.equals(addr2) && addr2.equals(addr1))
    System.out.println("Passed equality test #1");
  else
    System.out.println("Failed equality test #1");

  addr1 = InetAddress.getByName("www.ac.com");
  addr2 = InetAddress.getByName("www.hungry.com");

  if (!addr1.equals(addr2) && !addr2.equals(addr1))
    System.out.println("Passed equality test #2");
  else
    System.out.println("Failed equality test #2");

  /* Quick test to see if it looks like we're caching things */
  addr1 = InetAddress.getByName("www.urbanophile.com");    
  System.out.println("Got " + addr1.getHostName() + " " + addr1.getHostAddress());
  addr2 = InetAddress.getByName("www.hungry.com");
  System.out.println("Got " + addr2.getHostName() + " " + addr2.getHostAddress());
}

}

