/* X509Extension.java --- X.509 Extension
   Copyright (C) 1999 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.security.cert;
import java.util.Set;

/**
	Public interface for the X.509 Extension.

	This is used for X.509 v3 Certificates and CRL v2 (Certificate
	Revocation Lists) for managing attributes assoicated with
	Certificates, for managing the hierarchy of certificates,
	and for managing the distribution of CRL. This extension
	format is used to define private extensions.

	Each extensions for a certificate or CRL must be marked
	either critical or non-critical. If the certificate/CRL 
	system encounters a critical extension not recognized then 
	it must reject the certificate. A non-critical extension
	may be just ignored if not recognized.


	The ASN.1 definition for this class is: 

	 Extensions  ::=  SEQUENCE SIZE (1..MAX) OF Extension

	 Extension  ::=  SEQUENCE  {
	     extnId        OBJECT IDENTIFIER,
	     critical      BOOLEAN DEFAULT FALSE,
	     extnValue     OCTET STRING
	                   -- contains a DER encoding of a value
	                   -- of the type registered for use with
	                   -- the extnId object identifier value
	 }
 	
	@author Mark Benvenuto

	@since JDK 1.2
*/
public interface X509Extension
{

  /**
     Returns true if the certificate contains a critical extension
     that is not supported.

     @return true if has unsupported extension, false otherwise	
  */
  public boolean hasUnsupportedCriticalExtension();

  /**
     Returns a set of the CRITICAL extension OIDs from the 
     certificate/CRL that the object implementing this interface
     manages.

     @return A Set containing the OIDs. If there are no CRITICAL
     extensions or extensions at all this returns null.
  */
  public Set getCriticalExtensionOIDs();

  /**
     Returns a set of the NON-CRITICAL extension OIDs from the 
     certificate/CRL that the object implementing this interface
     manages.

     @return A Set containing the OIDs. If there are no NON-CRITICAL
     extensions or extensions at all this returns null.
  */
  public Set getNonCriticalExtensionOIDs();

  /**
     Returns the DER encoded OCTET string for the specified
     extension value identified by a OID. The OID is a string
     of number separated by periods. Ex: 12.23.45.67
  */
  public byte[] getExtensionValue(String oid);

}
