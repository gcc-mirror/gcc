/* DSAParameters.java --- DSA Parameters Implementation
   Copyright (C) 1999,2003 Free Software Foundation, Inc.

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


package gnu.java.security.provider;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import java.math.BigInteger;

import java.security.AlgorithmParametersSpi;
import java.security.InvalidAlgorithmParameterException;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;
import java.security.spec.InvalidParameterSpecException;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import gnu.java.io.ASN1ParsingException;
import gnu.java.security.der.DER;
import gnu.java.security.der.DEREncodingException;
import gnu.java.security.der.DERReader;
import gnu.java.security.der.DERValue;
import gnu.java.security.der.DERWriter;

import gnu.java.security.util.Prime;

/*
	ASN.1 Encoding for DSA from rfc2459 

        id-dsa ID ::= { iso(1) member-body(2) us(840) x9-57(10040)
                  x9cm(4) 1 }

        Dss-Parms  ::=  SEQUENCE  {
            p             INTEGER,
            q             INTEGER,
            g             INTEGER  }

*/
public class DSAParameters extends AlgorithmParametersSpi
{
private BigInteger q; // the small prime
private BigInteger p; // the big prime
private BigInteger g;


public void engineInit(AlgorithmParameterSpec paramSpec)
                            throws InvalidParameterSpecException
{
	if( paramSpec instanceof DSAParameterSpec ) {
		DSAParameterSpec dsaParamSpec = (DSAParameterSpec)paramSpec;
		p = dsaParamSpec.getP();
		q = dsaParamSpec.getQ();
		g = dsaParamSpec.getG();
	}
	else
		throw new InvalidParameterSpecException("Only accepts DSAParameterSpec");
}

public void engineInit(byte[] params)
                            throws IOException
{
	DERReader in = new DERReader(params);
	DERValue val = in.read();
	if (val.getValue() != DER.CONSTRUCTED_VALUE)
		throw new ASN1ParsingException("badly formed parameters");
	try
		{
			p = (BigInteger) in.read().getValue();
			q = (BigInteger) in.read().getValue();
			g = (BigInteger) in.read().getValue();
		}
	catch (Exception x)
		{
			throw new ASN1ParsingException("badly formed parameters");
		}
}

public void engineInit(byte[] params, String format)
                            throws IOException
{
	if( !format.equals("ASN.1") )
		throw new IOException("Invalid Format: Only accepts ASN.1");
	engineInit( params );	
}

public AlgorithmParameterSpec engineGetParameterSpec(Class paramSpec)
                          throws InvalidParameterSpecException
{
	if( paramSpec.isAssignableFrom(DSAParameterSpec.class) )
		return new DSAParameterSpec(p, q, g);
	else
		throw new InvalidParameterSpecException("Only accepts DSAParameterSpec");
}

public byte[] engineGetEncoded()
                                    throws IOException
{
	ByteArrayOutputStream bout = new ByteArrayOutputStream();
	ArrayList seq = new ArrayList(3);
	seq.add(new DERValue(DER.INTEGER, p));
	seq.add(new DERValue(DER.INTEGER, q));
	seq.add(new DERValue(DER.INTEGER, g));
	DERWriter.write(bout, new DERValue(DER.CONSTRUCTED | DER.SEQUENCE, seq));
	return bout.toByteArray();
}


public byte[] engineGetEncoded(String format)
                                    throws IOException
{
	if( !format.equals("ASN.1") )
		throw new IOException("Invalid Format: Only accepts ASN.1");
	return engineGetEncoded();
}

public String engineToString()
{
	String lineSeparator = System.getProperty("line.seperator");
	return ("q: " + q + " p: " + p + " g: " + g);
}

}
