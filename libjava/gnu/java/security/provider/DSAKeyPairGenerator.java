/* GnuDSAKeyPairGenerator.java --- Gnu DSA Key Pair Generator
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

import java.math.BigInteger;
import java.security.AlgorithmParameterGenerator;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidParameterException;
import java.security.KeyPair;
import java.security.KeyPairGeneratorSpi;
import java.security.SecureRandom;
import java.security.interfaces.DSAParams;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;
import java.util.Random;

public class DSAKeyPairGenerator extends KeyPairGeneratorSpi 
	implements java.security.interfaces.DSAKeyPairGenerator
{
int keysize;
SecureRandom random;
private BigInteger q = null; // the small prime
private BigInteger p = null; // the big prime
private BigInteger g = null;

DSAKeyPairGenerator()
{
	keysize = 1024;
}

public void initialize(int keysize, SecureRandom random)
{
	//if( ((keysize % 64) != 0) || (keysize < 512) || (keysize > 1024) )
        //        throw new InvalidAlgorithmParameterException("Invalid key size");

	this.keysize = keysize;
	this.random = random;
}

public void initialize(AlgorithmParameterSpec params,
                       SecureRandom random)
                throws InvalidAlgorithmParameterException
{
	if( !( params instanceof DSAParameterSpec ) )
		throw new InvalidAlgorithmParameterException("Must be DSAParameterSpec");

	DSAParameterSpec dsaparameterspec = (DSAParameterSpec)params;
	p = dsaparameterspec.getP();
	q = dsaparameterspec.getQ();
	g = dsaparameterspec.getG();
	this.random = random;
}

public void initialize(DSAParams params, SecureRandom random) 
	throws InvalidParameterException
{
	if(params.getP() != null) 
		p = params.getP();
	else
		throw new InvalidParameterException();

	if(params.getQ() != null) 
		q = params.getQ();
	else
		throw new InvalidParameterException();
	
	if(params.getG() != null) 
		g = params.getG();
	else
		throw new InvalidParameterException();
	
	this.random = random;
}

public void initialize(int modlen, boolean genParams, SecureRandom random) 
	throws InvalidParameterException
{
	if( ((modlen % 64) != 0) || (modlen < 512) || (modlen > 1024) )
                throw new InvalidParameterException();

	if( (genParams == false) && (modlen != 512) && (modlen != 768) && (modlen != 1024)  )
		throw new InvalidParameterException();
	this.keysize = modlen;
	this.random = random;
	p = null;
	q = null;
	g = null;
}

public KeyPair generateKeyPair()
{
	if( getDefaults() == false) {
		try {
			AlgorithmParameterGenerator apgDSA = AlgorithmParameterGenerator.getInstance("DSA");
			AlgorithmParameters apDSA = apgDSA.generateParameters();
			DSAParameterSpec dsaparameterspec = (DSAParameterSpec)apDSA.getParameterSpec( DSAParameterSpec.class );
			p = dsaparameterspec.getP();
			q = dsaparameterspec.getQ();
			g = dsaparameterspec.getG();
		} catch ( Exception e ) {
			return null;
		}
	}

	BigInteger x = new BigInteger( 159, new Random() );

	BigInteger y = g.modPow( x, p );

	return new KeyPair( new GnuDSAPublicKey(y,p,q,g), new GnuDSAPrivateKey(x,p,q,g));
	//return new KeyPair( public, private );
}

//These constants are Sun's Constants copied from the 
//Cryptography Specification
private boolean getDefaults()
{
	if( keysize == 512) {
		p = new BigInteger("fca682ce8e12caba26efccf7110e526db078b05edecbcd1eb4a208f3ae1617ae01f35b91a47e6df63413c5e12ed0899bcd132acd50d99151bdc43ee737592e17", 16);
		q = new BigInteger("962eddcc369cba8ebb260ee6b6a126d9346e38c5", 16);
		g = new BigInteger("678471b27a9cf44ee91a49c5147db1a9aaf244f05a434d6486931d2d14271b9e35030b71fd73da179069b32e2935630e1c2062354d0da20a6c416e50be794ca4", 16);
		return true;
	} else if( keysize == 768) {
		p = new BigInteger("e9e642599d355f37c97ffd3567120b8e25c9cd43e927b3a9670fbec5d890141922d2c3b3ad2480093799869d1e846aab49fab0ad26d2ce6a22219d470bce7d777d4a21fbe9c270b57f607002f3cef8393694cf45ee3688c11a8c56ab127a3daf", 16);
		q = new BigInteger("9cdbd84c9f1ac2f38d0f80f42ab952e7338bf511", 16);
		g = new BigInteger("30470ad5a005fb14ce2d9dcd87e38bc7d1b1c5facbaecbe95f190aa7a31d23c4dbbcbe06174544401a5b2c020965d8c2bd2171d3668445771f74ba084d2029d83c1c158547f3a9f1a2715be23d51ae4d3e5a1f6a7064f316933a346d3f529252", 16);
	} else if( keysize == 512) {
		p = new BigInteger("fd7f53811d75122952df4a9c2eece4e7f611b7523cef4400c31e3f80b6512669455d402251fb593d8d58fabfc5f5ba30f6cb9b556cd7813b801d346ff26660b76b9950a5a49f9fe8047b1022c24fbba9d7feb7c61bf83b57e7c6a8a6150f04fb83f6d3c51ec3023554135a169132f675f3ae2b61d72aeff22203199dd14801c7", 16);
		q = new BigInteger("9760508f15230bccb292b982a2eb840bf0581cf5", 16);
		g = new BigInteger("f7e1a085d69b3ddecbbcab5c36b857b97994afbbfa3aea82f9574c0b3d0782675159578ebad4594fe67107108180b449167123e84c281613b7cf09328cc8a6e13c167a8b547c8d28e0a3ae1e2bb3a675916ea37f0bfa213562f1fb627a01243bcca4f1bea8519089a883dfe15ae59f06928b665e807b552564014c3bfecf492a", 16);
	}
	return false;
}

}
