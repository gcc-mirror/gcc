// MessageDigest.java

/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security;

// FIXME: This is just a stub for a proper implementation.
public abstract class MessageDigest
{
    private static final byte[] dummy = { 0 };

    public static MessageDigest getInstance(String algorithm)
	throws NoSuchAlgorithmException
    {
	Object	obj;

	try {
	    obj = Class.forName(algorithm).newInstance();
	} catch (Exception e) {
	    throw new NoSuchAlgorithmException("algorithm " 
					       + algorithm 
					       + " not available.");
	}
	
	return (MessageDigest) obj;
    }

    public void update(byte input)
    {
	// FIXME
    }

    public void update(byte[] input, int offset, int len)
    {
	// FIXME
    }

    public void update(byte[] input)
    {
	// FIXME
    }

    public byte[] digest()
    {
	return dummy;
    }

    public byte[] digest(byte[] input)
    {
	update(input);
	return digest();
    }
    
    public void reset()
    {
	// FIXME
    }
}
