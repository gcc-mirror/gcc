// Primes.java

/** Copyright 1998
 * Roedy Green
 * Canadian Mind Products
 * 5317 Barker Avenue
 * Burnaby, BC Canada V5H 2N6
 * tel: (604) 435-3016
 * mailto:roedy@mindprod.com
 * http://mindprod.com
 */
// May be freely distributed for any purpose but military

import java.util.BitSet;

/**
  * @author Roedy Green
  * @version 1.10 1998 November 10
  * Calculate primes using Eratostheses Sieve.
  * Tell if a given number is prime.
  * Find a prime just below a given number.
  * Find a prime just above a given number.
  */
  
/* 
 * version 1.1 1998 November 10 - new address and phone.  
 */
class Primes
    {

    /**
      * constructors
      */
    Primes()
        {
        ensureCapacity(1000);
        }

    /**
      * @param capacity - largest number you will be asking if prime.
      * If give too small a number, it will automatically grow by
      * recomputing the sieve array.
      */
    Primes (int capacity)
        {
        ensureCapacity(capacity);
        }

    /**
      * @param candidate - is this a prime?
      */
    public boolean isPrime(int candidate)
        {
        ensureCapacity(candidate);
        if (candidate < 3) return candidate != 0;
        if (candidate % 2 == 0 ) return false;
        return !b.get(candidate/2);
        }

    /**
      * @return first prime higher than candidate
      */
    public int above(int candidate)
    {
        do
            {
            // see what we can find in the existing sieve
            for (int i=candidate+1; i<= sieveCapacity; i++)
                {
                if (isPrime(i)) return i;
                }
            // Keep building ever bigger sieves till we succeed.
            // The next prime P' is between P+2 and P^2 - 2.
            // However that is a rather pessimistic upper bound.
            // Ideally some theorem would tell us how big we need to build
            // to find one.
            ensureCapacity(Math.max(candidate*2, sieveCapacity*2));
            } // end do
        while (true);
        } // end above

    /**
      * @param return first prime less than candidate
      */
    public int below (int candidate)
    {
        for (candidate--; candidate > 0; candidate--)
            {
            if (isPrime(candidate)) return candidate;
            }
        // candidate was 1 or 0 or -ve
        return 0;
        }

    /**
      * calc all primes in the range 1..n,
      * not the first n primes.
      * @param n, highest candidate, not necessarily prime.
      * @return list of primes 1..n in an array
      */
    public final int[] getPrimes(int n)
        {
        // calculate the primes
        ensureCapacity(n);

        // pass 1: count primes
        int countPrimes = 0;
        for (int i = 0; i <= n; i++)
            {
            if (isPrime(i)) countPrimes++;
            }

        // pass 2: construct array of primes
        int [] primes = new int[countPrimes];
        countPrimes = 0;
        for (int i = 0; i <= n; i++)
            {
            if (isPrime(i)) primes[countPrimes++] = i;
            }
        return primes;
        } // end getPrimes

    /**
      * calculate the sieve, bit map of all primes 0..n
      * @param n highest number evalutated by the sieve, not necessarily prime.
      */
    private final void sieve ( int n )
        {
        // Presume BitSet b set is big enough for our purposes.
        // Presume all even numbers are already marked composite, effectively.
        // Presume all odd numbers are already marked prime (0 in bit map).
        int last = (int)(Math.sqrt(n))+1;
        for (int candidate = 3; candidate <= last; candidate += 2)
            {
            // only look at odd numbers
            if (!b.get(candidate/2) /* if candidate is prime */)
                {
                // Our candidate is prime.
                // Only bother to mark multiples of primes. Others already done.
                // no need to mark even multiples, already done
                int incr = candidate*2;
                for ( int multiple = candidate + incr; multiple < n; multiple += incr)
                    {
                    b.set(multiple/2); // mark multiple as composite
                    } // end for multiple
                } // end if
            } // end for candidate
        // at this point our sieve b is correct, except for 0..2
        } // end sieve

    /**
      * Ensure have a sieve to tackle primes as big as n.
      * If we don't allocate a sieve big enough and calculate it.
      * @param n - ensure sieve big enough to evaluate n for primality.
      */
    private void ensureCapacity (int n)
        {
        if ( n > sieveCapacity )
            {
            b = new BitSet((n+1)/2);
            // starts out all 0, presume all numbers prime
            sieveCapacity = n;
            sieve(n);
            }
        // otherwise existing sieve is fine
        } // end ensureCapacity

    private int sieveCapacity;
    // biggest number we have computed in our sieve.
    // our BitSet array is indexed 0..N (odd only)

    private BitSet b; /* true for each odd number if is composite */

    /**
      * Demonstrate and test the methods
      */
    public static void main (String[] args)
        {
        // print primes 1..101
        Primes calc = new Primes(106);
        int[] primes = calc.getPrimes(101);
        for (int i=0; i<primes.length; i++)
            {
            System.out.println(primes[i]);
            }

        // demonstrate isPrime, above, below
        System.out.println(calc.isPrime(149));
        System.out.println(calc.below(149));
        System.out.println(calc.above(149));

        // print all the primes just greater than powers of 2
        calc = new Primes(10000000);
        for (int pow=8; pow < 10000000; pow*=2)
            System.out.println(calc.above(pow));

        // Validate that isPrime works by comparing it with brute force
        for (int i=3; i<=151; i++)
            {
            boolean prime = true;
            for (int j=2; j<i; j++)
                {
                if (i % j == 0 )
                    {
                    prime = false;
                    break;
                    }
                } // end for j
            if ( calc.isPrime(i) != prime ) System.out.println(i + " oops");
            } // end for i

        } // end main
} // end Primes
