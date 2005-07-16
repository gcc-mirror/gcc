/* Written by Artur Biesiadowski <abies@pg.gda.pl> */

/*
   This class test basic 4 conversion types and compares results to ready ones, done
   on sure VM (suns JDK). Conversions are
   (obj instanceof clazz)
   (clazz)obj
   clazz.isInstance(obj)
   clazz1.isAssignableFrom(clazz2);
   
   Hopefully all needed cases are covered. If you want to add object just put it
   into objs table. If you want to add class, you need to add it to both cls and to
   testCode method. Of course you need to regenerate results after that.
 */


/*
   You can copy/modify/use this file for any purposes, as long as you do not delete
   my name from top of that file. Of course you can add your own below that :)
 */


import java.io.*;

interface I1 {}
interface I2 {}
interface I3 extends I2{}
class A1 implements I1 {}
class AB12 extends A1 implements I2 {}
class ABC12 extends AB12 {}
class D3 implements I3 {}

public class TestCasts
{

   public Object objs[] = 
   {
      null,
      new Object(),
      new A1(),
      new AB12(),
      new ABC12(),
      new D3(),
      new A1[1],
      new AB12[1],
      new ABC12[1],
      new D3[1],
      new I1[1],
      new I2[1],
      new I3[1],
      new int[1],
      new A1[1][1],
      new AB12[1][1],
      new I1[1][1]      
   };
   
   public Class cls[] = 
   {
      Object.class,
      A1.class,
      AB12.class,
      ABC12.class,
      D3.class,
      I1.class,
      I2.class,
      I3.class,
      Cloneable.class,
      Serializable.class,
      A1[].class,
      AB12[].class,
      ABC12[].class,
      D3[].class,
      I1[].class,
      I2[].class,
      I3[].class,
      int[].class,
      A1[][].class,
      AB12[][].class,
      I1[][].class
   };
   
   java.util.Vector results = new java.util.Vector(1000);
   boolean verbose = false;
   boolean generate = false;
   String filename = "TestCasts-results.txt";

   public static void main(String argv[] )
   {   
      TestCasts tc = new TestCasts();
      if ( argv.length > 0 )
      {
         int i;
         for ( i =0; i < argv.length;i++ )
         {
            if ( argv[i].equals("-g") )
            {
               tc.generate = true;
            }  
            else if ( argv[i].equals("-v") )
            {
               tc.verbose = true;
            }
            else if ( argv[i].equals("-f") )
            {
               i++;
               if ( i > argv.length )
               {
                  System.out.println("You need to specify filename after -f");
                  System.exit(1);
               }
               tc.filename = argv[i];
            }
            else
            {
               System.out.println( "Options are: -v -g -f file");
               System.out.println( "[-v] verbose ");
               System.out.println( "[-g] generate result table");
               System.out.println( "[-f file] read/write tests from/to file (default "+tc.filename+")");
               System.exit(1);
            }  
         }
      }
      
      
      tc.test();
      //System.out.println(tc.results);
      System.out.println( "Performed " + tc.counter + " tests");
      if ( tc.generate )
         System.out.println( "True: " + tc.genTrue + "\tfalse: " + tc.genFalse);
      else
      {
         System.out.println( "Passed: " + tc.passed + "\tfailed: " + tc.failed); 
         if (tc.failed == 0 )
            System.out.println("PASSED: all cast tests");
      }  
   }
   
   
   public final void test()
   {
      if (!generate)
         readResultsFromFile();
         
      int i;
      int j;
      for ( i=0; i < objs.length; i++ )
      {
         for ( j=0; j < cls.length; j++ )
         {
            reportClIsInst(objs[i], cls[j], cls[j].isInstance(objs[i]) );
         }
      }  

      for (i=0; i < objs.length; i++ )
      {
         testCode(objs[i]);
      }
                  
      for ( i=0; i < cls.length; i++ )
      {
         for ( j=0; j < cls.length; j++ )
         {
            reportClIsAssign(cls[i], cls[j], cls[i].isAssignableFrom(cls[j]));
         }
      }

      if ( generate )
         writeResultsToFile();      
   }


   public final void testCode(Object o)
   {
   
      reportInstanceof(o, Object.class, (o instanceof Object) );
      try
      {
         Object r1 = (Object) o;
         reportCast(o, Object.class, true );
      } catch (ClassCastException e) {
            reportCast(o,Object.class, false );
         }
      
      reportInstanceof(o, A1.class, (o instanceof A1) );
      try
      {
         A1 r1 = (A1) o;
         reportCast(o, A1.class, true );
      } catch (ClassCastException e) {
            reportCast(o,A1.class, false );
         }
      reportInstanceof(o, AB12.class, (o instanceof AB12) );
      try
      {
         AB12 r1 = (AB12) o;
         reportCast(o, AB12.class, true );
      } catch (ClassCastException e) {
            reportCast(o,AB12.class, false );
         }
      reportInstanceof(o, ABC12.class, (o instanceof ABC12) );
      try
      {
         ABC12 r1 = (ABC12) o;
         reportCast(o, ABC12.class, true );
      } catch (ClassCastException e) {
            reportCast(o,ABC12.class, false );
         }  
      reportInstanceof(o, D3.class, (o instanceof D3) );
      try
      {
         D3 r1 = (D3) o;
         reportCast(o, D3.class, true );
      } catch (ClassCastException e) {
            reportCast(o,D3.class, false );
         }        
      reportInstanceof(o, I1.class, (o instanceof I1) );
      try
      {
         I1 r1 = (I1) o;
         reportCast(o, I1.class, true );
      } catch (ClassCastException e) {
            reportCast(o,I1.class, false );
         }
      reportInstanceof(o, I2.class, (o instanceof I2) );
      try
      {
         I2 r1 = (I2) o;
         reportCast(o, I2.class, true );
      } catch (ClassCastException e) {
            reportCast(o,I2.class, false );
         }                 
      reportInstanceof(o, I3.class, (o instanceof I3) );
      try
      {
         I3 r1 = (I3) o;
         reportCast(o, I3.class, true );
      } catch (ClassCastException e) {
            reportCast(o,I3.class, false );
         }        
      reportInstanceof(o, Cloneable.class, (o instanceof Cloneable) );
      try
      {
         Cloneable r1 = (Cloneable) o;
         reportCast(o, Cloneable.class, true );
      } catch (ClassCastException e) {
            reportCast(o,Cloneable.class, false );
         }  
      
      reportInstanceof(o, Serializable.class, (o instanceof Serializable) );
      try
      {
         Serializable r1 = (Serializable) o;
         reportCast(o, Serializable.class, true );
      } catch (ClassCastException e) {
            reportCast(o,Serializable.class, false );
         }        
      reportInstanceof(o, A1[].class, (o instanceof A1[]) );
      try
      {
         A1[] r1 = (A1[]) o;
         reportCast(o, A1[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,A1[].class, false );
         }           

      reportInstanceof(o, AB12[].class, (o instanceof AB12[]) );
      try
      {
         AB12[] r1 = (AB12[]) o;
         reportCast(o, AB12[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,AB12[].class, false );
         }
      reportInstanceof(o, ABC12[].class, (o instanceof ABC12[]) );
      try
      {
         ABC12[] r1 = (ABC12[]) o;
         reportCast(o, ABC12[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,ABC12[].class, false );
         }
      reportInstanceof(o, D3[].class, (o instanceof D3[]) );
      try
      {
         D3[] r1 = (D3[]) o;
         reportCast(o, D3[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,D3[].class, false );
         }
      reportInstanceof(o, I1[].class, (o instanceof I1[]) );
      try
      {
         I1[] r1 = (I1[]) o;
         reportCast(o, I1[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,I1[].class, false );
         }  
      reportInstanceof(o, I2[].class, (o instanceof I2[]) );
      try
      {
         I2[] r1 = (I2[]) o;
         reportCast(o, I2[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,I2[].class, false );
         }     

      reportInstanceof(o, I3[].class, (o instanceof I3[]) );
      try
      {
         I3[] r1 = (I3[]) o;
         reportCast(o, I3[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,I3[].class, false );
         }
            
      reportInstanceof(o, int[].class, (o instanceof int[]) );
      try
      {
         int[] r1 = (int[]) o;
         reportCast(o, int[].class, true );
      } catch (ClassCastException e) {
            reportCast(o,int[].class, false );
         }
         
      reportInstanceof(o, A1[][].class, (o instanceof A1[][]) );
      try
      {
         A1[][] r1 = (A1[][]) o;
         reportCast(o, A1[][].class, true );
      } catch (ClassCastException e) {
            reportCast(o,A1[][].class, false );
         }                                                                                
      reportInstanceof(o, AB12[][].class, (o instanceof AB12[][]) );
      try
      {
         AB12[][] r1 = (AB12[][]) o;
         reportCast(o, AB12[][].class, true );
      } catch (ClassCastException e) {
            reportCast(o,AB12[][].class, false );
         }
      reportInstanceof(o, I1[][].class, (o instanceof I1[][]) );
      try
      {
         I1[][] r1 = (I1[][]) o;
         reportCast(o, I1[][].class, true );
      } catch (ClassCastException e) {
            reportCast(o,I1[][].class, false );
         }

   }
   
   int counter = 0;
   int passed = 0;
   int failed = 0;
   int genTrue = 0;
   int genFalse =0;
   
   public final boolean result(boolean b )
   {
      counter++;
      if ( generate )
      {
         if (b )
         {
            genTrue++;
            results.addElement(Boolean.TRUE);
         }  
         else
         {
            genFalse++;
            results.addElement(Boolean.FALSE);
         }  
         return true;
      }
      else
      {
         if ( ((Boolean)results.elementAt(counter-1)).booleanValue() != b )
         {
            failed++;
            return false;
         }
         else
         {
            passed++;
            return true;
         }
      }
         
   }

   public final void reportClIsInst(Object obj, Class cl, boolean b )
   {
      if ( result(b) )
      {
         if ( verbose )
            System.out.println("PASSED: "+obj +"\tis\t"+ cl + "\t?" + b);
      }
      else
      {
         System.out.println("FAILED: " + cl + ".isInstance(" + obj + ") is\t" + b );
      }
   }
   
   public final void reportClIsAssign( Class c1, Class c2, boolean b )
   {
      if ( result(b) )
      {
         if (verbose)
            System.out.println("PASSED: "+c1 + "\tisAssignableFrom\t" + c2 + "\t?\t" + b);
      }
      else
      {
         System.out.println("FAILED: " + c1 + ".isAssigableFrom(" + c2 + ") is " + b); 
      }
   }
   
   public final void reportInstanceof( Object obj, Class cl, boolean b )
   {
      if ( result(b) )
      {
         if ( verbose )
            System.out.println("PASSED: "+obj +"\tinstanceof\t"+ cl + "\t?" + b);
      }
      else
      {
         System.out.println("FAILED: (" + obj + "instanceof\t" + cl  + ")\tis\t" + b );
      }
   }
   
   public final void reportCast( Object obj, Class cl, boolean b )
   {
      if ( result(b) )
      {
         if ( verbose )
            System.out.println("PASSED: "+obj +"\tcastto     \t"+ cl + "\t?" + b);
      }
      else
      {
         System.out.println("FAILED: " +  obj + "\tcastto    \t" + cl + "\tis\t" + b );
      }
   }

   public final void readResultsFromFile()
   {
      try{
         int i;
         FileInputStream fin = new FileInputStream(filename);
         while ( (i=fin.read()) != -1 )
         {
            results.addElement( i==1 ? Boolean.TRUE : Boolean.FALSE );
         }
      } catch (IOException e )
         {
            System.out.println("Cannot read from file " + filename);
            System.out.println(e);
            System.exit(1);
         }
   }

   public final void writeResultsToFile()
   {
      try{
         int i;
         FileOutputStream fos = new FileOutputStream(filename);
         for ( i=0; i < counter; i++ )
         {
            fos.write( ((Boolean)results.elementAt(i)).booleanValue() ? 1 : 0 );
         }
         fos.close();
      } catch (IOException e )
         {
            System.out.println("Cannot read from file " + filename);
            System.out.println(e);
            System.exit(1);
         }
   }                       
}
