/*************************************************************************
/* StreamTokenizerTest.java -- Test the StreamTokenizer class
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
/*************************************************************************/

import java.io.*;

public class StreamTokenizerTest
{

public static void
main(String[] argv)
{
  System.out.println("Started test of StreamTokenizer");
  
  try
    {
      System.out.println("Test 1: Basic Parsing Test");

      StreamTokenizer st = new StreamTokenizer(new 
                               FileInputStream("./stream-tokenizer.data"));

      System.out.println("No tokens read: " + st.toString());
      int j = 0;
      for (;;)
        {
          int ttype = st.nextToken();
          switch(ttype)
            {
              case StreamTokenizer.TT_NUMBER:
                System.out.println("Read a number: " + st.toString());
                break;

              case StreamTokenizer.TT_WORD:
                System.out.println("Read a word: " + st.toString());
                ++j;
                if (j == 2)
                  {
                    st.ordinaryChar('/');
                    st.eolIsSignificant(true);
                    st.lowerCaseMode(true);
                    st.slashStarComments(true);
                    st.slashSlashComments(true);
                  }
                break;

              case StreamTokenizer.TT_EOL:
                System.out.println("Read an EOL: " + st.toString());
                break;

              case StreamTokenizer.TT_EOF:
                System.out.println("Read an EOF: " + st.toString());

              case '\'':
              case '"':
                System.out.println("Got a quote:" + st.toString());
                break;

              default:
                System.out.println("Got an ordinary:" + st.toString());
                break; 
           }
         if (ttype == StreamTokenizer.TT_EOF)
           break;
        }

      System.out.println("PASSED: Basic Parsing Test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: Basic Parsing Test: " + e);
    } 

  System.out.println("Finished test of StreamTokenizer");
}

} // class StreamTokenizerTest

