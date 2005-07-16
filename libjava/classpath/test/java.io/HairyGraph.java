
import java.io.*;

class GraphNode implements Serializable
{
  GraphNode( String s )
  {
    this.s = s;
  }

  public String toString()
  {
    return this.s;
  }
  
  String s;
  GraphNode a;
  GraphNode b;
  GraphNode c;
  GraphNode d;
}


public class HairyGraph implements Serializable
{
  GraphNode A;
  GraphNode B;
  GraphNode C;
  GraphNode D;
  
  HairyGraph()
  {
    A = new GraphNode( "A" );
    B = new GraphNode( "B" );
    C = new GraphNode( "C" );
    D = new GraphNode( "D" );

    A.a = B;
    A.b = C;
    A.c = D;
    A.d = A;
    
    B.a = C;
    B.b = D;
    B.c = A;
    B.d = B;

    C.a = D;
    C.b = A;
    C.c = B;
    C.d = C;
    
    D.a = A;
    D.b = B;
    D.c = C;
    D.d = D;
  }

  public boolean equals( Object o )
  {
    HairyGraph hg = (HairyGraph)o;

    return (A.a == B.d) && (A.a == C.c) && (A.a == D.b)
      && (A.b == B.a) && (A.b == C.d) && (A.b == D.c)
      && (A.c == B.b) && (A.c == C.a) && (A.c == D.d)
      && (A.d == B.c) && (A.d == C.b) && (A.d == D.a);
  }

  void printOneLevel( GraphNode gn )
  {
    System.out.println( "GraphNode< " + gn + ": " + gn.a + ", " + gn.b
			+ ", " + gn.c + ", " + gn.d + " >" );
  }
  
}
