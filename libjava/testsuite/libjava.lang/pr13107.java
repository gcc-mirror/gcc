class pr13107
{
  public static void main(String[] args)
  {
    for (int i = 0; i < 1; i++) {
      String s = "A";

      if (s == "A")
        continue;
      
      try{
	try{
          System.out.println(s);
	}
	finally{
	  if (s != "A")
	    throw new Error();
	}
      }
      catch(Exception e){
	s = "B";
      }
    }
  }
}
