public class PR8866
{
  public static void main (String args[])
    {
      String attTypeString = "";

      switch (args.length) {
      case 1:
          attTypeString = "string";
      case 4:
          attTypeString = "ID";;
      case 5:
          attTypeString = "IDREF";;
      case 6:
          attTypeString = "NMTOKEN";;
      case 7:
          attTypeString = "NOTATION";;
      default:
          ;
      }
    }
}
