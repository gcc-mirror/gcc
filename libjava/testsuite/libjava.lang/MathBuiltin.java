class MathBuiltin
{
  static double abs(double x)
  {
    return Math.abs(x);
  }

  static double acos(double x)
  {
    return Math.acos(x);
  }

  static double asin(double x)
  {
    return Math.asin(x);
  }

  static double atan(double x)
  {
    return Math.atan(x);
  }

  static double atan2(double x, double y)
  {
    return Math.atan2(x,y);
  }

  static double ceil(double x)
  {
    return Math.ceil(x);
  }

  static double cos(double x)
  {
    return Math.cos(x);
  }

  static double exp(double x)
  {
    return Math.exp(x);
  }

  static double floor(double x)
  {
    return Math.floor(x);
  }

  static double log(double x)
  {
    return Math.log(x);
  }

  static double max(double x, double y)
  {
    return Math.max(x,y);
  }

  static double min(double x, double y)
  {
    return Math.min(x,y);
  }

  static double pow(double x, double y)
  {
    return Math.pow(x,y);
  }

  static double sin(double x)
  {
    return Math.sin(x);
  }

  static double sqrt(double x)
  {
    return Math.sqrt(x);
  }

  static double tan(double x)
  {
    return Math.tan(x);
  }

  public static void main(String argv[])
  {
    double sum = abs (1.0) + acos (1.0) + asin (1.0) + atan (1.0)
		 + atan2 (1.0, 1.0) + ceil (1.0) + cos (1.0) + exp (1.0)
		 + floor (1.0) + log(1.0) + max(1.0, 1.0) + min (1.0, 1.0)
		 + pow (1.0, 1.0) + sin (1.0) + sqrt(1.0) + tan(1.0);
  }
}

