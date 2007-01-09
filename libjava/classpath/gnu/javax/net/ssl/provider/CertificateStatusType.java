package gnu.javax.net.ssl.provider;

public enum CertificateStatusType
{
  OCSP (1);
  
  public final int value;
  
  private CertificateStatusType (final int value)
  {
    this.value = value;
  }
}
