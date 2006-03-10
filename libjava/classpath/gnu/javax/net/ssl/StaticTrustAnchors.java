/* StaticTrustAnchors.java -- static list of CA certificates.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import java.util.LinkedList;

import javax.net.ssl.ManagerFactoryParameters;

/**
 * This class implements a simple set of trust anchors suitable for
 * initializing a TrustManagerFactory for the "JessieX509" algorithm.
 *
 * <p>The important field of this class is the {@link #CA_CERTS}
 * constant, which contains an array of commonly accepted CA
 * certificates.
 */
public class StaticTrustAnchors implements ManagerFactoryParameters
{

  // Fields.
  // -------------------------------------------------------------------------

  private X509Certificate[] certs;

  // Constructor.
  // -------------------------------------------------------------------------

  public StaticTrustAnchors(X509Certificate[] certs)
  {
    this.certs = (X509Certificate[]) certs.clone();
  }

  // Class method.
  // -------------------------------------------------------------------------

  public static X509Certificate generate(CertificateFactory factory,
                                         String encoded)
  {
    try
      {
        ByteArrayInputStream in =
          new ByteArrayInputStream(encoded.getBytes("UTF-8"));
        return (X509Certificate) factory.generateCertificate(in);
      }
    catch (Exception x)
      {
        return null;
      }
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public X509Certificate[] getCertificates()
  {
    return (X509Certificate[]) certs.clone();
  }

  // Constant.
  // -------------------------------------------------------------------------

  /**
   * A list of known certificate authority certificates. This set of
   * certificates is the same as the default CA certificates used by
   * Mozilla.
   */
  public static final StaticTrustAnchors CA_CERTS;

  // Static initializer.
  // -------------------------------------------------------------------------

  static
  {
    LinkedList certs = new LinkedList();
    CertificateFactory factory = null;

    try
      {
        factory = CertificateFactory.getInstance("X.509");
      }
    catch (CertificateException ce)
      {
        throw new Error(ce.toString());
      }

    X509Certificate cert = generate(factory,
      // ABAecom_=sub.__Am._Bankers_Assn.=_Root_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDtTCCAp2gAwIBAgIRANAeQJAAAEZSAAAAAQAAAAQwDQYJKoZIhvcNAQEF\n" +
      "BQAwgYkxCzAJBgNVBAYTAlVTMQswCQYDVQQIEwJEQzETMBEGA1UEBxMKV2Fz\n" +
      "aGluZ3RvbjEXMBUGA1UEChMOQUJBLkVDT00sIElOQy4xGTAXBgNVBAMTEEFC\n" +
      "QS5FQ09NIFJvb3QgQ0ExJDAiBgkqhkiG9w0BCQEWFWFkbWluQGRpZ3NpZ3Ry\n" +
      "dXN0LmNvbTAeFw05OTA3MTIxNzMzNTNaFw0wOTA3MDkxNzMzNTNaMIGJMQsw\n" +
      "CQYDVQQGEwJVUzELMAkGA1UECBMCREMxEzARBgNVBAcTCldhc2hpbmd0b24x\n" +
      "FzAVBgNVBAoTDkFCQS5FQ09NLCBJTkMuMRkwFwYDVQQDExBBQkEuRUNPTSBS\n" +
      "b290IENBMSQwIgYJKoZIhvcNAQkBFhVhZG1pbkBkaWdzaWd0cnVzdC5jb20w\n" +
      "ggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCx0xHgeVVDBwhMywVC\n" +
      "AOINg0Y95JO6tgbTDVm9PsHOQ2cBiiGo77zM0KLMsFWWU4RmBQDaREmA2FQK\n" +
      "pSWGlO1jVv9wbKOhGdJ4vmgqRF4vz8wYXke8OrFGPR7wuSw0X4x8TAgpnUBV\n" +
      "6zx9g9618PeKgw6hTLQ6pbNfWiKX7BmbwQVo/ea3qZGULOR4SCQaJRk665Wc\n" +
      "OQqKz0Ky8BzVX/tr7WhWezkscjiw7pOp03t3POtxA6k4ShZsiSrK2jMTecJV\n" +
      "jO2cu/LLWxD4LmE1xilMKtAqY9FlWbT4zfn0AIS2V0KFnTKo+SpU+/94Qby9\n" +
      "cSj0u5C8/5Y0BONFnqFGKECBAgMBAAGjFjAUMBIGA1UdEwEB/wQIMAYBAf8C\n" +
      "AQgwDQYJKoZIhvcNAQEFBQADggEBAARvJYbk5pYntNlCwNDJALF/VD6Hsm0k\n" +
      "qS8Kfv2kRLD4VAe9G52dyntQJHsRW0mjpr8SdNWJt7cvmGQlFLdh6X9ggGvT\n" +
      "ZOirvRrWUfrAtF13Gn9kCF55xgVM8XrdTX3O5kh7VNJhkoHWG9YA8A6eKHeg\n" +
      "TYjHInYZw8eeG6Z3ePhfm1bR8PIXrI6dWeYf/le22V7hXZ9F7GFoGUHhsiAm\n" +
      "/lowdiT/QHI8eZ98IkirRs3bs4Ysj78FQdPB4xTjQRcm0HyncUwZ6EoPclgx\n" +
      "fexgeqMiKL0ZJGA/O4dzwGvky663qyVDslUte6sGDnVdNOVdc22esnVApVnJ\n" +
      "TzFxiNmIf1Q=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // AOL_Time_Warner_Root_Certification_Authority_1.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIID5jCCAs6gAwIBAgIBATANBgkqhkiG9w0BAQUFADCBgzELMAkGA1UEBhMC\n" +
      "VVMxHTAbBgNVBAoTFEFPTCBUaW1lIFdhcm5lciBJbmMuMRwwGgYDVQQLExNB\n" +
      "bWVyaWNhIE9ubGluZSBJbmMuMTcwNQYDVQQDEy5BT0wgVGltZSBXYXJuZXIg\n" +
      "Um9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAxMB4XDTAyMDUyOTA2MDAw\n" +
      "MFoXDTM3MTEyMDE1MDMwMFowgYMxCzAJBgNVBAYTAlVTMR0wGwYDVQQKExRB\n" +
      "T0wgVGltZSBXYXJuZXIgSW5jLjEcMBoGA1UECxMTQW1lcmljYSBPbmxpbmUg\n" +
      "SW5jLjE3MDUGA1UEAxMuQU9MIFRpbWUgV2FybmVyIFJvb3QgQ2VydGlmaWNh\n" +
      "dGlvbiBBdXRob3JpdHkgMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC\n" +
      "ggEBAJnej8Mlo2k06AX3dLm/WpcZuS+U0pPlLYnKhHw/EEMbjIt8hFj4JHxI\n" +
      "zyr9wBXZGH6EGhfT257XyuTZ16pYUYfw8ItITuLCxFlpMGK2MKKMCxGZYTVt\n" +
      "fu/FsRkGIBKOQuHfD5YQUqjPnF+VFNivO3ULMSAfRC+iYkGzuxgh28pxPIzs\n" +
      "trkNn+9R7017EvILDOGsQI93f7DKeHEMXRZxcKLXwjqFzQ6axOAAsNUl6twr\n" +
      "5JQtOJyJQVdkKGUZHLZEtMgxa44Be3ZZJX8VHIQIfHNlIAqhBC4aMqiaILGc\n" +
      "LCFZ5/vP7nAtCMpjPiybkxlqpMKX/7eGV4iFbJ4VFitNLLMCAwEAAaNjMGEw\n" +
      "DwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUoTYwFsuGkABFgFOxj8jYPXy+\n" +
      "XxIwHwYDVR0jBBgwFoAUoTYwFsuGkABFgFOxj8jYPXy+XxIwDgYDVR0PAQH/\n" +
      "BAQDAgGGMA0GCSqGSIb3DQEBBQUAA4IBAQCKIBilvrMvtKaEAEAwKfq0FHNM\n" +
      "eUWn9nDg6H5kHgqVfGphwu9OH77/yZkfB2FK4V1Mza3u0FIy2VkyvNp5ctZ7\n" +
      "CegCgTXTCt8RHcl5oIBN/lrXVtbtDyqvpxh1MwzqwWEFT2qaifKNuZ8u77Bf\n" +
      "WgDrvq2g+EQFZ7zLBO+eZMXpyD8Fv8YvBxzDNnGGyjhmSs3WuEvGbKeXO/oT\n" +
      "LW4jYYehY0KswsuXn2Fozy1MBJ3XJU8KDk2QixhWqJNIV9xvrr2eZ1d3iVCz\n" +
      "vhGbRWeDhhmH05i9CBoWH1iCC+GWaQVLjuyDUTEH1dSf/1l7qG6Fz9NLqUmw\n" +
      "X7A5KGgOc90lmt4S\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // AOL_Time_Warner_Root_Certification_Authority_2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIF5jCCA86gAwIBAgIBATANBgkqhkiG9w0BAQUFADCBgzELMAkGA1UEBhMC\n" +
      "VVMxHTAbBgNVBAoTFEFPTCBUaW1lIFdhcm5lciBJbmMuMRwwGgYDVQQLExNB\n" +
      "bWVyaWNhIE9ubGluZSBJbmMuMTcwNQYDVQQDEy5BT0wgVGltZSBXYXJuZXIg\n" +
      "Um9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAyMB4XDTAyMDUyOTA2MDAw\n" +
      "MFoXDTM3MDkyODIzNDMwMFowgYMxCzAJBgNVBAYTAlVTMR0wGwYDVQQKExRB\n" +
      "T0wgVGltZSBXYXJuZXIgSW5jLjEcMBoGA1UECxMTQW1lcmljYSBPbmxpbmUg\n" +
      "SW5jLjE3MDUGA1UEAxMuQU9MIFRpbWUgV2FybmVyIFJvb3QgQ2VydGlmaWNh\n" +
      "dGlvbiBBdXRob3JpdHkgMjCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\n" +
      "ggIBALQ3WggWmRToVbEbJGv8x4vmh6mJ7ouZzU9AhqS2TcnZsdw8TQ2FTBVs\n" +
      "RotSeJ/4I/1n9SQ6aF3Q92RhQVSji6UI0ilbm2BPJoPRYxJWSXakFsKlnUWs\n" +
      "i4SVqBax7J/qJBrvuVdcmiQhLE0OcR+mrF1FdAOYxFSMFkpBd4aVdQxHAWZg\n" +
      "/BXxD+r1FHjHDtdugRxev17nOirYlxcwfACtCJ0zr7iZYYCLqJV+FNwSbKTQ\n" +
      "2O9ASQI2+W6p1h2WVgSysy0WVoaP2SBXgM1nEG2wTPDaRrbqJS5Gr42whTg0\n" +
      "ixQmgiusrpkLjhTXUr2eacOGAgvqdnUxCc4zGSGFQ+aJLZ8lN2fxI2rSAG2X\n" +
      "+Z/nKcrdH9cG6rjJuQkhn8g/BsXS6RJGAE57COtCPStIbp1n3UsC5ETzkxml\n" +
      "J85per5n0/xQpCyrw2u544BMzwVhSyvcG7mm0tCq9Stz+86QNZ8MUhy/XCFh\n" +
      "EVsVS6kkUfykXPcXnbDS+gfpj1bkGoxoigTTfFrjnqKhynFbotSg5ymFXQNo\n" +
      "Kk/SBtc9+cMDLz9l+WceR0DTYw/j1Y75hauXTLPXJuuWCpTehTacyH+BCQJJ\n" +
      "Kg71ZDIMgtG6aoIbs0t0EfOMd9afv9w3pKdVBC/UMejTRrkDfNoSTllkt1Ex\n" +
      "MVCgyhwn2RAurda9EGYrw7AiShJbAgMBAAGjYzBhMA8GA1UdEwEB/wQFMAMB\n" +
      "Af8wHQYDVR0OBBYEFE9pbQN+nZ8HGEO8txBO1b+pxCAoMB8GA1UdIwQYMBaA\n" +
      "FE9pbQN+nZ8HGEO8txBO1b+pxCAoMA4GA1UdDwEB/wQEAwIBhjANBgkqhkiG\n" +
      "9w0BAQUFAAOCAgEAO/Ouyuguh4X7ZVnnrREUpVe8WJ8kEle7+z802u6teio0\n" +
      "cnAxa8cZmIDJgt43d15Ui47y6mdPyXSEkVYJ1eV6moG2gcKtNuTxVBFT8zRF\n" +
      "ASbI5Rq8NEQh3q0l/HYWdyGQgJhXnU7q7C+qPBR7V8F+GBRn7iTGvboVsNIY\n" +
      "vbdVgaxTwOjdaRITQrcCtQVBynlQboIOcXKTRuidDV29rs4prWPVVRaAMCf/\n" +
      "drr3uNZK49m1+VLQTkCpx+XCMseqdiThawVQ68W/ClTluUI8JPu3B5wwn3la\n" +
      "5uBAUhX0/Kr0VvlEl4ftDmVyXr4m+02kLQgH3thcoNyBM5kYJRF3p+v9WAks\n" +
      "mWsbivNSPxpNSGDxoPYzAlOL7SUJuA0t7Zdz7NeWH45gDtoQmy8YJPamTQr5\n" +
      "O8t1wswvziRpyQoijlmn94IM19drNZxDAGrElWe6nEXLuA4399xOAU++CrYD\n" +
      "062KRffaJ00psUjf5BHklka9bAI+1lHIlRcBFanyqqryvy9lG2/QuRqT9Y41\n" +
      "xICHPpQvZuTpqP9BnHAqTyo5GJUefvthATxRCC4oGKQWDzH9OmwjkyB24f0H\n" +
      "hdFbP9IcczLd+rn4jM8Ch3qaluTtT4mNU0OrDhPAARW0eTjb/G49nlG2uBOL\n" +
      "Z8/5fNkiHfZdxRwBL5joeiQYvITX+txyW/fBOmg=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // AddTrust_External_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIENjCCAx6gAwIBAgIBATANBgkqhkiG9w0BAQUFADBvMQswCQYDVQQGEwJT\n" +
      "RTEUMBIGA1UEChMLQWRkVHJ1c3QgQUIxJjAkBgNVBAsTHUFkZFRydXN0IEV4\n" +
      "dGVybmFsIFRUUCBOZXR3b3JrMSIwIAYDVQQDExlBZGRUcnVzdCBFeHRlcm5h\n" +
      "bCBDQSBSb290MB4XDTAwMDUzMDEwNDgzOFoXDTIwMDUzMDEwNDgzOFowbzEL\n" +
      "MAkGA1UEBhMCU0UxFDASBgNVBAoTC0FkZFRydXN0IEFCMSYwJAYDVQQLEx1B\n" +
      "ZGRUcnVzdCBFeHRlcm5hbCBUVFAgTmV0d29yazEiMCAGA1UEAxMZQWRkVHJ1\n" +
      "c3QgRXh0ZXJuYWwgQ0EgUm9vdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC\n" +
      "AQoCggEBALf3GjPm8gAELTngTlvtH7xsD821+iO2zt6bETOXpClMfZOfvUq8\n" +
      "k+0DGuOPz+VtUFrWlymUWoCwSXrbLpX9uMq/NzgtHj6RQa1wVsfwTz/oMp50\n" +
      "ysiQVOnGXw94nZpAPA6sYapeFI+eh6FqUNzXmk6vBbOmcZSccbNQYArHE504\n" +
      "B4YCqOmoaSYYkKtMsE8jqzpPhNjfzp/haW+710LXa0Tkx63ubUFfclpxCDez\n" +
      "eWWkWaCUN/cALw3CknLa0Dhy2xSoRcRdKn23tNbE7qzNE0S3ySvdQwAl+mG5\n" +
      "aWpYIxG3pzOPVnVZ9c0p10a3CitlttNCbxWyuHv77+ldU9U0WicCAwEAAaOB\n" +
      "3DCB2TAdBgNVHQ4EFgQUrb2YejS0Jvf6xCZU7wO94CTLVBowCwYDVR0PBAQD\n" +
      "AgEGMA8GA1UdEwEB/wQFMAMBAf8wgZkGA1UdIwSBkTCBjoAUrb2YejS0Jvf6\n" +
      "xCZU7wO94CTLVBqhc6RxMG8xCzAJBgNVBAYTAlNFMRQwEgYDVQQKEwtBZGRU\n" +
      "cnVzdCBBQjEmMCQGA1UECxMdQWRkVHJ1c3QgRXh0ZXJuYWwgVFRQIE5ldHdv\n" +
      "cmsxIjAgBgNVBAMTGUFkZFRydXN0IEV4dGVybmFsIENBIFJvb3SCAQEwDQYJ\n" +
      "KoZIhvcNAQEFBQADggEBALCb4IUlwtYj4g+WBpKdQZic2YR5gdkeWxQHIzZl\n" +
      "j7DYd7usQWxHYINRsPkyPef89iYTx4AWpb9a/IfPeHmJIZriTAcKhjW88t5R\n" +
      "xNKWt9x+Tu5w/Rw56wwCURQtjr0W4MHfRnXnJK3s9EK0hZNwEGe6nQY1ShjT\n" +
      "K3rMUUKhemPR5ruhxSvCNr4TDea9Y355e6cJDUCrat2PisP29owaQgVR1EX1\n" +
      "n6diIWgVIEM8med8vSTYqZEXc4g/VhsxOBi0cQ+azcgOno4uG+GMmIPLHzHx\n" +
      "REzGBHNJdmAPx/i9F4BrLunMTA5amnkPIAou1Z5jJh5VkpTYghdae9C8x49O\n" +
      "hgQ=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // AddTrust_Low-Value_Services_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEGDCCAwCgAwIBAgIBATANBgkqhkiG9w0BAQUFADBlMQswCQYDVQQGEwJT\n" +
      "RTEUMBIGA1UEChMLQWRkVHJ1c3QgQUIxHTAbBgNVBAsTFEFkZFRydXN0IFRU\n" +
      "UCBOZXR3b3JrMSEwHwYDVQQDExhBZGRUcnVzdCBDbGFzcyAxIENBIFJvb3Qw\n" +
      "HhcNMDAwNTMwMTAzODMxWhcNMjAwNTMwMTAzODMxWjBlMQswCQYDVQQGEwJT\n" +
      "RTEUMBIGA1UEChMLQWRkVHJ1c3QgQUIxHTAbBgNVBAsTFEFkZFRydXN0IFRU\n" +
      "UCBOZXR3b3JrMSEwHwYDVQQDExhBZGRUcnVzdCBDbGFzcyAxIENBIFJvb3Qw\n" +
      "ggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCWltQhSWDia+hBBwze\n" +
      "xODcEyPNwTXH+9ZOEQpnXvUGW2ulCDtbKRY654eyNAbFvAWlA3yCyykQruGI\n" +
      "gb3WntP+LVbBFc7jJp0VLhD7Bo8wBN6ntGO0/7Gcrjyvd7ZWxbWroulpOj0O\n" +
      "M3kyP3CCkplhbY0wCI9xP6ZIVxn4JdxLZlyldI+Yrsj5wAYi56xz36Uu+1Lc\n" +
      "sRVlIPo1Zmne3yzxbrww2ywkEtvrNTVokMsAsJchPXQhI2U0K7t4WaPW4XY5\n" +
      "mqRJjox0r26kmqPZm9I4XJuiGMx1I4S+6+JNM3GOGvDC+Mcdoq0Dlyz4zyXG\n" +
      "9rgkMbFjXZJ/Y/AlyVMuH79NAgMBAAGjgdIwgc8wHQYDVR0OBBYEFJWxtPCU\n" +
      "tr3H2tERCSG+wa9J/RB7MAsGA1UdDwQEAwIBBjAPBgNVHRMBAf8EBTADAQH/\n" +
      "MIGPBgNVHSMEgYcwgYSAFJWxtPCUtr3H2tERCSG+wa9J/RB7oWmkZzBlMQsw\n" +
      "CQYDVQQGEwJTRTEUMBIGA1UEChMLQWRkVHJ1c3QgQUIxHTAbBgNVBAsTFEFk\n" +
      "ZFRydXN0IFRUUCBOZXR3b3JrMSEwHwYDVQQDExhBZGRUcnVzdCBDbGFzcyAx\n" +
      "IENBIFJvb3SCAQEwDQYJKoZIhvcNAQEFBQADggEBACxtZBsfzQ3duQH6lmM0\n" +
      "MkhHma6X7f1yFqZzR1r0693p9db7RcwpiURdv0Y5PejuvE1Uhh4dbOMXJ0Ph\n" +
      "iVYrqW9yTkkz43J8KiOavD7/KCrto/8cI7pDVwlnTUtiBi34/2ydYB7YHEt9\n" +
      "tTEv2dB8Xfjea4MYeDdXL+gzB2ffHsdrKpV2ro9Xo/D0UrSpUwjP4E/TelOL\n" +
      "/bscVjby/rK25Xa71SJlpz/+0WatC7xrmYbvP33zGDLKe8bjq2RGlfgmadlV\n" +
      "g3sslgf/WSxEo8bl6ancoWOAWiFeIc9TVPC6b4nbqKqVz4vjccweGyBECMB6\n" +
      "tkD9xOQ14R0WHNC8K47Wcdk=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // AddTrust_Public_Services_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEFTCCAv2gAwIBAgIBATANBgkqhkiG9w0BAQUFADBkMQswCQYDVQQGEwJT\n" +
      "RTEUMBIGA1UEChMLQWRkVHJ1c3QgQUIxHTAbBgNVBAsTFEFkZFRydXN0IFRU\n" +
      "UCBOZXR3b3JrMSAwHgYDVQQDExdBZGRUcnVzdCBQdWJsaWMgQ0EgUm9vdDAe\n" +
      "Fw0wMDA1MzAxMDQxNTBaFw0yMDA1MzAxMDQxNTBaMGQxCzAJBgNVBAYTAlNF\n" +
      "MRQwEgYDVQQKEwtBZGRUcnVzdCBBQjEdMBsGA1UECxMUQWRkVHJ1c3QgVFRQ\n" +
      "IE5ldHdvcmsxIDAeBgNVBAMTF0FkZFRydXN0IFB1YmxpYyBDQSBSb290MIIB\n" +
      "IjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA6Rowj4OIFMEg2Dybjxt+\n" +
      "A3S72mnTRqX4jsIMEZBRpS9mVEBV6tsfSlbunyNu9DnLoblv8n75XYcmYZ4c\n" +
      "+OLspoH4IcUkzBEMP9smcnrHAZcHF/nXGCwwfQ56HmIexkvA/X1id9NEHif2\n" +
      "P0tEs7c42TkfYNVRknMDtABp4/MUTu7R3AnPdzRGULD4EfL+OHn3Bzn+UZKX\n" +
      "C1sIXzSGAa2Il+tmzV7R/9x98oTaunet3IAIx6eH1lWfl2royBFkuucZKT8R\n" +
      "s3iQhCBSWxHveNCD9tVIkNAwHM+A+WD+eeSI8t0A65RF62WUaUC6wNW0uLp9\n" +
      "BBGo6zEFlpROWCGOn9Bg/QIDAQABo4HRMIHOMB0GA1UdDgQWBBSBPjfYkrAf\n" +
      "d59ctKtzquf2NGAv+jALBgNVHQ8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zCB\n" +
      "jgYDVR0jBIGGMIGDgBSBPjfYkrAfd59ctKtzquf2NGAv+qFopGYwZDELMAkG\n" +
      "A1UEBhMCU0UxFDASBgNVBAoTC0FkZFRydXN0IEFCMR0wGwYDVQQLExRBZGRU\n" +
      "cnVzdCBUVFAgTmV0d29yazEgMB4GA1UEAxMXQWRkVHJ1c3QgUHVibGljIENB\n" +
      "IFJvb3SCAQEwDQYJKoZIhvcNAQEFBQADggEBAAP3FUr4JNojVhaTdt02KLmu\n" +
      "G7jD8WS6IBh4lSknVwW8fCr0uVFV2ocC3g8WFzH4qnkuCRO7r7IgGRLlk/lL\n" +
      "+YPoRNWyQSW/iHVv/xD8SlTQX/D67zZzfRs2RcYhbbQVuE7PnFylPVoAjgbj\n" +
      "PGsye/Kf8Lb93/AoGEjwxrzQvzSAlsJKsW2Ox5BF3i9nrEUEo3rcVZLJR2bY\n" +
      "GozH7ZxOmuASu7VqTITh4SINhwBk/ox9Yjllpu9CtoAlEmEBqCQTcAARJl/6\n" +
      "NVDFSMwGR+gn2HCNX2TmoUQmXiLsks3/QppEIW1cxeMiHV9HEufOX1362Kqx\n" +
      "My3ZdvJOOjMMK7MtkAY=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // AddTrust_Qualified_Certificates_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEHjCCAwagAwIBAgIBATANBgkqhkiG9w0BAQUFADBnMQswCQYDVQQGEwJT\n" +
      "RTEUMBIGA1UEChMLQWRkVHJ1c3QgQUIxHTAbBgNVBAsTFEFkZFRydXN0IFRU\n" +
      "UCBOZXR3b3JrMSMwIQYDVQQDExpBZGRUcnVzdCBRdWFsaWZpZWQgQ0EgUm9v\n" +
      "dDAeFw0wMDA1MzAxMDQ0NTBaFw0yMDA1MzAxMDQ0NTBaMGcxCzAJBgNVBAYT\n" +
      "AlNFMRQwEgYDVQQKEwtBZGRUcnVzdCBBQjEdMBsGA1UECxMUQWRkVHJ1c3Qg\n" +
      "VFRQIE5ldHdvcmsxIzAhBgNVBAMTGkFkZFRydXN0IFF1YWxpZmllZCBDQSBS\n" +
      "b290MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA5B6a/twJWoek\n" +
      "n0e+EV+vhDTbYjx5eLfpMLXsDBwqxBb/4Oxx64r1EW7tTw2R0hIYLUkVAcKk\n" +
      "IhPHEWT/IhKauY5cLwjPcWqzZwFZ8V1G87B4pfYOQnrjfxvM0PC3KP0q6p6z\n" +
      "sLkEqv32x7SxuCqg+1jxGaBvcCV+PmlKfw8i2O+tCBGaKZnhqkRFmhJePp1t\n" +
      "UvznoD1oL/BLcHwTOK28FSXx1s6rosAx1i+f4P8UWfyEk9mHfExUE+uf0S0R\n" +
      "+Bg6Ot4l2ffTQO2kBhLEO+GRwVY18BTcZTYJbqukB8c10cIDMzZbdSZtQvES\n" +
      "a0NvS3GU+jQd7RNuyoB/mC9suWXY6QIDAQABo4HUMIHRMB0GA1UdDgQWBBQ5\n" +
      "lYtii1zJ1IC6WA+XPxUIQ8yYpzALBgNVHQ8EBAMCAQYwDwYDVR0TAQH/BAUw\n" +
      "AwEB/zCBkQYDVR0jBIGJMIGGgBQ5lYtii1zJ1IC6WA+XPxUIQ8yYp6FrpGkw\n" +
      "ZzELMAkGA1UEBhMCU0UxFDASBgNVBAoTC0FkZFRydXN0IEFCMR0wGwYDVQQL\n" +
      "ExRBZGRUcnVzdCBUVFAgTmV0d29yazEjMCEGA1UEAxMaQWRkVHJ1c3QgUXVh\n" +
      "bGlmaWVkIENBIFJvb3SCAQEwDQYJKoZIhvcNAQEFBQADggEBABmrder4i2Vh\n" +
      "lRO6aQTvhsoToMeqT2QbPxj2qC0sVY8FtzDqQmodwCVRLae/DLPt7wh/bDxG\n" +
      "GuoYQ992zPlmhpwsaPXpF/gxsxjE1kh9I0xowX67ARRvxdlu3rsEQmr49lx9\n" +
      "5dr6h+sNNVJn0J6XdgWTP5XHAeZpVTh/EGGZyeNfpso+gmNIquIISD6q8rKF\n" +
      "Yqa0p9m9N5xotS1WfbC3P6CxB9bpT9zeRXEwMn8bLgn5v1Kh7sKAPgZcLlVA\n" +
      "wRv1cEWw3F369nJad9Jjzc9YiQBCYz95OdBEsIJuQRno3eDBiFrRHnGTHyQw\n" +
      "dOUeqN48Jzd/g66ed8/wMLH/S5noxqE=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // America_Online_Root_Certification_Authority_1.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDpDCCAoygAwIBAgIBATANBgkqhkiG9w0BAQUFADBjMQswCQYDVQQGEwJV\n" +
      "UzEcMBoGA1UEChMTQW1lcmljYSBPbmxpbmUgSW5jLjE2MDQGA1UEAxMtQW1l\n" +
      "cmljYSBPbmxpbmUgUm9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAxMB4X\n" +
      "DTAyMDUyODA2MDAwMFoXDTM3MTExOTIwNDMwMFowYzELMAkGA1UEBhMCVVMx\n" +
      "HDAaBgNVBAoTE0FtZXJpY2EgT25saW5lIEluYy4xNjA0BgNVBAMTLUFtZXJp\n" +
      "Y2EgT25saW5lIFJvb3QgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgMTCCASIw\n" +
      "DQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKgv6KRpBgNHw+kqmP8ZonCa\n" +
      "xlCyfqXfaE0bfA+2l2h9LaaLl+lkhsmj76CGv2BlnEtUiMJIxUo5vxTjWVXl\n" +
      "GbR0yLQFOVwWpeKVBeASrlmLojNoWBym1BW32J/X3HGrfpq/m44zDyL9Hy7n\n" +
      "BzbvYjnF3cu6JRQj3gzGPTzOggjmZj7aUTsWOqMFf6Dch9Wc/HKpoH145Lcx\n" +
      "VR5lu9RhsCFg7RAycsWSJR74kEoYeEfffjA3PlAb2xzTa5qGUwew76wGePiE\n" +
      "mf4hjUyAtgyC9mZweRrTT6PP8c9GsEsPPt2IYriMqQkoO3rHl+Ee5fSfwMCu\n" +
      "JKDIodkP1nsmgmkyPacCAwEAAaNjMGEwDwYDVR0TAQH/BAUwAwEB/zAdBgNV\n" +
      "HQ4EFgQUAK3Zo/Z59m50qX8zPYEX10zPM94wHwYDVR0jBBgwFoAUAK3Zo/Z5\n" +
      "9m50qX8zPYEX10zPM94wDgYDVR0PAQH/BAQDAgGGMA0GCSqGSIb3DQEBBQUA\n" +
      "A4IBAQB8itEfGDeC4Liwo+1WlchiYZwFos3CYiZhzRAW18y0ZTTQEYqtqKkF\n" +
      "Zu90821fnZmv9ov761KyBZiibyrFVL0lvV+uyIbqRizBs73B6UlwGBaXCBOM\n" +
      "IOAbLjpHyx7kADCVW/RFo8AasAFOq73AI25jP4BKxQft3OJvx8Fi8eNy1gTI\n" +
      "dGcL+oiroQHIb/AUr9KZzVGTfu0uOMe9zkZQPXLjeSWdm4grECDdpbgyn43g\n" +
      "Kd8hdIaC2y+CMMbHNYaz+ZZfRtsMRf3zUMNvxsNIrUam4SdHCh0Om7bCd39j\n" +
      "8uB9Gr784N/Xx6dssPmuujz9dLQR6FgNgLzTqIA6me11zEZ7\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // America_Online_Root_Certification_Authority_2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIFpDCCA4ygAwIBAgIBATANBgkqhkiG9w0BAQUFADBjMQswCQYDVQQGEwJV\n" +
      "UzEcMBoGA1UEChMTQW1lcmljYSBPbmxpbmUgSW5jLjE2MDQGA1UEAxMtQW1l\n" +
      "cmljYSBPbmxpbmUgUm9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAyMB4X\n" +
      "DTAyMDUyODA2MDAwMFoXDTM3MDkyOTE0MDgwMFowYzELMAkGA1UEBhMCVVMx\n" +
      "HDAaBgNVBAoTE0FtZXJpY2EgT25saW5lIEluYy4xNjA0BgNVBAMTLUFtZXJp\n" +
      "Y2EgT25saW5lIFJvb3QgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgMjCCAiIw\n" +
      "DQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBAMxBRR3pPU0Q9oyxQcngXssN\n" +
      "t79Hc9PwVU3dxgz6sWYFas14tNwC206B89enfHG8dWOgXeMHDEjsJcQDIPT/\n" +
      "DjsS/5uN4cbVG7RtIuOx238hZK+GvFciKtZHgVdEglZTvYYUAQv8f3SkWq7x\n" +
      "uhG1m1hagLQ3eAkzfDJHA1zEpYNI9FdWboE2JxhP7JsowtS013wMPgwr38oE\n" +
      "18aO6lhOqKSlGBxsRZijQdEt0sdtjRnxrXm3gT+9BoInLRBYBbV4Bbkv2wxr\n" +
      "kJB+FFk4u5QkE+XRnRTf04JNRvCAOVIyD+OEsnpD8l7eXz8d3eOyG6ChKiMD\n" +
      "bi4BFYdcpnV1x5dhvt6G3NRI270qv0pV2uh9UPu0gBe4lL8BPeraunzgWGcX\n" +
      "uVjgiIZGZ2ydEEdYMtA1fHkqkKJaEBEjNa0vzORKW6fIJ/KD3l67Xnfn6KVu\n" +
      "Y8INXWHQjNJsWiEOyiijzirplcdIz5ZvHZIlyMbGwcEMBawmxNJ10uEqZ8A9\n" +
      "W6Wa6897GqidFEXlD6CaZd4vKL3Ob5Rmg0gp2OpljK+T2WSfVVcmv2/LNzGZ\n" +
      "o2C7HK2JNDJiuEMhBnIMoVxtRsX6Kc8w3onccVvdtjc+31D1uAclJuW8tf48\n" +
      "ArO3+L5DwYcRlJ4jbBeKuIonDFRH8KmzwICMoCfrHRnjB453cMor9H124Hhn\n" +
      "AgMBAAGjYzBhMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFE1FwWg4u3Op\n" +
      "aaEg5+31IqEjFNeeMB8GA1UdIwQYMBaAFE1FwWg4u3OpaaEg5+31IqEjFNee\n" +
      "MA4GA1UdDwEB/wQEAwIBhjANBgkqhkiG9w0BAQUFAAOCAgEAZ2sGuV9FOypL\n" +
      "M7PmG2tZTiLMubekJcmnxPBUlgtk87FYT15R/LKXeydlwuXK5w0MJXti4/qf\n" +
      "tIe3RUavg6WXSIylvfEWK5t2LHo1YGwRgJfMqZJS5ivmae2p+DYtLHe/YUjR\n" +
      "Ywu5W1LtGLBDQiKmsXeu3mnFzcccobGlHBD7GL4acN3Bkku+KVqdPzW+5X1R\n" +
      "+FXgJXUjhx5c3LqdsKyzadsXg8n33gy8CNyRnqjQ1xU3c6U1uPx+xURABsPr\n" +
      "+CKAXEfOAuMRn0T//ZoyzH1kUQ7rVyZ2OuMeIjzCpjbdGe+n/BLzJsBZMYVM\n" +
      "nNjP36TMzCmT/5RtdlwTCJfy7aULTd3oyWgOZtMADjMSW7yV5TKQqLPGbIOt\n" +
      "d+6Lfn6xqavT4fG2wLHqiMDn05DpKJKUe2h7lyoKZy2FAjgQ5ANh1NolNscI\n" +
      "WC2hp1GvMApJ9aZphwctREZ2jirlmjvXGKL8nDgQzMY70rUXOm/9riW99XJZ\n" +
      "ZLF0KjhfGEzfz3EEWjbUvy+ZnOjZurGV5gJLIaFb1cFPj65pbVPbAZO1XB4Y\n" +
      "3WRayhgoPmMEEf0cjQAPuDffZ4qdZqkCapH/E8ovXYO8h5Ns3CRRFgQlZvqz\n" +
      "2cK6Kb6aSDiCmfS/O0oxGfm/jiEzFMpPVF/7zvuPcX/9XhmgD0uRuMRUvAaw\n" +
      "RY8mkaKO/qk=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Baltimore_CyberTrust_Code_Signing_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDpjCCAo6gAwIBAgIEAgAAvzANBgkqhkiG9w0BAQUFADBnMQswCQYDVQQG\n" +
      "EwJJRTESMBAGA1UEChMJQmFsdGltb3JlMRMwEQYDVQQLEwpDeWJlclRydXN0\n" +
      "MS8wLQYDVQQDEyZCYWx0aW1vcmUgQ3liZXJUcnVzdCBDb2RlIFNpZ25pbmcg\n" +
      "Um9vdDAeFw0wMDA1MTcxNDAxMDBaFw0yNTA1MTcyMzU5MDBaMGcxCzAJBgNV\n" +
      "BAYTAklFMRIwEAYDVQQKEwlCYWx0aW1vcmUxEzARBgNVBAsTCkN5YmVyVHJ1\n" +
      "c3QxLzAtBgNVBAMTJkJhbHRpbW9yZSBDeWJlclRydXN0IENvZGUgU2lnbmlu\n" +
      "ZyBSb290MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyHGaGBKO\n" +
      "etv5mvxBr9jy9AmOrT/+Zzc82skmULGxPsvoTnMA8rLc88VG+wnvGJbOp+Cc\n" +
      "hF0gDnqgqjaL+ii2eC6z7OhH8wTwkCO06q/lU7gF90ddK4bxp6TGOzW20g1S\n" +
      "Qdf0knXhogpQVoe+lwt7M4UQuSgY7jPqSBHXW5FHdiLU7s9d56hOHJ2Wkd2c\n" +
      "vXQJqHJhqrAhOvE9LANWCdLB3MO1x1Q3q+YmorJGcXPKEYjuvOdk99ARGnNA\n" +
      "WshJLA+375B/aIAEOAsbDzvU9aCzwo7hNLSAmW2edtSSKUCxldI3pGcSf+Bi\n" +
      "u641xZk2gkS45ngYM2Fxk1stjZ94lYLrbQIDAQABo1owWDATBgNVHSUEDDAK\n" +
      "BggrBgEFBQcDAzAdBgNVHQ4EFgQUyEE0XBUVBOVA8tGrmm8kknqHQlowEgYD\n" +
      "VR0TAQH/BAgwBgEB/wIBAzAOBgNVHQ8BAf8EBAMCAQYwDQYJKoZIhvcNAQEF\n" +
      "BQADggEBAFJ0qpVLIozHPZak/l36L7W86/AL6VY4HdFtDaG8aIvwxYClJDT9\n" +
      "8pYYEYahNvU351RA1WQfw19wQmstOceeUgXO52py0o1yP0dQg6vHjSXJsOOn\n" +
      "UxaVpmpT6hidj3ipd3ca+bSXR1mIJyi1yuEu1z4Oog24IkQD49FjsEE6ofWk\n" +
      "Lfd2HgRUmXgyQNcrfE26ppyweW4Hvozs7tc4aVvBDFZon/7r0eHIiPnyzX++\n" +
      "hbREZwBQPvQmA2Tqd33oXj4cN0fI1uqk8zY8l8I5cgWUGSXD1zdBD8Efh4r9\n" +
      "qr7psWRX5NuSoc/hSeg7H5ETWsOP2SVYSYBHD8YDrqzjv7fAqio=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Baltimore_CyberTrust_Mobile_Commerce_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICfTCCAeagAwIBAgIEAgAAuDANBgkqhkiG9w0BAQUFADBhMQswCQYDVQQG\n" +
      "EwJJRTESMBAGA1UEChMJQmFsdGltb3JlMRMwEQYDVQQLEwpDeWJlclRydXN0\n" +
      "MSkwJwYDVQQDEyBCYWx0aW1vcmUgQ3liZXJUcnVzdCBNb2JpbGUgUm9vdDAe\n" +
      "Fw0wMDA1MTIxODIwMDBaFw0yMDA1MTIyMzU5MDBaMGExCzAJBgNVBAYTAklF\n" +
      "MRIwEAYDVQQKEwlCYWx0aW1vcmUxEzARBgNVBAsTCkN5YmVyVHJ1c3QxKTAn\n" +
      "BgNVBAMTIEJhbHRpbW9yZSBDeWJlclRydXN0IE1vYmlsZSBSb290MIGfMA0G\n" +
      "CSqGSIb3DQEBAQUAA4GNADCBiQKBgQCjbbE4Vqz8tVYh3sCQXSZHgsZ9jx+g\n" +
      "hY8vu9ThHB3yJB8osC+5pKVvoiIgZP6ERzx+K2xparjUwJaOjFINzW9B1L8E\n" +
      "rqeBLy2YSNLBlKO1GV1dUWT0jkGwm8AtIqBexthaEmO8EUpeJhId4iYF5g9f\n" +
      "Ih96X3aUrs9aKA6rRdoiMQIDAQABo0IwQDAdBgNVHQ4EFgQUyeKPwAImWrbA\n" +
      "B+N/lAcY2y6lmnAwDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAYYw\n" +
      "DQYJKoZIhvcNAQEFBQADgYEAUwgLJgl4QnPU7Hp3Rw3jCzNx764zFE37+v0a\n" +
      "t1H15JkcBnHXKRnX5hUgUVFGbU/eGEmY0Ph4u3HojQEG1ddkj5TfR/6ghWk2\n" +
      "qS9CemhKEtaLC3BECqQE7yaIwTVxOF0bW0hC8OeUHHCVNKir9avieK318FL9\n" +
      "m+pCDOjYVL5TZvU=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Baltimore_CyberTrust_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDdzCCAl+gAwIBAgIEAgAAuTANBgkqhkiG9w0BAQUFADBaMQswCQYDVQQG\n" +
      "EwJJRTESMBAGA1UEChMJQmFsdGltb3JlMRMwEQYDVQQLEwpDeWJlclRydXN0\n" +
      "MSIwIAYDVQQDExlCYWx0aW1vcmUgQ3liZXJUcnVzdCBSb290MB4XDTAwMDUx\n" +
      "MjE4NDYwMFoXDTI1MDUxMjIzNTkwMFowWjELMAkGA1UEBhMCSUUxEjAQBgNV\n" +
      "BAoTCUJhbHRpbW9yZTETMBEGA1UECxMKQ3liZXJUcnVzdDEiMCAGA1UEAxMZ\n" +
      "QmFsdGltb3JlIEN5YmVyVHJ1c3QgUm9vdDCCASIwDQYJKoZIhvcNAQEBBQAD\n" +
      "ggEPADCCAQoCggEBAKMEuyKrmD1X6CZymrV51Cni4eiVgLGw41uOKymaZN+h\n" +
      "Xe2wCQVt2yguzmKiYv60iNoS6zjrIZ3AQSsBUnuId9Mcj8e6uYi1agnnc+gR\n" +
      "QKfRzMpijS3ljwumUNKoUMMo6vWrJYeKmpYcqWe4PwzV9/lSEy/CG9VwcPCP\n" +
      "wBLKBsua4dnKM3p31vjsufFoREJIE9LAwqSuXmD+tqYF/LTdB1kC1FkYmGP1\n" +
      "pWPgkAx9XbIGevOF6uvUA65ehD5f/xXtabz5OTZydc93Uk3zyZAsuT3lySNT\n" +
      "Px8kmCFcB5kpvcY67Oduhjprl3RjM71oGDHweI12v/yejl0qhqdNkNwnGjkC\n" +
      "AwEAAaNFMEMwHQYDVR0OBBYEFOWdWTCCR1jMrPoIVDaGezq1BE3wMBIGA1Ud\n" +
      "EwEB/wQIMAYBAf8CAQMwDgYDVR0PAQH/BAQDAgEGMA0GCSqGSIb3DQEBBQUA\n" +
      "A4IBAQCFDF2O5G9RaEIFoN27TyclhAO992T9Ldcw46QQF+vaKSm2eT929hkT\n" +
      "I7gQCvlYpNRhcL0EYWoSihfVCr3FvDB81ukMJY2GQE/szKN+OMY3EU/t3Wgx\n" +
      "jkzSswF07r51XgdIGn9w/xZchMB5hbgF/X++ZRGjD8ACtPhSNzkE1akxehi/\n" +
      "oCr0Epn3o0WC4zxe9Z2etciefC7IpJ5OCBRLbf1wbWsaY71k5h+3zvDyny67\n" +
      "G7fyUIhzksLi4xaNmjICq44Y3ekQEe5+NauQrz4wlHrQMz2nZQ/1/I6eYs9H\n" +
      "RCwBXbsdtTLSR9I4LtD+gdwyah617jzV/OeBHRnDJELqYzmp\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Digital_Signature_Trust_Co._Global_CA_1.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDKTCCApKgAwIBAgIENnAVljANBgkqhkiG9w0BAQUFADBGMQswCQYDVQQG\n" +
      "EwJVUzEkMCIGA1UEChMbRGlnaXRhbCBTaWduYXR1cmUgVHJ1c3QgQ28uMREw\n" +
      "DwYDVQQLEwhEU1RDQSBFMTAeFw05ODEyMTAxODEwMjNaFw0xODEyMTAxODQw\n" +
      "MjNaMEYxCzAJBgNVBAYTAlVTMSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVy\n" +
      "ZSBUcnVzdCBDby4xETAPBgNVBAsTCERTVENBIEUxMIGdMA0GCSqGSIb3DQEB\n" +
      "AQUAA4GLADCBhwKBgQCgbIGpzzQeJN3+hijM3oMv+V7UQtLodGBmE5gGHKlR\n" +
      "EmlvMVW5SXIACH7TpWJENySZj9mDSI+ZbZUTu0M7LklOiDfBu1h//uG9+Lth\n" +
      "zfNHwJmm8fOR6Hh8AMthyUQncWlVSn5JTe2io74CTADKAqjuAQIxZA9SLRN0\n" +
      "dja1erQtcQIBA6OCASQwggEgMBEGCWCGSAGG+EIBAQQEAwIABzBoBgNVHR8E\n" +
      "YTBfMF2gW6BZpFcwVTELMAkGA1UEBhMCVVMxJDAiBgNVBAoTG0RpZ2l0YWwg\n" +
      "U2lnbmF0dXJlIFRydXN0IENvLjERMA8GA1UECxMIRFNUQ0EgRTExDTALBgNV\n" +
      "BAMTBENSTDEwKwYDVR0QBCQwIoAPMTk5ODEyMTAxODEwMjNagQ8yMDE4MTIx\n" +
      "MDE4MTAyM1owCwYDVR0PBAQDAgEGMB8GA1UdIwQYMBaAFGp5fpFpRhgTCgJ3\n" +
      "pVlbYJglDqL4MB0GA1UdDgQWBBRqeX6RaUYYEwoCd6VZW2CYJQ6i+DAMBgNV\n" +
      "HRMEBTADAQH/MBkGCSqGSIb2fQdBAAQMMAobBFY0LjADAgSQMA0GCSqGSIb3\n" +
      "DQEBBQUAA4GBACIS2Hod3IEGtgllsofIH160L+nEHvI8wbsEkBFKg05+k7lN\n" +
      "QseSJqBcNJo4cvj9axY+IO6CizEqkzaFI4iKPANo08kJD038bKTaKHKTDomA\n" +
      "sH3+gG9lbRgzl4vCa4nuYD3Im+9/KzJic5PLPON74nZ4RbyhkwS7hp86W0N6\n" +
      "w4pl\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Digital_Signature_Trust_Co._Global_CA_2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIID2DCCAsACEQDQHkCLAAACfAAAAAIAAAABMA0GCSqGSIb3DQEBBQUAMIGp\n" +
      "MQswCQYDVQQGEwJ1czENMAsGA1UECBMEVXRhaDEXMBUGA1UEBxMOU2FsdCBM\n" +
      "YWtlIENpdHkxJDAiBgNVBAoTG0RpZ2l0YWwgU2lnbmF0dXJlIFRydXN0IENv\n" +
      "LjERMA8GA1UECxMIRFNUQ0EgWDExFjAUBgNVBAMTDURTVCBSb290Q0EgWDEx\n" +
      "ITAfBgkqhkiG9w0BCQEWEmNhQGRpZ3NpZ3RydXN0LmNvbTAeFw05ODEyMDEx\n" +
      "ODE4NTVaFw0wODExMjgxODE4NTVaMIGpMQswCQYDVQQGEwJ1czENMAsGA1UE\n" +
      "CBMEVXRhaDEXMBUGA1UEBxMOU2FsdCBMYWtlIENpdHkxJDAiBgNVBAoTG0Rp\n" +
      "Z2l0YWwgU2lnbmF0dXJlIFRydXN0IENvLjERMA8GA1UECxMIRFNUQ0EgWDEx\n" +
      "FjAUBgNVBAMTDURTVCBSb290Q0EgWDExITAfBgkqhkiG9w0BCQEWEmNhQGRp\n" +
      "Z3NpZ3RydXN0LmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB\n" +
      "ANLGJrbnpT3BxGjVUG9TxW9JEwm4ryxIjRRqoxdfWvnTLnUv2Chi0ZMv/E3U\n" +
      "q4flCMeZ55I/db3rJbQVwZsZPdJEjdd0IG03Ao9pk1uKxBmd9LIO/BZsubEF\n" +
      "koPRhSxglD5FVaDZqwgh5mDoO3TymVBRaNADLbGAvqPYUrBEzUNKcI5YhZXh\n" +
      "TizWLUFv1oTnyJhEykfbLCSlaSbPa7gnYsP0yXqSI+0TZ4KuRS5F5X5yP4Wd\n" +
      "lGIQ5jyRoa13AOAV7POEgHJ6jm5gl8ckWRA0g1vhpaRptlc1HHhZxtMvOnNn\n" +
      "7pTKBBMFYgZwI7P0fO5F2WQLW0mqpEPOJsREEmy43XkCAwEAATANBgkqhkiG\n" +
      "9w0BAQUFAAOCAQEAojeyP2n714Z5VEkxlTMr89EJFEliYIalsBHiUMIdBlc+\n" +
      "LegzZL6bqq1fG03UmZWii5rJYnK1aerZWKs17RWiQ9a2vAd5ZWRzfdd5ynvV\n" +
      "WlHG4VMElo04z6MXrDlxawHDi1M8Y+nuecDkvpIyZHqzH5eUYr3qsiAVlfuX\n" +
      "8ngvYzZAOONGDx3drJXK50uQe7FLqdTF65raqtWjlBRGjS0f8zrWkzr2Pnn8\n" +
      "6Oawde3uPclwx12qgUtGJRzHbBXjlU4PqjI3lAoXJJIThFjSY28r9+ZbYgsT\n" +
      "F7ANUkz+/m9c4pFuHf2kYtdo+o56T9II2pPc8JIRetDccpMMc5NihWjQ9A==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Digital_Signature_Trust_Co._Global_CA_3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDKTCCApKgAwIBAgIENm7TzjANBgkqhkiG9w0BAQUFADBGMQswCQYDVQQG\n" +
      "EwJVUzEkMCIGA1UEChMbRGlnaXRhbCBTaWduYXR1cmUgVHJ1c3QgQ28uMREw\n" +
      "DwYDVQQLEwhEU1RDQSBFMjAeFw05ODEyMDkxOTE3MjZaFw0xODEyMDkxOTQ3\n" +
      "MjZaMEYxCzAJBgNVBAYTAlVTMSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVy\n" +
      "ZSBUcnVzdCBDby4xETAPBgNVBAsTCERTVENBIEUyMIGdMA0GCSqGSIb3DQEB\n" +
      "AQUAA4GLADCBhwKBgQC/k48Xku8zExjrEH9OFr//Bo8qhbxe+SSmJIi2A7fB\n" +
      "w18DW9Fvrn5C6mYjuGODVvsoLeE4i7TuqAHhzhy2iCoiRoX7n6dwqUcUP87e\n" +
      "ZfCocfdPJmyMvMa1795JJ/9IKn3oTQPMx7JSxhcxEzu1TdvIxPbDDyQq2gyd\n" +
      "55FbgM2UnQIBA6OCASQwggEgMBEGCWCGSAGG+EIBAQQEAwIABzBoBgNVHR8E\n" +
      "YTBfMF2gW6BZpFcwVTELMAkGA1UEBhMCVVMxJDAiBgNVBAoTG0RpZ2l0YWwg\n" +
      "U2lnbmF0dXJlIFRydXN0IENvLjERMA8GA1UECxMIRFNUQ0EgRTIxDTALBgNV\n" +
      "BAMTBENSTDEwKwYDVR0QBCQwIoAPMTk5ODEyMDkxOTE3MjZagQ8yMDE4MTIw\n" +
      "OTE5MTcyNlowCwYDVR0PBAQDAgEGMB8GA1UdIwQYMBaAFB6CTShlgDzJQW6s\n" +
      "NS5ay97u+DlbMB0GA1UdDgQWBBQegk0oZYA8yUFurDUuWsve7vg5WzAMBgNV\n" +
      "HRMEBTADAQH/MBkGCSqGSIb2fQdBAAQMMAobBFY0LjADAgSQMA0GCSqGSIb3\n" +
      "DQEBBQUAA4GBAEeNg61i8tuwnkUiBbmi1gMOOHLnnvx75pO2mqWilMg0HZHR\n" +
      "xdf0CiUPPXiBng+xZ8SQTGPdXqfiup/1902lMXucKS1M/mQ+7LZT/uqb7YLb\n" +
      "dHVLB3luHtgZg3Pe9T7Qtd7nS2h9Qy4qIOF+oHhEngj1mPnHfxsb1gYgAlih\n" +
      "w6ID\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Digital_Signature_Trust_Co._Global_CA_4.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIID2DCCAsACEQDQHkCLAAB3bQAAAAEAAAAEMA0GCSqGSIb3DQEBBQUAMIGp\n" +
      "MQswCQYDVQQGEwJ1czENMAsGA1UECBMEVXRhaDEXMBUGA1UEBxMOU2FsdCBM\n" +
      "YWtlIENpdHkxJDAiBgNVBAoTG0RpZ2l0YWwgU2lnbmF0dXJlIFRydXN0IENv\n" +
      "LjERMA8GA1UECxMIRFNUQ0EgWDIxFjAUBgNVBAMTDURTVCBSb290Q0EgWDIx\n" +
      "ITAfBgkqhkiG9w0BCQEWEmNhQGRpZ3NpZ3RydXN0LmNvbTAeFw05ODExMzAy\n" +
      "MjQ2MTZaFw0wODExMjcyMjQ2MTZaMIGpMQswCQYDVQQGEwJ1czENMAsGA1UE\n" +
      "CBMEVXRhaDEXMBUGA1UEBxMOU2FsdCBMYWtlIENpdHkxJDAiBgNVBAoTG0Rp\n" +
      "Z2l0YWwgU2lnbmF0dXJlIFRydXN0IENvLjERMA8GA1UECxMIRFNUQ0EgWDIx\n" +
      "FjAUBgNVBAMTDURTVCBSb290Q0EgWDIxITAfBgkqhkiG9w0BCQEWEmNhQGRp\n" +
      "Z3NpZ3RydXN0LmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB\n" +
      "ANx18IzAdZaawGIfJvfE4Zrq4FZzW5nNAUSoCLbVp9oaBBg5kkp4o4HC9Xd6\n" +
      "ULRw/5qrxsfKboNPQpj7Jgva3G3WqZlVUmfpKAOS3OWwBZoPFflrWXJW8vo5\n" +
      "/Kpo7g8fEIMv/J36F5bdguPmRX3AS4BEH+0s4IT9kVySVGkl5WJp3OXuAFK9\n" +
      "MwutdQKFp2RQLcUZGTDAJtvJ0/0uma1ZtQtN1EGuhUhDWdy3qOKi3sOP17ih\n" +
      "YqZoUFLkzzGnlIXan0YyF1bl8utmPRL/Q9uY73fPy4GNNLHGUEom0eQ+QVCv\n" +
      "bK4iNC7Va26Dunm4dmVI2gkpZGMiuftHdoWMhkTLCdsCAwEAATANBgkqhkiG\n" +
      "9w0BAQUFAAOCAQEAtTYOXeFhKFoRZcA/gwN5Tb4opgsHAlKFzfiR0BBstWog\n" +
      "WxyQ2TA8xkieil5k+aFxd+8EJx8H6+Qm93N0yUQYGmbT4EOvkTvRyyzYdFQ6\n" +
      "HE3K1GjNI3wdEJ5F6fYAbqbNGf9PLCmPV03Ed5K+4EwJ+11EhmYhqLkyolbV\n" +
      "6YyDfFk/xPEL553snr2cGA4+wjl5KLcDDQjLxufZATdQEOzMYRZA1K8xdHv8\n" +
      "PzGn0EdzMzkbzE5q10mDEQb+64JYMzJM8FasHpwvVpp7wUocpf1VNs78lk30\n" +
      "sPDst2yC7S8xmUJMqbINuBVd8d+6ybVK1GSYsyapMMj9puyrliGtf8J4tg==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Entrust.net_Global_Secure_Personal_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEgzCCA+ygAwIBAgIEOJ725DANBgkqhkiG9w0BAQQFADCBtDEUMBIGA1UE\n" +
      "ChMLRW50cnVzdC5uZXQxQDA+BgNVBAsUN3d3dy5lbnRydXN0Lm5ldC9HQ0NB\n" +
      "X0NQUyBpbmNvcnAuIGJ5IHJlZi4gKGxpbWl0cyBsaWFiLikxJTAjBgNVBAsT\n" +
      "HChjKSAyMDAwIEVudHJ1c3QubmV0IExpbWl0ZWQxMzAxBgNVBAMTKkVudHJ1\n" +
      "c3QubmV0IENsaWVudCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTAeFw0wMDAy\n" +
      "MDcxNjE2NDBaFw0yMDAyMDcxNjQ2NDBaMIG0MRQwEgYDVQQKEwtFbnRydXN0\n" +
      "Lm5ldDFAMD4GA1UECxQ3d3d3LmVudHJ1c3QubmV0L0dDQ0FfQ1BTIGluY29y\n" +
      "cC4gYnkgcmVmLiAobGltaXRzIGxpYWIuKTElMCMGA1UECxMcKGMpIDIwMDAg\n" +
      "RW50cnVzdC5uZXQgTGltaXRlZDEzMDEGA1UEAxMqRW50cnVzdC5uZXQgQ2xp\n" +
      "ZW50IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIGfMA0GCSqGSIb3DQEBAQUA\n" +
      "A4GNADCBiQKBgQCTdLS25MVL1qFof2LV7PdRV7NySpj10InJrWPNTTVRaoTU\n" +
      "rcloeW+46xHbh65cJFET8VQlhK8pK5/jgOLZy93GRUk0iJBeAZfv6lOm3fzB\n" +
      "3ksqJeTpNfpVBQbliXrqpBFXO/x8PTbNZzVtpKklWb1m9fkn5JVn1j+SgF7y\n" +
      "NH0rhQIDAQABo4IBnjCCAZowEQYJYIZIAYb4QgEBBAQDAgAHMIHdBgNVHR8E\n" +
      "gdUwgdIwgc+ggcyggcmkgcYwgcMxFDASBgNVBAoTC0VudHJ1c3QubmV0MUAw\n" +
      "PgYDVQQLFDd3d3cuZW50cnVzdC5uZXQvR0NDQV9DUFMgaW5jb3JwLiBieSBy\n" +
      "ZWYuIChsaW1pdHMgbGlhYi4pMSUwIwYDVQQLExwoYykgMjAwMCBFbnRydXN0\n" +
      "Lm5ldCBMaW1pdGVkMTMwMQYDVQQDEypFbnRydXN0Lm5ldCBDbGllbnQgQ2Vy\n" +
      "dGlmaWNhdGlvbiBBdXRob3JpdHkxDTALBgNVBAMTBENSTDEwKwYDVR0QBCQw\n" +
      "IoAPMjAwMDAyMDcxNjE2NDBagQ8yMDIwMDIwNzE2NDY0MFowCwYDVR0PBAQD\n" +
      "AgEGMB8GA1UdIwQYMBaAFISLdP3FjcD/J20gN0V8/i3OutN9MB0GA1UdDgQW\n" +
      "BBSEi3T9xY3A/ydtIDdFfP4tzrrTfTAMBgNVHRMEBTADAQH/MB0GCSqGSIb2\n" +
      "fQdBAAQQMA4bCFY1LjA6NC4wAwIEkDANBgkqhkiG9w0BAQQFAAOBgQBObzWA\n" +
      "O9GK9Q6nIMstZVXQkvTnhLUGJoMShAusO7JE7r3PQNsgDrpuFOow4DtifH+L\n" +
      "a3xKp9U1PL6oXOpLu5OOgGarDyn9TS2/GpsKkMWr2tGzhtQvJFJcem3G8v7l\n" +
      "TRowjJDyutdKPkN+1MhQGof4T4HHdguEOnKdzmVml64mXg==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Entrust.net_Global_Secure_Server_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIElTCCA/6gAwIBAgIEOJsRPDANBgkqhkiG9w0BAQQFADCBujEUMBIGA1UE\n" +
      "ChMLRW50cnVzdC5uZXQxPzA9BgNVBAsUNnd3dy5lbnRydXN0Lm5ldC9TU0xf\n" +
      "Q1BTIGluY29ycC4gYnkgcmVmLiAobGltaXRzIGxpYWIuKTElMCMGA1UECxMc\n" +
      "KGMpIDIwMDAgRW50cnVzdC5uZXQgTGltaXRlZDE6MDgGA1UEAxMxRW50cnVz\n" +
      "dC5uZXQgU2VjdXJlIFNlcnZlciBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTAe\n" +
      "Fw0wMDAyMDQxNzIwMDBaFw0yMDAyMDQxNzUwMDBaMIG6MRQwEgYDVQQKEwtF\n" +
      "bnRydXN0Lm5ldDE/MD0GA1UECxQ2d3d3LmVudHJ1c3QubmV0L1NTTF9DUFMg\n" +
      "aW5jb3JwLiBieSByZWYuIChsaW1pdHMgbGlhYi4pMSUwIwYDVQQLExwoYykg\n" +
      "MjAwMCBFbnRydXN0Lm5ldCBMaW1pdGVkMTowOAYDVQQDEzFFbnRydXN0Lm5l\n" +
      "dCBTZWN1cmUgU2VydmVyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIGfMA0G\n" +
      "CSqGSIb3DQEBAQUAA4GNADCBiQKBgQDHwV9OcfHO8GCGD9JYf9Mzly0XonUw\n" +
      "tZZkJi9ow0SrqHXmAGc0V55lxyKbc+bT3QgON1WqJUaBbL3+qPZ1V1eMkGxK\n" +
      "wz6LS0MKyRFWmponIpnPVZ5h2QLifLZ8OAfc439PmrkDQYC2dWcTC5/oVzbI\n" +
      "XQA23mYU2m52H083jIITiQIDAQABo4IBpDCCAaAwEQYJYIZIAYb4QgEBBAQD\n" +
      "AgAHMIHjBgNVHR8EgdswgdgwgdWggdKggc+kgcwwgckxFDASBgNVBAoTC0Vu\n" +
      "dHJ1c3QubmV0MT8wPQYDVQQLFDZ3d3cuZW50cnVzdC5uZXQvU1NMX0NQUyBp\n" +
      "bmNvcnAuIGJ5IHJlZi4gKGxpbWl0cyBsaWFiLikxJTAjBgNVBAsTHChjKSAy\n" +
      "MDAwIEVudHJ1c3QubmV0IExpbWl0ZWQxOjA4BgNVBAMTMUVudHJ1c3QubmV0\n" +
      "IFNlY3VyZSBTZXJ2ZXIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkxDTALBgNV\n" +
      "BAMTBENSTDEwKwYDVR0QBCQwIoAPMjAwMDAyMDQxNzIwMDBagQ8yMDIwMDIw\n" +
      "NDE3NTAwMFowCwYDVR0PBAQDAgEGMB8GA1UdIwQYMBaAFMtswGvjuz7L/CKc\n" +
      "/vuLkpyw8m4iMB0GA1UdDgQWBBTLbMBr47s+y/winP77i5KcsPJuIjAMBgNV\n" +
      "HRMEBTADAQH/MB0GCSqGSIb2fQdBAAQQMA4bCFY1LjA6NC4wAwIEkDANBgkq\n" +
      "hkiG9w0BAQQFAAOBgQBi24GRzsiad0Iv7L0no1MPUBvqTpLwqa+poLpIYcvv\n" +
      "yQbvH9X07t9WLebKahlzqlO+krNQAraFJnJj2HVQYnUUt7NQGj/KEQALhUVp\n" +
      "bbalrlHhStyCP2yMNLJ3a9kC9n8O6mUE8c1UyrrJzOCE98g+EZfTYAkYvAX/\n" +
      "bIkz8OwVDw==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Entrust.net_Premium_2048_Secure_Server_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEXDCCA0SgAwIBAgIEOGO5ZjANBgkqhkiG9w0BAQUFADCBtDEUMBIGA1UE\n" +
      "ChMLRW50cnVzdC5uZXQxQDA+BgNVBAsUN3d3dy5lbnRydXN0Lm5ldC9DUFNf\n" +
      "MjA0OCBpbmNvcnAuIGJ5IHJlZi4gKGxpbWl0cyBsaWFiLikxJTAjBgNVBAsT\n" +
      "HChjKSAxOTk5IEVudHJ1c3QubmV0IExpbWl0ZWQxMzAxBgNVBAMTKkVudHJ1\n" +
      "c3QubmV0IENlcnRpZmljYXRpb24gQXV0aG9yaXR5ICgyMDQ4KTAeFw05OTEy\n" +
      "MjQxNzUwNTFaFw0xOTEyMjQxODIwNTFaMIG0MRQwEgYDVQQKEwtFbnRydXN0\n" +
      "Lm5ldDFAMD4GA1UECxQ3d3d3LmVudHJ1c3QubmV0L0NQU18yMDQ4IGluY29y\n" +
      "cC4gYnkgcmVmLiAobGltaXRzIGxpYWIuKTElMCMGA1UECxMcKGMpIDE5OTkg\n" +
      "RW50cnVzdC5uZXQgTGltaXRlZDEzMDEGA1UEAxMqRW50cnVzdC5uZXQgQ2Vy\n" +
      "dGlmaWNhdGlvbiBBdXRob3JpdHkgKDIwNDgpMIIBIjANBgkqhkiG9w0BAQEF\n" +
      "AAOCAQ8AMIIBCgKCAQEArU1LqRKGsuqjIAcVFmQqK0vRvwtKTY7tgHalZ7d4\n" +
      "QMBzQshowNtTK91euHaYNZOLGp18EzoOH1u3Hs/lJBQesYGpjX24zGtLA/EC\n" +
      "DNyrpUAkAH90lKGdCCmziAv1h3edVc3kw37XamSrhRSGlVuXMlBvPci6Zgzj\n" +
      "/L24ScF2iUkZ/cCovYmjZy/Gn7xxGWC4LeksyZB2ZnuU4q941mVTXTzWnLLP\n" +
      "KQP5L6RQstRIzgUyVYr9smRMDuSYB3Xbf9+5CFVghTAp+XtIpGmG4zU/HoZd\n" +
      "enoVve8AjhUiVBcAkCaTvA5JaJG/+EfTnZVCwQ5N328mz8MYIWJmQ3DW1cAH\n" +
      "4QIDAQABo3QwcjARBglghkgBhvhCAQEEBAMCAAcwHwYDVR0jBBgwFoAUVeSB\n" +
      "0RGAvtiJuQijMfmhJAkWuXAwHQYDVR0OBBYEFFXkgdERgL7YibkIozH5oSQJ\n" +
      "FrlwMB0GCSqGSIb2fQdBAAQQMA4bCFY1LjA6NC4wAwIEkDANBgkqhkiG9w0B\n" +
      "AQUFAAOCAQEAWUesIYSKF8mciVMeuoCFGsY8Tj6xnLZ8xpJdGGQC49MGCBFh\n" +
      "fGPjK50xA3B20qMooPS7mmNz7W3lKtvtFKkrxjYR0CvrB4ul2p5cGZ1WEvVU\n" +
      "KcgF7bISKo30Axv/55IQh7A6tcOdBTcSo8f0FbnVpDkWm1M6I5HxqIKiaoho\n" +
      "wXkCIryqptau37AUX7iH0N18f3v/rxzP5tsHrV7bhZ3QKw0z2wTR5klAEyt2\n" +
      "+z7pnIkPFc4YsIV4IU9rTw76NmfNB/L/CNDi3tm/Kq+4h4YhPATKt5Rof888\n" +
      "6ZjXOP/swNlQ8C5LWK5Gb9Auw2DaclVyvUxFnmG6v4SBkgPR0ml8xQ==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Entrust.net_Secure_Personal_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIE7TCCBFagAwIBAgIEOAOR7jANBgkqhkiG9w0BAQQFADCByTELMAkGA1UE\n" +
      "BhMCVVMxFDASBgNVBAoTC0VudHJ1c3QubmV0MUgwRgYDVQQLFD93d3cuZW50\n" +
      "cnVzdC5uZXQvQ2xpZW50X0NBX0luZm8vQ1BTIGluY29ycC4gYnkgcmVmLiBs\n" +
      "aW1pdHMgbGlhYi4xJTAjBgNVBAsTHChjKSAxOTk5IEVudHJ1c3QubmV0IExp\n" +
      "bWl0ZWQxMzAxBgNVBAMTKkVudHJ1c3QubmV0IENsaWVudCBDZXJ0aWZpY2F0\n" +
      "aW9uIEF1dGhvcml0eTAeFw05OTEwMTIxOTI0MzBaFw0xOTEwMTIxOTU0MzBa\n" +
      "MIHJMQswCQYDVQQGEwJVUzEUMBIGA1UEChMLRW50cnVzdC5uZXQxSDBGBgNV\n" +
      "BAsUP3d3dy5lbnRydXN0Lm5ldC9DbGllbnRfQ0FfSW5mby9DUFMgaW5jb3Jw\n" +
      "LiBieSByZWYuIGxpbWl0cyBsaWFiLjElMCMGA1UECxMcKGMpIDE5OTkgRW50\n" +
      "cnVzdC5uZXQgTGltaXRlZDEzMDEGA1UEAxMqRW50cnVzdC5uZXQgQ2xpZW50\n" +
      "IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIGdMA0GCSqGSIb3DQEBAQUAA4GL\n" +
      "ADCBhwKBgQDIOpleMRffrCdvkHvkGf9FozTC28GoT/Bo6oT9n3V5z8GKUZSv\n" +
      "x1cDR2SerYIbWtp/N3hHuzeYEpbOxhN979IMMFGpOZ5V+Pux5zDeg7K6PvHV\n" +
      "iTs7hbqqdCz+PzFur5GVbgbUB01LLFZHGARS2g4Qk79jkJvh34zmAqTmT173\n" +
      "iwIBA6OCAeAwggHcMBEGCWCGSAGG+EIBAQQEAwIABzCCASIGA1UdHwSCARkw\n" +
      "ggEVMIHkoIHhoIHepIHbMIHYMQswCQYDVQQGEwJVUzEUMBIGA1UEChMLRW50\n" +
      "cnVzdC5uZXQxSDBGBgNVBAsUP3d3dy5lbnRydXN0Lm5ldC9DbGllbnRfQ0Ff\n" +
      "SW5mby9DUFMgaW5jb3JwLiBieSByZWYuIGxpbWl0cyBsaWFiLjElMCMGA1UE\n" +
      "CxMcKGMpIDE5OTkgRW50cnVzdC5uZXQgTGltaXRlZDEzMDEGA1UEAxMqRW50\n" +
      "cnVzdC5uZXQgQ2xpZW50IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MQ0wCwYD\n" +
      "VQQDEwRDUkwxMCygKqAohiZodHRwOi8vd3d3LmVudHJ1c3QubmV0L0NSTC9D\n" +
      "bGllbnQxLmNybDArBgNVHRAEJDAigA8xOTk5MTAxMjE5MjQzMFqBDzIwMTkx\n" +
      "MDEyMTkyNDMwWjALBgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUxPucKXuXzUyW\n" +
      "/O5bs8qZdIuV6kwwHQYDVR0OBBYEFMT7nCl7l81MlvzuW7PKmXSLlepMMAwG\n" +
      "A1UdEwQFMAMBAf8wGQYJKoZIhvZ9B0EABAwwChsEVjQuMAMCBJAwDQYJKoZI\n" +
      "hvcNAQEEBQADgYEAP66K8ddmAwWePvrqHEa7pFuPeJoSSJn59DXeDDYHAmsQ\n" +
      "OokUgZwxpnyyQbJq5wcBoUv5nyU7lsqZwz6hURzzwy5E97BnRqqS5TvaHBkU\n" +
      "ODDV4qIxJS7x7EU47fgGWANzYrAQMY9Av2TgXD7FTx/aEkP/TOYGJqibGapE\n" +
      "PHayXOw=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Entrust.net_Secure_Server_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIE2DCCBEGgAwIBAgIEN0rSQzANBgkqhkiG9w0BAQUFADCBwzELMAkGA1UE\n" +
      "BhMCVVMxFDASBgNVBAoTC0VudHJ1c3QubmV0MTswOQYDVQQLEzJ3d3cuZW50\n" +
      "cnVzdC5uZXQvQ1BTIGluY29ycC4gYnkgcmVmLiAobGltaXRzIGxpYWIuKTEl\n" +
      "MCMGA1UECxMcKGMpIDE5OTkgRW50cnVzdC5uZXQgTGltaXRlZDE6MDgGA1UE\n" +
      "AxMxRW50cnVzdC5uZXQgU2VjdXJlIFNlcnZlciBDZXJ0aWZpY2F0aW9uIEF1\n" +
      "dGhvcml0eTAeFw05OTA1MjUxNjA5NDBaFw0xOTA1MjUxNjM5NDBaMIHDMQsw\n" +
      "CQYDVQQGEwJVUzEUMBIGA1UEChMLRW50cnVzdC5uZXQxOzA5BgNVBAsTMnd3\n" +
      "dy5lbnRydXN0Lm5ldC9DUFMgaW5jb3JwLiBieSByZWYuIChsaW1pdHMgbGlh\n" +
      "Yi4pMSUwIwYDVQQLExwoYykgMTk5OSBFbnRydXN0Lm5ldCBMaW1pdGVkMTow\n" +
      "OAYDVQQDEzFFbnRydXN0Lm5ldCBTZWN1cmUgU2VydmVyIENlcnRpZmljYXRp\n" +
      "b24gQXV0aG9yaXR5MIGdMA0GCSqGSIb3DQEBAQUAA4GLADCBhwKBgQDNKIM0\n" +
      "VBuJ8w+vN5Ex/68xYMmo6LIQaO2f55M28Qpku0f1BBc/I0dNxScZgSYMVHIN\n" +
      "iC3ZH5oSn7yzcdOAGT9HZnuMNSjSuQrfJNqc1lB5gXpa0zf3wkrYKZImZNHk\n" +
      "mGw6AIr1NJtl+O3jEP/9uElY3KDegjlrgbEWGWG5VLbmQwIBA6OCAdcwggHT\n" +
      "MBEGCWCGSAGG+EIBAQQEAwIABzCCARkGA1UdHwSCARAwggEMMIHeoIHboIHY\n" +
      "pIHVMIHSMQswCQYDVQQGEwJVUzEUMBIGA1UEChMLRW50cnVzdC5uZXQxOzA5\n" +
      "BgNVBAsTMnd3dy5lbnRydXN0Lm5ldC9DUFMgaW5jb3JwLiBieSByZWYuIChs\n" +
      "aW1pdHMgbGlhYi4pMSUwIwYDVQQLExwoYykgMTk5OSBFbnRydXN0Lm5ldCBM\n" +
      "aW1pdGVkMTowOAYDVQQDEzFFbnRydXN0Lm5ldCBTZWN1cmUgU2VydmVyIENl\n" +
      "cnRpZmljYXRpb24gQXV0aG9yaXR5MQ0wCwYDVQQDEwRDUkwxMCmgJ6AlhiNo\n" +
      "dHRwOi8vd3d3LmVudHJ1c3QubmV0L0NSTC9uZXQxLmNybDArBgNVHRAEJDAi\n" +
      "gA8xOTk5MDUyNTE2MDk0MFqBDzIwMTkwNTI1MTYwOTQwWjALBgNVHQ8EBAMC\n" +
      "AQYwHwYDVR0jBBgwFoAU8BdiE1U9s/8KAGv7UISX8+1i0BowHQYDVR0OBBYE\n" +
      "FPAXYhNVPbP/CgBr+1CEl/PtYtAaMAwGA1UdEwQFMAMBAf8wGQYJKoZIhvZ9\n" +
      "B0EABAwwChsEVjQuMAMCBJAwDQYJKoZIhvcNAQEFBQADgYEAkNwwAvpkdMKn\n" +
      "CqV8IY00F6j7Rw7/JXyNEwr75Ji174z4xRAN95K+8cPV1ZVqBLssziY2Zcgx\n" +
      "xufuP+NXdYR6Ee9GTxj005i7qIcyunL2POI9n9cd2cNgQ4xYDiKWL2KjLB+6\n" +
      "rQXvqzJ4h6BUcxm1XAX5Uj5tLUUL9wqT6u0G+bI=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Equifax_Secure_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDIDCCAomgAwIBAgIENd70zzANBgkqhkiG9w0BAQUFADBOMQswCQYDVQQG\n" +
      "EwJVUzEQMA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1\n" +
      "cmUgQ2VydGlmaWNhdGUgQXV0aG9yaXR5MB4XDTk4MDgyMjE2NDE1MVoXDTE4\n" +
      "MDgyMjE2NDE1MVowTjELMAkGA1UEBhMCVVMxEDAOBgNVBAoTB0VxdWlmYXgx\n" +
      "LTArBgNVBAsTJEVxdWlmYXggU2VjdXJlIENlcnRpZmljYXRlIEF1dGhvcml0\n" +
      "eTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAwV2xWGcIYu6gmi0fCG2R\n" +
      "FGiYCh7+2gRvE4RiIcPRfM6fBeC4AfBONOziipUEZKzxa1NfBbPLZ4C/QgKO\n" +
      "/t0BCezhABRP/PvwDN1Dulsr4R+AcJkVV5MW8Q+XarfCaCMczE1ZMKxRHjuv\n" +
      "K9buY0V7xdlfUNLjUA86iOe/FP3gx7kCAwEAAaOCAQkwggEFMHAGA1UdHwRp\n" +
      "MGcwZaBjoGGkXzBdMQswCQYDVQQGEwJVUzEQMA4GA1UEChMHRXF1aWZheDEt\n" +
      "MCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2VydGlmaWNhdGUgQXV0aG9yaXR5\n" +
      "MQ0wCwYDVQQDEwRDUkwxMBoGA1UdEAQTMBGBDzIwMTgwODIyMTY0MTUxWjAL\n" +
      "BgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUSOZo+SvSspXXR9gjIBBPM5iQn9Qw\n" +
      "HQYDVR0OBBYEFEjmaPkr0rKV10fYIyAQTzOYkJ/UMAwGA1UdEwQFMAMBAf8w\n" +
      "GgYJKoZIhvZ9B0EABA0wCxsFVjMuMGMDAgbAMA0GCSqGSIb3DQEBBQUAA4GB\n" +
      "AFjOKer89961zgK5F7WF0bnj4JXMJTENAKaSbn+2kmOeUJXRmm/kEd5jhW6Y\n" +
      "7qj/WsjTVbJmcVfewCHrPSqnI0kBBIZCe/zuf6IWUrVnZ9NA2zsmWLIodz2u\n" +
      "FHdh1voqZiegDfqnc1zqcPGUIWVEX/r87yloqaKHee9570+sB3c4\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Equifax_Secure_Global_eBusiness_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICkDCCAfmgAwIBAgIBATANBgkqhkiG9w0BAQQFADBaMQswCQYDVQQGEwJV\n" +
      "UzEcMBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5jLjEtMCsGA1UEAxMkRXF1\n" +
      "aWZheCBTZWN1cmUgR2xvYmFsIGVCdXNpbmVzcyBDQS0xMB4XDTk5MDYyMTA0\n" +
      "MDAwMFoXDTIwMDYyMTA0MDAwMFowWjELMAkGA1UEBhMCVVMxHDAaBgNVBAoT\n" +
      "E0VxdWlmYXggU2VjdXJlIEluYy4xLTArBgNVBAMTJEVxdWlmYXggU2VjdXJl\n" +
      "IEdsb2JhbCBlQnVzaW5lc3MgQ0EtMTCBnzANBgkqhkiG9w0BAQEFAAOBjQAw\n" +
      "gYkCgYEAuucXkAJlsTRVPEnCUdXfp9E3j9HngXNBUmCbnaEXJnitx7HoJpQy\n" +
      "td4zjTov2/KaelpzmKNc6fuKcxtc58O/gGzNqfTWK8D3+ZmqY6KxRwIP1ORR\n" +
      "OhI8bIpaVIRw28HFkM9yRcuoWcDNM50/o5brhTMhHD4ePmBudpxnhcXIw2EC\n" +
      "AwEAAaNmMGQwEQYJYIZIAYb4QgEBBAQDAgAHMA8GA1UdEwEB/wQFMAMBAf8w\n" +
      "HwYDVR0jBBgwFoAUvqigdHJQa0S3ySPY+6j/s1draGwwHQYDVR0OBBYEFL6o\n" +
      "oHRyUGtEt8kj2Puo/7NXa2hsMA0GCSqGSIb3DQEBBAUAA4GBADDiAVGqx+pf\n" +
      "2rnQZQ8w1j7aDRRJbpGTJxQx78T3LUX47Me/okENI7SS+RkAZ70Br83gcfxa\n" +
      "z2TE4JaY0KNA4gGK7ycH8WUBikQtBmV1UsCGECAhX2xrD2yuCRyv8qIYNMR1\n" +
      "pHMc8Y3c7635s3a0kr/clRAevsvIO1qEYBlWlKlV\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Equifax_Secure_eBusiness_CA_1.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICgjCCAeugAwIBAgIBBDANBgkqhkiG9w0BAQQFADBTMQswCQYDVQQGEwJV\n" +
      "UzEcMBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5jLjEmMCQGA1UEAxMdRXF1\n" +
      "aWZheCBTZWN1cmUgZUJ1c2luZXNzIENBLTEwHhcNOTkwNjIxMDQwMDAwWhcN\n" +
      "MjAwNjIxMDQwMDAwWjBTMQswCQYDVQQGEwJVUzEcMBoGA1UEChMTRXF1aWZh\n" +
      "eCBTZWN1cmUgSW5jLjEmMCQGA1UEAxMdRXF1aWZheCBTZWN1cmUgZUJ1c2lu\n" +
      "ZXNzIENBLTEwgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAM4vGbwXt3fe\n" +
      "k6lfWg0XTzQaDJj0ItlZ1MRoRvC0NcWFAyDGr0WlIVFFQesWWDYyb+JQYmT5\n" +
      "/VGcqiTZ9J2DKocKIdMSODRsjQBuWqDZQu4aIZX5UkxVWsUPOE9G+m34LjXW\n" +
      "HXzr4vCwdYDIqROsvojvOm6rXyo4YgKwEnv+j6YDAgMBAAGjZjBkMBEGCWCG\n" +
      "SAGG+EIBAQQEAwIABzAPBgNVHRMBAf8EBTADAQH/MB8GA1UdIwQYMBaAFEp4\n" +
      "MlIR21kWNl7fwRQ2QGpHfEyhMB0GA1UdDgQWBBRKeDJSEdtZFjZe38EUNkBq\n" +
      "R3xMoTANBgkqhkiG9w0BAQQFAAOBgQB1W6ibAxHm6VZMzfmpTMANmvPMZWnm\n" +
      "JXbMWbfWVMMdzZmsGd20hdXgPfxiIKeES1hl8eL5lSE/9dR+WB5Hh1Q+WKG1\n" +
      "tfgq73HnvMP2sUlG4tega+VWeponmHxGYhTnyfxuAxJ5gDgdSIKN/Bf+KpYr\n" +
      "tWKmpj29f5JZzVoqgrI3eQ==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Equifax_Secure_eBusiness_CA_2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDIDCCAomgAwIBAgIEN3DPtTANBgkqhkiG9w0BAQUFADBOMQswCQYDVQQG\n" +
      "EwJVUzEXMBUGA1UEChMORXF1aWZheCBTZWN1cmUxJjAkBgNVBAsTHUVxdWlm\n" +
      "YXggU2VjdXJlIGVCdXNpbmVzcyBDQS0yMB4XDTk5MDYyMzEyMTQ0NVoXDTE5\n" +
      "MDYyMzEyMTQ0NVowTjELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDkVxdWlmYXgg\n" +
      "U2VjdXJlMSYwJAYDVQQLEx1FcXVpZmF4IFNlY3VyZSBlQnVzaW5lc3MgQ0Et\n" +
      "MjCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEA5Dk5kx5SBhsoNviyoynF\n" +
      "7Y6yEb3+6+e0dMKP/wXn2Z0GvxLIPw7y1tEkshHe0XMJitSxLJgJDR5QRrKD\n" +
      "pkWNYmi7hRsgcDKqQM2mll/EcTc/BPO3QSQ5BxoeLmFYoBIL5aXfxavqN3HM\n" +
      "HMg3OrmXUqesxWoklE6ce8/AatbfIb0CAwEAAaOCAQkwggEFMHAGA1UdHwRp\n" +
      "MGcwZaBjoGGkXzBdMQswCQYDVQQGEwJVUzEXMBUGA1UEChMORXF1aWZheCBT\n" +
      "ZWN1cmUxJjAkBgNVBAsTHUVxdWlmYXggU2VjdXJlIGVCdXNpbmVzcyBDQS0y\n" +
      "MQ0wCwYDVQQDEwRDUkwxMBoGA1UdEAQTMBGBDzIwMTkwNjIzMTIxNDQ1WjAL\n" +
      "BgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUUJ4L6q9euSBIplBqy/3YIHqngnYw\n" +
      "HQYDVR0OBBYEFFCeC+qvXrkgSKZQasv92CB6p4J2MAwGA1UdEwQFMAMBAf8w\n" +
      "GgYJKoZIhvZ9B0EABA0wCxsFVjMuMGMDAgbAMA0GCSqGSIb3DQEBBQUAA4GB\n" +
      "AAyGgq3oThr1jokn4jVYPSm0B482UJW/bsGe68SQsoWou7dC4A8HOd/7npCy\n" +
      "0cE+U58DRLB+S/Rv5Hwf5+Kx5Lia78O9zt4LMjTZ3ijtM2vE1Nc9ElirfQkt\n" +
      "y3D1E4qUoSek1nDFbZS1yX2doNLGCEnZZpum0/QL3MUmV+GRMOrN\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // GTE_CyberTrust_Global_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICWjCCAcMCAgGlMA0GCSqGSIb3DQEBBAUAMHUxCzAJBgNVBAYTAlVTMRgw\n" +
      "FgYDVQQKEw9HVEUgQ29ycG9yYXRpb24xJzAlBgNVBAsTHkdURSBDeWJlclRy\n" +
      "dXN0IFNvbHV0aW9ucywgSW5jLjEjMCEGA1UEAxMaR1RFIEN5YmVyVHJ1c3Qg\n" +
      "R2xvYmFsIFJvb3QwHhcNOTgwODEzMDAyOTAwWhcNMTgwODEzMjM1OTAwWjB1\n" +
      "MQswCQYDVQQGEwJVUzEYMBYGA1UEChMPR1RFIENvcnBvcmF0aW9uMScwJQYD\n" +
      "VQQLEx5HVEUgQ3liZXJUcnVzdCBTb2x1dGlvbnMsIEluYy4xIzAhBgNVBAMT\n" +
      "GkdURSBDeWJlclRydXN0IEdsb2JhbCBSb290MIGfMA0GCSqGSIb3DQEBAQUA\n" +
      "A4GNADCBiQKBgQCVD6C28FCc6HrHiM3dFw4usJTQGz0O9pTAipTHBsiQl8i4\n" +
      "ZBp6fmw8U+E3KHNgf7KXUwefU/ltWJTSr41tiGeA5u2ylc9yMcqlHHK6XALn\n" +
      "ZELn+aks1joNrI1CqiQBOeacPwGFVw1Yh0X404Wqk2kmhXBIgD8SFcd5tB8F\n" +
      "LztimQIDAQABMA0GCSqGSIb3DQEBBAUAA4GBAG3rGwnpXtlR22ciYaQqPEh3\n" +
      "46B8pt5zohQDhT37qw4wxYMWM4ETCJ57NE7fQMh017l93PR2VX2bY1QY6fDq\n" +
      "81yx2YtCHrnAlU66+tXifPVoYb+O7AWXX1uw16OFNMQkpw0PlZPvy5TYnh+d\n" +
      "XIVtx6quTx8itc2VrbqnzPmrC3p/\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // GTE_CyberTrust_Root_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIB+jCCAWMCAgGjMA0GCSqGSIb3DQEBBAUAMEUxCzAJBgNVBAYTAlVTMRgw\n" +
      "FgYDVQQKEw9HVEUgQ29ycG9yYXRpb24xHDAaBgNVBAMTE0dURSBDeWJlclRy\n" +
      "dXN0IFJvb3QwHhcNOTYwMjIzMjMwMTAwWhcNMDYwMjIzMjM1OTAwWjBFMQsw\n" +
      "CQYDVQQGEwJVUzEYMBYGA1UEChMPR1RFIENvcnBvcmF0aW9uMRwwGgYDVQQD\n" +
      "ExNHVEUgQ3liZXJUcnVzdCBSb290MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCB\n" +
      "iQKBgQC45k+625h8cXyvRLfTD0bZZOWTwUKOx7pJjTUteueLveUFMVnGsS8K\n" +
      "DPufpz+iCWaEVh43KRuH6X4MypqfpX/1FZSj1aJGgthoTNE3FQZor734sLPw\n" +
      "KfWVWgkWYXcKIiXUT0Wqx73llt/51KiOQswkwB6RJ0q1bQaAYznEol44AwID\n" +
      "AQABMA0GCSqGSIb3DQEBBAUAA4GBABKzdcZfHeFhVYAA1IFLezEPI2PnPfMD\n" +
      "+fQ2qLvZ46WXTeorKeDWanOB5sCJo9Px4KWlIjeaY8JIILTbcuPI9tl8vrGv\n" +
      "U9oUtCG41tWW4/5ODFlitppK+ULdjG+BqXH/9ApybW1EDp3zdHSo1TRJ6V6e\n" +
      "6bR64eVaH4QwnNOfpSXY\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // GeoTrust_Global_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDVDCCAjygAwIBAgIDAjRWMA0GCSqGSIb3DQEBBQUAMEIxCzAJBgNVBAYT\n" +
      "AlVTMRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMRswGQYDVQQDExJHZW9UcnVz\n" +
      "dCBHbG9iYWwgQ0EwHhcNMDIwNTIxMDQwMDAwWhcNMjIwNTIxMDQwMDAwWjBC\n" +
      "MQswCQYDVQQGEwJVUzEWMBQGA1UEChMNR2VvVHJ1c3QgSW5jLjEbMBkGA1UE\n" +
      "AxMSR2VvVHJ1c3QgR2xvYmFsIENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8A\n" +
      "MIIBCgKCAQEA2swYYzD99BcjGlZ+W988bDjkcbd4kdS8odhM+KhDtgPpTSEH\n" +
      "CIjaWC9mOSm9BXiLnTjoBbdqfnGk5sRgprDvgOSJKA+eJdbtg/OtppHHmMlC\n" +
      "GDUUna2YRpIuT8rxh0PBFpVXLVDviS2Aelet8u5fa9IAjbkU+BQVNdnARqN7\n" +
      "csiRv8lVK83Qlz6cJmTM386DGXHKTubU1XupGc1V3sjs0l44U+VcT4wt/lAj\n" +
      "Nvxm5suOpDkZALeVAjmRCw7+OC7RHQWa9k0+bw8HHa8sHo9gOeL6NlMTOdRe\n" +
      "JivbPagUvTLrGAMoUgRx5aszPeE4uwc2hGKceeoWMPRfwCvocWvk+QIDAQAB\n" +
      "o1MwUTAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBTAephojYn7qwVkDBF9\n" +
      "qn1luMrMTjAfBgNVHSMEGDAWgBTAephojYn7qwVkDBF9qn1luMrMTjANBgkq\n" +
      "hkiG9w0BAQUFAAOCAQEANeMpauUvXVSOKVCUn5kaFOSPeCpilKInZ57Qzxpe\n" +
      "R+nBsqTP3UEaBU6bS+5Kb1VSsyShNwrrZHYqLizz/Tt1kL/6cdjHPTfStQWV\n" +
      "Yrmm3ok9Nns4d0iXrKYgjy6myQzCsplFAMfOEVEiIuCl6rYVSAlk6l5PdPcF\n" +
      "PseKUgzbFbS9bZvlxrFUaKnjaZC2mqUPuLk/IH2uSrW4nOQdtqvmlKXBx4Ot\n" +
      "2/Unhw4EbNX/3aBd7YdStysVAq45pmp06drE57xNNB6pXE0zX5IJL4hmXXeX\n" +
      "xx12E6nV5fEWCRE11azbJHFwLJhWC9kXtNHjUStedejV0NxPNO3CBWaAocvm\n" +
      "Mw==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // GlobalSign_Root_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDdTCCAl2gAwIBAgILAgAAAAAA1ni3lAUwDQYJKoZIhvcNAQEEBQAwVzEL\n" +
      "MAkGA1UEBhMCQkUxGTAXBgNVBAoTEEdsb2JhbFNpZ24gbnYtc2ExEDAOBgNV\n" +
      "BAsTB1Jvb3QgQ0ExGzAZBgNVBAMTEkdsb2JhbFNpZ24gUm9vdCBDQTAeFw05\n" +
      "ODA5MDExMjAwMDBaFw0xNDAxMjgxMjAwMDBaMFcxCzAJBgNVBAYTAkJFMRkw\n" +
      "FwYDVQQKExBHbG9iYWxTaWduIG52LXNhMRAwDgYDVQQLEwdSb290IENBMRsw\n" +
      "GQYDVQQDExJHbG9iYWxTaWduIFJvb3QgQ0EwggEiMA0GCSqGSIb3DQEBAQUA\n" +
      "A4IBDwAwggEKAoIBAQDaDuaZjc6j40+Kfvvxi4Mla+pIH/EqsLmVEQS98GPR\n" +
      "4mdmzxzdzxtIK+6NiY6arymAZavpxy0Sy6scTHAHoT0KMM0VjU/43dSMUBUc\n" +
      "71DuxC73/OlS8pF94G3VNTCOXkNz8kHp1Wrjsok6Vjk4bwY8iGlbKk3Fp1S4\n" +
      "bInMm/k8yuX9ifUSPJJ4ltbcdG6TRGHRjcdGsnUOhugZitVtbNV4FpWi6cgK\n" +
      "OOvyJBNPc1STE4U6G7weNLWLBYy5d4ux2x8gkasJU26Qzns3dLlwR5EiUWMW\n" +
      "ea6xrkEmCMgZK9FGqkjWZCrXgzT/LCrBbBlDSgeF59N89iFo7+ryUp9/k5DP\n" +
      "AgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIABjAdBgNVHQ4EFgQUYHtmGkUNl8qJ\n" +
      "UC99BM00qP/8/UswDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQQFAAOC\n" +
      "AQEArqqf/LfSyx9fOSkoGJ40yWxPbxrwZKJwSk8ThptgKJ7ogUmYfQq75bCd\n" +
      "PTbbjwVR/wkxKh/diXeeDy5slQTthsu0AD+EAk2AaioteAuubyuig0SDH81Q\n" +
      "gkwkr733pbTIWg/050deSY43lv6aiAU62cDbKYfmGZZHpzqmjIs8d/5GY6dT\n" +
      "2iHRrH5Jokvmw2dZL7OKDrssvamqQnw1wdh/1acxOk5jQzmvCLBhNIzTmKlD\n" +
      "NPYPhyk7ncJWWJh3w/cbrPad+D6qp1RF8PX51TFl/mtYnHGzHtdS6jIX/EBg\n" +
      "Hcl5JLL2bP2oZg6C3ZjL2sJETy6ge/L3ayx2EYRGinij4w==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // RSA_Root_Certificate_1.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIC5zCCAlACAQEwDQYJKoZIhvcNAQEFBQAwgbsxJDAiBgNVBAcTG1ZhbGlD\n" +
      "ZXJ0IFZhbGlkYXRpb24gTmV0d29yazEXMBUGA1UEChMOVmFsaUNlcnQsIElu\n" +
      "Yy4xNTAzBgNVBAsTLFZhbGlDZXJ0IENsYXNzIDMgUG9saWN5IFZhbGlkYXRp\n" +
      "b24gQXV0aG9yaXR5MSEwHwYDVQQDExhodHRwOi8vd3d3LnZhbGljZXJ0LmNv\n" +
      "bS8xIDAeBgkqhkiG9w0BCQEWEWluZm9AdmFsaWNlcnQuY29tMB4XDTk5MDYy\n" +
      "NjAwMjIzM1oXDTE5MDYyNjAwMjIzM1owgbsxJDAiBgNVBAcTG1ZhbGlDZXJ0\n" +
      "IFZhbGlkYXRpb24gTmV0d29yazEXMBUGA1UEChMOVmFsaUNlcnQsIEluYy4x\n" +
      "NTAzBgNVBAsTLFZhbGlDZXJ0IENsYXNzIDMgUG9saWN5IFZhbGlkYXRpb24g\n" +
      "QXV0aG9yaXR5MSEwHwYDVQQDExhodHRwOi8vd3d3LnZhbGljZXJ0LmNvbS8x\n" +
      "IDAeBgkqhkiG9w0BCQEWEWluZm9AdmFsaWNlcnQuY29tMIGfMA0GCSqGSIb3\n" +
      "DQEBAQUAA4GNADCBiQKBgQDjmFGWHOjVsQaBalfDcnWTq8+epvzzFlLWLU2f\n" +
      "NUSoLgRNB0mKOCn1dzfnt6td3zZxFJmP3MKS8edgkpfs2Ejcv8ECIMYkpChM\n" +
      "MFp2bbFc893enhBxoYjHW5tBbcqwuI4V7q0zK89HBFx1cQqYJJgpp0lZpd34\n" +
      "t0NiYfPT4tBVPwIDAQABMA0GCSqGSIb3DQEBBQUAA4GBAFa7AliEZwgs3x/b\n" +
      "e0kz9dNnnfS0ChCzycUs4pJqcXgn8nCDQtM+z6lU9PHYkhaM0QTLS6vJn0Wu\n" +
      "PIqpsHEzXcjFV9+vqDWzf4mH6eglkrh/hXqu1rweN1gqZ8mRzyqBPu3GOd/A\n" +
      "PhmcGcwTTYJBtYze4D1gCCAPRX5ron+jjBXu\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // RSA_Security_1024_v3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICXDCCAcWgAwIBAgIQCgEBAQAAAnwAAAALAAAAAjANBgkqhkiG9w0BAQUF\n" +
      "ADA6MRkwFwYDVQQKExBSU0EgU2VjdXJpdHkgSW5jMR0wGwYDVQQLExRSU0Eg\n" +
      "U2VjdXJpdHkgMTAyNCBWMzAeFw0wMTAyMjIyMTAxNDlaFw0yNjAyMjIyMDAx\n" +
      "NDlaMDoxGTAXBgNVBAoTEFJTQSBTZWN1cml0eSBJbmMxHTAbBgNVBAsTFFJT\n" +
      "QSBTZWN1cml0eSAxMDI0IFYzMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKB\n" +
      "gQDV3f5mCc8kPD6ugU5OisRpgFtZO9+5TUzKtS3DJy08rwBCbbwoppbPf9dY\n" +
      "rIMKo1W1exeQFYRMiu4mmdxY78c4pqqv0I5CyGLXq6yp+0p9v+r+Ek3d/yYt\n" +
      "bzZUaMjShFbuklNhCbM/OZuoyZu9zp9+1BlqFikYvtc6adwlWzMaUQIDAQAB\n" +
      "o2MwYTAPBgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwIBBjAfBgNVHSME\n" +
      "GDAWgBTEwBykB5T9zU0B1FTapQxf3q4FWjAdBgNVHQ4EFgQUxMAcpAeU/c1N\n" +
      "AdRU2qUMX96uBVowDQYJKoZIhvcNAQEFBQADgYEAPy1q4yZDlX2Jl2X7deRy\n" +
      "HUZXxGFraZ8SmyzVWujAovBDleMf6XbN3Ou8k6BlCsdNT1+nr6JGFLkM88y9\n" +
      "am63nd4lQtBU/55oc2PcJOsiv6hy8l4A4Q1OOkNumU4/iXgDmMrzVcydro7B\n" +
      "qkWY+o8aoI2II/EVQQ2lRj6RP4vr93E=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // RSA_Security_2048_v3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDYTCCAkmgAwIBAgIQCgEBAQAAAnwAAAAKAAAAAjANBgkqhkiG9w0BAQUF\n" +
      "ADA6MRkwFwYDVQQKExBSU0EgU2VjdXJpdHkgSW5jMR0wGwYDVQQLExRSU0Eg\n" +
      "U2VjdXJpdHkgMjA0OCBWMzAeFw0wMTAyMjIyMDM5MjNaFw0yNjAyMjIyMDM5\n" +
      "MjNaMDoxGTAXBgNVBAoTEFJTQSBTZWN1cml0eSBJbmMxHTAbBgNVBAsTFFJT\n" +
      "QSBTZWN1cml0eSAyMDQ4IFYzMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB\n" +
      "CgKCAQEAt49VcdKA3XtpeafwGFAyPGJn9gqVB93mG/Oe2dJBVGutn3y+Gc37\n" +
      "RqtBaB4Y6lXIL5F4iSj7Jylg/9+PjDvJSZu1pJTOAeo+tWN7fyb9Gd3AIb2E\n" +
      "0S1PRsNO3Ng3OTsor8udGuorryGlwSMiuLgbWhOHV4PR8CDn6E8jQrAApX2J\n" +
      "6elhc5SYcSa8LWrg903w8bYqODGBDSnhAMFRD0xS+ARaqn1y07iHKrtjEAMq\n" +
      "s6FPDVpeRrc9DvV07Jmf+T0kgYim3WBU6JU2PcYJk5qjEoAAVZkZR73QpXzD\n" +
      "uvsf9/UP+Ky5tfQ3mBMY3oVbtwyCO4dvlTlYMNpuAWgXIszACwIDAQABo2Mw\n" +
      "YTAPBgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwIBBjAfBgNVHSMEGDAW\n" +
      "gBQHw1EwpKrpRa41JPr/JCwz0LGdjDAdBgNVHQ4EFgQUB8NRMKSq6UWuNST6\n" +
      "/yQsM9CxnYwwDQYJKoZIhvcNAQEFBQADggEBAF8+hnZuuDU8TjYcHnmYv/3V\n" +
      "EhF5Ug7uMYm83X/50cYVIeiKAVQNOvtUudZj1LGqlk2iQk3UUx+LEN5/Zb5g\n" +
      "EydxiKRz44Rj0aRV4VCT5hsOedBnvEbIvz8XDZXmxpBp3ue0L96VfdASPz0+\n" +
      "f00/FGj1EVDVwfSQpQgdMWD/YIwjVAqv/qFuxdF6Kmh4zx6CCiC0H63lhbJq\n" +
      "aHVOrSU3lIW+vaHU6rcMSzyd6BIA8F+sDeGscGNz9395nzIlQnQFgCi/vcEk\n" +
      "llgVsRch6YlL2weIZ/QVrXA+L02FO8K32/6YaCOJ4XQP3vTFhGMpG8zLB8kA\n" +
      "pKnXwiJPZ9d37CAFYd4=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // TC_TrustCenter__Germany__Class_2_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDXDCCAsWgAwIBAgICA+owDQYJKoZIhvcNAQEEBQAwgbwxCzAJBgNVBAYT\n" +
      "AkRFMRAwDgYDVQQIEwdIYW1idXJnMRAwDgYDVQQHEwdIYW1idXJnMTowOAYD\n" +
      "VQQKEzFUQyBUcnVzdENlbnRlciBmb3IgU2VjdXJpdHkgaW4gRGF0YSBOZXR3\n" +
      "b3JrcyBHbWJIMSIwIAYDVQQLExlUQyBUcnVzdENlbnRlciBDbGFzcyAyIENB\n" +
      "MSkwJwYJKoZIhvcNAQkBFhpjZXJ0aWZpY2F0ZUB0cnVzdGNlbnRlci5kZTAe\n" +
      "Fw05ODAzMDkxMTU5NTlaFw0xMTAxMDExMTU5NTlaMIG8MQswCQYDVQQGEwJE\n" +
      "RTEQMA4GA1UECBMHSGFtYnVyZzEQMA4GA1UEBxMHSGFtYnVyZzE6MDgGA1UE\n" +
      "ChMxVEMgVHJ1c3RDZW50ZXIgZm9yIFNlY3VyaXR5IGluIERhdGEgTmV0d29y\n" +
      "a3MgR21iSDEiMCAGA1UECxMZVEMgVHJ1c3RDZW50ZXIgQ2xhc3MgMiBDQTEp\n" +
      "MCcGCSqGSIb3DQEJARYaY2VydGlmaWNhdGVAdHJ1c3RjZW50ZXIuZGUwgZ8w\n" +
      "DQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBANo46O0yAClxgwENv4wB3NrGrTmk\n" +
      "qYov1YtcaF9QxmL1Zr3KkSLsqh1R1z2zUbKDTl3LSbDwTFXlay3HhQswHJJO\n" +
      "gtTKAu33b77c4OMUuAVT8pr0VotanoWT0bSCVq5Nu6hLVxa8/vhYnvgpjbB7\n" +
      "zXjJT6yLZwzxnPv8V5tXXE8NAgMBAAGjazBpMA8GA1UdEwEB/wQFMAMBAf8w\n" +
      "DgYDVR0PAQH/BAQDAgGGMDMGCWCGSAGG+EIBCAQmFiRodHRwOi8vd3d3LnRy\n" +
      "dXN0Y2VudGVyLmRlL2d1aWRlbGluZXMwEQYJYIZIAYb4QgEBBAQDAgAHMA0G\n" +
      "CSqGSIb3DQEBBAUAA4GBAIRS+yjf/x91AbwBvgRWl2p0QiQxg/lGsQaKic+W\n" +
      "LDO/jLVfenKhhQbOhvgFjuj5Jcrag4wGrOs2bYWRNAQ29ELw+HkuCkhcq8xR\n" +
      "T3h2oNmsGb0q0WkEKJHKNhAngFdb0lz1wlurZIFjdFH0l7/NEij3TWZ/p/Ac\n" +
      "ASZ4smZHcFFk\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // TC_TrustCenter__Germany__Class_3_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDXDCCAsWgAwIBAgICA+swDQYJKoZIhvcNAQEEBQAwgbwxCzAJBgNVBAYT\n" +
      "AkRFMRAwDgYDVQQIEwdIYW1idXJnMRAwDgYDVQQHEwdIYW1idXJnMTowOAYD\n" +
      "VQQKEzFUQyBUcnVzdENlbnRlciBmb3IgU2VjdXJpdHkgaW4gRGF0YSBOZXR3\n" +
      "b3JrcyBHbWJIMSIwIAYDVQQLExlUQyBUcnVzdENlbnRlciBDbGFzcyAzIENB\n" +
      "MSkwJwYJKoZIhvcNAQkBFhpjZXJ0aWZpY2F0ZUB0cnVzdGNlbnRlci5kZTAe\n" +
      "Fw05ODAzMDkxMTU5NTlaFw0xMTAxMDExMTU5NTlaMIG8MQswCQYDVQQGEwJE\n" +
      "RTEQMA4GA1UECBMHSGFtYnVyZzEQMA4GA1UEBxMHSGFtYnVyZzE6MDgGA1UE\n" +
      "ChMxVEMgVHJ1c3RDZW50ZXIgZm9yIFNlY3VyaXR5IGluIERhdGEgTmV0d29y\n" +
      "a3MgR21iSDEiMCAGA1UECxMZVEMgVHJ1c3RDZW50ZXIgQ2xhc3MgMyBDQTEp\n" +
      "MCcGCSqGSIb3DQEJARYaY2VydGlmaWNhdGVAdHJ1c3RjZW50ZXIuZGUwgZ8w\n" +
      "DQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBALa0wTUFLg2N7KBAahwOJ6ZQkmtQ\n" +
      "GwfeLud2zODa/ISoXoxjaitN2U4CdhHBC/KNecoAtvGwDtf7pBc9r6tpepYn\n" +
      "v68zoZoqWarEtTcI8hKlMbZD9TKWcSgoq40oht+77uMMfTDWw1Krj10nnGvA\n" +
      "o+cFa1dJRLNu6mTP0o56UHd3AgMBAAGjazBpMA8GA1UdEwEB/wQFMAMBAf8w\n" +
      "DgYDVR0PAQH/BAQDAgGGMDMGCWCGSAGG+EIBCAQmFiRodHRwOi8vd3d3LnRy\n" +
      "dXN0Y2VudGVyLmRlL2d1aWRlbGluZXMwEQYJYIZIAYb4QgEBBAQDAgAHMA0G\n" +
      "CSqGSIb3DQEBBAUAA4GBABY9xs3Bu4VxhUafPiCPUSiZ7C1FIWMjWwS7TJC4\n" +
      "iJIETb19AaM/9uzO8d7+feXhPrvGq14L3T2WxMup1Pkm5gZOngylerpuw3yC\n" +
      "GdHHsbHD2w2Om0B8NwvxXej9H5CIpQ5ON2QhqE6NtJ/x3kit1VYYUimLRzQS\n" +
      "CdS7kjXvD9s0\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Thawte_Personal_Basic_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDITCCAoqgAwIBAgIBADANBgkqhkiG9w0BAQQFADCByzELMAkGA1UEBhMC\n" +
      "WkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTESMBAGA1UEBxMJQ2FwZSBUb3du\n" +
      "MRowGAYDVQQKExFUaGF3dGUgQ29uc3VsdGluZzEoMCYGA1UECxMfQ2VydGlm\n" +
      "aWNhdGlvbiBTZXJ2aWNlcyBEaXZpc2lvbjEhMB8GA1UEAxMYVGhhd3RlIFBl\n" +
      "cnNvbmFsIEJhc2ljIENBMSgwJgYJKoZIhvcNAQkBFhlwZXJzb25hbC1iYXNp\n" +
      "Y0B0aGF3dGUuY29tMB4XDTk2MDEwMTAwMDAwMFoXDTIwMTIzMTIzNTk1OVow\n" +
      "gcsxCzAJBgNVBAYTAlpBMRUwEwYDVQQIEwxXZXN0ZXJuIENhcGUxEjAQBgNV\n" +
      "BAcTCUNhcGUgVG93bjEaMBgGA1UEChMRVGhhd3RlIENvbnN1bHRpbmcxKDAm\n" +
      "BgNVBAsTH0NlcnRpZmljYXRpb24gU2VydmljZXMgRGl2aXNpb24xITAfBgNV\n" +
      "BAMTGFRoYXd0ZSBQZXJzb25hbCBCYXNpYyBDQTEoMCYGCSqGSIb3DQEJARYZ\n" +
      "cGVyc29uYWwtYmFzaWNAdGhhd3RlLmNvbTCBnzANBgkqhkiG9w0BAQEFAAOB\n" +
      "jQAwgYkCgYEAvLyTU23AUE+CFeZIlDWmWr5vQvoPR+53dXLdjUmbllegeNTK\n" +
      "P1GzaQuRdhciB5dqxFGTS+CN7zeVoQxN2jSQHReJl+A1OFdKwPQIcOk8RHtQ\n" +
      "fmGakOMj04gRRif1CwcOu93RfyAKiLlWCy4cgNrx454p7xS9CkT7G1sY0b8j\n" +
      "kyECAwEAAaMTMBEwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQQFAAOB\n" +
      "gQAt4plrsD16iddZopQBHyvdEktTwq1/qqcAXJFAVyVKOKqEcLnZgA+le1z7\n" +
      "c8a914phXAPjLSeoF+CEhULcXpvGt7Jtu3Sv5D/Lp7ew4F2+eIMllNLbgQ95\n" +
      "B21P9DkVWlIBe94y1k049hJcBlDfBVu9FEuh3ym6O0GN92NWod8isQ==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Thawte_Personal_Freemail_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDLTCCApagAwIBAgIBADANBgkqhkiG9w0BAQQFADCB0TELMAkGA1UEBhMC\n" +
      "WkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTESMBAGA1UEBxMJQ2FwZSBUb3du\n" +
      "MRowGAYDVQQKExFUaGF3dGUgQ29uc3VsdGluZzEoMCYGA1UECxMfQ2VydGlm\n" +
      "aWNhdGlvbiBTZXJ2aWNlcyBEaXZpc2lvbjEkMCIGA1UEAxMbVGhhd3RlIFBl\n" +
      "cnNvbmFsIEZyZWVtYWlsIENBMSswKQYJKoZIhvcNAQkBFhxwZXJzb25hbC1m\n" +
      "cmVlbWFpbEB0aGF3dGUuY29tMB4XDTk2MDEwMTAwMDAwMFoXDTIwMTIzMTIz\n" +
      "NTk1OVowgdExCzAJBgNVBAYTAlpBMRUwEwYDVQQIEwxXZXN0ZXJuIENhcGUx\n" +
      "EjAQBgNVBAcTCUNhcGUgVG93bjEaMBgGA1UEChMRVGhhd3RlIENvbnN1bHRp\n" +
      "bmcxKDAmBgNVBAsTH0NlcnRpZmljYXRpb24gU2VydmljZXMgRGl2aXNpb24x\n" +
      "JDAiBgNVBAMTG1RoYXd0ZSBQZXJzb25hbCBGcmVlbWFpbCBDQTErMCkGCSqG\n" +
      "SIb3DQEJARYccGVyc29uYWwtZnJlZW1haWxAdGhhd3RlLmNvbTCBnzANBgkq\n" +
      "hkiG9w0BAQEFAAOBjQAwgYkCgYEA1GnX1LCUZFtx6UfYDFG26nKRsIRefS0N\n" +
      "j3sS34UldSh0OkIsYyeflXtL734Zhx2G6qPduc6WZBrCFG5ErHzmj+hND3Ef\n" +
      "QDimAKOHePb5lIZererAXnbr2RSjXW56fAylS1V/Bhkpf56aJtVquzgkCGqY\n" +
      "x7Hao5iR/Xnb5VrEHLkCAwEAAaMTMBEwDwYDVR0TAQH/BAUwAwEB/zANBgkq\n" +
      "hkiG9w0BAQQFAAOBgQDH7JJ+Tvj1lqVnYiqk8E0RYNBvjWBYYawmu1I1XAjP\n" +
      "MPuoSpaKH2JCI4wXD/S6ZJwXrEcp352YXtJsYHFcoqzceePnbgBHH7UNKOgC\n" +
      "neSa/RP0ptl8sfjcXyMmCZGAc9AUG95DqYMl8uacLxXK/qarigd1iwzdUYRr\n" +
      "5PjRzneigQ==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Thawte_Personal_Premium_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDKTCCApKgAwIBAgIBADANBgkqhkiG9w0BAQQFADCBzzELMAkGA1UEBhMC\n" +
      "WkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTESMBAGA1UEBxMJQ2FwZSBUb3du\n" +
      "MRowGAYDVQQKExFUaGF3dGUgQ29uc3VsdGluZzEoMCYGA1UECxMfQ2VydGlm\n" +
      "aWNhdGlvbiBTZXJ2aWNlcyBEaXZpc2lvbjEjMCEGA1UEAxMaVGhhd3RlIFBl\n" +
      "cnNvbmFsIFByZW1pdW0gQ0ExKjAoBgkqhkiG9w0BCQEWG3BlcnNvbmFsLXBy\n" +
      "ZW1pdW1AdGhhd3RlLmNvbTAeFw05NjAxMDEwMDAwMDBaFw0yMDEyMzEyMzU5\n" +
      "NTlaMIHPMQswCQYDVQQGEwJaQTEVMBMGA1UECBMMV2VzdGVybiBDYXBlMRIw\n" +
      "EAYDVQQHEwlDYXBlIFRvd24xGjAYBgNVBAoTEVRoYXd0ZSBDb25zdWx0aW5n\n" +
      "MSgwJgYDVQQLEx9DZXJ0aWZpY2F0aW9uIFNlcnZpY2VzIERpdmlzaW9uMSMw\n" +
      "IQYDVQQDExpUaGF3dGUgUGVyc29uYWwgUHJlbWl1bSBDQTEqMCgGCSqGSIb3\n" +
      "DQEJARYbcGVyc29uYWwtcHJlbWl1bUB0aGF3dGUuY29tMIGfMA0GCSqGSIb3\n" +
      "DQEBAQUAA4GNADCBiQKBgQDJZtn4B0TPuYwu8KHvE0VsBd/eJxZRNkERbGw7\n" +
      "7f4QfRKe5ZtCmv5gMcNmt3M6SK5O0DI3lIi1DbbZ8/JE2dWIEt12TfIa/G8j\n" +
      "Hnrx2JhFTgcQ7xZC0EN1bUre4qrJMf8fAHB8Zs8QJQi6+u4A6UYDZicRFTuq\n" +
      "W/KY3TZCstqIdQIDAQABoxMwETAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3\n" +
      "DQEBBAUAA4GBAGk2ifc0KjNyL2071CKyuG+axTZmDhs8obF1Wub9NdP4qPIH\n" +
      "b4Vnjt4rueIXsDqg8A6iAJrf8xQVbrvIhVqYgPn/vnQdPfP+MCXRNzRn+qVx\n" +
      "eTBhKXLA4CxM+1bkOqhv5TJZUtt1KFBZDPgLGeSs2a+WjS9Q2wfD6h+rM+D1\n" +
      "KzGJ\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Thawte_Premium_Server_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDJzCCApCgAwIBAgIBATANBgkqhkiG9w0BAQQFADCBzjELMAkGA1UEBhMC\n" +
      "WkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTESMBAGA1UEBxMJQ2FwZSBUb3du\n" +
      "MR0wGwYDVQQKExRUaGF3dGUgQ29uc3VsdGluZyBjYzEoMCYGA1UECxMfQ2Vy\n" +
      "dGlmaWNhdGlvbiBTZXJ2aWNlcyBEaXZpc2lvbjEhMB8GA1UEAxMYVGhhd3Rl\n" +
      "IFByZW1pdW0gU2VydmVyIENBMSgwJgYJKoZIhvcNAQkBFhlwcmVtaXVtLXNl\n" +
      "cnZlckB0aGF3dGUuY29tMB4XDTk2MDgwMTAwMDAwMFoXDTIwMTIzMTIzNTk1\n" +
      "OVowgc4xCzAJBgNVBAYTAlpBMRUwEwYDVQQIEwxXZXN0ZXJuIENhcGUxEjAQ\n" +
      "BgNVBAcTCUNhcGUgVG93bjEdMBsGA1UEChMUVGhhd3RlIENvbnN1bHRpbmcg\n" +
      "Y2MxKDAmBgNVBAsTH0NlcnRpZmljYXRpb24gU2VydmljZXMgRGl2aXNpb24x\n" +
      "ITAfBgNVBAMTGFRoYXd0ZSBQcmVtaXVtIFNlcnZlciBDQTEoMCYGCSqGSIb3\n" +
      "DQEJARYZcHJlbWl1bS1zZXJ2ZXJAdGhhd3RlLmNvbTCBnzANBgkqhkiG9w0B\n" +
      "AQEFAAOBjQAwgYkCgYEA0jY2aovXwlue2oFBYo847kkEVdbQ7xwblRZH7xhI\n" +
      "NTpS9CtqBo87L+pW46+GjZ4X9560ZXUCTe/LCaIhUdib0GfQug2SBhRz1JPL\n" +
      "lyoAnFxODLz6FVL88kRu2hFKbgifLy3j+ao6hnO2RlNYyIkFvYMRuHM/qgeN\n" +
      "9EJN50CdHDcCAwEAAaMTMBEwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0B\n" +
      "AQQFAAOBgQAmSCwWwlj66BZ0DKqqX1Q/8tfJeGBeXm43YyJ3Nn6yF8Q0ufUI\n" +
      "hfzJATj/Tb7yFkJD57taRvvBxhEf8UqwKEbJw8RCfbz6q1lu1bdRiBHjpIUZ\n" +
      "a4JMpAwSremkrj/xw0llmozFyD4lt5SZu5IycQfwhl7tUCemDaYj+bvLpgcU\n" +
      "Qg==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Thawte_Server_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDEzCCAnygAwIBAgIBATANBgkqhkiG9w0BAQQFADCBxDELMAkGA1UEBhMC\n" +
      "WkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTESMBAGA1UEBxMJQ2FwZSBUb3du\n" +
      "MR0wGwYDVQQKExRUaGF3dGUgQ29uc3VsdGluZyBjYzEoMCYGA1UECxMfQ2Vy\n" +
      "dGlmaWNhdGlvbiBTZXJ2aWNlcyBEaXZpc2lvbjEZMBcGA1UEAxMQVGhhd3Rl\n" +
      "IFNlcnZlciBDQTEmMCQGCSqGSIb3DQEJARYXc2VydmVyLWNlcnRzQHRoYXd0\n" +
      "ZS5jb20wHhcNOTYwODAxMDAwMDAwWhcNMjAxMjMxMjM1OTU5WjCBxDELMAkG\n" +
      "A1UEBhMCWkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTESMBAGA1UEBxMJQ2Fw\n" +
      "ZSBUb3duMR0wGwYDVQQKExRUaGF3dGUgQ29uc3VsdGluZyBjYzEoMCYGA1UE\n" +
      "CxMfQ2VydGlmaWNhdGlvbiBTZXJ2aWNlcyBEaXZpc2lvbjEZMBcGA1UEAxMQ\n" +
      "VGhhd3RlIFNlcnZlciBDQTEmMCQGCSqGSIb3DQEJARYXc2VydmVyLWNlcnRz\n" +
      "QHRoYXd0ZS5jb20wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBANOkUG7I\n" +
      "/1Zr5s9dtuoMaHVHoqrC2oQl/Kj0R1HahbUgdJSGHg91yekIYfUGbTBuFRkC\n" +
      "6VLAYttNmZ7iagxEOM3+vuNkCXDF/rFrKbYvScg71CcEJRCXL+eQbcAoQpnX\n" +
      "TEPew/UhbVSfXcNY4cDk2VuwuNy0e982OsK1ZiIS1ocNAgMBAAGjEzARMA8G\n" +
      "A1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQEEBQADgYEAB/pMaVz7lcxG7oWD\n" +
      "TSEwjsrZqG9JGubaUeNgcGyEYRGhGshIPllDfU+VPaGLtwtimHp1it2ITk6e\n" +
      "QNuozDJ0uW8NxuOzRAvZim+aKZuZGCg70eNAKJpaPNW15yAbi8qkq43pUdni\n" +
      "TCxZqdq5snUb9kLy78fyGPmJvKP/iiMucEc=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Thawte_Time_Stamping_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICoTCCAgqgAwIBAgIBADANBgkqhkiG9w0BAQQFADCBizELMAkGA1UEBhMC\n" +
      "WkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTEUMBIGA1UEBxMLRHVyYmFudmls\n" +
      "bGUxDzANBgNVBAoTBlRoYXd0ZTEdMBsGA1UECxMUVGhhd3RlIENlcnRpZmlj\n" +
      "YXRpb24xHzAdBgNVBAMTFlRoYXd0ZSBUaW1lc3RhbXBpbmcgQ0EwHhcNOTcw\n" +
      "MTAxMDAwMDAwWhcNMjAxMjMxMjM1OTU5WjCBizELMAkGA1UEBhMCWkExFTAT\n" +
      "BgNVBAgTDFdlc3Rlcm4gQ2FwZTEUMBIGA1UEBxMLRHVyYmFudmlsbGUxDzAN\n" +
      "BgNVBAoTBlRoYXd0ZTEdMBsGA1UECxMUVGhhd3RlIENlcnRpZmljYXRpb24x\n" +
      "HzAdBgNVBAMTFlRoYXd0ZSBUaW1lc3RhbXBpbmcgQ0EwgZ8wDQYJKoZIhvcN\n" +
      "AQEBBQADgY0AMIGJAoGBANYrWHhhRYZT6jR7UZztsOYuGA7+4F+oJ9O0yeB8\n" +
      "WU4WDnNUYMF/9p8u6TqFJBU820cEY8OexJQaWt9MevPZQx08EHp5JduQ/vBR\n" +
      "5zDWQQD9nyjfeb6Uu522FOMjhdepQeBMpHmwKxqL8vg7ij5FrHGSALSQQZj7\n" +
      "X+36ty6K+Ig3AgMBAAGjEzARMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcN\n" +
      "AQEEBQADgYEAZ9viwuaHPUCDhjc1fR/OmsMMZiCouqoEiYbC9RAIDb/LogWK\n" +
      "0E02PvTX72nGXuSwlG9KuefeW4i2e9vjJ+V2w/A1wcu1J5szedyQpgCed/r8\n" +
      "zSeUQhac0xxo7L9c3eWpexAKMnRUEzGLhQOEkbdYATAUOK8oyvyxUBkZCayJ\n" +
      "SdM=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // UTN-USER_First-Network_Applications.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEZDCCA0ygAwIBAgIQRL4Mi1AAJLQR0zYwS8AzdzANBgkqhkiG9w0BAQUF\n" +
      "ADCBozELMAkGA1UEBhMCVVMxCzAJBgNVBAgTAlVUMRcwFQYDVQQHEw5TYWx0\n" +
      "IExha2UgQ2l0eTEeMBwGA1UEChMVVGhlIFVTRVJUUlVTVCBOZXR3b3JrMSEw\n" +
      "HwYDVQQLExhodHRwOi8vd3d3LnVzZXJ0cnVzdC5jb20xKzApBgNVBAMTIlVU\n" +
      "Ti1VU0VSRmlyc3QtTmV0d29yayBBcHBsaWNhdGlvbnMwHhcNOTkwNzA5MTg0\n" +
      "ODM5WhcNMTkwNzA5MTg1NzQ5WjCBozELMAkGA1UEBhMCVVMxCzAJBgNVBAgT\n" +
      "AlVUMRcwFQYDVQQHEw5TYWx0IExha2UgQ2l0eTEeMBwGA1UEChMVVGhlIFVT\n" +
      "RVJUUlVTVCBOZXR3b3JrMSEwHwYDVQQLExhodHRwOi8vd3d3LnVzZXJ0cnVz\n" +
      "dC5jb20xKzApBgNVBAMTIlVUTi1VU0VSRmlyc3QtTmV0d29yayBBcHBsaWNh\n" +
      "dGlvbnMwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCz+5Gh5DZV\n" +
      "hawGNFugmliy+LUPBXeDrjKxdpJo7CNKyXY/45y2N3kDuatpjQclthln5LAb\n" +
      "GHNhSuh+zdMvZOOmfAz6F4CjDUeJT1FxL+78P/m4FoCHiZMlIJpDgmkkdihZ\n" +
      "NaEdwH+DBmQWICzTSaSFtMBhf1EI+GgVkYDLpdXuOzr0hAReYFmnjDRy7rh4\n" +
      "xdE7EkpvfmUnuaRVxblvQ6TFHSyZwFKkeEwVs0CYCGtDxgGwenv1axwiP8vv\n" +
      "/6jQOkt2FZ7S0cYu49tXGzKiuG/ohqY/cKvlcJKrRB5AUPuco2LkbG6gyN7i\n" +
      "gEL66S/ozjIEj3yNtxyjNTwV3Z7DrpelAgMBAAGjgZEwgY4wCwYDVR0PBAQD\n" +
      "AgHGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFPqGydvguul49Uuo1hXf\n" +
      "8NPhahQ8ME8GA1UdHwRIMEYwRKBCoECGPmh0dHA6Ly9jcmwudXNlcnRydXN0\n" +
      "LmNvbS9VVE4tVVNFUkZpcnN0LU5ldHdvcmtBcHBsaWNhdGlvbnMuY3JsMA0G\n" +
      "CSqGSIb3DQEBBQUAA4IBAQCk8yXM0dSRgyLQzDKrm5ZONJFUICU0YV8qAhXh\n" +
      "i6r/fWRRzwr/vH3YIWp4yy9Rb/hCHTO967V7lMPDqaAt39EpHx3+jz+7qEUq\n" +
      "f9FuVSTiuwL7MT++6LzsQCv4AdRWOOTKRIK1YSAhZ2X28AvnNPilwpyjXEAf\n" +
      "hZOVBt5P1CeptqX8Fs1zMT+4ZSfP1FMa8Kxun08FDAOBp4QpxFq9ZFdyrTvP\n" +
      "NximmMatBrTcCKME1SmklpoSZ0qMYEWd8SOasACcaLWYUNPvji6SZbFIPiG+\n" +
      "FTAqDbUMo2s/rn9X9R+WfN9v3YIwLGUbQErNaLly7HF27FSOH4UMAWr6pjis\n" +
      "H8SE\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // ValiCert_Class_1_VA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIC5zCCAlACAQEwDQYJKoZIhvcNAQEFBQAwgbsxJDAiBgNVBAcTG1ZhbGlD\n" +
      "ZXJ0IFZhbGlkYXRpb24gTmV0d29yazEXMBUGA1UEChMOVmFsaUNlcnQsIElu\n" +
      "Yy4xNTAzBgNVBAsTLFZhbGlDZXJ0IENsYXNzIDEgUG9saWN5IFZhbGlkYXRp\n" +
      "b24gQXV0aG9yaXR5MSEwHwYDVQQDExhodHRwOi8vd3d3LnZhbGljZXJ0LmNv\n" +
      "bS8xIDAeBgkqhkiG9w0BCQEWEWluZm9AdmFsaWNlcnQuY29tMB4XDTk5MDYy\n" +
      "NTIyMjM0OFoXDTE5MDYyNTIyMjM0OFowgbsxJDAiBgNVBAcTG1ZhbGlDZXJ0\n" +
      "IFZhbGlkYXRpb24gTmV0d29yazEXMBUGA1UEChMOVmFsaUNlcnQsIEluYy4x\n" +
      "NTAzBgNVBAsTLFZhbGlDZXJ0IENsYXNzIDEgUG9saWN5IFZhbGlkYXRpb24g\n" +
      "QXV0aG9yaXR5MSEwHwYDVQQDExhodHRwOi8vd3d3LnZhbGljZXJ0LmNvbS8x\n" +
      "IDAeBgkqhkiG9w0BCQEWEWluZm9AdmFsaWNlcnQuY29tMIGfMA0GCSqGSIb3\n" +
      "DQEBAQUAA4GNADCBiQKBgQDYWYJ6ibiWuqYvaG9YLqdUHAZu9OqNSLwxlBfw\n" +
      "8068srg1knaw0KWlAdcAAxIiGQj4/xEjm84H9b9pGib+TunRf50sQB1ZaG6m\n" +
      "+FiwnRqP0z/x3BkGgagO4DrdyFNFCQbmD3DD+kCmDuJWBQ8YTfwggtFzVXSN\n" +
      "dnKgHZ0dwN0/cQIDAQABMA0GCSqGSIb3DQEBBQUAA4GBAFBoPUn0LBwGlN+V\n" +
      "YH+Wexf+T3GtZMjdd9LvWVXoP+iOBSoh8gfStadS/pyxtuJbdxdA6nLWI8so\n" +
      "gTLDAHkY7FkXicnGah5xyf23dKUlRWnFSKsZ4UWKJWsZ7uW7EvV/96aNUcPw\n" +
      "nXS3qT6gpf+2SQMT2iLM7XGCK5nPOrf1LXLI\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // ValiCert_Class_2_VA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIC5zCCAlACAQEwDQYJKoZIhvcNAQEFBQAwgbsxJDAiBgNVBAcTG1ZhbGlD\n" +
      "ZXJ0IFZhbGlkYXRpb24gTmV0d29yazEXMBUGA1UEChMOVmFsaUNlcnQsIElu\n" +
      "Yy4xNTAzBgNVBAsTLFZhbGlDZXJ0IENsYXNzIDIgUG9saWN5IFZhbGlkYXRp\n" +
      "b24gQXV0aG9yaXR5MSEwHwYDVQQDExhodHRwOi8vd3d3LnZhbGljZXJ0LmNv\n" +
      "bS8xIDAeBgkqhkiG9w0BCQEWEWluZm9AdmFsaWNlcnQuY29tMB4XDTk5MDYy\n" +
      "NjAwMTk1NFoXDTE5MDYyNjAwMTk1NFowgbsxJDAiBgNVBAcTG1ZhbGlDZXJ0\n" +
      "IFZhbGlkYXRpb24gTmV0d29yazEXMBUGA1UEChMOVmFsaUNlcnQsIEluYy4x\n" +
      "NTAzBgNVBAsTLFZhbGlDZXJ0IENsYXNzIDIgUG9saWN5IFZhbGlkYXRpb24g\n" +
      "QXV0aG9yaXR5MSEwHwYDVQQDExhodHRwOi8vd3d3LnZhbGljZXJ0LmNvbS8x\n" +
      "IDAeBgkqhkiG9w0BCQEWEWluZm9AdmFsaWNlcnQuY29tMIGfMA0GCSqGSIb3\n" +
      "DQEBAQUAA4GNADCBiQKBgQDOOnHK5avIWZJV16vYdA757tn2VUdZZUcOBVXc\n" +
      "65g2PFxTXdMwzzjsvUGJ7SVCCSRrCl6zfN1SLUzm1NZ9WlmpZdRJEy0kTRxQ\n" +
      "b7XBhVQ7/nHk01xC+YDgkRoKWzk2Z/M/VXwbP7RfZHM047QSv4dk+NoS/zcn\n" +
      "wbNDu+97bi5p9wIDAQABMA0GCSqGSIb3DQEBBQUAA4GBADt/UG9vUJSZSWI4\n" +
      "OB9L+KXIPqeCgfYrx+jFzug6EILLGACOTb2oWH+heQC1u+mNr0HZDzTuIYEZ\n" +
      "oDJJKPTEjlbVUjP9UNV+mWwD5MlM/Mtsq2azSiGM5bUMMj4QssxsodyamEwC\n" +
      "W/POuZ6lcg5Ktz885hZo+L7tdEy8W9ViH0Pd\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // ValiCert_OCSP_Responder.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDSDCCArGgAwIBAgIBATANBgkqhkiG9w0BAQUFADCBsjEkMCIGA1UEBxMb\n" +
      "VmFsaUNlcnQgVmFsaWRhdGlvbiBOZXR3b3JrMRcwFQYDVQQKEw5WYWxpQ2Vy\n" +
      "dCwgSW5jLjEsMCoGA1UECxMjQ2xhc3MgMSBWYWxpZGF0aW9uIEF1dGhvcml0\n" +
      "eSAtIE9DU1AxITAfBgNVBAMTGGh0dHA6Ly93d3cudmFsaWNlcnQubmV0LzEg\n" +
      "MB4GCSqGSIb3DQEJARYRaW5mb0B2YWxpY2VydC5jb20wHhcNMDAwMjEyMTE1\n" +
      "MDA1WhcNMDUwMjEwMTE1MDA1WjCBsjEkMCIGA1UEBxMbVmFsaUNlcnQgVmFs\n" +
      "aWRhdGlvbiBOZXR3b3JrMRcwFQYDVQQKEw5WYWxpQ2VydCwgSW5jLjEsMCoG\n" +
      "A1UECxMjQ2xhc3MgMSBWYWxpZGF0aW9uIEF1dGhvcml0eSAtIE9DU1AxITAf\n" +
      "BgNVBAMTGGh0dHA6Ly93d3cudmFsaWNlcnQubmV0LzEgMB4GCSqGSIb3DQEJ\n" +
      "ARYRaW5mb0B2YWxpY2VydC5jb20wgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJ\n" +
      "AoGBAMeML6fDQIc7PdfEmlgUZArDCDliGs/S66nxaXSKyg5adsyiUk7Q88R6\n" +
      "tfimHLujp6RTh1uNwAC71WYk53TGFsivyANi1TKHolKRRJSVqEdDbaVInPZM\n" +
      "ddVPYufJ/3v0JIynvCh2tTKgJXO3Ry94+Eb5hxTwd/wKd+hP/Ywf+mLZAgMB\n" +
      "AAGjbDBqMA8GCSsGAQUFBzABBQQCBQAwEwYDVR0lBAwwCgYIKwYBBQUHAwkw\n" +
      "CwYDVR0PBAQDAgGGMDUGCCsGAQUFBwEBBCkwJzAlBggrBgEFBQcwAYYZaHR0\n" +
      "cDovL29jc3AyLnZhbGljZXJ0Lm5ldDANBgkqhkiG9w0BAQUFAAOBgQAVxeC4\n" +
      "NHISBiCoYpWT0byTupCr3E6Njo2YTOMy9Ss/s5f7qqKtQJetaL1crVMO0Kaz\n" +
      "DawamY2qMB7PDnD/ArB3ZYPN2gdcUs1Zu6LI4rQWg4/UlXmTLei/RJMxkjDT\n" +
      "NDTxEPshrC70w11kY3qZ4ZqrQh1IZqZ3N7hVPK3+ZbBi6Q==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_1_Public_Primary_Certification_Authority.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICPTCCAaYCEQDNun9W8N/kvFT+IqyzcqpVMA0GCSqGSIb3DQEBAgUAMF8x\n" +
      "CzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE3MDUGA1UE\n" +
      "CxMuQ2xhc3MgMSBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhv\n" +
      "cml0eTAeFw05NjAxMjkwMDAwMDBaFw0yODA4MDEyMzU5NTlaMF8xCzAJBgNV\n" +
      "BAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE3MDUGA1UECxMuQ2xh\n" +
      "c3MgMSBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTCB\n" +
      "nzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEA5Rm/baNWYS2ZSHH2Z965jeu3\n" +
      "noaACpEO+jglr0aIguVzqKCbJF0NH8xlbgyw0FaEGIeaBpsQoXPftFg5a27B\n" +
      "9hXVqKg/qhIGjTGsf7A01480Z4gJzRQR4k5FVmkfeAKA2txHkSm7NsljXMXg\n" +
      "1y2He6G3MrB7MLoqLzGq7qNn2tsCAwEAATANBgkqhkiG9w0BAQIFAAOBgQBM\n" +
      "P7iLxmjf7kMzDl3ppssHhE16M/+SG/Q2rdiVIjZoEWx8QszznC7EBz8UsA9P\n" +
      "/5CSdvnivErpj82ggAr3xSnxgiJduLHdgSOjeyUVRjB5FvjqBUuUfx3CHMjj\n" +
      "t/QQQDwTw18fU+hI5Ia0e6E1sHslurjTjqs/OJ0ANACY89FxlA==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_1_Public_Primary_Certification_Authority_-_G2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDAjCCAmsCEEzH6qqYPnHTkxD4PTqJkZIwDQYJKoZIhvcNAQEFBQAwgcEx\n" +
      "CzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UE\n" +
      "CxMzQ2xhc3MgMSBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhv\n" +
      "cml0eSAtIEcyMTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5jLiAt\n" +
      "IEZvciBhdXRob3JpemVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2lnbiBU\n" +
      "cnVzdCBOZXR3b3JrMB4XDTk4MDUxODAwMDAwMFoXDTI4MDgwMTIzNTk1OVow\n" +
      "gcExCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoG\n" +
      "A1UECxMzQ2xhc3MgMSBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1\n" +
      "dGhvcml0eSAtIEcyMTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5j\n" +
      "LiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2ln\n" +
      "biBUcnVzdCBOZXR3b3JrMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCq\n" +
      "0Lq+Fi24g9TK0g+8djHKlNgdk4xWArzZbxpvUjZudVYKVdPfQ4chEWWKfo+9\n" +
      "Id5rMj8bhDSVBZ1BNeuS65bdqlk/AVNtmU/t5eIqWpDBucSmFc/IReumXY6c\n" +
      "PvBkJHalzasab7bYe1FhbqZ/h8jit+U03EGI6glAvnOSPWvndQIDAQABMA0G\n" +
      "CSqGSIb3DQEBBQUAA4GBAKlPww3HZ74sy9mozS11534Vnjty637rXC0Jh9Zr\n" +
      "bWB85a7FkCMMXErQr7Fd88e2CtvgFZMN3QO8x3aKtd1Pw5sTdbgBwObJW2ul\n" +
      "uIncrKTdcu1OofdPvAbT6shkdHvClUGcZXNY8ZCaPGqxmMnEh7zPRW1F4m4i\n" +
      "P/68DzFc6PLZ\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_1_Public_Primary_Certification_Authority_-_G3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEGjCCAwICEQCLW3VWhFSFCwDPrzhIzrGkMA0GCSqGSIb3DQEBBQUAMIHK\n" +
      "MQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNV\n" +
      "BAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5\n" +
      "IFZlcmlTaWduLCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBD\n" +
      "BgNVBAMTPFZlcmlTaWduIENsYXNzIDEgUHVibGljIFByaW1hcnkgQ2VydGlm\n" +
      "aWNhdGlvbiBBdXRob3JpdHkgLSBHMzAeFw05OTEwMDEwMDAwMDBaFw0zNjA3\n" +
      "MTYyMzU5NTlaMIHKMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24s\n" +
      "IEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNV\n" +
      "BAsTMShjKSAxOTk5IFZlcmlTaWduLCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQg\n" +
      "dXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlTaWduIENsYXNzIDEgUHVibGljIFBy\n" +
      "aW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgLSBHMzCCASIwDQYJKoZI\n" +
      "hvcNAQEBBQADggEPADCCAQoCggEBAN2E1Lm0+afY8wR4nN493GwTFtl63SRR\n" +
      "ZsDHJlkNrAYIwpTRMx/wgzUfbhvI3qpuFU5UJ+/EbRrsC+MO8ESlV8dAWB6j\n" +
      "Rx9x7GD2bZTIGDnt/kIYVt/kTEkQeE4BdjVjEjbdZrwBBDajVWjVojYJrKsh\n" +
      "JlQGrT/KFOCsyq0GHZXi+J3x4GD/wn91K0zM2v6HmSHquv4+VNfSWXjbPG7P\n" +
      "oBMAGrgnoeS+Z5bKoMWznN3JdZ7rMJpfo83ZrngZPyPpXNspva1VyBtUjGP2\n" +
      "6KbqxzcSXKMpHgLZ2x87tNcPVkeBFQRKr4Mn0cVYiMHd9qqnoxjaaKptEVHh\n" +
      "v2Vrn5Z20T0CAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAq2aN17O6x5q25lXQ\n" +
      "BfGfMY1aqtmqRiYPce2lrVNWYgFHKkTp/j90CxObufRNG7LRX7K20ohcs5/N\n" +
      "y9Sn2WCVhDr4wTcdYcrnsMXlkdpUpqwxga6X3s0IrLjAl4B/bnKk52kTlWUf\n" +
      "xJM8/XmPBNQ+T+r3ns7NZ3xPZQL/kYVUc8f/NveGLezQXk//EZ9yBta4GvFM\n" +
      "DSZl4kSAHsef493oCtrspSCAaWihT37ha88HQfqDjrw43bAuEbFrskLMmrz5\n" +
      "SCJ5ShkPshw+IHTZasO+8ih4E1Z5T21Q6huwtVexN2ZYI/PcD98Kh8TvhgXV\n" +
      "OBRgmaNL3gaWcSzy27YfpO8/7g==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_1_Public_Primary_OCSP_Responder.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDnjCCAwegAwIBAgIQK2jUo0aexTsoCas4XX8nIDANBgkqhkiG9w0BAQUF\n" +
      "ADBfMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xNzA1\n" +
      "BgNVBAsTLkNsYXNzIDEgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBB\n" +
      "dXRob3JpdHkwHhcNMDAwODA0MDAwMDAwWhcNMDQwODAzMjM1OTU5WjCBpzEX\n" +
      "MBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRy\n" +
      "dXN0IE5ldHdvcmsxOzA5BgNVBAsTMlRlcm1zIG9mIHVzZSBhdCBodHRwczov\n" +
      "L3d3dy52ZXJpc2lnbi5jb20vUlBBIChjKTAwMS4wLAYDVQQDEyVDbGFzcyAx\n" +
      "IFB1YmxpYyBQcmltYXJ5IE9DU1AgUmVzcG9uZGVyMIGfMA0GCSqGSIb3DQEB\n" +
      "AQUAA4GNADCBiQKBgQC57V56Ondfzl86UvzNZPdxtW9qlsZZklWUXS9bLsER\n" +
      "6iaKy6eBPPZaRN56Ey/9WlHZezcmSsAnPwQDalbBgyzhb1upVFAkSsYuekyh\n" +
      "WzdUJCExH6F4GHansXDaItBq/gdiQMb39pt9DAa4S8co5GYjhFHvRreT2IEz\n" +
      "y+U2rMboBQIDAQABo4IBEDCCAQwwIAYDVR0RBBkwF6QVMBMxETAPBgNVBAMT\n" +
      "CE9DU1AgMS0xMDEGA1UdHwQqMCgwJqAkoCKGIGh0dHA6Ly9jcmwudmVyaXNp\n" +
      "Z24uY29tL3BjYTEuY3JsMBMGA1UdJQQMMAoGCCsGAQUFBwMJMEIGCCsGAQUF\n" +
      "BwEBBDYwNDAyBggrBgEFBQcwAaYmFiRodHRwOi8vb2NzcC52ZXJpc2lnbi5j\n" +
      "b20vb2NzcC9zdGF0dXMwRAYDVR0gBD0wOzA5BgtghkgBhvhFAQcBATAqMCgG\n" +
      "CCsGAQUFBwIBFhxodHRwczovL3d3dy52ZXJpc2lnbi5jb20vUlBBMAkGA1Ud\n" +
      "EwQCMAAwCwYDVR0PBAQDAgeAMA0GCSqGSIb3DQEBBQUAA4GBAHCQ3bjkvlMX\n" +
      "fH8C6dX3i5mTMWCNfuZgayTvYKzSzpHegG0JpNO4OOVEynJeDS3Bd5y9LAN4\n" +
      "KY2kpXeH9fErJq3MB2w6VFoo4AnzTQoEytRYaQuns/XdAaXn3PAfusFdkI2z\n" +
      "6k/BEVmXarIrE7HarZehs7GgIFvKMquNzxPwHynD\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_2_Public_Primary_Certification_Authority.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICPDCCAaUCEC0b/EoXjaOR6+f/9YtFvgswDQYJKoZIhvcNAQECBQAwXzEL\n" +
      "MAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMTcwNQYDVQQL\n" +
      "Ey5DbGFzcyAyIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9y\n" +
      "aXR5MB4XDTk2MDEyOTAwMDAwMFoXDTI4MDgwMTIzNTk1OVowXzELMAkGA1UE\n" +
      "BhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMTcwNQYDVQQLEy5DbGFz\n" +
      "cyAyIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIGf\n" +
      "MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC2WoujDWojg4BrzzmH9CETMwZM\n" +
      "JaLtVRKXxaeAufqDwSCg+i8VDXyhYGt+eSz6Bg86rvYbb7HS/y8oUl+DfUvE\n" +
      "erf4Zh+AVPy3wo5ZShRXRtGak75BkQO7FYCTXOvnzAhsPz6zSvz/S2wj1VCC\n" +
      "JkQZjiPDceoZJEcEnnW/yKYAHwIDAQABMA0GCSqGSIb3DQEBAgUAA4GBAIob\n" +
      "K/o5wXTXXtgZZKJYSi034DNHD6zt96rbHuSLBlxgJ8pFUs4W7z8GZOeUaHxg\n" +
      "MxURaa+dYo2jA1Rrpr7l7gUYYAS/QoD90KioHgE796Ncr6Pc5iaAIzy4RHT3\n" +
      "Cq5Ji2F4zCS/iIqnDupzGUH9TQPwiNHleI2lKk/2lw0Xd8rY\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_2_Public_Primary_Certification_Authority_-_G2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDAzCCAmwCEQC5L2DMiJ+hekYJuFtwbIqvMA0GCSqGSIb3DQEBBQUAMIHB\n" +
      "MQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xPDA6BgNV\n" +
      "BAsTM0NsYXNzIDIgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRo\n" +
      "b3JpdHkgLSBHMjE6MDgGA1UECxMxKGMpIDE5OTggVmVyaVNpZ24sIEluYy4g\n" +
      "LSBGb3IgYXV0aG9yaXplZCB1c2Ugb25seTEfMB0GA1UECxMWVmVyaVNpZ24g\n" +
      "VHJ1c3QgTmV0d29yazAeFw05ODA1MTgwMDAwMDBaFw0yODA4MDEyMzU5NTla\n" +
      "MIHBMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xPDA6\n" +
      "BgNVBAsTM0NsYXNzIDIgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBB\n" +
      "dXRob3JpdHkgLSBHMjE6MDgGA1UECxMxKGMpIDE5OTggVmVyaVNpZ24sIElu\n" +
      "Yy4gLSBGb3IgYXV0aG9yaXplZCB1c2Ugb25seTEfMB0GA1UECxMWVmVyaVNp\n" +
      "Z24gVHJ1c3QgTmV0d29yazCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEA\n" +
      "p4gBIXQs5xoD8JjhlzwPIQjxnNuX6Zr8wgQGE75fUsjMHiwSViy4AWkszJkf\n" +
      "rbCWrnkE8hM5wXuYuggs6MKEEyyqaekJ9MepAqRCwiNPStjwDqL7MWzJ5m+Z\n" +
      "Jwf15vRMeJ5t60aG+rmGyVTyssSv1EYcWskVMP8NbPUtDm3Of3cCAwEAATAN\n" +
      "BgkqhkiG9w0BAQUFAAOBgQByLvl/0fFx+8Se9sVeUYpAmLho+Jscg9jinb3/\n" +
      "7aHmZuovCfTK1+qlK5X2JGCGTUQug6XELaDTrnhpb3LabK4I8GOSN+a7xDAX\n" +
      "rXfMSTWqz9iP0b63GJZHc2pUIjRkLbYWm1lbtFFZOrMLFPQS32eg9K0yZF6x\n" +
      "RnInjBJ7xUS0rg==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_2_Public_Primary_Certification_Authority_-_G3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEGTCCAwECEGFwy0mMX5hFKeewptlQW3owDQYJKoZIhvcNAQEFBQAwgcox\n" +
      "CzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjEfMB0GA1UE\n" +
      "CxMWVmVyaVNpZ24gVHJ1c3QgTmV0d29yazE6MDgGA1UECxMxKGMpIDE5OTkg\n" +
      "VmVyaVNpZ24sIEluYy4gLSBGb3IgYXV0aG9yaXplZCB1c2Ugb25seTFFMEMG\n" +
      "A1UEAxM8VmVyaVNpZ24gQ2xhc3MgMiBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZp\n" +
      "Y2F0aW9uIEF1dGhvcml0eSAtIEczMB4XDTk5MTAwMTAwMDAwMFoXDTM2MDcx\n" +
      "NjIzNTk1OVowgcoxCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwg\n" +
      "SW5jLjEfMB0GA1UECxMWVmVyaVNpZ24gVHJ1c3QgTmV0d29yazE6MDgGA1UE\n" +
      "CxMxKGMpIDE5OTkgVmVyaVNpZ24sIEluYy4gLSBGb3IgYXV0aG9yaXplZCB1\n" +
      "c2Ugb25seTFFMEMGA1UEAxM8VmVyaVNpZ24gQ2xhc3MgMiBQdWJsaWMgUHJp\n" +
      "bWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAtIEczMIIBIjANBgkqhkiG\n" +
      "9w0BAQEFAAOCAQ8AMIIBCgKCAQEArwoNwtUs22e5LeWUJ92lvuCwTY+zYVY8\n" +
      "1nzD9M0+hsuiiOLh2KRpxbXiv8GmR1BeRjmL1Za6tW8UvxDOJxOeBUebMXoT\n" +
      "2B/Z0wI3i60sR/COgQanDTAM6/c8DyAd3HJG7qUCyFvDyVZpTMUYwZF7C9UT\n" +
      "AJu878NIPkZgIIUq1ZC2zYugzDLdt/1AVbJQHFauzI13TccgTacxdu9okoqQ\n" +
      "HgiBVrKtaaNS0MscxCM9H5n+TOgWY47GCI72MfbS+uV23bUckqNJzc0BzWjN\n" +
      "qWm6o+sdDZykIKbBoMXRRkwXbdKsZj+WjOCE1Db/IlnF+RFgqF8EffIa9iVC\n" +
      "YQ/ESrg+iQIDAQABMA0GCSqGSIb3DQEBBQUAA4IBAQA0JhU8wI1NQ0kdvekh\n" +
      "ktdmnLfexbjQ5F1fdiLAJvmEOjr5jLX77GDx6M4EsMjdpwOPMPOY36TmpDHf\n" +
      "0xwLRtxyID+u7gU8pDM/CzmscHhzS5kr3zDCVLCoO1Wh/hYozUK9dG6A2ydE\n" +
      "p85EXdQbkJgNHkKUsQAsBNB0owIFImNjzYO1+8FtYmtpdf1dcEG59b98377B\n" +
      "MnMiIYtYgXsVkXq642RIsH/7NiXaldDxJBQX3RiAa0YjOVT1jmIJBB2UkKab\n" +
      "5iXiQkWquJCtvgiPqQtCGJTPcjnhsUPgKM+351psE2tJs//jGHyJizNdrDPX\n" +
      "p/naOlXJWBD5qu9ats9LS98q\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_2_Public_Primary_OCSP_Responder.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDnjCCAwegAwIBAgIQCUYX5h3Y1BygDKBi6HmKpzANBgkqhkiG9w0BAQUF\n" +
      "ADBfMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xNzA1\n" +
      "BgNVBAsTLkNsYXNzIDIgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBB\n" +
      "dXRob3JpdHkwHhcNMDAwODAxMDAwMDAwWhcNMDQwNzMxMjM1OTU5WjCBpzEX\n" +
      "MBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRy\n" +
      "dXN0IE5ldHdvcmsxOzA5BgNVBAsTMlRlcm1zIG9mIHVzZSBhdCBodHRwczov\n" +
      "L3d3dy52ZXJpc2lnbi5jb20vUlBBIChjKTAwMS4wLAYDVQQDEyVDbGFzcyAy\n" +
      "IFB1YmxpYyBQcmltYXJ5IE9DU1AgUmVzcG9uZGVyMIGfMA0GCSqGSIb3DQEB\n" +
      "AQUAA4GNADCBiQKBgQDQymMxYX9ENHwFfQs9apDLeUt3Cj9LxyPlwGItfpx+\n" +
      "PoiHkdCs6E1Jh6KWkIrdBKUCP4yb6Yn+YqDiWr3I3bR45qVCkwhnAcAgTddc\n" +
      "9F3as+M3plIaLExlTYqH2aij8UlUuzxcgFFoxvtJ/wtVqxXd+5rBuR10DbKM\n" +
      "RF2J/J/5gwIDAQABo4IBEDCCAQwwIAYDVR0RBBkwF6QVMBMxETAPBgNVBAMT\n" +
      "CE9DU1AgMS0yMDEGA1UdHwQqMCgwJqAkoCKGIGh0dHA6Ly9jcmwudmVyaXNp\n" +
      "Z24uY29tL3BjYTIuY3JsMBMGA1UdJQQMMAoGCCsGAQUFBwMJMEIGCCsGAQUF\n" +
      "BwEBBDYwNDAyBggrBgEFBQcwAaYmFiRodHRwOi8vb2NzcC52ZXJpc2lnbi5j\n" +
      "b20vb2NzcC9zdGF0dXMwRAYDVR0gBD0wOzA5BgtghkgBhvhFAQcBATAqMCgG\n" +
      "CCsGAQUFBwIBFhxodHRwczovL3d3dy52ZXJpc2lnbi5jb20vUlBBMAkGA1Ud\n" +
      "EwQCMAAwCwYDVR0PBAQDAgeAMA0GCSqGSIb3DQEBBQUAA4GBAB99CW4kRnUE\n" +
      "nPMmm+M5bhfvvL2iG9IChIar0ECXLMRDiDcZayKoA3FQnSDcNmAgmnMtc1Vs\n" +
      "WJsswrQ0LHozQsqR2elDr88e4PXEeqs/cmMeqTfhWzuIsxOGgpBXy1f/9Fa+\n" +
      "It3jl6jhvCJDwt1N2/aBnpIUnjkPE1TegtjAXjSN\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_3_Public_Primary_Certification_Authority.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICPDCCAaUCEHC65B0Q2Sk0tjjKewPMur8wDQYJKoZIhvcNAQECBQAwXzEL\n" +
      "MAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMTcwNQYDVQQL\n" +
      "Ey5DbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9y\n" +
      "aXR5MB4XDTk2MDEyOTAwMDAwMFoXDTI4MDgwMTIzNTk1OVowXzELMAkGA1UE\n" +
      "BhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMTcwNQYDVQQLEy5DbGFz\n" +
      "cyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIGf\n" +
      "MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDJXFme8huKARS0EN8EQNvjV69q\n" +
      "RUCPhAwL0TPZ2RHP7gJYHyX3KqhEBarsAx94f56TuZoAqiN91qyFomNFx3In\n" +
      "zPRMxnVx0jnvT0Lwdd8KkMaOIG+YD/isI19wKTakyYbnsZogy1Olhec9vn2a\n" +
      "/iRFM9x2Fe0PonFkTGUugWhFpwIDAQABMA0GCSqGSIb3DQEBAgUAA4GBALtM\n" +
      "EivPLCYATxQT3ab7/AoRhIzzKBxnki98tsX63/Dolbwdj2wsqFHMc9ikwFPw\n" +
      "TtYmwHYBV4GSXiHx0bH/59AhWM1pF+NEHJwZRDmJXNycAA9WjQKZ7aKQRUzk\n" +
      "uxCkPfAyAw7xzvjoyVGM5mKf5p/AfbdynMk2OmufTqj/ZA1k\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_3_Public_Primary_Certification_Authority_-_G2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDAjCCAmsCEH3Z/gfPqB63EHln+6eJNMYwDQYJKoZIhvcNAQEFBQAwgcEx\n" +
      "CzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UE\n" +
      "CxMzQ2xhc3MgMyBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhv\n" +
      "cml0eSAtIEcyMTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5jLiAt\n" +
      "IEZvciBhdXRob3JpemVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2lnbiBU\n" +
      "cnVzdCBOZXR3b3JrMB4XDTk4MDUxODAwMDAwMFoXDTI4MDgwMTIzNTk1OVow\n" +
      "gcExCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoG\n" +
      "A1UECxMzQ2xhc3MgMyBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1\n" +
      "dGhvcml0eSAtIEcyMTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5j\n" +
      "LiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2ln\n" +
      "biBUcnVzdCBOZXR3b3JrMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDM\n" +
      "XtERXVxp0KvTuWpMmR9ZmDCOFoUgRm1HP9SFIIThbbP4pO0M8RcPO/mn+SXX\n" +
      "wc+EY/J8Y8+iR/LGWzOOZEAEaMGAuWQcRXfH2G71lSk8UOg013gfqLptQ5GV\n" +
      "j0VXXn7F+8qkBOvqlzdUMG+7AUcyM83cV5tkaWH4mx0ciU9cZwIDAQABMA0G\n" +
      "CSqGSIb3DQEBBQUAA4GBAFFNzb5cy5gZnBWyATl4Lk0PZ3BwmcYQWpSkU01U\n" +
      "bSuvDV1Ai2TT1+7eVmGSX6bEHRBhNtMsJzzoKQm5EWR0zLVznxxIqbxhAe7i\n" +
      "F6YM40AIOw7n60RzKprxaZLvcRTDOaxxp5EJb+RxBrO6WVcmeQD2+A2iMzAo\n" +
      "1KpYoJ2daZH9\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_3_Public_Primary_Certification_Authority_-_G3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEGjCCAwICEQCbfgZJoz5iudXukEhxKe9XMA0GCSqGSIb3DQEBBQUAMIHK\n" +
      "MQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNV\n" +
      "BAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5\n" +
      "IFZlcmlTaWduLCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBD\n" +
      "BgNVBAMTPFZlcmlTaWduIENsYXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlm\n" +
      "aWNhdGlvbiBBdXRob3JpdHkgLSBHMzAeFw05OTEwMDEwMDAwMDBaFw0zNjA3\n" +
      "MTYyMzU5NTlaMIHKMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24s\n" +
      "IEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNV\n" +
      "BAsTMShjKSAxOTk5IFZlcmlTaWduLCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQg\n" +
      "dXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlTaWduIENsYXNzIDMgUHVibGljIFBy\n" +
      "aW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgLSBHMzCCASIwDQYJKoZI\n" +
      "hvcNAQEBBQADggEPADCCAQoCggEBAMu6nFL8eB8aHm8bN3O9+MlrlBIwT/A2\n" +
      "R/XQkQr1F8ilYcEWQE37imGQ5XYgwREGfassbqb1EUGO+i2tKmFZpGcmTNDo\n" +
      "vFJbcCAEWNF6yaRpvIMXZK0Fi7zQWM6NjPXr8EJJC52XJ2cybuGukxUccLwg\n" +
      "TS8Y3pKI6GyFVxEa6X7jJhFUokWWVYPKMIno3Nij7SqAP395ZVc+FSBmCC+V\n" +
      "k7+qRy+oRpfwEuL+wgorUeZ25rdGt+INpsyow0xZVYnm6FNcHOqd8GIWC6fJ\n" +
      "Xwzw3sJ2zq/3avL6QaaiMxTJ5Xpj055iN9WFZZ4O5lMkdBteHRJTW8cs54NJ\n" +
      "OxWuimi5V5cCAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAERSWwauSCPc/L8my\n" +
      "/uRan2Te2yFPhpk0djZX3dAVL8WtfxUfN2JzPtTnX84XA9s1+ivbrmAJXx5f\n" +
      "j267Cz3qWhMeDGBvtcC1IyIuBwvLqXTLR7sdwdela8wv0kL9Sd2nic9TutoA\n" +
      "Wii/gt/4uhMdUIaC/Y4wjylGsB49Ndo4YhYYSq3mtlFs3q9i6wHQHiT+eo8S\n" +
      "GhJouPtmmRQURVyu565pF4ErWjfJXir0xuKhXFSbplQAz/DxwceYMBo7Nhbb\n" +
      "o27q/a2ywtrvAkcTisDxszGtTxzhT5yvDwyd93gN2PQ1VoDat20Xj50egWTh\n" +
      "/sVFuq1ruQp6Tk9LhO5L8X3dEQ==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_3_Public_Primary_OCSP_Responder.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDojCCAwugAwIBAgIQLpaev7ZibOx76XPM42zBhDANBgkqhkiG9w0BAQUF\n" +
      "ADBfMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xNzA1\n" +
      "BgNVBAsTLkNsYXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlvbiBB\n" +
      "dXRob3JpdHkwHhcNMDAwODA0MDAwMDAwWhcNMDQwODAzMjM1OTU5WjCBpzEX\n" +
      "MBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRy\n" +
      "dXN0IE5ldHdvcmsxOzA5BgNVBAsTMlRlcm1zIG9mIHVzZSBhdCBodHRwczov\n" +
      "L3d3dy52ZXJpc2lnbi5jb20vUlBBIChjKTAwMS4wLAYDVQQDEyVDbGFzcyAz\n" +
      "IFB1YmxpYyBQcmltYXJ5IE9DU1AgUmVzcG9uZGVyMIGfMA0GCSqGSIb3DQEB\n" +
      "AQUAA4GNADCBiQKBgQDx5AgOg7t140jluNum8Lmr6Txix141W9ACVBHYydFW\n" +
      "uXZLuat65s269gwE1n7WsAplrE454/H3LaMlOe+wi8++2wxdbnD0B81w9zrA\n" +
      "PjUW7XiMQ8/CJi5H1oZ9nPG+1mcMIiWkymXmH3p4KC8/BdsEIb/hRWb+PLeC\n" +
      "7Vq4FhW5VQIDAQABo4IBFDCCARAwIAYDVR0RBBkwF6QVMBMxETAPBgNVBAMT\n" +
      "CE9DU1AgMS0zMDUGA1UdHwQuMCwwKqAooCaGJGh0dHA6Ly9jcmwudmVyaXNp\n" +
      "Z24uY29tL3BjYTMuMS4xLmNybDATBgNVHSUEDDAKBggrBgEFBQcDCTBCBggr\n" +
      "BgEFBQcBAQQ2MDQwMgYIKwYBBQUHMAGmJhYkaHR0cDovL29jc3AudmVyaXNp\n" +
      "Z24uY29tL29jc3Avc3RhdHVzMEQGA1UdIAQ9MDswOQYLYIZIAYb4RQEHAQEw\n" +
      "KjAoBggrBgEFBQcCARYcaHR0cHM6Ly93d3cudmVyaXNpZ24uY29tL1JQQTAJ\n" +
      "BgNVHRMEAjAAMAsGA1UdDwQEAwIHgDANBgkqhkiG9w0BAQUFAAOBgQAC9lNj\n" +
      "wKke8tCLMzCPSJtMsFa0g3FKvtxQ2PW24AvbvXhP6c8JNNopSZ0Bc1qRkYJU\n" +
      "LBMK03cjzzf8Y96n4/a3tWlFKEnDkdyqRxypiJksBSqNjYr6YuJatwAgXTnE\n" +
      "KMLL/J6oia5bPY4S6jKy/OsU1wkVGsDNG9W1FU5B1ZbjTg==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_4_Public_Primary_Certification_Authority_-_G2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDAjCCAmsCEDKIjprS9esTR/h/xCA3JfgwDQYJKoZIhvcNAQEFBQAwgcEx\n" +
      "CzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UE\n" +
      "CxMzQ2xhc3MgNCBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhv\n" +
      "cml0eSAtIEcyMTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5jLiAt\n" +
      "IEZvciBhdXRob3JpemVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2lnbiBU\n" +
      "cnVzdCBOZXR3b3JrMB4XDTk4MDUxODAwMDAwMFoXDTI4MDgwMTIzNTk1OVow\n" +
      "gcExCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoG\n" +
      "A1UECxMzQ2xhc3MgNCBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1\n" +
      "dGhvcml0eSAtIEcyMTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5j\n" +
      "LiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2ln\n" +
      "biBUcnVzdCBOZXR3b3JrMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC6\n" +
      "8OTP+cSuhVS5B1f5j8V/aBH4xBewRNzjMHPVKmIquNDMHO0oW369atyzkSTK\n" +
      "QWI8/AIBvxwWMZQFl3Zuoq29YRdsTjCG8FE3KlDHqGKB3FtKqsGgtG7rL+VX\n" +
      "xbErQHDbWk2hjh+9Ax/YA9SPTJlxvOKCzFjomDqG04Y48wApHwIDAQABMA0G\n" +
      "CSqGSIb3DQEBBQUAA4GBAIWMEsGnuVAVess+rLhDityq3RS6iYF+ATwjcSGI\n" +
      "L4LcY/oCRaxFWdcqWERbt5+BO5JoPeI3JPV7bI92NZYJqFmduc4jq3TWg/0y\n" +
      "cyfYaT5DdPauxYma51N86Xv2S/PBZYPejYqcPIiNOVn8qj8ijaHBZlCBckzt\n" +
      "ImRPT8qAkbYp\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Class_4_Public_Primary_Certification_Authority_-_G3.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIEGjCCAwICEQDsoKeLbnVqAc/EfMwvlF7XMA0GCSqGSIb3DQEBBQUAMIHK\n" +
      "MQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNV\n" +
      "BAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNVBAsTMShjKSAxOTk5\n" +
      "IFZlcmlTaWduLCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxRTBD\n" +
      "BgNVBAMTPFZlcmlTaWduIENsYXNzIDQgUHVibGljIFByaW1hcnkgQ2VydGlm\n" +
      "aWNhdGlvbiBBdXRob3JpdHkgLSBHMzAeFw05OTEwMDEwMDAwMDBaFw0zNjA3\n" +
      "MTYyMzU5NTlaMIHKMQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24s\n" +
      "IEluYy4xHzAdBgNVBAsTFlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOjA4BgNV\n" +
      "BAsTMShjKSAxOTk5IFZlcmlTaWduLCBJbmMuIC0gRm9yIGF1dGhvcml6ZWQg\n" +
      "dXNlIG9ubHkxRTBDBgNVBAMTPFZlcmlTaWduIENsYXNzIDQgUHVibGljIFBy\n" +
      "aW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgLSBHMzCCASIwDQYJKoZI\n" +
      "hvcNAQEBBQADggEPADCCAQoCggEBAK3LpRFpxlmr8Y+1GQ9Wzsy1HyDkniYl\n" +
      "S+BzZYlZ3tCD5PUPtbut8XzoIfzk6AzufEUiGXaStBO3IFsJ+mGuqPKljYXC\n" +
      "KtbeZjbSmwL0qJJgfJxptI8kHtCGUvYynEFYHiK9zUVilQhu0GbdU6LM8BDc\n" +
      "VHOLBKFGMzNcF0C5nk3T875Vg+ixiY5afJqWIpA7iCXy0lOIAgwLePLmNxdL\n" +
      "MEYH5IBtptiWLugs+BGzOA1mppvqySNb247i8xOOGlktqgLw7KSHZtzBP/XY\n" +
      "ufTsgsbSPZUd5cBPhMnZo0QoBmrXRazwa2rvTl/4EYIeOGM0ZlDUPpNz+jDD\n" +
      "Zq3/ky2X7wMCAwEAATANBgkqhkiG9w0BAQUFAAOCAQEAj/ola09b5KROJ1Wr\n" +
      "IhVZPMq1CtRK26vdoV9TxaBXOcLORyu+OshWv8LZJxA6sQU8wHcxuzrTBXtt\n" +
      "mhwwjIDLk5Mqg6sFUYICABFna/OIYUdfA5PVWw3g8dShMjWFsjrbsIKr0csK\n" +
      "vE+MW8VLADsfKoKmfjaF3H48ZwC15DtS4KjrXRX5xm3wrR0OhbepmnMUWluP\n" +
      "QSjA1egtTaRezarZ7c7c2NU8Qh0XwRJdRTjDOPP8hS6DRkiy1yBfkjaP53kP\n" +
      "mF6Z6PDQpLv1U70qzlmwr25/bLvSHgCwIe34QWKCudiyxLtGUPMxxY8BqHTr\n" +
      "9Xgn2uf3ZkPznoM+IKrDNWCRzg==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_RSA_Secure_Server_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIICNDCCAaECEAKtZn5ORf5eV288mBle3cAwDQYJKoZIhvcNAQECBQAwXzEL\n" +
      "MAkGA1UEBhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMu\n" +
      "MS4wLAYDVQQLEyVTZWN1cmUgU2VydmVyIENlcnRpZmljYXRpb24gQXV0aG9y\n" +
      "aXR5MB4XDTk0MTEwOTAwMDAwMFoXDTEwMDEwNzIzNTk1OVowXzELMAkGA1UE\n" +
      "BhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5LCBJbmMuMS4wLAYD\n" +
      "VQQLEyVTZWN1cmUgU2VydmVyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5MIGb\n" +
      "MA0GCSqGSIb3DQEBAQUAA4GJADCBhQJ+AJLOesGugz5aqomDV6wlAXYMra6O\n" +
      "LDfO6zV4ZFQD5YRAUcm/jwjiioII0haGN1XpsSECrXZogZoFokvJSyVmIlZs\n" +
      "iAeP94FZbYQHZXATcXY+m3dM41CJVphIuR2nKRoTLkoRWZweFdVJVCxzOmmC\n" +
      "sZc5nG1wZ0jl3S3WyB57AgMBAAEwDQYJKoZIhvcNAQECBQADfgBl3X7hsuyw\n" +
      "4jrg7HFGmhkRuNPHoLQDQCYCPgmc4RKz0Vr2N6W3YQO2WxZpO8ZECAyIUwxr\n" +
      "l0nHPjXcbLm7qt9cuzovk2C2qUtN8iD3zV9/ZHuO3ABc1/p3yjkWWW8O6tO1\n" +
      "g39NTUJWdrTJXwT4OPjr0l91X817/OWOgHz8UA==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Secure_Server_OCSP_Responder.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDnzCCAwygAwIBAgIRAP9F1SddJPuzwjkkU1fhT94wDQYJKoZIhvcNAQEF\n" +
      "BQAwXzELMAkGA1UEBhMCVVMxIDAeBgNVBAoTF1JTQSBEYXRhIFNlY3VyaXR5\n" +
      "LCBJbmMuMS4wLAYDVQQLEyVTZWN1cmUgU2VydmVyIENlcnRpZmljYXRpb24g\n" +
      "QXV0aG9yaXR5MB4XDTAwMDgwNDAwMDAwMFoXDTA0MDgwMzIzNTk1OVowgZ4x\n" +
      "FzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZWZXJpU2lnbiBU\n" +
      "cnVzdCBOZXR3b3JrMTswOQYDVQQLEzJUZXJtcyBvZiB1c2UgYXQgaHR0cHM6\n" +
      "Ly93d3cudmVyaXNpZ24uY29tL1JQQSAoYykwMDElMCMGA1UEAxMcU2VjdXJl\n" +
      "IFNlcnZlciBPQ1NQIFJlc3BvbmRlcjCBnzANBgkqhkiG9w0BAQEFAAOBjQAw\n" +
      "gYkCgYEAuFGZZIUO7rMKaPC/Y3YdU/X8oXiMM+6f9L452psPTUepjyDoS0S9\n" +
      "zs17kNEw6JDEJXuJKN699pMd/7n/krWpjeSuzOLDB4Nqo3IQASdiIqY1Jjkt\n" +
      "ns9gDPxHpNfQQninHWzQy08VpykKtJVFxLHnWgnXOZXYHTWewr2zXcEMSx8C\n" +
      "AwEAAaOCAR0wggEZMCAGA1UdEQQZMBekFTATMREwDwYDVQQDEwhPQ1NQIDEt\n" +
      "NDA+BgNVHR8ENzA1MDOgMaAvhi1odHRwOi8vY3JsLnZlcmlzaWduLmNvbS9S\n" +
      "U0FTZWN1cmVTZXJ2ZXItcC5jcmwwEwYDVR0lBAwwCgYIKwYBBQUHAwkwQgYI\n" +
      "KwYBBQUHAQEENjA0MDIGCCsGAQUFBzABpiYWJGh0dHA6Ly9vY3NwLnZlcmlz\n" +
      "aWduLmNvbS9vY3NwL3N0YXR1czBEBgNVHSAEPTA7MDkGC2CGSAGG+EUBBwEB\n" +
      "MCowKAYIKwYBBQUHAgEWHGh0dHBzOi8vd3d3LnZlcmlzaWduLmNvbS9SUEEw\n" +
      "CQYDVR0TBAIwADALBgNVHQ8EBAMCB4AwDQYJKoZIhvcNAQEFBQADfgAAsxBT\n" +
      "ZpxJky4xoAJC0lhXfmah/huKYRhQQCweK0Gl1tv/rAgcWgVtAlwqtpZPR9u+\n" +
      "TtvOzLqGuBjOsRKRX2P380g+zPFNE+RtCZR4AJLLoyCdBgtqoEMHztEZbI8Y\n" +
      "dZqfFzP9qSa44+LewqjEWop/mNYHBmvMVp6GcM7U7w==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Verisign_Time_Stamping_Authority_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDzTCCAzagAwIBAgIQU2GyYK7bcY6nlLMTM/QHCTANBgkqhkiG9w0BAQUF\n" +
      "ADCBwTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMTww\n" +
      "OgYDVQQLEzNDbGFzcyAzIFB1YmxpYyBQcmltYXJ5IENlcnRpZmljYXRpb24g\n" +
      "QXV0aG9yaXR5IC0gRzIxOjA4BgNVBAsTMShjKSAxOTk4IFZlcmlTaWduLCBJ\n" +
      "bmMuIC0gRm9yIGF1dGhvcml6ZWQgdXNlIG9ubHkxHzAdBgNVBAsTFlZlcmlT\n" +
      "aWduIFRydXN0IE5ldHdvcmswHhcNMDAwOTI2MDAwMDAwWhcNMTAwOTI1MjM1\n" +
      "OTU5WjCBpTEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsTFlZl\n" +
      "cmlTaWduIFRydXN0IE5ldHdvcmsxOzA5BgNVBAsTMlRlcm1zIG9mIHVzZSBh\n" +
      "dCBodHRwczovL3d3dy52ZXJpc2lnbi5jb20vcnBhIChjKTAwMSwwKgYDVQQD\n" +
      "EyNWZXJpU2lnbiBUaW1lIFN0YW1waW5nIEF1dGhvcml0eSBDQTCBnzANBgkq\n" +
      "hkiG9w0BAQEFAAOBjQAwgYkCgYEA0hmdZ8IAIVlizrQJIkRpivglWtvtDbc2\n" +
      "fk7gu5Q+kCWHwmFHKdm9VLhjzCx9abQzNvQ3B5rB3UBU/OB4naCTuQk9I1F/\n" +
      "RMIUdNsKvsvJMDRAmD7Q1yUQgZS9B0+c1lQn3y6ov8uQjI11S7zi6ESHzeZB\n" +
      "CiVu6PQkAsVSD27smHUCAwEAAaOB3zCB3DAPBgNVHRMECDAGAQH/AgEAMEUG\n" +
      "A1UdIAQ+MDwwOgYMYIZIAYb4RQEHFwEDMCowKAYIKwYBBQUHAgEWHGh0dHBz\n" +
      "Oi8vd3d3LnZlcmlzaWduLmNvbS9ycGEwMQYDVR0fBCowKDAmoCSgIoYgaHR0\n" +
      "cDovL2NybC52ZXJpc2lnbi5jb20vcGNhMy5jcmwwCwYDVR0PBAQDAgEGMEIG\n" +
      "CCsGAQUFBwEBBDYwNDAyBggrBgEFBQcwAaYmFiRodHRwOi8vb2NzcC52ZXJp\n" +
      "c2lnbi5jb20vb2NzcC9zdGF0dXMwDQYJKoZIhvcNAQEFBQADgYEAgnBold+2\n" +
      "DcIBcBlK0lRWHqzyRUyHuPU163hLBanInTsZIS5wNEqi9YngFXVF5yg3ADQn\n" +
      "Keg3S/LvRJdrF1Eaw1adPBqK9kpGRjeM+sv1ZFo4aC4cw+9wzrhGBha/937n\n" +
      "tag+RaypJXUie28/sJyU58dzq6wf7iWbwBbtt8pb8BQ=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Visa_International_Global_Root_2.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDgDCCAmigAwIBAgICAx4wDQYJKoZIhvcNAQEFBQAwYTELMAkGA1UEBhMC\n" +
      "VVMxDTALBgNVBAoTBFZJU0ExLzAtBgNVBAsTJlZpc2EgSW50ZXJuYXRpb25h\n" +
      "bCBTZXJ2aWNlIEFzc29jaWF0aW9uMRIwEAYDVQQDEwlHUCBSb290IDIwHhcN\n" +
      "MDAwODE2MjI1MTAwWhcNMjAwODE1MjM1OTAwWjBhMQswCQYDVQQGEwJVUzEN\n" +
      "MAsGA1UEChMEVklTQTEvMC0GA1UECxMmVmlzYSBJbnRlcm5hdGlvbmFsIFNl\n" +
      "cnZpY2UgQXNzb2NpYXRpb24xEjAQBgNVBAMTCUdQIFJvb3QgMjCCASIwDQYJ\n" +
      "KoZIhvcNAQEBBQADggEPADCCAQoCggEBAKkBcLWqxEDwq2omYXkZAPy/mzdZ\n" +
      "DK9vZBv42pWUJGkzEXDK41Z0ohdXZFwgBuHW73G3O/erwWnQSaSxBNf0V2KJ\n" +
      "XLB1LRckaeNCYOTudNargFbYiCjh+20i/SN8RnNPflRzHqgsVVh1t0zzWkWl\n" +
      "Ahr62p3DRcMiXvOL8WAp0sdftAw6UYPvMPjU58fy+pmjIlC++QU3o63tmsPm\n" +
      "7IgbthknGziLgE3sucfFicv8GjLtI/C1AVj59o/ghalMCXI5Etuz9c9OYmTa\n" +
      "xhkVOmMd6RdVoUwiPDQyRvhlV7or7zaMavrZ2UT0qt2E1w0cslSsMoW0ZA3e\n" +
      "QbuxNMYBhjJk1Z8CAwEAAaNCMEAwHQYDVR0OBBYEFJ59SzS/ca3CBfYDdYDO\n" +
      "qU8axCRMMA8GA1UdEwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgEGMA0GCSqG\n" +
      "SIb3DQEBBQUAA4IBAQAhpXYUVfmtJ3CPPPTVbMjMCqujmAuKBiPFyWHbmQdp\n" +
      "NSYx/scuhMKZYdQN6X0uEyt8joW2hcdLzzW2LEc9zikv2G+fiRxkk78IvXbQ\n" +
      "kIqUs38oW26sTTMs7WXcFsziza6kPWKSBpUmv9+55CCmc2rBvveURNZNbyoL\n" +
      "axhNdBA2aGpawWqn3TYpjLgwi08hPwAuVDAHOrqK5MOeyti12HvOdUVmB/Rt\n" +
      "Ldh6yumJivIj2C/LbgA2T/vwLwHMD8AiZfSr4k5hLQOCfZEWtTDVFN5ex5D8\n" +
      "ofyrEK9ca3CnB+8phuiyJccg/ybdd+95RBTEvd07xQObdyPsoOy7Wjm1zK0G\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // Visa_eCommerce_Root.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIDojCCAoqgAwIBAgIQE4Y1TR0/BvLB+WUF1ZAcYjANBgkqhkiG9w0BAQUF\n" +
      "ADBrMQswCQYDVQQGEwJVUzENMAsGA1UEChMEVklTQTEvMC0GA1UECxMmVmlz\n" +
      "YSBJbnRlcm5hdGlvbmFsIFNlcnZpY2UgQXNzb2NpYXRpb24xHDAaBgNVBAMT\n" +
      "E1Zpc2EgZUNvbW1lcmNlIFJvb3QwHhcNMDIwNjI2MDIxODM2WhcNMjIwNjI0\n" +
      "MDAxNjEyWjBrMQswCQYDVQQGEwJVUzENMAsGA1UEChMEVklTQTEvMC0GA1UE\n" +
      "CxMmVmlzYSBJbnRlcm5hdGlvbmFsIFNlcnZpY2UgQXNzb2NpYXRpb24xHDAa\n" +
      "BgNVBAMTE1Zpc2EgZUNvbW1lcmNlIFJvb3QwggEiMA0GCSqGSIb3DQEBAQUA\n" +
      "A4IBDwAwggEKAoIBAQCvV95WHm6h2mCxlCfLF9sHP4CFT8icttD0b0/Pmdjh\n" +
      "28JIXDqsOTPHH2qLJj0rNfVIsZHBAk4ElpF7sDPwsRROEW+1QK8bRaVK7362\n" +
      "rPKgH1g/EkZgPI2h4H3PVz4zHvtH8aoVlwdVZqW1LS7YgFmypw23RuwhY/81\n" +
      "q6UCzyr0TP579ZRdhE2o8mCP2w4lPJ9zcc+U30rq299yOIzzlr3xF7zSujtF\n" +
      "Wsan9sYXiwGd/BmoKoMWuDpI/k4+oKsGGelT84ATB+0tvz8KPFUgOSwsAGl0\n" +
      "lUq8ILKpeeUYiZGo3BxN77t+Nwtd/jmliFKMAGzsGHxBvfaLdXe6YJ2E5/4t\n" +
      "AgMBAAGjQjBAMA8GA1UdEwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgEGMB0G\n" +
      "A1UdDgQWBBQVOIMPPyw/cDMezUb+B4wg4NfDtzANBgkqhkiG9w0BAQUFAAOC\n" +
      "AQEAX/FBfXxcCLkr4NWSR/pnXKUTwwMhmytMiUbPWU3J/qVAtmPN3XEolWcR\n" +
      "zCSs00Rsca4BIGsDoo8Ytyk6feUWYFN4PMCvFYP3j1IzJL1kk5fui/fbGKht\n" +
      "cbP3LBfQdCVp9/5rPJS+TUtBjE7ic9DjkCJzQ83z7+pzzkWKsKZJ/0x9nXGI\n" +
      "xHYdkFsd7v3M9+79YKWxehZx0RbQfBI8bGmX265fOZpwLwU8GUYEmSA20GBu\n" +
      "YQa7FkKMcPcw++DbZqMAAb3mLNqRX6BGi01qnD093QVG/na/oAo85ADmJ7f/\n" +
      "hC3euiInlhBx6yLt398znM/jra6O1I7mT1GvFpLgXPYHDw==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // beTRUSTed_Root_CA-Baltimore_Implementation.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIFajCCBFKgAwIBAgIEPLU9RjANBgkqhkiG9w0BAQUFADBmMRIwEAYDVQQK\n" +
      "EwliZVRSVVNUZWQxGzAZBgNVBAsTEmJlVFJVU1RlZCBSb290IENBczEzMDEG\n" +
      "A1UEAxMqYmVUUlVTVGVkIFJvb3QgQ0EtQmFsdGltb3JlIEltcGxlbWVudGF0\n" +
      "aW9uMB4XDTAyMDQxMTA3Mzg1MVoXDTIyMDQxMTA3Mzg1MVowZjESMBAGA1UE\n" +
      "ChMJYmVUUlVTVGVkMRswGQYDVQQLExJiZVRSVVNUZWQgUm9vdCBDQXMxMzAx\n" +
      "BgNVBAMTKmJlVFJVU1RlZCBSb290IENBLUJhbHRpbW9yZSBJbXBsZW1lbnRh\n" +
      "dGlvbjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALx+xDmcjOPW\n" +
      "HIb/ymKt4H8wRXqOGrO4x/nRNv8i805qX4QQ+2aBw5R5MdKR4XeOGCrDFN5R\n" +
      "9U+jK7wYFuK13XneIviCfsuBH/0nLI/6l2Qijvj/YaOcGx6Sj8CoCd8JEey3\n" +
      "fTGaGuqDIQY8n7pc/5TqarjDa1U0Tz0yH92BFODEPM2dMPgwqZfT7syj0B9f\n" +
      "HBOB1BirlNFjw55/NZKeX0Tq7PQiXLfoPX2k+YmpkbIq2eszh+6l/ePazIjm\n" +
      "iSZuxyuC0F6dWdsU7JGDBcNeDsYq0ATdcT0gTlgn/FP7eHgZFLL8kFKJOGJg\n" +
      "B7Sg7KxrUNb9uShr71ItOrL/8QFArDcCAwEAAaOCAh4wggIaMA8GA1UdEwEB\n" +
      "/wQFMAMBAf8wggG1BgNVHSAEggGsMIIBqDCCAaQGDysGAQQBsT4AAAEJKIOR\n" +
      "MTCCAY8wggFIBggrBgEFBQcCAjCCAToaggE2UmVsaWFuY2Ugb24gb3IgdXNl\n" +
      "IG9mIHRoaXMgQ2VydGlmaWNhdGUgY3JlYXRlcyBhbiBhY2tub3dsZWRnbWVu\n" +
      "dCBhbmQgYWNjZXB0YW5jZSBvZiB0aGUgdGhlbiBhcHBsaWNhYmxlIHN0YW5k\n" +
      "YXJkIHRlcm1zIGFuZCBjb25kaXRpb25zIG9mIHVzZSwgdGhlIENlcnRpZmlj\n" +
      "YXRpb24gUHJhY3RpY2UgU3RhdGVtZW50IGFuZCB0aGUgUmVseWluZyBQYXJ0\n" +
      "eSBBZ3JlZW1lbnQsIHdoaWNoIGNhbiBiZSBmb3VuZCBhdCB0aGUgYmVUUlVT\n" +
      "VGVkIHdlYiBzaXRlLCBodHRwOi8vd3d3LmJldHJ1c3RlZC5jb20vcHJvZHVj\n" +
      "dHNfc2VydmljZXMvaW5kZXguaHRtbDBBBggrBgEFBQcCARY1aHR0cDovL3d3\n" +
      "dy5iZXRydXN0ZWQuY29tL3Byb2R1Y3RzX3NlcnZpY2VzL2luZGV4Lmh0bWww\n" +
      "HQYDVR0OBBYEFEU9w6nR3D8kVpgccxiIav+DR+22MB8GA1UdIwQYMBaAFEU9\n" +
      "w6nR3D8kVpgccxiIav+DR+22MA4GA1UdDwEB/wQEAwIBBjANBgkqhkiG9w0B\n" +
      "AQUFAAOCAQEASZK8o+6svfoNyYt5hhwjdrCAWXf82n+0S9/DZEtqTg6t8n1Z\n" +
      "dwWtColzsPq8y9yNAIiPpqCy6qxSJ7+hSHyXEHu67RMdmgduyzFiEuhjA6p9\n" +
      "beP4G3YheBufS0OM00mG9htc9i5gFdPp43t1P9ACg9AYgkHNZTfqjjJ+vWuZ\n" +
      "XTARyNtIVBw74acT02pIk/c9jH8F6M7ziCpjBLjqflh8AXtb4cV97yHgjQ5d\n" +
      "UX2xZ/2jvTg2xvI4hocalmhgRvsoFEdV4aeADGvi6t9NfJBIoDa9CReJf8Py\n" +
      "05yc493EG931t3GzUwWJBtDLSoDByFOQtTwxiBdQn8nEDovYqAJjDQ==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // beTRUSTed_Root_CA.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIFLDCCBBSgAwIBAgIEOU99hzANBgkqhkiG9w0BAQUFADBaMQswCQYDVQQG\n" +
      "EwJXVzESMBAGA1UEChMJYmVUUlVTVGVkMRswGQYDVQQDExJiZVRSVVNUZWQg\n" +
      "Um9vdCBDQXMxGjAYBgNVBAMTEWJlVFJVU1RlZCBSb290IENBMB4XDTAwMDYy\n" +
      "MDE0MjEwNFoXDTEwMDYyMDEzMjEwNFowWjELMAkGA1UEBhMCV1cxEjAQBgNV\n" +
      "BAoTCWJlVFJVU1RlZDEbMBkGA1UEAxMSYmVUUlVTVGVkIFJvb3QgQ0FzMRow\n" +
      "GAYDVQQDExFiZVRSVVNUZWQgUm9vdCBDQTCCASIwDQYJKoZIhvcNAQEBBQAD\n" +
      "ggEPADCCAQoCggEBANS0c3oTCjhVAb6JVuGUntS+WutKNHUbYSnE4a0IYCF4\n" +
      "SP+00PpeQY1hRIfo7clY+vyTmt9P6j41ffgzeubx181vSUs9Ty1uDoM6GHh3\n" +
      "o8/n9E1z2Jo7Gh2+lVPPIJfCzz4kUmwMjmVZxXH/YgmPqsWPzGCgc0rXOD8V\n" +
      "cr+il7dw6K/ifhYGTPWqZCZyByWtNfwYsSbX2P8ZDoMbjNx4RWc0PfSvHI3k\n" +
      "bWvtILNnmrRhyxdviTX/507AMhLn7uzf/5cwdO2NR47rtMNE5qdMf1ZD6Li8\n" +
      "tr76g5fmu/vEtpO+GRg+jIG5c4gW9JZDnGdzF5DYCW5jrEq2I8QBoa2k5MUC\n" +
      "AwEAAaOCAfgwggH0MA8GA1UdEwEB/wQFMAMBAf8wggFZBgNVHSAEggFQMIIB\n" +
      "TDCCAUgGCisGAQQBsT4BAAAwggE4MIIBAQYIKwYBBQUHAgIwgfQagfFSZWxp\n" +
      "YW5jZSBvbiB0aGlzIGNlcnRpZmljYXRlIGJ5IGFueSBwYXJ0eSBhc3N1bWVz\n" +
      "IGFjY2VwdGFuY2Ugb2YgdGhlIHRoZW4gYXBwbGljYWJsZSBzdGFuZGFyZCB0\n" +
      "ZXJtcyBhbmQgY29uZGl0aW9ucyBvZiB1c2UsIGFuZCBjZXJ0aWZpY2F0aW9u\n" +
      "IHByYWN0aWNlIHN0YXRlbWVudCwgd2hpY2ggY2FuIGJlIGZvdW5kIGF0IGJl\n" +
      "VFJVU1RlZCdzIHdlYiBzaXRlLCBodHRwczovL3d3dy5iZVRSVVNUZWQuY29t\n" +
      "L3ZhdWx0L3Rlcm1zMDEGCCsGAQUFBwIBFiVodHRwczovL3d3dy5iZVRSVVNU\n" +
      "ZWQuY29tL3ZhdWx0L3Rlcm1zMDQGA1UdHwQtMCswKaAnoCWkIzAhMRIwEAYD\n" +
      "VQQKEwliZVRSVVNUZWQxCzAJBgNVBAYTAldXMB0GA1UdDgQWBBQquZtpLjub\n" +
      "2M3eKjEENGvKBxirZzAfBgNVHSMEGDAWgBQquZtpLjub2M3eKjEENGvKBxir\n" +
      "ZzAOBgNVHQ8BAf8EBAMCAf4wDQYJKoZIhvcNAQEFBQADggEBAHlh26Nebhax\n" +
      "6nZR+csVm8tpvuaBa58oH2U+3RGFktToQb9+M70j5/Egv6S0phkBxoyNNXxl\n" +
      "pE8JpNbYIxUFE6dDea/bow6be3ga8wSGWsb2jCBHOElQBp1yZzrwmAOtlmdE\n" +
      "/D8QDYZN5AA7KXvOOzuZhmElQITcE2K3+spZ1gMe1lMBzW1MaFVA4e5rxyoA\n" +
      "AEiCswoBw2AqDPeCNe5IhpbkdNQ96gFxugR1QKepfzk5mlWXKWWuGVUlBXJH\n" +
      "0+gY3Ljpr0NzARJ0o+FcXxVdJPP55PS2Z2cS52QiivalQaYctmBjRYoQtLpG\n" +
      "EK5BV2VsPyMQPyEQWbfkQN0mDCP2qq4=\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // beTRUSTed_Root_CA_-_Entrust_Implementation.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIGUTCCBTmgAwIBAgIEPLVPQDANBgkqhkiG9w0BAQUFADBmMRIwEAYDVQQK\n" +
      "EwliZVRSVVNUZWQxGzAZBgNVBAsTEmJlVFJVU1RlZCBSb290IENBczEzMDEG\n" +
      "A1UEAxMqYmVUUlVTVGVkIFJvb3QgQ0EgLSBFbnRydXN0IEltcGxlbWVudGF0\n" +
      "aW9uMB4XDTAyMDQxMTA4MjQyN1oXDTIyMDQxMTA4NTQyN1owZjESMBAGA1UE\n" +
      "ChMJYmVUUlVTVGVkMRswGQYDVQQLExJiZVRSVVNUZWQgUm9vdCBDQXMxMzAx\n" +
      "BgNVBAMTKmJlVFJVU1RlZCBSb290IENBIC0gRW50cnVzdCBJbXBsZW1lbnRh\n" +
      "dGlvbjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALr0RAOqEmq1\n" +
      "Q+xVkrYwfTVXDNvzDSduTPdQqJtOK2/b9a0cS12zqcH+e0TrW6MFDR/FNCsw\n" +
      "ACnxeECypP869AGIF37m1CbTukzqMvtDd5eHI8XbQ6P1KqNRXuE70mVpflUV\n" +
      "m3rnafdE4Fe1FehmYA8NA/uCjqPoEXtsvsdjDheT389Lrm5zdeDzqrmkwAkb\n" +
      "hepxKYhBMvnwKg5sCfJ0a2ZsUhMfGLzUPvfYbiCeyv78IZTuEyhL11xeDGbu\n" +
      "6bsPwTSxfwh28z0mcMmLJR1iJAzqHHVOwBLkuhMdMCktVjMFu5dZfsZJT4nX\n" +
      "LySotohAtWSSU1Yk5KKghbNekLQSM80CAwEAAaOCAwUwggMBMIIBtwYDVR0g\n" +
      "BIIBrjCCAaowggGmBg8rBgEEAbE+AAACCSiDkTEwggGRMIIBSQYIKwYBBQUH\n" +
      "AgIwggE7GoIBN1JlbGlhbmNlIG9uIG9yIHVzZSBvZiB0aGlzIENlcnRpZmlj\n" +
      "YXRlIGNyZWF0ZXMgYW4gYWNrbm93bGVkZ21lbnQgYW5kIGFjY2VwdGFuY2Ug\n" +
      "b2YgdGhlIHRoZW4gYXBwbGljYWJsZSBzdGFuZGFyZCB0ZXJtcyBhbmQgY29u\n" +
      "ZGl0aW9ucyBvZiB1c2UsIHRoZSBDZXJ0aWZpY2F0aW9uIFByYWN0aWNlIFN0\n" +
      "YXRlbWVudCBhbmQgdGhlIFJlbHlpbmcgUGFydHkgQWdyZWVtZW50LCB3aGlj\n" +
      "aCBjYW4gYmUgZm91bmQgYXQgdGhlIGJlVFJVU1RlZCB3ZWIgc2l0ZSwgaHR0\n" +
      "cHM6Ly93d3cuYmV0cnVzdGVkLmNvbS9wcm9kdWN0c19zZXJ2aWNlcy9pbmRl\n" +
      "eC5odG1sMEIGCCsGAQUFBwIBFjZodHRwczovL3d3dy5iZXRydXN0ZWQuY29t\n" +
      "L3Byb2R1Y3RzX3NlcnZpY2VzL2luZGV4Lmh0bWwwEQYJYIZIAYb4QgEBBAQD\n" +
      "AgAHMIGJBgNVHR8EgYEwfzB9oHugeaR3MHUxEjAQBgNVBAoTCWJlVFJVU1Rl\n" +
      "ZDEbMBkGA1UECxMSYmVUUlVTVGVkIFJvb3QgQ0FzMTMwMQYDVQQDEypiZVRS\n" +
      "VVNUZWQgUm9vdCBDQSAtIEVudHJ1c3QgSW1wbGVtZW50YXRpb24xDTALBgNV\n" +
      "BAMTBENSTDEwKwYDVR0QBCQwIoAPMjAwMjA0MTEwODI0MjdagQ8yMDIyMDQx\n" +
      "MTA4NTQyN1owCwYDVR0PBAQDAgEGMB8GA1UdIwQYMBaAFH1w5a44iwY/qhwa\n" +
      "j/nPJDCqhIQWMB0GA1UdDgQWBBR9cOWuOIsGP6ocGo/5zyQwqoSEFjAMBgNV\n" +
      "HRMEBTADAQH/MB0GCSqGSIb2fQdBAAQQMA4bCFY2LjA6NC4wAwIEkDANBgkq\n" +
      "hkiG9w0BAQUFAAOCAQEAKrgXzh8QlOu4mre5X+za95IkrNySO8cgjfKZ5V04\n" +
      "ocI07cUTWVwFtStPYZuR+0H8/NU8TZh2BvWBfevdkObRVlTa4y0MnxEylCIB\n" +
      "evZsLHRnBMylj44ss0O1lKLQfelifwa+JwGDnjr9iu6YQ0pr17WXOzq/T220\n" +
      "Y/ozADQuLW2WyXvKmWO6vvT2MKAtmJbpVkQFqUSjYRDrgqFnXbxdJ3Wqiig2\n" +
      "KjiS2d2kXgClzMx8KSreKJCrt+G2/30lC0DYqjSjLd4H61/OCt3Kfjp9JsFi\n" +
      "aDrmLzfzgYYhxKlkqu9FNtEaZnz46TfW1mG+oq1I59/mdP7TbX3SJdysYlep\n" +
      "9w==\n" +
      "-----END CERTIFICATE-----\n");
    if (cert != null) certs.add(cert);

    cert = generate(factory,
      // beTRUSTed_Root_CA_-_RSA_Implementation.crt
      "-----BEGIN CERTIFICATE-----\n" +
      "MIIFaDCCBFCgAwIBAgIQO1nHe81bV569N1KsdrSqGjANBgkqhkiG9w0BAQUF\n" +
      "ADBiMRIwEAYDVQQKEwliZVRSVVNUZWQxGzAZBgNVBAsTEmJlVFJVU1RlZCBS\n" +
      "b290IENBczEvMC0GA1UEAxMmYmVUUlVTVGVkIFJvb3QgQ0EgLSBSU0EgSW1w\n" +
      "bGVtZW50YXRpb24wHhcNMDIwNDExMTExODEzWhcNMjIwNDEyMTEwNzI1WjBi\n" +
      "MRIwEAYDVQQKEwliZVRSVVNUZWQxGzAZBgNVBAsTEmJlVFJVU1RlZCBSb290\n" +
      "IENBczEvMC0GA1UEAxMmYmVUUlVTVGVkIFJvb3QgQ0EgLSBSU0EgSW1wbGVt\n" +
      "ZW50YXRpb24wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDkujQw\n" +
      "CY5X0LkGLG9uJIAiv11DpvpPrILnHGhwhRujbrWqeNluB0s/6d/16uhUoWGK\n" +
      "Di9pdRi3DOUUjXFumLhV/AyV0Jtu4S2I1DpAa5LxmZZk3tv/ePTulh1HiXzU\n" +
      "vrmIdyM6CeYEnm2qXtLIvZpOGd+J6lsOfsPktPDgaTuID0GQ+NRxQyTBjyZL\n" +
      "O1bp/4xsN+lFrYWMU8NghpBKlsmzVLC7F/AcRdnUGxlkVgoZ98zh/4avflhe\n" +
      "rHqQH8koOUV7orbHnB/ahdQhhlkwk75TMzf270HPM8ercmsl9fNTGwxMLvF1\n" +
      "S++gh/f+ihXQbNXL+WhTuXAVE8L1LvtDNXUtAgMBAAGjggIYMIICFDAMBgNV\n" +
      "HRMEBTADAQH/MIIBtQYDVR0gBIIBrDCCAagwggGkBg8rBgEEAbE+AAADCSiD\n" +
      "kTEwggGPMEEGCCsGAQUFBwIBFjVodHRwOi8vd3d3LmJldHJ1c3RlZC5jb20v\n" +
      "cHJvZHVjdHNfc2VydmljZXMvaW5kZXguaHRtbDCCAUgGCCsGAQUFBwICMIIB\n" +
      "OhqCATZSZWxpYW5jZSBvbiBvciB1c2Ugb2YgdGhpcyBDZXJ0aWZpY2F0ZSBj\n" +
      "cmVhdGVzIGFuIGFja25vd2xlZGdtZW50IGFuZCBhY2NlcHRhbmNlIG9mIHRo\n" +
      "ZSB0aGVuIGFwcGxpY2FibGUgc3RhbmRhcmQgdGVybXMgYW5kIGNvbmRpdGlv\n" +
      "bnMgb2YgdXNlLCB0aGUgQ2VydGlmaWNhdGlvbiBQcmFjdGljZSBTdGF0ZW1l\n" +
      "bnQgYW5kIHRoZSBSZWx5aW5nIFBhcnR5IEFncmVlbWVudCwgd2hpY2ggY2Fu\n" +
      "IGJlIGZvdW5kIGF0IHRoZSBiZVRSVVNUZWQgd2ViIHNpdGUsIGh0dHA6Ly93\n" +
      "d3cuYmV0cnVzdGVkLmNvbS9wcm9kdWN0c19zZXJ2aWNlcy9pbmRleC5odG1s\n" +
      "MAsGA1UdDwQEAwIBBjAfBgNVHSMEGDAWgBSp7BR++dlDzFMrFK3P9/BZiUHN\n" +
      "GTAdBgNVHQ4EFgQUqewUfvnZQ8xTKxStz/fwWYlBzRkwDQYJKoZIhvcNAQEF\n" +
      "BQADggEBANuXsHXqDMTBmMpWBcCorSZIry0g6IHHtt9DwSwddUvUQo3neqh0\n" +
      "3GZCWYez9Wlt2ames30cMcH1VOJZJEnl7r05pmuKmET7m9cqg5c0Lcd9NUwt\n" +
      "NLg+DcTsiCevnpL9UGGCqGAHFFPMZRPB9kdEadIxyKbdLrML3kqNWz2rDcI1\n" +
      "UqJWN8wyiyiFQpyRQHpwKzg21eFzGh/l+n5f3NacOzDq28BbJ1zTcwfBwvNM\n" +
      "m2+fG8oeqqg4MwlYsq78B+g23FW6L09A/nq9BqaBwZMifIYRCgZ3SK41ty8y\n" +
      "mmFei74pnykkiFY5LKjSq5YDWtRIn7lAhAuYaPsBQ9Yb4gmxlxw=\n" +
      "-----END CERTIFICATE-----\n");

    CA_CERTS = new StaticTrustAnchors((X509Certificate[]) certs.toArray(new X509Certificate[0]));
  }
}
