package java.nio.channels.spi;

public abstract class AbstractSelectableChannel
{
  public final boolean isBlocking()
  {
    return true;
  }
}

