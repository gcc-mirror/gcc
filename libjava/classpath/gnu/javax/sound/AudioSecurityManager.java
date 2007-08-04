package gnu.javax.sound;

import javax.sound.sampled.AudioPermission;

public class AudioSecurityManager
{
  public static enum Permission
  {
    PLAY, RECORD, ALL
  }
  
  public static final void checkPermissions()
  {
    checkPermissions(Permission.ALL);
  }
  
  public static final void checkPermissions(Permission permission)
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      {
        String perm = null;
        switch (permission)
          {
          case PLAY:
            perm = "play";
            break;
            
          case RECORD:
            perm = "record";
            break;
            
          case ALL: default:
            perm = "*";
            break;
          }
        
        sm.checkPermission(new AudioPermission(perm));
      }
  }
}
