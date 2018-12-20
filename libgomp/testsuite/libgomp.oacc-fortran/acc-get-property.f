! Test the `acc_get_property' and '`acc_get_property_string' library
! functions.
! { dg-do run }

      USE OPENACC
      IMPLICIT NONE

      INTEGER(ACC_DEVICE_PROPERTY) V
      CHARACTER*256 S
      LOGICAL R

      ! Verify that the vendor is a non-empty string.
      CALL ACC_GET_PROPERTY_STRING (0, ACC_DEVICE_DEFAULT,
     +                              ACC_PROPERTY_VENDOR, S)
      R = S /= ""
      IF (S /= "") PRINT "(A, A)", "OpenACC vendor: ", TRIM (S)

      ! For the rest just check that they do not crash.
      V = ACC_GET_PROPERTY (0, ACC_DEVICE_DEFAULT,
     +                      ACC_PROPERTY_MEMORY)
      IF (V /= 0) PRINT "(A, I0)", "OpenACC total memory: ", V
      V = ACC_GET_PROPERTY (0, ACC_DEVICE_DEFAULT,
     +                      ACC_PROPERTY_FREE_MEMORY)
      IF (V /= 0) PRINT "(A, I0)", "OpenACC free memory: ", V
      CALL ACC_GET_PROPERTY_STRING (0, ACC_DEVICE_DEFAULT,
     +                              ACC_PROPERTY_NAME, S)
      IF (S /= "") PRINT "(A, A)", "OpenACC name: ", TRIM (S)
      CALL ACC_GET_PROPERTY_STRING (0, ACC_DEVICE_DEFAULT,
     +                              ACC_PROPERTY_DRIVER, S)
      IF (S /= "") PRINT "(A, A)", "OpenACC driver: ", TRIM (S)

      IF (.NOT. R) STOP 1
      END
