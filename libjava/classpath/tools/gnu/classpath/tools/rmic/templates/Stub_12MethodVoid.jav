  /** @inheritDoc */
  public void #method_name(#argument_list) #throws
  {
    try
      {
        ref.invoke(this, met_#method_name,
          #object_arg_list,
          #method_hash);
      }
    catch (RuntimeException e)
      {
        throw e;
      }
    catch (RemoteException e)
      {
        throw e;
      }
    catch (Exception e)
      {
        UnexpectedException uex = new UnexpectedException(exception_message);
        uex.detail = e;
        throw uex;
      }
  }
   