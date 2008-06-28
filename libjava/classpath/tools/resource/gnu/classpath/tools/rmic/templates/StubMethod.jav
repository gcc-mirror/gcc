  /** @inheritDoc */
  public #return_type #method_name(#argument_list) #throws
  {
    try
      {
        InputStream in = null;
        try
          {
             OutputStream out = 
               (OutputStream) _request("#giop_method_name", true);
#argument_write                
             in = _invoke(out);
             #read_return
          }
        catch (ApplicationException ex)
          {
             in = ex.getInputStream();
             throw new UnexpectedException(in.read_string());
          }
        catch (RemarshalException ex)
          {
             return #method_name(#argument_names);
          }
        finally
          {
             _releaseReply(in);
          }
        }
    catch (SystemException ex)
      {
        throw Util.mapSystemException(ex);
      }
   }
