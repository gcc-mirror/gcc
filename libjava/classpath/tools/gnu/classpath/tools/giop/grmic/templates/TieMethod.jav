          #hashCodeLabel
            // #method_name
            if (method.equals("#giop_method_name"))
              {
#read_and_define_args
                OutputStream out = reply.createReply();
                #return_type result = 
                  target.#method_name(#argument_names);
                #write_result                  
                return out;
              }
