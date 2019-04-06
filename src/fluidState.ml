
open Printf
   
let setting = object
    val mutable show_vector_field = false;
                                    
    method switch_vector_field_visibility =
      printf "state changed\n%!";
      match show_vector_field with
      | true -> show_vector_field <- false
      | false -> show_vector_field <- true

    method visibility_vector_field = show_vector_field
end;;
