
let setting = object
    val mutable show_vector_field = false;
    val mutable left_mouse_clicks : (int * int) list = [];

    method switch_vector_field_visibility =
      match show_vector_field with
      | true -> show_vector_field <- false
      | false -> show_vector_field <- true

    method visibility_vector_field = show_vector_field

    method left_mouse_click p =
      match p with
      | (x , y) -> left_mouse_clicks <- (x , y) :: left_mouse_clicks

    method handle_left_mouse_clicks f =
      List.iter f left_mouse_clicks;
      left_mouse_clicks <- []
end;;
