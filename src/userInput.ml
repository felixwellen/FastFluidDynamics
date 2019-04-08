
type user_fan_state =
  | Nothing_happening
  | Arrow_starting_at of int * int
;;

let setting = object
    val mutable vector_field_switch = true;
    val mutable div_switch = true;
    val mutable run_simulation_switch = true;
    val mutable left_mouse_clicks : (int * int) list = [];
    val mutable fan_state = Nothing_happening;
    val mutable forces_to_add : ((int * int) * (int * int)) list = [];
    val mutable last_mouse_pos = (0,0);
                                                                   
    method pause_unpause = run_simulation_switch <- not run_simulation_switch
    method switch_vector_field_visibility = vector_field_switch <- not vector_field_switch
    method switch_div = div_switch <- not div_switch

    method visibility_vector_field = vector_field_switch
    method run_simulation = run_simulation_switch
    method show_div = div_switch

    method set_last_mouse_pos p = last_mouse_pos <- p
      
    method get_fan_state = fan_state
    method get_last_mouse_pos = last_mouse_pos
                    
    method left_mouse_click p =
      match p with
      | (x , y) -> left_mouse_clicks <- (x , y) :: left_mouse_clicks

    method right_mouse_click (x,y) =
      Printf.printf "%d %d \n%!" x y;
      match fan_state with
      | Nothing_happening ->
         fan_state <- Arrow_starting_at (x,y);
      | Arrow_starting_at (x0,y0) ->
         fan_state <- Nothing_happening;
         forces_to_add <- ((x0,y0),(x-x0,y-y0)) :: forces_to_add
                      
    method handle_left_mouse_clicks f =
      List.iter f left_mouse_clicks;
      left_mouse_clicks <- []
      
    method apply_to_user_fans f =
      List.iter f forces_to_add;
      left_mouse_clicks <- []
end;;
