(* Fun experiments without meaning *)

module Config = struct
  let width = 256
  let height = 256
  let diff = 1.0
  let visc = 100.0
end;;

let windowWidth = 800;;
let windowHeight = 800;;
let dt = 0.01;;

module Data = Grid.DataGrid(Config);;

let to_model_coord x y =
  (int_of_float(float(x) *. float Config.width /. float windowWidth)
  , Config.height - int_of_float(float y *. float Config.height /. float windowHeight));;

let model_coord_to_opengl_coord x y =
  (float x /. float Config.width *. 2.0 -. 1.0 , float y /. float Config.height *. 2.0 -. 1.0)

let make_image () =
  let image =
    GlPix.create `ubyte ~format:`rgb ~width:Config.width ~height:Config.height in
  for i = 0 to Config.width - 1 do
    for j = 0 to Config.height - 1 do
      Raw.sets (GlPix.to_raw image) ~pos:(3*(j*Config.width+i))
        (if (Data.input#show_div) then (Data.color_with_div i j) else (Data.color i j));
    done;
  done;
  image

let myinit () =
  GlClear.color (0.0, 0.0, 0.0);
  Gl.enable `depth_test;
  GlFunc.depth_func `less;
  let image = make_image () in
  GlPix.store (`unpack_alignment 1);
  GlTex.image2d image;
  List.iter (GlTex.parameter ~target:`texture_2d)
    [ `wrap_s `clamp;
      `wrap_t `clamp;
      `mag_filter `nearest;
      `min_filter `nearest ];
  GlTex.env (`mode `decal);
  Gl.enable `texture_2d;
  GlDraw.shade_model `flat

let draw_arrow x y dx dy c thickness eps =
  (*     
              |\
 t| +---------+ \
    | o       |s >
    +---------+ /
              |/
          
   *)
  if Math.length2d (dx , dy) > eps then
    let o = (x , y) in
    let s = Math.add2d o (dx, dy) in
    let d = thickness /. 2.0 in
    let t = Math.scale_to d (Math.perpendicular (dx , dy)) in
    let t' = Math.perpendicular t in
    let t'' = Math.perpendicular t' in
    let (u , v) = (Math.add2d t' t , Math.add2d t' t'') in
    GlDraw.color c;
    GlDraw.begins `quads;
    List.iter (fun (x , y) -> GlDraw.vertex3(x, y, 0.01))
      [ Math.add2d o u ; Math.add2d o v ; Math.add2d s v ; Math.add2d s u ];
    GlDraw.ends ();
    let (u , v , w) = (Math.add2d u t , Math.add2d v t'' , Math.scale2d (-3.2) t') in
    GlDraw.begins `triangles;
    List.iter (fun (x , y) -> GlDraw.vertex3(x, y, 0.01))
      [ Math.add2d s u ; Math.add2d s v ; Math.add2d s w ];
    GlDraw.ends ()

let  draw_capped_arrow x y dx dy cap =
  let l = 2.0 *. Math.length2d (dx , dy) in
  let (dx , dy) = if l > cap then (cap /. l *. dx , cap /. l *. dy) else (dx , dy) in
  let c = (0.5 *. l /. cap , max 0.1 (1.0 -. l /. cap) , max 0.1 (1.0 -. l /. cap)) in
  draw_arrow x y dx dy c 0.01 0.0001
  
let draw_velocity_field () =
  Gl.disable `texture_2d;
  for i = 0 to int_of_float (1.99 /. 0.05) do
    for j = 0 to int_of_float (1.99 /. 0.05) do
      let x , y = float i *. 0.05 -. 1.0 , float j *. 0.05 -. 1.0 in
      let (dx , dy) = Math.scale2d dt (Data.get_velocity_at (x , y)) in
      draw_capped_arrow x y dx dy 0.05;
    done;
  done

let possibly_draw_fan_arrow () = ()
                                   (*
  match Data.input#get_fan_state with
  | Nothing_happening -> ()
  | Arrow_starting_at (x,y) ->
     let (x', y') = Data.input#get_last_mouse_pos; in
     let dx , dy = model_coord_to_opengl_coord (x'-x) (y'-y) in
     let (x , y) = model_coord_to_opengl_coord x y in
     draw_capped_arrow x y dx dy 0.1
                                    *)     
let display () =
  GlClear.clear [`color;`depth];
  Gl.enable `texture_2d;
  GlDraw.begins `quads;
  GlTex.coord2(0.0, 0.0); GlDraw.vertex3(-1.0, -1.0, 0.0);
  GlTex.coord2(0.0, 1.0); GlDraw.vertex3(-1.0, 1.0, 0.0);
  GlTex.coord2(1.0, 1.0); GlDraw.vertex3(1.0, 1.0, 0.0);
  GlTex.coord2(1.0, 0.0); GlDraw.vertex3(1.0, -1.0, 0.0);
  GlDraw.ends ();
  if Data.input#visibility_vector_field then
    draw_velocity_field ();
  possibly_draw_fan_arrow ();
  Gl.flush ()
  
let move_forward_in_time () =
  if Data.input#run_simulation then
    Data.perform_time_step dt;
  myinit ();
  display ()

let reshape ~w ~h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:60.0 ~aspect:(1.0 *. float w /. float h) ~z:(1.0,30.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlMat.translate ~z:(-1.7342) ()
  
(* mouse button event *)
let mouse ~button ~state ~x ~y =
  match button, state with
  | Glut.LEFT_BUTTON, Glut.DOWN ->
     Data.input#left_mouse_click (to_model_coord x y);
  | Glut.RIGHT_BUTTON, Glut.DOWN ->
     Data.input#right_mouse_click (to_model_coord x y);
  | _ -> ()
;;

let _keyboard_callback ~key ~x ~y = 
  match (char_of_int key) with
  | 'q' -> exit 0;
  | 'v' -> Data.input#switch_vector_field_visibility;
  | 's' -> Data.input#pause_unpause;
  | 'd' -> Data.input#switch_div;
  | _ -> ()
       
(* active mouse motion *)
let mouse_motion ~x ~y =
  Data.input#set_last_mouse_pos (to_model_coord x y)
    
let main () =
  ignore(Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true () ;
  Glut.initWindowSize ~w:windowWidth ~h:windowHeight ;
  ignore(Glut.createWindow ~title:"stokes");
  myinit ();
  Glut.mouseFunc ~cb:mouse;
  Glut.motionFunc ~cb:mouse_motion;
  Glut.keyboardFunc ~cb:_keyboard_callback;
  Glut.displayFunc ~cb:display;
  Glut.reshapeFunc ~cb:reshape;
  Glut.idleFunc ~cb:(Some move_forward_in_time);
  Glut.mainLoop ()

let _ = main ()

