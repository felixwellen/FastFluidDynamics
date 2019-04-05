(* Fun experiments without meaning *)

module Config = struct
  let width = 512
  let height = 512
  let diff = 1.0
end;;

module Data = Grid.DataGrid(Config);;

let make_image () =
  let image =
    GlPix.create `ubyte ~format:`rgb ~width:Config.width ~height:Config.height in
  for i = 0 to Config.width - 1 do
    for j = 0 to Config.height - 1 do
      Raw.sets (GlPix.to_raw image) ~pos:(3*(j*Config.width+i)) (Data.color i j) 
    done
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

let display () =
  GlClear.clear [`color;`depth];
  GlDraw.begins `quads;
  GlTex.coord2(0.0, 0.0); GlDraw.vertex3(-1.0, -1.0, 0.0);
  GlTex.coord2(0.0, 1.0); GlDraw.vertex3(-1.0, 1.0, 0.0);
  GlTex.coord2(1.0, 1.0); GlDraw.vertex3(1.0, 1.0, 0.0);
  GlTex.coord2(1.0, 0.0); GlDraw.vertex3(1.0, -1.0, 0.0);
  GlDraw.ends ();
  Gl.flush ()

let move_forward_in_time () =
  Data.perform_time_step 0.001;
  myinit ();
  display ()

let reshape ~w ~h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:60.0 ~aspect:(1.0 *. float w /. float h) ~z:(1.0,30.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlMat.translate ~z:(-2.0) ()

let main () =
  ignore(Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true () ;
  Glut.initWindowSize ~w:800 ~h:800 ;
  ignore(Glut.createWindow ~title:"checker");
  myinit ();
  Glut.displayFunc ~cb:display;
  Glut.reshapeFunc ~cb:reshape;
  Glut.idleFunc ~cb:(Some move_forward_in_time);
  Glut.mainLoop ()

let _ = main ()

