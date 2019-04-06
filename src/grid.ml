
open Printf

module type Config = sig
  val width : int
  val height : int
  val diff : float
  val visc : float
end;;

(*

height
  ^
  |
 j|
  +----->  width
     i
 *)

module DataGrid =
  functor (C : Config) ->
  struct
    exception Invalid_grid_coord of int * int 
    let to_index i j =
      if (i > C.width-1 || i < 0 || j < 0 || j > C.height-1)
      then raise (Invalid_grid_coord (i,j))
      else j * C.width + i
    let index_bound = C.height * C.width
    let index_i index = index mod C.width
    let index_j index = index / C.width

    let write_to_array f arr =
      Array.iteri
        (fun index x ->
          arr.(index) <- f (index_i index) (index_j index))
        arr
        
    let make_new_array f =
      let new_arr = Array.create_float index_bound in
      write_to_array f new_arr;
      new_arr
      
    let copy_to arr1 arr2 =
      write_to_array
        (fun i j ->
          arr1.(to_index i j)
        )
        arr2

    let rec do_times k f = if (k > 0) then (f; do_times (k-1) f) else ()
                         
    let boundary i j = (i == 0) || (i >= C.width-1)
                       || (j == 0) || (j == C.height-1)
                     
    let cap d = max 0.0 (min d 1.0) 
                       
    let density =
      let value i j = if (i < 10 || i > C.width - 10)
                         || (j < 10 || j > C.height - 10)
                      then (Random.float 1.0) else 0.0
      in make_new_array value

    let buffer = make_new_array (fun i j -> 0.0)

    let null_buffer () = write_to_array (fun i j -> 0.0) buffer

    let velocity_x = make_new_array (fun i j ->
                         let ty = (float j /. float C.height) -. 0.5 in
                         (0.25 -. ty *. ty)
                       )
    let velocity_y = make_new_array (fun i j ->
                         let tx = float i /. float C.width in
                         let ty = float j /. float C.height in
                         (0.25 -. tx *. tx) 
                       )

    let force_x i j =
      let tx = float i /. float C.width in
      (* let ty = float j /. float C.height in *)
      0.01 *. (1.0 -. tx) *. (1.0 -. tx) *. (1.0 -. tx) 
      
    let force_y i j =
      (*      let tx = float i /. float C.width in *)
      let ty = float j /. float C.height in 
      0.01 *. (1.0 -. ty) *. (1.0 -. ty) *. (1.0 -. ty) 


    let den i j = density.(to_index i j)
    let buf i j = buffer.(to_index i j)
    let vel_x i j = velocity_x.(to_index i j)
    let vel_y i j = velocity_y.(to_index i j)
                  
    let color_density i j =
      let d = int_of_float (255.0 *. den i j) in
      [|d;d;d|]

    let color_velocity i j =
      let dx = int_of_float (128.0 *. vel_x i j +. 128.0) in
      let dy = int_of_float (128.0 *. vel_y i j +. 128.0) in
      [|dx;dy;0|]
      
    let cap_x v = min (max 0.5 v) (float C.width -. 2.5)
    let cap_y v = min (max 0.5 v) (float C.height -. 2.5)
      
    let set_boundary target src b = 
      for i = 1 to C.width - 2 do
        target.(to_index i 0) <-
          if b==2 then -. src i 1 else src i 1;
        target.(to_index i (C.height-1)) <-
          if b==2 then -. src i (C.height-2) else src i (C.height-2);
      done;
      for j = 1 to C.height -2 do
        target.(to_index 0 j) <-
          if b==1 then -. src 1 j else src 1 j;
        target.(to_index (C.width-1) j) <-
          if b==1 then -. src (C.width-2) j else src (C.width-2) j;
      done;
      target.(to_index 0 0) <- 0.5 *. (src 1 0 +. src 0 1);
      target.(to_index (C.width-1) 0) <- 0.5 *. (src (C.width-2) 0 +. src (C.width-1) 1);
      target.(to_index (C.width-1) (C.height-1)) <-
        0.5 *. (src (C.width-2) (C.height-1) +. src (C.width-1) (C.height-2));
      target.(to_index 0 (C.height-1)) <- 0.5 *. (src 1 (C.height-1) +. src 0 (C.height-2))
      
    let advect_to_buffer dt src force_x force_y b =
      let dt0 = dt*.float C.width in
      let update_function i j =
        if not (boundary i j)
        then let x = cap_x (float i -. dt0 *. force_x i j) in
             let y = cap_y (float j -. dt0 *. force_y i j) in
             let i0 = int_of_float x in
             let i1 = i0+1 in
             let j0 = int_of_float y in
             let j1 = j0+1 in
             let s1 = x -. float(i0) in
             let s0 = 1.0 -. s1 in
             let t1 = y -. float(j0) in
             let t0 = 1.0 -. t1 in
             s0 *. (t0 *. src i0 j0 +. t1 *. src i0 j1) +.
             s1 *. (t0 *. src i1 j0 +. t1 *. src i1 j1)
        else 0.0
      in
      write_to_array update_function buffer;
      set_boundary buffer src b

    let diffuse_in_buffer dt flow src b =
      null_buffer ();
      let a = flow *. dt *. float_of_int (C.width * C.height) in
      let update i j =
        if not (boundary i j)
        then (src i j
              +. a*.(src (i-1) j
                     +. src (i+1) j
                     +. src i (j-1)
                     +. src i (j+1)))
             /. (1.0 +. 4.0 *. a)
        else src i j
      in
      do_times 20
        (write_to_array update buffer; set_boundary buffer src b)
      
    let project dt =
      null_buffer ();
      let h = 1.0 /. float(C.width) in
      let div i j =
        if boundary i j
        then 0.0
        else -0.5 *. (vel_x (i+1) j -. vel_x (i-1) j +.
                      vel_y i (j+1) -. vel_y i (j-1))
      in
      let p = buf in
      let update i j =
        if boundary i j
        then 0.0
        else (div i j +. p (i-1) j +. p (i+1) j +. p i (j-1) +. p i (j+1)) /. 4.0
      in
      do_times 20
        (fun () -> (write_to_array update buffer); set_boundary buffer p 0);
      let result_x i j =
        if boundary i j
        then 0.0
        else vel_x i j -. 0.5 *. (p (i+1) j -. p (i-1) j) /. h 
      in write_to_array result_x velocity_x;
      let result_y i j =
           if boundary i j
           then 0.0
           else vel_y i j -. 0.5 *. (p i (j+1) -. p i (j-1)) /. h 
      in write_to_array result_y velocity_y;
         set_boundary velocity_x vel_x 1;
         set_boundary velocity_y vel_y 2

    let advect dt target src force_x force_y b =
      advect_to_buffer dt src force_x force_y b;
      copy_to buffer target

    let diffuse dt flow src target b =
      diffuse_in_buffer dt flow src b;
      copy_to buffer target
      
    let adjust_velocity_field dt =
      diffuse dt C.visc vel_x velocity_x 1;
      diffuse dt C.visc vel_y velocity_y 2;
      project dt;
      advect dt velocity_x vel_x force_x force_y 1;
      advect dt velocity_y vel_y force_x force_y 2;
      project dt
      
    let perform_time_step dt =
      adjust_velocity_field dt;
      diffuse dt C.diff den density 0;
      advect dt density den vel_x vel_y 0
      
  end;;
