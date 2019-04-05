
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

    let null_buffer = write_to_array (fun i j -> 0.0) buffer

    let velocity_x = make_new_array (fun i j ->
                         let z = (float j /. float C.height) -. 0.5 in
                         8.0 *. (0.25 -. z *. z)
                       )
    let velocity_y = make_new_array (fun i j ->
                         let tx = float i /. float C.width in
                         let ty = float j /. float C.height in
                         tx *. (0.5 -. ty)
                       )

    let force_x i j =
      let tx = float i /. float C.width in
      let ty = float j /. float C.height in
      (1.0 -. tx) *. (1.0 -. tx) *. (1.0 -. tx)
      
    let force_y i j =
      let tx = float i /. float C.width in
      let ty = float j /. float C.height in
      0.0

    let den i j = density.(to_index i j)
    let buf i j = buffer.(to_index i j)
    let vel_x i j = velocity_x.(to_index i j)
    let vel_y i j = velocity_y.(to_index i j)
                  
    let color i j =
      let d = int_of_float (255.0 *. den i j) in
      [|d;d;d|]

    let cap_x v = min (max 0.5 v) (float C.width -. 2.5)
    let cap_y v = min (max 0.5 v) (float C.height -. 2.5)
      
    let advect_to_buffer dt src force_x force_y =
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
        else src i j
      in
      write_to_array update_function buffer

    let diffuse_in_buffer dt flow src =
      null_buffer;
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
      do_times 10
        (write_to_array update buffer)
      
    let project dt =
      null_buffer;
      let h = 1.0 /. float(C.width) in
      let div i j =
        if boundary i j
        then 0.0
        else -0.5 *. (vel_x (i+1) j -. vel_x (i-1) j +.
                      vel_y i (j+1) -. vel_y i (j-1))
      in
      let p = buf in
      let update i j =
        if boundary i j then 0.0
        else (div i j +. p (i-1) j +. p (i+1) j +. p i (j-1) +. p i (j+1)) /. 4.0
      in
      do_times 10
        (write_to_array update buffer);
      let result_x i j =
        if boundary i j
        then 0.0
        else vel_x i j -. 0.5 *. (p (i+1) j -. p (i-1) j) /. h
      in write_to_array result_x velocity_x;
         let result_y i j =
           if boundary i j
           then 0.0
           else vel_y i j -. 0.5 *. (p i (j+1) -. p i (j-1)) /. h
      in write_to_array result_y velocity_y
      
      
    let advect dt target src force_x force_y =
      advect_to_buffer dt src force_x force_y;
      copy_to buffer target

    let diffuse dt flow src target =
      diffuse_in_buffer dt flow src;
      copy_to buffer target
      
    let adjust_velocity_field dt =
      diffuse dt C.visc vel_x velocity_x;
      diffuse dt C.visc vel_y velocity_y;
      (* project dt; *) 
      advect dt velocity_x vel_x force_x force_y;
      advect dt velocity_y vel_y force_x force_y
      (* project dt *)
      
    let perform_time_step dt =
      diffuse dt C.diff den density;
      advect dt density den vel_x vel_y;
      adjust_velocity_field dt
      
  end;;
