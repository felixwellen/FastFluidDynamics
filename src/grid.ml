
module type Config = sig
  val width : int
  val height : int
  val diff : float
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
                         
    let boundary i j = (i == 0) || (i >= C.width-1) || (j == 0) || (j == C.height-1)
                     
    let cap d = max 0.0 (min d 1.0) 
                       
    let density =
      let value i j = if (i < 10 || i > C.width - 10)
                         || (j < 10 || j > C.height - 10)
                      then (Random.float 1.0) else 0.0
      in make_new_array value

    let buffer = make_new_array (fun i j -> 0.0)
    let null_buffer = write_to_array (fun i j -> 0.0) buffer

    let velocity_x = make_new_array (fun i j -> 1.0)
    let velocity_y = make_new_array (fun i j -> 0.0)
                    
    let den i j = density.(to_index i j)
    let buf i j = buffer.(to_index i j)
    let vel_x i j = velocity_x.(to_index i j)
    let vel_y i j = velocity_y.(to_index i j)
                  
    let color i j =
      let d = int_of_float (255.0 *. den i j) in
      [|d;d;d|]

    let cap_x v = min (max 0.5 v) (float C.width -. 2.5)
    let cap_y v = min (max 0.5 v) (float C.height -. 2.5)
      
    let advect dt =
      let dt0 = dt*.float C.width in
      let update_function i j =
        if not (boundary i j)
        then let x = cap_x (float i -. dt0 *. vel_x i j) in
             let y = cap_y (float j -. dt0 *. vel_y i j) in
             let i0 = int_of_float x in
             let i1 = i0+1 in
             let j0 = int_of_float y in
             let j1 = j0+1 in
             let s1 = x -. float(i0) in
             let s0 = 1.0 -. s1 in
             let t1 = y -. float(j0) in
             let t0 = 1.0 -. t1 in
             s0 *. (t0 *. den i0 j0 +. t1 *. den i0 j1) +.
             s1 *. (t0 *. den i1 j0 +. t1 *. den i1 j1)
        else den i j
      in
      write_to_array update_function buffer;
      copy_to buffer density
          
    let diffuse_good dt =
      null_buffer;
      let a = C.diff *. dt *. float_of_int (C.width * C.height) in
      let update_function i j =
        if not (boundary i j)
        then cap ((den i j
                   +. a*.(buf (i-1) j
                          +. buf (i+1) j
                          +. buf i (j-1)
                          +. buf i (j+1)))
                  /. (1.0 +. 4.0 *. a))
        else den i j
      in
      do_times 10
        (write_to_array update_function buffer);
      copy_to buffer density
      
    let diffuse_bad dt =
      let flow = C.diff *. dt *. float_of_int (C.width * C.height) in
      let update_function i j =
        if not (boundary i j)
        then cap (den i j
                  +. flow *.  (
                    den i (j-1)
                    +. den i (j+1)
                    +. den (i-1) j
                    +. den (i+1) j
                    -. 4.0 *. den i j)) 
        else den i j
      in
      write_to_array update_function buffer;
      copy_to buffer density

    let perform_time_step dt =
      diffuse_good dt;
      advect dt
      
  end;;
