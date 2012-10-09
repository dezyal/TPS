let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
      
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(* level *)
let level (r,g,b) =
  (0.3 *. (float)r +. 0.59 *. (float)g +. 0.11 *. (float)b) /. 255.

(* color2grey *)
let color2grey (r,g,b) =
  let px = int_of_float(level(r,g,b)*.255.) in
    (px,px,px)

(* image2grey *)
let image2grey src dst =
 let (w,h) = get_dims src in
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      Sdlvideo.put_pixel_color dst x y
       (color2grey (Sdlvideo.get_pixel_color src x y))
    done
  done;
  dst

(* out_of_bounds *)
let out_of_bounds mtx width height x y =
  x < 0 || x > width-1 || y < 0 || y > height-1

(* Convolution filter *)
let conv_filter_3x3 mtx w h =
  let filter = [|0;1;0;1;-4;1;0;1;0|] and factor = 1.0 in
  let new_mtx = Array.make_matrix w h 100 in
  for x = 0 to w-1 do
   for y = 0 to h-1 do
     let a = if not(out_of_bounds mtx w h (x-1) (y-1)) then
       mtx.(x-1).(y-1) * filter.(0) else 0 in
     let b = if not(out_of_bounds mtx w h (x) (y-1)) then
       mtx.(x).(y-1) * filter.(1) else 0 in
     let c = if not(out_of_bounds mtx w h (x+1) (y-1)) then
       mtx.(x+1).(y-1) * filter.(2) else 0 in
     let d = if not(out_of_bounds mtx w h (x-1) y) then
       mtx.(x-1).(y) * filter.(3) else 0 in
     let e = if not(out_of_bounds mtx w h x y) then
       mtx.(x).(y) * filter.(4) else 0 in
     let f = if not(out_of_bounds mtx w h (x+1) y) then
       mtx.(x+1).(y) * filter.(5) else 0 in
     let g = if not(out_of_bounds mtx w h (x-1) (y+1)) then
       mtx.(x-1).(y+1) * filter.(6) else 0 in
     let h = if not(out_of_bounds mtx w h (x) (y+1)) then
       mtx.(x).(y+1) * filter.(7) else 0 in
     let i = if not(out_of_bounds mtx w h (x+1) (y+1)) then
       mtx.(x+1).(y+1) * filter.(8) else 0 in
      let px = int_of_float((float)(a+b+c+d+e+f+g+h+i)*.factor) in
      new_mtx.(x).(y) <- if px < 0 then 0 else px
   done
  done;
  new_mtx 

(*
Grise une surface et renvoi la matrice de couleurs correspondante.
*)
let su2grey su w h =
  let mtx = Array.make_matrix w h 0 in
    for c = 0 to h-1 do
      for l = 0 to w-1 do
        let (r, g, b) = Sdlvideo.get_pixel_color su l c in
          let lu = int_of_float
          (0.299 *. float_of_int(r) +.
          0.587 *. float_of_int(g) +.
          0.114 *. float_of_int(b)) in
            mtx.(l).(c) <- lu
      done
    done;
    mtx

let mtx2su mtx w h pfi =
  let bpp = pfi.Sdlvideo.bits_pp and rm = pfi.Sdlvideo.rmask and
  gm = pfi.Sdlvideo.gmask and bm = pfi.Sdlvideo.bmask and
  am = pfi.Sdlvideo.amask in
    let su = Sdlvideo.create_RGB_surface [`SWSURFACE] ~w:w ~h:h
    ~bpp:bpp ~rmask:rm ~gmask:gm ~bmask:bm ~amask:am in
      let clr = ref 0 in
        Sdlvideo.lock su;
        for c = 0 to h-1 do
          for l = 0 to w-1 do
            clr := mtx.(l).(c);
            Sdlvideo.put_pixel_color su l c (!clr, !clr, !clr)
          done
        done;
        Sdlvideo.unlock su;
        su


let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Aucun fichier à charger";
    sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
    let pfi = Sdlvideo.surface_format img in
    let (w,h) = get_dims img in
    let new_su = Sdlvideo.create_RGB_surface_format img [`SWSURFACE] w h in
    let img_grey = image2grey img new_su in
    let img_cont = mtx2su (conv_filter_3x3 (su2grey img w h) w h) w h pfi in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      show img display;
      wait_key ();
      show img_cont display;
      wait_key (); 
      exit 0
  end

let _ = main ()
