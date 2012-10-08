(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(*
show img dst
affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

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

(* main *)
let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Aucun fichier à charger";
    sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
    let (w,h) = get_dims img in
    let new_surface = Sdlvideo.create_RGB_surface_format img [`SWSURFACE] w h in
    let img_grey = image2grey img new_surface in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      show img display;
      wait_key ();
      show img_grey display;
      wait_key (); 
      exit 0
  end

let _ = main ()
