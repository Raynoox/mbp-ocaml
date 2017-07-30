(* ocamlc graphics.cma fractal.ml -o fractal *)

open Graphics;;

let width, height = 800, 600;;
(* CIRCLE FRACTAL *)
let rec simpleCircleFractal x y r diff=
    if r > 600 then draw_circle 400 300 r else
    let increse  = float_of_int r *. diff in
        draw_circle  400  300 r;
        simpleCircleFractal  400  300 (int_of_float increse) diff;;

(* CIRCLE FRACTAL 2 *)
let rec radiusFractal x y r =
   if r < 5 then draw_circle x y r else
   let halfSize = r/2 in
        draw_circle x y r;
        radiusFractal (x+r) y halfSize;
        radiusFractal (x-r) y halfSize;
        radiusFractal x (y-r) halfSize;
        radiusFractal x (y+r) halfSize;;

(* heighway FRACTAL *)
let heighwayX x1 y1 x2 y2 = 
    (x1+x2)/2 + (y2-y1)/2;; (* dzielimy na pol i wysokosc z trojkoata rownoramiennego prostokatnego*)
let heighwayY x1 y1 x2 y2 =
    (x1-x2)/2 + (y1+y2)/2;;
let rec heighway n x1 y1 x2 y2 = if n == 1 then
            (moveto x1 y1; lineto x2 y2;)
            else
            (
            (heighway (n-1) x1 y1 (heighwayX x1 y1 x2 y2) (heighwayY x1 y1 x2 y2));
            (heighway (n-1) x2 y2 (heighwayX x1 y1 x2 y2) (heighwayY x1 y1 x2 y2));
            );;

(* TREE FRACTAL *)
let pointEnd x y radius length =
    x +. length *. cos radius,
    y +. length *. sin radius;;
let pointX x radius length =
    x +. length *. cos radius;;
let pointY y radius length =
    y +. length *. sin radius;;
let linedraw x y radius length width =
    let x_end, y_end = pointEnd x y radius length in
    set_line_width (int_of_float width);
    moveto (int_of_float x) (int_of_float y);
    lineto (int_of_float x_end) (int_of_float y_end);;

let pi = 4. *. atan 1.;;
let angleToRad = pi /. 180.0;;
let rec treeFractal x y angle length width diff angle2=
    set_color green;
    if length -. diff*.2.0 > 0. then
	set_color red;
    if length > 0. then
        let endx, endy = pointEnd x y angle length in
        linedraw x y angle length width;
        treeFractal endx endy (angle +. angle2 *. angleToRad) (length -. diff) (width *. 0.75) diff angle2;
        treeFractal endx endy (angle -. angle2 *. angleToRad) (length -. diff) (width *. 0.75) diff angle2;;



(* KOCH FRACTAL *)
let splitToThree p1 p2 = (p1*2)/3 + p2/3;;
let calculateNewTrianglePointX x1 x2 y1 y2 = 
	int_of_float (float_of_int(x1+x2)/. 2.0 -. (((float_of_int (y2-y1)/.3.0)/.2.0)*. sqrt 3.0));; (* srednia z x'ow i korekcja o roznice miedzy y jezeli w sa w innej plaszczyznie ze wzoru na wysokosc trojkata rownobocznego*)
let calculateNewTrianglePointY x1 x2 y1 y2 =
	int_of_float (float_of_int(y1+y2)/. 2.0 +. (((float_of_int (x2-x1)/.3.0)/.2.0)*. sqrt 3.0))
let rec break_line n x1 y1 x2 y2 = if n == 0 then
                (moveto x1 y1;
                lineto x2 y2;)
                else
                    (
                    break_line (n-1) x1 y1 (splitToThree x1 x2) (splitToThree y1 y2);
                    break_line (n-1) (splitToThree x1 x2) (splitToThree y1 y2) (calculateNewTrianglePointX x1 x2 y1 y2) (calculateNewTrianglePointY x1 x2 y1 y2) ;
                    break_line (n-1) (calculateNewTrianglePointX x1 x2 y1 y2) (calculateNewTrianglePointY x1 x2 y1 y2) (splitToThree x2 x1) (splitToThree y2 y1);
                    break_line (n-1) (splitToThree x2 x1) (splitToThree y2 y1) x2 y2;
                    );;




(* *********************** MAIN ******************* *)


print_string "Welcome in fractal generator !  \n ";;
print_string " ************************** \n " ;;


let rec menu n=
    let disp_main_menu=
    print_string "1: circle fractal \n ";
    print_string "2: circle v2 fractal \n " ;
    print_string "3: heighway fractal \n ";
    print_string "4: tree fractal \n ";
    print_string "5: koch fractal \n ";
    print_string "What do you choose: " in
    let read = read_int disp_main_menu
     in
    let () =
         if read = 1 then
                let () = print_string " Radius: " in
                let read = read_int () in
                let () = print_string " diff: " in
                let readDiff = read_float () in
                try open_graph " 800x600";
                set_color black;
				moveto 395 90;
				draw_string "Okregi";
                            simpleCircleFractal 300 400 read readDiff;
                            print_string " \n \n "
                      with
                     | Graphic_failure("fatal I/O error") -> print_string " ERROR ";
       else
           if read = 2 then
                let () = print_string "Radius: " in
                let read = read_int () in
                try open_graph " 800x600";
                set_color black;
				moveto 395 90;
				draw_string "Okregi 2";
                            radiusFractal 400 300 read;
                            print_string " \n \n "
                      with
                     | Graphic_failure("fatal I/O error") -> print_string " ERROR ";
       else
           if read = 3 then
                let () = print_string " n: " in
                let read = read_int () in
                try open_graph " 800x600";
                set_color black;
				moveto 395 90;
				draw_string "Heighway fractal";
                            heighway read 200 300 600 300;
                            print_string " \n \n "
                      with
                     | Graphic_failure("fatal I/O error") -> print_string " ERROR ";
       else
           if read = 4 then
                let ()= print_string " length " in
                let readLength= read_float () in
                let ()= print_string " diff " in
                let readDiff = read_float () in
                let () =print_string "angle " in
                let readAngl= read_float () in
                try open_graph " 800x600";
                set_color black;
				moveto 395 90;
				draw_string "Drzewo";
                            treeFractal 400. 200. (pi *. 0.5) readLength 8. readDiff readAngl ;
                            print_string " \n \n "
                      with
                     | Graphic_failure("fatal I/O error") -> print_string " ERROR ";
        else
           if read = 5 then
                let () = print_string " Number of diverges: " in
                let read = read_int () in
                try open_graph " 800x600";
                set_color black;
				moveto 395 90;
				draw_string "Platek kocha";
                            break_line read 300 300 400 474 ;
                            break_line read 400 474 500 300 ;
                            break_line read 500 300 300 300 ;
                            print_string " \n \n "
                      with
                     | Graphic_failure("fatal I/O error") -> print_string " ERROR ";

       else print_string "a"
       in menu n;;

menu 1;;
read_line();;
