(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Sm

let _ =
	let _data = read_sm_file "j102_2.mm" in
  let job_idx1, job_idx2 = 1, 2 in
  Printf.printf "start%d + duration%d <= start%d\n" job_idx1 job_idx1 job_idx2
