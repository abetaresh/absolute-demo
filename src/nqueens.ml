open Sm

let _ =
	let _data = read_sm_file "j102_2.mm" in
  let job_idx1, job_idx2 = 1, 2 in
  Printf.printf "start%d + duration%d <= start%d\n" job_idx1 job_idx1 job_idx2
	let MRCPSP mrcpsp => Mrcpsp.formula_of_rcpsp rcpsp decompositions