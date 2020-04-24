
type resources_info = {
  renewable: int;
  nonrenewable: int;
  doubly_constrained: int;
}


let check_resources_info r =
  if r.doubly_constrained > 0 then
    failwith "ProGenMax model with nonrenewable or doubly constrained resources: not yet implemented."
  else
    r

type mode = {
  mode_idx: int;
  duration: int;
  resources_usage: int list;
}

type job = {
  job_index: int;
  modes_num: int;
  modes: mode list;
}

(* start1 + duration1 <= start2 *)
(* start1 + duration1 <= start3 *)

(* start<job_index> + weights[0] <= start<job_successors[0]> *)
type precedence = {
  job_index: int;
  successors: int;
  job_successors: int list;
}

type project = {
  project_idx: int;
  jobs_number: int;
  horizon: int;
  precedence_relations: precedence list;
  jobs: job list;
  resources_idx: int list;
}

type mrcpsp = {
  resources_capacities: int list;
  projects: project list;
}

let map_projects f mrcpsp = { mrcpsp with projects = (List.map f mrcpsp.projects) }

let number_of_resources mrcpsp = List.length mrcpsp.resources_capacities

let compute_horizon project =
  let horizon = List.fold_left (fun a (j:job) ->
    let max_dur = List.fold_left (fun d m -> max d m.duration) 0 j.modes in
    a + max_dur
    ) 0 project.jobs in
  {project with horizon = horizon}