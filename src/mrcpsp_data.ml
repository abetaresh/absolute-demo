
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

type modes = {
  mode_idx: int;
  duration: int;
  resources_usage: int list;
}
type precedence = {
  job_index: int;
  mode: int;
  successors: int;
  job_successors: int list;
  weights: int list;
}

type job = {
  job_index: int;
  mode: int list;
}

type project = {
  project_idx: int;
  jobs_number: int;
  horizon: int;
  precedence_relations: precedence list;
  jobs: job list;
  mode_idx: int list;
  resources_idx: int list;
}

type mrcpsp = {
  resources_capacities: int list;
  projects: project list;
}

let map_projects f mrcpsp = { mrcpsp with projects = (List.map f mrcpsp.projects) }

let number_of_resources mrcpsp = List.length mrcpsp.resources_capacities

let compute_horizon project =
  let horizon = List.fold_left (fun a j ->
    let weights = List.flatten (List.map (fun (p:precedence) ->
      if p.job_index = j.job_index then p.weights else []) project.precedence_relations) in
    let max_dur = List.fold_left max j.duration weights in
    a + max_dur
    ) 0 project.jobs in
  {project with horizon = horizon}