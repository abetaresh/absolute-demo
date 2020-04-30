open Bounds
open Lang
open Lang.Ast
open Lang_decompositions.Cumulative
open Parsers_scheduling.Mrcpsp_data
open Model_utility
open Kobecore

module Mrcpsp_model(D: Cumulative_decomposition) =
struct
  module Resource = D.C
  module Duration = D.D
  module Mode = D.M

  let start_job_name i = "start_j" ^ (string_of_int i)
  let job_mode_idx i = "mode_j"^ (string_of_int i)

  let makespan_name project = start_job_name project.jobs_number

  (*make_mode should return task_mode as a result*)
  let get_task_mode ri mode = 
    let m = job_mode_idx mode.mode_idx
    let r = List.nth mode.resources_usage ri in 
    let d = mode.duration in
    let task_mode =  D.{ mode_num=job_mode_idx mode.mode_idx;
      duration=Param (Duration.of_int_up mode.duration); } in
    D.{ mode_task; id=mode.mode_idx;
      resources_usage = Resource.of_int_up r;}

  (*for every mode in the mode list (m) generate the data of duration, resources_usage and mode _idx*)
  let make_task ri job =
    let m = List.nth job.modes in 
    let mode_data = List.map (get_task_mode ri) job.modes in 
    let task = D.{ mode_data; id=job.job_index;
        modes = Mode.of_int_up m;}


  let make_tasks project ri = List.map (make_task ri) project.jobs

  let make_cumulative Mrcpsp project horizon ri capi =
    let capacity = List.nth Mrcpsp.resources_capacities capi in
    let capacity = Resource.of_int_down capacity in
    let tasks = make_tasks project ri in
    D.cumulative tasks horizon capacity D.default_name_factory

  let all_cumulatives Mrcpsp project =
    (* note that the index 0 does not matter here, as `resources_usage` is not used by `shared_constraint`. *)
    let tasks = make_tasks project 0 in
    let horizon = Duration.of_int_up project.horizon in
    let shared_constraints = D.shared_constraints tasks horizon D.default_name_factory in
    let cumulatives = List.mapi (make_cumulative Mrcpsp project horizon)
      project.resources_idx in
    let cumulatives = Rewritting.conjunction cumulatives in
    Rewritting.map_formula (fun f -> Rewritting.conjunction [f; cumulatives]) shared_constraints

  let time_variables project =
    List.map (fun job -> start_job_name job.job_index) project.jobs

  let quantify_time_variables project qf =
    let vars = time_variables project in
    quantify_vars vars (Concrete Duration.concrete_ty) qf

  let var_domain_constraints project =
    let vars = time_variables project in
    let ty = Duration.concrete_ty in
    Rewritting.conjunction (
      (dom_of_var ty Bound_rat.zero Bound_rat.zero (List.hd vars))::
      (dom_of_vars (List.tl vars) ty Bound_rat.zero (Bound_rat.of_int project.horizon))
    )

  (* Generalized temporal constraints: ensure a precedence (with a possible timelag) between the tasks. 
  let temporal_constraints project =
    let precedence_constraint (prec:precedence) (j,w) =
      D.precedence
        (start_job_name prec.job_index)
        (start_job_name j)
        (Param (Duration.of_int_up w)) in
    let all_successors (precedence:precedence) =
      List.map (precedence_constraint precedence)
        (List.map2 (fun x y -> (x,y)) precedence.job_successors precedence.weights) in
    Rewritting.conjunction
      (List.flatten (List.map all_successors project.precedence_relations))
*)
  let formula_of_Mrcpsp Mrcpsp =
    let project = List.hd Mrcpsp.projects in
    let makespan = makespan_name project in
    (*let var_domains = var_domain_constraints project in*)
    (*let precedences = temporal_constraints project in*)
    let cumulatives = all_cumulatives Mrcpsp project in
    (*let all_constraints =
      Rewritting.map_formula
        (fun f -> Rewritting.conjunction [f; var_domains; precedences])
        cumulatives in*)
    { qf=(quantify_time_variables project);
      optimise=(Minimize makespan) }
end

let help_msg = "Currently, only `TimeRD` or `TaskRD` decomposition for cumulative is supported."

module Mrcpsp_TaskRD = Mrcpsp_model(MakeTaskRD(Bound_int)(Bound_int))
module Mrcpsp_TimeRD = Mrcpsp_model(MakeTimeRD(Bound_int))

let formula_of_Mrcpsp Mrcpsp (decompositions: Bench_instance_j.decomposition list) =
  if List.length decompositions > 1 then
    System.eprintf_and_exit ("More than one decomposition for Mrcpsp were given.\n" ^ help_msg)
  else
    let decomposition = List.hd decompositions in
    match decomposition.name with
    | "TimeRD" -> Mrcpsp_TimeRD.formula_of_Mrcpsp Mrcpsp
    | "TaskRD" -> Mrcpsp_TaskRD.formula_of_Mrcpsp Mrcpsp
    | _ -> System.eprintf_and_exit (
        "Unknown cumulative decomposition for Mrcpsp `" ^ decomposition.name ^ "`.\n" ^ help_msg) *)

