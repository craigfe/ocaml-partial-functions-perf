let ( = ) = Int.equal

external id : 'a -> 'a = "%identity"

module Container = struct
  type 'a t = int * 'a

  let find_opt (k, v) k' = if k = k' then Some v else None

  let find_exn =
    let exn = Invalid_argument "find_exn'" in
    fun (k, v) k' -> if k = k' then v else raise exn

  let find_and_call (k, v) k' ~some ~none = if k = k' then some v else none ()
  let find_opt' t k = find_and_call ~some:Option.some ~none:(fun () -> None) t k

  let find_exn' =
    let none =
      let exn = Invalid_argument "find_exn'" in
      fun () -> raise exn
    in
    fun t k -> find_and_call ~some:id ~none t k
end

open Bechamel

let bench_fn pass fn =
  let iters = 1_000_000 in
  let t = (1, 2) in
  let k = if pass then 1 else -1 in
  Staged.stage (fun () ->
      for _ = 1 to iters do
        fn t k
      done)

type _ typ = Opt : 'a option typ | Exn : 'a typ

let suite =
  let test : type a. a typ -> string -> (int Container.t -> int -> a) -> Test.t =
   fun typ name fn ->
    let true_, false_ =
      match typ with
      | Opt -> (
          ( (fun t k -> match fn t k with Some _ -> () | None -> assert false),
            fun t k -> match fn t k with None -> () | Some _ -> assert false ))
      | Exn -> (
          ( (fun t k -> match fn t k with _ -> () | exception _ -> assert false),
            fun t k -> match fn t k with exception _ -> () | _ -> assert false ))
    in
    Test.make_grouped ~name
      [
        Test.make ~name:" true " (bench_fn true true_);
        Test.make ~name:" false" (bench_fn false false_);
      ]
  in

  Test.make_grouped ~name:""
    [
      test Opt "find_opt            " Container.find_opt;
      test Opt "find_opt (indirect) " Container.find_opt';
      test Exn "find_exn            " Container.find_exn;
      test Exn "find_exn (indirect) " Container.find_exn';
    ]

let metrics = Toolkit.Instance.[ minor_allocated; monotonic_clock ]

let benchmark () =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let quota = Time.second 0.5 in
  let cfg = Benchmark.cfg ~limit:2000 ~quota ~kde:(Some 1000) () in
  let raw_results = Benchmark.all cfg metrics suite in
  List.map (fun i -> Analyze.all ols i raw_results) metrics |> Analyze.merge ols metrics

let rect =
  match Notty_unix.winsize Unix.stdout with
  | Some (w, h) -> { Bechamel_notty.w; h }
  | None -> { Bechamel_notty.w = 80; h = 1 }

let () =
  List.iter (fun v -> Bechamel_notty.Unit.add v (Measure.unit v)) metrics;
  benchmark ()
  |> Bechamel_notty.Multiple.image_of_ols_results ~rect ~predictor:Measure.run
  |> Notty_unix.eol |> Notty_unix.output_image
