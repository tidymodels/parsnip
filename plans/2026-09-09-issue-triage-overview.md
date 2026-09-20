# Parsnip open-issue triage (2026-09-09)

All 84 open issues in tidymodels/parsnip were reviewed (body plus all comments), oldest to newest, and classified by their actual content rather than their labels. Bugs get a plan file in this directory; everything else gets a one-line rationale below.

Classification key:

- **Bug**: defect in existing behavior (errors, wrong results, silently ignored arguments) → plan file
- **Feature**: new capability; includes requests where the current error is correct but unhelpful
- **Docs**: documentation change
- **Upkeep**: internal refactoring, CI, conventions, or open design discussion

## Bug plan files

| Plan file | Issue(s) | One-line root cause |
|---|---|---|
| [issue 432](2026-09-09-issue-0432-mars-cv-prune-quosure.md) | [#432](https://github.com/tidymodels/parsnip/issues/432) | `mars()` with `prune_method = "cv"` + `prod_degree`: quosure reaches earth unevaluated on the cv re-dispatch path; silently makes `prune_method` untunable. |
| [issues 472 + 1409](2026-09-09-issue-0472-1409-earth-multiclass-class-pred.md) | [#472](https://github.com/tidymodels/parsnip/issues/472), [#1409](https://github.com/tidymodels/parsnip/issues/1409) | earth `type = "class"` post (`R/mars_data.R`) hardcodes a binary threshold; multiclass fits silently predict one class for every row. The `prob` half was fixed in #1334; `class` was not. |
| [issue 492](2026-09-09-issue-0492-fit-dots-inconsistent.md) | [#492](https://github.com/tidymodels/parsnip/issues/492) | `fit()` dots behave differently on each interface path: silently dropped, silently honored, or raw engine error. |
| [issue 796](2026-09-09-issue-0796-xgb-monotone-sign.md) | [#796](https://github.com/tidymodels/parsnip/issues/796) | xgboost binary-outcome recoding (`event_level = "first"`) silently inverts the effective sign of `monotone_constraints`. |
| [issue 857](2026-09-09-issue-0857-glmnet-multi-predict-raw.md) | [#857](https://github.com/tidymodels/parsnip/issues/857) | `multi_predict(type = "raw")` is silently ignored for glmnet `linear_reg()` and errors inside glmnet for logistic/multinomial. |
| [issue 974](2026-09-09-issue-0974-kernlab-one-svc-predict.md) | [#974](https://github.com/tidymodels/parsnip/issues/974) | kernlab `one-svc` returns a logical matrix; `predict_class()` crashes with `$ operator is invalid for atomic vectors`. Scope contested (may belong in tidyclust/applicable). |
| [issue 999](2026-09-09-issue-0999-xgb-custom-objective-probs.md) | [#999](https://github.com/tidymodels/parsnip/issues/999) | Function-valued xgboost `objective` falls through `xgb_predict()`'s switch; raw margins are silently labeled probabilities. |
| [issue 1069](2026-09-09-issue-1069-glmnet-relax.md) | [#1069](https://github.com/tidymodels/parsnip/issues/1069) | `set_engine("glmnet", relax = TRUE)` fails: `relax.glmnet()` re-evaluates parsnip's recorded call, which still contains quosures. |
| [issue 1251](2026-09-09-issue-1251-set-model-arg-func-vector.md) | [#1251](https://github.com/tidymodels/parsnip/issues/1251) | `set_model_arg()` accepts the documented character-vector `func` form, then tuning breaks much later with `$ operator is invalid for atomic vectors`. |
| [issues 1256 + 1258](2026-09-09-issue-1256-1258-pred-type-dots.md) | [#1256](https://github.com/tidymodels/parsnip/issues/1256), [#1258](https://github.com/tidymodels/parsnip/issues/1258) | `check_pred_type_dots()` prints the literal string `bad_args` in its error, and its allowlist blocks the old `quantile` argument so the intended deprecation warning is dead code. |
| [issue 1260](2026-09-09-issue-1260-double-registration.md) | [#1260](https://github.com/tidymodels/parsnip/issues/1260) | "Model already registered" abort blocks attaching extension packages on parallel workers. Stalled waiting on the reporter's reprex; plan flags what needs confirming. |
| [issue 1398](2026-09-09-issue-1398-bag-tree-rpart-args.md) | [#1398](https://github.com/tidymodels/parsnip/issues/1398) | `bag_tree()` + rpart + censored regression has no tunable args unless baguette loads first; mode-independent arg registration belongs in parsnip. Partly blocked on #1351. |
| [issue 1405](2026-09-09-issue-1405-liblinear-cost-arg.md) | [#1405](https://github.com/tidymodels/parsnip/issues/1405) | LiblineaR `cost` registered as `original = "C"` but the function argument is `cost`; the value is silently swallowed, so tuning `cost` does nothing. |
| [issue 1406](2026-09-09-issue-1406-kknn-multi-predict-neighbors.md) | [#1406](https://github.com/tidymodels/parsnip/issues/1406) | kknn `multi_predict()` without `neighbors` re-evaluates the unevaluated `ks` call outside its environment → length-zero condition error. |
| [issue 1407](2026-09-09-issue-1407-bart-interval-bounds.md) | [#1407](https://github.com/tidymodels/parsnip/issues/1407) | BART classification intervals: a double `apply()` sorts bounds across observations and breaks the result shape. |
| [issue 1408](2026-09-09-issue-1408-predict-raw-opts.md) | [#1408](https://github.com/tidymodels/parsnip/issues/1408) | `predict_raw()` uses `opts[[!dup_args]]` (`[[` with a logical vector), so any protected-name collision errors instead of dropping the argument. |
| [issue 1410](2026-09-09-issue-1410-multi-predict-args-workflow.md) | [#1410](https://github.com/tidymodels/parsnip/issues/1410) | `multi_predict_args.workflow()` uses a stale extraction path and never recurses, returning `NULL` instead of argument names. |

## Recommended to close or verify-and-close

| Issue | Reason |
|---|---|
| [#761](https://github.com/tidymodels/parsnip/issues/761) | dbarts case weights are implemented exactly as topepo specified (`R/bart_data.R` includes `weights` in `data` and `protect`; NEWS for 1.5.0 records it). Only the requested test cases remain worth confirming. |
| [#1143](https://github.com/tidymodels/parsnip/issues/1143) | xgboost quantile regression now registered (`R/boost_tree_data.R`, `objective = "reg:quantileerror"`, added via #1321). The remaining `probably` engine-swap half lives in another repo. |
| [#1355](https://github.com/tidymodels/parsnip/issues/1355) | Labeled bug, but the suggested fix is already on `main`: `xy_xy()` now prefers `env$y_var` (`R/fit_helpers.R:126-134`), mirroring `xy_form()`. Needs a verification pass and closure; also the failing branch is currently unreachable (no matrix-interface censored engines). |

## Mislabeled issues worth relabeling

| Issue | Current label | Actual |
|---|---|---|
| #472, #492, #999, #1251 | feature (or feature/documentation) | bug — each describes wrong or silently inconsistent current behavior |
| #796 | feature | bug — silently inverted constraint direction |
| #857 | feature | bug — argument silently ignored / inconsistent error |
| #1405, #1406, #1407, #1408, #1409, #1410 | tidy-dev-day only | bug — #1405 and #1409 produce silently wrong results |
| #520 | documentation | upkeep — code-organization refactor |
| #1400 | feature | upkeep — export existing internal helpers to de-duplicate censored |

## All other issues (no plan file)

Sorted oldest to newest.

| Issue | Filed | Class | Rationale |
|---|---|---|---|
| [#118](https://github.com/tidymodels/parsnip/issues/118) | 2019-01 | Feature | xgboost `gblinear` engine for `linear_reg()`. |
| [#119](https://github.com/tidymodels/parsnip/issues/119) | 2019-01 | Feature | ranger `quantreg` prediction never registered; partially superseded by the `"quantile regression"` mode (grf engine, #1209). |
| [#197](https://github.com/tidymodels/parsnip/issues/197) | 2019-07 | Feature | Map `penalty`/`mixture` to rstanarm priors; design contested (thread converges on horseshoe, not Bayesian LASSO). |
| [#198](https://github.com/tidymodels/parsnip/issues/198) | 2019-07 | Feature | L1 penalty for keras `linear_reg()`; L2-only is documented. Re-scope against the new keras3 engine. |
| [#266](https://github.com/tidymodels/parsnip/issues/266) | 2020-02 | Feature | Allow successes/failures matrix outcomes for `logistic_reg()`; the current factor-only check is intentional. |
| [#380](https://github.com/tidymodels/parsnip/issues/380) | 2020-10 | Upkeep | Sweep for missing `type = "raw"` coverage; the listed combos are stale (includes removed `surv_reg`) — re-audit first. |
| [#458](https://github.com/tidymodels/parsnip/issues/458) | 2021-03 | Feature | Validate `new_data` columns at `predict()` (workflows already does); related to #856. |
| [#520](https://github.com/tidymodels/parsnip/issues/520) | 2021-06 | Upkeep | Consolidate engine definitions into per-engine files; no user-visible change. |
| [#574](https://github.com/tidymodels/parsnip/issues/574) | 2021-10 | Upkeep | Make missing-engine and wrong-engine errors equally informative; `show_engines()` already exists. |
| [#602](https://github.com/tidymodels/parsnip/issues/602) | 2021-11 | Feature | Proportional `mtry`; blocked on the `mtry = 1` count-vs-proportion ambiguity per simonpcouch. |
| [#616](https://github.com/tidymodels/parsnip/issues/616) | 2021-12 | Feature | Replace interval prediction types with an `interval` argument; API redesign, cross-ref censored#134. |
| [#637](https://github.com/tidymodels/parsnip/issues/637) | 2022-01 | Feature | Main `class_weights`-style argument for cost-sensitive learning. |
| [#693](https://github.com/tidymodels/parsnip/issues/693) | 2022-03 | Docs | Restore engine/parsnip argument-mapping tables lost in the #456 refactor. |
| [#722](https://github.com/tidymodels/parsnip/issues/722) | 2022-05 | Feature | `augment()` without `new_data` re-predicting training data; current guardrail is intentional. |
| [#724](https://github.com/tidymodels/parsnip/issues/724) | 2022-05 | Feature | `autoplot()` diagnostics for fits; Emil now leans toward it not belonging in parsnip (wontfix candidate). |
| [#725](https://github.com/tidymodels/parsnip/issues/725) | 2022-05 | Docs | Document where parsnip overrides dials default parameter ranges. |
| [#761](https://github.com/tidymodels/parsnip/issues/761) | 2022-06 | Feature | dbarts case weights — appears already implemented; see close list above. |
| [#765](https://github.com/tidymodels/parsnip/issues/765) | 2022-07 | Feature | Pass an explicit validation data frame to xgboost; interface design needed. |
| [#904](https://github.com/tidymodels/parsnip/issues/904) | 2022-07 | Feature | Multitarget random forest via randomForestSRC; umbrella #35. |
| [#903](https://github.com/tidymodels/parsnip/issues/903) | 2022-08 | Feature | Beta regression (betareg/gamlss); third-party prototype exists. |
| [#784](https://github.com/tidymodels/parsnip/issues/784) | 2022-08 | Feature | Spatial regression engines; blocked on incompatible data formats. |
| [#798](https://github.com/tidymodels/parsnip/issues/798) | 2022-08 | Docs | Document which engines support multivariate outcomes. The `.estimate_metrics()` error in the comments is a separate tune/yardstick defect and should be split out. |
| [#812](https://github.com/tidymodels/parsnip/issues/812) | 2022-09 | Docs | Document per-engine missing-value support; topepo sketched a roxygen-template plan. |
| [#830](https://github.com/tidymodels/parsnip/issues/830) | 2022-10 | Feature | xgboost native categorical predictors; waiting on upstream xgboost release. |
| [#856](https://github.com/tidymodels/parsnip/issues/856) | 2023-01 | Feature | Better error when `new_data` doesn't match a matrix-interface glmnet fit; behavior itself is correct. |
| [#857](https://github.com/tidymodels/parsnip/issues/857) | 2023-01 | **Bug** | See plan file above. |
| [#878](https://github.com/tidymodels/parsnip/issues/878) | 2023-02 | Upkeep | Remove `predict_<type>_glmnet()` wrappers and deprecate `eval_args()`; interacts with #857 and #1069. |
| [#1012](https://github.com/tidymodels/parsnip/issues/1012) | 2023-03 | Feature | Error informatively when a custom model lacks `set_encoding()` (`get_encoding()` returning `NULL` hits `dplyr::filter()`); reporter's issue was resolved, kept open for validation. |
| [#974](https://github.com/tidymodels/parsnip/issues/974) | 2023-05 | **Bug** | See plan file above. |
| [#999](https://github.com/tidymodels/parsnip/issues/999) | 2023-09 | **Bug** | See plan file above. |
| [#1007](https://github.com/tidymodels/parsnip/issues/1007) | 2023-10 | Docs | Explain when to pick each engine in model docs. |
| [#1041](https://github.com/tidymodels/parsnip/issues/1041) | 2024-01 | Docs | De-duplicate `@param mode`/`@param engine` roxygen text. |
| [#1042](https://github.com/tidymodels/parsnip/issues/1042) | 2024-01 | Docs | Document when to use non-default `set_encoding()` options. |
| [#1084](https://github.com/tidymodels/parsnip/issues/1084) | 2024-03 | Feature | Unseen factor levels break `lm()` prediction; upstream `stats::lm()` behavior — the actionable work is a better parsnip error. |
| [#1094](https://github.com/tidymodels/parsnip/issues/1094) | 2024-04 | Upkeep | Add `check_args()` methods to remaining model types (currently nothing to check). |
| [#1095](https://github.com/tidymodels/parsnip/issues/1095) | 2024-04 | Feature | Type/length validation of main arguments in `check_args()`. |
| [#1112](https://github.com/tidymodels/parsnip/issues/1112) | 2024-04 | Feature | Report/print the event level for `logistic_reg()`. |
| [#1114](https://github.com/tidymodels/parsnip/issues/1114) | 2024-04 | Feature | Support multiple engine interfaces per model (registration-system design); motivates the kernlab stack-overflow failure. |
| [#1115](https://github.com/tidymodels/parsnip/issues/1115) | 2024-04 | Feature | Switch `kernlab::ksvm()` to the xy interface; a commenter reports it fixes wide-data failures. |
| [#1129](https://github.com/tidymodels/parsnip/issues/1129) | 2024-07 | Upkeep | Cron revdep-check action; unresolved discussion (simonpcouch skeptical). |
| [#1143](https://github.com/tidymodels/parsnip/issues/1143) | 2024-07 | Feature | xgboost quantile regression — largely implemented; see close list above. |
| [#1145](https://github.com/tidymodels/parsnip/issues/1145) | 2024-08 | Upkeep | Skip R-version-inappropriate GHA installs/tests. |
| [#1146](https://github.com/tidymodels/parsnip/issues/1146) | 2024-08 | Feature | ptLasso engine; likely blocked like sparse-group LASSO (#595). |
| [#1155](https://github.com/tidymodels/parsnip/issues/1155) | 2024-08 | Upkeep | Harmonize `set_args()` vs `tunable()` default ranges; a real mismatch would matter, so audit first. |
| [#1163](https://github.com/tidymodels/parsnip/issues/1163) | 2024-08 | Upkeep | Refine `glm_grouped()` error message (report outcome levels); route through `check_outcome()`. |
| [#1184](https://github.com/tidymodels/parsnip/issues/1184) | 2024-09 | Upkeep | Promote "model fit failed" prediction warning to an error with `parent = object$fit`; extract shared helper. |
| [#1195](https://github.com/tidymodels/parsnip/issues/1195) | 2024-09 | Upkeep | Stop snapshot-testing base R error text; simonpcouch listed the affected snap files. |
| [#1205](https://github.com/tidymodels/parsnip/issues/1205) | 2024-09 | Upkeep | `make_parameter_list()` should follow function-signature argument order in engine docs. |
| [#1226](https://github.com/tidymodels/parsnip/issues/1226) | 2024-12 | Docs | Add `?model_type_engine` aliases for `?details_*` topics. |
| [#1232](https://github.com/tidymodels/parsnip/issues/1232) | 2025-01 | Feature | Register non-tunable model args / formally register engine args. |
| [#1252](https://github.com/tidymodels/parsnip/issues/1252) | 2025-02 | Docs | `predict._elnet` dispatch on custom `"_elnet"`-classed fits is correct S3 behavior per topepo; document the extension-author pitfall. |
| [#1260](https://github.com/tidymodels/parsnip/issues/1260) | 2025-02 | **Bug** | See plan file above (stalled on reporter). |
| [#1286](https://github.com/tidymodels/parsnip/issues/1286) | 2025-09 | Feature | `dann` engine for `nearest_neighbor()`. |
| [#1287](https://github.com/tidymodels/parsnip/issues/1287) | 2025-09 | Feature | `lmrob` engine for `linear_reg()`; contributor volunteered. |
| [#1296](https://github.com/tidymodels/parsnip/issues/1296) | 2025-10 | Docs | roxygen for per-engine missing-data handling; started in PR #1290. |
| [#1298](https://github.com/tidymodels/parsnip/issues/1298) | 2025-10 | Feature | New `ordinal_reg()` model; active collaboration with @corybrunson. |
| [#1301](https://github.com/tidymodels/parsnip/issues/1301) | 2025-10 | Upkeep | Column-ordering convention for interval predictions; related to #1353. |
| [#1311](https://github.com/tidymodels/parsnip/issues/1311) | 2025-11 | Feature | Helper for binary deviance residuals. |
| [#1315](https://github.com/tidymodels/parsnip/issues/1315) | 2025-12 | Upkeep | Drop old-xgboost compatibility layer (~6 months after #1307). |
| [#1322](https://github.com/tidymodels/parsnip/issues/1322) | 2026-01 | Upkeep | Test that scans `man/rmd` for missing standard sections. |
| [#1331](https://github.com/tidymodels/parsnip/issues/1331) | 2026-02 | Upkeep | Whether `qrnn` belongs in Suggests after #1323. |
| [#1351](https://github.com/tidymodels/parsnip/issues/1351) | 2026-03 | Upkeep | Design question: collision semantics when two extensions register the same argument; blocks #1398. |
| [#1353](https://github.com/tidymodels/parsnip/issues/1353) | 2026-03 | Feature | Prediction-column selection helpers; overlaps #1301. |
| [#1354](https://github.com/tidymodels/parsnip/issues/1354) | 2026-03 | Docs | gls engine works once multilevelmod is loaded; kept open for two doc gaps. |
| [#1355](https://github.com/tidymodels/parsnip/issues/1355) | 2026-03 | Bug (fixed) | Fix already on `main`; see close list above. |
| [#1366](https://github.com/tidymodels/parsnip/issues/1366) | 2026-04 | Feature | `show_models()` reverse lookup; topepo prototyped two variants. |
| [#1368](https://github.com/tidymodels/parsnip/issues/1368) | 2026-04 | Docs | Fix developer instructions for model-info updates; remove residual `inst/models.tsv` usage. |
| [#1400](https://github.com/tidymodels/parsnip/issues/1400) | 2026-08 | Upkeep | Export `multi_predict()` helper internals so censored can drop its copies. |
| [#1411](https://github.com/tidymodels/parsnip/issues/1411) | 2026-09 | Upkeep | Harmonize penalty-path defaults for elastic-net `ordinal_reg` engines; deliberate design question in an unreleased extension, depends on #1298. |

Issues #432, #472, #492, #796, #1069, #1251, #1256, #1258, #1398, #1405–#1410 are omitted from this table because they have plan files above.
