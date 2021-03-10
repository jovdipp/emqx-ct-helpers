%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-define(CT_ALL, all).
-define(CT_SUITE, suite).
-define(CT_INIT_PER_SUITE, init_per_suite).
-define(CT_END_PER_SUITE, end_per_suite).
-define(CT_INIT_PER_GROUP, init_per_group).
-define(CT_END_PER_GROUP, end_per_group).

-define(MASTER_ORCHESTRATION, ct_master_orchestration ).
-define(JOB_MATRIX, job_matrix).
-define(JOB_OPTIONS, job_options).
-define(INIT_PER_JOB, init_per_job).
-define(END_PER_JOB, end_per_job).

-define(FWD_FUNCTIONS, [{?CT_ALL, 0}, {?CT_SUITE, 0}, {?CT_INIT_PER_SUITE, 1},
                        {?CT_END_PER_SUITE, 1}, {?CT_INIT_PER_GROUP, 2}, {?CT_END_PER_GROUP, 2},
                        {?MASTER_ORCHESTRATION, 1}, {?JOB_MATRIX, 0}, {?JOB_OPTIONS, 1},
                        {?INIT_PER_JOB, 1}, {?END_PER_JOB, 2}]).

-define(INTERNAL_PREFIX, ct_jobs_override_).
