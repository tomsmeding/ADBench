# C++ Runner

## Overview
C++ Runner is one of the _benchmark runners_ described in [Architecture.md](../Architecture.md). C++ runner loads and runs _testing modules_, which in this case are dynamic shared libraries with `.dll` extension on all platforms. 

Each _testing module_ contains an implementation of some algorithm computing objective functions and their derivatives. Each objective should be supported by both the runner and the module to be benchmarked.

The runner has no information about the objective types supported by module. However, a module may support only some of the objective types. That's why if a user asks the runner to benchmark any of unsupported objectives, the runner prints an error to `stderr` and stops.

Every module, for every objective it supports, defines a class that implements the corresponding templated [`ITest`](./Modules.md#itest-implementation) interface.

There is also one exported factory function for each such class in each module.

## Supported Objective Types
Currently supported objective types and their factory functions:
     
| Full Name | Short Name | Function Name |
| -- | -- | -- |
| Gaussian Mixture Model Fitting | GMM | `get_gmm_test` |
| Bundle Adjustment| BA | `get_ba_test` |
| Hand Tracking | Hand | `get_hand_test` |
| Long short-term memory | LSTM | `get_lstm_test` |


## Command Line

```powershell
CPPRunner test_type module_path input_filepath output_dir minimum_measurable_time nruns_F nruns_J time_limit [-rep]
```

 - `test_type` - the short type name of a benchmarking objective function. It may also be equal to "Hand-Complicated" that designates the complicated case of the "Hand" objective, where the variable U is considered (see [srajer-autodiff-screen.pdf](../../Documents/srajer-autodiff-screen.pdf), page 5).
 - `module_path` - an absolute or relative path to a .dll module responsible for testing algorithm of calculation.
 - `input_filepath` - an absolute or relative path to an input file.
 - `output_dir` - a directory where output files will be stored.
 - `minimum_measurable_time` - minimum time that the computation needs to run to produce a reliable measurement.
 - `nruns_F` - maximum number of times to run the computation of the objective function for timing.
 - `nruns_J` - maximum number of times to run the computation of the considered derivative (gradient or Jacobian) for timing.
 - `time_limit` - soft (see [Methodology.md](../Methodology.md)) time limit for benchmarking the computation of either the objective function or its gradient/Jacobian.
 - `-rep` *(applicable only for GMM)* - if enabled all input data points have one shared value.

## Adding new modules
(see [Modules.md](./Modules.md))
## Adding new objective types

 1. Define `TInput`, `TOutput` and (if necessary) `TParameters` structs in the `/src/cpp/shared/TData.h` file where `T` is the name of a new objective. 

    `TInput` is the data type for the new objective's inputs, `TOutput` is a structure that contains outputs of both new objective function and its target jacobian.
    `TParameters` is a data type containing all configurable parameters of the benchmark. If there are no such parameters in the benchmark then just use the `DefaultParameters` struct.

 2. In the `ModuleLoader` class (`/src/cpp/runner/ModuleLoader.h`) implement the following function:
    ```cpp
    std::unique_ptr<ITest<TInput, TOutput>> get_T_test() const;
    ```
    It should find a function with the same name in the future shared library, execute it and return its result. In the case of the error it should throw a `runtime_error`. 
    
    Note that this function should be really similar to those already existing, for example `get_gmm_test`.
 3. Create `/src/cpp/runner/TBenchmark.h` with the following content:
    ```cpp
    #pragma once

    #include "Benchmark.h"
    ```

    Define the following functions in the `TBenchmark.h` and implement them in the `TBenchmark.cpp`:

    - 
      ```cpp
      template<>
      TInput read_input_data<TInput,TParameters>(const std::string& input_file, const TParameters& params);
      ```
          
      Opens input_file and loads data to the structure of the TInput type. 
      
      The format of the input file is specific for each objective type.
    - 
      ```cpp
      template<>
      unique_ptr<ITest<TInput, TOutput>> get_test<TInput, TOutput>(const ModuleLoader& module_loader);
      ```
          
      Chooses and calls the right method of the ModuleLoader corresponding to the TInput and TOutput types.
    - 
      ```cpp
      template<>
      void save_output_to_file<TOutput>(const TOutput& output, const string& output_prefix, const string& input_basename,
                                        const string& module_basename);
      ```
          
      Saves results of computations stored in a structure of the TOutput type to the output files:
      
        - `output_prefix + input_basename + "_F_" + module_basename + ".txt"` - stores the value of the objective function
        - `output_prefix + input_basename + "_J_" + module_basename + ".txt"` - stores the value of the objective function derivative
          
      The format of the output files is specific for each objective type.
4.  Include `TBenchmark.h` in `main.cpp` and add a new else-if branch to the `main` function as follows:
    ```cpp
    if (test_type == "GMM") {
        run_benchmark<GMMInput, GMMOutput, GMMParameters>(module_path, input_filepath, output_prefix, minimum_measurable_time, nruns_F, nruns_J, time_limit, { replicate_point });
    }
        ...
    else if (test_type == "T") {
            run_benchmark<TInput, TOutput, TParameters>(module_path, input_filepath, output_prefix, minimum_measurable_time, nruns_F, nruns_J, time_limit, parameters);
    }  
        ...
    ```
5. Add `TBenchmark.cpp` to `TestExtenders` environment variable inside `/src/cpp/runner/CMakeLists.txt`. 

## Input/Output files format

See [FileFormat.md](../FileFormat.md#input/output-files-format).