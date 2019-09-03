
CONTENTS
---------------------


 * runDirectory.R
 * build
   * input
   * code
   * temp
   * output
 * analysis
   * input
   * code
   * temp
   * output

INTRODUCTION
---------------------


To replicate the empirical work, run:

    "runDirectory.R"

which empty "build/temp", "build/output", "analysis/temp", and analysis/output; then run scripts in "build/code" and "analysis/code" in order and return equaveltent results in "analysis/output". 

Scripts in "build/code" process raw data saved in "build/input". Processed data is saved under "build/output" and simultaneously pushed to "analysis/input" for the next step. Scripts in "analysis/code" conduct summary and regression on processed data. The reults are reported as both text- and LaTeX-formated tables under "analysis/output".

