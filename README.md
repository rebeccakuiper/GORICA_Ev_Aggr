# Evidence aggregation using GORIC(A)
This is a simulation research regarding evidence aggregation using GORIC(A) published under the GNU General Public License (version 3).

See 'https://psyarxiv.com/qv76x' for information about GORIC(A) evidence aggregation.

See `https://github.com/rebeccakuiper/Tutorials` (Tutorial_GORIC_restriktor_evSyn.html) for a step-by-step tutorial on ways to perform GORIC(A) evidence aggregation.

Go to [the Shiny app](https://utrecht-university.shinyapps.io/GoricEvSyn/) to perform GORIC(A) evidence aggregation within a web-GUI.
There is also an R function - called evSyn - in the restriktor package to perform Evidence Synthesis.

Contact r.m.kuiper@uu.nl for any questions/comments related to the simulation or the manuscript.

## Run simulation + reproduce results
All files needed to run the simulation and reproduce our results are in `./sim/` and `./analysis/`.
The runtime of the complete simulation $\approx$ 4.6h using following specs:
- RAM: 16GB
- Processor: 11th Gen Intel(R) Core(TM) i7-1165G7 (quad core, 8 logical processors)
- Clockspeed: 2.80Ghz

`./simulation/` contains the following files which should be run in order to run the simulation and reproduce the results:

- `./expand_conditions.R`, file where the unique conditions are specified. Results are saved to `./conditions.Rdata`
- `./simulation.R`, runs the simulation where for each unique condition the added and equal evidence GORIC(A) weights are saved in `./results/results_added/`\* and `./results/results_equal/`\*, respectively. This file also appends the results to the corresponding condition and saves it as `./results/results_added.Rdata`* or `./results/results_equal.Rdata`.\*
- `./functions.R` is a file containing the actual simulation wrapper function as well as the dependency functions.
- `./testruns/`\* folder containing preliminary runs to test the simulation and its output.


`./analysis/` contains the following files for processing the output, obtain performance metrics and does the (statistical) analyses on these metrics.

- `./process_results.R`: processes the simulation output and obtains performance metrics.
  - output is saved at `./data/data_to_analyse.RData` and `./data/data/avg_per_condition.RData`.
- `./analyse_results.R`: does corresponding (statistical) analyses on results. Also obtains plots and tables.
- `./functions_analysis.R`: a file with functions used in the analysis.
- `./post_analysis.R`: calculates hypothesis selection rate for different selection thresholds and plots its relationship.

\*Note that the `./simulation/results/` and `.simulation/testruns/` folders are not in the remote due to size limitations. Still, results can be reproduced by following the steps above. For full access, please email to r.m.kuiper@uu.nl.
