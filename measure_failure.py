import subprocess
import json
import matplotlib.pyplot as plt
import os

# Define the project directory
project_dir = os.path.dirname(os.path.abspath(__file__))

# Define the parameters
algorithms = ['gossip', 'push-sum']
topologies = ['full', '3D', 'line', 'imp3D']
failure_rates = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5]
size = 1000  # Fixed size for failure experiments

# Dictionary to store results
results = {alg: {topo: {} for topo in topologies} for alg in algorithms}

# Run simulations
for alg in algorithms:
    for topo in topologies:
        for failure_rate in failure_rates:
            print(f"Running {alg} {topo} {size} {failure_rate}")
            cmd = ['gleam', 'run', str(size), topo, alg, str(failure_rate)]
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=project_dir)
            if result.returncode == 0:
                # The last line is the time
                lines = result.stdout.strip().split('\n')
                time_str = lines[-1]
                try:
                    time = int(time_str)
                    results[alg][topo][str(failure_rate)] = time
                    print(f"Time: {time} ms")
                except ValueError:
                    print(f"Failed to parse time: {time_str}")
            else:
                print(f"Error running {alg} {topo} {size} {failure_rate}: {result.stderr}")

# Save to JSON
with open('results_failure.json', 'w') as f:
    json.dump(results, f, indent=2)

print("Results saved to results_failure.json")

# Load results
with open('results_failure.json', 'r') as f:
    data = json.load(f)

# Create plots folder
os.makedirs('plots', exist_ok=True)

# Create plots
for alg in ['gossip', 'push-sum']:
    fig, ax = plt.subplots()
    for topo in ['full', '3D', 'line', 'imp3D']:
        failure_rates = sorted([float(fr) for fr in data[alg][topo].keys()])
        times = [data[alg][topo][str(fr)] for fr in failure_rates]
        ax.plot(failure_rates, times, label=topo, marker='o')
    ax.set_xlabel('Failure Rate')
    ax.set_ylabel('Convergence Time (ms)')
    ax.set_title(f'{alg.capitalize()} Convergence Time vs Failure Rate (1000 nodes)')
    ax.legend()
    ax.grid(True)
    plt.savefig(f'plots/{alg}_failure.png')
    plt.close(fig)

print("Plots saved in 'plots' folder")