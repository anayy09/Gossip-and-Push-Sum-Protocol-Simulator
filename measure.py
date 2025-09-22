import subprocess
import json
import matplotlib.pyplot as plt
import os

# Define the project directory
project_dir = os.path.dirname(os.path.abspath(__file__))

# Define the parameters
algorithms = ['gossip', 'push-sum']
topologies = ['full', '3D', 'line', 'imp3D']
sizes = [100, 250, 500, 1000, 2500, 5000, 10000]

# Dictionary to store results
results = {alg: {topo: {} for topo in topologies} for alg in algorithms}

# Run simulations
for alg in algorithms:
    for topo in topologies:
        for size in sizes:
            print(f"Running {alg} {topo} {size}")
            cmd = ['gleam', 'run', str(size), topo, alg]
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=project_dir)
            if result.returncode == 0:
                # The last line is the time
                lines = result.stdout.strip().split('\n')
                time_str = lines[-1]
                try:
                    time = int(time_str)
                    results[alg][topo][str(size)] = time
                    print(f"Time: {time} ms")
                except ValueError:
                    print(f"Failed to parse time: {time_str}")
            else:
                print(f"Error running {alg} {topo} {size}: {result.stderr}")

# Save to JSON
with open('results.json', 'w') as f:
    json.dump(results, f, indent=2)

print("Results saved to results.json")

# Load results
with open('results.json', 'r') as f:
    data = json.load(f)

# Create plots folder
os.makedirs('plots', exist_ok=True)

# Create plots
for alg in ['gossip', 'push-sum']:
    fig, ax = plt.subplots()
    for topo in ['full', '3D', 'line', 'imp3D']:
        sizes = sorted([int(s) for s in data[alg][topo].keys()])
        times = [data[alg][topo][str(s)] for s in sizes]
        ax.plot(sizes, times, label=topo, marker='o')
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.set_xlabel('Network Size')
    ax.set_ylabel('Convergence Time (ms)')
    ax.set_title(f'{alg.capitalize()} Convergence Time vs Network Size')
    ax.legend()
    ax.grid(True)
    plt.savefig(f'plots/{alg}_convergence.png')
    plt.close(fig)

print("Plots saved in 'plots' folder")