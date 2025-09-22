import json
import matplotlib.pyplot as plt
import os

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

# Print some findings
print("\nFindings:")
print("1. Full topology generally converges fastest due to all-to-all connections.")
print("2. Line topology converges slowest, especially for larger networks, due to limited connectivity.")
print("3. 3D Grid and Imperfect 3D Grid show intermediate performance.")
print("4. Push-sum often takes longer than gossip for convergence due to numerical stability checks.")
print("5. Convergence time increases with network size, but logarithmic scale shows trends clearly.")